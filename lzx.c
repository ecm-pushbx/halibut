#include <assert.h>
#include <stddef.h>

#include "halibut.h"
#include "huffman.h"
#include "lz77.h"
#include "lzx.h"

#define OUR_LZX_WINSIZE 0x10000
#define LZX_MINMATCHLEN 2
#define LZX_MAXMATCHLEN 257

int lzx_compute_position_slot(int pos, int *footer_bits)
{
    if (pos < 4) {
        /* The bottom four position slots cover one value each. */
        *footer_bits = 0;
        return pos;
    } else if (pos >= 0x40000) {
        /* _All_ slots from 36 onwards are 2^17 values wide. */
        *footer_bits = 17;
        return 34 + (pos >> 17);
    } else {
        /* In between, there are two slots for each power-of-2 size,
         * so that slots 4,5 have width 2^1, 6,7 have width 2^2, 8,9
         * have width 2^3, ..., and 34,35 have width 2^16. */
        int bits = 16;
        int shifted = pos;
        if (shifted < (1<<(18-8))) shifted <<= 8, bits -= 8;
        if (shifted < (1<<(18-4))) shifted <<= 4, bits -= 4;
        if (shifted < (1<<(18-2))) shifted <<= 2, bits -= 2;
        if (shifted < (1<<(18-1))) shifted <<= 1, bits -= 1;
        *footer_bits = bits;
        return 2 + 2*bits + ((shifted >> 16) & 1);
    }
}

typedef enum LZXSymType {
    LST_MAINTREE, LST_LENTREE, LST_ALIGNOFFTREE,
    LST_MAINTREE_PRETREE_1, LST_MAINTREE_PRETREE_2, LST_LENTREE_PRETREE,
    LST_NTREES, dummy_enum_const = LST_NTREES-1,
    LST_REALIGN_BITSTREAM,
    LST_RAWBITS_BASE /* add the number of actual bits to this code */
} LZXSymType;

typedef struct LZXSym {
    LZXSymType type;
    int value;
} LZXSym;

typedef struct LZXBuffer {
    LZXSym *syms;
    int nsyms, symsize;
} LZXBuffer;

typedef struct LZXInfo {
    LZXBuffer *buf;
    int r0, r1, r2;                    /* saved match offsets */
} LZXInfo;

static void lzx_buffer_init(LZXBuffer *buf)
{
    buf->syms = NULL;
    buf->nsyms = buf->symsize = 0;
}

static void lzx_addsym(LZXBuffer *buf, LZXSymType type, int value)
{
    if (buf->nsyms >= buf->symsize) {
        assert(buf->nsyms == buf->symsize);
        buf->symsize = buf->nsyms * 5 / 4 + 16384;
        buf->syms = sresize(buf->syms, buf->symsize, LZXSym);
    }
    buf->syms[buf->nsyms].type = type;
    buf->syms[buf->nsyms].value = value;
    buf->nsyms++;
}

static void lzx_literal(struct LZ77Context *ctx, unsigned char c)
{
    LZXBuffer *buf = ((LZXInfo *)ctx->userdata)->buf;
    lzx_addsym(buf, LST_MAINTREE, c);
}

static void lzx_match(struct LZ77Context *ctx, int match_offset, int totallen)
{
    LZXInfo *info = (LZXInfo *)ctx->userdata;
    LZXBuffer *buf = info->buf;

    /*
     * First, this variant of LZX has a maximum match length of 257
     * bytes, so if lz77.c reports a longer match than that, we must
     * break it up.
     */
    while (totallen > 0) {
        int len, length_header, length_footer, len_pos_header;
        int formatted_offset, position_slot, position_verbatim_bits;
        int position_verbatim_value, position_aligned_offset;

        if (totallen <= LZX_MAXMATCHLEN) {
            /* We can emit all of the (remaining) match length in one go. */
            len = totallen;
        } else if (totallen >= LZX_MAXMATCHLEN+LZX_MINMATCHLEN) {
            /* There's enough match left that we can emit a
             * maximum-length chunk and still be assured of being able
             * to emit what's left as a viable followup match. */
            len = LZX_MAXMATCHLEN;
        } else {
            /* The in-between case, where we have _only just_ too long
             * a match to emit in one go, so that if we emitted a
             * max-size chunk then what's left would be under the min
             * size and we couldn't emit it. */
            len = totallen - LZX_MINMATCHLEN;
        }
        totallen -= len;

        /*
         * Now we're outputting a single LZX-level match of length
         * 'len'. Break the length up into a 'header' (included in the
         * starting LST_MAINTREE symbol) and a 'footer' (tacked on
         * afterwards using LST_LENTREE).
         */
        if (len < 9) {
            length_header = len - 2;   /* in the range {0,...,6} */
            length_footer = -1;        /* not transmitted at all */
        } else {
            length_header = 7;         /* header indicates more to come */
            length_footer = len - 9;   /* in the range {0,...,248} */
        }

        /*
         * Meanwhile, the raw backward distance is first transformed
         * into the 'formatted offset', by either adding 2 or using
         * one of the low-numbered special codes meaning to use one of
         * the three most recent match distances.
         */
        if (match_offset == info->r0) {
            /* Reuse the most recent distance */
            formatted_offset = 0;
        } else if (match_offset == info->r1) {
            /* Reuse the 2nd most recent, and swap it into first place */
            int tmp = info->r1;
            info->r1 = info->r0;
            info->r0 = tmp;
            formatted_offset = 1;
        } else if (match_offset == info->r2) {
            /* Reuse the 3rd most recent and swap it to first place.
             * This is intentionally not quite a move-to-front
             * shuffle, which would permute (r0,r1,r2)->(r2,r0,r1); MS
             * decided that just swapping r0 with r2 was a better
             * performance tradeoff. */
            int tmp = info->r2;
            info->r2 = info->r0;
            info->r0 = tmp;
            formatted_offset = 2;
        } else {
            /* This offset matches none of the three saved values.
             * Put it in r0, and move up the rest of the list. */
            info->r2 = info->r1;
            info->r1 = info->r0;
            info->r0 = match_offset;
            formatted_offset = match_offset + 2;
        }

        /*
         * The formatted offset now breaks up into a 'position slot'
         * (encoded as part of the starting symbol) and an offset from
         * the smallest position value covered by that slot. The
         * system of slots is designed so that every slot's width is a
         * power of two and its base value is a multiple of its width,
         * so we can get the offset just by taking the bottom n bits
         * of the full formatted offset, once the choice of position
         * slot tells us what n is.
         */
        position_slot = lzx_compute_position_slot(
            formatted_offset, &position_verbatim_bits);
        position_verbatim_value = formatted_offset &
            ((1 << position_verbatim_bits)-1);

        /*
         * If there are three or more additional bits, then the last 3
         * of them are (potentially, depending on block type which we
         * haven't decided about yet) transmitted using the aligned
         * offset tree. The rest are sent verbatim.
         */
        if (position_verbatim_bits >= 3) {
            position_aligned_offset = position_verbatim_value & 7;
            position_verbatim_bits -= 3;
            position_verbatim_value >>= 3;
        } else {
            position_aligned_offset = -1; /* not transmitted */
        }

        /*
         * Combine the length header and position slot into the full
         * set of information encoded by the starting symbol.
         */
        len_pos_header = position_slot * 8 + length_header;

        /*
         * And now we've finished figuring out _what_ to output, so
         * output it.
         */
        lzx_addsym(buf, LST_MAINTREE, 256 + len_pos_header);
        if (length_footer >= 0)
            lzx_addsym(buf, LST_LENTREE, length_footer);
        if (position_verbatim_bits > 0)
            lzx_addsym(buf, LST_RAWBITS_BASE + position_verbatim_bits,
                       position_verbatim_value);
        if (position_aligned_offset >= 0)
            lzx_addsym(buf, LST_ALIGNOFFTREE, position_aligned_offset);
    }
}

void lzx_lz77_inner(LZXInfo *info, const unsigned char *data, int len)
{
    struct LZ77Context lz77c;
    lz77_init(&lz77c, OUR_LZX_WINSIZE);
    lz77c.literal = lzx_literal;
    lz77c.match = lzx_match;
    lz77c.userdata = info;
    lz77_compress(&lz77c, data, len, true);
    lz77_cleanup(&lz77c);
}

void lzx_lz77(LZXBuffer *buf, const unsigned char *data,
              int totallen, int realign_interval)
{
    LZXInfo info;

    info.r0 = info.r1 = info.r2 = 1;
    info.buf = buf;

    while (totallen > 0) {
        int thislen =
            totallen < realign_interval ? totallen : realign_interval;
        lzx_lz77_inner(&info, data, thislen);
        data += thislen;
        totallen -= thislen;
        if (totallen > 0)
            lzx_addsym(info.buf, LST_REALIGN_BITSTREAM, 0);
    }
}

typedef struct LZXHuf {
    int nsyms;
    unsigned char *lengths;
    unsigned char *oldlengths; /* for pretree encoding to diff against */
    int *codes;
} LZXHuf;

typedef struct LZXHufs {
    LZXHuf hufs[LST_NTREES];
} LZXHufs;

void lzx_build_tree(LZXSym *syms, int nsyms, LZXSymType which, LZXHufs *hufs)
{
    int i, max_code_len;
    int *freqs;
    LZXHuf *huf = &hufs->hufs[which];

    switch (which) {
      default:
        assert(0 && "Bad lzx_build_tree tree type");
      case LST_MAINTREE:
        /*
         * Trees encoded via a pretree have a max code length of 16,
         * because that's the limit of what the pretree alphabet can
         * represent.
         */
        max_code_len = 16;

        /*
         * Number of symbols in the main tree is 256 literals, plus 8n
         * match header symbols where n is the largest position slot
         * number that might be needed to address any offset in the
         * window.
         */
        {
            int ignored, last_slot;
            last_slot = lzx_compute_position_slot(OUR_LZX_WINSIZE-1, &ignored);
            huf->nsyms = 8 * (last_slot+1) + 256;
        }
        break;
      case LST_LENTREE:
        max_code_len = 16;             /* pretree again */
        huf->nsyms = 249;              /* a fixed value in the spec */
        break;
      case LST_MAINTREE_PRETREE_1:
      case LST_MAINTREE_PRETREE_2:
      case LST_LENTREE_PRETREE:
        /* Pretree code lengths are stored in 4-bit fields, so they
         * can't go above 15. There are a standard 20 symbols in the
         * pretree alphabet. */
        max_code_len = 15;
        huf->nsyms = 20;
        break;
      case LST_ALIGNOFFTREE:
        /* The aligned-offset tree has 8 elements stored in 3-bit
         * fields. */
        max_code_len = 7;
        huf->nsyms = 8;
        break;
    }

    freqs = snewn(huf->nsyms, int);

    /*
     * Count up the symbol frequencies.
     */
    for (i = 0; i < huf->nsyms; i++)
        freqs[i] = 0;
    for (i = 0; i < nsyms; i++)
        if (syms[i].type == which)
            freqs[syms[i].value]++;

    /*
     * Build the Huffman table.
     */
    huf->lengths = snewn(huf->nsyms, unsigned char);
    build_huffman_tree(freqs, huf->lengths, huf->nsyms, max_code_len);
    huf->codes = snewn(huf->nsyms, int);
    compute_huffman_codes(huf->lengths, huf->codes, huf->nsyms);

    /*
     * Cleanup.
     */
    sfree(freqs);
}

void lzx_tree_with_pretree(LZXHuf *huf, int symoffset, int symlimit,
                           LZXBuffer *buf, LZXSymType pretree_symtype)
{
    int i, r;

    if (!huf->oldlengths) {
        huf->oldlengths = snewn(huf->nsyms, unsigned char);
        for (i = 0; i < huf->nsyms; i++)
            huf->oldlengths[i] = 0;
    }

    for (i = symoffset; i < symlimit; i++) {
        for (r = 1; i+r < symlimit; r++)
            if (huf->lengths[i+r] != huf->lengths[i])
                break;

        if (r >= 4) {
            /*
             * We have at least one run of the same code length long
             * enough to use one of the run-length encoding symbols.
             */
            while (r >= 4) {
                int thisrun;
                if (huf->lengths[i] == 0) {
                    thisrun = r > 20+31 ? 20+31 : r;
                    if (thisrun >= 20) {
                        lzx_addsym(buf, pretree_symtype, 18);
                        lzx_addsym(buf, LST_RAWBITS_BASE + 5, thisrun - 20);
                    } else {
                        lzx_addsym(buf, pretree_symtype, 17);
                        lzx_addsym(buf, LST_RAWBITS_BASE + 4, thisrun - 4);
                    }
                } else {
                    thisrun = r > 5 ? 5 : r;
                    lzx_addsym(buf, pretree_symtype, 19);
                    lzx_addsym(buf, LST_RAWBITS_BASE + 1, thisrun - 4);
                    lzx_addsym(buf, pretree_symtype,
                               (huf->oldlengths[i]-huf->lengths[i] + 17) % 17);
                }
                r -= thisrun;
                i += thisrun;
            }

            if (r == 0) {
                i--;        /* compensate for normal loop increment */
                continue;
            }
        }

        /*
         * Otherwise, emit a normal non-encoded symbol.
         */
        lzx_addsym(buf, pretree_symtype,
                   (huf->oldlengths[i]-huf->lengths[i] + 17) % 17);
    }
}

void lzx_tree_simple(LZXHuf *huf, LZXBuffer *buf, int bits)
{
    int i;
    for (i = 0; i < huf->nsyms; i++)
        lzx_addsym(buf, LST_RAWBITS_BASE + bits, huf->lengths[i]);
}

typedef struct LZXBitstream {
    struct LZXEncodedFile *ef;
    size_t data_size, resets_size;
    unsigned short bitbuffer;
    int nbits;
    int first_block;
} LZXBitstream;

void lzx_write_bits(LZXBitstream *bs, int value, int bits)
{
    while (bs->nbits + bits >= 16) {
        int thisbits = 16 - bs->nbits;
        bs->bitbuffer = (bs->bitbuffer << thisbits) |
            (value >> (bits-thisbits));

        if (bs->ef->data_len+2 > bs->data_size) {
            bs->data_size = bs->ef->data_len * 5 / 4 + 65536;
            bs->ef->data = sresize(bs->ef->data, bs->data_size,
                                   unsigned char);
        }
        bs->ef->data[bs->ef->data_len++] = bs->bitbuffer;
        bs->ef->data[bs->ef->data_len++] = bs->bitbuffer >> 8;

        bs->bitbuffer = 0;
        bs->nbits = 0;

        bits -= thisbits;
        value &= (1<<bits) - 1;
    }

    bs->bitbuffer = (bs->bitbuffer << bits) | value;
    bs->nbits += bits;
}

void lzx_realign(LZXBitstream *bs)
{
    lzx_write_bits(bs, 0, 15 & -(unsigned)bs->nbits);
}

void lzx_write_reset_table_entry(LZXBitstream *bs)
{
    lzx_write_bits(bs, 0, 15 & -(unsigned)bs->nbits);

    if (bs->ef->n_resets >= bs->resets_size) {
        bs->resets_size = bs->ef->n_resets * 5 / 4 + 256;
        bs->ef->reset_byte_offsets = sresize(bs->ef->reset_byte_offsets,
                                             bs->resets_size, size_t);
    }
    bs->ef->reset_byte_offsets[bs->ef->n_resets++] = bs->ef->data_len;
}

void lzx_huf_encode(LZXSym *syms, int nsyms, LZXHufs *hufs, LZXBitstream *bs)
{
    int i;
    for (i = 0; i < nsyms; i++) {
        LZXSymType type = syms[i].type;
        int value = syms[i].value;

        if (type >= LST_RAWBITS_BASE) {
            lzx_write_bits(bs, value, type - LST_RAWBITS_BASE);
        } else if (type == LST_REALIGN_BITSTREAM) {
            /* Realign the bitstream to a 16-bit boundary, and write a
             * reset table entry giving the resulting byte offset. */
            lzx_realign(bs);
            lzx_write_reset_table_entry(bs);
        } else {
            lzx_write_bits(bs, hufs->hufs[type].codes[value],
                           hufs->hufs[type].lengths[value]);
        }
    }
}

void lzx_encode_block(LZXSym *syms, int nsyms, int blocksize,
                      LZXHufs *hufs, LZXBitstream *bs)
{
    LZXBuffer header[8];
    int i, blocktype;

    for (i = 0; i < (int)lenof(header); i++)
        lzx_buffer_init(&header[i]);

    /*
     * Build the Huffman trees for the main alphabets used in the
     * block.
     */
    lzx_build_tree(syms, nsyms, LST_MAINTREE, hufs);
    lzx_build_tree(syms, nsyms, LST_LENTREE, hufs);
    lzx_build_tree(syms, nsyms, LST_ALIGNOFFTREE, hufs);

    /*
     * Encode each of those as a sequence of pretree symbols.
     */
    lzx_tree_with_pretree(&hufs->hufs[LST_MAINTREE], 0, 256,
                          &header[3], LST_MAINTREE_PRETREE_1);
    lzx_tree_with_pretree(&hufs->hufs[LST_MAINTREE], 256,
                          hufs->hufs[LST_MAINTREE].nsyms,
                          &header[5], LST_MAINTREE_PRETREE_2);
    lzx_tree_with_pretree(&hufs->hufs[LST_LENTREE], 0,
                          hufs->hufs[LST_LENTREE].nsyms,
                          &header[7], LST_LENTREE_PRETREE);

    /*
     * Build the pretree for each of those encodings.
     */
    lzx_build_tree(header[3].syms, header[3].nsyms,
                   LST_MAINTREE_PRETREE_1, hufs);
    lzx_build_tree(header[5].syms, header[5].nsyms,
                   LST_MAINTREE_PRETREE_2, hufs);
    lzx_build_tree(header[7].syms, header[7].nsyms,
                   LST_LENTREE_PRETREE, hufs);

    /*
     * Decide whether we're keeping the aligned offset tree or not.
     */
    {
        int with, without;

        with = 3*8;                    /* cost of transmitting tree */
        without = 0;                   /* or not */

        for (i = 0; i < nsyms; i++)
            if (syms[i].type == LST_ALIGNOFFTREE) {
                with += hufs->hufs[LST_ALIGNOFFTREE].lengths[syms[i].value];
                without += 3;
            }

        if (with < without) {
            /* Yes, it's a win to use the aligned offset tree. */
            blocktype = 2;
        } else {
            /* No, we do better by throwing it away. */
            blocktype = 1;

            /* Easiest way to simulate that is to pretend we're still
             * using an aligned offset tree in the encoding, but to
             * chuck away our code lengths and replace them with the
             * fixed-length trivial tree. */
            for (i = 0; i < 8; i++) {
                hufs->hufs[LST_ALIGNOFFTREE].lengths[i] = 3;
                hufs->hufs[LST_ALIGNOFFTREE].codes[i] = i;
            }
        }
    }

    /*
     * Encode all the simply encoded trees (the three pretrees and the
     * aligned offset tree).
     */
    lzx_tree_simple(&hufs->hufs[LST_MAINTREE_PRETREE_1], &header[2], 4);
    lzx_tree_simple(&hufs->hufs[LST_MAINTREE_PRETREE_2], &header[4], 4);
    lzx_tree_simple(&hufs->hufs[LST_LENTREE_PRETREE], &header[6], 4);
    if (blocktype == 2)
        lzx_tree_simple(&hufs->hufs[LST_ALIGNOFFTREE], &header[1], 3);

    /*
     * Top-level block header.
     */
    if (bs->first_block) {
        /*
         * Also include the whole-file header which says whether E8
         * call translation is on. We never turn it on, because we
         * don't support it (since in this use case it doesn't seem
         * likely to be particularly useful anyway).
         *
         * It looks like a layer violation to put the output of this
         * whole-file header inside the per-block function like this,
         * but in fact it has to be done here because the first reset
         * table entry really is supposed to point to the _start_ of
         * the whole-file header.
         */
        lzx_addsym(&header[0], LST_RAWBITS_BASE + 1, 0);
        bs->first_block = false;
    }
    lzx_addsym(&header[0], LST_RAWBITS_BASE + 3, blocktype);
    lzx_addsym(&header[0], LST_RAWBITS_BASE + 24, blocksize);

    /*
     * Ensure the bit stream starts off aligned, and output an initial
     * reset-table entry.
     */
    lzx_realign(bs);
    lzx_write_reset_table_entry(bs);

    /*
     * Write out all of our symbol sequences in order: all of those
     * assorted header fragments, then the main LZ77 token sequence.
     */
    for (i = 0; i < (int)lenof(header); i++)
        lzx_huf_encode(header[i].syms, header[i].nsyms, hufs, bs);
    lzx_huf_encode(syms, nsyms, hufs, bs);

    /*
     * Clean up.
     */
    for (i = 0; i < (int)lenof(header); i++)
        sfree(header[i].syms);
    for (i = 0; i < (int)lenof(hufs->hufs); i++) {
        sfree(hufs->hufs[i].codes);
        sfree(hufs->hufs[i].lengths);
    }
}

struct LZXEncodedFile *lzx(const void *vdata, int totallen,
                           int realign_interval, int reset_interval)
{
    const unsigned char *data = (const unsigned char *)vdata;
    LZXBitstream bs;
    LZXHufs hufs;
    int i;

    bs.ef = snew(struct LZXEncodedFile);
    bs.ef->data = NULL;
    bs.ef->reset_byte_offsets = NULL;
    bs.ef->data_len = bs.data_size = 0;
    bs.ef->n_resets = bs.resets_size = 0;
    bs.bitbuffer = 0;
    bs.nbits = 0;

    for (i = 0; i < (int)lenof(hufs.hufs); i++)
        hufs.hufs[i].oldlengths = NULL;

    while (totallen > 0) {
        int thislen =
            totallen < reset_interval ? totallen : reset_interval;
        LZXBuffer buf;

        lzx_buffer_init(&buf);

        lzx_lz77(&buf, data, thislen, realign_interval);
        data += thislen;
        totallen -= thislen;

        /*
         * Block boundaries are chosen completely trivially: since we
         * have to terminate a block every time we reach the (fairly
         * short) reset interval in any case, it doesn't hurt us much
         * to just fix the assumption that every (reset_interval)
         * bytes of the input turn into exactly one block, i.e. the
         * whole of buf.syms that we just constructed is output in one
         * go. We _could_ try improving on this by clever
         * block-boundary heuristics, but I don't really think it's
         * worth it.
         */
        bs.first_block = true; /* reset every time we reset the LZ state */
        lzx_encode_block(buf.syms, buf.nsyms, thislen, &hufs, &bs);

        sfree(buf.syms);
    }

    for (i = 0; i < (int)lenof(hufs.hufs); i++)
        sfree(hufs.hufs[i].oldlengths);

    /* Realign to a 16-bit boundary, i.e. flush out any last few
     * unwritten bits. */
    lzx_realign(&bs);

    return bs.ef;
}

#ifdef LZX_TEST
/*
gcc -g -O0 -DLZX_TEST -o lzxtest -Icharset lzx.c lz77.c huffman.c malloc.c
*/
#include <err.h>
int main(int argc, char **argv)
{
    FILE *fp;
    long insize;
    unsigned char *inbuf;
    struct LZXEncodedFile *ef;

    if (argc != 3)
        errx(1, "expected infile and outfile arguments");

    fp = fopen(argv[1], "rb");
    if (!fp)
        err(1, "%s: open", argv[1]);
    fseek(fp, 0, SEEK_END);
    insize = ftell(fp);
    rewind(fp);
    inbuf = snewn(insize, unsigned char);
    fread(inbuf, 1, insize, fp);
    fclose(fp);

    ef = lzx(inbuf, insize, 0x8000, 0x10000);

    fp = fopen(argv[2], "wb");
    if (!fp)
        err(1, "%s: open", argv[2]);
    fwrite(ef->data, 1, ef->data_len, fp);
    fclose(fp);

    sfree(ef->data);
    sfree(ef->reset_byte_offsets);
    sfree(ef);
    sfree(inbuf);

    return 0;
}

wchar_t *ustrdup(wchar_t const *s) { assert(0 && "should be unused"); }
void fatalerr_nomemory(void) { errx(1, "out of memory"); }
#endif
