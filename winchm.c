/*
 * winchm.c: direct output of .CHM files.
 */

#include <assert.h>
#include <stdio.h>

#include "halibut.h"
#include "tree234.h"
#include "lzx.h"

#define PUT_32BIT_LSB_FIRST(cp, value) do { \
  ((unsigned char *)cp)[0] = 0xFF & (value);      \
  ((unsigned char *)cp)[1] = 0xFF & ((value) >> 8); \
  ((unsigned char *)cp)[2] = 0xFF & ((value) >> 16); \
  ((unsigned char *)cp)[3] = 0xFF & ((value) >> 24); } while (0)

#define PUT_32BIT_MSB_FIRST(cp, value) do { \
  ((unsigned char *)cp)[3] = 0xFF & (value); \
  ((unsigned char *)cp)[2] = 0xFF & ((value) >> 8); \
  ((unsigned char *)cp)[1] = 0xFF & ((value) >> 16); \
  ((unsigned char *)cp)[0] = 0xFF & ((value) >> 24); } while (0)

#define PUT_16BIT_LSB_FIRST(cp, value) do { \
  ((unsigned char *)cp)[0] = 0xFF & (value); \
  ((unsigned char *)cp)[1] = 0xFF & ((value) >> 8); } while (0)

#define RDADD_32BIT_LSB_FIRST(rs, value) do { \
        unsigned char out[4]; \
        PUT_32BIT_LSB_FIRST(out, value); \
        rdaddsn(rs, (void *)out, sizeof(out));  \
    } while (0)

#define RDADD_32BIT_MSB_FIRST(rs, value) do { \
        unsigned char out[4]; \
        PUT_32BIT_MSB_FIRST(out, value); \
        rdaddsn(rs, (void *)out, sizeof(out)); \
    } while (0)

#define RDADD_16BIT_LSB_FIRST(rs, value) do { \
        unsigned char out[2]; \
        PUT_16BIT_LSB_FIRST(out, value); \
        rdaddsn(rs, (void *)out, sizeof(out)); \
    } while (0)

static void guid(rdstringc *rs, unsigned long w0,
                 unsigned short h0, unsigned short h1,
                 unsigned char b0, unsigned char b1,
                 unsigned char b2, unsigned char b3,
                 unsigned char b4, unsigned char b5,
                 unsigned char b6, unsigned char b7)
{
    RDADD_32BIT_LSB_FIRST(rs, w0);
    RDADD_16BIT_LSB_FIRST(rs, h0);
    RDADD_16BIT_LSB_FIRST(rs, h1);
    rdaddc(rs, b0);
    rdaddc(rs, b1);
    rdaddc(rs, b2);
    rdaddc(rs, b3);
    rdaddc(rs, b4);
    rdaddc(rs, b5);
    rdaddc(rs, b6);
    rdaddc(rs, b7);
}

static void itsf(rdstringc *rs,
                 const rdstringc *directory, const rdstringc *content0)
{
    int headersize_field;
    int headersect_off, headersect_off_field, headersect_size_field;
    int directory_off_field, content0_off_field, filesize_field;

    /* Main file header */
    rdaddsc(rs, "ITSF");               /* main file magic number */
    RDADD_32BIT_LSB_FIRST(rs, 3);      /* file format version */
    headersize_field = rs->pos;
    RDADD_32BIT_LSB_FIRST(rs, 0);      /* size of main header; fill in later */
    RDADD_32BIT_LSB_FIRST(rs, 1);      /* unknown, always observed to be 1 */
    RDADD_32BIT_MSB_FIRST(rs, 0x12345678); /* timestamp (FIXME) */
    RDADD_32BIT_LSB_FIRST(rs, 0x809); /* language code (FIXME: configurable) */
    guid(rs,0x7C01FD10,0x7BAA,0x11D0,0x9E,0x0C,0x00,0xA0,0xC9,0x22,0xE6,0xEC);
    guid(rs,0x7C01FD11,0x7BAA,0x11D0,0x9E,0x0C,0x00,0xA0,0xC9,0x22,0xE6,0xEC);
    headersect_off_field = rs->pos;
    RDADD_32BIT_LSB_FIRST(rs, 0); /* header section offset; fill in later */
    RDADD_32BIT_LSB_FIRST(rs, 0); /* MSW of 64-bit field */
    headersect_size_field = rs->pos;
    RDADD_32BIT_LSB_FIRST(rs, 0); /* header section size; fill in later */
    RDADD_32BIT_LSB_FIRST(rs, 0); /* MSW of 64-bit field */
    directory_off_field = rs->pos;
    RDADD_32BIT_LSB_FIRST(rs, 0); /* directory offset; fill in later */
    RDADD_32BIT_LSB_FIRST(rs, 0); /* MSW of 64-bit field */
    RDADD_32BIT_LSB_FIRST(rs, directory->pos);
    RDADD_32BIT_LSB_FIRST(rs, 0); /* MSW of 64-bit field */
    content0_off_field = rs->pos;
    RDADD_32BIT_LSB_FIRST(rs, 0); /* content section 0 offset; fill in later */
    RDADD_32BIT_LSB_FIRST(rs, 0); /* MSW of 64-bit field */
    PUT_32BIT_LSB_FIRST(rs->text + headersize_field, rs->pos);

    /* 'Header section' */
    headersect_off = rs->pos;
    PUT_32BIT_LSB_FIRST(rs->text + headersect_off_field, rs->pos);
    RDADD_32BIT_LSB_FIRST(rs, 0x1FE); /* magic number */
    RDADD_32BIT_LSB_FIRST(rs, 0); /* unknown, always observed to be 0 */
    filesize_field = rs->pos;
    RDADD_32BIT_LSB_FIRST(rs, 0); /* file size; fill in later */
    RDADD_32BIT_LSB_FIRST(rs, 0); /* MSW of 64-bit field */
    RDADD_32BIT_LSB_FIRST(rs, 0); /* unknown, always observed to be 0 */
    RDADD_32BIT_LSB_FIRST(rs, 0); /* unknown, always observed to be 0 */
    PUT_32BIT_LSB_FIRST(rs->text + headersect_size_field,
                        rs->pos - headersect_off);

    PUT_32BIT_LSB_FIRST(rs->text + directory_off_field, rs->pos);
    rdaddsn(rs, directory->text, directory->pos);

    PUT_32BIT_LSB_FIRST(rs->text + content0_off_field, rs->pos);
    rdaddsn(rs, content0->text, content0->pos);

    PUT_32BIT_LSB_FIRST(rs->text + filesize_field, rs->pos);
}

static void encint(rdstringc *rs, unsigned val)
{
    int i, j, topbit;

    /* ENCINT in the CHM format is big-endian, but it's easier to
     * write little-endian and byte-reverse afterwards. */

    i = rs->pos; /* first byte index */

    topbit = 0;
    while (val >= 0x80) {
        rdaddc(rs, (val & 0x7F) | topbit);
        val >>= 7;
        topbit = 0x80;
    }

    j = rs->pos; /* last byte index */
    rdaddc(rs, val | topbit);

    while (j > i) {
        char tmp = rs->text[i];
        rs->text[i] = rs->text[j];
        rs->text[j] = tmp;
        i++;
        j--;
    }
}

struct chm_directory_entry {
    char *filename;                    /* free this when done */
    int which_content_section;
    int offset_in_content_section;
    int file_size;
};

static int strcmp_chm(const char *a, const char *b)
{
    /*
     * CHM directory sorting criterion appears to be case-insensitive,
     * and based on sorting the _lowercased_ text. (Hence, in
     * particular, '_' sorts before any alphabetic character.)
     */
    while (*a || *b) {
        char ac = *a, bc = *b;
        if (ac >= 'A' && ac <= 'Z') ac += 'a'-'A';
        if (bc >= 'A' && bc <= 'Z') bc += 'a'-'A';
        if (ac != bc)
            return ac < bc ? -1 : +1;
        a++;
        b++;
    }

    return 0;
}

int chm_directory_entry_cmp(void *av, void *bv)
{
    const struct chm_directory_entry
        *a = (const struct chm_directory_entry *)av,
        *b = (const struct chm_directory_entry *)bv;
    return strcmp_chm(a->filename, b->filename);
}

int chm_directory_entry_find(void *av, void *bv)
{
    const char *a = (const char *)av;
    const struct chm_directory_entry
        *b = (const struct chm_directory_entry *)bv;
    return strcmp_chm(a, b->filename);
}

struct chm_index_entry {
    char *first_filename; /* shared pointer with some chm_directory_entry */
    int chunk_index;
};

static void directory(rdstringc *rs, tree234 *files)
{
    const int chunksize = 4096;
    const int encoded_density = 2;
    const int useful_density = 1 + (1 << encoded_density);
    int dirhdr_size_field, dirhdr_size2_field, dirhdr_depth_field;
    int dirhdr_root_field, dirhdr_tail_field, dirhdr_nchunks_field;
    int curr_chunk, depth, filename_index;
    tree234 *index;

    assert(rs->pos == 0);
    assert(count234(files) > 0);

    /* Directory header */
    rdaddsc(rs, "ITSP");               /* directory header magic number */
    RDADD_32BIT_LSB_FIRST(rs, 1);      /* format version */
    dirhdr_size_field = rs->pos;
    RDADD_32BIT_LSB_FIRST(rs, 0); /* directory header size; fill in later */
    RDADD_32BIT_LSB_FIRST(rs, 10);     /* unknown; observed to be 10 */
    RDADD_32BIT_LSB_FIRST(rs, chunksize);
    RDADD_32BIT_LSB_FIRST(rs, encoded_density);
    dirhdr_depth_field = rs->pos;
    RDADD_32BIT_LSB_FIRST(rs, 0); /* B-tree depth; fill in later */
    dirhdr_root_field = rs->pos;
    RDADD_32BIT_LSB_FIRST(rs, 0); /* root chunk index; fill in later */
    RDADD_32BIT_LSB_FIRST(rs, 0); /* head of PMGL chunk list; always 0 here */
    dirhdr_tail_field = rs->pos;
    RDADD_32BIT_LSB_FIRST(rs, 0); /* tail of PMGL chunk list; fill in later */
    RDADD_32BIT_LSB_FIRST(rs, 0xFFFFFFFFU); /* unknown; observed to be -1 */
    dirhdr_nchunks_field = rs->pos;
    RDADD_32BIT_LSB_FIRST(rs, 0); /* total number of chunks; fill in later */
    RDADD_32BIT_LSB_FIRST(rs, 0x409);  /* language (FIXME) */
    guid(rs,0x5D02926A,0x212E,0x11D0,0x9D,0xF9,0x00,0xA0,0xC9,0x22,0xE6,0xEC);
    dirhdr_size2_field = rs->pos;
    RDADD_32BIT_LSB_FIRST(rs, 0); /* directory header size; fill in later */
    RDADD_32BIT_LSB_FIRST(rs, 0xFFFFFFFFU); /* unknown; observed to be -1 */
    RDADD_32BIT_LSB_FIRST(rs, 0xFFFFFFFFU); /* unknown; observed to be -1 */
    RDADD_32BIT_LSB_FIRST(rs, 0xFFFFFFFFU); /* unknown; observed to be -1 */
    PUT_32BIT_LSB_FIRST(rs->text + dirhdr_size_field, rs->pos);
    PUT_32BIT_LSB_FIRST(rs->text + dirhdr_size2_field, rs->pos);

    index = newtree234(NULL);
    curr_chunk = 0;
    depth = 1;
    /* Write out lowest-level PMGL chunks full of actual directory entries */
    filename_index = 0;
    while (filename_index < count234(files)) {
        rdstringc chunk = {0, 0, NULL};
        rdstringc reversed_quickref = {0, 0, NULL};
        int chunk_endlen_field, chunk_nextptr_field;
        int n_entries, offset_of_first_entry;
        int saved_pos, saved_rq_pos, i;

        rdaddsc(&chunk, "PMGL");
        chunk_endlen_field = chunk.pos;
        RDADD_32BIT_LSB_FIRST(&chunk, 0); /* space at end; fill in later */
        RDADD_32BIT_LSB_FIRST(&chunk, 0); /* unknown; observed to be 0 */
        if (curr_chunk == 0) {
            RDADD_32BIT_LSB_FIRST(&chunk, 0xFFFFFFFF); /* 'null' prev ptr */
        } else {
            RDADD_32BIT_LSB_FIRST(&chunk, curr_chunk - 1);
        }
        chunk_nextptr_field = chunk.pos; /* may overwrite 'next' ptr later */
        RDADD_32BIT_LSB_FIRST(&chunk, curr_chunk + 1);

        /* Enter this chunk in our index for the next level of the
         * B-tree (if we end up needing one). */
        {
            struct chm_directory_entry *ent = (struct chm_directory_entry *)
                index234(files, filename_index);
            struct chm_index_entry *ient = snew(struct chm_index_entry);
            assert(ent);
            ient->first_filename = ent->filename;
            ient->chunk_index = curr_chunk;
            addpos234(index, ient, count234(index));
        }

        /* Start accumulating the quick-reference index at the end of this
         * chunk. We'll build it up backwards, and reverse it halfwordwise
         * when we copy it into the end of our output chunk. */
        RDADD_16BIT_LSB_FIRST(&reversed_quickref, 0);
        offset_of_first_entry = chunk.pos;

        n_entries = 0;
        /* Write filenames into this chunk until it's full, or until
         * we run out of filenames. */
        while (1) {
            struct chm_directory_entry *ent = (struct chm_directory_entry *)
                index234(files, filename_index++);
            if (!ent) {
                /* Run out of filenames, so this is the last PMGL chunk.
                 * Reset its 'next' pointer to the 'null' -1 value. */
                PUT_32BIT_LSB_FIRST(chunk.text + chunk_nextptr_field,
                                    0xFFFFFFFFU);
                /* And point the directory header's tail pointer at
                 * this chunk. */
                PUT_32BIT_LSB_FIRST(rs->text + dirhdr_tail_field, curr_chunk);
                break;
            }

            /* Save the sizes of stuff in this chunk, so we can put
             * them back if this entry turns out to overflow. */
            saved_pos = chunk.pos;
            saved_rq_pos = reversed_quickref.pos;

            if (n_entries > 0 && n_entries % useful_density == 0) {
                /* Add a quick-reference index pointer. */
                RDADD_16BIT_LSB_FIRST(&reversed_quickref, chunk.pos -
                                      offset_of_first_entry);
            }

            encint(&chunk, strlen(ent->filename));
            rdaddsc(&chunk, ent->filename);
            encint(&chunk, ent->which_content_section);
            encint(&chunk, ent->offset_in_content_section);
            encint(&chunk, ent->file_size);
            if (chunk.pos + reversed_quickref.pos > chunksize) {
                filename_index--;
                chunk.pos = saved_pos;
                reversed_quickref.pos = saved_rq_pos;
                break;
            }

            /* If we didn't overflow, then commit to this entry and
             * loop round for the next one. */
            n_entries++;
        }

        /* Finalise the chunk. */
        assert(chunk.pos + reversed_quickref.pos <= chunksize);
        PUT_32BIT_LSB_FIRST(chunk.text + chunk_endlen_field,
                            chunksize - chunk.pos);
        PUT_16BIT_LSB_FIRST(reversed_quickref.text, n_entries);
        rdaddc_rep(&chunk, 0, chunksize - chunk.pos - reversed_quickref.pos);
        for (i = reversed_quickref.pos - 2; i >= 0; i -= 2)
            rdaddsn(&chunk, reversed_quickref.text+i, 2);

        assert(chunk.pos == chunksize);
        rdaddsn(rs, chunk.text, chunk.pos);
        sfree(chunk.text);
        sfree(reversed_quickref.text);
        curr_chunk++;
    }

    /* Write out as many layers of PMGI index chunks as it takes to
     * reduce the total number of chunks at the current level to 1. */
    while (count234(index) > 1) {
        tree234 *prev_index;
        int index_index = 0;

        prev_index = index;
        index = newtree234(NULL);
        depth++;

        while (index_index < count234(prev_index)) {
            rdstringc chunk = {0, 0, NULL};
            rdstringc reversed_quickref = {0, 0, NULL};
            int chunk_endlen_field;
            int n_entries, offset_of_first_entry;
            int saved_pos, saved_rq_pos, i;

            rdaddsc(&chunk, "PMGI");
            chunk_endlen_field = chunk.pos;
            RDADD_32BIT_LSB_FIRST(&chunk, 0); /* space at end; fill in later */

            /* Enter this chunk in our index for the next level of the
             * B-tree (if we end up needing one). */
            {
                struct chm_index_entry *ent = (struct chm_index_entry *)
                    index234(prev_index, index_index);
                struct chm_index_entry *ient = snew(struct chm_index_entry);
                assert(ent);
                ient->first_filename = ent->first_filename;
                ient->chunk_index = curr_chunk;
                addpos234(index, ient, count234(index));
            }

            /* Start accumulating the quick-reference index at the end
             * of this chunk, as above. */
            RDADD_16BIT_LSB_FIRST(&reversed_quickref, 0);
            offset_of_first_entry = chunk.pos;

            n_entries = 0;
            /* Write index entries into this chunk until it's full, or
             * until we run out of chunks at the previous level. */
            while (1) {
                struct chm_index_entry *ent = (struct chm_index_entry *)
                index234(prev_index, index_index++);
                if (!ent)
                    break;

                /* Save the sizes of stuff in this chunk, so we can put
                 * them back if this entry turns out to overflow. */
                saved_pos = chunk.pos;
                saved_rq_pos = reversed_quickref.pos;

                if (n_entries > 0 && n_entries % useful_density == 0) {
                    /* Add a quick-reference index pointer. */
                    RDADD_16BIT_LSB_FIRST(&reversed_quickref, chunk.pos -
                                          offset_of_first_entry);
                }

                encint(&chunk, strlen(ent->first_filename));
                rdaddsc(&chunk, ent->first_filename);
                encint(&chunk, ent->chunk_index);
                if (chunk.pos + reversed_quickref.pos > chunksize) {
                    index_index--;
                    chunk.pos = saved_pos;
                    reversed_quickref.pos = saved_rq_pos;
                    break;
                }

                /* If we didn't overflow, then commit to this entry and
                 * loop round for the next one. */
                n_entries++;
            }

            /* Finalise the chunk. */
            assert(chunk.pos + reversed_quickref.pos <= chunksize);
            PUT_32BIT_LSB_FIRST(chunk.text + chunk_endlen_field,
                                chunksize - chunk.pos);
            PUT_16BIT_LSB_FIRST(reversed_quickref.text, n_entries);
            rdaddc_rep(&chunk, 0,
                       chunksize - chunk.pos - reversed_quickref.pos);
            for (i = reversed_quickref.pos - 2; i >= 0; i -= 2)
                rdaddsn(&chunk, reversed_quickref.text+i, 2);

            assert(chunk.pos == chunksize);
            rdaddsn(rs, chunk.text, chunk.pos);
            sfree(chunk.text);
            sfree(reversed_quickref.text);
            curr_chunk++;
        }

        /*
         * Now free the old index.
         */
        while (1) {
            struct chm_index_entry *ent = (struct chm_index_entry *)
                delpos234(prev_index, 0);
            if (!ent)
                break;
            sfree(ent);
        }
        freetree234(prev_index);
    }

    /*
     * Finished! We've reduced to a single chunk. Free the remaining
     * index (which must have size 1).
     */
    assert(count234(index) == 1);
    sfree(delpos234(index, 0));
    freetree234(index);

    /* Fill in the deferred fields in the main header. */
    PUT_32BIT_LSB_FIRST(rs->text + dirhdr_depth_field, depth);
    PUT_32BIT_LSB_FIRST(rs->text + dirhdr_root_field, curr_chunk-1);
    PUT_32BIT_LSB_FIRST(rs->text + dirhdr_nchunks_field, curr_chunk);
}

static int sys_start(rdstringc *rs, int code)
{
    int toret = rs->pos;
    RDADD_16BIT_LSB_FIRST(rs, code);
    RDADD_16BIT_LSB_FIRST(rs, 0);      /* length; overwrite later */
    return toret;
}
static void sys_end(rdstringc *rs, int recstart)
{
    PUT_16BIT_LSB_FIRST(rs->text + recstart+2, rs->pos - (recstart+4));
}

struct chm_window {
    char *name;
    char *title;
    char *contentsfile;
    char *indexfile;
    char *rootfile;
    int navpaneflags;
    int toolbarflags;
};

struct chm {
    tree234 *files;
    tree234 *windows;
    tree234 *stringtab;
    rdstringc content0;                /* outer uncompressed container */
    rdstringc content1;                /* compressed subfile */
    rdstringc outfile;
    rdstringc stringsfile;
    char *title, *contents_filename, *index_filename, *default_topic;
    char *default_window;
    struct chm_section *rootsecthead, *rootsecttail;
    struct chm_section *allsecthead, *allsecttail;
};

struct chm_section {
    /* Logical links within the section tree structure */
    struct chm_section *firstchild, *lastchild, *nextsibling, *parent;
    /* Link all chm_sections together into one big list, in a
     * topological order (i.e. every section comes after its
     * parent) */
    struct chm_section *next;

    char *title, *url;
    int tocidx_offset_1, tocidx_offset_2;
    int topic_index, urltbl_offset, urlstr_offset;
};

struct chm_stringtab_entry {
    struct chm *chm;
    int strtab_offset;
};

static int chm_stringtab_cmp(void *av, void *bv)
{
    const struct chm_stringtab_entry
        *a = (const struct chm_stringtab_entry *)av,
        *b = (const struct chm_stringtab_entry *)bv;
    return strcmp(a->chm->stringsfile.text + a->strtab_offset,
                  b->chm->stringsfile.text + b->strtab_offset);
}

static int chm_stringtab_find(void *av, void *bv)
{
    const char *a = (const char *)av;
    const struct chm_stringtab_entry
        *b = (const struct chm_stringtab_entry *)bv;
    return strcmp(a, b->chm->stringsfile.text + b->strtab_offset);
}

int chm_intern_string(struct chm *chm, const char *string)
{
    struct chm_stringtab_entry *ent;
    int size;

    if (!string)
        return 0;

    if ((ent = (struct chm_stringtab_entry *)find234(
             chm->stringtab, (void *)string, chm_stringtab_find)) == NULL) {
        ent = snew(struct chm_stringtab_entry);
        ent->chm = chm;

        /* Pad to ensure the string doesn't cross a page boundary. */
        size = strlen(string) + 1;  /* include the NUL terminator */
        assert(size < 0x1000);  /* avoid really serious trouble */
        if ((chm->stringsfile.pos ^ (chm->stringsfile.pos + size-1)) >> 12)
            rdaddc_rep(&chm->stringsfile, 0, 0xFFF & -chm->stringsfile.pos);

        ent->strtab_offset = chm->stringsfile.pos;
        rdaddsc(&chm->stringsfile, string);
        rdaddc(&chm->stringsfile, '\0');
        add234(chm->stringtab, ent);
    }
    return ent->strtab_offset;
}

struct chm *chm_new(void)
{
    struct chm *chm = snew(struct chm);
    chm->files = newtree234(chm_directory_entry_cmp);
    chm->windows = newtree234(NULL);
    chm->stringtab = newtree234(chm_stringtab_cmp);
    chm->content0 = empty_rdstringc;
    chm->content1 = empty_rdstringc;
    chm->outfile = empty_rdstringc;
    chm->stringsfile = empty_rdstringc;
    chm->title = NULL;
    chm->contents_filename = NULL;
    chm->index_filename = NULL;
    chm->default_topic = NULL;
    chm->default_window = NULL;
    chm->rootsecthead = chm->rootsecttail = NULL;
    chm->allsecthead = chm->allsecttail = NULL;
    chm_intern_string(chm, "");        /* preinitialise the strings table */
    return chm;
}

void chm_free(struct chm *chm)
{
    struct chm_directory_entry *ent;
    struct chm_window *win;
    struct chm_stringtab_entry *str;
    struct chm_section *sect;

    while ((ent = delpos234(chm->files, 0)) != NULL) {
        sfree(ent->filename);
        sfree(ent);
    }
    freetree234(chm->files);

    while ((win = delpos234(chm->windows, 0)) != NULL) {
        sfree(win->name);
        sfree(win->title);
        sfree(win->contentsfile);
        sfree(win->indexfile);
        sfree(win->rootfile);
        sfree(win);
    }
    freetree234(chm->windows);

    while ((str = delpos234(chm->stringtab, 0)) != NULL) {
        sfree(str);
    }
    freetree234(chm->stringtab);

    for (sect = chm->allsecthead; sect ;) {
        struct chm_section *tmp = sect->next;
        sfree(sect->title);
        sfree(sect->url);
        sfree(sect);
        sect = tmp;
    }

    sfree(chm->content0.text);
    sfree(chm->content1.text);
    sfree(chm->outfile.text);
    sfree(chm->stringsfile.text);

    sfree(chm->title);
    sfree(chm->contents_filename);
    sfree(chm->index_filename);
    sfree(chm->default_topic);
    sfree(chm->default_window);

    sfree(chm);
}

static void chm_add_file_internal(struct chm *chm, const char *name,
                                  const char *data, int len,
                                  rdstringc *sect, int which_sect)
{
    struct chm_directory_entry *ent = snew(struct chm_directory_entry);
    ent->filename = dupstr(name);
    ent->which_content_section = which_sect;
    ent->offset_in_content_section = sect->pos;
    ent->file_size = len;
    add234(chm->files, ent);
    rdaddsn(sect, data, len);
}

static struct chm_directory_entry *chm_find_file(
    struct chm *chm, const char *name)
{
    return find234(chm->files, (void *)name, chm_directory_entry_find);
}

static char *add_leading_slash(const char *str)
{
    char *toret = snewn(2 + strlen(str), char);
    toret[0] = '/';
    strcpy(toret+1, str);
    return toret;
}

void chm_add_file(struct chm *chm, const char *name, const char *data, int len)
{
    char *name_with_slash = add_leading_slash(name);
    chm_add_file_internal(chm, name_with_slash, data, len, &chm->content1, 1);
    sfree(name_with_slash);
}

void chm_title(struct chm *chm, const char *title)
{
    chm->title = dupstr(title);
}

void chm_contents_filename(struct chm *chm, const char *name)
{
    chm->contents_filename = dupstr(name);
}

void chm_index_filename(struct chm *chm, const char *name)
{
    chm->index_filename = dupstr(name);
}

void chm_default_topic(struct chm *chm, const char *name)
{
    chm->default_topic = dupstr(name);
}

void chm_default_window(struct chm *chm, const char *name)
{
    chm->default_window = dupstr(name);
}

void chm_add_window(struct chm *chm, const char *winname, const char *title,
                    const char *contentsfile, const char *indexfile,
                    const char *rootfile, int navpaneflags, int toolbarflags)
{
    struct chm_window *win = snew(struct chm_window);
    win->name = dupstr(winname);
    win->title = dupstr(title);
    win->contentsfile = contentsfile ? dupstr(contentsfile) : NULL;
    win->indexfile = indexfile ? dupstr(indexfile) : NULL;
    win->rootfile = dupstr(rootfile);
    win->navpaneflags = navpaneflags;
    win->toolbarflags = toolbarflags;
    addpos234(chm->windows, win, count234(chm->windows));
}

struct chm_section *chm_add_section(struct chm *chm,
                                    struct chm_section *parent,
                                    const char *title, const char *url)
{
    struct chm_section *sect = snew(struct chm_section);
    sect->title = dupstr(title);
    sect->url = dupstr(url);
    sect->firstchild = sect->lastchild = sect->nextsibling = sect->next = NULL;
    if (parent) {
        sect->parent = parent;
        if (parent->lastchild) {
            parent->lastchild->nextsibling = sect;
        } else {
            parent->firstchild = sect;
        }
        parent->lastchild = sect;
    } else {
        sect->parent = NULL;
        if (chm->rootsecttail) {
            chm->rootsecttail->nextsibling = sect;
        } else {
            chm->rootsecthead = sect;
        }
        chm->rootsecttail = sect;
    }
    if (chm->allsecttail) {
        chm->allsecttail->next = sect;
    } else {
        chm->allsecthead = sect;
    }
    chm->allsecttail = sect;
    return sect;
}

struct chm_urltbl_entry {
    /*
     * Records of #URLTBL, before their order is finalised.
     *
     * The first word of this record is listed as 'unknown, perhaps
     * some kind of unique ID' in chmspec. But my observation in HTML
     * Help Workshop's output is that it's actually a hash of the
     * target URL, and the file is sorted by them. chm_url_hash()
     * below implements the hash algorithm.
     */
    unsigned long hash;
    int topic_index;
    int urlstr_pos;
    int topics_offset_to_update;
};

int chm_urltbl_entry_cmp(void *av, void *bv)
{
    const struct chm_urltbl_entry
        *a = (const struct chm_urltbl_entry *)av,
        *b = (const struct chm_urltbl_entry *)bv;
    if (a->hash < b->hash) return -1;
    if (a->hash > b->hash) return +1;
    if (a->topic_index < b->topic_index) return -1;
    if (a->topic_index > b->topic_index) return -1;
    return 0;
}

static unsigned long chm_url_hash(const char *str)
{
    const char *p;
    unsigned long hash;

    hash = 0;
    for (p = str; *p; p++) {
        /*
         * Multiply `hash' by 43.
         */
        {
            unsigned long bottom, top;
            bottom = (hash & 0xFFFFUL) * 43;
            top = ((hash >> 16) & 0xFFFFUL) * 43;
            top += (bottom >> 16);
            bottom &= 0xFFFFUL;
            top &= 0xFFFFUL;
            hash = (top << 16) | bottom;
        }

        /*
         * Add the mapping value for this byte to `hash'.
         */
        {
            int c = (signed char)*p;

            /*
             * Translation rule determined by getting hhc.exe to hash
             * a lot of strings and analysing the results. I was able
             * to confirm this mapping rule for all byte values except
             * for NUL, CR, LF, ^Z and backslash: the first four of
             * those I couldn't find any way to get hhc to insert into
             * a URL, and the last one is automatically translated
             * into '/', presumably for reasons of Windows vs URI path
             * syntax normalisation.
             */
            int val = (c == '/' ? 0x2c : c <= 'Z' ? c-0x30 : c-0x50);

            if (val > 0 && hash > (0xFFFFFFFFUL - val)) {
                hash -= (0xFFFFFFFFUL - val) + 1;
            } else if (val < 0 && hash < (unsigned long)-val) {
                hash += (0xFFFFFFFFUL + val) + 1;
            } else
                hash += val;
        }
    }

    /*
     * Special case: an output hash of 0 is turned into 1, which I
     * conjecture is so that in some context or other 0 can be
     * reserved to mean something like 'null' or 'no hash value
     * available'.
     */
    if (hash == 0)
        hash = 1;

    return hash;
}

const char *chm_build(struct chm *chm, int *outlen)
{
    rdstringc dir = {0, 0, NULL};
    rdstringc sysfile = {0, 0, NULL};
    struct LZXEncodedFile *ef;
    int rec;

    chm_add_file_internal(chm, "/", "", 0, &chm->content0, 0);

    RDADD_32BIT_LSB_FIRST(&sysfile, 3); /* #SYSTEM file version */

    rec = sys_start(&sysfile, 9);  /* identify CHM-producing tool */
    rdaddsc(&sysfile, "Halibut, ");
    rdaddsc(&sysfile, version);
    rdaddc(&sysfile, '\0');
    sys_end(&sysfile, rec);

    rec = sys_start(&sysfile, 12);  /* number of 'information types' */
    RDADD_32BIT_LSB_FIRST(&sysfile, 0);
    sys_end(&sysfile, rec);
    rec = sys_start(&sysfile, 15);  /* checksum of 'information types' */
    RDADD_32BIT_LSB_FIRST(&sysfile, 0);
    sys_end(&sysfile, rec);
    /* actual section of 'information types', whatever those might be */
    chm_add_file_internal(chm, "/#ITBITS", "", 0, &chm->content0, 0);

    if (chm->title) {
        rec = sys_start(&sysfile, 3);  /* document title */
        rdaddsc(&sysfile, chm->title);
        rdaddc(&sysfile, '\0');
        sys_end(&sysfile, rec);
    }

    if (chm->default_topic) {
        rec = sys_start(&sysfile, 2);
        rdaddsc(&sysfile, chm->default_topic);
        rdaddc(&sysfile, '\0');
        sys_end(&sysfile, rec);
    }

    if (chm->contents_filename) {
        rec = sys_start(&sysfile, 0);
        rdaddsc(&sysfile, chm->contents_filename);
        rdaddc(&sysfile, '\0');
        sys_end(&sysfile, rec);
    }

    if (chm->index_filename) {
        rec = sys_start(&sysfile, 1);
        rdaddsc(&sysfile, chm->index_filename);
        rdaddc(&sysfile, '\0');
        sys_end(&sysfile, rec);
    }

    if (chm->default_window) {
        rec = sys_start(&sysfile, 5);
        rdaddsc(&sysfile, chm->default_window);
        rdaddc(&sysfile, '\0');
        sys_end(&sysfile, rec);
    }

    rec = sys_start(&sysfile, 4);
    RDADD_32BIT_LSB_FIRST(&sysfile, 0x809); /* language again (FIXME) */
    RDADD_32BIT_LSB_FIRST(&sysfile, 0);     /* DBCS: off */
    RDADD_32BIT_LSB_FIRST(&sysfile, 1);     /* full-text search: on */
    RDADD_32BIT_LSB_FIRST(&sysfile, 0); /* no KLinks (whatever they are) */
    RDADD_32BIT_LSB_FIRST(&sysfile, 0); /* no ALinks (whatever they are) */
    RDADD_32BIT_LSB_FIRST(&sysfile, 0x11223344); /* timestamp LSW (FIXME) */
    RDADD_32BIT_LSB_FIRST(&sysfile, 0x55667788); /* timestamp MSW (FIXME) */
    RDADD_32BIT_LSB_FIRST(&sysfile, 0);          /* unknown */
    RDADD_32BIT_LSB_FIRST(&sysfile, 0);          /* unknown */
    sys_end(&sysfile, rec);

    {
        rdstringc winfile = {0, 0, NULL};
        int i, s;
        struct chm_window *win;

        RDADD_32BIT_LSB_FIRST(&winfile, count234(chm->windows));
        RDADD_32BIT_LSB_FIRST(&winfile, 196); /* size of each entry */
        for (i = 0;
             (win = (struct chm_window *)index234(chm->windows, i)) != NULL;
             i++) {
            RDADD_32BIT_LSB_FIRST(&winfile, 196); /* size of entry */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* not Unicode */
            s = chm_intern_string(chm, win->name);
            RDADD_32BIT_LSB_FIRST(&winfile, s);
            /* Bitmap of which fields are used: 2 means nav pane
             * style, 0x200 means whether nav pane is initially
             * closed, 0x400 means tab position */
            RDADD_32BIT_LSB_FIRST(&winfile, 0x502);
            /* Nav pane styles:
             *  0x40000 = user can control window size/pos
             *  0x20000 = advanced full-text search UI
             *  0x00400 = include a search tab
             *  0x00100 = keep contents/index in sync with current topic
             *  0x00020 = three-pane window */
            RDADD_32BIT_LSB_FIRST(&winfile, win->navpaneflags);
            s = chm_intern_string(chm, win->title);
            RDADD_32BIT_LSB_FIRST(&winfile, s);
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* window styles */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* window ex styles */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* window rect.left */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* window rect.top */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* window rect.right */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* window rect.bottom */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* window show state */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* only used at runtime */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* only used at runtime */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* only used at runtime */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* only used at runtime */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* only used at runtime */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* only used at runtime */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* nav pane width */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* topic rect.left */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* topic rect.top */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* topic rect.right */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* topic rect.bottom */
            s = chm_intern_string(chm, win->contentsfile);
            RDADD_32BIT_LSB_FIRST(&winfile, s);
            s = chm_intern_string(chm, win->indexfile);
            RDADD_32BIT_LSB_FIRST(&winfile, s);
            s = chm_intern_string(chm, win->rootfile);
            RDADD_32BIT_LSB_FIRST(&winfile, s);
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* no Home button target */
            RDADD_32BIT_LSB_FIRST(&winfile, win->toolbarflags);
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* nav pane initially open */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* default nav pane = TOC */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* nav pane tabs at top */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* WM_NOTIFY id */
            rdaddc_rep(&winfile, 0, 20);        /* tab order block */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* history to keep */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* no Jump 1 button target */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* no Jump 2 button target */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* no Jump 1 button text */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* no Jump 2 button text */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* window min rect.left */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* window min rect.top */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* window min rect.right */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* window min rect.bottom */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* no information types */
            RDADD_32BIT_LSB_FIRST(&winfile, 0); /* no custom tabs */
        }
        assert(winfile.pos == 8 + 196 * count234(chm->windows));
        chm_add_file_internal(chm, "/#WINDOWS", winfile.text, winfile.pos,
                              &chm->content1, 1);
        sfree(winfile.text);
    }

    {
        struct chm_section *sect;
        rdstringc tocidx = {0, 0, NULL};
        rdstringc topics = {0, 0, NULL};
        rdstringc urltbl = {0, 0, NULL};
        rdstringc urlstr = {0, 0, NULL};
        int index, s, n_tocidx_3;
        struct chm_directory_entry *contentsfile = NULL, *indexfile = NULL;
        tree234 *urltbl_pre;
        struct chm_urltbl_entry *urltbl_entry;

        urltbl_pre = newtree234(chm_urltbl_entry_cmp);

        rdaddc_rep(&tocidx, 0, 0x1000);

        /* Write a header of one zero byte at the start of #URLSTR.
         * chmspec says this doesn't always appear, and is unclear on
         * what this is for, but I suspect it serves the same purpose
         * as the zero byte at the start of #STRINGS, namely that it
         * arranges that an absent string in the following records can
         * be represented by an offset of zero which will
         * automatically point to this byte and hence indicate the
         * empty string. */
        rdaddc(&urlstr, 0);

        if (chm->contents_filename) {
            char *withslash = add_leading_slash(chm->contents_filename);
            contentsfile = chm_find_file(chm, withslash);
            sfree(withslash);
            assert(contentsfile);
        }
        if (chm->index_filename) {
            char *withslash = add_leading_slash(chm->index_filename);
            indexfile = chm_find_file(chm, withslash);
            sfree(withslash);
            assert(indexfile);
        }

        index = 0;

        /* #TOCIDX header field pointing at start of type-1 records */
        PUT_32BIT_LSB_FIRST(tocidx.text + 0, tocidx.pos);

        /*
         * First pass over the section structure, generating in
         * parallel one of the multiple structure types in #TOCIDX and
         * the sole record in all the other files.
         */
        for (sect = chm->allsecthead; sect; sect = sect->next) {
            /* Size of the first kind of #TOCIDX record varies between
             * leaf and internal nodes */
            int tocidx_size_1 = (sect->firstchild ? 0x1c : 0x14);

            /*
             * Flags:
             *  - 8 means there's a local filename, which in _our_ CHM
             *    files there always is. If you unset this flag, you
             *    get a node in the contents treeview which doesn't
             *    open any page when clicked, and exists solely to
             *    contain children; in that situation the topic index
             *    field at position 0x08 in this record also stops
             *    being an index into #TOPICS and instead becomes an
             *    index into #STRINGS giving the node's title.
             *  - 4 apparently means the node should have the 'book'
             *    rather than 'page' icon in the TOC tree view in the
             *    help viewer
             *  - 1 means the node has a subtree in the tree view,
             *    which I take to mean (contrary to chmspec) that
             *    _this_ is the flag that means this node is a
             *    non-leaf node and hence has the two extra fields for
             *    first-child and whatever the other one means
             */
            unsigned tocidx_1_flags = (sect->firstchild ? 0x5 : 0) | 8;

            int urlstr_size;

            /* Pad to ensure the record isn't split between
             * 0x1000-byte pages of the file */
            while ((tocidx.pos ^ (tocidx.pos + tocidx_size_1 - 1)) >> 12)
                RDADD_32BIT_LSB_FIRST(&tocidx, 0);

            sect->topic_index = index++;

            /* Write the type-1 record in #TOCIDX */
            sect->tocidx_offset_1 = tocidx.pos;
            RDADD_16BIT_LSB_FIRST(&tocidx, 0); /* unknown */
            /* chmspec thinks this 16-bit field is 'unknown', but in
             * my observations it appears to be the index of an entry
             * in the #TOCIDX type-3 region. But I still don't know
             * what those are really for. */
            RDADD_16BIT_LSB_FIRST(&tocidx, sect->topic_index);
            RDADD_32BIT_LSB_FIRST(&tocidx, tocidx_1_flags);
            RDADD_32BIT_LSB_FIRST(&tocidx, sect->topic_index);
            RDADD_32BIT_LSB_FIRST(&tocidx, sect->parent ?
                                  sect->parent->tocidx_offset_1 : 0);
            RDADD_32BIT_LSB_FIRST(&tocidx, 0); /* 'next' ptr; fill in later */
            if (sect->firstchild) {
                RDADD_32BIT_LSB_FIRST(&tocidx, 0); /* child; fill in later */
                RDADD_32BIT_LSB_FIRST(&tocidx, 0); /* unknown */
            }
            assert(tocidx.pos == sect->tocidx_offset_1 + tocidx_size_1);

            /* Figure out our offset in #URLSTR, by ensuring we're not
             * going to overrun a page boundary (as usual). For this
             * we need our record length, which is two 32-bit fields
             * plus a NUL-terminated copy of the target file name / URL. */
            urlstr_size = 8 + strlen(sect->url) + 1;
            assert(urlstr_size < 0x1000); /* must _fit_ in a page! */
            if ((urlstr.pos ^ (urlstr.pos + urlstr_size - 1)) >> 12)
                rdaddc_rep(&urlstr, 0, 0xFFF & -urlstr_size);

            /*
             * Save everything we know so far about the #URLTBL record
             * we'll need to write.
             */
            urltbl_entry = snew(struct chm_urltbl_entry);
            urltbl_entry->hash = chm_url_hash(sect->url);
            urltbl_entry->topic_index = sect->topic_index;
            urltbl_entry->urlstr_pos = urlstr.pos;
            add234(urltbl_pre, urltbl_entry);

            /* Write the #TOPICS entry */
            RDADD_32BIT_LSB_FIRST(&topics, sect->tocidx_offset_1);
            s = chm_intern_string(chm, sect->title);
            RDADD_32BIT_LSB_FIRST(&topics, s);
            urltbl_entry->topics_offset_to_update = topics.pos;
            RDADD_32BIT_LSB_FIRST(&topics, 0); /* fill in later */
            RDADD_16BIT_LSB_FIRST(&topics, 6); /* flag as 'in contents' */
            RDADD_16BIT_LSB_FIRST(&topics, 0); /* unknown */

            /*
             * Write the #URLSTR entry.
             */
            RDADD_32BIT_LSB_FIRST(&urlstr, 0); /* URL string (null) */
            RDADD_32BIT_LSB_FIRST(&urlstr, 0); /* FrameName location (null) */
            rdaddsc(&urlstr, sect->url);       /* 'Local' */
            rdaddc(&urlstr, '\0');
        }

        /*
         * Add entries in #URLTBL, #URLSTR and #TOPICS for the
         * contents and index files. They don't form part of the tree
         * in #TOCIDX, though.
         */
        if (chm->contents_filename) {
            urltbl_entry = snew(struct chm_urltbl_entry);
            urltbl_entry->hash = chm_url_hash(chm->contents_filename);
            urltbl_entry->topic_index = index;
            urltbl_entry->urlstr_pos = urlstr.pos;
            add234(urltbl_pre, urltbl_entry);

            /* #TOPICS entry */
            RDADD_32BIT_LSB_FIRST(&topics, 0); /* no #TOCIDX entry */
            RDADD_32BIT_LSB_FIRST(&topics, 0xFFFFFFFFU); /* no title either */
            urltbl_entry->topics_offset_to_update = topics.pos;
            RDADD_32BIT_LSB_FIRST(&topics, 0); /* fill in later */
            RDADD_16BIT_LSB_FIRST(&topics, 2); /* flag as 'not in contents' */
            RDADD_16BIT_LSB_FIRST(&topics, 0); /* unknown */

            /* #URLSTR entry */
            RDADD_32BIT_LSB_FIRST(&urlstr, 0); /* URL string (null) */
            RDADD_32BIT_LSB_FIRST(&urlstr, 0); /* FrameName location (null) */
            rdaddsc(&urlstr, chm->contents_filename); /* 'Local' */
            rdaddc(&urlstr, '\0');

            /* And add the entry in #SYSTEM that cites the hash of the
             * #URLTBL entry. */
            rec = sys_start(&sysfile, 11);
            RDADD_32BIT_LSB_FIRST(&sysfile, urltbl_entry->hash);
            sys_end(&sysfile, rec);

            index++;
        }
        if (chm->index_filename) {
            urltbl_entry = snew(struct chm_urltbl_entry);
            urltbl_entry->hash = chm_url_hash(chm->index_filename);
            urltbl_entry->topic_index = index;
            urltbl_entry->urlstr_pos = urlstr.pos;
            add234(urltbl_pre, urltbl_entry);

            /* #TOPICS entry */
            RDADD_32BIT_LSB_FIRST(&topics, 0); /* no #TOCIDX entry */
            RDADD_32BIT_LSB_FIRST(&topics, 0xFFFFFFFFU); /* no title either */
            urltbl_entry->topics_offset_to_update = topics.pos;
            RDADD_32BIT_LSB_FIRST(&topics, 0); /* fill in later */
            RDADD_16BIT_LSB_FIRST(&topics, 2); /* flag as 'not in contents' */
            RDADD_16BIT_LSB_FIRST(&topics, 0); /* unknown */

            /* #URLSTR entry */
            RDADD_32BIT_LSB_FIRST(&urlstr, 0); /* URL string (null) */
            RDADD_32BIT_LSB_FIRST(&urlstr, 0); /* FrameName location (null) */
            rdaddsc(&urlstr, chm->index_filename); /* 'Local' */
            rdaddc(&urlstr, '\0');

            /* And add the entry in #SYSTEM that cites the hash of the
             * #URLTBL entry. */
            rec = sys_start(&sysfile, 7);
            RDADD_32BIT_LSB_FIRST(&sysfile, urltbl_entry->hash);
            sys_end(&sysfile, rec);

            index++;
        }

        /*
         * Now we've got all our #URLTBL entries, so we can write out
         * #URLTBL itself.
         */
        while ((urltbl_entry = delpos234(urltbl_pre, 0)) != NULL) {
            /* Pad #URLTBL to the beginning of this section's entry.
             * Entries are all 12 bytes long, but again there's some
             * padding to ensure that they don't cross a page
             * boundary. */
            while ((urltbl.pos ^ (urltbl.pos + 12 - 1)) >> 12)
                RDADD_32BIT_LSB_FIRST(&urltbl, 0);

            /* Fill in the link from #TOPICS to this entry's offset */
            PUT_32BIT_LSB_FIRST(topics.text +
                                urltbl_entry->topics_offset_to_update,
                                urltbl.pos);

            /* Write the entry itself. */
            RDADD_32BIT_LSB_FIRST(&urltbl, urltbl_entry->hash);
            RDADD_32BIT_LSB_FIRST(&urltbl, urltbl_entry->topic_index);
            RDADD_32BIT_LSB_FIRST(&urltbl, urltbl_entry->urlstr_pos);

            sfree(urltbl_entry);
        }
        freetree234(urltbl_pre);

        /*
         * Small follow-up pass filling in forward-pointing offset
         * fields in the #TOCIDX type-1 records which the previous
         * pass didn't know yet.
         */
        for (sect = chm->allsecthead; sect; sect = sect->next) {
            if (sect->nextsibling)
                PUT_32BIT_LSB_FIRST(tocidx.text + sect->tocidx_offset_1 + 0x10,
                                    sect->nextsibling->tocidx_offset_1);
            if (sect->firstchild)
                PUT_32BIT_LSB_FIRST(tocidx.text + sect->tocidx_offset_1 + 0x14,
                                    sect->firstchild->tocidx_offset_1);
        }

        /* #TOCIDX header field pointing at start of type-2 records */
        PUT_32BIT_LSB_FIRST(tocidx.text + 0xC, tocidx.pos);

        /*
         * Write the #TOCIDX type-2 records, which are just 4 bytes
         * long and just contain another copy of each topic's index,
         * but we need to have them there so that the type-3 records
         * can refer to them by offset.
         */
        for (sect = chm->allsecthead; sect; sect = sect->next) {
            sect->tocidx_offset_2 = tocidx.pos;
            RDADD_32BIT_LSB_FIRST(&tocidx, sect->topic_index);
        }

        /* Align the current #TOCIDX offset to 16 bytes */
        rdaddc_rep(&tocidx, 0, 0xF & -tocidx.pos);

        /* #TOCIDX header field pointing at start of type-3 records */
        PUT_32BIT_LSB_FIRST(tocidx.text + 0x4, tocidx.pos);

        /*
         * Write the #TOCIDX type-3 records.
         *
         * In help files I've examined, there are fewer of these than
         * you might expect; apparently not all sections rate one for
         * some reason. For the moment I'm just writing out one for
         * every section.
         */
        n_tocidx_3 = 0;
        for (sect = chm->allsecthead; sect; sect = sect->next) {
            RDADD_32BIT_LSB_FIRST(&tocidx, sect->tocidx_offset_1);
            RDADD_32BIT_LSB_FIRST(&tocidx, sect->topic_index + 666); /* ?! */
            RDADD_32BIT_LSB_FIRST(&tocidx, sect->tocidx_offset_2);
            RDADD_32BIT_LSB_FIRST(&tocidx, sect->topic_index);
            n_tocidx_3++;
        }

        /* #TOCIDX header field giving number of type-3 records */
        PUT_32BIT_LSB_FIRST(tocidx.text + 0x8, n_tocidx_3);

        chm_add_file_internal(chm, "/#TOCIDX", tocidx.text, tocidx.pos,
                              &chm->content1, 1);
        chm_add_file_internal(chm, "/#TOPICS", topics.text, topics.pos,
                              &chm->content1, 1);
        chm_add_file_internal(chm, "/#URLTBL", urltbl.text, urltbl.pos,
                              &chm->content1, 1);
        chm_add_file_internal(chm, "/#URLSTR", urlstr.text, urlstr.pos,
                              &chm->content1, 1);

        /*
         * Write #IDXHDR (and its mirror in #SYSTEM), which we
         * couldn't do until we knew how many topic nodes there were.
         */
        {
            int idxhdr_start;

            rec = sys_start(&sysfile, 13);
            idxhdr_start = sysfile.pos;

            rdaddsc(&sysfile, "T#SM");     /* #IDXHDR magic */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0x12345678); /* checksum? FIXME */
            RDADD_32BIT_LSB_FIRST(&sysfile, 1); /* unknown */
            RDADD_32BIT_LSB_FIRST(&sysfile, index); /* number of topic nodes */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0); /* unknown */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0xFFFFFFFFU); /* no image list */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0); /* unknown */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0); /* top-level node is
                                                 * not a folder */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0xFFFFFFFFU); /* no bg colour */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0xFFFFFFFFU); /* no fg colour */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0xFFFFFFFFU); /* no font spec */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0xFFFFFFFFU); /* no window style */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0xFFFFFFFFU); /* no ex win style */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0xFFFFFFFFU); /* unknown */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0xFFFFFFFFU); /* no frame name */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0xFFFFFFFFU); /* no window name */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0); /* no information types */
            RDADD_32BIT_LSB_FIRST(&sysfile, 1); /* unknown */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0); /* no merge files */
            RDADD_32BIT_LSB_FIRST(&sysfile, 0); /* unknown */
            rdaddc_rep(&sysfile, 0, 4096 - (sysfile.pos - idxhdr_start));

            chm_add_file_internal(chm, "/#IDXHDR", sysfile.text + idxhdr_start,
                                  sysfile.pos - idxhdr_start,
                                  &chm->content1, 1);
            sys_end(&sysfile, rec);
        }

        sfree(tocidx.text);
        sfree(topics.text);
        sfree(urltbl.text);
        sfree(urlstr.text);
    }

    /* Missing from #SYSTEM: */
    /* 10 (4-byte timestamp) */
    /* 6 (logical file name) */

    chm_add_file_internal(chm, "/#SYSTEM", sysfile.text, sysfile.pos,
                          &chm->content0, 0);
    sfree(sysfile.text);

    chm_add_file_internal(chm, "/#STRINGS", chm->stringsfile.text,
                          chm->stringsfile.pos, &chm->content1, 1);

    /*
     * ::DataSpace/NameList, giving the names of the two content sections.
     */
    {
        rdstringc dsnl = {0, 0, NULL};
        const char *p;
        int stringstart;

        RDADD_16BIT_LSB_FIRST(&dsnl, 0); /* total file size; fill in later */
        RDADD_16BIT_LSB_FIRST(&dsnl, 2); /* number of names */

        RDADD_16BIT_LSB_FIRST(&dsnl, 0); /* string length; fill in later */
        stringstart = dsnl.pos;
        for (p = "Uncompressed"; *p; p++)
            RDADD_16BIT_LSB_FIRST(&dsnl, *p);
        PUT_16BIT_LSB_FIRST(dsnl.text + stringstart - 2,
                            (dsnl.pos - stringstart) / 2);
        RDADD_16BIT_LSB_FIRST(&dsnl, 0); /* NUL terminator */

        RDADD_16BIT_LSB_FIRST(&dsnl, 0); /* string length; fill in later */
        stringstart = dsnl.pos;
        for (p = "MSCompressed"; *p; p++)
            RDADD_16BIT_LSB_FIRST(&dsnl, *p);
        PUT_16BIT_LSB_FIRST(dsnl.text + stringstart - 2,
                            (dsnl.pos - stringstart) / 2);
        RDADD_16BIT_LSB_FIRST(&dsnl, 0); /* NUL terminator */

        PUT_16BIT_LSB_FIRST(dsnl.text, dsnl.pos / 2);

        chm_add_file_internal(chm, "::DataSpace/NameList", dsnl.text, dsnl.pos,
                              &chm->content0, 0);

        sfree(dsnl.text);
    }

    /*
     * Actually compress the compressed-data section, load the
     * compressed version of it into the containing uncompressed
     * section, and write the auxiliary files describing it.
     */
    {
        rdstringc rs = {0, 0, NULL};
        const char *p;
        int orig_decomp_size = chm->content1.pos;
        size_t i;

        /* Pad to a realign-interval boundary */
        rdaddc_rep(&chm->content1, 0, 0x7FFF & -chm->content1.pos);

        ef = lzx(chm->content1.text, chm->content1.pos, 0x8000, 0x10000);
        chm_add_file_internal(
            chm, "::DataSpace/Storage/MSCompressed/Content",
            (char *)ef->data, ef->data_len, &chm->content0, 0);

        for (p = "{7FC28940-9D31-11D0-9B27-00A0C91E9C7C}"; *p; p++)
            RDADD_16BIT_LSB_FIRST(&rs, *p);
        rs.pos = 0x26; /* this file is always written truncated :-) */
        chm_add_file_internal(
            chm, "::DataSpace/Storage/MSCompressed/Transform/List",
            rs.text, rs.pos, &chm->content0, 0);
        rs.pos = 0;

        RDADD_32BIT_LSB_FIRST(&rs, orig_decomp_size);
        RDADD_32BIT_LSB_FIRST(&rs, 0); /* high word of 64-bit size */
        chm_add_file_internal(
            chm, "::DataSpace/Storage/MSCompressed/SpanInfo",
            rs.text, rs.pos, &chm->content0, 0);
        rs.pos = 0;

        RDADD_32BIT_LSB_FIRST(&rs, 6); /* file size */
        rdaddsc(&rs, "LZXC");          /* compression type identifier */
        RDADD_32BIT_LSB_FIRST(&rs, 2); /* version */
        RDADD_32BIT_LSB_FIRST(&rs, 2); /* reset interval in units of 2^15 */
        RDADD_32BIT_LSB_FIRST(&rs, 2); /* window size in units of 2^15 */
        RDADD_32BIT_LSB_FIRST(&rs, 1); /* reset interval multiplier */
        RDADD_32BIT_LSB_FIRST(&rs, 0); /* unknown */
        chm_add_file_internal(
            chm, "::DataSpace/Storage/MSCompressed/ControlData",
            rs.text, rs.pos, &chm->content0, 0);
        rs.pos = 0;

        RDADD_32BIT_LSB_FIRST(&rs, 2); /* unknown (version number?) */
        RDADD_32BIT_LSB_FIRST(&rs, ef->n_resets); /* reset table length */
        RDADD_32BIT_LSB_FIRST(&rs, 8); /* reset table entry size */
        RDADD_32BIT_LSB_FIRST(&rs, 0x28); /* reset table offset */
        RDADD_32BIT_LSB_FIRST(&rs, orig_decomp_size); /* uncompressed len */
        RDADD_32BIT_LSB_FIRST(&rs, 0); /* MSW */
        RDADD_32BIT_LSB_FIRST(&rs, ef->data_len); /* compressed len */
        RDADD_32BIT_LSB_FIRST(&rs, 0); /* MSW */
        RDADD_32BIT_LSB_FIRST(&rs, 0x8000); /* realign interval */
        RDADD_32BIT_LSB_FIRST(&rs, 0); /* MSW */
        for (i = 0; i < ef->n_resets; i++) {
            RDADD_32BIT_LSB_FIRST(&rs, ef->reset_byte_offsets[i]);
            RDADD_32BIT_LSB_FIRST(&rs, 0); /* MSW */
        }
        chm_add_file_internal(
            chm, "::DataSpace/Storage/MSCompressed/Transform/"
            "{7FC28940-9D31-11D0-9B27-00A0C91E9C7C}/InstanceData/ResetTable",
            rs.text, rs.pos, &chm->content0, 0);
        rs.pos = 0;
    }

    sfree(ef->data);
    sfree(ef->reset_byte_offsets);
    sfree(ef);

    directory(&dir, chm->files);
    itsf(&chm->outfile, &dir, &chm->content0);
    sfree(dir.text);

    assert(outlen);
    *outlen = chm->outfile.pos;
    return chm->outfile.text;
}
