/*
 * iso6937.c - the _almost_ single-byte character set ISO/IEC 6937.
 *
 * Also, a tiny variation on it which adds the Euro sign at the
 * previously unused position 0xA4, used in DVB metadata.
 */

#ifndef ENUM_CHARSETS

#include "charset.h"
#include "internal.h"

/*
 * ISO/IEC 6937 is a _mostly_ single-byte character sets, except that
 * the 0xC0-0xCF range of bytes are introducer characters for two-byte
 * encodings of accented letters.
 *
 * You'd be forgiven for mistaking the bytes in the C0-CF range for
 * something more like combining characters, because the two-byte
 * encodings are organised in a very semantic way: each introducer
 * character corresponds to a specific diacritic mark, in the sense
 * that all the two-byte encodings beginning with that introducer byte
 * have an ASCII alphabetic character as their second byte and encode
 * that letter with the given diacritic.
 *
 * But it would be a mistake to consider this to have anything to do
 * with the Unicode combining characters for those diacritics, because
 * (a) the ISO 6937 diacritic bytes are _prefixes_, not combining
 * characters applied afterwards; (b) ISO 6937 specifies an exact list
 * of the permissible second bytes after each introducer; (c) the
 * right translation of one of these two-byte encodings is the single
 * Unicode code point for the accented letter, and not a separate pair
 * of (letter, combining character) code points.
 *
 * So this is better viewed as simply a multibyte _encoding_, just
 * with an unusually mnemonic organisation.
 *
 * Implementation strategy: the single-byte encodings for this charset
 * (or rather, this pair of very similar charsets) are handled by a
 * pair of mapping tables in sbcs.dat, only declared with the 'tables'
 * rather than 'charset' keyword so that sbcsgen.pl doesn't generate
 * the top-level charset_spec. So the read and write functions below
 * can call sbcs_to_unicode and sbcs_from_unicode on those tables just
 * like the ones in sbcs.c.
 *
 * The two-byte pairs are dealt with using the pair of mapping tables
 * below. These are generated by Perl from a minimal amount of
 * starting data that just gives each prefix character along with the
 * corresponding Unicode combining character and the list of letters
 * it's allowed to apply to; the Perl script runs over UnicodeData.txt
 * to achieve the translation of (letter, combining character) pairs
 * to precombined code points.
 */

/*

perl -e '
    while (<<>>) {
        chomp; @_ = split /;/,$_; @d = split / /,$_[5];
        if (2 == @d) {
            ($p, $s, $c) = (hex $d[0], hex $d[1], hex $_[0]);
            $combine{$p,$s} = $c if $p && $s && $c;
        }
    }
    @forward = (" ERROR,") x 0x400;
    for $t ( [0xC1, 0x300, "AEIOUaeiou" ],
             [0xC2, 0x301, "ACEILNORSUYZacegilnorsuyz" ],
             [0xC3, 0x302, "ACEGHIJOSUWYaceghijosuwy" ],
             [0xC4, 0x303, "AINOUainou" ],
             [0xC5, 0x304, "AEIOUaeiou" ],
             [0xC6, 0x306, "AGUagu" ],
             [0xC7, 0x307, "CEGIZcegz" ],
             [0xC8, 0x308, "AEIOUYaeiouy" ],
             [0xCA, 0x30A, "AUau" ],
             [0xCB, 0x327, "CGKLNRSTcklnrst" ],
             [0xCD, 0x30B, "OUou" ],
             [0xCE, 0x328, "AEIUaeiu" ],
             [0xCF, 0x30C, "CDELNRSTZcdelnrstz" ] ) {
        ($prefix, $cc, $letters) = @$t;
        for $letter (unpack "C*", $letters) {
            $cp = $combine{$letter,$cc};
            $offset = ($prefix - 0xC0) * 0x40 + ($letter - 0x40);
            $forward[$offset] = sprintf " 0x%04x,", $cp;
            push @backward, [$cp, (sprintf " %d,", $offset)];
        }
    }
    @backward = map { $_->[1] } sort {$a->[0] <=> $b->[0]} @backward;
    print "static const unsigned short iso6937_2byte_forward[0x400] = {\n";
    $line = "   ";
    for $e (@forward, "sentinel" x 100) {
        if (length($line.$e) > 77) { print "$line\n"; $line = "   "; }
        $line .= $e;
    }
    print "};\n\n";
    $line = "   ";
    print "static const unsigned short iso6937_2byte_backward[] = {\n";
    for $e (@backward, "sentinel" x 100) {
        if (length($line.$e) > 77) { print "$line\n"; $line = "   "; }
        $line .= $e;
    }
    print "};\n\n";
' UnicodeData.txt

 */

static const unsigned short iso6937_2byte_forward[0x400] = {
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, 0x00c0, ERROR, ERROR, ERROR, 0x00c8,
    ERROR, ERROR, ERROR, 0x00cc, ERROR, ERROR, ERROR, ERROR, ERROR, 0x00d2,
    ERROR, ERROR, ERROR, ERROR, ERROR, 0x00d9, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x00e0, ERROR, ERROR,
    ERROR, 0x00e8, ERROR, ERROR, ERROR, 0x00ec, ERROR, ERROR, ERROR, ERROR,
    ERROR, 0x00f2, ERROR, ERROR, ERROR, ERROR, ERROR, 0x00f9, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x00c1,
    ERROR, 0x0106, ERROR, 0x00c9, ERROR, ERROR, ERROR, 0x00cd, ERROR, ERROR,
    0x0139, ERROR, 0x0143, 0x00d3, ERROR, ERROR, 0x0154, 0x015a, ERROR,
    0x00da, ERROR, ERROR, ERROR, 0x00dd, 0x0179, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, 0x00e1, ERROR, 0x0107, ERROR, 0x00e9, ERROR, 0x01f5, ERROR,
    0x00ed, ERROR, ERROR, 0x013a, ERROR, 0x0144, 0x00f3, ERROR, ERROR,
    0x0155, 0x015b, ERROR, 0x00fa, ERROR, ERROR, ERROR, 0x00fd, 0x017a,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x00c2, ERROR, 0x0108, ERROR,
    0x00ca, ERROR, 0x011c, 0x0124, 0x00ce, 0x0134, ERROR, ERROR, ERROR,
    ERROR, 0x00d4, ERROR, ERROR, ERROR, 0x015c, ERROR, 0x00db, ERROR, 0x0174,
    ERROR, 0x0176, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x00e2,
    ERROR, 0x0109, ERROR, 0x00ea, ERROR, 0x011d, 0x0125, 0x00ee, 0x0135,
    ERROR, ERROR, ERROR, ERROR, 0x00f4, ERROR, ERROR, ERROR, 0x015d, ERROR,
    0x00fb, ERROR, 0x0175, ERROR, 0x0177, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, 0x00c3, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    0x0128, ERROR, ERROR, ERROR, ERROR, 0x00d1, 0x00d5, ERROR, ERROR, ERROR,
    ERROR, ERROR, 0x0168, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, 0x00e3, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, 0x0129, ERROR, ERROR, ERROR, ERROR, 0x00f1, 0x00f5, ERROR,
    ERROR, ERROR, ERROR, ERROR, 0x0169, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x0100, ERROR, ERROR, ERROR,
    0x0112, ERROR, ERROR, ERROR, 0x012a, ERROR, ERROR, ERROR, ERROR, ERROR,
    0x014c, ERROR, ERROR, ERROR, ERROR, ERROR, 0x016a, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x0101, ERROR,
    ERROR, ERROR, 0x0113, ERROR, ERROR, ERROR, 0x012b, ERROR, ERROR, ERROR,
    ERROR, ERROR, 0x014d, ERROR, ERROR, ERROR, ERROR, ERROR, 0x016b, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    0x0102, ERROR, ERROR, ERROR, ERROR, ERROR, 0x011e, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    0x016c, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, 0x0103, ERROR, ERROR, ERROR, ERROR, ERROR, 0x011f, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, 0x016d, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x010a, ERROR, 0x0116, ERROR,
    0x0120, ERROR, 0x0130, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x017b,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x010b, ERROR,
    0x0117, ERROR, 0x0121, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, 0x017c, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x00c4, ERROR,
    ERROR, ERROR, 0x00cb, ERROR, ERROR, ERROR, 0x00cf, ERROR, ERROR, ERROR,
    ERROR, ERROR, 0x00d6, ERROR, ERROR, ERROR, ERROR, ERROR, 0x00dc, ERROR,
    ERROR, ERROR, 0x0178, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    0x00e4, ERROR, ERROR, ERROR, 0x00eb, ERROR, ERROR, ERROR, 0x00ef, ERROR,
    ERROR, ERROR, ERROR, ERROR, 0x00f6, ERROR, ERROR, ERROR, ERROR, ERROR,
    0x00fc, ERROR, ERROR, ERROR, 0x00ff, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x00c5, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x016e, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x00e5, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x016f, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, 0x00c7, ERROR, ERROR, ERROR, 0x0122, ERROR, ERROR, ERROR,
    0x0136, 0x013b, ERROR, 0x0145, ERROR, ERROR, ERROR, 0x0156, 0x015e,
    0x0162, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, 0x00e7, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, 0x0137, 0x013c, ERROR, 0x0146, ERROR, ERROR, ERROR,
    0x0157, 0x015f, 0x0163, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, 0x0150, ERROR, ERROR, ERROR, ERROR, ERROR, 0x0170,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, 0x0151, ERROR, ERROR, ERROR, ERROR,
    ERROR, 0x0171, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, 0x0104, ERROR, ERROR, ERROR, 0x0118, ERROR, ERROR,
    ERROR, 0x012e, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, 0x0172, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, 0x0105, ERROR, ERROR, ERROR, 0x0119,
    ERROR, ERROR, ERROR, 0x012f, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, 0x0173, ERROR, ERROR, ERROR, ERROR,
    ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x010c,
    0x010e, 0x011a, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, 0x013d, ERROR,
    0x0147, ERROR, ERROR, ERROR, 0x0158, 0x0160, 0x0164, ERROR, ERROR, ERROR,
    ERROR, ERROR, 0x017d, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    ERROR, 0x010d, 0x010f, 0x011b, ERROR, ERROR, ERROR, ERROR, ERROR, ERROR,
    0x013e, ERROR, 0x0148, ERROR, ERROR, ERROR, 0x0159, 0x0161, 0x0165,
    ERROR, ERROR, ERROR, ERROR, ERROR, 0x017e, ERROR, ERROR, ERROR, ERROR,
    ERROR,
};

static const unsigned short iso6937_2byte_backward[] = {
    65, 129, 193, 257, 513, 641, 707, 69, 133, 197, 517, 73, 137, 201, 521,
    270, 79, 143, 207, 271, 527, 85, 149, 213, 533, 153, 97, 161, 225, 289,
    545, 673, 739, 101, 165, 229, 549, 105, 169, 233, 553, 302, 111, 175,
    239, 303, 559, 117, 181, 245, 565, 185, 569, 321, 353, 385, 417, 897,
    929, 131, 163, 195, 227, 451, 483, 963, 995, 964, 996, 325, 357, 453,
    485, 901, 933, 965, 997, 199, 231, 391, 423, 455, 487, 711, 200, 232,
    265, 297, 329, 361, 905, 937, 457, 202, 234, 715, 747, 140, 172, 716,
    748, 972, 1004, 142, 174, 718, 750, 974, 1006, 335, 367, 847, 879, 146,
    178, 722, 754, 978, 1010, 147, 179, 211, 243, 723, 755, 979, 1011, 724,
    756, 980, 1012, 277, 309, 341, 373, 405, 437, 661, 693, 853, 885, 917,
    949, 215, 247, 217, 249, 537, 154, 186, 474, 506, 986, 1018, 167,
};

/* This returns ERROR if the code point doesn't exist. */
static long int iso6937_2byte_to_unicode(int prefix, int letter)
{
    if (!(prefix >= 0xC0 && prefix < 0xD0 && letter >= 0x40 && letter < 0x80))
        return ERROR;
    return iso6937_2byte_forward[(prefix - 0xC0) * 0x40 + (letter - 0x40)];
}

/* This returns true if it filled in the output values */
static bool iso6937_2byte_from_unicode(long int cp, int *prefix, int *letter)
{
    int lo = -1, hi = lenof(iso6937_2byte_backward);

    while (hi - lo >= 2) {
        int mid = (hi + lo) / 2;
        int midpos = iso6937_2byte_backward[mid];
        long int midcp = iso6937_2byte_forward[midpos];
        if (cp == midcp) {
            *prefix = 0xC0 + (midpos >> 6);
            *letter = 0x40 + (midpos & 0x3F);
            return true;
        } else if (cp < midcp) {
            hi = mid;
        } else {
            lo = mid;
        }
    }
    return false;
}

void read_iso6937(charset_spec const *charset, long int input_chr,
                  charset_state *state,
                  void (*emit)(void *ctx, long int output), void *emitctx)
{
    const sbcs_data *sd = charset->data;

    if (input_chr >= 0xC0 && input_chr < 0xD0) {
        /*
         * Input bytes in the C0-DF region of this encoding are
         * 'combining characters', but not in the Unicode sense of
         * mapping to separate Unicode code points. Instead, they're
         * prefixes which modify a specific set of subsequent printing
         * characters. Stash such a byte in the conversion state to
         * use in the next call.
         */
        if (state->s0) {
            emit(emitctx, ERROR);      /* the previous prefix was erroneous */
        }
        state->s0 = input_chr;
    } else {
        if (state->s0) {
            long int output = iso6937_2byte_to_unicode(state->s0, input_chr);
            emit(emitctx, output);
            state->s0 = 0;

            /*
             * If we've successfully emitted a character, we're done.
             * Otherwise, we'll take the view that the ERROR we've
             * emitted corresponded to _just_ the misplaced prefix
             * byte, so we'll fall through to the emit() below which
             * will output the unmodified followup byte too.
             */
            if (output != ERROR)
                return;
        }

        emit(emitctx, sbcs_to_unicode(sd, input_chr));
    }
}

bool write_iso6937(charset_spec const *charset, long int input_chr,
                   charset_state *state,
                   void (*emit)(void *ctx, long int output), void *emitctx)
{
    const struct sbcs_data *sd = charset->data;
    long int ret;
    int prefix, letter;

    UNUSEDARG(state);

    if (input_chr == -1)
	return true;		       /* stateless; no cleanup required */

    if ((ret = sbcs_from_unicode(sd, input_chr)) != ERROR) {
        emit(emitctx, ret);
        return true;
    } else if (iso6937_2byte_from_unicode(input_chr, &prefix, &letter)) {
        emit(emitctx, prefix);
        emit(emitctx, letter);
        return true;
    } else {
        return false;
    }
}

extern const sbcs_data sbcsdata_ISO6937, sbcsdata_ISO6937_EURO;

const charset_spec charset_CS_ISO6937 = {
    CS_ISO6937, read_iso6937, write_iso6937, &sbcsdata_ISO6937
};
const charset_spec charset_CS_ISO6937_EURO = {
    CS_ISO6937_EURO, read_iso6937, write_iso6937, &sbcsdata_ISO6937_EURO
};

#else /* ENUM_CHARSETS */

ENUM_CHARSET(CS_ISO6937)
ENUM_CHARSET(CS_ISO6937_EURO)

#endif /* ENUM_CHARSETS */
