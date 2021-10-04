/*
 * csshow.c: display a character-set table on a terminal.
 *
 * Can display a selected block of Unicode, or the whole of any
 * single-byte character set for which libcharset knows the
 * translation.
 *
 * Intended mostly for quick-reference use - it might very well be
 * quicker to type 'csshow U+0400' than to click around for ages in a
 * browser finding the appropriate Unicode chart. But it also works
 * well as a test of the specific font you've configured in your
 * terminal window, of course.
 *
 * Possible extra features:
 *  - configurable row length.
 *  - option to disambiguate the various classes of failure in the
 *    output, e.g. if terminfo gives us control sequences to change
 *    colours then we could colour the missing characters differently
 *    depending on why they're missing.
 *     + this mode probably implies that we must also display all
 *       characters in the range, whether printable or not, because
 *       the whole point might be to disambiguate the various causes
 *       of undisplayability. (In particular, don't forget to turn off
 *       the early exit when nothing in the range is printable at
 *       all.)
 */

/*
 * Feature macros I've found necessary to make the standard headers
 * declare wcwidth and snprintf (on various systems).
 */
#define _XOPEN_SOURCE 500
#define _C99_SOURCE

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <ctype.h>

#ifndef HAVE_NO_WCWIDTH
#include <wchar.h>
#endif

#ifndef HAVE_NO_WCTYPE
#include <wctype.h>
#endif

#include "charset.h"

static const char *helptext =
    "usage: csshow CHARSET-NAME [ ENCODING-PREFIX-BYTE... ]\n"
    " e.g.: csshow Win1252       show a whole single-byte charset\n"
    "       csshow Shift-JIS     show all single-byte chars in a "
    "multibyte charset\n"
    "       csshow Shift-JIS 9C  show all chars encoded as 9C xx in "
    "Shift-JIS\n\n"
    "   or: csshow BASE-UNICODE-VALUE [ +RANGE-LENGTH | END-UNICODE-VALUE ]\n"
    " e.g.: csshow U+2500        show 0x100 characters starting at U+2500 "
    "inclusive\n"
    " e.g.: csshow U+2500 +128   show a different number of characters\n"
    " e.g.: csshow U+2500 +0x80  same effect, but you can write the length "
    "in hex\n"
    "       csshow U+2500 U+2580   or specify the (non-inclusive) range "
    "endpoint\n\n"
    " also: csshow --help          display this help text\n"
    ;

static void help(FILE *fp)
{
    fputs(helptext, fp);
}

enum Trans {
    BAD_CHAR_IN_SOURCE_CHARSET,
    BAD_CHAR_IN_OUTPUT_CHARSET,
    UNPRINTABLE_CHAR,
    MULTIBYTE_INTRODUCER,
    FIRST_PRINTABLE_VALUE,
    COMBINING_CHAR = FIRST_PRINTABLE_VALUE,
    WIDE_PRINTABLE_CHAR,
    NORMAL_PRINTABLE_CHAR
};
struct translated_char {
    enum Trans type;
    char buf[7]; /* maximum even theoretical UTF-8 code length, plus NUL */
};

struct buf {
    char *data;
    size_t size, len;
};
static char *buf_add_space(struct buf *buf, size_t space)
{
    char *toret;

    if (buf->size - buf->len < space) {
        buf->size = (buf->len + space) * 5 / 4 + 64;
        buf->data = realloc(buf->data, buf->size);
        if (!buf->data) {
            fprintf(stderr, "csshow: out of memory\n");
            exit(1);
        }
    }

    toret = buf->data + buf->len;
    buf->len += space;
    return toret;
}

static enum Trans try_translate_from_source(
    const char *in, int inlen, int charset, wchar_t *wc_out)
{
    const char *cp;
    int clen, ret0, ret1;

    cp = in;
    clen = inlen;
    ret1 = charset_to_unicode(&cp, &clen, wc_out, 1, charset, NULL, L"?", 1);

    cp = in;
    clen = inlen;
    ret0 = charset_to_unicode(&cp, &clen, wc_out, 1, charset, NULL, L"", 0);

    if (ret0 == 1 && ret1 == 1) {
        /* Successful translation into Unicode */
        return NORMAL_PRINTABLE_CHAR;
    } else if (ret0 == 0 && ret1 == 0) {
        /* No output, even _with_ a replacement character
         * defined for bad chars, means the input
         * character has been absorbed into the charset
         * state but not _yet_ generated any output or
         * discovered an error. In other words, this is a
         * multibyte introducer. */
        return MULTIBYTE_INTRODUCER;
    } else {
        return BAD_CHAR_IN_SOURCE_CHARSET;
    }
}

int main(int argc, char **argv)
{
    bool doing_opts = true;
    int source_charset = CS_ASCII, output_charset = CS_NONE;
    unsigned long base = 0, size = 0x100, rowlen = 0x10;
    struct buf prefix = { NULL, 0, 0 };
    enum ArgsState {
        AS_INITIAL,
        AS_UNICODE_ENDRANGE,
        AS_MBCS_PREFIX,
        AS_DONE
    } args_state = AS_INITIAL;

    while (--argc > 0) {
        const char *p = *++argv;
        if (*p == '-' && doing_opts) {
            if (!strcmp(p, "--")) {
                doing_opts = false;
            } else if (!strcmp(p, "--help")) {
                help(stdout);
                return 0;
            } else {
                fprintf(stderr, "csshow: unrecognised option '%s'\n", p);
                return 1;
            }
        } else if (args_state == AS_INITIAL) {
            /*
             * First argument can be a Unicode code point or a
             * single-byte charset name.
             */

            int cs;

            if (toupper((unsigned char)p[0]) == 'U' &&
                (p[1] == '-' || p[1] == '+')) {
                source_charset = CS_NONE; /* means just translate Unicode */
                base = strtoul(p+2, NULL, 16);
                args_state = AS_UNICODE_ENDRANGE;
            } else if ((cs = charset_from_localenc(p)) != CS_NONE) {
                source_charset = cs;
                base = 0;
                args_state = AS_MBCS_PREFIX;
            } else {
                fprintf(stderr, "csshow: unrecognised argument '%s'\n", p);
                return 1;
            }
        } else if (args_state == AS_UNICODE_ENDRANGE) {
            /*
             * If the first argument was a Unicode code point, then
             * the next argument is taken to be an end point for the
             * range, so that you can print larger ranges than 256
             * characters.
             */

            if (toupper((unsigned char)p[0]) == 'U' &&
                (p[1] == '-' || p[1] == '+')) {
                /* U+XXXX / U-XXXXXXXX specify the end code point of
                 * the range. (Exclusive.) */
                size = strtoul(p+2, NULL, 16) - base;
            } else if (p[0] == '+') {
                /* +NNNN specifies the size of the range. We use
                 * strtoul in base 0 so that decimal or 0xHEX are both
                 * accepted. */
                size = strtoul(p+1, NULL, 0);
            }

            /* No further arguments expected. */
            args_state = AS_DONE;

        } else if (args_state == AS_MBCS_PREFIX) {
            /*
             * If the first argument was a charset name, then further
             * arguments are taken to be hex byte values to accumulate
             * into an encoding prefix. This allows you to say, for
             * example, 'csshow Shift-JIS 89' to see the slice of the
             * Shift-JIS encoding consisting of characters whose first
             * encoding byte is 0x89, indexed by their second byte.
             */

            *buf_add_space(&prefix, 1) = strtoul(p, NULL, 16);
        } else {
            fprintf(stderr, "csshow: extra argument '%s' unexpected\n", p);
            return 1;
        }
    }

#ifndef HAVE_NO_WCTYPE
    setlocale(LC_CTYPE, "");
#endif

    if (output_charset == CS_NONE)
        output_charset = charset_from_locale();

    {
        struct translated_char *trans;
        const char *rowheadfmt;
        int rowheadwidth, colwidth;
        bool printed_a_line, skipped_a_line;
        unsigned long i, j;
        enum Trans transret;
        char *suffix_position = NULL;
        wchar_t wc;

        if (source_charset != CS_NONE) {
            /*
             * First, check that the prefix doesn't already form a
             * completed character or an error.
             */
            transret = try_translate_from_source(
                prefix.data, prefix.len, source_charset, &wc);
            if (transret == BAD_CHAR_IN_SOURCE_CHARSET) {
                fprintf(stderr, "csshow: prefix sequence is not valid\n");
                return 1;
            } else if (transret != MULTIBYTE_INTRODUCER) {
                fprintf(stderr, "csshow: prefix sequence generates output\n");
                return 1;
            }

            /*
             * Make space in the prefix buffer to put each test byte on
             * the end.
             */
            suffix_position = buf_add_space(&prefix, 1);
        }

        trans = malloc(size * sizeof(struct translated_char));
        if (!trans) {
            fprintf(stderr, "csshow: out of memory\n");
            return 1;
        }

        /*
         * Initial loop figuring out what characters we have in our
         * block, and in what way each of them is weird.
         */
        for (i = 0; i < size; i++) {
            unsigned long charcode = base + i;

            trans[i].buf[0] = '\0';

            if (source_charset == CS_NONE) {
                wc = charcode;
            } else {
                *suffix_position = charcode;
                transret = try_translate_from_source(
                    prefix.data, prefix.len, source_charset, &wc);

                if (transret != NORMAL_PRINTABLE_CHAR) {
                    trans[i].type = transret;
                    continue;
                }
            }

            {
                const wchar_t *wcp = &wc;
                int wclen = 1;
                bool error = false;

                int ret = charset_from_unicode(
                    &wcp, &wclen, trans[i].buf, sizeof(trans[i].buf) - 1,
                    output_charset, NULL, &error);

                assert(0 <= ret);
                assert((size_t)ret < sizeof(trans[i].buf));
                trans[i].buf[ret] = '\0';

                if (wclen != 0 || ret == 0 || error) {
                    trans[i].type = BAD_CHAR_IN_OUTPUT_CHARSET;
                    trans[i].buf[0] = '\0';
                    continue;
                }
            }

            /*
             * OK, we have a Unicode character and a corresponding
             * UTF-8 sequence. But it might still be something we have
             * to take care over printing.
             */
#ifndef HAVE_NO_WCTYPE
            if (!iswprint(wc)) {
                trans[i].type = UNPRINTABLE_CHAR;
                trans[i].buf[0] = '\0';
                continue;
            }
#endif
            {
#ifndef HAVE_NO_WCWIDTH
                int width = wcwidth(wc);
#else
                int width = 1;
#endif

                switch (width) {
                  case 0:
                    trans[i].type = COMBINING_CHAR;
                    break;
                  case 1:
                    trans[i].type = NORMAL_PRINTABLE_CHAR;
                    break;
                  case 2:
                    trans[i].type = WIDE_PRINTABLE_CHAR;
                    break;
                  default:
                    /* If we somehow had wcwidth but not wctype, weird
                     * returns from wcwidth give us a second way to
                     * identify non-printable control characters. */
                    trans[i].type = UNPRINTABLE_CHAR;
                    trans[i].buf[0] = '\0';
                    break;
                }
            }
        }

        /*
         * Special case: if _nothing_ in our range is printable, we'll
         * just terminate now.
         */
        for (i = 0; i < size; i++)
            if (trans[i].type >= FIRST_PRINTABLE_VALUE)
                break;
        if (i == size) {
            fprintf(stderr, "csshow: nothing printable at all in this"
                    " character range\n");
            return 1;
        }

        /*
         * Now we can figure out whether there are any wide
         * characters, in which case we should space out our table a
         * bit more. (We might also have to do that if rowlen is
         * large.)
         */
        {
            char testbuf[64];
            colwidth = snprintf(testbuf, sizeof(testbuf),
                                "%-2x", (unsigned)(rowlen-1));
        }
        if (colwidth < 3) {
            for (i = 0; i < size; i++)
                if (trans[i].type == WIDE_PRINTABLE_CHAR)
                    colwidth = 3;
        }

        /*
         * Work out the width of the heading column on the left.
         */
        if (source_charset == CS_NONE) {
            rowheadfmt = "U+%-6.4X";
        } else {
            rowheadfmt = "%-4.2X";
        }
        {
            char testbuf[64];
            rowheadwidth = snprintf(testbuf, sizeof(testbuf),
                                    rowheadfmt, (unsigned)(base + (size-1)));
        }

        /* Heading line. */
        printf("%*s", rowheadwidth, "");
        for (i = 0; i < rowlen; i++)
            printf("%-*X", colwidth, (unsigned)i);
        printf("\n");

        printed_a_line = false;
        skipped_a_line = false;

        for (j = 0; j < size; j += rowlen) {
            /* See if we're skipping this row completely. */
            bool skip = true;
            for (i = 0; i < rowlen && j+i < size; i++)
                if (trans[j+i].type >= FIRST_PRINTABLE_VALUE)
                    skip = false;
            if (skip) {
                skipped_a_line = true;
                continue;
            }

            /* We're printing this line, but we might need to print a
             * blank line to indicate a previous skipped one. But we
             * don't do that at the very start or end - we only
             * indicate a skipped line between two that _were_
             * printed. */
            if (skipped_a_line && printed_a_line) {
                printf("\n");
            }
            skipped_a_line = false;

            printed_a_line = true;
            printf(rowheadfmt, (unsigned)(base + j));;
            for (i = 0; i < rowlen && j+i < size; i++) {
                int chars_left = colwidth;
                struct translated_char *chr = &trans[j+i];

                switch (chr->type) {
                  case COMBINING_CHAR:
                    /* Print a space first, for the combining char to
                     * safely combine with. */
                    printf(" %s", chr->buf);
                    chars_left--;
                    break;
                  case WIDE_PRINTABLE_CHAR:
                    fputs(chr->buf, stdout);
                    chars_left -= 2;
                    break;
                  case NORMAL_PRINTABLE_CHAR:
                    fputs(chr->buf, stdout);
                    chars_left--;
                    break;
                  default:
                    /* Unprintable for one reason or another. */
                    break;
                }

                if (i+1 < rowlen && j+i+1 < size)
                    printf("%*s", chars_left, "");
            }
            printf("\n");
        }
    }

    return 0;
}
