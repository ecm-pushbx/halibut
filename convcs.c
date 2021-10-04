/*
 * convcs.c - general libcharset test/demo program which converts
 * between two arbitrary charsets.
 */

/*
 * Possible further features:
 *
 *  - charset override for HTML mode, of the form 'use the charset *I*
 *    specify for the source translation, even if the <meta> tag says
 *    something else - but then overwrite <meta> in any case to
 *    specify the destination charset'.
 *
 *     + Perhaps a nice approach would be an option of the form
 *       '--html-override IFSRC:THENSRC' to force an override to
 *       THENSRC *if* the charset specified in <meta> is IFSRC,
 *       otherwise to leave it alone. (Rationale: I expect this to be
 *       useful in some situation where a particular charset is being
 *       mislabelled, e.g. Win1252 or Mac Pirard as ISO8859-1; so if
 *       you have a large collection of HTML files only some of which
 *       are in the mislabelled charset, you can safely convert the
 *       whole lot using this option.)
 *
 *  - an option to spot UTF-16 BOMs (and perhaps also the UTF-8 'BOM'
 *    encoding) and use those as a charset override. (There's probably
 *    an existing recommendation for how that should interact with
 *    HTML <meta> tags, if both options are enabled at once, which
 *    seems quite plausible.)
 *
 *  - for HTML mode, an option to suppress rewriting of the <meta>
 *    tag.
 */

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <locale.h>
#include "charset.h"

#define lenof(x) ( sizeof((x)) / sizeof(*(x)) )

static int convert(FILE *fp);
static void convert_got_data(const char *inbuf, int inlen);
static void convert_done(void);

static const char *helptext =
    "usage:   convcs [options] SOURCE-CHARSET DESTINATION-CHARSET "
        "[INPUT-FILE]\n"
    " note:   SOURCE-CHARSET and DESTINATION-CHARSET can be '-' to indicate\n"
    "           the system locale's charset\n"
    " e.g.:   convcs Win1252 UTF-8               [reads from standard input]\n"
    "         convcs EUC-JP Shift-JIS file.txt   [reads from test.txt]\n"
    "options: --replacement=TEXT     use TEXT for untranslatable characters\n"
    "         --html                 treat input as HTML: if a <meta> tag specifies\n"
    "                                  a charset, use it to override SOURCE-CHARSET,\n"
    "                                  and rewrite the tag to DESTINATION-CHARSET\n"
    "   also: convcs --help          display this help text\n"
    ;

static void help(FILE *fp)
{
    fputs(helptext, fp);
}

bool match_long_opt(const char *argument, const char *optname,
                    const char **val)
{
    size_t optlen = strlen(optname);
    if (strcspn(argument, "=") != optlen)
        return false;              /* not the right length to match */
    if (memcmp(argument, optname, optlen) != 0)
        return false;              /* doesn't match the leading text */
    if (argument[optlen])
        *val = argument + optlen + 1;
    else
        *val = NULL;
    return true;
}

static int srcset = CS_NONE;
static int dstset = CS_NONE;
static bool html_mode = false;
static const wchar_t *replacement_cooked = NULL;
static int replacement_cooked_len = 0;

int main(int argc, char **argv)
{
    bool doing_opts = true;
    int localeset;
    const char *replacement_raw = NULL;
    const char *inptr;
    int inlen;
    const char *infilename = NULL;

    setlocale(LC_CTYPE, "");
    localeset = charset_from_locale();

    while (--argc > 0) {
        const char *p = *++argv;
        const char *v;
        if (*p == '-' && p[1] && doing_opts) {
            if (!strcmp(p, "--")) {
                doing_opts = false;
            } else if (match_long_opt(p, "--help", &v)) {
                help(stdout);
                return 0;
            } else if (match_long_opt(p, "--replacement", &v)) {
                if (!v) {
                    if (--argc > 0) {
                        v = *++argv;
                    } else {
                        fprintf(stderr, "convcs: option '%s' expects an "
                                "argument\n", p);
                        return 1;
                    }
                }
                replacement_raw = v;
            } else if (match_long_opt(p, "--html", &v)) {
                html_mode = true;
            } else {
                fprintf(stderr, "convcs: unrecognised option '%s'\n", p);
                return 1;
            }
        } else {
            if (srcset == CS_NONE) {
                srcset = !strcmp(p, "-")? localeset : charset_from_localenc(p);
                if (srcset == CS_NONE) {
                    fprintf(stderr, "convcs: unrecognised source charset "
                            "'%s'\n", p);
                    return 1;
                }
            } else if (dstset == CS_NONE) {
                dstset = !strcmp(p, "-")? localeset : charset_from_localenc(p);
                if (dstset == CS_NONE) {
                    fprintf(stderr, "convcs: unrecognised destination charset "
                            "'%s'\n", p);
                    return 1;
                }
            } else if (!infilename) {
                infilename = p;
            } else {
                fprintf(stderr, "convcs: unexpected extra argument '%s'\n", p);
                return 1;
            }
        }
    }

    if (replacement_raw) {
        wchar_t *repl;

        inptr = replacement_raw;
        inlen = strlen(replacement_raw);
        replacement_cooked_len = charset_to_unicode(
            &inptr, &inlen, NULL, -1, localeset, NULL, NULL, 0);

        replacement_cooked = repl =
            malloc(replacement_cooked_len * sizeof(wchar_t));
        if (!replacement_cooked) {
            fprintf(stderr, "convcs: out of memory\n");
            return 1;
        }

        inptr = replacement_raw;
        inlen = strlen(replacement_raw);
        replacement_cooked_len = charset_to_unicode(
            &inptr, &inlen, repl, replacement_cooked_len,
            localeset, NULL, NULL, 0);
    }

    if (srcset == CS_NONE) {
	fprintf(stderr, "convcs: expected a source charset; try '--help'\n");
	return 1;
    }
    if (dstset == CS_NONE) {
	fprintf(stderr, "convcs: expected a destination charset;"
                " try '--help'\n");
	return 1;
    }

    if (infilename) {
        FILE *infile;
        int toreturn;

        infile = fopen(infilename, "rb");
        if (!infile) {
            fprintf(stderr, "convcs: could not open input file '%s'\n",
                    infilename);
            return 1;
        }
        toreturn = convert(infile);
        fclose(infile);
        return toreturn;
    } else {
        return convert(stdin);
    }
}

static int convert(FILE *infile)
{
    char inbuf[1024];
    int rdret;

    if (html_mode) {
        int html_srcset;
        size_t namepos, namelen;

	rdret = fread(inbuf, 1, sizeof(inbuf), infile);

	if (rdret <= 0)
            return 0;                  /* EOF */

        html_srcset = charset_from_html_prefix(
            inbuf, rdret, &namepos, &namelen);
        if (html_srcset != CS_NONE) {
            const char *output_cs_name = charset_to_mimeenc(dstset);
            srcset = html_srcset;
            assert(namepos + namelen <= (size_t)rdret);
            convert_got_data(inbuf, namepos);
            convert_got_data(output_cs_name, strlen(output_cs_name));
            convert_got_data(inbuf + namepos + namelen,
                             rdret - namepos - namelen);
        } else {
            convert_got_data(inbuf, rdret);
        }
    }

    bool eof = false;
    while (!eof) {
        /*
         * Manual loop on getc which has the feature of fgets that we
         * stop if we see a newline (so that when convcs is run
         * interactively in a terminal it will deliver each translated
         * line promptly), but also has the feature of fread that it
         * provides the correct buffer length even in the face of NUL
         * bytes in the input.
         */

        size_t nread = 0;
        while (nread < lenof(inbuf)) {
            int c = getc(infile);
            if (c == EOF) {
                eof = true;
                break;
            }
            inbuf[nread++] = c;
            if (c == '\n')
                break;
        }

        if (nread)
            convert_got_data(inbuf, nread);
    }
    convert_done();
    return 0;
}

static charset_state instate = CHARSET_INIT_STATE;
static charset_state outstate = CHARSET_INIT_STATE;

static void convert_got_data(const char *inbuf, int inlen)
{
    char outbuf[256];
    wchar_t midbuf[256];
    const char *inptr;
    const wchar_t *midptr;
    int midlen, inret, midret;

    if (!inlen)
        return;

    inptr = inbuf;

    /* When converting to Unicode, pass in 'replacement_cooked' so
     * that libcharset will substitute it for anything
     * untranslatable in the source charset. */
    while ( (inret = charset_to_unicode(&inptr, &inlen, midbuf,
                                        lenof(midbuf), srcset,
                                        &instate, replacement_cooked,
                                        replacement_cooked_len)) > 0) {
        bool error;

        midlen = inret;
        midptr = midbuf;

        /* When converting _from_ Unicode, respond to encoding
         * errors by translating 'replacement_cooked' to the
         * destination charset and emitting it in place of each
         * Unicode character without a representation in the
         * destination charset. */
        while ( (midret = charset_from_unicode(&midptr, &midlen, outbuf,
                                               lenof(outbuf), dstset,
                                               &outstate, &error)) > 0 ||
                error) {
            fwrite(outbuf, 1, midret, stdout);
            if (error) {
                const wchar_t *repl_ptr = replacement_cooked;
                int repl_len = replacement_cooked_len;

                /* When translating the replacement string itself,
                 * we don't ask for error reports from
                 * charset_from_unicode, because that would just
                 * get too recursively confusing! */
                while ( (midret = charset_from_unicode(
                             &repl_ptr, &repl_len, outbuf, lenof(outbuf),
                             dstset, &outstate, NULL)) > 0) {
                    fwrite(outbuf, 1, midret, stdout);
                }

                /* Now advance past the char that caused the error. */
                midptr++;
                midlen--;
            }
        }
    }
}

static void convert_done(void)
{
    char outbuf[256];
    int midret;

    /*
     * Reset encoding state.
     */
    while ( (midret = charset_from_unicode(NULL, NULL, outbuf,
					   lenof(outbuf), dstset,
					   &outstate, NULL)) > 0) {
	fwrite(outbuf, 1, midret, stdout);
    }
}
