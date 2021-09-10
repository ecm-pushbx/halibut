/*
 * htmlcs.c - implement the charset_from_html_prefix function.
 *
 * Uses, hopefully exactly, the algorithm described at
 * https://html.spec.whatwg.org/multipage/syntax.html#prescan-a-byte-stream-to-determine-its-encoding
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>

#include "charset.h"

struct file {
    const char *data;      /* data from start of stream */
    size_t pos;            /* how many bytes in data have we stepped past */
    size_t len;            /* how many bytes in data are meaningful */
    size_t cs_start;       /* start pos of charset name, if we've found one */
    size_t cs_len;         /* length of charset name, if we've found one */
};

static void file_init(struct file *f, const void *data, size_t len)
{
    f->data = data;
    f->pos = 0;
    f->len = len;
    f->cs_start = f->cs_len = 0;
}

static bool looking_at(struct file *f, ...)
{
    size_t ourpos;
    va_list ap;
    bool to_return;

    ourpos = f->pos;
    va_start(ap, f);
    while (1) {
        const char *validchars = va_arg(ap, const char *);
        if (!*validchars) {
            to_return = true;
            break;
        }
        if (ourpos == f->len) {
            to_return = false;
            break;
        }
        if (!strchr(validchars, f->data[ourpos])) {
            to_return = false;
            break;
        }
        ourpos++;
    }
    va_end(ap);

    return to_return;
}

static bool looking_at_eof(struct file *f)
{
    if (f->pos < f->len)
        return false;
    return true;
}

static char this_char(struct file *f)
{
    assert(f->pos < f->len);
    return f->data[f->pos];
}

static bool advance(struct file *f)
{
    if (f->pos == f->len)
        return false;
    f->pos++;
    return true;
}

static bool string_match(const char *a, const char *b)
{
    while (1) {
        char ac = *a++, bc = *b++;

        if (ac >= 'A' && ac <= 'Z')
            ac += 0x20;
        if (bc >= 'A' && bc <= 'Z')
            bc += 0x20;

        if (ac != bc)
            return false;

        if (!ac)
            return true;
    }
}

static int get_charset_from(const char *string)
{
    int charset;

    if (string_match(string, "x-user-defined"))
        return CS_CP1252;
    charset = charset_from_mimeenc(string);
    if (charset == CS_UTF16 || charset == CS_UTF16BE || charset == CS_UTF16LE)
        charset = CS_UTF8;
    return charset;
}

#define WHITESPACE " \t\n\f\r"
#define UPPER "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
#define LOWER "abcdefghijklmnopqrstuvwxyz"
#define LETTER UPPER LOWER

struct attr_str {
    /* Fixed-size buffer large enough to hold any attribute name or
     * value that we will translate into anything other than
     * 'unrecognised'. */
    char str[128];
    int len;

    size_t filepos; /* starting location in the file we got it from */
};

static void attr_str_init(struct attr_str *as)
{
    if (!as)
        return;

    as->len = 0;
    as->str[as->len] = '\0';
}

static void attr_str_append_file_char(struct attr_str *as,
                                      struct file *f, bool lowercase)
{
    char c;

    if (!as)
        return;

    if (as->len == sizeof(as->str)-1 ||  /* too long */
        as->str[0] == 0x7F ||            /* string already marked invalid */
        looking_at_eof(f) ||             /* end of file */
        (unsigned char)(c = this_char(f)) >= 0x80) /* non-ASCII character */
    {
        /* Set the string to something that definitely won't match
         * anything we will want to compare it against. */
        as->str[0] = 0x7F;
        as->str[1] = '\0';
        as->len = 1;
    } else {
        if (lowercase && strchr(UPPER, c))
            c += 0x20;

        if (as->len == 0) {
            as->filepos = f->pos;
        } else {
            assert(as->filepos + as->len == f->pos);
        }

        as->str[as->len++] = c;
        as->str[as->len] = '\0';
    }
}

static int get_charset_from_meta(struct file *f)
{
    const char *end;
    bool end_mandatory;
    struct attr_str csname;
    int i;

    while (!looking_at(f, "Cc", "Hh", "Aa", "Rr", "Ss", "Ee", "Tt", "")) {
        if (!advance(f))
            return CS_NONE;
    }
    for (i = 0; i < 7; i++) {
        /* Skip past the word "charset" */
        if (!advance(f))
            assert(0 && "Should still be in the word 'charset'!");
    }

    while (looking_at(f, WHITESPACE, "")) {
        if (!advance(f))
            assert(0 && "But there's whitespace ahead!");
    }

    if (!looking_at(f, "=", ""))
        return CS_NONE;
    if (!advance(f))
        assert(0 && "But we just saw an = ahead of us!");

    while (looking_at(f, WHITESPACE, "")) {
        if (!advance(f))
            assert(0 && "But there's whitespace ahead!");
    }

    if (looking_at(f, "\"'", "")) {
        end = looking_at(f, "\"", "") ? "\"" : "'";
        end_mandatory = true;
        if (!advance(f))
            assert(0 && "But we just saw a quote character!");
    } else {
        end = WHITESPACE ";";
        end_mandatory = false;
    }

    attr_str_init(&csname);
    while (1) {
        if (looking_at(f, end, ""))
            break;
        if (looking_at_eof(f)) {
            if (end_mandatory)
                return CS_NONE;
            else
                break;
        }

        attr_str_append_file_char(&csname, f, false);
        if (!advance(f))
            assert(0 && "But we just found we weren't looking at EOF!");
    }

    f->cs_start = csname.filepos;
    f->cs_len = csname.len;
    return get_charset_from(csname.str);
}

static bool get_attribute(struct file *f, struct attr_str *attr_name,
                          struct attr_str *attr_value)
{
    bool attr_name_nonempty;

    while (looking_at(f, WHITESPACE "/", "")) {
        if (!advance(f))
            assert(0 && "But there's whitespace (or a slash) ahead!");
    }
    if (looking_at(f, ">", ""))
        return false;
    attr_str_init(attr_name);
    attr_str_init(attr_value);
    attr_name_nonempty = false;
    while (1) {
        if (looking_at(f, WHITESPACE "=", "") && attr_name_nonempty) {
            break;
        } else if (looking_at(f, "/>", "")) {
            return true;
        } else {
            attr_str_append_file_char(attr_name, f, false);
            attr_name_nonempty = true;
        }

        if (!advance(f))
            return false;
    }

    while (looking_at(f, WHITESPACE, "")) {
        if (!advance(f))
            assert(0 && "But there's whitespace ahead!");
    }
    if (!looking_at(f, "=", ""))
        return true;
    if (!advance(f))
        assert(0 && "But we just saw an = ahead!");
    while (looking_at(f, WHITESPACE, "")) {
        if (!advance(f))
            assert(0 && "But there's whitespace ahead!");
    }

    if (looking_at(f, "\"'", "")) {
        const char *close_quote = looking_at(f, "\"", "") ? "\"" : "'";;

        while (1) {
            if (!advance(f))
                return false;

            if (looking_at(f, close_quote, "")) {
                if (!advance(f))
                    assert(0 && "But we just saw a closing quote!");
                return true;
            }

            attr_str_append_file_char(attr_value, f, true);
        }
    } else if (looking_at(f, ">", "")) {
        return true;
    } else {
        attr_str_append_file_char(attr_value, f, true);
        if (!advance(f))
            return true;
    }

    while (1) {
        if (looking_at(f, WHITESPACE ">", "")) {
            return true;
        } else {
            attr_str_append_file_char(attr_value, f, true);
        }

        if (!advance(f))
            return true;
    }
}

static int get_charset(struct file *f)
{
    while (1) {
        if (looking_at(f, "<", "!", "-", "-", "")) {
            while (!looking_at(f, "-", "-", ">", "")) {
                if (!advance(f))
                    return CS_NONE;
            }
        } else if (looking_at(f, "<", "Mm", "Ee", "Tt", "Aa",
                              WHITESPACE "/", "")) {
            struct attr_str attr_name, attr_value;
            int attributes_seen_mask = 0;
            bool got_pragma = false;
            enum { NP_NULL, NP_FALSE, NP_TRUE } need_pragma = NP_NULL;
            int charset = CS_NONE;

            while (!looking_at(f, WHITESPACE "/", "")) {
                if (!advance(f))
                    return CS_NONE;
            }

            while (get_attribute(f, &attr_name, &attr_value)) {
                enum { HTTP_EQUIV, CONTENT, CHARSET } attr;

                if (string_match(attr_name.str, "http-equiv")) {
                    attr = HTTP_EQUIV;
                } else if (string_match(attr_name.str, "content")) {
                    attr = CONTENT;
                } else if (string_match(attr_name.str, "charset")) {
                    attr = CHARSET;
                } else {
                    continue; /* ignore attributes we don't recognise */
                }

                if (attributes_seen_mask & (1 << attr))
                    continue; /* ignore attributes we've already seen one of */
                attributes_seen_mask |= (1 << attr);

                switch (attr) {
                  case HTTP_EQUIV:
                    if (string_match(attr_value.str, "content-type"))
                        got_pragma = true;
                    break;
                  case CONTENT:
                    {
                        struct file fvalue;
                        int new_charset;
                        file_init(&fvalue, attr_value.str, attr_value.len);
                        new_charset = get_charset_from_meta(&fvalue);
                        if (charset == CS_NONE && new_charset != CS_NONE) {
                            charset = new_charset;
                            f->cs_start = attr_value.filepos + fvalue.cs_start;
                            f->cs_len = fvalue.cs_len;
                            need_pragma = NP_TRUE;
                        }
                    }
                    break;
                  case CHARSET:
                    charset = get_charset_from(attr_value.str);
                    f->cs_start = attr_value.filepos;
                    f->cs_len = attr_value.len;
                    need_pragma = NP_FALSE;
                }
            }
            if (need_pragma == NP_NULL)
                continue;
            if (need_pragma == NP_TRUE && !got_pragma)
                continue;
            if (charset == CS_NONE)
                continue;
            return charset;
        } else if (looking_at(f, "<", LETTER, "") ||
                   looking_at(f, "<", "/", LETTER, "")) {
            while (!looking_at(f, WHITESPACE ">", "")) {
                if (!advance(f))
                    return CS_NONE;
            }
            while (get_attribute(f, NULL, NULL))
                /* do nothing */;
        } else if (looking_at(f, "<", "!/?", "")) {
            while (!looking_at(f, ">", "")) {
                if (!advance(f))
                    return CS_NONE;
            }
        } else {
            if (!advance(f))
                return CS_NONE;
        }
    }
}

int charset_from_html_prefix(const char *data, size_t len,
                             size_t *namepos, size_t *namelen)
{
    struct file f;
    int cs;

    file_init(&f, data, len);
    cs = get_charset(&f);

    if (cs != CS_NONE) {
        if (namepos)
            *namepos = f.cs_start;
        if (namelen)
            *namelen = f.cs_len;
    }

    return cs;
}

#ifdef TEST_HTMLCS

/*
gcc -DTEST_HTMLCS -g -O0 -o htmlcs htmlcs.c libcharset.a
*/

struct testcase {
    const char *string;
    size_t len;
    int expected_charset;
    size_t expected_namepos, expected_namelen;
    int line;
};

#define TESTCASE_POS(before, name, after, charset) \
    { before name after, sizeof(before name after)-1, \
            charset, sizeof(before)-1, sizeof(name)-1, __LINE__ }
#define TESTCASE_NEG(string) \
    { string, sizeof(string)-1, CS_NONE, 0, 0, __LINE__ }

const struct testcase testcases[] = {
    TESTCASE_POS("<html> <head> <meta http-equiv=\"content-type\" content=\"text/html; charset=", "US-ASCII", "\">", CS_ASCII),
    TESTCASE_POS("<meta http-equiv=\"content-type\" content=\"text/html; charset=", "US-ASCII", "\"/>", CS_ASCII),
    TESTCASE_NEG("<meta http-equiv=\"content-type\" content=\"text/html; charset=US-ASCII\'>"), /* mismatched quotes around content */
    TESTCASE_NEG("<!--<meta http-equiv=\"content-type\" content=\"text/html; charset=US-ASCII\">-->"), /* meta tag is commented out */
    TESTCASE_NEG("<tag attr='<meta http-equiv=\"content-type\" content=\"text/html; charset=US-ASCII\">'>"), /* meta tag is inside another tag's attribute! */
    TESTCASE_NEG("</tag attr='<meta http-equiv=\"content-type\" content=\"text/html; charset=US-ASCII\">'>"), /* even closing tags count as having attributes */
    TESTCASE_NEG("<meta content=\"text/html; charset=US-ASCII\">"), /* missing http-equiv when it is mandatory */
    TESTCASE_NEG("<meta http-equiv=dummy http-equiv=\"content-type\" content=\"text/html; charset=US-ASCII\">"), /* only the first http-equiv attribute is considered */
    TESTCASE_POS("<!--<meta http-equiv=\"content-type\" content=\"text/html; charset=US-ASCII\">--><meta http-equiv=\"content-type\" content=\"text/html; charset=", "ISO8859-1", "\">", CS_ISO8859_1),
    TESTCASE_POS("<meta charset=\"", "ISO8859-1", "\">", CS_ISO8859_1),
    TESTCASE_POS("<meta charset=\"", "ISO8859-1", "\"/>", CS_ISO8859_1),
    TESTCASE_POS("<meta charset='", "ISO8859-1", "'/>", CS_ISO8859_1),
    TESTCASE_POS("<meta charset=", "ISO8859-1", " />", CS_ISO8859_1),
    TESTCASE_POS("<meta charset=", "ISO8859-1", ">", CS_ISO8859_1),
    TESTCASE_POS("<meta charset = \"", "ISO8859-1", "\">", CS_ISO8859_1),
    TESTCASE_POS("<meta charset = ", "ISO8859-1", ">", CS_ISO8859_1),
    TESTCASE_NEG("<meta charset = \" ISO8859-1\">"), /* spaces between = and attribute value must be outside quotes, if there are quotes */
    TESTCASE_NEG("<meta charset=ISO8859-1/>"), /* an unquoted attribute must be terminated by space or >, not / */
    TESTCASE_POS("<meta http-equiv=\"content-type\" charset=\"", "ISO8859-1", "\"/>", CS_ISO8859_1),
    TESTCASE_POS("<META Charset=\"", "ISO8859-1", "\"/>", CS_ISO8859_1),
    TESTCASE_POS("<meta HTTP-Equiv=\"Content-Type\" conTent=\"text/html; chArset=", "us-Ascii", "\">", CS_ASCII),
    TESTCASE_POS("<meta charset='", "x-user-defined", "'/>", CS_CP1252),
    TESTCASE_POS("<meta charset='", "utf-16", "'/>", CS_UTF8),
    TESTCASE_POS("<meta charset='", "utf-16be", "'/>", CS_UTF8),
    TESTCASE_POS("<meta charset='", "utf-16le", "'/>", CS_UTF8),
    TESTCASE_NEG("<meta charset='****************************************************************************************************************************************************************************************************************************************************************'/>"), /* unrecognised string long enough to overflow the buffer in attr_str */
};

#define lenof(x) ( sizeof((x)) / sizeof(*(x)) )

int run_unit_tests(void)
{
    size_t i;
    int npass, nfail;

    npass = nfail = 0;
    for (i = 0; i < lenof(testcases); i++) {
        const struct testcase *tc = &testcases[i];
        int cs;
        size_t namepos, namelen;

        cs = charset_from_html_prefix(tc->string, tc->len, &namepos, &namelen);
        if (cs != tc->expected_charset) {
            printf("%s:%d: expected charset %s, got %s\n",
                   __FILE__, tc->line,
                   charset_to_localenc(tc->expected_charset),
                   charset_to_localenc(cs));
            nfail++;
            continue;
        }
        if (cs != CS_NONE) {
            if (namepos != tc->expected_namepos) {
                printf("%s:%d: expected namepos %d, got %d\n",
                       __FILE__, tc->line,
                       (int)tc->expected_namepos, (int)namepos);
                nfail++;
                continue;
            }
            if (namelen != tc->expected_namelen) {
                printf("%s:%d: expected namelen %d, got %d\n",
                       __FILE__, tc->line,
                       (int)tc->expected_namelen, (int)namelen);
                nfail++;
                continue;
            }
        }
        npass++;
    }

    printf("passed %d failed %d total %d\n", npass, nfail, npass+nfail);
    return nfail != 0;
}

void test_file(FILE *fp)
{
    char buf[1024];
    size_t got;
    int cs;
    size_t namepos, namelen;

    got = fread(buf, 1, 1024, fp);
    cs = charset_from_html_prefix(buf, got, &namepos, &namelen);

    if (cs == CS_NONE)
        printf("No charset\n");
    else
        printf("Got charset %s (at %d, len %d)\n",
               charset_to_localenc(cs), (int)namepos, (int)namelen);
}

int main(int argc, char **argv)
{
    if (argc <= 1) {
        return run_unit_tests();
    } else {
        FILE *fp = fopen(argv[1], "rb");
        if (!fp) {
            perror(argv[1]);
            exit(1);
        }
        test_file(fp);
        fclose(fp);
    }
    return 0;
}

#endif /* TEST_HTMLCS */
