/*
 * error.c: Halibut error handling
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "halibut.h"

/*
 * Error flags
 */
#define PREFIX 0x0001		       /* give `halibut:' prefix */
#define FILEPOS 0x0002		       /* give file position prefix */

static void do_error(const filepos *fpos, const char *fmt, ...)
{
    va_list ap;

    if (fpos) {
	fprintf(stderr, "%s:",
                fpos->filename ? fpos->filename : "<standard input>");
	if (fpos->line > 0)
	    fprintf(stderr, "%d:", fpos->line);
	if (fpos->col > 0)
	    fprintf(stderr, "%d:", fpos->col);
	fputc(' ', stderr);
    } else {
	fputs("halibut: ", stderr);
    }

    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);

    fputc('\n', stderr);
}

void fatalerr_nomemory(void)
{
    do_error(NULL, "out of memory");
    exit(EXIT_FAILURE);
}

void err_optnoarg(errorstate *es, const char *sp)
{
    es->fatal = true;
    do_error(NULL, "option `-%s' requires an argument", sp);
}

void err_nosuchopt(errorstate *es, const char *sp)
{
    es->fatal = true;
    do_error(NULL, "unrecognised option `-%s'", sp);
}

void err_cmdcharset(errorstate *es, const char *sp)
{
    es->fatal = true;
    do_error(NULL, "character set `%s' not recognised", sp);
}

void err_futileopt(errorstate *es, const char *sp, const char *sp2)
{
    do_error(NULL, "warning: option `-%s' has no effect%s", sp, sp2);
}

void err_noinput(errorstate *es)
{
    es->fatal = true;
    do_error(NULL, "no input files");
}

void err_cantopen(errorstate *es, const char *sp)
{
    es->fatal = true;
    do_error(NULL, "unable to open input file `%s'", sp);
}

void err_nodata(errorstate *es)
{
    es->fatal = true;
    do_error(NULL, "no data in input files");
}

void err_zerochar(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "the Unicode zero character is not permitted in input");
}

void err_brokencodepara(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "every line of a code paragraph should begin `\\c'");
}

void err_kwunclosed(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "expected `}' after paragraph keyword");
}

void err_kwexpected(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "expected a paragraph keyword");
}

void err_kwillegal(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "expected no paragraph keyword");
}

void err_kwtoomany(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "expected only one paragraph keyword");
}

void err_bodyillegal(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "expected no text after paragraph keyword");
}

void err_badparatype(errorstate *es, const wchar_t *wsp, const filepos *fpos)
{
    es->fatal = true;
    char *sp = utoa_locale_dup(wsp);
    do_error(fpos, "command `%s' unrecognised at start of paragraph", sp);
    sfree(sp);
}

void err_badmidcmd(errorstate *es, const wchar_t *wsp, const filepos *fpos)
{
    es->fatal = true;
    char *sp = utoa_locale_dup(wsp);
    do_error(fpos, "command `%s' unexpected in mid-paragraph", sp);
    sfree(sp);
}

void err_unexbrace(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "brace character unexpected in mid-paragraph");
}

void err_explbr(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "expected `{' after command");
}

void err_commenteof(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "end of file unexpected inside `\\#{...}' comment");
}

void err_kwexprbr(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "expected `}' after cross-reference");
}

void err_codequote(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "unable to nest \\q{...} within \\c{...} or \\cw{...}");
}

void err_missingrbrace(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "unclosed braces at end of paragraph");
}

void err_missingrbrace2(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "unclosed braces at end of input file");
}

void err_nestedstyles(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "unable to nest text styles");
}

void err_nestedindex(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "unable to nest index markings");
}

void err_indexcase(errorstate *es, const filepos *fpos, const wchar_t *wsp,
                   const filepos *fpos2, const wchar_t *wsp2)
{
    char *sp = utoa_locale_dup(wsp), *sp2 = utoa_locale_dup(wsp2);
    do_error(fpos, "warning: index tag `%s' used with different "
             "case (`%s') at %s:%d",
             sp, sp2, fpos2->filename, fpos2->line);
    sfree(sp);
    sfree(sp2);
}

void err_nosuchkw(errorstate *es, const filepos *fpos, const wchar_t *wsp)
{
    es->fatal = true;
    char *sp = utoa_locale_dup(wsp);
    do_error(fpos, "unable to resolve cross-reference to `%s'", sp);
    sfree(sp);
}

void err_multiBR(errorstate *es, const filepos *fpos, const wchar_t *wsp)
{
    es->fatal = true;
    char *sp = utoa_locale_dup(wsp);
    do_error(fpos, "multiple `\\BR' entries given for `%s'", sp);
    sfree(sp);
}

void err_nosuchidxtag(errorstate *es, const filepos *fpos, const wchar_t *wsp)
{
    es->fatal = true;
    char *sp = utoa_locale_dup(wsp);
    do_error(fpos, "`\\IM' on unknown index tag `%s'", sp);
    sfree(sp);
}

void err_cantopenw(errorstate *es, const char *sp)
{
    es->fatal = true;
    do_error(NULL, "unable to open output file `%s'", sp);
}

void err_macroexists(errorstate *es, const filepos *fpos, const wchar_t *wsp)
{
    es->fatal = true;
    char *sp = utoa_locale_dup(wsp);
    do_error(fpos, "macro `%s' already defined", sp);
    sfree(sp);
}

void err_sectjump(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "expected higher heading levels before this one");
}

void err_winhelp_ctxclash(errorstate *es, const filepos *fpos,
                          const char *sp, const char *sp2)
{
    es->fatal = true;
    do_error(fpos, "Windows Help context id `%s' clashes with "
             "previously defined `%s'", sp, sp2);
}

void err_multikw(errorstate *es, const filepos *fpos, const filepos *fpos2,
                 const wchar_t *wsp)
{
    es->fatal = true;
    char *sp = utoa_locale_dup(wsp);
    do_error(fpos, "paragraph keyword `%s' already defined at %s:%d",
             sp, fpos2->filename, fpos2->line);
    sfree(sp);
}

void err_misplacedlcont(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "\\lcont is only expected after a list item");
}

void err_sectmarkerinblock(errorstate *es, const filepos *fpos, const char *sp)
{
    es->fatal = true;
    do_error(fpos, "section headings are not supported within \\%s", sp);
}

void err_cfginsufarg(errorstate *es, const filepos *fpos, const char *sp,
                     int i)
{
    es->fatal = true;
    do_error(fpos, "\\cfg{%s} expects at least %d parameter%s",
             sp, i, (i==1)?"":"s");
}

void err_infonodechar(errorstate *es, const filepos *fpos, char c)
                      /* fpos might be NULL */
{
    es->fatal = true;
    do_error(fpos, "info output format does not support '%c' in"
             " node names; removing", c);
}

void err_text_codeline(errorstate *es, const filepos *fpos, int i, int j)
{
    do_error(fpos, "warning: code paragraph line is %d chars wide, wider"
             " than body width %d", i, j);
}

void err_htmlver(errorstate *es, const filepos *fpos, const wchar_t *wsp)
{
    es->fatal = true;
    char *sp = utoa_locale_dup(wsp);
    do_error(fpos, "unrecognised HTML version keyword `%s'", sp);
    sfree(sp);
}

void err_charset(errorstate *es, const filepos *fpos, const wchar_t *wsp)
{
    es->fatal = true;
    char *sp = utoa_locale_dup(wsp);
    do_error(fpos, "character set `%s' not recognised", sp);
    sfree(sp);
}

void err_nofont(errorstate *es, const filepos *fpos, const wchar_t *wsp)
{
    es->fatal = true;
    char *sp = utoa_locale_dup(wsp);
    do_error(fpos, "font `%s' not recognised", sp);
    sfree(sp);
}

void err_afmeof(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "AFM file ended unexpectedly");
}

void err_afmkey(errorstate *es, const filepos *fpos, const char *sp)
{
    es->fatal = true;
    do_error(fpos, "required AFM key '%s' missing", sp);
}

void err_afmvers(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "unsupported AFM version");
}

void err_afmval(errorstate *es, const filepos *fpos, const char *sp, int i)
{
    es->fatal = true;
    if (i == 1)
        do_error(fpos, "AFM key '%s' requires a value", sp);
    else
        do_error(fpos, "AFM key '%s' requires %d values", sp, i);
}

void err_pfeof(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "Type 1 font file ended unexpectedly");
}

void err_pfhead(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "Type 1 font file header line invalid");
}

void err_pfbad(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "Type 1 font file invalid");
}

void err_pfnoafm(errorstate *es, const filepos *fpos, const char *sp)
{
    es->fatal = true;
    do_error(fpos, "no metrics available for Type 1 font '%s'", sp);
}

void err_chmnames(errorstate *es)
{
    es->fatal = true;
    do_error(NULL, "only one of html-mshtmlhelp-chm and "
             "html-mshtmlhelp-hhp found");
}

void err_sfntnotable(errorstate *es, const filepos *fpos, const char *sp)
{
    es->fatal = true;
    do_error(fpos, "font has no '%s' table", sp);
}

void err_sfntnopsname(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "font has no PostScript name");
}

void err_sfntbadtable(errorstate *es, const filepos *fpos, const char *sp)
{
    es->fatal = true;
    do_error(fpos, "font has an invalid '%s' table", sp);
}

void err_sfntnounicmap(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "font has no UCS-2 character map");
}

void err_sfnttablevers(errorstate *es, const filepos *fpos, const char *sp)
{
    es->fatal = true;
    do_error(fpos, "font has an unsupported '%s' table version", sp);
}

void err_sfntbadhdr(errorstate *es, const filepos *fpos)
{
    es->fatal = true;
    do_error(fpos, "font has an invalid header");
}

void err_sfntbadglyph(errorstate *es, const filepos *fpos, unsigned wc)
{
    do_error(fpos,
             "warning: character U+%04X references a non-existent glyph",
             wc);
}

void err_chm_badname(errorstate *es, const filepos *fpos, const char *sp)
{
    es->fatal = true;
    do_error(fpos, "CHM internal file name `%s' begins with"
             " a reserved character", sp);
}
