/*
 * main.c: command line parsing and top level
 */

#include <assert.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include "halibut.h"
#include "paper.h"

static void dbg_prtsource(paragraph *sourceform);
static void dbg_prtwordlist(int level, word *w);
static void dbg_prtkws(keywordlist *kws);

static const struct pre_backend {
    void *(*func)(paragraph *, keywordlist *, indexdata *, psdata *,
                  errorstate *);
    int bitfield;
} pre_backends[] = {
    {paper_pre_backend, 0x0001}
};

static const struct backend {
    const char *name;
    void (*func)(paragraph *, keywordlist *, indexdata *, void *,
                 errorstate *);
    paragraph *(*filename)(char *filename);
    int bitfield, prebackend_bitfield;
} backends[] = {
    {"text", text_backend, text_config_filename, 0x0001, 0},
    {"xhtml", html_backend, html_config_filename, 0x0002, 0},
    {"html", html_backend, html_config_filename, 0x0002, 0},
    {"hlp", whlp_backend, whlp_config_filename, 0x0004, 0},
    {"whlp", whlp_backend, whlp_config_filename, 0x0004, 0},
    {"winhelp", whlp_backend, whlp_config_filename, 0x0004, 0},
    {"man", man_backend, man_config_filename, 0x0008, 0},
    {"info", info_backend, info_config_filename, 0x0010, 0},
    {"ps", ps_backend, ps_config_filename, 0x0020, 0x0001},
    {"pdf", pdf_backend, pdf_config_filename, 0x0040, 0x0001},
    {"chm", chm_backend, chm_config_filename, 0x0080, 0},
};

int main(int argc, char **argv) {
    char **infiles;
    int nfiles;
    bool nogo;
    bool reportcols;
    bool list_fonts;
    int input_charset;
    bool debug;
    int backendbits, prebackbits;
    int k, b;
    paragraph *cfg, *cfg_tail;
    void *pre_backend_data[16];
    errorstate es[1];

    /*
     * Use the specified locale everywhere. It'll be used for
     * output of error messages, and as the default character set
     * for input files if one is not explicitly specified.
     * 
     * However, we need to use standard numeric formatting for
     * output of things like PDF.
     */
    setlocale(LC_ALL, "");
    setlocale(LC_NUMERIC, "C");

    /*
     * Set up initial (default) parameters.
     */
    infiles = snewn(argc, char *);
    nfiles = 0;
    nogo = false;
    reportcols = false;
    list_fonts = false;
    input_charset = CS_ASCII;
    debug = false;
    backendbits = 0;
    cfg = cfg_tail = NULL;
    es->fatal = false;

    if (argc == 1) {
	usage();
	exit(EXIT_SUCCESS);
    }

    /*
     * Parse command line arguments.
     */
    while (--argc) {
	char *p = *++argv;
	if (*p == '-' && p[1]) {
	    /*
	     * An option.
	     */
	    while (p && *++p) {
		char c = *p;
		switch (c) {
		  case '-':
		    /*
		     * Long option.
		     */
		    {
			char *opt, *val;
			opt = p++;     /* opt will have _one_ leading - */
			while (*p && *p != '=')
			    p++;	       /* find end of option */
			if (*p == '=') {
			    *p++ = '\0';
			    val = p;
			} else
			    val = NULL;

			assert(opt[0] == '-');
			for (k = 0; k < (int)lenof(backends); k++)
			    if (!strcmp(opt+1, backends[k].name)) {
				backendbits |= backends[k].bitfield;
				if (val) {
				    paragraph *p = backends[k].filename(val);
				    assert(p);
				    if (cfg_tail)
					cfg_tail->next = p;
				    else
					cfg = p;
				    while (p->next)
					p = p->next;
				    cfg_tail = p;
				}
				break;
			    }
			if (k < (int)lenof(backends)) {
			    /* do nothing */;
			} else if (!strcmp(opt, "-input-charset")) {
			    if (!val) {
				err_optnoarg(es, opt);
			    } else {
				int charset = charset_from_localenc(val);
				if (charset == CS_NONE) {
				    err_cmdcharset(es, val);
				} else {
				    input_charset = charset;
				}
			    }
			} else if (!strcmp(opt, "-help")) {
			    help();
			    nogo = true;
			} else if (!strcmp(opt, "-version")) {
			    showversion();
			    nogo = true;
			} else if (!strcmp(opt, "-licence") ||
				   !strcmp(opt, "-license")) {
			    licence();
			    nogo = true;
			} else if (!strcmp(opt, "-list-charsets")) {
			    listcharsets();
			    nogo = true;
			} else if (!strcmp(opt, "-list-fonts")) {
			    list_fonts = true;
			} else if (!strcmp(opt, "-precise")) {
			    reportcols = true;
			} else {
			    err_nosuchopt(es, opt);
			}
		    }
		    p = NULL;
		    break;
		  case 'h':
		  case 'V':
		  case 'L':
		  case 'P':
		  case 'd':
		    /*
		     * Option requiring no parameter.
		     */
		    switch (c) {
		      case 'h':
			help();
			nogo = true;
			break;
		      case 'V':
			showversion();
			nogo = true;
			break;
		      case 'L':
			licence();
			nogo = true;
			break;
		      case 'P':
			reportcols = true;
			break;
		      case 'd':
			debug = true;
			break;
		    }
		    break;
		  case 'C':
		    /*
		     * Option requiring parameter.
		     */
		    p++;
		    if (!*p && argc > 1)
			--argc, p = *++argv;
		    else if (!*p) {
			char opt[2];
			opt[0] = c;
			opt[1] = '\0';
			err_optnoarg(es, opt);
		    }
		    /*
		     * Now c is the option and p is the parameter.
		     */
		    switch (c) {
		      case 'C':
			/*
			 * -C means we split our argument up into
			 * colon-separated chunks and assemble them
			 * into a config paragraph.
			 */
			{
			    char *s = dupstr(p), *q, *r;
			    paragraph *para;

			    para = cmdline_cfg_new();

			    q = r = s;
			    while (*q) {
				if (*q == ':') {
				    *r = '\0';
				    /* XXX ad-hoc diagnostic */
				    if (!strcmp(s, "input-charset"))
					err_futileopt(es, "Cinput-charset",
					      "; use --input-charset");
				    cmdline_cfg_add(para, s);
				    r = s;
				} else {
				    if (*q == '\\' && q[1])
					q++;
				    *r++ = *q;
				}
				q++;
			    }
			    *r = '\0';
			    cmdline_cfg_add(para, s);

			    if (cfg_tail)
				cfg_tail->next = para;
			    else
				cfg = para;
			    cfg_tail = para;
			}
			break;
		    }
		    p = NULL;	       /* prevent continued processing */
		    break;
		  default:
		    /*
		     * Unrecognised option.
		     */
		    {
			char opt[2];
			opt[0] = c;
			opt[1] = '\0';
			err_nosuchopt(es, opt);
		    }
		}
	    }
	} else {
	    /*
	     * A non-option argument.
	     */
	    if (!strcmp(p, "-"))
		infiles[nfiles++] = NULL;   /* special case: read stdin */
	    else
		infiles[nfiles++] = p;
	}
    }

    if (es->fatal)
	exit(EXIT_FAILURE);
    if (nogo)
	exit(EXIT_SUCCESS);

    /*
     * Do the work.
     */
    if (nfiles == 0 && !list_fonts) {
	err_noinput(es);
	usage();
	exit(EXIT_FAILURE);
    }

    {
	input in;
	paragraph *sourceform, *p;
	indexdata *idx;
	keywordlist *keywords;
        psdata *psd;

	in.filenames = infiles;
	in.nfiles = nfiles;
	in.currfp = NULL;
	in.currindex = 0;
	in.npushback = in.pushbacksize = 0;
	in.pushback = NULL;
	in.reportcols = reportcols;
	in.stack = NULL;
	in.defcharset = input_charset;
        in.es = es;

	idx = make_index();
        psd = psdata_new();

	sourceform = read_input(&in, idx, psd);
	if (list_fonts) {
	    listfonts(psd);
	    exit(EXIT_SUCCESS);
	}
	if (es->fatal)
	    exit(EXIT_FAILURE);
        assert(sourceform);

	/*
	 * Append the config directives acquired from the command
	 * line.
	 */
	{
	    paragraph *end;

	    end = sourceform;
	    while (end && end->next)
		end = end->next;
	    assert(end);

	    end->next = cfg;
	}

	sfree(in.pushback);

	sfree(infiles);

	keywords = get_keywords(sourceform, es);
	if (!keywords)
	    exit(EXIT_FAILURE);
	gen_citations(sourceform, keywords, es);
	subst_keywords(sourceform, keywords, es);

	for (p = sourceform; p; p = p->next)
	    if (p->type == para_IM)
		index_merge(idx, true, p->keyword, p->words, &p->fpos, es);

	build_index(idx);

	/*
	 * Set up attr_First / attr_Last / attr_Always, in the main
	 * document and in the index entries.
	 */
	for (p = sourceform; p; p = p->next)
	    mark_attr_ends(p->words);
	{
	    int i;
	    indexentry *entry;

	    for (i = 0; (entry = index234(idx->entries, i)) != NULL; i++)
		mark_attr_ends(entry->text);
	}

	if (debug) {
	    index_debug(idx);
	    dbg_prtkws(keywords);
	    dbg_prtsource(sourceform);
	}

	/*
	 * Select and run the pre-backends.
	 */
	prebackbits = 0;
        memset(pre_backend_data, 0, sizeof(pre_backend_data));
	for (k = 0; k < (int)lenof(backends); k++)
	    if (backendbits == 0 || (backendbits & backends[k].bitfield))
		prebackbits |= backends[k].prebackend_bitfield;
	for (k = 0; k < (int)lenof(pre_backends); k++)
	    if (prebackbits & pre_backends[k].bitfield) {
		assert(k < (int)lenof(pre_backend_data));
		pre_backend_data[k] =
		    pre_backends[k].func(sourceform, keywords, idx, psd, es);
	    }

	/*
	 * Run the selected set of backends.
	 */
	for (k = b = 0; k < (int)lenof(backends); k++)
	    if (b != backends[k].bitfield) {
		b = backends[k].bitfield;
		if (backendbits == 0 || (backendbits & b)) {
		    void *pbd = NULL;
		    int pbb = backends[k].prebackend_bitfield;
		    int m;

		    for (m = 0; m < (int)lenof(pre_backends); m++)
			if (pbb & pre_backends[m].bitfield) {
			    assert(m < (int)lenof(pre_backend_data));
			    pbd = pre_backend_data[m];
			    break;
			}
			    
		    backends[k].func(sourceform, keywords, idx, pbd, es);
		}
	    }

	free_para_list(sourceform);
	free_keywords(keywords);
	cleanup_index(idx);
        psdata_free(psd);
    }

    if (es->fatal)
        exit(EXIT_FAILURE);

    return 0;
}

static void dbg_prtsource(paragraph *sourceform) {
    /*
     * Output source form in debugging format.
     */

    paragraph *p;
    for (p = sourceform; p; p = p->next) {
	wchar_t *wp;
	printf("para %d ", p->type);
	if (p->keyword) {
	    wp = p->keyword;
	    while (*wp) {
		putchar('\"');
		for (; *wp; wp++)
		    putchar(*wp);
		putchar('\"');
		if (*++wp)
		    printf(", ");
	    }
	} else
	    printf("(no keyword)");
	printf(" {\n");
	dbg_prtwordlist(1, p->words);
	printf("}\n");
    }
}

static void dbg_prtkws(keywordlist *kws) {
    /*
     * Output keywords in debugging format.
     */

    int i;
    keyword *kw;

    for (i = 0; (kw = index234(kws->keys, i)) != NULL; i++) {
	wchar_t *wp;
	printf("keyword ");
	wp = kw->key;
	while (*wp) {
	    putchar('\"');
	    for (; *wp; wp++)
		putchar(*wp);
	    putchar('\"');
	    if (*++wp)
		printf(", ");
	}
	printf(" {\n");
	dbg_prtwordlist(1, kw->text);
	printf("}\n");
    }
}

static void dbg_prtwordlist(int level, word *w) {
    for (; w; w = w->next) {
	wchar_t *wp;
	printf("%*sword %d ", level*4, "", w->type);
	if (w->text) {
	    printf("\"");
	    for (wp = w->text; *wp; wp++)
		    putchar(*wp);
	    printf("\"");
	} else
	    printf("(no text)");
	if (w->breaks)
	    printf(" [breaks]");
	if (w->alt) {
	    printf(" alt = {\n");
	    dbg_prtwordlist(level+1, w->alt);
	    printf("%*s}", level*4, "");
	}
	printf("\n");
    }
}
