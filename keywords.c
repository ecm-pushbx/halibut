/*
 * keywords.c: keep track of all cross-reference keywords
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "halibut.h"

static int kwcmp(const void *av, const void *bv, void *cmpctx)
{
    const keyword *a = (const keyword *)av;
    const keyword *b = (const keyword *)bv;
    return ustrcmp(a->key, b->key);
}

static int kwfind(const void *av, const void *bv, void *cmpctx)
{
    wchar_t *a = (wchar_t *)av;
    const keyword *b = (const keyword *)bv;
    return ustrcmp(a, b->key);
}

keyword *kw_lookup(keywordlist *kl, wchar_t *str) {
    return findcmp234(kl->keys, str, kwfind, NULL);
}

/*
 * This function reads through source form and collects the
 * keywords. They get collected in a heap, sorted by Unicode
 * collation, last at the top (so that we can Heapsort them when we
 * finish).
 */
keywordlist *get_keywords(paragraph *source, errorstate *es) {
    bool errors = false;
    keywordlist *kl = snew(keywordlist);
    numberstate *n = number_init();
    int prevpara = para_NotParaType;

    number_cfg(n, source);

    kl->size = 0;
    kl->keys = newtree234(kwcmp, NULL);
    kl->nlooseends = kl->looseendssize = 0;
    kl->looseends = NULL;
    for (; source; source = source->next) {
	wchar_t *p, *q;
	p = q = source->keyword;

	/*
	 * Look for the section type override (`example',
	 * `question' or whatever - to replace `chapter' or
	 * `section' on a per-section basis).
	 */
	if (q) {
	    q = uadv(q);	       /* point q at the word beyond */
	    if (!*q) q = NULL;
	}
	
	/*
	 * Number the chapter / section / list-item / whatever.
	 * This also sets up the `parent', `child' and `sibling'
	 * links.
	 */
	source->kwtext = number_mktext(n, source, q, &prevpara, &errors, es);

	if (p && *p) {
	    if (source->kwtext || source->type == para_Biblio) {
		keyword *kw, *ret;

		kw = snew(keyword);
		kw->key = p;
		kw->text = source->kwtext;
		kw->para = source;
		ret = add234(kl->keys, kw);
		if (ret != kw) {
		    err_multikw(es, &source->fpos, &ret->para->fpos, p);
		    sfree(kw);
		    /* FIXME: what happens to kw->text? Does it leak? */
		}
	    }
	} else {
	    if (kl->nlooseends >= kl->looseendssize) {
		kl->looseendssize = kl->nlooseends + 32;
		kl->looseends = sresize(kl->looseends, kl->looseendssize,
					word *);
	    }
	    kl->looseends[kl->nlooseends++] = source->kwtext;
	}
    }

    number_free(n);

    if (errors) {
	free_keywords(kl);
	return NULL;
    }

    return kl;
}

void free_keywords(keywordlist *kl) {
    keyword *kw;
    while (kl->nlooseends)
	free_word_list(kl->looseends[--kl->nlooseends]);
    sfree(kl->looseends);
    while ( (kw = index234(kl->keys, 0)) != NULL) {
        delpos234(kl->keys, 0);
	free_word_list(kw->text);
	sfree(kw);
    }
    freetree234(kl->keys);
    sfree(kl);
}

void subst_keywords(paragraph *source, keywordlist *kl, errorstate *es) {
    for (; source; source = source->next) {
	word *ptr;
	for (ptr = source->words; ptr; ptr = ptr->next) {
	    if (ptr->type == word_UpperXref ||
		ptr->type == word_LowerXref) {
		keyword *kw;
		word **endptr, *close, *subst;

		kw = kw_lookup(kl, ptr->text);
		if (!kw) {
		    err_nosuchkw(es, &ptr->fpos, ptr->text);
		    subst = NULL;
		} else
		    subst = dup_word_list(kw->text);

		if (subst && ptr->type == word_LowerXref &&
		    kw->para->type != para_Biblio &&
		    kw->para->type != para_BiblioCited)
		    ustrlow(subst->text);

		close = snew(word);
		close->text = NULL;
		close->alt = NULL;
		close->type = word_XrefEnd;
		close->fpos = ptr->fpos;
		close->breaks = false;
		close->aux = 0;

		close->next = ptr->next;
		ptr->next = subst;

		for (endptr = &ptr->next; *endptr; endptr = &(*endptr)->next)
		    (*endptr)->fpos = ptr->fpos;

		*endptr = close;
		ptr = close;
	    }
	}
    }
}
