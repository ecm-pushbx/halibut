/*
 * lz77.c: common LZ77 compression code between Deflate and LZX.
 */

#include "halibut.h" /* only for snew, sfree etc */
#include "lz77.h"

/*
 * Modifiable parameters.
 */
#define HASHMAX 2039		       /* one more than max hash value */
#define MAXMATCH 32		       /* how many matches we track */
#define HASHCHARS 3		       /* how many chars make a hash */

/*
 * This compressor takes a less slapdash approach than the
 * gzip/zlib one. Rather than allowing our hash chains to fall into
 * disuse near the far end, we keep them doubly linked so we can
 * _find_ the far end, and then every time we add a new byte to the
 * window (thus rolling round by one and removing the previous
 * byte), we can carefully remove the hash chain entry.
 */

#define INVALID -1		       /* invalid hash _and_ invalid offset */
struct WindowEntry {
    short next, prev;		       /* array indices within the window */
    short hashval;
};

struct HashEntry {
    short first;		       /* window index of first in chain */
};

struct Match {
    int distance, len;
};

struct LZ77InternalContext {
    int winsize;
    struct WindowEntry *win; /* [winsize] */
    unsigned char *data; /* [winsize] */

    int winpos;
    struct HashEntry hashtab[HASHMAX];
    unsigned char pending[HASHCHARS];
    int npending;
};

static int lz77_hash(const unsigned char *data)
{
    return (257 * data[0] + 263 * data[1] + 269 * data[2]) % HASHMAX;
}

void lz77_init(struct LZ77Context *ctx, int winsize)
{
    struct LZ77InternalContext *st;
    int i;

    st = snew(struct LZ77InternalContext);

    ctx->ictx = st;

    st->winsize = winsize;
    st->win = snewn(st->winsize, struct WindowEntry);
    st->data = snewn(st->winsize, unsigned char);

    for (i = 0; i < st->winsize; i++)
	st->win[i].next = st->win[i].prev = st->win[i].hashval = INVALID;
    for (i = 0; i < HASHMAX; i++)
	st->hashtab[i].first = INVALID;
    st->winpos = 0;

    st->npending = 0;
}

void lz77_cleanup(struct LZ77Context *ctx)
{
    struct LZ77InternalContext *st = ctx->ictx;
    sfree(st->win);
    sfree(st->data);
    sfree(st);
}

static void lz77_advance(struct LZ77InternalContext *st,
			 unsigned char c, int hash)
{
    int off;

    /*
     * Remove the hash entry at winpos from the tail of its chain,
     * or empty the chain if it's the only thing on the chain.
     */
    if (st->win[st->winpos].prev != INVALID) {
	st->win[st->win[st->winpos].prev].next = INVALID;
    } else if (st->win[st->winpos].hashval != INVALID) {
	st->hashtab[st->win[st->winpos].hashval].first = INVALID;
    }

    /*
     * Create a new entry at winpos and add it to the head of its
     * hash chain.
     */
    st->win[st->winpos].hashval = hash;
    st->win[st->winpos].prev = INVALID;
    off = st->win[st->winpos].next = st->hashtab[hash].first;
    st->hashtab[hash].first = st->winpos;
    if (off != INVALID)
	st->win[off].prev = st->winpos;
    st->data[st->winpos] = c;

    /*
     * Advance the window pointer.
     */
    st->winpos = (st->winpos + 1) % st->winsize;
}

#define CHARAT(k) ( (k)<0 ? st->data[(st->winpos+k)%st->winsize] : data[k] )

void lz77_compress(struct LZ77Context *ctx,
                   const unsigned char *data, int len, int compress)
{
    struct LZ77InternalContext *st = ctx->ictx;
    int i, hash, distance, off, nmatch, matchlen, advance;
    struct Match defermatch, matches[MAXMATCH];
    int deferchr;

    /*
     * Add any pending characters from last time to the window. (We
     * might not be able to.)
     */
    for (i = 0; i < st->npending; i++) {
	unsigned char foo[HASHCHARS];
	int j;
	if (len + st->npending - i < HASHCHARS) {
	    /* Update the pending array. */
	    for (j = i; j < st->npending; j++)
		st->pending[j - i] = st->pending[j];
	    break;
	}
	for (j = 0; j < HASHCHARS; j++)
	    foo[j] = (i + j < st->npending ? st->pending[i + j] :
		      data[i + j - st->npending]);
	lz77_advance(st, foo[0], lz77_hash(foo));
    }
    st->npending -= i;

    defermatch.len = 0;
    deferchr = '\0';
    while (len > 0) {

	/* Don't even look for a match, if we're not compressing. */
	if (compress && len >= HASHCHARS) {
	    /*
	     * Hash the next few characters.
	     */
	    hash = lz77_hash(data);

	    /*
	     * Look the hash up in the corresponding hash chain and see
	     * what we can find.
	     */
	    nmatch = 0;
	    for (off = st->hashtab[hash].first;
		 off != INVALID; off = st->win[off].next) {
		/* distance = 1       if off == st->winpos-1 */
		/* distance = winsize if off == st->winpos   */
		distance = st->winsize -
                    (off + st->winsize - st->winpos) % st->winsize;
		for (i = 0; i < HASHCHARS; i++)
		    if (CHARAT(i) != CHARAT(i - distance))
			break;
		if (i == HASHCHARS) {
		    matches[nmatch].distance = distance;
		    matches[nmatch].len = 3;
		    if (++nmatch >= MAXMATCH)
			break;
		}
	    }
	} else {
	    nmatch = 0;
	    hash = INVALID;
	}

	if (nmatch > 0) {
	    /*
	     * We've now filled up matches[] with nmatch potential
	     * matches. Follow them down to find the longest. (We
	     * assume here that it's always worth favouring a
	     * longer match over a shorter one.)
	     */
	    matchlen = HASHCHARS;
	    while (matchlen < len) {
		int j;
		for (i = j = 0; i < nmatch; i++) {
		    if (CHARAT(matchlen) ==
			CHARAT(matchlen - matches[i].distance)) {
			matches[j++] = matches[i];
		    }
		}
		if (j == 0)
		    break;
		matchlen++;
		nmatch = j;
	    }

	    /*
	     * We've now got all the longest matches. We favour the
	     * shorter distances, which means we go with matches[0].
	     * So see if we want to defer it or throw it away.
	     */
	    matches[0].len = matchlen;
	    if (defermatch.len > 0) {
		if (matches[0].len > defermatch.len + 1) {
		    /* We have a better match. Emit the deferred char,
		     * and defer this match. */
		    ctx->literal(ctx, (unsigned char) deferchr);
		    defermatch = matches[0];
		    deferchr = data[0];
		    advance = 1;
		} else {
		    /* We don't have a better match. Do the deferred one. */
		    ctx->match(ctx, defermatch.distance, defermatch.len);
		    advance = defermatch.len - 1;
		    defermatch.len = 0;
		}
	    } else {
		/* There was no deferred match. Defer this one. */
		defermatch = matches[0];
		deferchr = data[0];
		advance = 1;
	    }
	} else {
	    /*
	     * We found no matches. Emit the deferred match, if
	     * any; otherwise emit a literal.
	     */
	    if (defermatch.len > 0) {
		ctx->match(ctx, defermatch.distance, defermatch.len);
		advance = defermatch.len - 1;
		defermatch.len = 0;
	    } else {
		ctx->literal(ctx, data[0]);
		advance = 1;
	    }
	}

	/*
	 * Now advance the position by `advance' characters,
	 * keeping the window and hash chains consistent.
	 */
	while (advance > 0) {
	    if (len >= HASHCHARS) {
		lz77_advance(st, *data, lz77_hash(data));
	    } else {
		st->pending[st->npending++] = *data;
	    }
	    data++;
	    len--;
	    advance--;
	}
    }
}

