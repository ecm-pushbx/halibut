/*
 * huffman.c: implementation of huffman.h.
 */

#include <assert.h>

#include "halibut.h"
#include "huffman.h"

static const unsigned fibonacci[] = {
    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597,
    2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418,
    317811, 514229, 832040, 1346269, 2178309, 3524578, 5702887, 9227465,
    14930352, 24157817, 39088169, 63245986, 102334155, 165580141, 267914296,
    433494437, 701408733, 1134903170, 1836311903, 2971215073,
};

/*
 * Binary heap functions used by buildhuf(). Each one assumes the
 * heap to be stored in an array of ints, with two ints per node
 * (user data and key). They take in the old heap length, and
 * return the new one.
 */
#define HEAPPARENT(x) (((x)-2)/4*2)
#define HEAPLEFT(x) ((x)*2+2)
#define HEAPRIGHT(x) ((x)*2+4)
static int addheap(int *heap, int len, int userdata, int key)
{
    int me, dad, tmp;

    me = len;
    heap[len++] = userdata;
    heap[len++] = key;

    while (me > 0) {
	dad = HEAPPARENT(me);
	if (heap[me+1] < heap[dad+1]) {
	    tmp = heap[me]; heap[me] = heap[dad]; heap[dad] = tmp;
	    tmp = heap[me+1]; heap[me+1] = heap[dad+1]; heap[dad+1] = tmp;
	    me = dad;
	} else
	    break;
    }

    return len;
}
static int rmheap(int *heap, int len, int *userdata, int *key)
{
    int me, lc, rc, c, tmp;

    len -= 2;
    *userdata = heap[0];
    *key = heap[1];
    heap[0] = heap[len];
    heap[1] = heap[len+1];

    me = 0;

    while (1) {
	lc = HEAPLEFT(me);
	rc = HEAPRIGHT(me);
	if (lc >= len)
	    break;
	else if (rc >= len || heap[lc+1] < heap[rc+1])
	    c = lc;
	else
	    c = rc;
	if (heap[me+1] > heap[c+1]) {
	    tmp = heap[me]; heap[me] = heap[c]; heap[c] = tmp;
	    tmp = heap[me+1]; heap[me+1] = heap[c+1]; heap[c+1] = tmp;
	} else
	    break;
	me = c;
    }

    return len;
}

struct hufscratch {
    int *parent, *length, *heap;
};

/*
 * The core of the Huffman algorithm: takes an input array of
 * symbol frequencies, and produces an output array of code
 * lengths.
 *
 * We cap the output code lengths to fit in an unsigned char (which is
 * safe since our clients will impose some smaller limit on code
 * length anyway). So if you see 255 in the output, it means '255 or
 * more' and is a sign that whatever limit you really wanted has
 * certainly been overflowed.
 */
static void buildhuf(struct hufscratch *sc, const int *freqs,
                     unsigned char *lengths, int nsyms)
{
    int heapsize;
    int i, j, n;
    int si, sj;

    for (i = 0; i < nsyms; i++)
        sc->parent[i] = 0;

    /*
     * Begin by building the heap.
     */
    heapsize = 0;
    for (i = 0; i < nsyms; i++)
	if (freqs[i] > 0)	       /* leave unused symbols out totally */
	    heapsize = addheap(sc->heap, heapsize, i, freqs[i]);

    /*
     * Now repeatedly take two elements off the heap and merge
     * them.
     */
    n = nsyms;
    while (heapsize > 2) {
	heapsize = rmheap(sc->heap, heapsize, &i, &si);
	heapsize = rmheap(sc->heap, heapsize, &j, &sj);
	sc->parent[i] = n;
	sc->parent[j] = n;
	heapsize = addheap(sc->heap, heapsize, n, si + sj);
	n++;
    }

    /*
     * Now we have our tree, in the form of a link from each node
     * to the index of its parent. Count back down the tree to
     * determine the code lengths.
     */
    for (i = 0; i < 2*nsyms+1; i++)
        sc->length[i] = 0;
    /* The tree root has length 0 after that, which is correct. */
    for (i = n-1; i-- ;)
	if (sc->parent[i] > 0)
	    sc->length[i] = 1 + sc->length[sc->parent[i]];

    /*
     * And that's it. (Simple, wasn't it?) Copy the lengths into
     * the output array and leave.
     * 
     * Here we cap lengths to fit in unsigned char.
     */
    for (i = 0; i < nsyms; i++)
	lengths[i] = (sc->length[i] > 255 ? 255 : sc->length[i]);
}

/*
 * Wrapper around buildhuf() which enforces the restriction on code
 * length.
 */
void build_huffman_tree(int *freqs, unsigned char *lengths,
                        int nsyms, int limit)
{
    struct hufscratch hsc, *sc = &hsc;
    int smallestfreq, totalfreq, nactivesyms;
    int num, denom, adjust;
    int i;
    int maxprob;

    sc->parent = snewn(2*nsyms+1, int);
    sc->length = snewn(2*nsyms+1, int);
    sc->heap = snewn(2*nsyms, int);

    /*
     * Nasty special case: if the frequency table has fewer than
     * two non-zero elements, we must invent some, because we can't
     * have fewer than one bit encoding a symbol.
     */
    assert(nsyms >= 2);
    {
	int count = 0;
	for (i = 0; i < nsyms; i++)
	    if (freqs[i] > 0)
		count++;
	if (count < 2) {
	    for (i = 0; i < nsyms && count > 0; i++)
		if (freqs[i] == 0) {
		    freqs[i] = 1;
		    count--;
		}
	}
    }

    /*
     * First, try building the Huffman table the normal way. If
     * this works, it's optimal, so we don't want to mess with it.
     */
    buildhuf(sc, freqs, lengths, nsyms);

    for (i = 0; i < nsyms; i++)
	if (lengths[i] > limit)
	    break;

    if (i == nsyms)
        goto cleanup;                  /* OK */

    /*
     * The Huffman algorithm can only ever generate a code length
     * of N bits or more if there is a symbol whose probability is
     * less than the reciprocal of the (N+2)th Fibonacci number
     * (counting from F_0=0 and F_1=1), i.e. 1/2584 for N=16, or
     * 1/55 for N=8. (This is a necessary though not sufficient
     * condition.)
     *
     * Why is this? Well, consider the input symbol with the
     * smallest probability. Let that probability be x. In order
     * for this symbol to have a code length of at least 1, the
     * Huffman algorithm will have to merge it with some other
     * node; and since x is the smallest probability, the node it
     * gets merged with must be at least x. Thus, the probability
     * of the resulting combined node will be at least 2x. Now in
     * order for our node to reach depth 2, this 2x-node must be
     * merged again. But what with? We can't assume the node it
     * merges with is at least 2x, because this one might only be
     * the _second_ smallest remaining node. But we do know the
     * node it merges with must be at least x, so our order-2
     * internal node is at least 3x.
     *
     * How small a node can merge with _that_ to get an order-3
     * internal node? Well, it must be at least 2x, because if it
     * was smaller than that then it would have been one of the two
     * smallest nodes in the previous step and been merged at that
     * point. So at least 3x, plus at least 2x, comes to at least
     * 5x for an order-3 node.
     *
     * And so it goes on: at every stage we must merge our current
     * node with a node at least as big as the bigger of this one's
     * two parents, and from this starting point that gives rise to
     * the Fibonacci sequence. So we find that in order to have a
     * node n levels deep (i.e. a maximum code length of n), the
     * overall probability of the root of the entire tree must be
     * at least F_{n+2} times the probability of the rarest symbol.
     * In other words, since the overall probability is 1, it is a
     * necessary condition for a code length of 16 or more that
     * there must be at least one symbol with probability <=
     * 1/F_18.
     *
     * (To demonstrate that a probability this big really can give
     * rise to a code length of 16, consider the set of input
     * frequencies { 1-epsilon, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55,
     * 89, 144, 233, 377, 610, 987 }, for arbitrarily small
     * epsilon.)
     *
     * So here buildhuf() has returned us an overlong code. So to
     * ensure it doesn't do it again, we add a constant to all the
     * (non-zero) symbol frequencies, causing them to become more
     * balanced and removing the danger. We can then feed the
     * results back to the standard buildhuf() and be
     * assert()-level confident that the resulting code lengths
     * contain nothing outside the permitted range.
     */
    assert(limit+3 < (int)lenof(fibonacci));
    maxprob = fibonacci[limit+3];
    totalfreq = nactivesyms = 0;
    smallestfreq = -1;
    for (i = 0; i < nsyms; i++) {
	if (freqs[i] == 0)
	    continue;
	if (smallestfreq < 0 || smallestfreq > freqs[i])
	    smallestfreq = freqs[i];
	totalfreq += freqs[i];
	nactivesyms++;
    }
    assert(smallestfreq <= totalfreq / maxprob);

    /*
     * We want to find the smallest integer `adjust' such that
     * (totalfreq + nactivesyms * adjust) / (smallestfreq +
     * adjust) is less than maxprob. A bit of algebra tells us
     * that the threshold value is equal to
     *
     *   totalfreq - maxprob * smallestfreq
     *   ----------------------------------
     *          maxprob - nactivesyms
     *
     * rounded up, of course. And we'll only even be trying this if
     * smallestfreq <= totalfreq / maxprob, which is precisely the
     * condition under which the numerator of this fraction is
     * positive.
     *
     * (As for the denominator, that could only be negative if there
     * were more than F_{n+2} symbols overall, in which case it
     * _wouldn't_ be possible to avoid having a symbol with
     * probability at most 1/F_{n+2}. So that is a constraint on the
     * input parameters to this function, which we enforce by
     * assertion.)
     */
    num = totalfreq - smallestfreq * maxprob;
    denom = maxprob - nactivesyms;
    assert(num > 0);   /* this just restates the assert above */
    assert(denom > 0); /* this is a constraint on the function parameters */
    adjust = (num + denom - 1) / denom;

    /*
     * Now add `adjust' to all the input symbol frequencies.
     */
    for (i = 0; i < nsyms; i++)
	if (freqs[i] != 0)
	    freqs[i] += adjust;

    /*
     * Rebuild the Huffman tree...
     */
    buildhuf(sc, freqs, lengths, nsyms);

    /*
     * ... and this time it ought to be OK.
     */
    for (i = 0; i < nsyms; i++)
	assert(lengths[i] <= limit);

  cleanup:
    /*
     * Finally, free our scratch space.
     */
    sfree(sc->parent);
    sfree(sc->length);
    sfree(sc->heap);
}

#define MAXCODELEN 31                  /* codes must fit in an int */

int compute_huffman_codes(const unsigned char *lengths, int *codes, int nsyms)
{
    unsigned count[MAXCODELEN], startcode[MAXCODELEN], code;
    int maxlen, i;

    /* Count the codes of each length. */
    maxlen = 0;
    for (i = 1; i < MAXCODELEN; i++)
	count[i] = 0;
    for (i = 0; i < nsyms; i++) {
	count[lengths[i]]++;
	if (maxlen < lengths[i])
	    maxlen = lengths[i];
    }

    /* Determine the starting code for each length block. */
    code = 0;
    for (i = 1; i < MAXCODELEN; i++) {
	startcode[i] = code;
	code += count[i];
	if (code > (1U << i))
	    maxlen = -1;	       /* overcommitted */
	code <<= 1;
    }
    if (code < (1U << MAXCODELEN))
	maxlen = -2;		       /* undercommitted */

    /* Determine the code for each symbol. */
    for (i = 0; i < nsyms; i++)
	codes[i] = startcode[lengths[i]]++;

    return maxlen;
}
