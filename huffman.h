/*
 * huffman.h: Huffman tree-building routines common to Deflate and LZX.
 */

/*
 * Take an input array 'freqs' of size 'nsyms' giving each symbol's
 * frequency. Return an output array 'lengths' of the same size giving
 * each symbol's code length. Enforce during construction that no code
 * length is greater than 'limit'.
 *
 * The 'freqs' array may be modified, as a side effect of the limit
 * enforcement.
 */
void build_huffman_tree(int *freqs, unsigned char *lengths,
                        int nsyms, int limit);

/*
 * Given a sequence of Huffman code lengths, compute the actual code
 * values. Each one is returned in codes[i] and occupies the bottom
 * lengths[i] bits of the integer. They are in natural big-endian
 * format, i.e. the initial bit of the code, governing the choice of
 * direction at the root node of the notional decode tree, is in the
 * most significant bit position.
 *
 * Returns the maximum code length found. Can also return -1 to
 * indicate the table was overcommitted (too many or too short codes
 * to exactly cover the possible space), which is a fatal error (the
 * output codes will not form a usable Huffman tree), or -2 to
 * indicate it was undercommitted (too few or too long codes), which
 * is a non-fatal error (the resulting tree would be usable, just
 * inefficient).
 */
int compute_huffman_codes(const unsigned char *lengths, int *codes, int nsyms);
