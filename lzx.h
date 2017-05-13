/*
 * lzx.h: LZX encoder for Windows CHM files.
 */

struct LZXEncodedFile {
    unsigned char *data;
    size_t data_len;

    size_t *reset_byte_offsets;
    size_t n_resets;
};

/*
 * Produce an LZX-compressed encoding of an input data block. Return
 * it, along with a list of byte offsets where the data stream is
 * realigned to a 16-bit boundary because one of realign_interval and
 * reset_interval has run out.
 *
 * The output structure and its fields 'data' and 'reset_byte_offsets'
 * are all dynamically allocated, and need freeing by the receiver
 * when finished with.
 */
struct LZXEncodedFile *lzx(const void *data, int len,
                           int realign_interval, int reset_interval);
