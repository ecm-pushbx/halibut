/*
 * toucs.c - convert charsets to Unicode.
 */

#include "charset.h"
#include "internal.h"

struct unicode_emit_param {
    wchar_t *output;
    int outlen;
    int writtenlen;
    const wchar_t *errstr;
    int errlen;
    bool stopped;
};

static void unicode_emit(void *ctx, long int output)
{
    struct unicode_emit_param *param = (struct unicode_emit_param *)ctx;
    wchar_t outval;
    wchar_t const *p;
    int outlen;

    if (output == ERROR) {
	if (param->errstr) {
	    p = param->errstr;
	    outlen = param->errlen;
	} else {
	    outval = 0xFFFD;	       /* U+FFFD REPLACEMENT CHARACTER */
	    p = &outval;
	    outlen = 1;
	}
    } else {
	outval = output;
	p = &outval;
	outlen = 1;
    }

    if (param->outlen < 0 || param->outlen >= outlen) {
	while (outlen > 0) {
	    if (param->output)
		*param->output++ = *p++;
	    if (param->outlen > 0)
		param->outlen--;
	    outlen--;
	    param->writtenlen++;
	}
    } else {
	param->stopped = true;
    }
}

int charset_to_unicode(const char **input, int *inlen,
		       wchar_t *output, int outlen,
		       int charset, charset_state *state,
		       const wchar_t *errstr, int errlen)
{
    charset_spec const *spec = charset_find_spec(charset);
    charset_state localstate = CHARSET_INIT_STATE;
    struct unicode_emit_param param;

    param.output = output;
    param.outlen = outlen;
    param.errstr = errstr;
    param.errlen = errlen;
    param.writtenlen = 0;
    param.stopped = false;

    if (state)
	localstate = *state;	       /* structure copy */

    while (*inlen > 0) {
	int lenbefore = param.writtenlen;
	spec->read(spec, (unsigned char)**input, &localstate,
		   unicode_emit, &param);
	if (param.stopped) {
	    /*
	     * The emit function has _tried_ to output some
	     * characters, but ran up against the end of the
	     * buffer. Leave immediately, and return what happened
	     * _before_ attempting to process this character.
	     */
	    return lenbefore;
	}
	if (state)
	    *state = localstate;   /* structure copy */
	(*input)++;
	(*inlen)--;
    }

    return param.writtenlen;
}
