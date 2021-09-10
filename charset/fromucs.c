/*
 * fromucs.c - convert Unicode to other character sets.
 */

#include "charset.h"
#include "internal.h"

struct charset_emit_param {
    char *output;
    int outlen;
    int writtenlen;
    bool stopped;
};

static void charset_emit(void *ctx, long int output)
{
    struct charset_emit_param *param = (struct charset_emit_param *)ctx;

    if (param->outlen != 0) {
	if (param->output)
	    *param->output++ = output;
	if (param->outlen > 0)
	    param->outlen--;
	param->writtenlen++;
    } else {
	param->stopped = true;
    }
}

int charset_from_unicode(const wchar_t **input, int *inlen,
			 char *output, int outlen,
			 int charset, charset_state *state, bool *error)
{
    charset_spec const *spec = charset_find_spec(charset);
    charset_state localstate = CHARSET_INIT_STATE;
    struct charset_emit_param param;
    int locallen;

    if (!input) {
	locallen = 1;
	inlen = &locallen;
    }

    param.output = output;
    param.outlen = outlen;
    param.writtenlen = 0;
    param.stopped = false;

    if (state)
	localstate = *state;	       /* structure copy */
    if (error)
	*error = false;

    while (*inlen > 0) {
	int lenbefore = param.writtenlen;
	bool ret;

	if (input)
	    ret = spec->write(spec, **input, &localstate,
			      charset_emit, &param);
	else
	    ret = spec->write(spec, -1, &localstate, charset_emit, &param);
	if (error && !ret) {
	    /*
	     * We have hit a difficult character, which the user
	     * wants to know about. Leave now.
	     */
	    *error = true;
	    return lenbefore;
	}
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
	    *state = localstate;       /* structure copy */
	if (input)
	    (*input)++;
	(*inlen)--;
    }
    return param.writtenlen;
}
