struct chm;

struct chm *chm_new(void);
void chm_free(struct chm *chm);
void chm_add_file(struct chm *chm, const char *name,
                  const char *data, int len);
void chm_title(struct chm *chm, const char *title);
void chm_contents_filename(struct chm *chm, const char *name);
void chm_index_filename(struct chm *chm, const char *name);
void chm_default_topic(struct chm *chm, const char *name);
void chm_default_window(struct chm *chm, const char *name);
void chm_add_window(struct chm *chm, const char *winname, const char *title,
                    const char *contentsfile, const char *indexfile,
                    const char *rootfile, int navpaneflags, int toolbarflags);

struct chm_section;
struct chm_section *chm_add_section(struct chm *chm,
                                    struct chm_section *parent,
                                    const char *title, const char *url);

const char *chm_build(struct chm *chm, int *outlen);
