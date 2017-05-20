SUBDIRS = charset

AM_CPPFLAGS = -I$(srcdir)/charset

bin_PROGRAMS = halibut

halibut_SOURCES = main.c halibut.h version.h malloc.c ustring.c		\
error.c help.c licence.c version.c misc.c tree234.c tree234.h input.c	\
in_afm.c in_pf.c in_sfnt.c keywords.c contents.c index.c biblio.c	\
bk_text.c bk_html.c bk_whlp.c bk_man.c bk_info.c bk_paper.c paper.h	\
bk_ps.c bk_pdf.c winhelp.c winhelp.h winchm.c winchm.h deflate.c	\
deflate.h lzx.c lzx.h lz77.c lz77.h huffman.c huffman.h psdata.c	\
wcwidth.c

halibut_LDADD = charset/libcharset.a

# Prevent 'make install' from recursing into $(SUBDIRS) and hence also
# installing libcharset's supporting binary utilities.
install:
	$(MAKE) $(AM_MAKEFLAGS) install-am

man1_MANS = halibut.1
noinst_DATA = halibut.info halibut.txt halibut.ps halibut.pdf halibut.chm
doc_DATA = index.html halibut.pdf

MANPAGE_SRC = $(srcdir)/doc/manpage.but
halibut.1: halibut $(MANPAGE_SRC)
	./halibut --man=$@ $(MANPAGE_SRC)

MANUAL_SRC := $(srcdir)/doc/blurb.but $(srcdir)/doc/intro.but	\
$(srcdir)/doc/running.but $(srcdir)/doc/input.but		\
$(srcdir)/doc/output.but $(srcdir)/doc/licence.but		\
$(srcdir)/doc/manpage.but $(srcdir)/doc/index.but

index.html: halibut $(MANUAL_SRC)
	./halibut --html $(MANUAL_SRC)

halibut.info: halibut $(MANUAL_SRC)
	./halibut --info=halibut.info $(MANUAL_SRC)

halibut.txt: halibut $(MANUAL_SRC)
	./halibut --text=$@ $(MANUAL_SRC)

halibut.ps: halibut $(MANUAL_SRC)
	./halibut --ps=$@ $(MANUAL_SRC)

halibut.pdf: halibut $(MANUAL_SRC)
	./halibut --pdf=$@ $(MANUAL_SRC)

halibut.chm: halibut $(MANUAL_SRC)
	./halibut --chm=$@ $(MANUAL_SRC)

# Install all the other parts of the HTML, and the Info documentation.
# I can't just add those file lists to the usual Automake
# dir_FILETYPES variables, because both are variable lists of
# filenames, and in addition, Automake doesn't even let me write an
# info_DATA variable at all because it expects that the only things
# you might ever install in $(infodir) are generated from Texinfo
# source.
install-data-hook: halibut.info
	$(INSTALL_DATA) *.html $(docdir)
	$(MKDIR_P) $(infodir)
	$(INSTALL_DATA) *.info* $(infodir)

EXTRA_DIST = $(MANUAL_SRC) $(MANPAGE_SRC)
