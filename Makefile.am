bin_PROGRAMS = convcs cstable csshow confuse
lib_LIBRARIES = libcharset.a

convcs_SOURCES = convcs.c
convcs_LDADD = libcharset.a

cstable_SOURCES = cstable.c
cstable_LDADD = libcharset.a

csshow_SOURCES = csshow.c
csshow_LDADD = libcharset.a

confuse_SOURCES = confuse.c
confuse_LDADD = libcharset.a

libcharset_a_SOURCES = charset.h internal.h sbcsdat.h big5enc.c		\
big5set.c cns11643.c cp949.c emacsenc.c enum.h euc.c fromucs.c		\
gb2312.c htmlcs.c hz.c iso2022.c iso2022s.c iso6937.c istate.c		\
jisx0208.c jisx0212.c ksx1001.c locale.c localenc.c macenc.c		\
mimeenc.c sbcs.c sbcsdat.c shiftjis.c slookup.c superset.c toucs.c	\
utf16.c utf7.c utf8.c xenc.c

BUILT_SOURCES = sbcsdat.c sbcsdat.h
CLEANFILES = sbcsdat.c sbcsdat.h
sbcsdat.c: sbcsgen.pl sbcs.dat
	perl $^ --source=sbcsdat.c
sbcsdat.h: sbcsgen.pl sbcs.dat
	perl $^ --header=sbcsdat.h

EXTRA_DIST = sbcsgen.pl sbcs.dat
