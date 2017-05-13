mandir=$(prefix)/man
man1dir=$(mandir)/man1

CHAPTERS := $(SITE) blurb intro running input output licence manpage index

INPUTS = $(patsubst %,%.but,$(CHAPTERS))

HALIBUT = ../build/halibut

all: index.html halibut.1

index.html: $(INPUTS) $(HALIBUT)
	$(HALIBUT) --text=halibut.txt --html --info=halibut.info \
		--ps=halibut.ps --pdf=halibut.pdf --chm=halibut.chm $(INPUTS)

halibut.1: manpage.but
	$(HALIBUT) --man=halibut.1 manpage.but

install:
	mkdir -p $(man1dir)
	$(INSTALL) -m 644 halibut.1 $(man1dir)/halibut.1

clean:
	rm -f *.html *.txt *.hlp *.cnt *.1 *.info* *.ps *.pdf *.chm
