GIT := git

RUBY := ruby
ERB := erb

LATEX := lualatex
PDF2SVG := pdf2svg
GS := gs

CD := cd
MV := mv
RM := rm

DATAFILES := $(shell $(GIT) ls-files Datafile '*/Datafile')
ROUTES := $(patsubst %/Datafile,%/,$(addprefix /,$(DATAFILES)))
INDICES := $(patsubst %,.%index.html,$(ROUTES))
ARTICLES := $(patsubst %,.%article.html,$(ROUTES))
NAVS := $(patsubst %,.%nav.html,$(ROUTES))

.PHONY: all
all: site/routes.json $(INDICES) $(ARTICLES) $(NAVS) $(patsubst %.tex,%.svg,$(shell $(GIT) ls-files Datafile '*.tex') $(wildcard library/*/*.tex))

%.pdf: %.tex
	$(CD) $(dir $<) && $(LATEX) $(notdir $<)
	$(GS) -o $(basename $<)-tmp.pdf -dNoOutputFonts -sDEVICE=pdfwrite $(basename $<).pdf
	$(MV) $(basename $<)-tmp.pdf $(basename $<).pdf
	$(RM) -f $(basename $<).aux $(basename $<).log

%.svg: %.pdf
	$(PDF2SVG) $< $@

site/routes.json: site/routes.rb $(DATAFILES)
	ROUTES="$(ROUTES)" $(RUBY) $< > $@

index.html: Datafile template/index.html.erb article.html nav.html $(shell ROUTE=/ $(RUBY) misc/lsdep.rb)
	ROUTE=/ $(ERB) -T - template/index.html.erb > $@

article.html: Datafile template/article.html.erb $(shell ROUTE=/ $(RUBY) misc/lsdep.rb)
	ROUTE=/ $(ERB) -T - template/article.html.erb > $@

nav.html: Datafile template/nav.html.erb $(shell ROUTE=/ $(RUBY) misc/lsdep.rb)
	ROUTE=/ $(ERB) -T - template/nav.html.erb > $@

.SECONDEXPANSION:
%/index.html: %/Datafile template/index.html.erb %/article.html %/nav.html $$(shell ROUTE=/$$(dir $$<) $(RUBY) misc/lsdep.rb)
	ROUTE=/$(dir $<) $(ERB) -T - template/index.html.erb > $@

%/article.html: %/Datafile template/article.html.erb $$(shell ROUTE=/$$(dir $$<) $(RUBY) misc/lsdep.rb)
	ROUTE=/$(dir $<) $(ERB) -T - template/article.html.erb > $@

%/nav.html: %/Datafile template/nav.html.erb $$(shell ROUTE=/$$(dir $$<) $(RUBY) misc/lsdep.rb)
	ROUTE=/$(dir $<) $(ERB) -T - template/nav.html.erb > $@
