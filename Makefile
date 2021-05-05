GIT := git

RUBY := ruby
ERB := erb

LATEX := lualatex
PDF2SVG := pdf2svg

DATAFILES := $(shell $(GIT) ls-files Datafile '*/Datafile')
ROUTES := $(patsubst %/Datafile,%/,$(addprefix /,$(DATAFILES)))
INDICES := $(patsubst %,.%index.html,$(ROUTES))
ARTICLES := $(patsubst %,.%article.html,$(ROUTES))
NAVS := $(patsubst %,.%nav.html,$(ROUTES))

.PHONY: all
all: site/routes.json $(INDICES) $(ARTICLES) $(NAVS) $(patsubst %.tex,%.svg,$(shell $(GIT) ls-files Datafile '*.tex'))

%.pdf: %.tex
	cd $(dir $<) && $(LATEX) $(notdir $<)
	rm -f $(basename $<).aux $(basename $<).log

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
