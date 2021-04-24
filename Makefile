GIT := git

RUBY := ruby
ERB := erb

ROUTES := $(patsubst %/Datafile,%/,$(addprefix /,$(shell $(GIT) ls-files Datafile '*/Datafile')))
INDICES := $(patsubst %,.%index.html,$(ROUTES))
ARTICLES := $(patsubst %,.%article.html,$(ROUTES))
NAVS := $(patsubst %,.%nav.html,$(ROUTES))

.PHONY: all
all: $(INDICES) $(ARTICLES) $(NAVS)

index.html: Datafile template/index.html.erb article.html nav.html $(shell ROUTE=/ $(RUBY) misc/lsdep.rb)
	ROUTE=/ ROUTES="$(ROUTES)" $(ERB) -T - template/index.html.erb > $@

article.html: Datafile template/article.html.erb $(shell ROUTE=/ $(RUBY) misc/lsdep.rb)
	ROUTE=/ ROUTES="$(ROUTES)" $(ERB) -T - template/article.html.erb > $@

nav.html: Datafile template/nav.html.erb $(shell ROUTE=/ $(RUBY) misc/lsdep.rb)
	ROUTE=/ ROUTES="$(ROUTES)" $(ERB) -T - template/nav.html.erb > $@

.SECONDEXPANSION:
%/index.html: %/Datafile template/index.html.erb %/article.html %/nav.html $$(shell ROUTE=/$$(dir $$<) $(RUBY) misc/lsdep.rb)
	ROUTE=/$(dir $<) ROUTES="$(ROUTES)" $(ERB) -T - template/index.html.erb > $@

%/article.html: %/Datafile template/article.html.erb $$(shell ROUTE=/$$(dir $$<) $(RUBY) misc/lsdep.rb)
	ROUTE=/$(dir $<) ROUTES="$(ROUTES)" $(ERB) -T - template/article.html.erb > $@

%/nav.html: %/Datafile template/nav.html.erb $$(shell ROUTE=/$$(dir $$<) $(RUBY) misc/lsdep.rb)
	ROUTE=/$(dir $<) ROUTES="$(ROUTES)" $(ERB) -T - template/nav.html.erb > $@
