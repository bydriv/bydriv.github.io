ERB := erb
GIT := git

ROUTES := $(patsubst %/Datafile,%/,$(addprefix /,$(shell $(GIT) ls-files Datafile '*/Datafile')))
INDICES := $(patsubst %,.%index.html,$(ROUTES))
ARTICLES := $(patsubst %,.%article.html,$(ROUTES))
NAVS := $(patsubst %,.%nav.html,$(ROUTES))

.PHONY: all
all: $(INDICES) $(ARTICLES) $(NAVS)

index.html: Datafile template/index.html.erb article.html nav.html
	ROUTE=/ ROUTES="$(ROUTES)" $(ERB) -T - template/index.html.erb > $@

%/index.html: %/Datafile template/index.html.erb %/article.html %/nav.html %/../nav.html
	ROUTE=$(patsubst %/Datafile,/%/,$<) ROUTES="$(ROUTES)" $(ERB) -T - template/index.html.erb > $@

article.html: Datafile template/article.html.erb
	ROUTE=/ ROUTES="$(ROUTES)" $(ERB) -T - template/article.html.erb > $@

%/article.html: %/Datafile template/article.html.erb
	ROUTE=$(patsubst %/Datafile,/%/,$<) ROUTES="$(ROUTES)" $(ERB) -T - template/article.html.erb > $@

nav.html: Datafile template/nav.html.erb
	ROUTE=/ ROUTES="$(ROUTES)" $(ERB) -T - template/nav.html.erb > $@

%/nav.html: %/Datafile template/nav.html.erb
	ROUTE=$(patsubst %/Datafile,/%/,$<) ROUTES="$(ROUTES)" $(ERB) -T - template/nav.html.erb > $@
