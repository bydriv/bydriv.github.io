ECHO := echo
EXPR := expr
TEST := test
TR := tr
WC := wc

ERB := erb
GIT := git

ROUTES := $(patsubst %/Datafile,%/,$(addprefix /,$(shell $(GIT) ls-files Datafile '*/Datafile')))
INDICES := $(patsubst %,.%index.html,$(ROUTES))
ARTICLES := $(patsubst %,.%article.html,$(ROUTES))
NAVS := $(patsubst %,.%nav.html,$(ROUTES))

.PHONY: all
all: $(INDICES) $(ARTICLES) $(NAVS)

index.html: Datafile template/index.html.erb article.html nav.html
	ROUTE_WITHIN=/ ROUTE=/ ROUTES="$(ROUTES)" $(ERB) -T - template/index.html.erb > $@

article.html: Datafile template/article.html.erb
	ROUTE_WITHIN=/ ROUTE=/ ROUTES="$(ROUTES)" $(ERB) -T - template/article.html.erb > $@

nav.html: Datafile template/nav.html.erb
	ROUTE_WITHIN=/ ROUTE=/ ROUTES="$(ROUTES)" $(ERB) -T - template/nav.html.erb > $@

%/index.html: %/Datafile template/index.html.erb %/article.html %/nav.html %/../nav.html
	if $(TEST) $$($(EXPR) $$($(ECHO) $< | $(TR) -cd / | $(WC) -m) % 2) = 0; \
	then ROUTE_WITHIN=$(dir /$<) ROUTE=$(dir /$<) ROUTES="$(ROUTES)" $(ERB) -T - template/index.html.erb > $@; \
	else ROUTE_WITHIN=$(dir $(patsubst %/,%,$(dir /$<))) ROUTE=$(dir /$<) ROUTES="$(ROUTES)" $(ERB) -T - template/index.html.erb > $@; \
	fi

%/article.html: %/Datafile template/article.html.erb
	if $(TEST) $$($(EXPR) $$($(ECHO) $< | $(TR) -cd / | $(WC) -m) % 2) = 0; \
	then ROUTE_WITHIN=$(dir /$<) ROUTE=$(dir /$<) ROUTES="$(ROUTES)" $(ERB) -T - template/article.html.erb > $@; \
	else ROUTE_WITHIN=$(dir $(patsubst %/,%,$(dir /$<))) ROUTE=$(dir /$<) ROUTES="$(ROUTES)" $(ERB) -T - template/article.html.erb > $@; \
	fi

%/nav.html: %/Datafile template/nav.html.erb
	if $(TEST) $$($(EXPR) $$($(ECHO) $< | $(TR) -cd / | $(WC) -m) % 2) = 0; \
	then ROUTE_WITHIN=$(dir /$<) ROUTE=$(dir /$<) ROUTES="$(ROUTES)" $(ERB) -T - template/nav.html.erb > $@; \
	else ROUTE_WITHIN=$(dir $(patsubst %/,%,$(dir /$<))) ROUTE=$(dir /$<) ROUTES="$(ROUTES)" $(ERB) -T - template/nav.html.erb > $@; \
	fi
