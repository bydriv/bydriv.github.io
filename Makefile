DEFAULT_TEMPLATE_HTML := etc/make/default/template.html

PANDOC := pandoc
PANDOC_OPTIONS := -f markdown+east_asian_line_breaks
PANDOC_OPTIONS_HTML := --mathml

.PHONY: all
all: \
	index.html \
	blog/index.html \
	$(patsubst %.md,%.html,$(wildcard blog/*/index.md)) \
	illustration/index.html \
	photo/index.html \
	literature/index.html \
	$(patsubst %.md,%.html,$(wildcard literature/*/index.md)) \
	$(patsubst %.md,%.html,$(wildcard literature/*/*/*.md)) \
	world/index.html \
	$(patsubst %.md,%.html,$(wildcard world/*/index.md)) \
	comic/index.html \
	$(patsubst %.md,%.html,$(wildcard comic/*/index.md)) \
	misc/index.html \
	$(patsubst %.md,%.html,$(wildcard misc/*/index.md)) \
	r18/index.html \
	$(patsubst %.md,%.html,$(wildcard r18/*/index.md))

%.html: %.md $(DEFAULT_TEMPLATE_HTML)
	$(PANDOC) \
		$(PANDOC_OPTIONS) \
		$(PANDOC_OPTIONS_HTML) \
		--template $(DEFAULT_TEMPLATE_HTML) \
		$< \
	> $@
