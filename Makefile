DEFAULT_TEMPLATE_HTML := etc/make/default/template.html

PANDOC := pandoc
PANDOC_OPTIONS := -f markdown+east_asian_line_breaks
PANDOC_OPTIONS_HTML :=

.PHONY: all
all: \
	index.html \
	illustration/index.html \
	photo/index.html

%.html: %.md $(DEFAULT_TEMPLATE_HTML)
	$(PANDOC) \
		$(PANDOC_OPTIONS) \
		$(PANDOC_OPTIONS_HTML) \
		--template $(DEFAULT_TEMPLATE_HTML) \
		$< \
	> $@
