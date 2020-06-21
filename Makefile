SITE_HEADER := etc/site/header.html

PANDOC := pandoc
PANDOC_OPTIONS := -f markdown+east_asian_line_breaks
PANDOC_TEMPLATE := etc/pandoc/template.html

.PHONY: all
all: \
	index.html \
	illustration/index.html \
	photo/index.html

%.html: %.md $(PANDOC_TEMPLATE)
	$(PANDOC) \
		$(PANDOC_OPTIONS) \
		--template $(PANDOC_TEMPLATE) \
		$< > $@
