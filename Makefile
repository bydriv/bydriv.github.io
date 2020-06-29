DEFAULT_TEMPLATE_HTML := etc/make/default/template.html

LITERATURE_TEMPLATE_TEX := etc/make/literature/template.tex
LITERATURE_PREPROCESS := etc/make/literature/preprocess.sed

GS := gs
LUALATEX := lualatex

PANDOC := pandoc
PANDOC_OPTIONS := -f markdown+east_asian_line_breaks
PANDOC_OPTIONS_HTML :=
PANDOC_OPTIONS_LATEX := -t latex --chapters

TMPDIR := $(shell mktemp -d)

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

literature/%.tex: literature/%.md $(LITERATURE_TEMPLATE_TEX)
	$(LITERATURE_PREPROCESS) $< \
	| $(PANDOC) \
		$(PANDOC_OPTIONS) \
		$(PANDOC_OPTIONS_LATEX) \
		--template $(LITERATURE_TEMPLATE_TEX) \
	> $@

literature/%.pdf: literature/%/index.tex
	cd $(dir $<) && $(LUALATEX) $(notdir $<)
	cd $(dir $<) && $(LUALATEX) $(notdir $<)
	rm $(basename $<).aux $(basename $<).log $(basename $<).out
	mv $(basename $<).pdf $@
	$(GS) -o $(TMPDIR)/tmp.pdf -dNoOutputFonts -sDEVICE=pdfwrite $@
	mv $(TMPDIR)/tmp.pdf $@
