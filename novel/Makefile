EMACS := emacs
YES := yes # YES YES YES "OH MY GOD".

%.txt.org: %.txt mkorg.pl
	perl mkorg.pl $< > $@

%.txt.tex: %.txt mktex.pl
	perl mktex.pl $< > $@

%.html: %.org %.txt.org ../setup.org
	$(YES) yes | $(EMACS) -batch -u $(shell whoami) $< -f org-html-export-to-html

%.pdf: %.tex %.txt.tex
	lualatex $<
	lualatex $<
	mv "$@" tmp.pdf
	gs -dNoOutputFonts -sDEVICE=pdfwrite -o "$@" tmp.pdf
	rm -f tmp.pdf
