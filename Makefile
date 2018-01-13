EMACS := emacs
YES := yes # YES YES YES "OH MY GOD".

%.html: %.org setup.org
	$(YES) yes | $(EMACS) -batch -u $(shell whoami) $< -f org-html-export-to-html
