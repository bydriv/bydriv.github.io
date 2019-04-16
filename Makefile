ANNE:=anne
HTML:=anne/shirley/html.rb
NOVEL:=shirley/novel.rb

index.html: index.anne $(wildcard prelude/*)
	cat prelude/header.html > $@
	cat $< | $(ANNE) | $(HTML) >> $@

%/index.html: %/index.anne $(wildcard prelude/*)
	cat prelude/header.html > $@
	cat $< | $(ANNE) | $(HTML) >> $@

novel/%/text.html: novel/%.txt shirley/novel.rb
	cat $< | $(ANNE) | $(NOVEL) > $@

%.html: %.anne $(wildcard prelude/*)
	cat $< | $(ANNE) | $(HTML) > $@
