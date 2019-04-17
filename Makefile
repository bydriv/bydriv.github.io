ANNE:=anne
HTML:=anne/shirley/html.rb
NOVEL:=shirley/novel.rb

.PHONY: all
all:
	make $$(find -name 'index.html')

%.json: %.anne $(ANNE)
	$(ANNE) < $< > $@

.INTERMEDIATE: index.json
index.html: index.json $(HTML) $(wildcard prelude/*) $(wildcard *.json)
	cat prelude/header.html > $@
	$(HTML) $< >> $@

%/index.html: %/index.json $(HTML) $(wildcard prelude/*) $(wildcard %/*.json)
	cat prelude/header.html > $@
	$(HTML) $< >> $@

novel/%/index.html: novel/%/index.json novel/%/text.html $(HTML) $(wildcard prelude/*) $(wildcard %/*.json)
	cat prelude/header.html > $@
	$(HTML) $< >> $@

novel/%/text.json: novel/%.txt $(ANNE)
	$(ANNE) < $< > $@

novel/%/text.html: novel/%/text.json $(NOVEL)
	$(NOVEL) $< > $@
