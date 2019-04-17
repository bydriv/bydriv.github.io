ANNE:=anne
HTML:=anne/shirley/html.rb
NOVEL:=shirley/novel.rb

.INTERMEDIATE: index.json $(patsubst %.anne,%.html,$(filter-out %index.anne,$(shell find -name '*.anne')))

.PHONY: all
all: $(patsubst %.anne,%.html,$(shell find -name 'index.anne'))

%.json: %.anne $(ANNE)
	$(ANNE) < $< > $@

%.html: %.json $(HTML)
	$(HTML) $< > $@

index.html: index.json $(HTML) $(wildcard prelude/*) $(patsubst %.anne,%.html,$(filter-out %index.anne,$(shell find -name '*.anne')))
	cat prelude/header.html > $@
	$(HTML) $< >> $@

%/index.html: %/index.json $(HTML) $(wildcard prelude/*) $(patsubst %.anne,%.html,$(filter-out %index.anne,$(shell find -name '*.anne')))
	cat prelude/header.html > $@
	$(HTML) $< >> $@

blog/%/index.html: blog/%/index.json $(HTML) $(wildcard prelude/*) $(patsubst %.anne,%.html,$(filter-out %index.anne,$(shell find -name '*.anne')))
	cat prelude/header.html > $@
	cat prelude/blog-header.html >> $@
	$(HTML) $< >> $@
	cat prelude/blog-footer.html >> $@

novel/%.json: novel/%.txt $(ANNE)
	$(ANNE) < $< > $@

novel/%.html: novel/%.json $(NOVEL)
	$(NOVEL) $< > $@
