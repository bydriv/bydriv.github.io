ANNE:=anne
HTML:=anne/shirley/html.rb
NOVEL:=shirley/novel.rb

.INTERMEDIATE: index.json $(patsubst %.anne,%.html,$(filter-out %index.anne,$(shell find -name '*.anne'))) $(filter-out blog/summary.html,$(patsubst %/index.anne,%/summary.html,$(shell find blog -name 'index.anne')))

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

blog/index.anne: $(filter-out blog/summary.html,$(patsubst %/index.anne,%/summary.html,$(shell find blog -name 'index.anne')))
	printf "" > $@
	for summary in $^; do \
		echo "#div(class=window)" >> $@; \
		echo "  [#div(class=header)]" >> $@; \
		echo "  [#div(class=content) [@cat $$summary] [#div(class=cont) ([#a(href=/$$(dirname $$summary)) READ MORE])]]" >> $@; \
		echo "" >> $@; \
	done
	sed -i '$$d' $@

blog/%/summary.json: blog/%/index.json
	cat $< | jq '{type:"document",value:.value|map(select(.type == "paragraph"))|.[0:10]}' > $@

blog/%/index.html: blog/%/index.json $(HTML) $(wildcard prelude/*) $(patsubst %.anne,%.html,$(filter-out %index.anne,$(shell find -name '*.anne')))
	cat prelude/header.html > $@
	cat prelude/blog-header.html >> $@
	$(HTML) $< >> $@
	cat prelude/blog-footer.html >> $@

novel/%.json: novel/%.txt $(ANNE)
	$(ANNE) < $< > $@

novel/%.html: novel/%.json $(NOVEL)
	$(NOVEL) $< > $@

novel/index.html: novel/index.json $(HTML) $(wildcard prelude/*) $(patsubst %.anne,%.html,$(filter-out %index.anne,$(shell find -name '*.anne')))
	cat prelude/header.html > $@
	$(HTML) $< >> $@

ss/%.json: ss/%.txt $(ANNE)
	$(ANNE) < $< > $@

ss/%.html: ss/%.json $(NOVEL)
	$(NOVEL) $< > $@

ss/index.html: ss/index.json $(HTML) $(wildcard prelude/*) $(patsubst %.anne,%.html,$(filter-out %index.anne,$(shell find -name '*.anne')))
	cat prelude/header.html > $@
	$(HTML) $< >> $@
