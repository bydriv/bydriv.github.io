ANNE:=anne
ENNA:=anne/bin/enna

%.enna: %.anne
	cat $< | $(ANNE) | $(ENNA) > $@

anne/%.html: anne/%.anne anne/shirley/html.rb
	cat $< | $(ANNE) | anne/shirley/html.rb > $@

anne/%.json: anne/%.anne
	cat $< | $(ANNE) | jq . > $@

blog/%.html: blog/%.anne anne/shirley/html.rb
	cat $< | $(ANNE) | anne/shirley/html.rb > $@

novel/%.html: novel/%.txt shirley/novel.rb
	cat $< | $(ANNE) | shirley/novel.rb > $@
