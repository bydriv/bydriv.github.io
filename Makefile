anne/%.html: anne/%.anne shirley/html.rb
	cat $< | bin/anne | shirley/html.rb > $@

anne/%.json: anne/%.anne
	cat $< | bin/anne | jq . > $@

blog/%.html: blog/%.anne shirley/html.rb
	cat $< | bin/anne | shirley/html.rb > $@

novel/%.html: novel/%.txt shirley/novel.rb
	cat $< | bin/anne | shirley/novel.rb > $@
