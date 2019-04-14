blog/%.html: blog/%.anne
	cat $< | bin/anne | bin/shirley shirley/html.json > $@

novel/%.html: novel/%.txt
	cat $< | bin/anne | bin/shirley shirley/novel.json > $@

novel/%.json: novel/%.txt
	cat $< | bin/anne | bin/shirley shirley/structured-novel.json | jq . > $@
