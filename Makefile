novel/%.json: novel/%.txt
	cat $< | bin/anne | bin/shirley shirley/structured-novel.json | jq . > $@
