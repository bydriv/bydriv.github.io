js/index.js: js/main.js js/hijack.js
	browserify $< -o $@

js/%.js: ml/%.js
	cp $< $@

ml/%.js: ml/%.ml
	cd ml && bsc $(basename $(notdir $<)).mli
	cd ml && bsc $(notdir $<)
