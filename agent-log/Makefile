.PHONY: all
all: zero.pdf first-mission.pdf violet-garden.pdf
	./build.sh .
	./build.sh zero
	./build.sh first-mission
	./build.sh violet-garden
	./build.sh emergence
	./build.sh can-machines-think
	./build.sh area-51
	./build.sh simian-army

%.pdf: %.tex %/index.tex agent-log.tex
	lualatex $<
	lualatex $<
	#pdfpun $@ --outfile $@ --nup 2x1
	gs -o tmp.pdf -dNoOutputFonts -sDEVICE=pdfwrite $@
	mv tmp.pdf $@

%/index.tex: %/index.md
	tail -n +5 $< \
		| perl -pe 's|!\[\]\((.*?)\)|\\includepdf{$(dir $@)\1}|g' \
		| perl -pe 's/！？/\\makebox[1\\zw][l]{\\rensuji{!?}}/g' \
		| perl -pe 's/！　/！/g' \
		| perl -pe 's/？　/？/g' > $@
