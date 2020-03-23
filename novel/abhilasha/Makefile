abhilasha.pdf: abhilasha.tex abhilasha.anne
	anne < abhilasha.anne | ./noveltex.rb > abhilasha.anne.tex
	lualatex abhilasha.tex && pdfpun abhilasha.pdf --outfile abhilasha.pdf --nup 2x1
	gs -o tmp.pdf -dNoOutputFonts -sDEVICE=pdfwrite abhilasha.pdf
	mv tmp.pdf abhilasha.pdf
