.PHONY: all
all: gradient-hue-polar/6bit.png gradient-hue-polar/9bit.png gradient-hue-polar/12bit.png gradient-hue-polar/15bit.png gradient-hue-polar/pixelized.png gradient-hue-polar/pixelized_6bit.png just-a-board/6bit.png just-a-board/9bit.png just-a-board/12bit.png just-a-board/15bit.png just-a-board/pixelized.png just-a-board/pixelized_6bit.png

gradient-hue-polar.png:
	convert -size 512x1536 gradient:'#FFF-#0FF' -rotate 90 -alpha set -virtual-pixel Transparent +distort Polar 255 +repage -rotate 90 -set colorspace HSB -colorspace RGB "$@"

%/6bit.png: %.png
	convert -depth 2 "$<" "$@"

%/9bit.png: %.png
	convert -depth 3 "$<" "$@"

%/12bit.png: %.png
	convert -depth 4 "$<" "$@"

%/15bit.png: %.png
	convert -depth 5 "$<" "$@"

%/pixelized.png: %.png
	./pixelize "$<" "$@"

%/pixelized_6bit.png: %/pixelized.png
	convert -depth 2 "$<" "$@"
