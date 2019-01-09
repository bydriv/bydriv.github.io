.PHONY: all
all: just-a-board/6bit.png just-a-board/9bit.png just-a-board/12bit.png just-a-board/15bit.png just-a-board/pixelized.png just-a-board/pixelized_6bit.png apple-bobbing/6bit.png apple-bobbing/9bit.png apple-bobbing/12bit.png apple-bobbing/15bit.png apple-bobbing/pixelized.png apple-bobbing/pixelized_6bit.png

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
