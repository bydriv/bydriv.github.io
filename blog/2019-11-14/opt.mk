#!/usr/bin/make -f
X:=defvalX
Y:=defvalY
Z:=defvalZ
.PHONY: main
main:
	@echo X="$(X)"
	@echo Y="$(Y)"
	@echo Z="$(Z)"
