#!/usr/bin/make -f
Y:=defvalY
Z:=defvalZ
.PHONY: main
main:
ifndef X
	@echo X is required
	@exit 1
endif
	@echo X="$(X)"
	@echo Y="$(Y)"
	@echo Z="$(Z)"
