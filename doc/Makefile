#!/usr/bin/env make -f

.PHONY: all
all: dist

.PHONY: dist
dist: public/spec.pdf
	npm run build

public/spec.pdf: public
	make -C spec main.pdf
	cp spec/main.pdf public/spec.pdf

public:
	mkdir -p $@
