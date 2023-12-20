#!/usr/bin/env make -f

.PHONY: all
all: dist

.PHONY: dist
dist: public/spec.pdf
	npm run build

public/spec.pdf: public spec/main.pdf
	cp spec/main.pdf $@

public:
	mkdir -p $@

spec/main.pdf:
	make -C spec main.pdf
