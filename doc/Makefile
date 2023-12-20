#!/usr/bin/env make -f

.PHONY: all
all: dist

.PHONY: dist
dist: public/spec.pdf .node-modules-installed.stamp
	npm run build

public/spec.pdf: public spec/main.pdf
	cp spec/main.pdf $@

public:
	mkdir -p $@

spec/main.pdf:
	make -C spec main.pdf

.node-modules-installed.stamp: package.json package-lock.json
	npm install
	@touch $@
