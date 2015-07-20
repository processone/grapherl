PATH  := node_modules/.bin:$(PATH)
SHELL := /bin/bash

source_files    := $(wildcard src/*.coffee)
build_files     := $(source_files:%.coffee=build/%.js)
app_bundle      := build/graph_web.js


.PHONY: all clean


all: $(app_bundle)


build/%.js: %.coffee
	coffee -co $(dir $@) $<


$(app_bundle): $(build_files)
	uglifyjs -cmo $@ $^


clean:
	rm -rf build

