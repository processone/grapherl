all: compile release

compile:
	rebar3 compile
	cd _build/default/lib/eleveldb/ && $(MAKE)

release:
	rebar3 release

console:
	_build/default/rel/grapherl/bin/grapherl console
