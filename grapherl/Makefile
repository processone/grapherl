CLIENT_PATH := $(wildcard _build/default/rel/grapherl/lib/graph_web-*)


all: compile release

compile:
	rebar3 compile
	cd _build/default/lib/eleveldb/ && $(MAKE)
	cd _build/default/lib/procket/ && $(MAKE)

release:
	rebar3 release

client:
	cd apps/graph_web_client && grunt
	cp -R apps/graph_web_client/build/* $(CLIENT_PATH)/priv


console:
	_build/default/rel/grapherl/bin/grapherl console
