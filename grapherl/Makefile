CLIENT_PATH := $(wildcard _build/default/rel/grapherl/lib/graph_web-*)


all: compile release

compile:
	rebar3 compile
	cd _build/default/lib/eleveldb/ && $(MAKE)
	cd _build/default/lib/procket/ && $(MAKE)


release:
	rebar3 release
	rm -rf $(CLIENT_PATH)/priv
	mkdir -p $(CLIENT_PATH)/priv
	cp -R apps/graph_web_client/build/* $(CLIENT_PATH)/priv

client:
	cd apps/graph_web_client && grunt
	rm -rf $(CLIENT_PATH)/priv
	mkdir -p $(CLIENT_PATH)/priv
	cp -R apps/graph_web_client/build/* $(CLIENT_PATH)/priv


console:
	_build/default/rel/grapherl/bin/grapherl console
