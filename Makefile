.PHONY: build test shell

default: build

build:
	rebar3 compile

test:
	rebar3 eunit

shell:
	rebar3 shell