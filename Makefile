.PHONY: build test shell

default: build

build:
	rebar3 compile

test:
	rebar3 eunit -v

shell:
	rebar3 shell

typecheck:
	dialyzer --src src/*.erl test/*.erl
