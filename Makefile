.PHONY: build test shell clean

default: build

build:
	rebar3 compile

test:
	rebar3 eunit -v

shell:
	rebar3 shell

typecheck:
	dialyzer --src src/*.erl src/utils/*.erl test/*.erl

clean:
	rm -rf *.xml _build 
