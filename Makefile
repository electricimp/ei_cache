all: get-deps compile eunit

REBAR ?= rebar3

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

eunit:
	$(REBAR) eunit skip_deps=true

console:
	erl -pa deps/*/ebin -pa ebin

clean:
	$(REBAR) clean
