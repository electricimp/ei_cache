all: get-deps compile eunit

get-deps:
	./rebar get-deps
	
compile:
	./rebar compile
	
eunit:
	./rebar skip_deps=true eunit

console:
	erl -pa deps/*/ebin -pa ebin

clean:
	./rebar clean
