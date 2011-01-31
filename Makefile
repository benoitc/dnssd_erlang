.PHONY: all doc clean test

all: deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

doc:	
	@./rebar doc skip_deps=true

clean:
	@./rebar clean

test: all
	@./rebar eunit skip_deps=true