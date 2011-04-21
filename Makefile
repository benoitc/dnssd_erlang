.PHONY: all doc clean test

all: compile

compile:
	@./rebar compile

doc:
	@./rebar doc skip_deps=true

clean:
	@./rebar clean

test: all
	@./rebar eunit skip_deps=true