all: compile

clean:
	rebar -v clean

deps:
	rebar -v get-deps

compile: deps
	rebar -v compile

test:
	rebar -v skip_deps=true eunit

.PHONY: all clean test
