

.PHONY: compile erl iex
all: compile

compile:
	rebar compile

erl: compile
	erl -pa deps/*/ebin ebin

iex: compile
	iex --erl '-pa deps/*/ebin ebin'
