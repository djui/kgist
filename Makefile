all: compile

compile:
	./rebar compile

.PHONY: deps
deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps
