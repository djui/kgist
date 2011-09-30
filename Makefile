all: compile

compile: deps
	./rebar compile

.PHONY: deps
deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps
