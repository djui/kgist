all: compile

compile: deps
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps
