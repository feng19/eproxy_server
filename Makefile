.PHONY: deps 

all: compile

compile:
	@./rebar compile

clean:
	@./rebar clean
