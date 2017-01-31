
BIN=_build/default/rel/iris/bin/iris

rebar: rebar
	curl -O https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

compile:
	./rebar3 compile

release:
	./rebar3 release

ct:
	${BIN} start
	./rebar3 ct
	@-${BIN} stop

start:
	${BIN} start

stop:
	${BIN} stop

console:
	${BIN} console

