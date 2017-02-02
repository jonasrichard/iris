
BIN=_build/default/rel/iris/bin/iris
VER=0.0.2
IMG=jonasrichard/iris:${VER}


.PHONY: docker-build docker-console

rebar: rebar
	curl -O https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

docker-build:
	docker build -t ${IMG} --no-cache .

docker-console:
	docker run -ti --network=iris_default ${IMG} /bin/bash

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

run1:
	VMARGS_PATH=$PWD/config/vm1.args \
	  _build/default/rel/iris/bin/iris console

run2:
	OTHER_NODE=iris1@192.168.11.220 \
	RELX_CONFIG_PATH=$PWD/config/sys1.config \
	VMARGS_PATH=$PWD/config/vm2.args \
	  _build/default/rel/iris/bin/iris console
