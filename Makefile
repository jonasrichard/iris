
REL=_build/default/rel/iris
BIN=${REL}/bin/iris
IMG=jonasrichard/iris:0.0.2


.PHONY: docker-build docker-console

docker-build:
	docker build -t ${IMG} .

docker-console:
	docker run -ti --network=docker_default ${IMG} /bin/bash

docker-dev:
	docker run -ti --rm --network=docker_default -v ${PWD}:${PWD} elixir:1.8.1 /bin/bash

cover:
	MIX_ENV=test mix coveralls

release:
	MIX_ENV=dev mix release --env=dev

start:
	${BIN} start

stop:
	${BIN} stop

console:
	${BIN} console

