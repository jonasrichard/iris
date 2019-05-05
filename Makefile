
REL=_build/default/rel/iris
BIN=${REL}/bin/iris
IMG=jonasrichard/iris:0.0.2


.PHONY: docker-build docker-console

docker-build:
	docker build -t ${IMG} --no-cache .

docker-console:
	docker run -ti --network=iris_default ${IMG} /bin/bash

cover:
	MIX_ENV=test mix coveralls

release:
	MIX_ENV=prod mix release --env=prod

start:
	${BIN} start

stop:
	${BIN} stop

console:
	${BIN} console

