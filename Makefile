
REL=_build/default/rel/iris
BIN=${REL}/bin/iris
VER=0.1
IMG=jonasrichard/iris:0.0.2


.PHONY: docker-build docker-console

#rebar: rebar
#	curl -O https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

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

test-build:
	rsync -rl ${REL} _build/sut1
	rsync -rl ${REL} _build/sut2
	unlink _build/sut1/iris/releases/0.1/sys.config
	unlink _build/sut2/iris/releases/0.1/sys.config
	config/replace.sh config/sys.config _build/sut1/iris/releases/0.1/sys.config iris.http.port=9080
	config/replace.sh config/sys.config _build/sut2/iris/releases/0.1/sys.config iris.http.port=9081
	sed -i '' "s/-sname iris/-name iris1@${IP}/g" _build/sut1/iris/releases/0.1/vm.args
	sed -i '' "s/-sname iris/-name iris2@${IP}/g" _build/sut2/iris/releases/0.1/vm.args

test-start:
	_build/sut1/iris/bin/iris start
	_build/sut2/iris/bin/iris start

test-stop:
	_build/sut1/iris/bin/iris stop
	_build/sut2/iris/bin/iris stop

