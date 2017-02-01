#!/bin/bash

IRIS=/srv/iris/_build/default/rel/iris
IP=$( ip -f inet addr show eth0 | grep -Po 'inet \K[\d.]+' )

cd ${IRIS}
sed -i "/-sname/ s/iris/iris@$IP/" releases/0.1/vm.args
sed -i "/-sname/ s/sname/name/" releases/0.1/vm.args

