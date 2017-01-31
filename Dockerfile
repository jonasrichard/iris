
FROM erlang:18.3

RUN cd /srv && git clone https://github.com/jonasrichard/iris.git
RUN cd /srv/iris && git pull
RUN cd /srv/iris && make rebar compile release 

ENTRYPOINT /srv/iris/_build/default/rel/iris/bin/iris console -noshell -noinput

