FROM elixir:1.8.1

WORKDIR /src
RUN git clone https://github.com/jonasrichard/iris.git

WORKDIR /src/iris
RUN mix local.hex --force; \
    mix local.rebar --force; \
    mix deps.get; \
    MIX_ENV=prod mix release

#RUN cd /srv && git clone https://github.com/jonasrichard/iris.git
#RUN cd /srv/iris && git pull
#RUN cd /srv/iris && mix deps.get

#COPY docker-init.sh /srv/docker-init.sh
#COPY iris.sh /srv/iris.sh

FROM alpine:latest

WORKDIR /srv
COPY --from=0 /src/iris/_build/prod/iris /srv/iris

ENTRYPOINT /bin/bash

