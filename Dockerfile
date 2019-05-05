FROM elixir:1.8.1

RUN mkdir -p /srv
WORKDIR /srv

RUN mix local.hex --force \
 && mix local.rebar --force

ENV MIX_ENV=prod

COPY . .
RUN mix deps.get && mix deps.compile

RUN mix compile && MIX_ENV=prod mix release --no-tar --verbose --env=prod

ENTRYPOINT /srv/_build/prod/rel/iris/bin/iris foreground

