ARG ELIXIR="1.19.2"
ARG ERLANG="28.1"
ARG DEBIAN="bookworm-20260223-slim"
ARG OS="debian-${DEBIAN}"
FROM "hexpm/elixir:${ELIXIR}-erlang-${ERLANG}-${OS}" as builder

WORKDIR /todo-project

ENV MIX_ENV=prod

COPY mix.exs mix.lock ./
COPY config config
COPY lib lib

RUN mix deps.get --only prod

RUN mix release

FROM debian:${DEBIAN}

WORKDIR /todo-project

RUN apt-get update -y && apt-get install -y openssl locales

COPY \
    --from=builder \
    --chown=nobody:root \
    /todo-project/_build/prod/rel/todo ./

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen

ENV LANG="en_US.UTF-8"
ENV LANGUAGE="en_US:en"
ENV LC_ALL="en_US.UTF-8"

CMD ["/todo-project/bin/todo", "start_iex"]

