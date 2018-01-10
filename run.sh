#!/usr/bin/env bash

EJABBERD_CONFIG_PATH=ejabberd.yml erl -sname ejabberd -pa ebin -pa deps/*/ebin -pa test -pa deps/elixir/lib/*/ebin/ -s ejabberd
