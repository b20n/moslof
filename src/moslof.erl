-module(moslof).
-export([start/0, stop/0]).

start() ->
    application:start(moslof).

stop() ->
    application:stop(moslof).
