-module(moslof_gauge).

-export([
    new/1,
    update/2,
    read/1,
    clear/1,
    delete/1
]).

-include("moslof.hrl").

new(Name) ->
    ets:insert(?GAUGE_TABLE, {Name, 0}).

update(Name, Value) ->
    ets:insert(?GAUGE_TABLE, {Name, Value}).

read(Name) ->
    [{_, Value}] = ets:lookup(?GAUGE_TABLE, Name),
    Value.

clear(Name) ->
    new(Name).

delete(Name) ->
    ets:delete(?GAUGE_TABLE, Name).

