-module(moslof_gauge).

-export([
    list/0,
    new/1,
    update/2,
    read/1,
    clear/1,
    delete/1
]).

-include("moslof.hrl").

list() ->
    list([ets:first(?GAUGE_TABLE)]).

list(['$end_of_table'|Ks]) ->
    Ks;
list([Key|_]=Ks) ->
    list([ets:next(?GAUGE_TABLE, Key)|Ks]).

new(Name) ->
    case ets:insert_new(?GAUGE_TABLE, {Name, 0}) of
        true -> ok;
        false -> {error, exists}
    end.

update(Name, Value) ->
    ets:insert(?GAUGE_TABLE, {Name, Value}).

read(Name) ->
    [{_, Value}] = ets:lookup(?GAUGE_TABLE, Name),
    Value.

clear(Name) ->
    new(Name).

delete(Name) ->
    ets:delete(?GAUGE_TABLE, Name).

