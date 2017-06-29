-module(moslof_histogram).

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
    list([ets:first(?HISTOGRAM_TABLE)]).

list(['$end_of_table'|Ks]) ->
    Ks;
list([Key|_]=Ks) ->
    list([ets:next(?HISTOGRAM_TABLE, Key)|Ks]).

new(Name) ->
    {ok, Histogram} = bukkit_hdr:new(),
    ets:insert(?HISTOGRAM_TABLE, {Name, Histogram}).

update(Name, Value) ->
    [{_, Histogram}] = ets:lookup(?HISTOGRAM_TABLE, Name),
    bukkit_hdr:update(Histogram, Value).

read(Name) ->
    [{_, Histogram}] = ets:lookup(?HISTOGRAM_TABLE, Name),
    bukkit_hdr:read(Histogram).

clear(Name) ->
    new(Name).

delete(Name) ->
    ets:delete(?HISTOGRAM_TABLE, Name).

