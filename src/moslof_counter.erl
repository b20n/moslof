-module(moslof_counter).

-export([
    new/1,
    inc/1,
    inc/2,
    dec/1,
    dec/2,
    read/1,
    clear/1,
    delete/1
]).

-include("moslof.hrl").

-define(WIDTH, 16).

new(Name) ->
    Counters = [{{Name, N}, 0} || N <- lists:seq(0, ?WIDTH - 1)],
    ets:insert(?COUNTER_TABLE, Counters).

inc(Name) ->
    ets:update_counter(?COUNTER_TABLE, key(Name), 1).

inc(Name, Value) ->
    ets:update_counter(?COUNTER_TABLE, key(Name), Value).

dec(Name) ->
    ets:update_counter(?COUNTER_TABLE, key(Name), -1).

dec(Name, Value) ->
    ets:update_counter(?COUNTER_TABLE, key(Name), -Value).

read(Name) ->
    Match = [{{{Name, '_'}, '$1'}, [], ['$1']}],
    lists:sum(ets:select(?COUNTER_TABLE, Match)).

clear(Name) ->
    new(Name).

delete(Name) ->
    Counters = [{Name, N} || N <- lists:seq(0, ?WIDTH - 1)],
    [ets:delete(?COUNTER_TABLE, Counter) || Counter <- Counters],
    ok.

key(Name) ->
    {Name, erlang:system_info(scheduler_id) band (?WIDTH - 1)}.

