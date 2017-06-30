-module(moslof_histogram).

-export([
    list/0,
    new/1,
    update/2,
    read/1,
    clear/1,
    delete/1,
    format_statistics/1
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
    case ets:insert_new(?HISTOGRAM_TABLE, {Name, Histogram}) of
        true -> ok;
        false -> {error, exists}
    end.

update(Name, Value) ->
    [{_, Histogram}] = ets:lookup(?HISTOGRAM_TABLE, Name),
    bukkit_hdr:update(Histogram, Value).

read(Name) ->
    [{_, Histogram}] = ets:lookup(?HISTOGRAM_TABLE, Name),
    format_statistics(bukkit_hdr:read(Histogram)).

clear(Name) ->
    new(Name).

delete(Name) ->
    ets:delete(?HISTOGRAM_TABLE, Name).

format_statistics({Min, Max, P50, P75, P90, P99, P999, Mean, undefined}) ->
    format_statistics({Min, Max, P50, P75, P90, P99, P999, Mean, 0.0});
format_statistics({Min, Max, P50, P75, P90, P99, P999, undefined, StdDev}) ->
    format_statistics({Min, Max, P50, P75, P90, P99, P999, 0.0, StdDev});
format_statistics({Min, Max, P50, P75, P90, P99, undefined, Mean, StdDev}) ->
    format_statistics({Min, Max, P50, P75, P90, P99, 0.0, Mean, StdDev});
format_statistics({Min, Max, P50, P75, P90, undefined, P999, Mean, StdDev}) ->
    format_statistics({Min, Max, P50, P75, P90, 0.0, P999, Mean, StdDev});
format_statistics({Min, Max, P50, P75, undefined, P99, P999, Mean, StdDev}) ->
    format_statistics({Min, Max, P50, P75, 0.0, P99, P999, Mean, StdDev});
format_statistics({Min, Max, P50, undefined, P90, P99, P999, Mean, StdDev}) ->
    format_statistics({Min, Max, P50, 0.0, P90, P99, P999, Mean, StdDev});
format_statistics({Min, Max, undefined, P75, P90, P99, P999, Mean, StdDev}) ->
    format_statistics({Min, Max, 0.0, P75, P90, P99, P999, Mean, StdDev});
format_statistics({Min, undefined, P50, P75, P90, P99, P999, Mean, StdDev}) ->
    format_statistics({Min, 0.0, P50, P75, P90, P99, P999, Mean, StdDev});
format_statistics({undefined, Max, P50, P75, P90, P99, P999, Mean, StdDev}) ->
    format_statistics({0.0, Max, P50, P75, P90, P99, P999, Mean, StdDev});
format_statistics({Min, Max, P50, P75, P90, P99, P999, Mean, StdDev}) ->
    [
        {min, Min},
        {max, Max},
        {arithmetic_mean, Mean},
        {standard_deviation, StdDev},
        {percentile, [{50, P50}, {75, P75}, {90, P90}, {99, P99}, {999, P999}]}
    ].
