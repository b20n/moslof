-module(moslof_sup).
-behavior(supervisor).

-export([start_link/0, init/1]).

-include("moslof.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ets:new(
        ?COUNTER_TABLE,
        [set, named_table, public, {write_concurrency, true}]
    ),
    ets:new(
        ?GAUGE_TABLE,
        [set, named_table, public, {write_concurrency, true}]
    ),
    ets:new(
        ?HISTOGRAM_TABLE,
        [set, named_table, public, {read_concurrency, true}]
    ),
    ets:new(
        ?WINDOWED_HISTOGRAM_TABLE,
        [set, named_table, public, {read_concurrency, true}]
    ),
    ets:new(
        ?WINDOWED_HISTOGRAM_PIDS_TABLE,
        [set, named_table, public, {read_concurrency, true}]
    ),
    {ok, {{one_for_one, 5, 10}, [
        {moslof_windowed_histogram_sup, {
            moslof_windowed_histogram_sup, start_link, []
        }, permanent, 5000, worker, [moslof_windowed_histogram_sup]
    }]}}.

