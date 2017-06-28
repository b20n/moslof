-module(moslof_windowed_histogram_sup).
-behavior(supervisor).

-export([start_child/6]).
-export([start_link/0, init/1]).

start_child(Name, Size, Resolution, Min, Max, SigFig) ->
    supervisor:start_child(
        ?MODULE, [
            Name,
            Size,
            Resolution,
            Min,
            Max,
            SigFig
        ]
    ).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 1, 5}, [{
        moslof_windowed_histogram,
        {moslof_windowed_histogram, start_link, []},
        transient,
        1000,
        worker,
        [moslof_windowed_histogram]
    }]}}.

