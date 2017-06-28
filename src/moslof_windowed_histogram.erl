-module(moslof_windowed_histogram).
-behavior(gen_server).

-export([
    new/1,
    update/2,
    read/1,
    clear/1,
    delete/1
]).

-export([
    start_link/6,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("moslof.hrl").

-record(state, {
    name,
    size,
    resolution,
    min,
    max,
    sigfig,
    windows,
    last
}).

new(Name) ->
    new(Name, 10000, 1000, 1, 100000, 3).

new(Name, Size, Resolution, Min, Max, SigFig) ->
    moslof_windowed_histogram_sup:start_child(
        Name,
        Size,
        Resolution,
        Min,
        Max,
        SigFig
    ).

update(Name, Value) ->
    [{_, Histogram}] = ets:lookup(?WINDOWED_HISTOGRAM_TABLE, Name),
    bukkit_hdr:update(Histogram, Value).

read(Name) ->
    [{_, Pid}] = ets:lookup(?WINDOWED_HISTOGRAM_PIDS_TABLE, Name),
    gen_server:call(Pid, read).

clear(Name) ->
    [{_, Pid}] = ets:lookup(?WINDOWED_HISTOGRAM_PIDS_TABLE, Name),
    gen_server:call(Pid, clear).

delete(Name) ->
    [{_, Pid}] = ets:lookup(?WINDOWED_HISTOGRAM_PIDS_TABLE, Name),
    gen_server:stop(Pid).

start_link(Name, Size, Resolution, Min, Max, SigFig) ->
    Args = [Name, Size, Resolution, Min, Max, SigFig],
    gen_server:start_link(?MODULE, Args, []).

init([Name, Size, Resolution, Min, Max, SigFig]) ->
    if
        Size rem Resolution =/= 0 ->
            {stop, badarg};
        true ->
            case ets:insert_new(?WINDOWED_HISTOGRAM_PIDS_TABLE, {Name, self()}) of
                false ->
                    {stop, exists};
                true ->
                    Windows = lists:map(fun(_) ->
                        {ok, Histogram} = bukkit_hdr:new(Min, Max, SigFig),
                        Histogram
                    end, lists:duplicate(Size div Resolution , null)),
                    ets:insert(?WINDOWED_HISTOGRAM_TABLE, {Name, hd(Windows)}),
                    State = #state{
                        name = Name,
                        size = Size,
                        resolution = Resolution,
                        min = Min,
                        max = Max,
                        sigfig = SigFig,
                        windows = Windows,
                        last = os:timestamp()
                    },
                    {ok, State, Resolution}
            end
    end.

handle_call(read, _From, State0) ->
    {State, Remaining} = maybe_slide_windows(State0),
    {reply, {ok, bukkit_hdr:read(State#state.windows)}, State, Remaining};
handle_call(clear, _From, State0) ->
    #state{
        size = Size,
        resolution = Resolution,
        min = Min,
        max = Max,
        sigfig = SigFig,
        name = Name
    } = State0,
    Windows = lists:map(fun(_) ->
        {ok, Histogram} = bukkit_hdr:new(Min, Max, SigFig),
        Histogram
    end, lists:duplicate(Size div Resolution, null)),
    ets:insert(?WINDOWED_HISTOGRAM_TABLE, {Name, hd(Windows)}),
    State = State0#state{
        windows = Windows,
        last = os:timestamp()
    },
    {reply, ok, State, State#state.resolution};
handle_call(_Msg, _From, State0) ->
    {State, Remaining} = maybe_slide_windows(State0),
    {noreply, State, Remaining}.

handle_cast(_Msg, State0) ->
    {State, Remaining} = maybe_slide_windows(State0),
    {noreply, State, Remaining}.

handle_info(timeout, State) ->
    {noreply, slide_windows(State), State#state.resolution};
handle_info(_Msg, State0) ->
    {State, Remaining} = maybe_slide_windows(State0),
    {noreply, State, Remaining}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    ets:delete(?WINDOWED_HISTOGRAM_TABLE, State#state.name),
    ets:delete(?WINDOWED_HISTOGRAM_PIDS_TABLE, State#state.name),
    ok.

maybe_slide_windows(State) ->
    #state{
        resolution = Resolution,
        last = Last
    } = State,
    Elapsed = round((timer:now_diff(os:timestamp(), Last) / 1000)),
    Remaining = Resolution - Elapsed,
    case Remaining =< 0 of
        false ->
            {State, Remaining};
        true ->
            {slide_windows(State), State#state.resolution}
    end.

slide_windows(State) ->
    #state{
        name = Name,
        size = Size,
        resolution = Resolution,
        min = Min,
        max = Max,
        sigfig = SigFig,
        windows = Windows0
    } = State,
    {ok, Latest} = bukkit_hdr:new(Min, Max, SigFig),
    ets:insert(?WINDOWED_HISTOGRAM_TABLE, {Name, Latest}),
    Windows = [Latest|lists:sublist(Windows0, (Size div Resolution) - 1)],
    State#state{
        windows = Windows,
        last = os:timestamp()
    }.
