%%%-------------------------------------------------------------------
%%% @author snorri
%%% @copyright (C) 2018, CCP hf
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2018 13:36
%%%-------------------------------------------------------------------
-module(mod_logstats).
-author("snorri.sturluson@ccpgames.com").

-behavior(gen_mod).
-behavior(gen_server).

%% gen_mod/supervisor callbacks.
-export([start/2, stop/1, reload/3, depends/2, mod_opt_type/1]).

%% gen_server callbacks.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-record(state, {
    server_host :: binary(),
    timer :: timer:tref(),
    datadog_url :: binary()
}).

-type state() :: #state{}.


start(Host, Opts) ->
    ?INFO_MSG("mod_logstats starting on ~s", [Host]),
    gen_mod:start_child(?MODULE, Host, Opts).

stop(Host) ->
    ?INFO_MSG("mod_logstats stopping on ~s", [Host]),
    gen_mod:stop_child(?MODULE, Host).

reload(_Host, _NewOpts, _OldOpts) ->
    erlang:error(not_implemented).

depends(_Host, _Opts) ->
    [].

mod_opt_type(interval) -> fun (I) when is_integer(I), I > 0 -> I end;
mod_opt_type(_) ->
    [interval].

init([ServerHost, Opts]) ->
    Interval = gen_mod:get_opt(interval, Opts, 60),
    Url = get_datadog_url(),
    ?INFO_MSG("mod_logstats starting timer on ~s with ~p seconds interval on ~s",
        [ServerHost, Interval, Url]),
    {ok, Timer} = timer:send_interval(timer:seconds(Interval), logstats),
    {ok, #state{server_host = ServerHost, timer = Timer, datadog_url = Url}}.

handle_info(logstats, State) ->
    Server = State#state.server_host,

    UserCount = length(ejabberd_sm:dirty_get_my_sessions_list()),
    UserCountMetric = format_metric(user_count, UserCount, Server),

    ProcessCount = length(erlang:processes()),
    ProcessCountMetric = format_metric(process_count, ProcessCount, Server),

    TotalMemory = erlang:memory(total),
    TotalMemoryMetric = format_metric(memory_total, TotalMemory, Server),

    ProcessesMemory = erlang:memory(processes),
    ProcessesMemoryMetric = format_metric(memory_processes, ProcessesMemory, Server),

    SystemMemory = erlang:memory(system),
    SystemMemoryMetric = format_metric(memory_system, SystemMemory, Server),

    AtomMemory = erlang:memory(atom),
    AtomMemoryMetric = format_metric(memory_atom, AtomMemory, Server),

    BinaryMemory = erlang:memory(binary),
    BinaryMemoryMetric = format_metric(memory_binary, BinaryMemory, Server),

    EtsMemory = erlang:memory(ets),
    EtsMemoryMetric = format_metric(memory_ets, EtsMemory, Server),

    RunQueue = erlang:statistics(run_queue),
    RunQueueMetric = format_metric(run_queue, RunQueue, Server),

    MetricsList = [
        UserCountMetric,
        ProcessCountMetric,
        TotalMemoryMetric,
        ProcessesMemoryMetric,
        SystemMemoryMetric,
        AtomMemoryMetric,
        BinaryMemoryMetric,
        EtsMemoryMetric,
        RunQueueMetric
    ],
    CombinedMetrics = logutils:join_without_trailing_separator(MetricsList, ", "),

    Payload = lists:flatten(io_lib:format("{\"series\": [~s]}", [CombinedMetrics])),

    Request = {State#state.datadog_url, [{"te", "chunked"}], "application/json", Payload},
    httpc:request(post, Request, [], [{body_format, binary}]),

    {noreply, State}.

format_metric(Name, UserCount, Server) ->
    Template = "{\"metric\":\"ejabberd.~s\", \"points\": [[~p, ~p]], \"host\": \"~s\", \"tags\":[\"~s\"]}",
    lists:flatten(io_lib:format(
        Template,
        [
            logutils:any_to_binary(Name),
            erlang:system_time(second),
            UserCount,
            Server,
            logutils:any_to_binary(erlang:node())
            ])).

handle_call(Request, From, State) ->
    ?ERROR_MSG("Got unexpected request from ~p: ~p", [From, Request]),
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ?INFO_MSG("mod_logstats stopping timer on ~s", [State#state.server_host]),
    timer:cancel(State#state.timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_datadog_url() ->
    case ejabberd_config:env_binary_to_list(ejabberd, datadog_url) of
	{ok, Url} ->
	    Url;
	undefined ->
	    case os:getenv("EJABBERD_DATADOG_URL") of
		false ->
		    none;
		Url ->
		    Url
	    end
    end.

