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
    timer :: timer:tref()
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
    ?INFO_MSG("mod_logstats starting timer on ~s with ~p seconds interval",
        [ServerHost, Interval]),
    {ok, Timer} = timer:send_interval(timer:seconds(Interval), logstats),
    {ok, #state{server_host = ServerHost, timer = Timer}}.

handle_info(logstats, State) ->
    Server = State#state.server_host,
    UserCount = length(ejabberd_sm:dirty_get_my_sessions_list()),
    ProcessCount = length(erlang:processes()),
    ?INFO_MSG("{server: ~s, num_users_online: ~p, num_processes: ~p}", [Server, UserCount, ProcessCount]),
    {noreply, State}.

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

