-module(mod_pi).

-behaviour(gen_mod).
-behaviour(gen_server).

-define(FETCH_INTERVAL, timer:minutes(60)).

%%%===================================================================
%%% gen_mod API
%%%===================================================================

-export([start/2,stop/1, reload/3, depends/2, mod_opt_type/1]).

%%%===================================================================
%%% gen_Server API
%%%===================================================================

-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================
-export([filter_packet/1]).

-include("xmpp.hrl").
-include("ejabberd.hrl").
-include("logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {host = <<"">> :: binary()}).

%%%===================================================================
%%% gen_mod API
%%%===================================================================
start(_Host, _Opts) ->
    gen_mod:start_child(?MODULE, _Host, _Opts).

stop(_Host) ->
    gen_mod:stop_child(?MODULE, _Host).
	
reload(_Host, _NewOpts, _OldOpts) ->
    ok.
	
depends(_Host, _Opts) ->
    [].

-spec mod_opt_type(bannedwords_replace_endpoint) -> fun((binary()) -> binary());
		(bannedwords_regex_endpoint) -> fun((binary()) -> binary()).
mod_opt_type(bannedwords_regex_endpoint) -> fun iolist_to_binary/1;	
mod_opt_type(bannedwords_replace_endpoint) -> fun iolist_to_binary/1;
mod_opt_type(_) ->
    [bannedwords_regex_endpoint, bannedwords_replace_endpoint].
	
%%%===================================================================
%%% gen_Server API
%%%===================================================================
init([Host, _Opts]) ->
    process_flag(trap_exit, true),
	ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 0),
    
    catch ets:new(compiled_regexes, [named_table, public, set]),
    erlang:send_after(timer:seconds(5), self(), {fetch_regex, Host}),
    catch ets:new(bw_replacewords, [named_table, public, set]),
    erlang:send(self(), {fetch_replace, Host}),
    {ok, #state{host = Host}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    ?ERROR_MSG("got unexpected cast = ~p", [_Msg]),
    {noreply, State}.

handle_info({fetch_regex, Host}, State) ->
    %% Fetch the regex list from the web service
    fetch_regex(Host),
    erlang:send_after(?FETCH_INTERVAL, self(), {fetch_regex, Host}),
    {noreply, State};

handle_info({fetch_replace, Host}, State) ->
    fetch_replacewords(Host),
    {noreply, State};
	
handle_info(_Info, State) ->
    ?ERROR_MSG("got unexpected info = ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _) ->
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, filter_packet, 0).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% compile the regex list and return the hash list of it
process_regex_list([{H}|T]) ->
    Hash = proplists:get_value(<<"hash">>, H),
    case ets:lookup(compiled_regexes, Hash) of
	[] ->
            Regex = proplists:get_value(<<"regex">>, H),
	    {Status, Pattern} = re:compile(Regex, [unicode]),
	    case Status of
        	ok ->
            	    ets:insert(compiled_regexes, {Hash, Pattern});
        	_ ->
            	    ?ERROR_MSG("Failed to compile regex ~n~w", [Status])
            end;
	_ ->
	    ?DEBUG("Regex ~n~s already existing", [Hash])
    end,
    [{Hash} | process_regex_list(T)];
process_regex_list([]) -> [].

get_hashlist_from_ets() ->
    ets:select(compiled_regexes, ets:fun2ms(fun({H, P}) -> H end)).

get_patternlist_from_ets() ->
    ets:select(compiled_regexes, ets:fun2ms(fun({H, P}) -> P end)).

%% remove the patterns that are not in the latest regex list
remove_deleted_pattern_from_ets([H|T], NewHashList) ->
    case lists:keyfind(H,1,NewHashList) of
	false ->
	    ets:delete(compiled_regexes, H),
	    ?DEBUG("removed deleted pattern in hash: ~n~w", [H]),
	    remove_deleted_pattern_from_ets(T, NewHashList);
	_ ->
	    remove_deleted_pattern_from_ets(T, NewHashList)
    end;
remove_deleted_pattern_from_ets([], _) -> [].

get_replacewords_from_ets() ->
    ets:lookup_element(bw_replacewords, <<"ReplaceWords">>, 2).

-spec fetch_replacewords(binary()) -> ok.
fetch_replacewords(Host) ->
    Replace_EndPoint = ejabberd_config:get_option({bannedwords_replace_endpoint, Host}, <<"http://10.3.20.61:8000/banned_words/replaceword/">>),
    ?DEBUG("Fetching replacement words from ~n~s", [Replace_EndPoint]),
    {Status, {Err, _, Res}} = httpc:request(binary_to_list(Replace_EndPoint)),
    {_, ErrCode, _} = Err,

    case ErrCode of
        200 ->
            {Response} = jiffy:decode(Res),
	    {RepList} = proplists:get_value(<<"result">>, Response),
            RepWords = proplists:get_value(<<"replace">>, RepList),
	    ets:insert(bw_replacewords, {<<"ReplaceWords">>, RepWords});
	_ ->
	    ?ERROR_MSG("Error connecting to ~s to fetch replacement ~n~w", [Replace_EndPoint, Status])
    end.

-spec fetch_regex(binary()) -> ok.
fetch_regex(Host) ->
    Regexes_EndPoint = ejabberd_config:get_option({bannedwords_regex_endpoint, Host}, <<"http://10.3.20.61:8000/banned_words/regexes/">>),
    ?DEBUG("Fetching Regexes from ~n~s", [Regexes_EndPoint]),
    
    {Status, {Err, _, Res}} = httpc:request(binary_to_list(Regexes_EndPoint)),
    {_, ErrCode, _} = Err,	
    case ErrCode of
	    200 ->
	        {Response} = jiffy:decode(Res),
		RegexList = proplists:get_value(<<"result">>, Response),
		NewHashList = process_regex_list(RegexList),
                PL_IN_ETS = get_hashlist_from_ets(),
		remove_deleted_pattern_from_ets(PL_IN_ETS, NewHashList);	
	    _ ->
	        ?ERROR_MSG("Error connecting to ~s to fetch regex~n~w", [Regexes_EndPoint, Status]), 
	        Err
	end.

%% run through all the re pattern for words check
-spec match_words_regex_list([],_) -> true.
match_words_regex_list([H|T], Words) ->
    case re:run(Words, H) of
	nomatch ->
	    match_words_regex_list(T, Words);
	_ ->
	    false
    end;	
match_words_regex_list([], _) -> true.

-spec validate_message(binary()) -> true.
validate_message(MessageText) ->
    Regex_Pattern_List = get_patternlist_from_ets(),
    match_words_regex_list(Regex_Pattern_List, MessageText).

-spec inject_body(stanza(), binary()) -> stanza().
inject_body(#message{} = Pkt, Msg) ->
    NewBody = xmpp:mk_text(Msg),
    Pkt#message{body = NewBody}.

	
%%%===================================================================
%%% API
%%%===================================================================	
-spec filter_packet(Pkt) -> Pkt.
filter_packet(#message{body = Body} = Pkt) ->
    ?DEBUG("filter_packet: ~n~s", [xmpp:pp(Pkt)]),
    case Body of
	[]->
	    Pkt;
	_->
	    Text = xmpp:get_text(Body),
    	    case xmpp:get_type(Pkt) of
  		chat ->
		    case validate_message(Text) of
			false ->
			    inject_body(Pkt, get_replacewords_from_ets());
			true ->
			    Pkt
		    end;
		groupchat ->
		    case validate_message(Text) of
			false ->
			    inject_body(Pkt, get_replacewords_from_ets());
			true ->
			    Pkt
		    end;
		_->
	    	    Pkt
    	    end
    end;
filter_packet(#iq{} = Pkt) ->
    Pkt;
filter_packet(#presence{} = Pkt) ->
    Pkt.
