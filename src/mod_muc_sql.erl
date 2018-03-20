%%%-------------------------------------------------------------------
%%% File    : mod_muc_sql.erl
%%% Author  : Evgeny Khramtsov <ekhramtsov@process-one.net>
%%% Created : 13 Apr 2016 by Evgeny Khramtsov <ekhramtsov@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2017   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

-behaviour(mod_muc).
-behaviour(mod_muc_room).

%% API
-export([init/2, store_room/4, restore_room/3, forget_room/3,
	 can_use_nick/4, can_use_room_name/3, get_rooms/2, get_nick/3, set_nick/4,
	 import/3, export/1]).
-export([register_online_room/4, unregister_online_room/4, find_online_room/3,
	 get_online_rooms/3, count_online_rooms/2, rsm_supported/0,
	 register_online_user/4, unregister_online_user/4,
	 count_online_rooms_by_user/3, get_online_rooms_by_user/3]).
-export([set_affiliation/6, set_affiliations/4, get_affiliation/5,
	 get_affiliations/3, search_affiliation/4]).
-export([get_system_rooms/2, get_rooms_by_title/3, get_rooms_by_affiliation/4,
     get_room_title/3, get_rooms_for_me/3]).

-include("jid.hrl").
-include("mod_muc.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

%%%===================================================================
%%% API
%%%===================================================================
init(Host, Opts) ->
    case gen_mod:ram_db_mod(Host, Opts, mod_muc) of
	?MODULE ->
	    clean_tables(Host);
	_ ->
	    ok
    end.

clear_affiliations(Opts) ->
	lists:keyreplace(affiliations, 1, Opts, {affiliations, []}).

store_room(LServer, Host, Name, Opts) ->
    ShouldStore = case Name of
        <<"local_", _/binary>> ->
			{false, Opts, false};
        <<"wormhole_", _/binary>> ->
			{false, Opts, false};
        <<"incursion_", _/binary>> ->
			{false, Opts, false};
        <<"resourcewars_", _/binary>> ->
			{false, Opts, false};
	    <<"alliance_", _/binary>> ->
			{true, clear_affiliations(Opts), false};
	    <<"corp_", _/binary>> ->
			{true, clear_affiliations(Opts), false};
	    <<"fleet_", _/binary>> ->
			{true, clear_affiliations(Opts), false};
	    <<"faction_", _/binary>> ->
			{true, clear_affiliations(Opts), false};
	    <<"player_", _/binary>> ->
			{true, Opts, true};
        _ ->
			{true, Opts, false}
	end,

	case ShouldStore of
		{true, NewOpts, StoreAffiliations} ->
			?INFO_MSG("store_room ~s", [Name]),
			Title = proplists:get_value(title, NewOpts),
			ComparisonKey = comparison_key_from_title(Title),
			Affiliations = proplists:get_value(affiliations, NewOpts),

			SOpts = misc:term_to_expr(NewOpts),
			F = fun () ->
				?SQL_UPSERT_T(
						   "muc_room",
						   ["!name=%(Name)s",
							"!host=%(Host)s",
							"opts=%(SOpts)s",
							"title=%(Title)s",
							"comparison_key=%(ComparisonKey)s"]),
				case StoreAffiliations of
					true ->
						?INFO_MSG("store_affiliations ~s", [Name]),
						ejabberd_sql:sql_query_t(
							?SQL("delete from muc_room_affiliation"
								 " where name=%(Name)s and host=%(Host)s")
						),
						lists:foreach(
							fun({JID, {A, _}}) ->
								Username = jid:to_string(JID),
								Affiliation = atom_to_list(A),
								?SQL_UPSERT_T(
									"muc_room_affiliation",
									[
										"!name=%(Name)s",
										"!host=%(Host)s",
										"!username=%(Username)s",
										"affiliation=%(Affiliation)s"
									]
								)
							end,
							Affiliations);
					_ ->
						{atomic, ok}
				end
			end,
			{atomic, _} = ejabberd_sql:sql_transaction(LServer, F);
		_ ->
			ok
    end.

comparison_key_from_title(Title) ->
	LowerCase = string:to_lower(unicode:characters_to_list(Title)),
	Tokens = string:tokens(LowerCase, " "),
	ComparisonKey = string:join(Tokens, ""),
	unicode:characters_to_binary(ComparisonKey).

restore_room(LServer, Host, Name) ->
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(opts)s from muc_room where name=%(Name)s"
                      " and host=%(Host)s")) of

	{selected, [{Opts}]} ->
	    mod_muc:opts_to_binary(ejabberd_sql:decode_term(Opts));
	_ ->
	    error
    end.

forget_room(LServer, Host, Name) ->
    F = fun () ->
		ejabberd_sql:sql_query_t(
                  ?SQL("delete from muc_room where name=%(Name)s"
                       " and host=%(Host)s"))
	end,
    ejabberd_sql:sql_transaction(LServer, F).

can_use_nick(LServer, Host, JID, Nick) ->
    SJID = jid:encode(jid:tolower(jid:remove_resource(JID))),
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(jid)s from muc_registered "
                      "where nick=%(Nick)s"
                      " and host=%(Host)s")) of
	{selected, [{SJID1}]} -> SJID == SJID1;
	_ -> true
    end.

can_use_room_name(LServer, Host, Name) ->
	case get_rooms_by_title(LServer, Host, Name) of
		[] ->
			?INFO_MSG("can_use_room_name '~s' true", [Name]),
			true;
		Result ->
			?INFO_MSG("can_use_room_name '~s' false (~p)", [Name, Result]),
			false
	end.

get_rooms(LServer, Host) ->
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(name)s, @(opts)s from muc_room"
                      " where host=%(Host)s")) of
	{selected, RoomOpts} ->
	    lists:map(
	      fun({Room, Opts}) ->
		      #muc_room{name_host = {Room, Host},
				opts = mod_muc:opts_to_binary(
					 ejabberd_sql:decode_term(Opts))}
	      end, RoomOpts);
	Err ->
	    ?ERROR_MSG("failed to get rooms: ~p", [Err]),
	    []
    end.

get_room_title(LServer, Host, Room) ->
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(title)s from muc_room"
                      " where host=%(Host)s"
                      " and name=%(Room)s")) of
    {selected, [{Title}]} ->
        Title;
	_ ->
        <<"">>
    end.

get_rooms_by_affiliation(LServer, Host, JID, Affiliation) ->
    SJID = jid:encode(jid:tolower(jid:remove_resource(JID))),
	Aff = atom_to_list(Affiliation),
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(name)s from muc_room_affiliation"
                      " where host=%(Host)s"
                      " and username=%(SJID)s"
                      " and affiliation=%(Aff)s")) of
	{selected, Rooms} ->
        RoomsWithNames = lists:flatmap(
            fun({R}) ->
                case catch ejabberd_sql:sql_query(
                            LServer,
                            ?SQL("select @(title)s from muc_room"
                                 " where host=%(Host)s"
                                 " and name=%(R)s")) of
                    {selected, [{Title}]} ->
                        [{R, Title}];
                    _ ->
                        []
                end
            end,
            Rooms
        ),
	    RoomsWithNames;
	Err ->
	    ?ERROR_MSG("failed to get rooms: ~p", [Err]),
	    []
    end.

get_rooms_for_me(LServer, Host, JID) ->
    SJID = jid:encode(jid:tolower(jid:remove_resource(JID))),
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(muc_room.name)s, @(muc_room.title)s, @(affiliation)s"
		              " from muc_room"
				      " inner join muc_room_affiliation"
				      "		on muc_room_affiliation.name=muc_room.name"
                      " where muc_room.host=%(Host)s"
				 	  "		and muc_room_affiliation.host=%(Host)s"
                      " and username=%(SJID)s"
                      " and affiliation in ('owner', 'member', 'admin')")) of
	{selected, Rooms} ->
		Rooms;
	Err ->
	    ?ERROR_MSG("failed to get rooms: ~p", [Err]),
	    []
    end.


get_system_rooms(LServer, Host) ->
	Filter = "system%",
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(name)s, @(title)s from muc_room"
                      " where host=%(Host)s"
                      " and name like %(Filter)s")) of
	{selected, Rooms} ->
		Rooms;
	Err ->
	    ?ERROR_MSG("failed to get rooms: ~p", [Err]),
	    []
    end.

get_rooms_by_title(LServer, Host, Name) ->
	ComparisonKey = comparison_key_from_title(Name),
	case string:equal(ComparisonKey, "") of
		true -> [];
		_ ->
			case catch ejabberd_sql:sql_query(
						 LServer,
						 ?SQL("select @(name)s, @(title)s from muc_room"
							  " where host=%(Host)s"
							  " and comparison_key=%(ComparisonKey)s")) of
			{selected, Rooms} ->
				Rooms;
			Err ->
				?ERROR_MSG("failed to get rooms: ~p", [Err]),
				[]
			end
	end.

get_nick(LServer, Host, From) ->
    SJID = jid:encode(jid:tolower(jid:remove_resource(From))),
    case catch ejabberd_sql:sql_query(
                 LServer,
                 ?SQL("select @(nick)s from muc_registered where"
                      " jid=%(SJID)s and host=%(Host)s")) of
	{selected, [{Nick}]} -> Nick;
	_ -> error
    end.

set_nick(LServer, Host, From, Nick) ->
    JID = jid:encode(jid:tolower(jid:remove_resource(From))),
    F = fun () ->
		case Nick of
		    <<"">> ->
			ejabberd_sql:sql_query_t(
			  ?SQL("delete from muc_registered where"
                               " jid=%(JID)s and host=%(Host)s")),
			ok;
		    _ ->
			Allow = case ejabberd_sql:sql_query_t(
				       ?SQL("select @(jid)s from muc_registered"
                                            " where nick=%(Nick)s"
                                            " and host=%(Host)s")) of
				    {selected, [{J}]} -> J == JID;
				    _ -> true
				end,
			if Allow ->
				?SQL_UPSERT_T(
                                  "muc_registered",
                                  ["!jid=%(JID)s",
                                   "!host=%(Host)s",
                                   "nick=%(Nick)s"]),
				ok;
			   true ->
				false
			end
		end
	end,
    ejabberd_sql:sql_transaction(LServer, F).

set_affiliation(_ServerHost, _Room, _Host, _JID, _Affiliation, _Reason) ->
    {error, not_implemented}.

set_affiliations(_ServerHost, _Room, _Host, _Affiliations) ->
    {error, not_implemented}.

get_affiliation(_ServerHost, _Room, _Host, _LUser, _LServer) ->
    {error, not_implemented}.

get_affiliations(_ServerHost, _Room, _Host) ->
    {error, not_implemented}.

search_affiliation(_ServerHost, _Room, _Host, _Affiliation) ->
    {error, not_implemented}.

register_online_room(ServerHost, Room, Host, Pid) ->
    PidS = misc:encode_pid(Pid),
    NodeS = erlang:atom_to_binary(node(Pid), latin1),
    case ?SQL_UPSERT(ServerHost,
		     "muc_online_room",
		     ["!name=%(Room)s",
		      "!host=%(Host)s",
		      "node=%(NodeS)s",
		      "pid=%(PidS)s"]) of
	ok ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to update 'muc_online_room': ~p", [Err]),
	    Err
    end.

unregister_online_room(ServerHost, Room, Host, Pid) ->
    %% TODO: report errors
    PidS = misc:encode_pid(Pid),
    NodeS = erlang:atom_to_binary(node(Pid), latin1),
    ejabberd_sql:sql_query(
      ServerHost,
      ?SQL("delete from muc_online_room where name=%(Room)s and "
	   "host=%(Host)s and node=%(NodeS)s and pid=%(PidS)s")).

find_online_room(ServerHost, Room, Host) ->
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("select @(pid)s, @(node)s from muc_online_room where "
		"name=%(Room)s and host=%(Host)s")) of
	{selected, [{PidS, NodeS}]} ->
	    try {ok, misc:decode_pid(PidS, NodeS)}
	    catch _:{bad_node, _} -> error
	    end;
	{selected, []} ->
	    error;
	Err ->
	    ?ERROR_MSG("failed to select 'muc_online_room': ~p", [Err]),
	    error
    end.

count_online_rooms(ServerHost, Host) ->
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("select @(count(*))d from muc_online_room "
		"where host=%(Host)s")) of
	{selected, [{Num}]} ->
	    Num;
	Err ->
	    ?ERROR_MSG("failed to select 'muc_online_room': ~p", [Err]),
	    0
    end.

get_online_rooms(ServerHost, Host, _RSM) ->
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("select @(name)s, @(pid)s, @(node)s from muc_online_room "
		"where host=%(Host)s")) of
	{selected, Rows} ->
	    lists:flatmap(
	      fun({Room, PidS, NodeS}) ->
		      try [{Room, Host, misc:decode_pid(PidS, NodeS)}]
		      catch _:{bad_node, _} -> []
		      end
	      end, Rows);
	Err ->
	    ?ERROR_MSG("failed to select 'muc_online_room': ~p", [Err]),
	    []
    end.

rsm_supported() ->
    false.

register_online_user(ServerHost, {U, S, R}, Room, Host) ->
    NodeS = erlang:atom_to_binary(node(), latin1),
    case ?SQL_UPSERT(ServerHost, "muc_online_users",
		     ["!username=%(U)s",
		      "!server=%(S)s",
		      "!resource=%(R)s",
		      "!name=%(Room)s",
		      "!host=%(Host)s",
		      "node=%(NodeS)s"]) of
	ok ->
	    ok;
	Err ->
	    ?ERROR_MSG("failed to update 'muc_online_users': ~p", [Err]),
	    Err
    end.

unregister_online_user(ServerHost, {U, S, R}, Room, Host) ->
    %% TODO: report errors
    ejabberd_sql:sql_query(
      ServerHost,
      ?SQL("delete from muc_online_users where username=%(U)s and "
	   "server=%(S)s and resource=%(R)s and name=%(Room)s and "
	   "host=%(Host)s")).

count_online_rooms_by_user(ServerHost, U, S) ->
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("select @(count(*))d from muc_online_users where "
		"username=%(U)s and server=%(S)s")) of
	{selected, [{Num}]} ->
	    Num;
	Err ->
	    ?ERROR_MSG("failed to select 'muc_online_users': ~p", [Err]),
	    0
    end.

get_online_rooms_by_user(ServerHost, U, S) ->
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("select @(name)s, @(host)s from muc_online_users where "
		"username=%(U)s and server=%(S)s")) of
	{selected, Rows} ->
	    Rows;
	Err ->
	    ?ERROR_MSG("failed to select 'muc_online_users': ~p", [Err]),
	    []
    end.

export(_Server) ->
    [{muc_room,
      fun(Host, #muc_room{name_host = {Name, RoomHost}, opts = Opts}) ->
              case str:suffix(Host, RoomHost) of
                  true ->
                      SOpts = misc:term_to_expr(Opts),
                      [?SQL("delete from muc_room where name=%(Name)s"
                            " and host=%(RoomHost)s;"),
                       ?SQL("insert into muc_room(name, host, opts) "
                            "values ("
                            "%(Name)s, %(RoomHost)s, %(SOpts)s);")];
                  false ->
                      []
              end
      end},
     {muc_registered,
      fun(Host, #muc_registered{us_host = {{U, S}, RoomHost},
                                nick = Nick}) ->
              case str:suffix(Host, RoomHost) of
                  true ->
                      SJID = jid:encode(jid:make(U, S)),
                      [?SQL("delete from muc_registered where"
                            " jid=%(SJID)s and host=%(RoomHost)s;"),
                       ?SQL("insert into muc_registered(jid, host, "
                            "nick) values ("
                            "%(SJID)s, %(RoomHost)s, %(Nick)s);")];
                  false ->
                      []
              end
      end}].

import(_, _, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
clean_tables(ServerHost) ->
    NodeS = erlang:atom_to_binary(node(), latin1),
    ?DEBUG("Cleaning SQL muc_online_room table...", []),
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("delete from muc_online_room where node=%(NodeS)s")) of
	{updated, _} ->
	    ok;
	Err1 ->
	    ?ERROR_MSG("failed to clean 'muc_online_room' table: ~p", [Err1]),
	    Err1
    end,
    ?DEBUG("Cleaning SQL muc_online_users table...", []),
    case ejabberd_sql:sql_query(
	   ServerHost,
	   ?SQL("delete from muc_online_users where node=%(NodeS)s")) of
	{updated, _} ->
	    ok;
	Err2 ->
	    ?ERROR_MSG("failed to clean 'muc_online_users' table: ~p", [Err2]),
	    Err2
    end.
