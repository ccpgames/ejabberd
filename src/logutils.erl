%%%-------------------------------------------------------------------
%%% @author snorri
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Mar 2018 10:05
%%%-------------------------------------------------------------------
-module(logutils).
-author("snorri.sturluson@ccpgames.com").

%% API
-export([proplist_to_json/1, any_to_binary/1]).

proplist_to_json(Proplist) ->
    Lines = proplist_to_json_fields(Proplist),
    Combined = join_without_trailing_separator(Lines, ", "),
    lists:flatten(io_lib:format("{~s}", [Combined])).

proplist_to_json_fields(Proplist) ->
    proplist_to_json_fields(Proplist, []).

proplist_to_json_fields([], Acc) -> Acc;
proplist_to_json_fields([{Key, Value}], Acc) ->
    KeyB = any_to_binary(Key),
    ValueB = any_to_binary(Value),
    Line = lists:flatten(io_lib:format("\"~s\": \"~s\"", [(KeyB), (ValueB)])),
    lists:append([Line], Acc);

proplist_to_json_fields([H|T], Acc) ->
    proplist_to_json_fields(T, proplist_to_json_fields([H], Acc)).

join_without_trailing_separator([], _Sep) ->
    [];
join_without_trailing_separator([H, T], Sep) ->
    H ++ Sep ++ T;
join_without_trailing_separator([H|T], Sep) ->
    join_without_trailing_separator(T, H ++ Sep, Sep).

join_without_trailing_separator([], Acc, _Sep) ->
    Acc;
join_without_trailing_separator([H], Acc, _Sep) ->
    Acc ++ H;
join_without_trailing_separator([H|T], Acc, Sep) ->
    join_without_trailing_separator(T, Acc ++ H ++ Sep, Sep).

any_to_binary(V) when is_atom(V)    -> any_to_binary(atom_to_list(V));
any_to_binary(V) when is_pid(V)     -> any_to_binary(pid_to_list(V));
any_to_binary(V) when is_list(V)    -> list_to_binary(V);
any_to_binary(V) when is_integer(V) -> integer_to_binary(V);
any_to_binary(V) when is_binary(V)  -> V.
