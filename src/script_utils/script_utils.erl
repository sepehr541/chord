-module(script_utils).
-import(chord_api, [join/2, create/1, rpc_getEntry/2, rpc_putEntry/3]).
-import(chord_utils, [nameToNode/1, toInt/1]).
-import(chord_ft_utils, [ft_node/2, ft_start/2, ft_interval/2]).
-include("../chord_types.hrl").

-export([
    create_ring/1,
    rand_select_from_list/1,
    perform_puts/2,
    perform_gets/2,
    generate_keys/1,
    generate_keys/2,
    perform_for_keys/3,
    printFT/1,
    printInterval/1
]).
-export([average/1, std/2]).

%
% Ring Setup
%
create_ring(Size) ->
    Node1 = node1,
    % Start node1
    {ok, _} = create(Node1),

    JoinedNodes = lists:map(
        fun(N) ->
            Name = list_to_atom("node" ++ integer_to_list(N)),
            join(Name, Node1),
            nameToNode(Name)
        end,
        lists:seq(2, Size)
    ),
    [nameToNode(Node1)] ++ JoinedNodes.

%
% KV setup
%

rand_select_from_list(List) ->
    lists:nth(rand:uniform(length(List)), List).

generate_keys(M) ->
    lists:map(fun(I) -> "key" ++ integer_to_list(I) end, lists:seq(1, M)).

generate_keys(M, KeySize) ->
    lists:map(
        fun(I) ->
            Tag = integer_to_list(I),
            lists:duplicate(KeySize - length(Tag), $a) ++ Tag
        end,
        lists:seq(1, M)
    ).

perform_for_keys(Fun, Nodes, Keys) ->
    lists:map(
        fun(Key) ->
            Fun(Nodes, Key)
        end,
        Keys
    ).

time_func_gen(Fun) ->
    fun(Nodes, Key) ->
        Node = rand_select_from_list(Nodes),
        StartTime = erlang:monotonic_time(),
        % Assuming put_operation/2 is your Put function
        Fun(Node, Key),
        EndTime = erlang:monotonic_time(),
        erlang:convert_time_unit(EndTime - StartTime, native, microsecond)
    end.

perform_puts(Nodes, Keys) ->
    perform_for_keys(
        time_func_gen(fun(Node, Key) ->
            % io:format("~p <- ~p~n", [Node#chord_node.ref, Key]),
            rpc_putEntry(Node, Key, Key)
        end),
        Nodes,
        Keys
    ).

perform_gets(Nodes, Keys) ->
    perform_for_keys(time_func_gen(fun(Node, Key) -> rpc_getEntry(Node, Key) end), Nodes, Keys).

%
% Math functions
%
average(List) ->
    sum(List) / length(List).

sum(List) ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, List).

std(List, Avg) ->
    SqrSum = lists:foldl(fun(X, Acc) -> (X - Avg) * (X - Avg) + Acc end, 0, List),
    math:sqrt(SqrSum / length(List)).

%
% Fingertable
%

-define(printIntervalCommon(LSym, RSym),
    LSym ++ integer_to_list(toInt(Left)) ++ ", " ++ integer_to_list(toInt(Right)) ++ RSym
).

printInterval(#interval_Closed_Open{left = Left, right = Right}) ->
    ?printIntervalCommon("[", ")");
printInterval(#interval_Open_Closed{left = Left, right = Right}) ->
    ?printIntervalCommon("(", "]");
printInterval(#interval_Closed_Closed{left = Left, right = Right}) ->
    ?printIntervalCommon("[", "]");
printInterval(#interval_Open_Open{left = Left, right = Right}) ->
    ?printIntervalCommon("(", ")").

printFT(State) ->
    #chord_node{id = Id, ref= Ref } = State#state.this,
    io:format("State: { id = ~p, ref = ~p }~n", [toInt(Id), Ref]),
    io:format("Index, Start, Interval, Node~n", []),
    array:map(
        fun(Index, _) ->
            K = Index + 1,
            io:format(
                "~p, ~p, ~p, ~p~n",
                [
                    K,
                    toInt(ft_start(State, K)),
                    printInterval(ft_interval(State, K)),
                    (ft_node(State, K))#chord_node.ref
                ]
            )
        end,
        State#state.ft
    ).
