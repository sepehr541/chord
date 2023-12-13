%%%-------------------------------------------------------------------
%% @doc chord public API
%% @end
%%%-------------------------------------------------------------------

-module(chord_app).

-behaviour(application).

-include("chord_types.hrl").

-export([start/2, stop/1]).

-import(chord_request_handlers, [handleRequest/3, findSuccessor/2]).
-import(chord_utils, [isIdInRange/3, hash/1]).

start(_StartType, _StartArgs) ->
    chord_sup:start_link().

stop(_State) ->
    ok.

-spec pidToNode(Pid) -> Node when
    Pid :: pid(),
    Node :: chord_node().

pidToNode(Pid) ->
    #chord_node{id = hash(Pid), pid = Pid}.




