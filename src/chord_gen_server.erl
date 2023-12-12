-module(chord_gen_server).
-behaviour(gen_server).

-include("chord_types.hrl").
-import(chord_utils, [hash/1]).
-import(chord_ft_utils, [ft_new/2]).
-import(chord_api, [rpc_findSuccessor/2, initFingerTable/2]).
-import(chord_circular_interval, [isInInterval/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%
% gen_server callbacks
%
-spec init(Args) -> Result when
    Args::list(atom()),
    Result::any().

init([NodeName]) -> 
    State = initState(NodeName),
    {ok, State};

init([NodeName, BootstrapNode]) ->
    State = initState(NodeName),
    UpdatedState = initFingerTable(State, BootstrapNode),
    %notify(UpdatedState),
    {ok, UpdatedState}.

%
% handlers
%
-spec handleFindSuccessor(Node, Remote, Id) -> any() when
    Node :: state(),
    Remote :: chord_node(),
    Id :: id().

handleFindSuccessor(Node, Remote, Id) ->
    % case findSuccessor(Node, Id) of
    %     #foundSuccessor{targetId = Id} = Found ->
    %         sendResponse(Node, Remote, Found);
    %     #askNode{targetId = Id, nodeToAsk = Closest} ->
    %         sendRequest(Node, Closest, #findSuccessor{targetId = Id}),
    %         receive
    %             #response{remote = Closest, payload = #foundSuccessor{targetId = Id} = Found} ->
    %                 sendResponse(Node, Remote, Found)
    %         end
    % end.
    todo.


-spec handleNotify(State, Remote) -> UpdatedState when
    State::state(),
    Remote::chord_node(),
    UpdatedState::state().

handleNotify(#state{pred = Pred} = State, Remote) when Pred =:= nil -> 
    State#state{pred = Remote};
handleNotify(#state{this = This, pred = Pred} = State, Remote) ->
    Interval = #interval_Open_Open{left = Pred#chord_node.id, right = This#chord_node.id},
    case isInInterval(Remote#chord_node.id, Interval) of
        true -> State#state{pred = Remote};
        false -> State
    end.


-spec handleGetKey(State, Remote, Key) -> Result when
    State::state(),
    Remote::chord_node(),
    Key::key(),
    Result:: foundEntry() | entryNotFound().

% if I am the only Node
handleGetKey(#state{this=This, pred=This, kvstore=KVStore}, Remote, Key) ->
    % check if the hash of key is range of me
    EntryId = hash(Key),
    #entryNotFound{}.


%
% helpers
%
-spec initState(NodeName) -> state() when
    NodeName::atom().

initState(NodeName) ->
    This = #chord_node{id = hash(NodeName), pid = NodeName},
    Ft = ft_new(?M, This),
    #state{this = This, pred = This, ft = Ft}.





