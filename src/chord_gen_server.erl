-module(chord_gen_server).
-behaviour(gen_server).

-include("chord_types.hrl").
-import(chord_utils, [hash/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
-export([create/1, join/2]).

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
    UpdatedState = initFingerTable(State, BootstrapNode)
    notify(UpdatedState),
    {ok, UpdatedState}.





%
% API
%
-spec create(NodeName::atom()) -> any().
create(NodeName) ->
    Name = {local, NodeName},
    gen_server:start_link(Name, ?MODULE, [NodeName], []).

-spec join(NodeName, BootstrapNode) -> any() when
    NodeName::atom(),
    BootstrapNode::atom().

join(NodeName, BootstrapNode) ->
    % check if bootstrapNode is registered
    case whereis(BootstrapNode) of
        undefined -> error("Bootstrap node does not exist");
        _ -> gen_server:start_link({local, NodeName}, ?MODULE, [NodeName, BootstrapNode], [])
    end.

-spec rpc_findSuccessor(LocalState, Remote, Id) -> Successor when
    LocalState::state(),
    Remote::atom(),
    Id::id(),
    Successor::chord_node().

rpc_findSuccessor(LocalState, Remote, Id) ->
    gen_server:call(Remote, #findSuccessor{targetId=Id}).



%
% helpers
%
-spec newFingerTable(Size, DefaultValue) -> fingertable() when
    Size::pos_integer(),
    DefaultValue::any().

newFingerTable(Size, DefaultValue) ->
    array:new([{size, Size}, {default, DefaultValue}, {fixed, true}]).

-spec initState(NodeName) -> state() when
    NodeName::atom().

initState(NodeName) ->
    This = #chord_node{id = hash(NodeName), pid = NodeName},
    Ft = newFingerTable(?M, This),
    #state{this = This, pred = This, ft = Ft}.


-spec initFingerTable(State, BootstrapNode) -> UpdatedState when
    State::state(),
    BootstrapNode::atom(),
    UpdatedState::state().

initFingerTable(State, BootstrapNode) ->
    Successor = rpc_findSuccessor(State, BootstrapNode, todo).



