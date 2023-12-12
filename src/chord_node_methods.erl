-module(chord_node_methods).
-include("chord_types.hrl").
-import(chord_circular_interval, [isInInterval/2]).

-export([closestPrecedingFinger/2]).

-spec closestPrecedingFinger(State, Id) -> foundSuccessor() | askNode() when
    State :: state(),
    Id :: id().

closestPrecedingFinger(#state{this = Node, ft = Ft}, Id) ->
    % ask node in FT
    Fun = fun(_, Entry, Acc) ->
        case Acc of
            undefined ->
                EntryNodeId = Entry#fte.successor#chord_node.id,
                Interval = #interval_Open_Open{left =  Node#chord_node.id, right = Id},
                case isInInterval(EntryNodeId, Interval) of
                    true -> #askNode{targetId = Id, nodeToAsk = Entry};
                    false -> Acc
                end;
            _ ->
                Acc
        end
    end,
    Result = array:sparse_foldr(Fun, undefined, Ft), % right to left
    case Result of
        % I am the successor
        undefined -> #foundSuccessor{targetId = Id, successor = Node};
        _ -> Result
    end.


-spec findSuccessor(Node, Id) -> Successor when
    Node :: state(),
    Id :: id(),
    Successor :: chord_node().


findSuccessor(Node, Id) ->
    PredOfId = findPredecessor(Node, Id),
    getSuccessor(Node, PredOfId).


-spec findPredecessor(Node, Id) -> Predecessor when
    Node::state(),
    Id::id(),
    Predecessor::chord_node().

findPredecessor(Node, Id) ->
    findPredecessorHelper(Node, Node, Id).

-spec findPredecessorHelper(Node, NodePrime, Id) when
    Node :: state(),
    NodePrime :: chord_node(),
    Id :: id().

findPredecessorHelper(Node, NodePrime, Id) ->
    Successor = getSuccessor(Node, Node),
    Interval = #interval_Open_Closed{ left = Node#chord_node.id, right = Successor#chord_node.id},
    case isInInterval(Id, j) of
        true -> todo
    end.

-spec findSuccessorHelper(Node, Id) -> Result when
    Node :: state(),
    Id :: id(),
    Result :: foundSuccessor() | askNode().

findSuccessorHelper(State, Id) ->
    Successor = array:get(0, State#state.ft),
    CurrId = State#state.this#chord_node.id,
    SuccessorId = Successor#chord_node.id,
    % successor is my successor
    case isIdInRange(Id, CurrId, SuccessorId) of
        true -> #foundSuccessor{targetId = Id, successor = Successor};
        false when Id =:= SuccessorId -> #foundSuccessor{targetId = Id, successor = Successor};
        _ -> closestPrecedingFinger(State, Id)
    end.
