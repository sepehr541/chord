-module(chord_request_handlers).

-include("chord_types.hrl").
-import(chord_messaging, [sendResponse/3, sendRequest/3]).
-import(chord_fingertable, [getEntry/2]).
-import(chord_utils, [isIdInRange/3, hash/1]).

-export([handleRequest/3]).
-export([findSuccessor/2]).

-spec handleRequest(Node, Remote, Payload) -> ok when
    Node :: state(),
    Remote :: chord_node(),
    Payload :: request_payload().

handleRequest(Node, Remote, Payload) ->
    case Payload of
        #findSuccessor{targetId = Id} -> handleFindSuccessor(Node, Remote, Id);
        #notify{} -> handleNotify(Node, Remote);
        #getEntry{key = Key} -> handleGetKey(Node, Remote, Key)
    end,
    ok.

-spec handleFindSuccessor(Node, Remote, Id) -> any() when
    Node :: state(),
    Remote :: chord_node(),
    Id :: id().

handleFindSuccessor(Node, Remote, Id) ->
    case findSuccessor(Node, Id) of
        #foundSuccessor{targetId = Id} = Found ->
            sendResponse(Node, Remote, Found);
        #askNode{targetId = Id, nodeToAsk = Closest} ->
            sendRequest(Node, Closest, #findSuccessor{targetId = Id}),
            receive
                #response{remote = Closest, payload = #foundSuccessor{targetId = Id} = Found} ->
                    sendResponse(Node, Remote, Found)
            end
    end.

-spec findSuccessor(Node, Id) -> Result when
    Node :: state(),
    Id :: id(),
    Result :: foundSuccessor() | askNode().

findSuccessor(State, Id) ->
    Successor = array:get(0, State#state.ft),
    CurrId = State#state.this#chord_node.id,
    SuccessorId = Successor#chord_node.id,
    % successor is my successor
    case isIdInRange(Id, CurrId, SuccessorId) of
        true -> #foundSuccessor{targetId = Id, successor = Successor};
        false when Id =:= SuccessorId -> #foundSuccessor{targetId = Id, successor = Successor};
        _ -> closestPrecedingFinger(State, Id)
    end.

-spec closestPrecedingFinger(State, Id) -> foundSuccessor() | askNode() when
    State :: state(),
    Id :: id().

closestPrecedingFinger(#state{this = Node, ft = Ft}, Id) ->
    % ask node in FT
    Fun = fun(_, Entry, Acc) ->
        case Acc of
            undefined ->
                case isIdInRange(Entry#chord_node.id, Node#chord_node.id, Id) of
                    true -> #askNode{targetId = Id, nodeToAsk = Entry};
                    false -> Acc
                end;
            _ ->
                Acc
        end
    end,
    Result = array:sparse_foldr(Fun, undefined, Ft),
    case Result of
        % I am the successor
        undefined -> #foundSuccessor{targetId = Id, successor = Node};
        _ -> Result
    end.


-spec handleNotify(State, Remote) -> UpdatedState when
    State::state(),
    Remote::chord_node(),
    UpdatedState::state().

handleNotify(#state{pred = Pred} = State, Remote) when Pred =:= nil -> 
    State#state{pred = Remote};
handleNotify(#state{this = This, pred = Pred} = State, Remote) ->
    case isIdInRange(Remote#chord_node.id, Pred#chord_node.id, This#chord_node.id) of
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
    todo.