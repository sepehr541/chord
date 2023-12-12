-module(chord_request_handlers).

-include("chord_types.hrl").
-import(chord_messaging, [sendResponse/3, sendRequest/3]).
-import(chord_fingertable, [getEntry/2]).
-import(chord_utils, [hash/1]).
-import(chord_node_methods, [findSuccessor/2]).
-import(chord_circular_interval, [isInInterval/2]).

-export([handleRequest/3]).

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