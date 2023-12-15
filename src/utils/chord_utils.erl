-module(chord_utils).

-include("../chord_types.hrl").

-export([hash/1, pow/2, mod/2, toInt/1, toBin/1, nameToNode/1, extract_state/1, distance/2]).
-export([printNode/1]).

-spec hash(Term) -> Digest when
    Term :: term(),
    Digest :: binary().

hash(Term) ->
    toBin(toInt(crypto:hash(sha, term_to_binary(Term))) rem pow(2, ?M)).


-spec pow(Base, Exp) -> Result when 
    Base::integer(),
    Exp::non_neg_integer(),
    Result::non_neg_integer().

pow(Base, Exp) ->
    pow(Base, Exp, 1).

pow(_, 0, Acc) -> Acc;
pow(Base, Exp, Acc) when Exp > 0 -> pow(Base, Exp - 1, Acc * Base).


%
% From Stackoverflow: https://stackoverflow.com/a/858649
%
mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.


%
% Helpers for binary <-> integer
%
-spec toInt(Bin) -> Int when
    Bin :: binary(),
    Int :: non_neg_integer().

toInt(Bin) ->
    binary:decode_unsigned(Bin).

-spec toBin(Int) -> Bin when
    Int :: non_neg_integer(),
    Bin :: binary().

toBin(Int) ->
    binary:encode_unsigned(Int).



%
% create chord_node from Name atom
%
-spec nameToNode(Name) -> Node when
    Name :: atom(),
    Node :: chord_node().

nameToNode(Name) ->
    #chord_node{id = hash(Name), ref = Name}.



%
% Get the state of the gen_server Node with given name
%
extract_state(NodeName) ->
    sys:get_state(whereis(NodeName)).


%
% Distance between IDs
%
-spec distance(A, B) -> Distance when
    A::id(),
    B::id(),
    Distance::non_neg_integer().


distance(A, B) ->
    RingSize =  pow(2, ?M),
    (toInt(B) - toInt(A) + RingSize) rem RingSize.


%
% print chord_node()
%

printNode(#chord_node{id = Id, ref = Ref}) ->
    "(" ++  atom_to_list(Ref) ++ ", " ++ integer_to_list(toInt(Id)) ++ ")".