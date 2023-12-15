-module(chord_circular_interval).

-include("../chord_types.hrl").
-export([isInInterval/2]).

-spec isInInterval(Id, Interval) -> boolean() when
    Id :: id(),
    Interval :: interval().

-define(compare(Op1, Op2, Op3, Op4),
    ((Lower < Upper) andalso (Lower Op1 Id) andalso (Id Op2 Upper)) orelse
    ((Lower > Upper) andalso ((Id Op3 Upper) orelse (Id Op4 Lower)))
).



% [Lower, Upper)
isInInterval(Id, #interval_Closed_Open{left = Lower, right = Upper}) ->
    ?compare(=<, <, <, >=);

% (Lower, Upper)
isInInterval(Id, #interval_Open_Open{left = Lower, right = Upper}) ->
    ?compare(<, <, <, >);

% (Lower, Upper]
isInInterval(Id, #interval_Open_Closed{left = Lower, right = Upper}) ->
    ?compare(<, =<, =<, >);

% [Lower, Upper]
isInInterval(Id, #interval_Closed_Closed{left = Id, right = Id}) -> true;
isInInterval(Id, #interval_Closed_Closed{left = Lower, right = Upper}) ->
    ?compare(=<, =<, =<, >=);

% else
isInInterval(_, _) -> false.
