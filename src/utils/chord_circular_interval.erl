-module(chord_circular_interval).

-include("../chord_types.hrl").
-export([isInInterval/2]).

-spec isInInterval(Id, Interval) -> boolean() when
    Id :: id(),
    Interval :: interval().

% (Lower, Upper)
isInInterval(Id, #interval_Open_Open{left = Lower, right = Upper}) when (Lower < Upper) andalso (Lower < Id) andalso (Id < Upper) -> true;
isInInterval(Id, #interval_Open_Open{left = Lower, right = Upper}) when (Lower > Upper) andalso ((Id < Upper) orelse (Id > Lower)) -> true;

% (Lower, Upper]
isInInterval(Id, #interval_Open_Closed{left = Lower, right = Upper}) when (Lower < Upper) andalso (Lower < Id) andalso (Id =< Upper) -> true;
isInInterval(Id, #interval_Open_Closed{left = Lower, right = Upper}) when (Lower > Upper) andalso ((Id =< Upper) orelse (Id > Lower)) -> true;


% [Lower, Upper]
isInInterval(Id, #interval_Closed_Closed{left = Lower, right = Upper}) when (Lower < Upper) andalso (Lower =< Id) andalso (Id =< Upper) -> true;
isInInterval(Id, #interval_Closed_Closed{left = Lower, right = Upper}) when (Lower > Upper) andalso ((Id =< Upper) orelse (Id >= Lower)) -> true;
isInInterval(Id, #interval_Closed_Closed{left = Id, right = Id}) -> true;

% [Lower, Upper)
isInInterval(Id, #interval_Closed_Open{left = Lower, right = Upper}) when (Lower < Upper) andalso (Lower =< Id) andalso (Id < Upper) -> true;
isInInterval(Id, #interval_Closed_Open{left = Lower, right = Upper}) when (Lower > Upper) andalso ((Id < Upper) orelse (Id >= Lower)) -> true;


% else
isInInterval(_, _) -> false.
