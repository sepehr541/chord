-module(chord_circular_interval_tests).
-include_lib("eunit/include/eunit.hrl").

-include("src/chord_types.hrl").

-import(chord_circular_interval, [isInInterval/2]).

% Test cases
isInInterval_test() ->
    % Open-Open Interval
    ?assert(isInInterval(<<5>>, #interval_Open_Open{left = <<3>>, right = <<7>>})),
    ?assertNot(isInInterval(<<3>>, #interval_Open_Open{left = <<3>>, right = <<7>>})),
    ?assertNot(isInInterval(<<7>>, #interval_Open_Open{left = <<3>>, right = <<7>>})),
    ?assert(isInInterval(<<12>>, #interval_Open_Open{left = <<10>>, right = <<5>>})),
    ?assertNot(isInInterval(<<8>>, #interval_Open_Open{left = <<10>>, right = <<5>>})),

    % Open-Closed Interval
    ?assert(isInInterval(<<5>>, #interval_Open_Closed{left = <<3>>, right = <<7>>})),
    ?assertNot(isInInterval(<<3>>, #interval_Open_Closed{left = <<3>>, right = <<7>>})),
    ?assert(isInInterval(<<7>>, #interval_Open_Closed{left = <<3>>, right = <<7>>})),
    ?assert(isInInterval(<<4>>, #interval_Open_Closed{left = <<10>>, right = <<5>>})),

    % Closed-Closed Interval
    ?assert(isInInterval(<<5>>, #interval_Closed_Closed{left = <<3>>, right = <<7>>})),
    ?assert(isInInterval(<<3>>, #interval_Closed_Closed{left = <<3>>, right = <<7>>})),
    ?assert(isInInterval(<<7>>, #interval_Closed_Closed{left = <<3>>, right = <<7>>})),
    ?assert(isInInterval(<<7>>, #interval_Closed_Closed{left = <<7>>, right = <<3>>})),
    ?assert(isInInterval(<<3>>, #interval_Closed_Closed{left = <<7>>, right = <<3>>})),
    ?assert(isInInterval(<<8>>, #interval_Closed_Closed{left = <<7>>, right = <<3>>})),
    ?assert(isInInterval(<<2>>, #interval_Closed_Closed{left = <<7>>, right = <<3>>})),
    ?assert(isInInterval(<<3>>, #interval_Closed_Closed{left = <<3>>, right = <<3>>})),

    % Closed-Open Interval
    ?assert(isInInterval(<<5>>, #interval_Closed_Open{left = <<3>>, right = <<7>>})),
    ?assert(isInInterval(<<3>>, #interval_Closed_Open{left = <<3>>, right = <<7>>})),
    ?assertNot(isInInterval(<<7>>, #interval_Closed_Open{left = <<3>>, right = <<7>>})),
    ?assert(isInInterval(<<10>>, #interval_Closed_Open{left = <<10>>, right = <<5>>})),
    ?assertNot(isInInterval(<<5>>, #interval_Closed_Open{left = <<10>>, right = <<5>>})),
    ?assert(isInInterval(<<11>>, #interval_Closed_Open{left = <<10>>, right = <<5>>})),
    ?assert(isInInterval(<<4>>, #interval_Closed_Open{left = <<10>>, right = <<5>>})).
