-module(chord_utils_tests).
-include_lib("eunit/include/eunit.hrl").
-import(chord_utils, [pow/2, mod/2]).


%% pow/3 tests
pow_test() ->
    [?_assertEqual(1, pow(2, 0)),
     ?_assertEqual(9, pow(3, 2)),
     ?_assertEqual(1024, pow(2, 10)),
     ?_assertEqual(0, pow(0, 3))].

%% mod/2 tests
mod_test() ->
    [?_assertEqual(1, mod(5, 2)),
     ?_assertEqual(2, mod(-3, 5)),
     ?_assertEqual(0, mod(0, 3)),
     ?_assertEqual(1, mod(1000001, 1000000))].

