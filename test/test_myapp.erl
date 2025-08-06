-module(test_myapp).

-include_lib("eunit/include/eunit.hrl").

my_nif_test() ->
    ?assertEqual(7, my_nif:add(3, 4)).

my_port_test_() ->
    {setup,
     fun() -> my_port:start() end,
     fun(_) -> my_port:stop() end,
     [?_assertEqual([13], my_port:foo(12)),
      ?_assertEqual([24], my_port:bar(12))]}.