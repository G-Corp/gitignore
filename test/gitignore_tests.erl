-module(gitignore_tests).

-include_lib("eunit/include/eunit.hrl").

gitignore_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_default())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_default() ->
  ?assert(true).


