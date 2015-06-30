-include_lib("eunit/include/eunit.hrl").
-include_lib("swirl/include/swirl.hrl").

-define(N, 10000).
-define(T, fun (Test) -> test(Test) end).
