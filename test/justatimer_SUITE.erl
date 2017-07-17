-module(justatimer_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server callbacks
-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([test_create/1,
         test_start/1,
         test_restart/1,
         test_stop/1]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

groups() ->
    [].

all() ->
    [test_create,
     test_start,
     test_restart,
     test_stop].

suite() ->
    [{timetrap,{minutes,10}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test_create(_Config) ->
    {justatimer, undefined, 1000, msg} = justatimer:create(msg, 1000),
    ok.

test_start(_Config) ->
    Timer = justatimer:create(msg, 10),
    Timer1  = justatimer:start(Timer),
    false = (Timer == Timer1),
    Timer1 = justatimer:start(Timer1),
    receive
        msg ->
            ok
    after 100 ->
              ct:fail(timer_did_not_work)
    end.

test_restart(_Config) ->
    Timer = justatimer:create(msg, 10),
    Timer1  = justatimer:restart(Timer),
    false = (Timer == Timer1),
    Timer2 = justatimer:restart(Timer1),
    false = (Timer1 == Timer2),
    Timer3 = justatimer:restart(10, Timer2),
    false = (Timer2 == Timer3),
    Timer4 = justatimer:restart(msg, 10, Timer3),
    false = (Timer3 == Timer4),
    receive
        msg ->
            ok
    after 100 ->
              ct:fail(timer_did_not_work)
    end.

test_stop(_Config) ->
    Timer = justatimer:create(msg, 10),
    Timer = justatimer:stop(Timer),
    Timer1  = justatimer:start(Timer),
    false = (Timer == Timer1),
    Timer = justatimer:stop(Timer1),
    receive
        _ ->
            ct:fail(timer_did_not_stop)
    after 100 ->
          ok
    end.


