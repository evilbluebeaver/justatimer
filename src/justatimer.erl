-module(justatimer).

-export([create/2,
         start/1,
         restart/1,
         restart/2,
         restart/3,
         stop/1]).

-record(justatimer, {ref :: reference() | undefined,
                     timeout :: non_neg_integer(),
                     msg :: term()}).

-opaque justatimer() :: #justatimer{}.
%% @type justatimer(). structure for presenting a timer

-export_type([justatimer/0]).

%% @doc
%% Creates a timer structure. Message `Msg' will be delivered to the calling process after `Timeout' ms after timer starts.
%% @end
-spec create(Msg :: term(), Timeout :: non_neg_integer()) -> justatimer().
create(Msg, Timeout) ->
    #justatimer{msg=Msg, timeout=Timeout}.

%% @doc
%% Starts the timer. If the timer has already been stared nothing happens.
%% @end
-spec start(Timer :: justatimer()) -> justatimer().
start(#justatimer{ref=undefined, timeout=Timeout, msg=Msg}=Timer) ->
    Ref = erlang:send_after(Timeout, self(), Msg),
    Timer#justatimer{ref=Ref};
start(Timer) ->
    Timer.

%% @doc
%% Restarts the timer with the new timeout. If the timer hasn't been started it will be started.
%% @end
-spec restart(Timeout :: non_neg_integer(), Timer :: justatimer()) -> justatimer().
restart(Timeout, Timer) ->
    restart(Timer#justatimer{timeout=Timeout}).

%% @doc
%% Restarts the timer with the new timeout and message. If the timer hasn't been started it will be started.
%% @end
-spec restart(Msg :: term(), Timeout :: non_neg_integer(), Timer :: justatimer()) -> justatimer().
restart(Msg, Timeout, Timer) ->
    restart(Timer#justatimer{msg=Msg, timeout=Timeout}).

%% @doc
%% Restarts the timer with the old timeout. If the timer hasn't been started it will be started.
%% @end
-spec restart(Timer :: justatimer()) -> justatimer().
restart(#justatimer{ref=undefined, timeout=Timeout, msg=Msg}=Timer) ->
    Ref = erlang:send_after(Timeout, self(), Msg),
    Timer#justatimer{ref=Ref};
restart(#justatimer{ref=Ref,timeout=Timeout, msg=Msg}=Timer) ->
    erlang:cancel_timer(Ref),
    Ref1 = erlang:send_after(Timeout, self(), Msg),
    Timer#justatimer{ref=Ref1}.

%% @doc
%% Stops the timer. If the timer hasn't beean stared nothing happens.
%% @end
-spec stop(Timer :: justatimer()) -> justatimer().
stop(#justatimer{ref=undefined}=Timer)->
    Timer;
stop(#justatimer{ref=Ref, msg=Msg}=Timer)->
    erlang:cancel_timer(Ref),
    receive Msg -> ok after 0 -> ok end,
    Timer#justatimer{ref=undefined}.
