justatimer
=====

Just a wrap for erlang:send_after and erlang:cancel_timer

Build
-----

    $ rebar3 compile

Documentation
-----

    $ rebar3 edoc

Usage
-----
    ...
    Timer = justatimer:create(timer_msg, 1000),
    StartedTimer = justatimer:start(Timer)
    ...
    handle_info(timer_msg, State) ->
    ...
    StoppedTimer = justatimer:stop(StartedTimer)
    ...
    RestartedTimer1 = justatimer:restart(Timer)
    ...
    RestartedTimer2 = justatimer:restart(500, Timer)
    ...


