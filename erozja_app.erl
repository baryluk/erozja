-module(erozja_app).
-behaviour(application).

-export([start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
	{ok, Pid} = erozja_sup:start_link(),
	{ok, Pid}.

stop(_State) ->
	ok.

config_change(_Changed, _New, _Removed) ->
	ok.
