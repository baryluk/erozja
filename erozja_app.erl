-module(erozja_app).
-author('baryluk@smp.if.uj.edu.pl').

-behaviour(application).

-export([start/2, stop/1, config_change/3]).
-export([go/0]).

start(_Type, _Args) ->
	{ok, Pid} = erozja_sup:start_link(),
	{ok, Pid}.

stop(_State) ->
	ok.

config_change(_Changed, _New, _Removed) ->
	ok.


go() ->
	application:start(sasl),
	application:start(inets),
	application:start(wx),
	application:start(erozja),
	erozja_ompl:start_testrss(),
	erozja_gui:start().

