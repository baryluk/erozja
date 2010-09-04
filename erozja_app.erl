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
	ok = application:start(sasl),
	ok = application:start(crypto),
	ok = application:start(public_key),
	ok = application:start(ssl),
	ok = application:start(inets),
	ok = application:start(wx),
	code:add_path("/sctank2/Projekty/Erlang/to_unicode"),
	ok = application:start(erozja),
	erozja_ompl:start_testrss(),
	erozja_gui_wx:start().

