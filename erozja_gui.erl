-module(erozja_gui).

-behaviour(gen_server).

-export([start/0, start_link/0, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-include_lib("wx/include/wx.hrl").

% we are using wxWidgets here
% for Erlang, beyond wx, there is also etk (Erlang Tk from Tcl/Tk), gs, x11, ex11.
%

-record(state, {win}).


start() ->
	gen_server:start(?MODULE, noargs, []).

start_link() ->
	gen_server:start_link(?MODULE, noargs, []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

init(noargs) ->
	MyWin = wxWindow:new(),
	wxWindow:centerOnParent(MyWin, [{dir,?wxVERTICAL}]),
	{ok, #state{win=MyWin}}.


handle_call(Unknown, From, State) ->
	io:format("~p: Unknown message ~p from ~p~n", [?MODULE, Unknown, From]),
	{noreply, State}.

handle_cast(stop, State) ->
	{stop, stop, State};
handle_cast(Unknown, State) ->
	io:format("~p: Unknown message ~p~n", [?MODULE, Unknown]),
	{noreply, State}.


handle_info(Msg, State) ->
	io:format("~p: Unknown message ~p~n", [?MODULE, Msg]),
	{noreply, State}.

terminate(_Reason, _State = #state{win=MyWin}) ->
	wxWindow:destroy(MyWin),
	void.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
