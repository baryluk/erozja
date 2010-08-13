-module(erozja_manager).

-behaviour(gen_server).

-export([start/0, start_link/0, stop/0, add_feed/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {feeds}).


-define(REG, {local, ?MODULE}).
-define(REGC, ?MODULE).

start() ->
	gen_server:start(?REG, ?MODULE, noargs, []).

start_link() ->
	gen_server:start_link(?REG, ?MODULE, noargs, []).


stop() ->
	gen_server:cast(?REGC, stop).

add_feed(URL) ->
	gen_server:call(?REGC, {add_feed, URL}).


init(noargs) ->
	{ok, #state{feeds=[]}}.

handle_call({add_feed, URL}, _From, State = #state{feeds=Feeds}) ->
	{ok, Pid} = erozja_queues_sup:start_child(URL),
	NewFeed = {URL, Pid},
	NewState = State#state{feeds = [NewFeed | Feeds]},
	Result = ok,
	{reply, Result, NewState};

handle_call(Unknown, From, State) ->
	io:format("Unknown message ~p from ~p~n", [Unknown, From]),
	{noreply, State}.

handle_cast(stop, State) ->
	{stop, stop, State};
handle_cast(Unknown, State) ->
	io:format("Unknown message ~p~n", [Unknown]),
	{noreply, State}.

handle_info(Msg, State) ->
	io:format("Unknown message ~p~n", [Msg]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

