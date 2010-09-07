-module(erozja_manager).
-author('baryluk@smp.if.uj.edu.pl').


-behaviour(gen_server).

-export([start/0, start_link/0, stop/0, add_feed/1, list_feeds/0, force_update_all/0]).

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

add_feed(URL) when is_list(URL) ->
	gen_server:call(?REGC, {add_feed, URL}).

list_feeds() ->
	gen_server:call(?REGC, list_feeds).

force_update_all() ->
	gen_server:cast(?REGC, force_update_all).


init(noargs) ->
	{ok, #state{feeds=[]}}.


handle_call({add_feed, URL}, _From, State = #state{feeds=Feeds}) when is_list(URL) ->
	{ok, QueuePid} = erozja_queues_sup:start_child(URL),
	NewFeed = {URL, QueuePid},
	NewState = State#state{feeds = [NewFeed | Feeds]},
	Result = {ok, QueuePid},
	{reply, Result, NewState};

handle_call(list_feeds, _From, State = #state{feeds=Feeds}) ->
	Result = Feeds,
	{reply, Result, State};

handle_call(Unknown, From, State) ->
	io:format("~p: Unknown message ~p from ~p~n", [?MODULE, Unknown, From]),
	{noreply, State}.


handle_cast(stop, State) ->
	{stop, stop, State};

handle_cast(force_update_all, State = #state{feeds=Feeds}) ->
	lists:foreach(fun({_, QueuePid}) ->
		erozja_queue:force_update(QueuePid)
	end, Feeds),
	{noreply, State};

handle_cast(Unknown, State) ->
	io:format("~p: Unknown message ~p~n", [?MODULE, Unknown]),
	{noreply, State}.


handle_info(Msg, State) ->
	io:format("~p: Unknown message ~p~n", [?MODULE, Msg]),
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

