-module(erozja_queue).
-author('baryluk@smp.if.uj.edu.pl').


-behaviour(gen_server).

-export([start/0, start_link/0, start/1, start_link/1, stop/1, add_item/2, get_items/1,
	stop_update/1, force_update/1, set_options/2, last_update/1, subscribe/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {url, items=[], title, favicon=undefined, count_total=0, count_unreaded=0, tref=undefined, update_interval=60, last_update=0, loader_pid, loader_monitor, type, subscribed_to=[], subscribed_by=[]}).


-define(deb_enable, 1).
-include("erozja.hrl").

start(URL) ->
	gen_server:start(?MODULE, {url, URL}, []).

start_link(URL) ->
	gen_server:start_link(?MODULE, {url, URL}, []).

start() ->
	gen_server:start(?MODULE, {}, []).

start_link() ->
	gen_server:start_link(?MODULE, {}, []).


stop(Pid) ->
	gen_server:cast(Pid, stop).


get_items(Pid) ->
	gen_server:call(Pid, get_items).

stop_update(Pid) ->
	gen_server:cast(Pid, stop_update).

force_update(Pid) ->
	gen_server:call(Pid, force_update).

set_options(Pid, Opts) ->
	gen_server:call(Pid, {set_options, Opts}).

last_update(Pid) ->
	gen_server:call(Pid, last_update).

% this is called by loader or subqueues
add_item(Pid, Item) ->
	gen_server:cast(Pid, {add_item, Item}).

subscribe(Pid, Data) ->
	Self = self(),
	gen_server:call(Pid, {subscribe, Self, Data}).


init({url, URL}) ->
	State0 = #state{url=URL, type=feed, title=URL},
	State1 = set_timer(State0),
	{ok, State1};
init({}) ->
	{ok, #state{type=agg}}.

set_timer(State = #state{tref=OldTRef, update_interval=Interval}) ->
	case OldTRef of
		undefined -> ok;
		_ ->
			{ok, cancel} = timer:cancel(OldTRef),
			ok
	end,
	{ok, TRef} = timer:send_after(Interval*1000, update_by_timer),
	State#state{tref=TRef}.

handle_call(get_items, _From, State) ->
	Items = get_items0(State),
	{reply, Items, State};

handle_call({subscribe, Client, Data}, _From, State = #state{subscribed_by = Clients}) when is_pid(Client) ->
	Reply = get_summary(State),
	_MonitorRef = erlang:monitor(process, Client),
	{reply, Reply, State#state{subscribed_by=[{Client, Data} | Clients]}};


handle_call(force_update, _From, State0 = #state{type=feed}) ->
	State1 = start_update(State0),
	State2 = set_timer(State1),
	{reply, started_update, State2};

handle_call(last_update, _From, State = #state{last_update = LastUpdate}) ->
	{reply, LastUpdate, State};

handle_call(Unknown, From, State) ->
	?deb("Unknown call ~p from ~p~n", [Unknown, From]),
	{noreply, State}.

% messages/calls to subscribed agents:
%   {add_item, Item}
%   new_favicon
%   new_title
%   removed
%   started_update
%   updated

handle_cast({add_item, Item}, State = #state{subscribed_by = Clients}) ->
	State1 = add(Item, State),
	notify({add_item, Item}, Clients),
	{noreply, State1};
handle_cast(stop, State) ->
	{stop, stop, State};
handle_cast(Unknown, State) ->
	?deb("Unknown cast ~p~n", [Unknown]),
	{noreply, State}.

start_update(State = #state{url=URL,loader_pid=undefined, subscribed_by = Clients}) ->
	{LoaderPid, LoaderMonitor} = spawn_monitor(erozja_loader, url, [URL, self()]),
	notify({update_started, LoaderPid}, Clients),
	State#state{loader_pid=LoaderPid, loader_monitor=LoaderMonitor};
start_update(State = #state{url=_URL,loader_pid=OldLoader}) when is_pid(OldLoader) ->
	State.


handle_info(update_by_timer, State0) ->
	State1 = start_update(State0),
	State2 = set_timer(State1),
	{noreply, State2};
handle_info({'DOWN', MonitorRef, process, LoaderPid, Reason}, State1 = #state{loader_pid = LoaderPid, loader_monitor=MonitorRef, url=URL, subscribed_by = Clients}) ->
	case Reason of
		normal ->
			notify({update_done, LoaderPid, ok}, Clients),
			ok;
		_ ->
			notify({update_done, LoaderPid, error}, Clients),
			io:format("~p:~p: Loader ~p for ~p down:~n Reason ~p~n", [?MODULE, self(), LoaderPid, URL, Reason]),
			ok
	end,
	State2 = set_timer(State1),
	State3 = State2#state{loader_pid=undefined, loader_monitor=undefined},
	{noreply, State3};
handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, State1 = #state{subscribed_by = Clients, url = URL}) ->
	io:format("~p:~p: Subscriber ~p for ~p down:~n Reason ~p~n", [?MODULE, self(), Pid, URL, Reason]),
	NewClients = proplists:delete(Pid, Clients),
	State2 = State1#state{subscribed_by = NewClients},
	{noreply, State2};
handle_info(Msg, State) ->
	?deb("Unknown msg ~p~n", [Msg]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


get_items0(_State = #state{items=Items}) ->
	Items.

add(Item, State = #state{items=Items}) ->
	State#state{items = [Item | Items]}.

get_summary(#state{title = Title, favicon = Favicon, count_total = CountTotal, count_unreaded = CountUnreaded, last_update = LastUpdate}) ->
	{Title, Favicon, CountTotal, CountUnreaded, LastUpdate}.

notify(Msg, Clients) ->
	lists:foreach(fun({Pid, Data}) ->
		Pid ! {erozja_queue, self(), Data, Msg}
	end, Clients).
