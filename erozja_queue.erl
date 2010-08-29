-module(erozja_queue).
-author('baryluk@smp.if.uj.edu.pl').


-behaviour(gen_server).

-export([start/0, start_link/0, start/1, start_link/1, stop/1, add_item/2, loader_done/2, get_items/1,
	stop_update/1, force_update/1, set_options/2, last_update/1, subscribe/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {url, items=[], title, favicon=undefined, count_total=0, count_unreaded=0, tref=undefined, update_interval=600, last_update=0, loader_pid, loader_monitor, loader_started, type, subscribed_to=[], subscribed_by=[], added=0}).


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

% todo: limit number of items (sort by date)
% todo: option to not send content (only important metadata)
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

loader_done(Pid, Status) ->
	gen_server:cast(Pid, {loader_done, self(), Status}).

subscribe(Pid, Data) ->
	Self = self(),
	gen_server:call(Pid, {subscribe, Self, Data}).

store(Pid, Item, Opts) ->
	gen_server:call(Pid, {store, Item, Opts}).

% opts:
%   flagged
%   {tag, Tg}
%   seen

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

handle_cast({add_item, Item}, State = #state{}) ->
	State1 = add(Item, State),
	{noreply, State1};
handle_cast({loader_done, LoaderPid, Status}, State = #state{subscribed_by = Clients, loader_pid=LoaderPid, loader_started=LoaderStarted}) ->
	Diff = timer:now_diff(now(), LoaderStarted) div 1000,
	notify({update_done, LoaderPid, Diff, Status}, Clients),
	{noreply, State};
handle_cast(stop, State) ->
	{stop, stop, State};
handle_cast(Unknown, State) ->
	?deb("Unknown cast ~p~n", [Unknown]),
	{noreply, State}.

start_update(State = #state{url=URL, loader_pid=undefined, subscribed_by = Clients}) ->
	{LoaderPid, LoaderMonitor} = spawn_monitor(erozja_loader, url, [URL, self()]),
	Now = now(),
	?deb("Update for ~p started.~n", [URL]),
	notify({update_started, LoaderPid}, Clients),
	State#state{loader_pid=LoaderPid, loader_monitor=LoaderMonitor, loader_started=Now};
start_update(State = #state{url=URL,loader_pid=OldLoader}) when is_pid(OldLoader) ->
	?deb("Update for ~p already in progress.~n", [URL]),
	State.


handle_info(update_by_timer, State0) ->
	State1 = start_update(State0),
	State2 = set_timer(State1),
	{noreply, State2};
handle_info({'DOWN', MonitorRef, process, LoaderPid, Reason}, State1 = #state{loader_pid = LoaderPid, loader_monitor=MonitorRef, loader_started=LoaderStarted, url=URL, subscribed_by = Clients, added=Added}) ->
	Now = now(),
	Diff = timer:now_diff(Now, LoaderStarted) div 1000,
	case Reason of
		normal ->
			% normal not nacassarly mean, that it is done without error
			NewItemsCount = Added,
			notify({update_done, LoaderPid, Diff, {ok, NewItemsCount}}, Clients),
			ok;
		_ ->
			notify({update_done, LoaderPid, Diff, Reason}, Clients),
			?deb("Loader ~p for ~p down:~n Reason ~p~n", [LoaderPid, URL, Reason]),
			ok
	end,
	State2 = set_timer(State1),
	State3 = State2#state{loader_pid=undefined, loader_monitor=undefined, added=0, last_update=Now},
	{noreply, State3};
handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, State1 = #state{subscribed_by = Clients, url = URL}) ->
	?deb("Subscriber ~p for ~p down:~n Reason ~p~n", [Pid, URL, Reason]),
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

add(Item, State = #state{items=Items, subscribed_by=Clients, added=Added}) ->
	case is_already(Item, Items) of
		false ->
			State1 = State#state{items = [Item | Items], added=Added+1},
			notify({new_item, Item}, Clients),
			State1;
		%{updated, Position} ->
		%	
		true ->
			State
	end.

% todo: updated entries, removed entries?
is_already(_Item1 = #rss_item{guid=GUID1, link=Link1, title=Title1, pubdate=_PubDate1}, Items) ->
	lists:any(fun(_Item2 = #rss_item{guid=GUID2, link=Link2, title=Title2, pubdate=_PubDate2}) ->
		case {GUID1, GUID2} of
			{X1, X2} when (X1 =/= undefined) and (X2 =/= undefined) ->
				X1 =:= X2;
			_ ->
				case {Link1, Link2} of
					{Y1, Y2} when (Y1 =/= undefined) and (Y2 =/= undefined) ->
						Y1 =:= Y2;
					_ ->
						case {Title1, Title2} of
							{Z1, Z2} when (Z1 =/= undefined) and (Z2 =/= undefined) ->
								Z1 =:= Z2;
							_ ->
								false
						end
				end
		end
	end, Items).

get_summary(#state{title = Title, favicon = Favicon, count_total = CountTotal, count_unreaded = CountUnreaded, last_update = LastUpdate}) ->
	{Title, Favicon, CountTotal, CountUnreaded, LastUpdate}.

notify(Msg, Clients) ->
	lists:foreach(fun({Pid, Data}) ->
		Pid ! {erozja_queue, self(), Data, Msg}
	end, Clients).
