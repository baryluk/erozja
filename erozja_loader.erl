-module(erozja_loader).
-author('baryluk@smp.if.uj.edu.pl').


-export([url/1, url/2]).

-include("erozja.hrl").

% TODO: check if we are not in offline mode

url(URL) ->
	?deb("starting request to~n", [URL]),
	% TODO: ask manager to if we can start (to limit number of concurrant connections)
	
	FetchResult = httpc:request(get, {URL, []}, [{timeout, 60000}, {connect_timeout, 30000}], [{body_format, binary}]),
	try FetchResult of
		{ok, {{_, 200, _}, _Headers, Body}} ->
			?deb("ended request to~n", [URL]),
			ParingResult = erozja_parse:string(Body),
			ParingResult;
		E = {error, timeout} ->
			E;
		E = {error, {connect_failed, nxdomain}} ->
			E
	catch
		error:E ->
			{error, E}
	end.

url(URL, ParentQueue) ->
	case url(URL) of
		{ok, Items} ->
			?deb("fetched ~p items~n", [length(Items)]),
			lists:foreach(fun(Item) ->
				erozja_queue:add_item(ParentQueue, Item)
			end, Items),
			%erozja_queue:loader_done(ParentQueue, ok),
			ok;
		Other ->
			%erozja_queue:loader_done(ParentQueue, Other)
			exit(Other) % this will be send using monitor nicely
	end,
	ok.
