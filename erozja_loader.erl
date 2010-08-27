-module(erozja_loader).
-author('baryluk@smp.if.uj.edu.pl').


-export([url/1, url/2]).

-include("erozja.hrl").

% TODO: check if we are not in offline mode

url(URL) ->
	?deb("starting request to~n", [URL]),
	FetchResult = httpc:request(get, {URL, []}, [{timeout, 60000}, {connect_timeout, 30000}], [{body_format, binary}]),
	try FetchResult of
		{ok, {{_, 200, _}, _Headers, Body}} ->
			?deb("ended request to~n", [URL]),
			ParingResult = erozja_parse:string(Body),
			ParingResult;
		{error, timeout} ->
			{error, timeout}
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
			ok;
		Other ->
			Other
	end.
