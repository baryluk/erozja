-module(erozja_loader).

-export([url/1, url/2]).

url(URL) ->
	io:format("~p: starting request to ~p~n", [self(), URL]),
	Result = httpc:request(get, {URL, []}, [{timeout, 60000}, {connect_timeout, 30000}], [{body_format, binary}]),
	{ok, {{_, 200, _}, _Headers, Body}} = Result,
	io:format("~p: ended request to ~p~n", [self(), URL]),
	Result = erozja_parse:string(Body),
	Result.

url(URL, ParentQueue) ->
	{ok, Items} = url(URL),
	io:format("~p: fetched ~p items~n", [self(), length(Items)]),
	lists:foreach(fun(Item) ->
		erozja_queue:add_item(ParentQueue, Item)
	end, Items),
	ok.
