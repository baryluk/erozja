-module(erozja_loader).

-export([url/1, url/2]).

url(URL) ->
	Result = httpc:request(get, {URL, []}, [{timeout, 60000}, {connect_timeout, 30000}], [{body_format, binary}]),
	{ok, {{_, 200, _}, _Headers, Body}} = Result,
	erozja_parse:string(Body).

url(URL, Pid) ->
	{ok, Items} = url(URL),
	lists:foreach(fun(Item) ->
		erozja_queue:add_item(Pid, Item)
	end, Items),
	ok.
