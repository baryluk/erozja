-module(erozja_parse).

-export([file/1, string/1]).

-record(state, {list=[], last_characters=undefined, level=1, stack=[]}).

-include("erozja.hrl").

file(FileName) ->
	{ok, LastState, _} = xmerl_sax_parser:file(FileName,
			[{event_fun, fun level_event/3}, {event_state, #state{}}]),
	{ok, to_records(LastState#state.list)}.

string(Data) ->
	{ok, LastState, _} = xmerl_sax_parser:stream(Data,
			[{event_fun, fun level_event/3}, {event_state, #state{}}]),
	{ok, to_records(LastState#state.list)}.

to_records(List) ->
	Fetched = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
	Source = "digg",
	lists:foldl(fun(Item, Acc) ->
		Title = proplists:get_value(title, Item),
		Link = proplists:get_value(link, Item),
		PubDate = to_date(proplists:get_value(pubdate, Item)),
		Desc = proplists:get_value(description, Item),
		GUID = proplists:get_value(guid, Item),
		Record = #rss_item{title=Title, link=Link, pubdate=PubDate, desc=Desc, guid=GUID, fetched=Fetched, source=Source},
		[Record | Acc]
	end, [], List).

to_date(D=undefined) ->
	D;
to_date(DateString) ->
	% "Sun, 25 Jan 2009 19:20:02 +0000"
	DateTime = httpd_util:convert_request_date(DateString),
	Timestamp = calendar:datetime_to_gregorian_seconds(DateTime),
	Timestamp.


% helper functions to track stack

level_event(Event = {startElement, _, Name, _, _}, Location, State = #state{level=Level, stack=Stack}) ->
	NewState = event(Event, Location, State#state{level=Level+1, stack=[Name | Stack]}),
	NewState#state{level=Level+1, stack=[Name | Stack]};
level_event(Event = {endElement, _, Name, _}, Location, State = #state{level=Level, stack=[Name | Stack]}) ->
	NewState = event(Event, Location, State),
	NewState#state{level=Level-1, stack=Stack};
level_event(_Event = {endElement, _, Name, _}, _Location, _State) ->
	throw({somethingwrong, Name});
level_event(Event, Location, State = #state{level=Level, stack=Stack}) ->
	NewState = event(Event, Location, State),
	NewState#state{level=Level, stack=Stack}.


% 


-define(VALUE(ParentTitle, Title),
% For the start field, clean the characters buffer
event(_Event = {startElement, _, _, _, _}, _Location, State = #state{stack=[Title, ParentTitle | _]}) ->
	State#state{last_characters=undefined};
%% For the end field event, use the last set of characters 
%% encountered as the value for that field
event(_Event = {endElement, _, _, _}, _Location, State = #state{stack=[Title, ParentTitle | _], list=[Item | Rest], last_characters=Chars}) ->
	Updated = [{list_to_atom(string:to_lower(Title)), Chars} | Item],
	State#state{list=[Updated|Rest], last_characters=undefined}
).
% endof macro QUOTE_VALUE

%% Start "FutureQuote" creates a new, empty key-value list
%% for the quote
event(_Event = {startElement, _, _, _, _}, _Location, _State = #state{stack = ["item" | _], list=Items}) ->
	#state{list=[[]|Items], last_characters=undefined};

%% Characters are stores in the parser state
event(_Event = {characters, Chars}, _Location, State = #state{}) ->
	State#state{last_characters=Chars};

?VALUE("item", "title");
?VALUE("item", "link");
?VALUE("item", "description");
?VALUE("item", "pubDate");
?VALUE("item", "guid");

% not interesting tags
event(_Event = {endElement, _, _Other, _}, _Location, State = #state{}) ->
	State#state{last_characters=undefined};

%% Catch-all. Pass state on as-is
event(_Event, _Location, State) ->
	State.