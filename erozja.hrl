% author 'baryluk@smp.if.uj.edu.pl'

-record(rss_item, {
	title,    % string().
	link,     % string().
	pubdate,  % integer(). % unix timestamp
	desc,     % string().
	guid,     % string().
	fetched,  % integer(). % unix timestamp
	author,   % string().
	source    % atom(). % name of process
}).

-ifdef(deb_enable).
-define(deb(Format, Fields),
   begin
      io:format("~p ~p.erl:~p~n " ++ Format, [self(), ?MODULE, ?LINE | Fields])
   end).
-else.
-define(deb(Format, Fields),
   ok).
-endif.
