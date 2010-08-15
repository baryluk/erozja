% author 'baryluk@smp.if.uj.edu.pl'

-record(rss_item, {
	title,    % string().
	link,     % string().
	pubdate,  % integer(). % unix timestamp
	desc,     % string().
	guid,     % string().
	fetched,  % integer(). % unix timestamp
	source    % atom(). % name of process
}).
