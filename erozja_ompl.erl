-module(erozja_ompl).
-author('baryluk@smp.if.uj.edu.pl').

-export([start_testrss/0, merge/2]).

% this module reads ompl file and starts appropriate queues

testrss() ->
	[
	"http://rss.cnn.com/rss/cnn_topstories.rss",
	"http://newsrss.bbc.co.uk/rss/newsonline_world_edition/front_page/rss.xml",
	"http://rss.slashdot.org/Slashdot/slashdot",
	"http://feeds.digg.com/digg/popular.rss",
	"http://bash.org.pl/rss",
	"http://feed.torrentfreak.com/Torrentfreak",
	"http://lwn.net/headlines/rss",
	"http://www.osnews.com/files/recent.xml",
	"http://rss.7thguard.net/7thguard.xml",
	"http://feedproxy.google.com/distrowatch/BFai",
	"http://rss.gazeta.pl/pub/rss/gamecorner.xml",
	"http://feeds2.feedburner.com/nihilogic_games",
	"http://my.opera.com/chooseopera/xml/atom/blog/",
	"http://my.opera.com/unite/xml/atom/blog/",
	"http://my.opera.com/desktopteam/xml/atom/blog/",
	"http://git.kernel.org/?p=linux/kernel/git/torvalds/linux-2.6.git;a=atom",
	"http://www.debian.org/News/weekly/dwn.en.rdf",
	"http://news.debian.net/feed/",
	"http://planet.trapexit.org/rss20.xml",
	"http://forum.trapexit.org/rss.php?t=1&f=5",
	"http://forum.trapexit.org/rss.php?t=1&f=7",
	"http://www.protest-project.eu/events_rss.xml",
	"http://www.freebsd.org/news/rss.xml",
	"http://www.reddit.com/r/programming/new/.rss",
	"http://feeds.digg.com/digg/container/science/popular.rss",
	"http://xkcd.com/atom.xml",
	"http://www.phoronix.com/rss.php",
	"http://feeds.feedburner.com/ajaxian",
	"http://opium.org.pl/feed/",
	"http://feeds.feedburner.com/ILoveTypography",
	"http://wiki.xensource.com/xenwiki/RecentChanges?action=rss_rc&ddiffs=1&unique=1",
	"http://gdata.youtube.com/feeds/base/users/BlackMesaSourceMedia/uploads?alt=rss&v=2&orderby=published&client=ytapi-youtube-profile",
	"http://www.hlfallout.net/rss/forums/2-hlf-news/",
	"http://www.rp.pl/rss/9129.html",
	"http://przekroj.pl/cywilizacja_rss.html",
	"http://www.rp.pl/rss/337506.html",
	"http://feeds.nytimes.com/nyt/rss/Science",
	"http://www.acme.com/jef/apod/rss.xml",
	"http://tomsastroblog.com/?feed=atom",
	"http://arXiv.org/rss/cs.NA",
	"http://arxiv.org/rss/cs",
	"http://arXiv.org/rss/quant-ph",
	"http://www.money.pl/rss/main.xml",
	"file://./recent.xml"
	].
% <?xml version="1.0"?>
%<opml version="1.0">
%  <head>
%    <title>Liferea Feed List Export</title>
%  </head>
%  <body>
%    <outline title="&#x15A;mieszne" text="&#x15A;mieszne" description="&#x15A;mieszne" type="folder" id="pkwyvaq" sortColumn="time" viewMode="0" expanded="true">
%      <outline title="bash.org.pl" text="bash.org.pl" description="bash.org.pl" type="rss" id="gugvdye" sortColumn="time" viewMode="2" xmlUrl="http://bash.org.pl/rss" htmlUrl="http://bash.org.pl" updateInterval="-1" collapsed="true"/>
%
% outline can be nested to create hierarchy

% todo: unescape HTML entities like &amp;

% http://www.opml.org/spec
% http://www.opml.org/spec2

start_testrss() ->
	lists:foreach(fun(URL) ->
		erozja_manager:add_feed(URL)
	end, testrss()).

merge(List1, List2) ->
	ok.
