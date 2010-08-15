-module(erozja_sup).
-author('baryluk@smp.if.uj.edu.pl').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link()  ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

init(noargs) ->
	{ok, {{one_for_one, 1, 60},
			[
			{manager, {erozja_manager, start_link, []},
					permanent, 60000, worker, [erozja_manager]},
			{queues, {erozja_queues_sup, start_link, []},
					permanent, infinity, supervisor, [erozja_queues_sup]}
			%,
			% {server, {erozja_webserver_sup, start_link, []},
			%		permanent, infinity, supervisor, [erozja_webserver_sup]}
			]
	}}.
