-module(erozja_queues_sup).
-author('baryluk@smp.if.uj.edu.pl').

-behaviour(supervisor).

-export([start_link/0, init/1, start_child/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, noargs).

init(noargs)  ->
	{ok, {{simple_one_for_one, 0, 1},
			[{queue, {erozja_queue, start_link, []},
				temporary, 2000, worker, [erozja_queue]}]
	}}.

start_child(URL) ->
	supervisor:start_child(?MODULE, [URL]).
