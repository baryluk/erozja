{application, erozja,
	[{description, "This is RSS client, agregator, and server to be used by other clients."},
	{vsn, "0.9.0.0"},
	{modules, [erozja_app,
		erozja_loader,
		erozja_parse,
		erozja_queue
	]},
	{registered, [erozja_sup, erozja_queues_sup, erozja_manger, erozja_queue]},
	{applications, [kernel, stdlib, sasl, inets]},
	{mod, {erozja_app, []}},
	{env, []}
	]
}.
