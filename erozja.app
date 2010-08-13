{application, erozja,
	[{description, "This is RSS client, agregator, and server to be used by other clients."},
	{vsn, "0.9.0.1"},
	{modules, [erozja_app,
		erozja_loader,
		erozja_parse,
		erozja_queue,
		erozja_ompl,
		erozja_gui
	]},
	{registered, [erozja_sup, erozja_queues_sup, erozja_manger, erozja_queue, erozja_gui]},
	{applications, [kernel, stdlib, sasl, inets, wx]},
	{mod, {erozja_app, []}},
	{env, []}
	]
}.
