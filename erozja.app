{application, erozja,
	[{description, "This is Atom and RSS client, agregator, and server to be used by other clients."},
	{vsn, "0.9.0.1"},
	{modules, [erozja_app,
		erozja_sup,
		erozja_queues_sup,
		erozja_date,
		erozja_parse,
		erozja_loader,
		erozja_ompl,
		erozja_queue,
		erozja_manager,
		erozja_gui_wx
	]},
	{registered, [erozja_sup, erozja_queues_sup, erozja_manger, erozja_queue, erozja_gui_wx]},
	{applications, [kernel, stdlib, sasl, inets, wx]},
	{mod, {erozja_app, []}},
	{env, []}
	]
}.
