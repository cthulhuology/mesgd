{application, mesgd, [
	{description, "mesgd"},
	{vsn, "1"},
	{registered, []},
	{applications, [
		kernel,
		stdlib,
		crypto,
		asn1,
		public_key,
		ssl
	]},
	{ modules, [ 
		mesgd,
		mesgd_app,
		mesgd_sup,
		mesgd_jwt,
		mesgd_auth,
		mesgd_router,
		mesgd_logger,
		mesgd_static,
		mesgd_dynamic,
		mesgd_server,
		ujson,
		jwt
	]},
	{mod, { mesgd_app, []}},
	{env, []}
]}.
