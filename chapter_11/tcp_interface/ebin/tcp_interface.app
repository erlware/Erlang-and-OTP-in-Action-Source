{application, tcp_interface,
 [{description, "A simple text-based TCP interface to simple_cache"},
  {vsn, "0.1.0"},
  {modules, [ti_app,
             ti_sup,
	     ti_server]},
  {registered, [ti_sup]},
  {applications, [kernel, stdlib]},
  {mod, {ti_app, []}}
 ]}.
