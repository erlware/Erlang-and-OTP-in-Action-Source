{application, http_interface,
 [{description, "A RESTful HTTP interface to simple_cache"},
  {vsn, "0.1.0"},
  {modules, [hi_app,
             hi_sup,
	     hi_server]},
  {registered, [hi_sup]},
  {applications, [kernel, stdlib]},
  {mod, {hi_app, []}}
 ]}.
