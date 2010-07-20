{application, resource_discovery,
 [{description, "A simple resource discovery system"},
  {vsn, "0.1.0"},
  {modules, [resource_discovery,
             rd_app,
             rd_sup,
	     rd_server]},
  {registered, [rd_sup]},
  {applications, [kernel, stdlib]},
  {mod, {rd_app, []}}
 ]}.
