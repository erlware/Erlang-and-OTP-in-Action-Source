{application, gen_web_server,
 [{description, "A generic web server behaviour"},
  {vsn, "0.1.0"},
  {modules, [gen_web_server,
             gws_connection_sup,
	     gws_server]},
  {registered, []},
  {applications, [kernel, stdlib]}
 ]}.
