%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, json_parser,
 [{description, "JSON parser (using a port driver)"},
  {vsn, "0.1.0"},
  {modules, [jp_app,
             jp_sup,
             jp_server,
             json_parser]},
  {registered, [jp_sup]},
  {applications, [kernel, stdlib]},
  {mod, {jp_app, []}}
 ]}.
