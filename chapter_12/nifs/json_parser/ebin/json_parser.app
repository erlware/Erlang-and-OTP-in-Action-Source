%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, json_parser,
 [{description, "JSON parser (using NIFs)"},
  {vsn, "0.1.0"},
  {modules, [json_parser]},
  {applications, [kernel, stdlib]}
 ]}.
