%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, json_parser,
 [{description, "JSON parser (using plain ports)"},
  {vsn, "0.1.0"},
  {modules, [json_parser]},
  {applications, [kernel, stdlib]}
 ]}.
