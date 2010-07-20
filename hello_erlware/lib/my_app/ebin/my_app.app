%% This is the application resource file (.app file) for the my_app,
%% application.
{application, my_app, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [my_app_app,
              my_app_sup,
	      ma_hello_server]},
   {registered,[my_app_sup]},
   {applications, [kernel, stdlib, sasl]},
   {mod, {my_app_app,[]}},
   {start_phases, []}]}.

