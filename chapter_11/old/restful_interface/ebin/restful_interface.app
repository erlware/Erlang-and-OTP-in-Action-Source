%% This is the application resource file (.app file) for the application.
{application, restful_interface,
  [{description, "An interface mechanism using REST"},
   {vsn, "0.1.0"},
   {modules, [ri_app,
              ri_sup,
              ri_gws_impl
              ]},
   {registered,[pi_sup]},
   {applications, [kernel, stdlib]},
   {mod, {ri_app,[]}},
   {start_phases, []}]}.
