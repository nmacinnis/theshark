%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, theshark,
 [{description, "the shark"},
  {vsn, "0.1.0"},
  {modules, [theshark]},
  {registered, [theshark]},
  {applications, [kernel, stdlib]},
  {mod, {theshark, []}}
 ]}.
