%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, theshark,
 [{description, "the shark"},
  {vsn, "0.1.0"},
  {modules, [shark_sup, shark_app, theshark]},
  {registered, [theshark]},
  {applications, [kernel, stdlib]},
  {mod, {shark_app, [inets, crypto]}}
 ]}.
