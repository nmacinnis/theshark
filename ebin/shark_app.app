{application, shark_app,
 [{description, "the sharkest"},
  {vsn, "1"},
  {modules, [shark_app, shark_sup, theshark]},
  {registered, [theshark]},
  {applications, [kernel, stdlib, sasl, crypto, inets]},
  {mod, {shark_app,[]}}
 ]}.
