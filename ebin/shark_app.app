{application, shark_app,
 [{description, "the sharkest"},
  {vsn, "1"},
  {modules, [shark_app, shark_sup, shark_twitter_server]},
  {registered, [theshark]},
  {applications, [kernel, stdlib, sasl, crypto, inets, ssl]},
  {mod, {shark_app,[]}}
 ]}.
