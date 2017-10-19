{application, mcache, [
  {description, "Example mcache app."},
  {vsn, "0.1"},
  {modules, [mcache_app, mcache_sup, mcache_worker]},
  {registered, [mcache_worker]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {mcache_app, []}},
  {env, []}
]}.