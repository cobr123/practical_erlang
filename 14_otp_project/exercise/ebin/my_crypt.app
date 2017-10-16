{application, my_crypt, [
  {description, "Example app."},
  {vsn, "0.1"},
  {modules, [my_crypt_app, my_crypt_sup, my_crypt]},
  {registered, [my_crypt]},
  {applications, [
    kernel,
    stdlib,
    crypto
  ]},
  {mod, {my_crypt_app, []}},
  {env, []}
]}.