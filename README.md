rebar-project-template
======================

This is a set of rebar templates for distributed applications.  They build on RJ's erlang_rebar_example_project (https://github.com/RJ/erlang_rebar_example_project) with some tweaks.

The created project is meant to compile and work out-of-the-box.  I'd have it be an echo server or something but don't have time to spare right now for it.

Usage:

* Clone this repository into your .rebar/templates directory.  If it doesn't already exist, I think you can do this with the following command line:

``` shell
git clone https://github.com/robspassky/rebar-project-template.git ~/.rebar/templates
```

* Run it:

``` shell
rebar create template=project projectid=projectnamehere
```

* Sample session:

``` shell
robspassky@carmine:/tmp$ rebar create template=project projectid=projectnamehere
==> tmp (create)
Writing projectnamehere/Makefile
Writing projectnamehere/.gitignore
Writing projectnamehere/rebar.config
Writing projectnamehere/apps/projectnamehere/src/projectnamehere.app.src
Writing projectnamehere/apps/projectnamehere/src/projectnamehere.erl
Writing projectnamehere/apps/projectnamehere/src/projectnamehere_app.erl
Writing projectnamehere/apps/projectnamehere/src/projectnamehere_sup.erl
Writing projectnamehere/apps/projectnamehere/src/projectnamehere_server.erl
Writing projectnamehere/rel/files/vm.args
Writing projectnamehere/rel/reltool.config
Writing projectnamehere/rel/files/app.config
Writing projectnamehere/rel/files/erl
Writing projectnamehere/rel/files/nodetool
Writing projectnamehere/rel/files/projectnamehere
robspassky@carmine:/tmp$ cd projectnamehere/
robspassky@carmine:/tmp/projectnamehere$ make && make console
rebar clean
==> projectnamehere (clean)
==> rel (clean)
==> projectnamehere (clean)
rebar get-deps
==> projectnamehere (get-deps)
==> rel (get-deps)
==> projectnamehere (get-deps)
rebar compile
==> projectnamehere (compile)
Compiled src/projectnamehere.erl
Compiled src/projectnamehere_app.erl
Compiled src/projectnamehere_sup.erl
Compiled src/projectnamehere_server.erl
==> rel (compile)
==> projectnamehere (compile)
rebar generate -f
==> rel (generate)
rebar get-deps
==> projectnamehere (get-deps)
==> rel (get-deps)
==> projectnamehere (get-deps)
rebar compile
==> projectnamehere (compile)
==> rel (compile)
==> projectnamehere (compile)
rebar generate -f
==> rel (generate)
rel/projectnamehere/bin/projectnamehere console
Exec: /tmp/projectnamehere/rel/projectnamehere/erts-5.8.4/bin/erlexec -boot /tmp/projectnamehere/rel/projectnamehere/releases/0.1.0/projectnamehere -embedded -config /tmp/projectnamehere/rel/projectnamehere/etc/app.config -args_file /tmp/projectnamehere/rel/projectnamehere/etc/vm.args -- console
Root: /tmp/projectnamehere/rel/projectnamehere
Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:12:12] [rq:12] [async-threads:5] [hipe] [kernel-poll:true]

Eshell V5.8.4  (abort with ^G)
(projectnamehere@127.0.0.1)1> 
```

* Added "webservice-project " to project template.

``` shell
robspassky@carmine:/tmp/foo$ rebar create template=webservice-project projectid=projectnamehere
==> foo (create)
Writing projectnamehere/Makefile
Writing projectnamehere/.gitignore
Writing projectnamehere/rebar.config
Writing projectnamehere/apps/projectnamehere/src/projectnamehere.app.src
Writing projectnamehere/apps/projectnamehere/src/projectnamehere.erl
Writing projectnamehere/apps/projectnamehere/src/projectnamehere_app.erl
Writing projectnamehere/apps/projectnamehere/src/projectnamehere_sup.erl
Writing projectnamehere/apps/projectnamehere/src/projectnamehere_cowboy.erl
Writing projectnamehere/apps/projectnamehere/src/projectnamehere_cowboy_default.erl
Writing projectnamehere/apps/projectnamehere/priv/htdocs/my-first-page.html
Writing projectnamehere/rel/files/vm.args
Writing projectnamehere/rel/reltool.config
Writing projectnamehere/rel/files/app.config
Writing projectnamehere/rel/files/erl
Writing projectnamehere/rel/files/nodetool
Writing projectnamehere/rel/files/projectnamehere
robspassky@carmine:/tmp/foo$ pushd projectnamehere/
/tmp/foo/projectnamehere /tmp/foo /tmp/foo /tmp/foo /tmp/foo
robspassky@carmine:/tmp/foo/projectnamehere$ make console
rebar clean
==> projectnamehere (clean)
==> rel (clean)
==> projectnamehere (clean)
rebar get-deps
==> projectnamehere (get-deps)
==> rel (get-deps)
==> projectnamehere (get-deps)
Pulling cowboy from {git,"http://github.com/extend/cowboy.git",[]}
Cloning into cowboy...
==> cowboy (get-deps)
rebar compile
==> cowboy (compile)
Compiled src/cowboy_http_handler.erl
Compiled src/cowboy_http_websocket_handler.erl
Compiled src/cowboy.erl
Compiled src/cowboy_acceptor.erl
Compiled src/cowboy_sup.erl
Compiled src/cowboy_ssl_transport.erl
Compiled src/cowboy_tcp_transport.erl
Compiled src/cowboy_clock.erl
Compiled src/cowboy_dispatcher.erl
Compiled src/cowboy_http_protocol.erl
Compiled src/cowboy_acceptors_sup.erl
Compiled src/cowboy_requests_sup.erl
Compiled src/cowboy_app.erl
Compiled src/cowboy_http_websocket.erl
Compiled src/cowboy_listener_sup.erl
Compiled src/cowboy_http_req.erl
==> projectnamehere (compile)
Compiled src/projectnamehere_app.erl
Compiled src/projectnamehere_sup.erl
Compiled src/projectnamehere.erl
Compiled src/projectnamehere_cowboy.erl
Compiled src/projectnamehere_cowboy_default.erl
==> deps (compile)
==> rel (compile)
==> projectnamehere (compile)
rebar generate -f
==> rel (generate)
rel/projectnamehere/bin/projectnamehere console
Exec: /tmp/foo/projectnamehere/rel/projectnamehere/erts-5.8.4/bin/erlexec -boot /tmp/foo/projectnamehere/rel/projectnamehere/releases/0.1.0/projectnamehere -embedded -config /tmp/foo/projectnamehere/rel/projectnamehere/etc/app.config -args_file /tmp/foo/projectnamehere/rel/projectnamehere/etc/vm.args -- console
Root: /tmp/foo/projectnamehere/rel/projectnamehere
Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:12:12] [rq:12] [async-threads:5] [hipe] [kernel-poll:true]

Cowboy (<0.46.0>) is listening on port 6969 with 100 acceptors, a document root of /tmp/foo/projectnamehere/rel/projectnamehere/lib/projectnamehere-0.1.0/priv/htdocs and a file read buffer of 64768 bytes.
Eshell V5.8.4  (abort with ^G)
(projectnamehere@carmine)1>
```

Then, in another terminal...

``` shell
robspassky@carmine:~/Work/erlang$ curl http://carmine:6969/my-first-page.html
<html>
  <head>
    <title>projectnamehere webservice-project</title>
  </head>
  <body>
    <h1>Hi, Mom.</h1>
    <h2>This page is located in code:priv_dir(projectnamehere)/index.html unless the default docroot was overriden in app.config.</h2>
  </body>
</html>
```
