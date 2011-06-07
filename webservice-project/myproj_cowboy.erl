-module({{projectid}}_cowboy).

-export([
	 start_link/0
	]).

start_link() ->
    {ok, DocRoot} = application:get_env({{projectid}}, docroot),
    {ok, FileReadBuffer} = application:get_env({{projectid}}, filereadbuffer),
    {ok, Port} = application:get_env({{projectid}}, port),
    {ok, NumAcceptors} = application:get_env({{projectid}}, numacceptors),
    Dispatch = 
	[ 
	  {'_',    % Any virtual host
	   [
	    {'_', {{projectid}}_cowboy_default, [DocRoot, FileReadBuffer]} % default handler
	   ]
	  }
	],
    {ok, Pid} = cowboy:start_listener(http, NumAcceptors, 
				       cowboy_tcp_transport, [{port, Port}], 
				       cowboy_http_protocol, [{dispatch, Dispatch}]
				      ),
    io:format("Cowboy (~p) is listening on port ~p with ~p acceptors, a document root of ~s and a file read buffer of ~p bytes.~n", [Pid, Port, NumAcceptors, DocRoot, FileReadBuffer]),
    {ok, Pid}.
