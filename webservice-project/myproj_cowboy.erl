-module({{projectid}}_cowboy).

-export([
	 start_link/0
	]).

start_link() ->
    DocRoot        = my_get_env(docroot),
    FileReadBuffer = my_get_env(filereadbuffer),
    Port           = my_get_env(port),
    NumAcceptors   = my_get_env(numacceptors),
    Dispatch = 
	[ 
	  {'_',    % Any virtual host
	   [
	    {'_', {{projectid}}_cowboy_default, [DocRoot, FileReadBuffer]} % default handler
	   ]
	  }
	],
    {ok, Pid} = cowboy:start_listener(http, 
				      NumAcceptors, 
				      cowboy_tcp_transport, [{port, Port}], 
				      cowboy_http_protocol, [{dispatch, Dispatch}]
				     ),
    io:format("Web Server (cowboy) (~p) is listening on port ~p with ~p acceptors, a document root of ~s and a file read buffer of ~p bytes.~n", [Pid, Port, NumAcceptors, DocRoot, FileReadBuffer]),
    {ok, Pid}.

%%
%% Internal Functions
%%

my_get_env(docroot) ->
    case application:get_env({{projectid}}, docroot) of
	{ok, DocRootString} -> DocRootString;
	undefined -> list_to_binary(code:priv_dir({{projectid}}) ++ "/htdocs")
    end;

my_get_env(Name) ->
    {ok, Value} = application:get_env({{projectid}}, Name),
    Value.
