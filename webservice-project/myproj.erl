-module({{projectid}}).

%% Public Module -- Main  public API should go here
-export([
	 start/0
	]).

%% ===================================================================
%% API Functions
%% ===================================================================

start() ->
    {{projectid}}_sup:start_link().
