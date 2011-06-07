-module({{projectid}}).

%% Public Module -- Main  public API should go here
-export([
	 start_link/0,
	 start/0
	]).

%% ===================================================================
%% API Functions
%% ===================================================================

start() ->
    {{projectid}}_cowboy:init().

start_link() ->
    {{projectid}}_cowboy:init().

