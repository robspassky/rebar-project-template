-module({{projectid}}).

%% Public Module -- Main  public API should go here
-export([
	 start/0,
	 start_link/0
	]).

%% ===================================================================
%% API Functions
%% ===================================================================

start() ->
    {{projectid}}_server:init().

start_link() ->
    {{projectid}}_server:init().
