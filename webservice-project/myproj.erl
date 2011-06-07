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
    application:set_env({{projectid}}, port, 6969),
    application:set_env({{projectid}}, filereadbuffer, 64768),
    application:set_env({{projectid}}, numacceptors, 3),  % fewer acceptors as this is presumed an adhoc, interactive run
    application:start(cowboy),                            % and add any other application startups here
    {{projectid}}_cowboy:init().

start_link() ->
    {{projectid}}_cowboy:init().

