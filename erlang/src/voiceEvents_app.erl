%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the voiceEvents application.

-module(voiceEvents_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for voiceEvents.
start(_Type, _StartArgs) ->
    voiceEvents_deps:ensure(),
    voiceEvents_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for voiceEvents.
stop(_State) ->
    ok.
