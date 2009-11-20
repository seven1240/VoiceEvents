%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(voiceEvents).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the voiceEvents server.
start() ->
    voiceEvents_deps:ensure(),
    ensure_started(crypto),
    application:start(voiceEvents).

%% @spec stop() -> ok
%% @doc Stop the voiceEvents server.
stop() ->
    Res = application:stop(voiceEvents),
    application:stop(crypto),
    Res.
