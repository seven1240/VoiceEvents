%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the voiceEvents application.

-module(voiceEvents_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).


%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any -> Any end,   
    WebConfig = [
         {ip, Ip},
                 {port, 8000},
                 {docroot, voiceEvents_deps:local_path(["priv", "www"])}],
    Web = {voiceEvents_web,
           {voiceEvents_web, start, [WebConfig]},
           permanent, 5000, worker, dynamic},
	Endpoint = {endpoints, {endpoints, start_link, []}, permanent, 5000, worker, dynamic},
	FreeswitchConnector = {freeswitch_connector, {freeswitch_connector, start_link, []}, permanent, 5000, worker, dynamic},
	FreeswitchInterface = {freeswitch_event, {freeswitch_event, start_link, []}, permanent, 5000, worker, dynamic},
    Processes = [Web, FreeswitchInterface, FreeswitchConnector, Endpoint ],
    {ok, {{one_for_one, 10, 10}, Processes}}.
