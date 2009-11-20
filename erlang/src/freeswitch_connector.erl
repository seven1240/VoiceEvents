-module(freeswitch_connector). 
-behaviour(gen_server). 

-include("config.hrl").

-define(TIMEOUT, 5000).

-export([start_link/0]).

%% gen_event callbacks 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% init(Args) must return {ok, State} 
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error}
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
	io:format("Start Freeswitch Connector"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



init(_Args) -> 
%	freeswitch:event(?NODE, [background_job]).
    io:format("INIT CONNECTOR"),
	start_handler(?NODE, register_event_handler),
	{ok, 0}. 
	
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call(_Request, _From, N) -> Reply = N, {ok, N, N}. 
handle_info(_Info, N) -> {ok, N}. 

terminate(Reason, _State) ->
	io:format("TERMINATE! ~p ~n", [Reason]),
    ok.
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


start_handler(Node, Type) ->
	io:format("STAR HANDLER"),
	Self = self(),
	spawn_link(fun() ->
		monitor_node(Node, true),
		{foo, Node} ! Type,
		receive
			ok ->
				Self ! {Type, {ok, self()}},
				handler_loop(Node);
			{error,Reason} ->
				Self ! {Type, {error, Reason}}
		after ?TIMEOUT ->
				Self ! {Type, timeout}
		end
		end),

	receive
		{Type, X} -> X
	end.

handler_loop(Node) ->
	receive
		{event, H} ->
			io:format("New Event: ~n ~p ~n",[H]),
			freeswitch_event:process(H),
			handler_loop(Node)
	end.

