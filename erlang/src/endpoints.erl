%%%-------------------------------------------------------------------
%%% File    : gen_server_template.full
%%% Author  : my name <yourname@localhost.localdomain>
%%% Description : 
%%%
%%% Created :  2 Mar 2007 by my name <yourname@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(endpoints).

-behaviour(gen_server).

%% API
-export([start_link/0, add/1, count/0, delete/1, all/0, delete_all/0, find_for_event/1, find/1, success/2, failure/1, unregister_event/1, check_heartbeat/0, heartbeat_success/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%-record(state, {}).

-define(SERVER, ?MODULE).
-define(TABLE, endpoints).
-define(HEARBEAT, 21000).

-include("config.hrl").



%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	io:format("Start Endpoint"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add({_Id, _Event, _URL, _App, _Domain} = Endpoint) -> gen_server:call(?SERVER, {add, Endpoint}). 

delete(Key) -> gen_server:cast(?SERVER, {delete, Key}).

delete_all() -> gen_server:cast(?SERVER, delete_all).


find_for_event(Event) -> gen_server:call(?SERVER, {find_for_event, Event}).

count() -> gen_server:call(?SERVER, count).

all() -> gen_server:call(?SERVER, all).

find(Id) -> gen_server:call(?SERVER, {find, Id}).

success(Id, Time) -> gen_server:cast(?SERVER, {success, Id, Time}).

failure(Id) -> gen_server:cast(?SERVER, {failure, Id}).

check_heartbeat() -> gen_server:cast(?MODULE, check_heartbeat).

heartbeat_success() -> gen_server:cast(?MODULE, heartbeat_success).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Node) ->
	io:format("======= Init Endpoint ============"),
	open("test.dets", ?TABLE),
	register_events(),
	timer:apply_after(?HEARBEAT, ?SERVER, check_heartbeat, []),
  
%	All = dets:traverse(?TABLE, fun(X) -> io:format("~p~n", [X]), continue end),
%	io:format("RESULT ~p", [All]),

   T = erlang:now(),
  {ok, {T, T}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(count, _From, State) ->
	{reply, {ok, dets:info(?TABLE)}, State};


handle_call(all, _From, State) ->
	All = dets:foldl(fun({_Id, _URL, _Event, _Params} = E, Acc) -> [{struct, tuples(E)} | Acc]; (_A, Acc) -> Acc end, [], ?TABLE),
	{reply,  mochijson2:encode(All), State};

handle_call({find, Id}, _From, State) ->
	[Endpoint | _H] = dets:lookup(?TABLE, Id),
	{reply, {ok,  mochijson2:encode({struct, tuples(Endpoint)})}, State};
	
handle_call({find_for_event, Event}, _From, {LastBeat, LastCheck}) when is_list(Event) ->
	if
		Event =:= "HEARTBEAT" -> NewState = {erlang:now(), LastCheck};
		true -> NewState = {LastBeat, LastCheck}
	end,
	Endpoints = match_for_event(Event),
	{reply, Endpoints, NewState };

handle_call({find_for_event, Event}, From, State) when is_binary(Event) -> handle_call({find_for_event, binary_to_list(Event)}, From, State);


handle_call({add, {_Id, Event, URL, App, Domain} = E}, _From, State) ->
%%	http:request("http://127.0.0.1:3000/event_handler"),
%	Pid = spawn_link(fun() -> handler_loop(URL, Event, Filter) end),
	MaxId = last_key(), 
	N = dets:match_delete(?TABLE, {'$1', binary_to_list(Event), binary_to_list(URL), '$2'}),
	
	
	% {id, event, url, {filter, sequential_failures, total_failures, slow_requests, total_requests}}
	Endpoint = {MaxId + 1, binary_to_list(Event), binary_to_list(URL), {binary_to_list(App), binary_to_list(Domain), 0, 0, 0, 0}},
	dets:insert(?TABLE, Endpoint),
	register_event(binary_to_list(Event)),
	{reply, {ok, mochijson2:encode({struct, tuples(Endpoint)}), MaxId}, State};
	
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({failure, Key}, State) ->
	[{Id, Event, URL, {App, Domain, SFailures, TFailures, Slow, TRequests}} | _H] = dets:lookup(?TABLE, Key),
	dets:delete(?TABLE, Key),
	dets:insert(?TABLE, {Id, Event, URL, {App, Domain, SFailures + 1, TFailures + 1, Slow, TRequests + 1}}),
	{noreply, State};
	
handle_cast({success, Key, Time}, State) ->
	[{Id, Event, URL, {App, Domain, _SFailures, TFailures, Slow, TRequests}} | _H] = dets:lookup(?TABLE, Key),
	if
		Time > 100000 -> NSlow = Slow +1;
		true -> NSlow = Slow
	end,
	dets:delete(?TABLE, Key),
	dets:insert(?TABLE, {Id, Event, URL, {App, Domain, 0, TFailures, NSlow, TRequests + 1}}),
	{noreply, State};
	


handle_cast({delete, Key}, State) ->
	[{Id, Event, URL, Filter} | _H] = dets:lookup(?TABLE, Key),
	dets:delete(?TABLE, Key),
	NumLeft = length(match_for_event(Event)),
	io:format("delete ~p ~n", [NumLeft]),
	if 
		NumLeft > 0 -> true;
		true -> spawn(?MODULE, unregister_event, Event)
	end,
	{noreply, State};

handle_cast(delete_all, State) ->
	dets:delete_all_objects(?TABLE),
	{noreply, State};

handle_cast(check_heartbeat, {LastBeat, LastCheck} = State) ->
	io:format("STATE: ~p ~n", [State]),
	if
		LastBeat > LastCheck -> ok;
		true -> 
			error_logger:error_msg("No heartbeat from FreeSWITCH!"),
		%	register_events()
		ok
	end,
    timer:apply_after(?HEARBEAT, ?SERVER, check_heartbeat, []),
	{noreply, {LastBeat, erlang:now()}};

handle_cast(heartbeat_success, {LastBeat, LastCheck} = State) ->
	io:format("heartbeat success! ~p ~n",[State] ),

	{noreply, {erlang:now(), LastCheck}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	close(?TABLE),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
open(File, Name) -> 
	io:format("dets opened:~p~n", [File]), 
	Bool = filelib:is_file(File), 
		case dets:open_file(Name, [{file, File}]) of 
			{ok, ?MODULE} -> 
				case Bool of 
					true -> void; 
					false -> ok = dets:insert(Name, {free,1}) 
				end, 
				true; 
			{error,_Reason} -> 
				io:format("cannot open dets table~n"), 
				exit(eDetsOpen) 
		end. 
close(Name) -> dets:close(Name).


% tuples(Endpoint) ->
%     [{event, Endpoint#endpoint.event}, {url, Endpoint#endpoint.url}, {filter, Endpoint#endpoint.filter}].

match_for_event(Event) ->
	dets:match(?TABLE, {'$1', Event, '$2', '$3'}).

register_events() ->
	All = dets:traverse(?TABLE, fun({_Id, Event, _URL, _Params}) -> register_event(Event), io:format("REGISTERING ~p", [Event]), continue end),
	register_event("heartbeat").

register_event(Event) ->
	io:format("register event ~p ~n",[list_to_atom(string:to_lower(Event))]),

	case string:to_lower(string:substr(Event, 1, 7)) of
		"custom_" ->  
			ActualEvent = string:to_lower(string:substr(Event, 8, string:len(Event))),
			freeswitch:event(?NODE, [custom, list_to_atom(ActualEvent)]);
		_ -> 
			freeswitch:event(?NODE, [list_to_atom(string:to_lower(Event))])
	end.
	

unregister_event(Event) ->
	io:format("nix event ~p ~n", [Event]),
	freeswitch:nixevent(?NODE, [list_to_atom(string:to_lower(Event))]).

tuples({Id, Event, Url, {App, Domain, SFailures, TFailures, Slow, TRequests}}) ->
	[{id, Id}, {event, list_to_binary(Event)}, {url, list_to_binary(Url)}, {app, list_to_binary(App)}, {domain, list_to_binary(Domain)}, {sequential_failures, SFailures}, {total_failures, TFailures}, {slow_requests, Slow}, {total_requests,  TRequests}].
	
last_key() -> last_key({0, dets:first(?TABLE)}).

last_key({N, '$end_of_table'}) -> N;
last_key({LastN, NewN}) when NewN < LastN -> last_key({LastN, dets:next(?TABLE, NewN)});
last_key({LastN, NewN}) when NewN > LastN -> last_key({NewN, dets:next(?TABLE, NewN)}).


