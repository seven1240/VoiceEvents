-module(freeswitch_event). 
-behaviour(gen_server). 


-export([start_link/0, process/1, send_http_request/3]).

%% gen_event callbacks 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, bg_extract_commands/1]).

%% init(Args) must return {ok, State} 
-define(SERVER, ?MODULE).

-define(DFIELDS, ["system_domain", "variable_system_domain"]).
-define(AFIELDS, ["app_name", "variable_app_name"]).
-define(SFIELDS, ["profile_name", "variable_sofia_profile_name"]).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error}
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
	io:format("Start Freeswitch Event"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


process(Event) -> gen_server:cast(?SERVER, {event, Event}).

init(Node) -> 
	inets:start(),
	io:format("*** initialized freeswitch_event:~p~n",[Node]), 
%	freeswitch:event(?NODE, [background_job]).
	{ok, 0}. 
	
handle_cast({event, [undefined | RawParams]}, N) -> 
	handle_cast({event, RawParams}, N);
handle_cast({event, [UUID | RawParams]}, N) when is_list(UUID) ->
	handle_cast({event, lists:append([{"channel_uuid", UUID}], RawParams)}, N);
handle_cast({event, RawParams}, N) ->
	TempEventName = freeswitch:get_event_name(RawParams),
	
	case TempEventName of
		"CUSTOM" ->
			EventName = string:to_upper("custom_" ++ freeswitch:get_event_header(RawParams, "Event-Subclass"));
		_ -> EventName = TempEventName
	end,
	io:format("***EventName: ~p~n", [EventName]),
	
	%We need to do to different types of matches depending on the event name
					
	Endpoints =  endpoints:find_for_event(EventName),
	
	case EventName of
		"BACKGROUND_JOB" ->
			JobArgs = freeswitch:get_event_header(RawParams, "Job-Command-Arg"),
			Params = lists:append(RawParams, bg_extract_commands(JobArgs));
		"HEARTBEAT" ->
			io:format("HEARTBEAT"),
			endpoints:heartbeat_success(),
			Params = RawParams;
		_ ->
			Params = RawParams
	end,
	DomainValuesToFilter = lists:filter(fun({K,_V})->lists:member(K, ?DFIELDS) end, Params),
	lists:foreach(fun([Id, URL, {App, Domain, _S1, _S2, _S3, _S4}]) ->		
			case lists:any(fun({_K,V}) -> {match, Pos} = regexp:matches(V, Domain), Pos > 0 end, DomainValuesToFilter) of
				true ->	
						AppValuesToFilter = lists:filter(fun({K,_V})->lists:member(K, ?AFIELDS) end, Params),								
						% case lists:any(fun({_K,V}) -> {match, Pos} = regexp:matches(V, App), Pos > 0 end, AppValuesToFilter) of
						case lists:any(fun({_K,V}) -> V =:= App end, AppValuesToFilter) of
							true ->
									spawn(?MODULE, send_http_request, [Id, URL, Params]);
							false -> true
						end;
				false -> true
			end

		end, Endpoints),
	
	
	{noreply, N + 1}; 
handle_cast({event, Event}, N) -> 
	io:format("*** unmatched event:~p~n",[Event]), 
	{noreply, N}; 

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call(_Request, _From, N) -> Reply = N, {ok, N, N}. 
handle_info(_Info, N) -> {ok, N}. 

terminate(_Reason, _State) ->
    ok.
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


send_http_request(Id, URL, Params) ->
	io:format("Sending to ~p ~n", [URL]),
	Vars = 	join(lists:map(fun({K, V}) -> edoc_lib:escape_uri(K) ++ "=" ++ edoc_lib:escape_uri(V) end, Params), "&"),
	{Time, Response} = timer:tc(http, request, [post, {URL, [], "application/x-www-form-urlencoded", Vars},[{timeout, 5000}], []]),
	case Response of
		{ ok, {{_Http, 200, _Ok}, _Headers, _Body }} -> endpoints:success(Id, Time);
		_ -> endpoints:failure(Id)
	end.
	
bg_extract_commands(Str) ->
	case  regexp:matches(Str, "\{.*\}") of
		{match, [{Start, Length}]} ->
			ArgStr = string:substr(Str, Start + 1, Length - 2),
			ArgArr = string:tokens(ArgStr, ","),
			lists:map(fun(L) -> [K,V] = string:tokens(L, "="), {K, V} end, ArgArr);
		_ ->
			[]
	end.

join([H|T], Sep) ->
      H ++ lists:concat([Sep ++ X || X <- T]).
	
