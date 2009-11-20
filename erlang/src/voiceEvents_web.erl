%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for voiceEvents.

-module(voiceEvents_web).
-author('Jonathan Palley <jpalley@idapted.com>').

-export([start/1, stop/0, rest_response/3, rest_response/1]).

%% External API

start(Options) ->
    {_DocRoot, Options1} = get_option(docroot, Options),
%    Loop = fun (Req) ->
%                   ?MODULE:loop(Req, DocRoot)
%           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, {?MODULE, rest_response}} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

rest_response(Req, 'GET', "/endpoint_services.json") ->
	All = endpoints:all(),
%    Response = crud:retrieve(Id),
     Req:respond({200, [], All });

% crud_response(Req, 'GET', "/person/" ++ IdString) ->
%     Id = list_to_integer(IdString),
%     Response = crud:retrieve(Id),
%     Req:ok({"text/plain", ttp(Response)});
% 
%
rest_response(Req, 'GET', "/endpoint_services/" ++ Postfix) ->
	"nosj." ++ Id = lists:reverse(Postfix),
	{ok, Endpoint} = endpoints:find(list_to_integer(lists:reverse(Id))),
 	Req:respond({200, [], Endpoint});
 
rest_response(Req, 'DELETE', "/endpoint_services/" ++ Postfix) ->
	"nosj." ++ Id = lists:reverse(Postfix),
	endpoints:delete(list_to_integer(lists:reverse(Id))),
 	Req:respond({200, [], []});
 
rest_response(Req, 'POST', "/endpoint_services.json") ->
	Body = Req:recv_body(),
    {struct, Params} = mochijson2:decode(Body),
    io:format("... Endpiont: ~p~n", [Params]),
	Endpoint = {0, parse_params(<<"event">>, Params), parse_params(<<"url">>, Params), parse_params(<<"app">>, Params), parse_params(<<"domain">>, Params)}, 
	
	{ok, NewEndpoint, Id} = endpoints:add(Endpoint),
   Req:respond({300,
                [{status, created},
                 {location, "/endpoint_services/" ++ integer_to_list(Id)}],
                NewEndpoint});

rest_response(Req, _Method, Path) ->
    Req:respond({501, [], Path}).

rest_response(Req) ->
    io:format("~p ~p~n", [Req:get(path), Req:get(method)]),
    rest_response(Req, Req:get(method), Req:get(path)).


%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

parse_params(Param, Params) ->
	case lists:keysearch(Param,1, Params) of
        false -> false;
		{value, {Param, {struct, Result}}} -> list_to_tuple(Result);
		{value, {Param, Result}} -> Result
	end.