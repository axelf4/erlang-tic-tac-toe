-module(matchmaker_serv).
-behavior(gen_server).

-export([start_link/0, add_server/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

init([]) ->
	register(matchmaker, self()),
	{ok, []}.

add_server(Server) ->
	ok = gen_server:call(matchmaker, {add, Server}).

handle_call({ add, Server }, _From, [X | Rest]) ->
	pair_servers(Server, X),
	{reply, ok, Rest};
handle_call({ add, Server }, _From, Queue) ->
	{reply, ok, [Server | Queue]}.

handle_cast(_Request, State) ->
	{noreply, State}.

pair_servers(A, B) ->
	gen_server:cast(A, {pair_with, B}),
	gen_server:cast(B, {pair_with, A}).
