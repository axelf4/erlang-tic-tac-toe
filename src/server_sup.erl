-module(server_sup).
-behavior(supervisor).

-export([start_link/0, start_server/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Port = 8001,
	% {ok, Port} = application:get_env(port),
	{ok, ListenSocket} = gen_tcp:listen(Port, [{active, false}]),

	% spawn_link is synchronous and would block, meaning supervisor
	% would be unable to reply to any new child processes. Postpone
	% creation by performing it in new process.
	spawn_link(fun start_servers/0),

	{ok, { {simple_one_for_one, 60, 3600},
		   [{ socket
			, {tictactoe_serv, start_link, [ListenSocket]}
			, temporary, 1000, worker, [tictactoe_serv]
			}] }}.

start_server() ->
	supervisor:start_child(?MODULE, []).

start_servers() ->
	lists:foreach(fun(_) -> start_server() end, lists:seq(1, 10)).
