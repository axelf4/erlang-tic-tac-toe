-module(tictactoe_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, { {one_for_one, 60, 3600},
		   [ { server
			 , {server_sup, start_link, []}
			 , permanent, 1000, supervisor, [server_sup]
			 }
		   , { matchmaker
			 , {matchmaker_serv, start_link, []}
			 , permanent, 1000, worker, [matchmaker_serv]
			 }] }}.
