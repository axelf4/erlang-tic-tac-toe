-module(tictactoe_serv).
-behavior(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, { socket
			   , other
			   , state
			   , board
			   }).

start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
	gen_server:cast(self(), accept),
	{ok, #state{socket=Socket, state=accepting, board=board:new()}}.

handle_call({new_board, NewBoard}, From = {Other, _Tag}, State = #state{socket=Socket, other=Other, state=awaiting_opponent_move}) ->
	ok = gen_tcp:send(Socket, ["Opponent made their move. New board:\n" | board:to_io_list(NewBoard)]),
	case board:winner(NewBoard) of
		no_winner ->
			ok = gen_tcp:send(Socket, "What move? "),
			gen_server:cast(self, do_move),
			{reply, ok, State#state{board=NewBoard, state=querying_move}};
		is_tie ->
			gen_server:reply(From, ok),
			ok = gen_tcp:send(Socket, "Tie!\n"),
			{stop, is_tie, State#state{state=is_tie, board=NewBoard}};
		_ ->
			gen_server:reply(From, ok),
			ok = gen_tcp:send(Socket, "You lost!\n"),
			{stop, have_lost, State#state{state=have_lost, board=NewBoard}}
	end;

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast({ pair_with, Other }, State = #state{socket=Socket, state=awaiting_opponent}) ->
	ok = gen_tcp:send(Socket, "Starting game with opponent!\n"),

	_MonitorRef = monitor(process, Other),

	if self() < Other ->
			gen_server:cast(self(), do_move),
			{noreply, State#state{other=Other, state=querying_move}};
	   true ->
			{noreply, State#state{other=Other, state=awaiting_opponent_move}}
	end;

handle_cast(do_move, State = #state{socket=Socket, state=querying_move}) ->
	ok = gen_tcp:send(Socket, "What move? "),
	{noreply, State};

handle_cast(accept, State = #state{socket=ListenSocket, state=accepting}) ->
	{ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	inet:setopts(AcceptSocket, [{active, once}]),
	server_sup:start_server(),
	ok = gen_tcp:send(AcceptSocket, "Waiting for opponent...\n"),
	matchmaker_serv:add_server(self()),
	{noreply, State#state{socket=AcceptSocket, state=awaiting_opponent}}.

my_sign(#state{other=Other}) ->
	if self() < Other ->
			x;
	   true ->
			o
	end.

handle_info({tcp, Socket, Msg}, State = #state{socket=Socket, other=Other, state=querying_move, board=Board}) ->
	inet:setopts(Socket, [{active, once}]),
	io:format("Got msg: ~s~n", [Msg]),
	case string:to_integer(Msg) of
		{error, Reason} -> ok = gen_tcp:send(Socket, io_lib:format("Bad move: ~s. What move? ", [Reason])),
						   {noreply, State};
		{N, _} -> case board:set(N, my_sign(State), Board) of
					  {ok, NewBoard} ->
						  ok = gen_tcp:send(Socket, ["New board:\n" | board:to_io_list(NewBoard)]),
						  gen_server:call(Other, {new_board, NewBoard}),
						  case board:winner(NewBoard) of
							  no_winner ->
								  {noreply, State#state{state=awaiting_opponent_move, board=NewBoard}};
							  is_tie ->
								  ok = gen_tcp:send(Socket, "Tie!\n"),
								  {stop, is_tie, State#state{state=is_tie, board=NewBoard}};
							  _ ->
								  ok = gen_tcp:send(Socket, "You won!\n"),
								  {stop, have_won, State#state{state=have_won, board=NewBoard}}
							  end;
					  {error, Error} ->
						  io:format("Bad set: ~s~n", [Error]),
						  gen_server:cast(self(), do_move),
						  {noreply, State}
				  end
	end;
handle_info({tcp_closed, Socket}, State = #state{socket=Socket}) ->
	{stop, normal, State};
handle_info({tcp_error, Socket, Reason}, State = #state{socket=Socket}) ->
	{stop, Reason, State};

handle_info({'DOWN', _Reference, process, _Pid, _Reason}, State = #state{socket=Socket}) ->
	ok = gen_tcp:send(Socket, "Opponent process died\n"),
	{stop, opponent_died, State};
handle_info(_Request, State) ->
	{noreply, State}.
