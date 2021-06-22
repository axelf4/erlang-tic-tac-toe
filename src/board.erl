-module(board).

-export([new/0, occupied/2, set/3, winner/1, to_io_list/1]).

new() ->
	{board, [e || _ <- lists:seq(1, 9)]}.

%% Returns whether the given square is occupied.
occupied(N, {board, B}) when is_integer(N) and (1 =< N) and (N =< 9) ->
	lists:nth(N, B) /= e.

setnth(1, Value, [_ | Rest]) ->
	[Value | Rest];
setnth(N, Value, [Element | Rest]) -> [Element | setnth(N - 1, Value, Rest)].

set(_, V, _) when not ((V =:= x) or (V =:= o)) ->
	{error, bad_sign};
set(N, V, Board = {board, B}) when (V =:= x) or (V =:= o) ->
	case occupied(N, Board) of
		true -> {error, is_occupied};
		false -> {ok, {board, setnth(N, V, B)}}
	end.

winner({board, B}) ->
	Lines = [ [1, 2, 3], [4, 5, 6], [7, 8, 9] % Horizontals
			, [1, 4, 7], [2, 5, 8], [3, 6, 9] % Verticals
			, [1, 5, 9], [3, 5, 7] % Diagonals
			],
	IsTie = not lists:any(fun(Sign) -> Sign == e end, B),
	Result = lists:filtermap(fun(Line) ->
									 [First | Rest] = lists:map(fun(N) -> lists:nth(N, B) end, Line),
									 case lists:all(fun(Sign) -> Sign == First end, Rest) of
										 true when First /= e ->
											 {true, First};
										 _ ->
											 false
									 end
							 end, Lines),
	case Result of
		_ when IsTie ->
			is_tie;
		[Winner | _] ->
			Winner;
		_ ->
			no_winner
	end.

to_io_list({board, B}) ->
	Chars = lists:map(fun(Sign) ->
							  case Sign of
								  e -> $\ ;
								  x -> $x;
								  o -> $o
							  end
					  end, B),
	unicode:characters_to_binary(io_lib:format("┌─┬─┬─┐~n│~c│~c│~c│~n├─┼─┼─┤~n│~c│~c│~c│~n├─┼─┼─┤~n│~c│~c│~c│~n└─┴─┴─┘~n", Chars)).
