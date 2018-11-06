initial_board(Board) :- 
	Board = [
		[green, green, green, green, green, green, green],
		[green, blank, blank, blank, blank, blank, green],
		[green, blank, blank, blank, blank, blank, green],
		[green, blank, blank, blank, blank, blank, green],
		[green, blank, blank, blank, blank, blank, green],
		[green, blank, blank, blank, blank, blank, green],
		[green, green, green, green, green, green, green]].

example_board1(Board) :- 
	Board = [
		[green, green, green, green, green, green, green],
		[green, blank, blank, blank, blank, blank, green],
		[green, blank, blank, blank, blank, blank, green],
		[green, blank, blank, blank, blue, red, green],
		[green, blank, blank, red, blank, blank, green],
		[green, blank, blank, blue, blue, red, green],
		[green, green, green, green, green, green, green]].

example_board2(Board) :-
	Board = [
		[green, green, green, green, green, green, green],
		[green, blank, blank, blank, red, blank, green],
		[green, blank, red, blank, blue, red, green],
		[green, blank, red, blank, blue, red, green],
		[green, blank, blank, blank, blue, red, green],
		[green, red, blue, red, red, blue, green],
		[green, green, green, green, green, green, green]].			  
				  
				  
piece_text(green, 'G').
piece_text(red, 'R').
piece_text(blue, 'B').
piece_text(blank, ' ').


print_board([]) :-
	print_separator.
print_board(Board) :-
	Board = [H|T],
	print_separator,
	print_line(H),
	print_board(T).

print_line([]) :-
	print('|\n').
	
print_line(Line) :- 	
	Line = [H|T],
	print('|'),
	print_piece(H),
	print_line(T).

print_piece(Piece) :- 
	piece_text(Piece, Text),
	print(Text).

print_separator :- 
		print('*-*-*-*-*-*-*-*\n').
					
%GameState = [Board, Player]
gamestate(Board, Player, [Board, Player]).
get_board([Board,Player], Board).
get_player([Board, Player], Player).

set_board([Board, Player], NewBoard, [NewBoard, Player]).
set_player([Board, Player], NewPlayer, [Board, NewPlayer]).

start :- 
	initial_game_state(GameState),
	play(GameState).

initial_game_state(GameState):-
	initial_board(Board),
	gamestate(Board, red, GameState).
	
play(GameState, GameStateEnd):-
	get_move(Pos),
	valid_move(GameState, Pos), 
	make_move(GameState, Pos, GameStateEnd).


valid_move(GameState, Pos):-
	get_board(GameState, Board),
	get_piece(Board, Pos, blank).


get_piece([Row|_], [0, Column], Piece) :-
	get_piece_in_row(Row, 
	
	
get_piece(Board, [Row, Column], Piece):-
	
	
	