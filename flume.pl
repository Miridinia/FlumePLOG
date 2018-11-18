:- use_module(library(lists)).

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

example_board3(Board) :-
	Board = [
		[green, green, green, green, green, green, green],
		[green, red, red, red, red, red, green],
		[green, red, red, red, blue, red, green],
		[green, red, red, red, blue, red, green],
		[green, red, red, red, blue, red, green],
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
					
%GameState = [Board, Player, Red_Num, Blue_num]
gamestate(Board, Player, Red_Num, Blue_Num, [Board, Player, Red_Num, Blue_Num]).
get_board(GameState, Board):-
	nth1(1, GameState, Board).
get_player(GameState, Player):-
	nth1(2, GameState, Player).
get_red_num(GameState, Red_Num):-
	nth1(3, GameState, Red_Num).
get_blue_num(GameState, Blue_Num):-
	nth1(4, GameState, Blue_Num).


set_gamestate_elem(1, Elem, [_|GameState], [Elem|GameState]).
set_gamestate_elem(Index, Elem, [H|GameState], [H|NewGameState]):-
	Index > 1,
	Index_1 is Index-1,
	set_gamestate_elem(Index_1, Elem, GameState, NewGameState).

set_board(GameState, NewBoard, NewGameState):-
	set_gamestate_elem(1, NewBoard, GameState, NewGameState).
set_player(GameState, NewPlayer, NewGameState):-
	set_gamestate_elem(2, NewPlayer, GameState, NewGameState).
set_red_num(GameState, NewRedNum, NewGameState):-
	set_gamestate_elem(3, NewRedNum, GameState, NewGameState).
set_blue_num(GameState, NewBlueNum, NewGameState):-
	set_gamestate_elem(4, NewBlueNum, GameState, NewGameState).

increment_Red(GameState, NewGameState):-
	get_red_num(GameState, RedNum),
	NewRedNum is RedNum + 1,
	set_red_num(GameState, NewRedNum, NewGameState).
increment_Blue(GameState, NewGameState):-
	get_blue_num(GameState, BlueNum),
	NewBlueNum is BlueNum + 1,
	set_blue_num(GameState, NewBlueNum, NewGameState).

increment(GameState, red, NewGameState):-
	increment_Red(GameState, NewGameState).
increment(GameState, blue, NewGameState):-
	increment_Blue(GameState, NewGameState).


start :- 
	initial_game_state(GameState),
	game_loop(GameState).

game_loop(GameState):-
	get_board(GameState, Board),
	print_board(Board),
	\+ game_over(GameState, Winner),
	play(GameState, NewGameState),
	game_loop(NewGameState).

initial_game_state(GameState):-
	initial_board(Board),
	gamestate(Board, red, 0, 0, GameState).
		
play(GameState, NextGameState):-
	get_move(Pos),
	valid_move(GameState, Pos), 
	make_move(GameState, Pos, NextGameState).


game_over([_Board, _Player, Red, Blue], Winner) :-
	Red + Blue =:= 25,
	get_winner(Red, Blue, Winner).
	
get_winner(Red, Blue, 'red') :-
	Red > Blue, !.

get_winner(_Red, _Blue, 'blue').


valid_move(GameState, Pos):-
	get_board(GameState, Board),
	get_piece(Board, Pos, blank).


get_piece(Board, [RowNumber, ColumnNumber], Piece) :-
	nth0(RowNumber, Board, Row),
	nth0(ColumnNumber, Row, Piece).

set_piece([], _, _, []).
set_piece([Row|NextRows], [0, ColumnNumber], NewPiece, [NewRow|NextRows]):-
	set_piece_in_row(Row, ColumnNumber, NewPiece, NewRow).
set_piece([Row| NextRows], [RowNumber, ColumnNumber], NewPiece, [Row| NewNextRows]):-
	NewRowNumber is RowNumber - 1,
	set_piece(NextRows, [NewRowNumber, ColumnNumber], NewPiece, NewNextRows).

set_piece_in_row([], _, _, []).
set_piece_in_row([_|NextCells], 0, NewPiece, [NewPiece|NextCells]).
set_piece_in_row([Cell|NextCells], ColumnNumber, NewPiece, [Cell|NewNextCells]):-
	NewColumnNumber is ColumnNumber - 1,
	set_piece_in_row(NextCells, NewColumnNumber, NewPiece, NewNextCells). 

get_adjacent_pieces(Board, Pos, [Up, Down, Left, Right]) :-
	get_piece_up(Board, Pos, Up),
	get_piece_down(Board, Pos, Down),
	get_piece_left(Board, Pos, Left),
	get_piece_right(Board, Pos, Right).

get_piece_up(Board, [Row, Column], Up) :-
	NRow is Row - 1,
	get_piece(Board, [NRow, Column], Up).

get_piece_down(Board, [Row, Column], Down) :-
	NRow is Row + 1,
	get_piece(Board, [NRow, Column], Down).

get_piece_left(Board, [Row, Column], Left) :-
	NColumn is Column - 1,
	get_piece(Board, [Row, NColumn], Left).

get_piece_right(Board, [Row, Column], Right) :-
	NColumn is Column + 1,
	get_piece(Board, [Row, NColumn], Right).

make_move(GameState, Pos, NextGameState) :-
	get_board(GameState, Board),
	get_player(GameState, Player),
	set_piece(Board, Pos, Player, NewBoard),
	set_board(GameState, NewBoard, NewGameState),
	increment(NewGameState, Player, NewGameState_temp),
	value(NewGameState_temp, Blank),

	decide_next_player(NewGameState, Pos, NewPlayer),
	set_player(NewGameState_temp, NewPlayer, NextGameState).


next_player(blue, red).
next_player(red, blue).

same_player(GameState, Pos) :-
	get_board(GameState, Board),
	get_adjacent_pieces(Board, Pos, Pieces),
	delete(Pieces, blank, NonBlank),
	length(NonBlank, Length),
	Length > 2.	

decide_next_player(GameState, Pos, Player):-
	same_player(GameState, Pos),
	get_player(GameState, Player).

decide_next_player(GameState, Pos, NewPlayer):-
	\+ same_player(GameState, Pos),
	get_player(GameState, Player),
	next_player(Player, NewPlayer).



get_move(Pos):-
	repeat,
	read_line(Input),
	nth1(1, Input, X_temp),
	nth1(3, Input, Y_temp),
	check_number(X_temp),
	check_number(Y_temp),
	number_codes(X, [X_temp]),
	number_codes(Y, [Y_temp]),
	Pos = [X, Y].

check_number(Num):-
	char_code(Char, Num),
	Char @> '0',
	Char @< '6'.

value(GameState, Blank):-
	get_red_num(GameState, Red_Num),
	get_blue_num(GameState, Blue_Num),
	Total is Red_Num + Blue_Num,
	Blank is 25 - Total,

	format('Red: ~w ', Red_Num),
	format('Blue: ~w ', Blue_Num),
	format('Blanks: ~w', Blank), nl.
