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
					
%GameState = [Board, Player]
gamestate(Board, Player, [Board, Player]).
get_board([Board,_], Board).
get_player([_, Player], Player).

set_board([_, Player], NewBoard, [NewBoard, Player]).
set_player([Board, _], NewPlayer, [Board, NewPlayer]).

test :- 
	initial_game_state(GameState),
	make_move(GameState, [1, 5], NewGameState),
	get_board(NewGameState, Board),
	get_player(NewGameState, Player),
	print_board(Board),
	print(Player).

start :- 
	initial_game_state(GameState),
	play(GameState).

initial_game_state(GameState):-
	initial_board(Board),
	gamestate(Board, red, GameState).
		
play(GameState, NextGameState):-
	get_move(Pos),
	valid_move(GameState, Pos), 
	make_move(GameState, Pos, NextGameState).

game_finished(GameState):-
	get_board(GameState, Board),
	\+ get_piece(Board, _, blank).


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

next_player(blue, red).
next_player(red, blue).

same_player(GameState, Pos) :-
	get_board(GameState, Board),
	get_adjacent_pieces(Board, Pos, Pieces),
	delete(Pieces, blank, NonBlank),
	length(NonBlank, Length),
	Length > 2.	

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
	decide_next_player(NewGameState, Pos, NewPlayer),
	set_player(NewGameState, NewPlayer, NextGameState).

decide_next_player(GameState, Pos, Player):-
	same_player(GameState, Pos),
	get_player(GameState, Player).

decide_next_player(GameState, Pos, NewPlayer):-
	\+ same_player(GameState, Pos),
	get_player(GameState, Player),
	next_player(Player, NewPlayer).
	

