%Importing the lists and random libraries
:- use_module(library(lists)).
:- use_module(library(random)).

%Defining the initial board
initial_board(Board) :- 
	Board = [
		[green, green, green, green, green, green, green],
		[green, blank, blank, blank, blank, blank, green],
		[green, blank, blank, blank, blank, blank, green],
		[green, blank, blank, blank, blank, blank, green],
		[green, blank, blank, blank, blank, blank, green],
		[green, blank, blank, blank, blank, blank, green],
		[green, green, green, green, green, green, green]].

%Example boards, used for testing
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
				  
%Representation in text of each piece				  
piece_text(green, 'G').
piece_text(red, 'R').
piece_text(blue, 'B').
piece_text(blank, ' ').

%Printing the board, going line by line, and the piece by piece in that line, ending with the separator between lines
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
					
%GameState = [Board, Player, Red_Num, Blue_num, Mode]
%Sets and gets for several gameplay elements
gamestate(Board, Player, Red_Num, Blue_Num, Mode, [Board, Player, Red_Num, Blue_Num, Mode]).
get_board(GameState, Board):-
	nth1(1, GameState, Board).
get_player(GameState, Player):-
	nth1(2, GameState, Player).
get_red_num(GameState, Red_Num):-
	nth1(3, GameState, Red_Num).
get_blue_num(GameState, Blue_Num):-
	nth1(4, GameState, Blue_Num).
get_mode(GameState, Mode):-
	nth1(5, GameState, Mode).


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
set_mode(GameState, NewMode, NewGameState):-
	set_gamestate_elem(5, NewMode, GameState, NewGameState).

%Counting the number of pieces placed of either colour
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

%Starting the game, by choosing the mode of the game, applying the initial game state, and beginning the game loop
start :- 
	read_mode_input(Mode),
	initial_game_state(GameState, Mode),
	write('First play by red'), nl,
	game_loop(GameState).

%Initial game state, with an empty board, the player red, both score counters at zero, the chosen mode of play, and tranforming that into the gamestate
initial_game_state(GameState, Mode):-
	initial_board(Board),
	gamestate(Board, red, 0, 0, Mode, GameState).

%Loop of the game. It gets the board, prints it, and checks for game over. If it's not over, a move is made, and the game loop continues. 
game_loop(GameState):-
	get_board(GameState, Board),
	print_board(Board),
	game_over(GameState, _Winner);

	move(GameState, NewGameState),
	game_loop(NewGameState).

%Making a move, getting the player from the gamestate, the mode from the gamestate, and choosing a move to make depending on both thee factors
move(GameState, NextGameState):-
	get_player(GameState, Player),
	get_mode(GameState, Mode),
	choose_move(GameState, NextGameState, Player, Mode).

%The first player is human, so we ask them for their input and make their move, if it's valid. 
%If the second player is also human, we do the same. 
%If the second player is the computer, it randomly plays. 
choose_move(GameState, NextGameState, red, _):-
	get_move_input(Pos),
	valid_move(GameState, Pos),
	make_move(GameState, Pos, NextGameState).


choose_move(GameState, NextGameState, blue, '1'):-
	get_move_input(Pos),
	valid_move(GameState, Pos),
	make_move(GameState, Pos, NextGameState).

choose_move(GameState, NextGameState, blue, '2'):-
	valid_moves(GameState, List),
	listLength(List, Length),
	random(1, Length, Index),
	nth1(Index, List, Pos),
	make_move(GameState, Pos, NextGameState).




listLength([], 0).
listLength([_H | T], Size):- 
	listLength(T, NS), Size is 1 + NS.

%TODO: outros choose_move consoante o modo (blue, '2') para easy e (blue, '3') para hard





%Checks whether the game is over by counting how many pieces have been places by red and blue, and gets the winner accordingly
game_over([_Board, _Player, Red, Blue, _Mode], Winner) :-
	Red + Blue =:= 25,
	get_winner(Red, Blue, Winner),
	format('~w wins', Winner), nl.
	
get_winner(Red, Blue, red) :-
	Red > Blue, !.

get_winner(_Red, _Blue, blue).

%Verifies whether a move is valid by checking if the cell is blank
valid_move(GameState, Pos):-
	get_board(GameState, Board),
	get_piece(Board, Pos, blank).

%Getting a piece from a certain Pos, and setting it on a certain Pos with a certain color. 
get_piece(Board, [RowNumber, ColumnNumber], Piece) :-
	nth0(RowNumber, Board, Row),
	nth0(ColumnNumber, Row, Piece).

%This is done by first finding the correct row, going through them one by one
set_piece([], _, _, []).
set_piece([Row|NextRows], [0, ColumnNumber], NewPiece, [NewRow|NextRows]):-
	set_piece_in_row(Row, ColumnNumber, NewPiece, NewRow).
set_piece([Row| NextRows], [RowNumber, ColumnNumber], NewPiece, [Row| NewNextRows]):-
	NewRowNumber is RowNumber - 1,
	set_piece(NextRows, [NewRowNumber, ColumnNumber], NewPiece, NewNextRows).

%And then, once in the correct row, finding the correct column. 
set_piece_in_row([], _, _, []).
set_piece_in_row([_|NextCells], 0, NewPiece, [NewPiece|NextCells]).
set_piece_in_row([Cell|NextCells], ColumnNumber, NewPiece, [Cell|NewNextCells]):-
	NewColumnNumber is ColumnNumber - 1,
	set_piece_in_row(NextCells, NewColumnNumber, NewPiece, NewNextCells). 

%This gets the adjacent pieces, so we can see if the players will play again. 
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

%This actually makes the move, using all the previous functions to actually change the gamestate. It then decides the next player.
make_move(GameState, Pos, NextGameState) :-
	get_board(GameState, Board),
	get_player(GameState, Player),
	set_piece(Board, Pos, Player, NewBoard),
	set_board(GameState, NewBoard, NewGameState),
	increment(NewGameState, Player, NewGameState_temp),
	value(NewGameState_temp, _Blank),

	decide_next_player(NewGameState, Pos, NewPlayer),
	set_player(NewGameState_temp, NewPlayer, NextGameState),

	format('Next play by ~w', NewPlayer), nl.

%If the player is to change, red will become blue, and blue will become red.
next_player(blue, red).
next_player(red, blue).

%If a play puts a piece in a place with 3 or more adjacencies, that player will play again
same_player(GameState, Pos) :-
	get_board(GameState, Board),
	get_adjacent_pieces(Board, Pos, Pieces),
	delete(Pieces, blank, NonBlank),
	length(NonBlank, Length),
	Length > 2.	

%Verifies whether the player will change or stay the same based on the same_player function above. 
decide_next_player(GameState, Pos, Player):-
	same_player(GameState, Pos),
	get_player(GameState, Player).

decide_next_player(GameState, Pos, NewPlayer):-
	\+ same_player(GameState, Pos),
	get_player(GameState, Player),
	next_player(Player, NewPlayer).


%Gets the move from the user, checking whether it's correct
get_move_input(Pos):-
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

%Evaluates the current gamestate, by counting how many pieces red and blue have used so far
value(GameState, Blank):-
	get_red_num(GameState, Red_Num),
	get_blue_num(GameState, Blue_Num),
	Total is Red_Num + Blue_Num,
	Blank is 25 - Total,

	nl,
	format('Red: ~w ', Red_Num),
	format('Blue: ~w ', Blue_Num),
	format('Blanks: ~w', Blank), nl.

%Available gameplay modes
read_mode_input(Mode):-
	repeat,
	write('----- MODE -----'), nl,
	write('1 - Player vs. Player'), nl,
	write('2 - Player vs. Easy AI'), nl,
	write('3 - Player vs. Hard AI'), nl,
	write('Mode: '),
	get_char(Mode).
	%TODO: handle input

%list of all available positions
all_positions(AllPos):- 
	AllPos = [
		[0,0],[0,1],[0,2],[0,3],[0,4],[0,5],[0,6],
		[1,0],[1,1],[1,2],[1,3],[1,4],[1,5],[1,6],
		[2,0],[2,1],[2,2],[2,3],[2,4],[2,5],[2,6],
		[3,0],[3,1],[3,2],[3,3],[3,4],[3,5],[3,6],
		[4,0],[4,1],[4,2],[4,3],[4,4],[4,5],[4,6],
		[5,0],[5,1],[5,2],[5,3],[5,4],[5,5],[5,6],
		[6,0],[6,1],[6,2],[6,3],[6,4],[6,5],[6,6]].

%list of all valid moves
valid_moves(GameState, ValidMoveList) :-
	all_positions(MoveList),
	choose_valid_moves(GameState, MoveList, ValidMoveList).

%chooses a valid move by going through the list of all possible moves and seeing whether the first one is a valid move or not, and choosing a predicate accordingly.
choose_valid_moves(_, [], []).
choose_valid_moves(GameState, [ValidMove|MoveList], [ValidMove|ValidMoveList]):-
	valid_move(GameState, ValidMove),
	choose_valid_moves(GameState, MoveList, ValidMoveList).

choose_valid_moves(GameState, [InvalidMove|MoveList], ValidMoveList):-
	\+ valid_move(GameState, InvalidMove),
	choose_valid_moves(GameState, MoveList, ValidMoveList).
