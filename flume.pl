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
					
