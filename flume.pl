initial_board(Board) :- Board = [
				  [green, green, green, green, green, green, green],
				  [green, blank, blank, blank, blank, blank, green],
				  [green, blank, blank, blank, blank, blank, green],
				  [green, blank, blank, blank, blank, blank, green],
				  [green, blank, blank, blank, blank, blank, green],
				  [green, blank, blank, blank, blank, blank, green],
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
					
