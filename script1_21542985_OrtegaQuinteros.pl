:- use_module('codigo_fuente_21542985_OrtegaQuinteros/tda_game_21542985_ortegaquinteros').
:- use_module('codigo_fuente_21542985_OrtegaQuinteros/tda_board_21542985_ortegaquinteros').
:- use_module('codigo_fuente_21542985_OrtegaQuinteros/tda_player_21542985_ortegaquinteros').
:- use_module('codigo_fuente_21542985_OrtegaQuinteros/tda_piece_21542985_ortegaquinteros').

main() :-
player(1, "Luffy", "red", 0, 0, 0, 8, P1),
player(2, "Zoro", "green", 0, 0, 0, 8, P2),

piece("red", RedPiece),
piece("green", GreenPiece),

board(EmptyBoard),

game(P1, P2, EmptyBoard, 1, G0),

player_play(G0, P1, 0, G1),
player_play(G1, P2, 4, G2),
player_play(G2, P1, 1, G3),
player_play(G3, P2, 4, G4),
player_play(G4, P1, 2, G5),
player_play(G5, P2, 4, G6),
player_play(G6, P1, 3, G7),
 
write('¿Se puede jugar en el tablero vacío? '),
can_play(EmptyBoard),
nl,

game_get_full_board(G7, FullCurrentBoard),
game_get_board(G7, CurrentBoard),
write('¿Se puede jugar después de 7 movimientos? '),
can_play(FullCurrentBoard),
nl,

write('Jugador actual después de 7 movimientos: '),
get_current_player(G7, CurrentPlayer),
write(CurrentPlayer),
nl,

write('Verificación de victoria vertical: '),
check_vertical_win(FullCurrentBoard, VerticalWinner),
write(VerticalWinner),
nl,

write('Verificación de victoria horizontal: '),
check_horizontal_win(FullCurrentBoard, HorizontalWinner),
write(HorizontalWinner),
nl,

write('Verificación de victoria diagonal: '),
check_diagonal_win(FullCurrentBoard, DiagonalWinner),
write(DiagonalWinner),
nl,

write('Verificación de ganador: '),
who_is_winner(FullCurrentBoard, Winner),
write(Winner),
nl,

write('¿Es empate? '),
(is_draw(G7) -> write('Sí') ; write('No')),
nl,

end_game(G7, EndedGame),

write('Historial de movimientos: '),
game_history(EndedGame, History),
write(History),
nl,

write('Estado final del tablero: '),
game_get_board(EndedGame, FinalBoard),
write(FinalBoard).