:- use_module('codigo_fuente_21542985_OrtegaQuinteros/tda_game_21542985_ortegaquinteros').
:- use_module('codigo_fuente_21542985_OrtegaQuinteros/tda_board_21542985_ortegaquinteros').
:- use_module('codigo_fuente_21542985_OrtegaQuinteros/tda_player_21542985_ortegaquinteros').
:- use_module('codigo_fuente_21542985_OrtegaQuinteros/tda_piece_21542985_ortegaquinteros').

main() :-
player(1, "Juan", "red", 0, 0, 0, 10, P1),
player(2, "Mauricio", "yellow", 0, 0, 0, 10, P2),

piece("red", RedPiece),
piece("yellow", YellowPiece),

board(EmptyBoard),

game(P1, P2, EmptyBoard, 1, G0),

player_play(G0, P1, 0, G1),
player_play(G1, P2, 1, G2),
player_play(G2, P1, 1, G3),
player_play(G3, P2, 2, G4),
player_play(G4, P1, 2, G5),
player_play(G5, P2, 3, G6),
player_play(G6, P1, 2, G7),
player_play(G7, P2, 3, G8),
player_play(G8, P1, 3, G9),
player_play(G9, P2, 0, G10),
player_play(G10, P1, 3, G11), 

write('¿Se puede jugar en el tablero vacío? '),
can_play(EmptyBoard),
nl,

game_get_full_board(G11, FullCurrentBoard),
game_get_board(G11, CurrentBoard),
write('¿Se puede jugar después de 11 movimientos? '),
can_play(FullCurrentBoard),
nl,

write('Jugador actual después de 11 movimientos: '),
get_current_player(G11, CurrentPlayer),
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
(is_draw(G11) -> write('Sí') ; write('No')),
nl,

end_game(G11, EndedGame),

write('Historial de movimientos: '),
game_history(EndedGame, History),
write(History),
nl,

write('Estado final del tablero: '),
game_get_board(EndedGame, FinalBoard),
write(FinalBoard).

