% ---------- TDA BOARD ---------- %
:- module(tda_board_21542985_ortegaquinteros, [board/1, play_piece/4, play_piece_aux/5, update_history/4, push_column/3, can_play/1, check_vertical_win/2,
    check_vertical_win_aux/2, check_column_win/2, fill_board/2, fill_column/2, check_horizontal_win/2, check_horizontal_win_aux/2,
    check_row_win/3, check_diagonal_win/2, check_diagonal_superior_win/3, check_diagonal_inferior_win/3, who_is_winner/2]).
% ---------- Constructor ---------- %

% board
% Dominio: No recibe parametros de entrada
% Metas Primarias: board.
% Metas Secundarias: No posee metas secundarias
% Descripción: Predicado que crea un TDA board.

board([[[],[],[],[],[],[],[]],[]]).

% ---------- Modificadores ---------- %

% play_piece
% Dominio: Board (board) X Column (int) X Piece (piece) X NewBoard (board)
% Metas Primarias: play_piece.
% Metas Secundarias: play_piece_aux, push_column, update_history
% Descripción: Jugar una ficha en el tablero

play_piece([Board | History], ColumnIndex, Piece, [NewBoard | NewHistory]) :-
    play_piece_aux(Board, ColumnIndex, ColumnIndex, Piece, NewBoard),
    update_history(History, Piece, ColumnIndex, NewHistory).

% play_piece_aux
% Dominio: Board (board) X ColumnIndexVariable (int) X ColumnIndex (int) X Piece (piece) X NewBoard (board)
% Metas Primarias: play_piece_aux, push_column
% Metas Secundarias: play_piece_aux, push_column, update_history
% Descripción: Jugar una ficha en el tablero

play_piece_aux([ActualColumn | RestOfColumns], 0, _, Piece, [NewColumn | RestOfColumns]) :-  
    push_column(ActualColumn, Piece, NewColumn), !.

play_piece_aux([ActualColumn | RestOfColumns], ColumnIndexVariable, ColumnIndex, Piece, [ActualColumn | NewBoard]) :-
    ColumnIndexVariable > 0,
    NewColumnIndexVariable is ColumnIndexVariable - 1,
    play_piece_aux(RestOfColumns, NewColumnIndexVariable, ColumnIndex, Piece, NewBoard).

% update_history
% Dominio: History (list) X Piece (piece) X Index (int)
% Metas Primarias: play_piece_aux, push_column
% Metas Secundarias: No posee metas secundarias
% Descripción: Actualizar historial del tablero.

update_history(History, Piece, Index, [[Piece, Index] | History]).

% push_column
% Dominio: Column (list) X Piece (piece) X NewColumn (list)
% Metas Primarias: push_column.
% Metas Secundarias: No posee metas secundarias
% Descripción: Agregar una ficha a una columna

push_column(Column, Piece, [Piece | Column]) :- 
    length(Column, Largo), Largo < 6.

% ---------- Otros ---------- %

% can_play
% Dominio: Board (board)
% Metas Primarias: can_play.
% Metas Secundarias: No posee metas secundarias
% Descripción: Predicado que permite verificar si se puede realizar más jugadas en el tablero.

can_play([[A, B, C, D, E, F, G] | _]) :- 
    length(A, LA), length(B, LB), length(C, LC), length(D, LD), length(E, LE), length(F, LF), length(G, LG),
    LA < 6, LB < 6, LC < 6, LD < 6, LE < 6, LF < 6, LG < 6.
    
% check_vertical_win
% Dominio: Board (board) X Winner (int)
% Metas Primarias: check_vertical_win 
% Metas Secundarias: check_vertical_win_aux, check_column_win
% Descripcion: Predicado que permite verificar el estado actual del tablero y entregar el posible ganador que cumple con la regla de conectar 4 fichas de forma vertical.

check_vertical_win([Board | _], Winner) :-
    check_vertical_win_aux(Board, Winner).

% check_vertical_win_aux
% Dominio: Board (board) X Winner (int)
% Metas Primarias: check_vertical_win 
% Metas Secundarias: check_column_win
% Descripcion: Predicado que permite verificar el estado actual del tablero y entregar el posible ganador que cumple con la regla de conectar 4 fichas de forma vertical.

check_vertical_win_aux([], 0). % Caso Base, Empate

check_vertical_win_aux([ActualColumn | RestOfColumns], Winner) :- 
    (length(ActualColumn, Largo), Largo >= 4, check_column_win(ActualColumn, Winner) ->
    true ; % Caso Base, Hay ganador en columna actual
    check_vertical_win_aux(RestOfColumns, Winner)). % Caso Recursivo, No hay ganador en columna actual

% check_column_win
% Dominio: Column (list)
% Metas Primarias: check_column_win
% Metas Secundarias: No posee metas secundarias
% Descripcion: Predicado que permite verificar si existe una victoria en una columna.

check_column_win([A, B, C, D | _], Winner) :- % Caso Base, A, B, C, D son iguales => hay ganador vertical
    A == B, B == C, C == D,
    (A == "red" -> Winner = 1 ; Winner = 2).

check_column_win([_ | Resto], Winner) :- % Caso Recursivo, Se sigue iterando en la columna para encontrar ganador
    check_column_win(Resto, Winner).

% fill_board
% Dominio: Board (board)
% Metas Primarias: fill_board
% Metas Secundarias: fill_column
% Descripcion: Predicado que rellena el board.

fill_board([], []).

fill_board([ActualColumn | RestOfColumns], [FilledColumn | FilledBoard]) :-
    fill_column(ActualColumn, FilledColumn),
    fill_board(RestOfColumns, FilledBoard).

% fill_column
% Dominio: Column (list)
% Metas Primarias: fill_column
% Metas Secundarias: No posee metas secundarias
% Descripcion: Predicado que rellena una columna de un board.

fill_column(Column, FilledColumn) :-
    length(Column, Largo),
    Largo < 6 ->
    push_column(Column, " ", NewColumn),
    fill_column(NewColumn, FilledColumn) ;
    FilledColumn = Column.

% check_horizontal_win
% Dominio: Board (board) X Winner (int)
% Metas Primarias: check_horizontal_win
% Metas Secundarias: check_horizontal_win_aux, check_row_win, fill_board, fill_column
% Descripcion: Predicado que permite verificar si existe una victoria horizontal en un board.

check_horizontal_win([Board | _], Winner) :-
    check_horizontal_win_aux(Board, Winner), !.

% check_horizontal_win_aux
% Dominio: Board (board) X Winner (int)
% Metas Primarias: check_horizontal_win_aux
% Metas Secundarias: check_row_win, fill_board, fill_column
% Descripcion: Predicado que permite verificar si existe una victoria horizontal en un board.

check_horizontal_win_aux(Board, Winner) :-
    fill_board(Board, FilledBoard),
    check_row_win(FilledBoard, 6, Winner).

% check_row_win
% Dominio: Board (board)
% Metas Primarias: check_row_win
% Metas Secundarias: No posee metas secundarias
% Descripcion: Predicado que permite verificar si existe una victoria en una fila.

check_row_win(Board, _, 0) :-
    length(Board, Largo),
    Largo < 4.

check_row_win([_ | RestOfColumns], 0, Winner) :-
    check_row_win(RestOfColumns, 6, Winner).

check_row_win([A, B, C, D | _], Index, Winner) :-
    nth1(Index, A, TA), nth1(Index, B, TB), nth1(Index, C, TC), nth1(Index, D, TD),
    TA == TB, TB == TC, TC == TD, TA \= " ",
    (TA == "red" -> Winner = 1; Winner = 2).

check_row_win(Board, Index, Winner) :-
    NIndex is Index - 1,
    check_row_win(Board, NIndex, Winner). 

% check_diagonal_win
% Dominio: Board (board) X Winner (int)
% Metas Primarias: check_diagonal_win
% Metas Secundarias: fill_board, check_diagonal_superior_win, check_diagonal_inferior_win
% Descripcion: Predicado que permite verificar si existe una victoria diagonal en el tablero.

check_diagonal_win([Board | _], Winner) :-
    fill_board(Board, FilledBoard),
    check_diagonal_inferior_win(FilledBoard, 1, WinnerInferior),
    (WinnerInferior = 0 -> check_diagonal_superior_win(FilledBoard, 6, WinnerSuperior),
    Winner = WinnerSuperior ; Winner = WinnerInferior), !.

% check_diagonal_superior_win
% Dominio: Board (board) X Winner (int)
% Metas Primarias: check_diagonal_win_superior
% Metas Secundarias: No posee metas secundarias.
% Descripcion: Predicado que permite verificar si existe una victoria diagonal superior en el tablero.

check_diagonal_superior_win(Board, _, 0) :-
    length(Board, Largo),
    Largo < 4.

check_diagonal_superior_win([A, B, C, D | _], Index1, Winner) :-
    Index1 >= 4, Index2 is Index1 - 1, Index3 is Index1 - 2, Index4 is Index1 - 3,
    nth1(Index1, A, PA), nth1(Index2, B, PB), nth1(Index3, C, PC), nth1(Index4, D, PD),
    PA == PB, PB == PC, PC == PD, PA \= " ",
    (PA == "red" -> Winner = 1; Winner = 2).

check_diagonal_superior_win(Board, Index, Winner) :-
    Index > 1, NIndex is Index - 1,
    check_diagonal_superior_win(Board, NIndex, Winner).

check_diagonal_superior_win([_ | RestOfColumns], _, Winner) :-
    check_diagonal_superior_win(RestOfColumns, 6, Winner).

% check_diagonal_inferior_win
% Dominio: Board (board) X Winner (int)
% Metas Primarias: check_diagonal_win_inferior
% Metas Secundarias: No posee metas secundarias.
% Descripcion: Predicado que permite verificar si existe una victoria diagonal inferior en el tablero.

check_diagonal_inferior_win(Board, _, 0) :-
    length(Board, Largo),
    Largo < 4.

check_diagonal_inferior_win([A, B, C, D | _], Index1, Winner) :-
    Index2 is Index1 + 1, Index3 is Index1 + 2, Index4 is Index1 + 3,
    nth1(Index1, A, PA), nth1(Index2, B, PB), nth1(Index3, C, PC), nth1(Index4, D, PD),
    PA == PB, PB == PC, PC == PD, PA \= " ",
    (PA == "red" -> Winner = 1; Winner = 2).

check_diagonal_inferior_win(Board, Index, Winner) :-
    Index < 6, NIndex is Index + 1,
    check_diagonal_inferior_win(Board, NIndex, Winner).

check_diagonal_inferior_win([_ | RestOfColumns], _, Winner) :-
    check_diagonal_inferior_win(RestOfColumns, 1, Winner).

% who_is_winner
% Dominio: Board (board) X Winner (int)
% Metas Primarias: who_is_winner
% Metas Secundarias: check_vertical_win, check_horizontal_win, check_diagonal_win y sus respectivas secundarias.
% Descripcion: Predicado que permite verificar si existe una victoria en el tablero.

who_is_winner(Board, Winner) :-
    (check_vertical_win(Board, Winner), Winner \= 0 ->  true ;
    check_horizontal_win(Board, Winner), Winner \= 0 ->  true ;   
    check_diagonal_win(Board, Winner), Winner \= 0 ->  true ;   
    Winner = 0).