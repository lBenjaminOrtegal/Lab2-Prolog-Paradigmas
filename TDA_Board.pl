% ---------- TDA BOARD ---------- %

% ---------- Constructor ---------- %

% board
% Dominio: No recibe parametros de entrada
% Metas Primarias: board.
% Metas Secundarias: No posee metas secundarias
% Descripción: Predicado que crea un tda board

board([[],[],[],[],[],[],[]]).

% ---------- Modificadores ---------- %

% play_piece
% Dominio: Board (board) X Column (int) X Piece (piece) X NewBoard (board)
% Metas Primarias: play_piece.
% Metas Secundarias: push_column
% Descripción: Jugar una ficha en el tablero

play_piece([ActualColumn | RestOfColumns], 1, Piece, [NewColumn | RestOfColumns]) :-  % Caso Base, se llega a la columna indicada => se hace push de la pieza a columna y se retorna.
    push_column(ActualColumn, Piece, NewColumn).

play_piece([ActualColumn | RestOfColumns], ColumnIndex, Piece, [ActualColumn | NewBoard]) :- % Caso Recursivo, se verifica que el indice no sea el indicado y se retorna recursivamente con el resto de columnas
    ColumnIndex > 1,
    NewColumnIndex is ColumnIndex - 1, 
    play_piece(RestOfColumns, NewColumnIndex, Piece, NewBoard).

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

can_play([A, B, C, D, E, F, G]) :- 
    length(A, LA), length(B, LB), length(C, LC), length(D, LD), length(E, LE), length(F, LF), length(G, LG),
    (LA, LB, LC, LD, LE, LF, LG) == 6 -> false ; true.

% check_vertical_win
% Dominio: Board (board)
% Metas Primarias: check_vertical_win
% Metas Secundarias: check_column_win
% Descripcion: Predicado que permite verificar el estado actual del tablero y entregar el posible ganador que cumple con la regla de conectar 4 fichas de forma vertical.

check_vertical_win([], 0). % Caso Base, Empate

check_vertical_win([ActualColumn | RestOfColumns], Winner) :- 
    (length(ActualColumn, Largo), Largo >= 4, check_column_win(ActualColumn, Winner) ->
    true ; % Caso Base, Hay ganador en columna actual
    check_vertical_win(RestOfColumns, Winner)). % Caso Recursivo, No hay ganador en columna actual

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