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

push_column(Column, Piece, [Piece | Column]) :- length(Column, Largo), Largo < 6.

% ---------- Otros ---------- %

% can_play
% Dominio: Board (board)
% Metas Primarias: can_play.
% Metas Secundarias: No posee metas secundarias
% Descripción: Predicado que permite verificar si se puede realizar más jugadas en el tablero.

can_play([]). % Caso Base

can_play([ActualColumn|RestOfColumns]) :- length(ActualColumn, Size), Size < 6, can_play(RestOfColumns). % Caso Recursivo
