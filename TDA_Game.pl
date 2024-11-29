% ---------- TDA GAME ---------- %
:- use_module('tda_board').
:- use_module('tda_player').
:- use_module('tda_piece').

% ---------- Constructor ---------- %

% game
% Dominio: Player1 (player) X Player2 (player) X Board (board) X CurrentTurn (int)
% Metas Primarias: game
% Metas Secundarias: No posee metas secundarias
% Descripcion: Predicado que crea un TDA game.

game(Player1, Player2, Board, CurrentTurn, [Player1, Player2, Board, CurrentTurn]).

% ---------- Selectores ---------- %

% get_game_player1
% Dominio: Game (game)
% Metas Primarias: get_game_player1
% Metas Secundarias: No posee metas secundarias
% Descripcion: Predicado que otorga el player1 de un game.

game_get_player1([Player1 | _], Player1).

% get_game_player2
% Dominio: Game (game)
% Metas Primarias: get_game_player2
% Metas Secundarias: No posee metas secundarias
% Descripcion: Predicado que otorga el player2 de un game.

game_get_player2([_, Player2 | _], Player2).

% game_get_board
% Dominio: Game (game)
% Metas Primarias: game_get_board
% Metas Secundarias: No posee metas secundarias
% Descripcion: Predicado que otorga el board de un game.

game_get_board([_, _, [Board | _] | _], Board).

% get_game_full_board
% Dominio: Game (game)
% Metas Primarias: get_game_full_board
% Metas Secundarias: No posee metas secundarias
% Descripcion: Predicado que otorga el board completo de un game (incluyendo historial).

game_get_full_board([_, _, Board | _], Board).

% get_game_currenturn
% Dominio: Game (game)
% Metas Primarias: get_game_currenturn
% Metas Secundarias: No posee metas secundarias
% Descripcion: Predicado que otorga el turno actual de un game.

game_get_currenturn([_, _, _, CurrentTurn | _], CurrentTurn).

% get_current_player
% Dominio: Game (game)
% Metas Primarias: get_current_player
% Metas Secundarias: No posee metas secundarias
% Descripcion: Predicado que otorga el personaje cuyo turno esta en curso de un game.

get_current_player([Player1, _, [_ | [[Piece, _] | _]], _], CurrentPlayer) :-
    get_player_color(Player1, Color1),
    Piece == Color1 -> 
    CurrentPlayer = 2 ;
    CurrentPlayer = 1.

% ---------- Otros ---------- %

% game_history
% Dominio: Game (game)
% Metas Primarias: game_history
% Metas Secundarias: game_get_board
% Descripcion: Predicado que otorga el historial de un game.

game_history([_, _, [_ | ReversedHistory] | _], History) :-
    reverse(ReversedHistory, [[] | History]).

% is_draw
% Dominio: Game (game)
% Metas Primarias: is_draw
% Metas Secundarias: No posee metas secundarias
% Descripcion: Predicado que verifica si el juego queda en empate.

is_draw(Game) :-
    game_get_player1(Game, Player1),
    game_get_player2(Game, Player2),
    get_player_pieces(Player1, Pieces1),
    get_player_pieces(Player2, Pieces2),
    game_get_full_board(Game, FullBoard),
    can_play(FullBoard),
    Pieces1 =:= 0, Pieces2 =:= 0.