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

% ---------- Otros ---------- %

% game_history
% Dominio: Game (game)
% Metas Primarias: game_history
% Metas Secundarias: game_get_board
% Descripcion: Predicado que otorga el historial de un game.

game_history([_, _, [_ | ReversedHistory] | _], History) :-
    reverse(ReversedHistory, [[] | History]).