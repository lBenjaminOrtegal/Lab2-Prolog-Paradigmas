% ---------- TDA GAME ---------- %
:- module(tda_game_21542985_ortegaquinteros, [game/5, game_get_player1/2, game_get_player2/2, game_get_board/2, game_get_full_board/2, game_get_currenturn/2,
    get_current_player/2, end_game/2, player_play/4, player_play_aux/5, game_history/2, is_draw/1]).

:- use_module('tda_board_21542985_ortegaquinteros').
:- use_module('tda_player_21542985_ortegaquinteros').
:- use_module('tda_piece_21542985_ortegaquinteros').

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

get_current_player([_, _, [_, []], _], 1).

% ---------- Modificadores ---------- %

% end_game
% Dominio: Game (game)
% Metas Primarias: end_game
% Metas Secundarias: game_get_player1, game_get_player2, game_get_full_board, game_get_currenturn, update_stats.
% Descripcion: Predicado finaliza el juego actualizando las estadísticas de los jugadores según el resultado.

end_game(Game, EndGame) :-
    game_get_player1(Game, Player1),
    game_get_player2(Game, Player2),
    game_get_full_board(Game, FullBoard),
    game_get_currenturn(Game, CurrentTurn),
    update_stats(Game, [Player1, Player2], [NewPlayer1, NewPlayer2]),
    EndGame = [NewPlayer1, NewPlayer2, FullBoard, CurrentTurn].

% player_play
% Dominio: Game (game) X Player (player) X Column (int)
% Metas Primarias: player_play
% Metas Secundarias: player_play_aux
% Descripcion: Predicado que realiza un movimiento.

player_play(Game, Player, Column, NewGame) :-
    get_current_player(Game, CurrentPlayer),
    get_player_id(Player, Id),
    Id =:= CurrentPlayer, 
    player_play_aux(Game, Player, Column, 0, NewGame), !.

% player_play
% Dominio: Game (game) X Player (player) X Column (int) X State (int)
% Metas Primarias: player_play_aux
% Metas Secundarias: player_play_aux
% Descripcion: Predicado que realiza un movimiento.

player_play_aux(Game, Player, Column, 0, NewGame) :-
    get_player_id(Player, Id),
    (Id =:= 1 ->
    game_get_player1(Game, Player1),
    player_play_aux(Game, Player1, Column, 1, NewGame) ;
    game_get_player2(Game, Player2),
    player_play_aux(Game, Player2, Column, 2, NewGame)).

player_play_aux(Game, Player1, Column, 1, NewGame) :-
    game_get_player2(Game, Player2),
    game_get_currenturn(Game, CurrentTurn),
    game_get_full_board(Game, FullBoard),
    get_player_color(Player1, Piece),
    play_piece(FullBoard, Column, Piece, NewFullBoard),
    NewCurrentTurn is CurrentTurn + 1,
    player_less_piece(Player1, Player1Piece),
    who_is_winner(NewFullBoard, Winner),
    (Winner = 1 -> end_game([Player1Piece, Player2, NewFullBoard, NewCurrentTurn], NewGame) ;
    Winner = 0 -> NewGame = [Player1Piece, Player2, NewFullBoard, NewCurrentTurn]).

player_play_aux(Game, Player2, Column, 2, NewGame) :-
    game_get_player1(Game, Player1),
    game_get_currenturn(Game, CurrentTurn),
    game_get_full_board(Game, FullBoard),
    get_player_color(Player2, Piece),
    play_piece(FullBoard, Column, Piece, NewFullBoard),
    NewCurrentTurn is CurrentTurn + 1,
    player_less_piece(Player2, Player2Piece),
    who_is_winner(NewFullBoard, Winner),
    (Winner = 2 -> end_game([Player1, Player2Piece, NewFullBoard, NewCurrentTurn], NewGame);
    Winner = 0 -> NewGame = [Player1, Player2Piece, NewFullBoard, NewCurrentTurn]).

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
% Metas Secundarias: game_get_player1, game_get_player2, get_player_pieces, game_get_full_board, can_play
% Descripcion: Predicado que verifica si el juego queda en empate.

is_draw(Game) :-
    game_get_player1(Game, Player1),
    game_get_player2(Game, Player2),
    get_player_pieces(Player1, Pieces1),
    get_player_pieces(Player2, Pieces2),
    game_get_full_board(Game, FullBoard),
    \+ can_play(FullBoard),
    Pieces1 =:= 0, Pieces2 =:= 0.