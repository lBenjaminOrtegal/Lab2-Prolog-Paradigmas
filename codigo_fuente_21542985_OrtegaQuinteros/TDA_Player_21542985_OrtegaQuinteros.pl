% TDA PLAYER

:- module(tda_player_21542985_ortegaquinteros, 
    [player/8,
    get_player_id/2,
    get_player_nombre/2,
    get_player_color/2,
    get_player_wins/2,
    get_player_losses/2,
    get_player_draws/2,
    get_player_pieces/2,
    update_stats/3,
    player_less_piece/2]).

:- use_module('tda_board_21542985_ortegaquinteros').
% --- Constructor --- %

% player
% Dominio: Id (int) X Nombre (string) X Color (string) X Wins (int) X Losses (int) X Draws (int) X Pieces (int) X Player (player)
% Metas Primarias: player
% Metas Secundarias: No posee metas secundarias
% Descripcion: Predicado que crea un TDA player.

player(Id, Nombre, Color, Wins, Losses, Draws, Pieces,[Id, Nombre, Color, Wins, Losses, Draws, Pieces]).

% --- Selectores --- %

% get_player_X
% Dominio: Player (player)
% Metas Primarias: get_player_X
% Metas Secundarias: No posee metas secundarias
% Descripcion: Predicado que otorga X parametro de un player.

get_player_id([Id | _], Id).

get_player_nombre([_, Nombre | _], Nombre).

get_player_color([_, _, Color | _], Color).

get_player_wins([_, _, _, Wins | _], Wins).

get_player_losses([_, _, _, _, Losses | _], Losses).

get_player_draws([_, _, _, _, _, Draws | _], Draws).

get_player_pieces([_, _, _, _, _, _, Pieces], Pieces).

% --- Modificadores --- %

% player_less_piece
% Dominio: Player (player)
% Metas Primarias: player_less_piece
% Metas Secundarias: No posee metas secundarias.
% Descripcion: Predicado que disminuye en 1 las piezas del player.

player_less_piece([Id, Name, Color, Wins, Losses, Draws, Pieces], [Id, Name, Color, Wins, Losses, Draws, NewPieces]) :-
    NewPieces is Pieces - 1.

% --- Otros --- %

% update_stats
% Dominio: Game (game) X list (player, player)
% Metas Primarias: update_stats
% Metas Secundarias: get_player_X, who_is_winner
% Descripcion: Predicado que actualiza las estadisticas de una lista de players.

update_stats([Player1, Player2, Board | _], [Player1, Player2], NewStats) :-

    get_player_id(Player1, IdPlayer1), get_player_nombre(Player1, NombrePlayer1), get_player_color(Player1, ColorPlayer1),
    get_player_wins(Player1, WinsPlayer1), get_player_losses(Player1, LossesPlayer1), get_player_draws(Player1, DrawsPlayer1),
    get_player_pieces(Player1, PiecesPlayer1),

    get_player_id(Player2, IdPlayer2), get_player_nombre(Player2, NombrePlayer2), get_player_color(Player2, ColorPlayer2),
    get_player_wins(Player2, WinsPlayer2), get_player_losses(Player2, LossesPlayer2), get_player_draws(Player2, DrawsPlayer2),
    get_player_pieces(Player2, PiecesPlayer2),

    who_is_winner(Board, Winner),

    (Winner = 1 -> 
        NewWinsPlayer1 is WinsPlayer1 + 1, NewLossesPlayer2 is LossesPlayer2 + 1,
        NewStats = [[IdPlayer1, NombrePlayer1, ColorPlayer1, NewWinsPlayer1, LossesPlayer1, DrawsPlayer1, PiecesPlayer1],
                    [IdPlayer2, NombrePlayer2, ColorPlayer2, WinsPlayer2, NewLossesPlayer2, DrawsPlayer2, PiecesPlayer2]];
    Winner = 2 -> 
        NewWinsPlayer2 is WinsPlayer2 + 1, NewLossesPlayer1 is LossesPlayer1 + 1,
        NewStats = [[IdPlayer1, NombrePlayer1, ColorPlayer1, WinsPlayer1, NewLossesPlayer1, DrawsPlayer1, PiecesPlayer1],
                    [IdPlayer2, NombrePlayer2, ColorPlayer2, NewWinsPlayer2, LossesPlayer2, DrawsPlayer2, PiecesPlayer2]];
    Winner = 0 -> 
        NewDrawsPlayer1 is DrawsPlayer1 + 1, NewDrawsPlayer2 is DrawsPlayer2 + 1,
        NewStats = [[IdPlayer1, NombrePlayer1, ColorPlayer1, WinsPlayer1, LossesPlayer1, NewDrawsPlayer1, PiecesPlayer1],
                    [IdPlayer2, NombrePlayer2, ColorPlayer2, WinsPlayer2, LossesPlayer2, NewDrawsPlayer2, PiecesPlayer2]]).