% TDA PLAYER

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