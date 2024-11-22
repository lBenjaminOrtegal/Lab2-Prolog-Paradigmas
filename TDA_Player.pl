% TDA PLAYER

% --- Constructor --- %

player(Nombre,Id,Color,Wins,Losses,Draws,Pieces,[Nombre,Id,Color,Wins,Losses,Draws,Pieces]).

% --- Selectores --- %

get-player-nombre([Nombre|_],Nombre).

get-player-id([_,Id|_],Id).

get-player-color([_,_,Color|_],Color).

get-player-wins([_,_,_,Wins|_],Wins).

get-player-losses([_,_,_,_,Losses|_],Losses).

get-player-draws([_,_,_,_,_,Draws|_],Draws).

get-player-pieces([_,_,_,_,_,_,Pieces],Pieces).



