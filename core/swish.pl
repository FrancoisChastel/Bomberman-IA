:-module(main,[wall/1,path/1,bomb/1,block/1,accessible/3,move/5,movements/4,updateList/5]).

:- dynamic board/1.
:- dynamic playersList/1.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).	 % new
:- use_module(library(uri)).

:- http_handler(root(init), init,[]).		% (1)
:- http_handler(root(beat), beat,[]).


server(Port) :-						% (2)
http_server(http_dispatch, [port(Port)]).



%:- dynamic display/1.
wall('x').
path('_').
bomb('b').

block('x').
block('o').
block('b').

% Function : Accessible
% Objectif : Savoir si le point est acessible pour un joueur ou non
% Retour : Si le point est égal a 'x' ou 'o' la fonction
%         retourne false sinon true
accessible(Board,X,Y) :- nth0(Y,Board,Line), nth0(X,Line,Point), not(block(Point)).

% Function : Movements
% Objectif : Connaître si un mouvement est possible pour un joueur ou non
% Parameter 1: x-axis Player
% Parameter 2: y-axis Player
% Parameter 3: x-axis Destination
% Parameter 4: y-axis Destination
% Retour : Si il peut true sinon false
movements(Xp,Y,Xd,Y) :- Xp is Xd+1; Xd is Xp+1.
movements(X,Yp,X,Yd) :- Yp is Yd+1; Yd is Yp+1.

% Function : DefineBoard
% Objective : Define a specific board 
% Retour : True pour valider le changement de board
defineBoard(Board) :- assert(board(Board)).

%Liste des mouvements disponible dans le jeu
%Up 
move(0,X,Y,NewX,NewY):- NewX = X,NewY is Y-1.    
%Rigth
move(1,X,Y,NewX,NewY):- NewX is X+1,NewY = Y. 
%Down
move(2,X,Y,NewX,NewY):- NewX = X,NewY is Y+1. 
%Left
move(3,X,Y,NewX,NewY):- NewX is X-1,NewY = Y. 

%replace(NewElem,Index,List,NewList):- 
replace(Index,Current):- Index =:= Current ->
    writeln('X = Current');   writeln('X').
% V1.0 : Mouvement Aléatoire sur le plateau sans attaque
ia(X,Y,NewX,NewY):-repeat, random_between(0,4,Move),move(Move,X,Y,NewX,NewY),board(Board),accessible(Board,NewX,NewY),!.

% MouvementPlayer : update PlayerList with new players coordinates.
mouvementPlayer(NumPlayer, NewX, NewY) :- playersList(List),
    updateList(NumPlayer,List, NewList, NewX,NewY),
    retract(playersList(List)),
    assert(playersList(NewList)).
                 
% Replace [X,Y] in a coordinates list. listeMaj(Index,OldList,NewList,NewX,NewT).
updateList(0,[_|H],[[NewX,NewY]|H],NewX,NewY).                                                                        
updateList(Index,[L|H],[L|H2], NewX, NewY):- N is Index-1, updateList(N,H,H2,NewX,NewY).

% createMap :
% Retour : La carte de jeu
createMap(X):- X =[
       	  ['x','x','x','x','x','x','x','x','x'],
          ['x','A','_','_','_','_','_','B','x'],
          ['x','_','x','_','x','_','x','_','x'],
          ['x','_','_','_','_','_','_','_','x'],
          ['x','_','x','_','x','_','x','_','x'],
          ['x','_','_','_','_','_','_','_','x'],
          ['x','_','x','_','x','_','x','_','x'],
          ['x','C','_','_','_','_','_','D','x'],
          ['x','x','x','x','x','x','x','x','x']
         ].

play:- 	playersList(ListPlayer),
    	playersBeat(0, ListPlayer),
    	displayBoard,
    	writeln('PositionJoueur: '),
    	write(ListPlayer).


playHtml :-
    playersList(ListPlayer),
    playersBeat(0, ListPlayer),
    displayBoard,
    writeln('PositionJoueur: '),
    reply_html_page(title('Bomberman'),[p(write(ListPlayer))]).


%displayPlayerList([]).
%displayPlayerList([H|T]):-writeln(''), displayLine(H), displayPlayerList(T).
                     
playersBeat(_,[]).
playersBeat(Index,[[X,Y]|T]):-ia(X,Y,NewX,NewY),
    mouvementPlayer(Index, NewX, NewY),
    N is Index+1, playersBeat(N,T).

% createMap :
% Objectif : Affiche la carte du jeu en allant chercher la variable globale
displayBoard:- board(Board),display(Board).

% createMap :
% Objectif : Affiche la carte du jeu
displayLine([]).
displayLine([H|T]):-write(H), displayLine(T).
display([]).
display([Head|Tail]):-writeln(''),displayLine(Head),display(Tail).

test:- createMap(Board),assert(board(Board)),ia(1,1).

init(_Request):- 	createMap(Board),
    	assert(board(Board)),
    	assert(playersList([[1, 2], [2, 3], [4, 6]])).

reply_html_page([title('Howdy')],[h1('A Simple Web Page')],[p('Test')]).

beat(_Request) :- playersList(ListPlayer),
                playersBeat(0, ListPlayer),
                reply_json(json([list=ListPlayer])).



%%%%%%%%%%%%% Search if a case is dangerous	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Search for a particular bomb ... / Call by dangerParBombPlayer
dangerPerBomb(X,Y,H):- nth0(0,H,XBomb), nth0(1,H,YBomb), nth0(3,H,Puissance), ((      X=:= XBomb, (Val is  YBomb-Y, Val >= 0 ; Val is Y-YBomb, Val >=0)); (   Y=:= YBomb, (Val is  XBomb-X, Val >= 0 ; Val is X-XBomb, Val >=0))), Puissance >= Val .

% Search for a list of bomb belonging to a player ... / Call by danger 
dangerPerBombPlayer(X,Y,[H|T]):- dangerPerBomb(X,Y,H); dangerPerBombPlayer(X,Y,T).

% Search for all bombs of all players
% Parameter 1 : x-axis 
% Parameter 2 : y-axis
% Parameter 3 : ListBombOnGround -> list 3 dimensions
%%%%%%% [                               BombsPlayer1                                  ,  BombsPlayer2  ,  ...	] %%
%%%%%%%  [             BOMB1            ,           BOMB2              ,      BOMB3 ] , .......
%%%%%%%   [  X , Y , CountTime, Power ] , [  X , Y , CountTime , Power]
danger(X,Y,[H|T]):- dangerPerBombPlayer(X,Y,H); danger(X,Y,T).


