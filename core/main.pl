:-module(main,[wall/1,path/1,bomb/1,block/1,accessible/3,move/5]).

:- dynamic board/1.
:- dynamic joueur1/2.
:- dynamic joueur2/2.
:- dynamic joueur3/2.
:- dynamic joueur4/2.
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
ia(X,Y,NewX,NewY):-repeat, random_between(0,4,Move),move(Move,X,Y,NewX,NewY),accessible(NewX,NewY),!.

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

init:- createMap(Board),assert(board(Board)),displayBoard.
