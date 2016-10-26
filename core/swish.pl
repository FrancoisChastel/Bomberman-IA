:-module(main,[wall/1,path/1,bomb/1,block/1,accessible/3,move/5,movements/4,updateList/5]).


:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).	 % new
:- use_module(library(uri)).

:- http_handler(root(init), init,[]).		% (1)
:- http_handler(root(beat), beat,[]).


%%%%%%%%%%%%%%%% Server Side %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



server(Port) :-						% (2)
http_server(http_dispatch, [port(Port)]).

init(_Request):- 	createMap(Board),
    	assert(board(Board)),
    	assert(playersList([[1, 2], [2, 3], [4, 6]])).

reply_html_page([title('Howdy')],[h1('A Simple Web Page')],[p('Test')]).

beat(_Request) :- playersList(ListPlayer),
                playersBeat(0, ListPlayer),
                reply_json(json([list=ListPlayer])).

playHtml :-
    playersList(ListPlayer),
    playersBeat(0, ListPlayer),
    displayBoard,
    writeln('PositionJoueur: '),
    reply_html_page(title('Bomberman'),[p(write(ListPlayer))]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- dynamic display/1.

%%%%%%%%%%%%%%%% Dynamic and Static  predicats %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic board/1.
:- dynamic playersList/1.
:- dynamic bombsList/1.

wall('x').
path('_').
bomb('b').

block('x').
block('o').
block('b').

countTimeBomb(6).

%Liste des mouvements disponible dans le jeu
%Up 
move(0,X,Y,NewX,NewY):- NewX = X,NewY is Y-1.    
%Rigth
move(1,X,Y,NewX,NewY):- NewX is X+1,NewY = Y. 
%Down
move(2,X,Y,NewX,NewY):- NewX = X,NewY is Y+1. 
%Left
move(3,X,Y,NewX,NewY):- NewX is X-1,NewY = Y. 


% Function : Movements
% Objectif : Connaître si un mouvement est possible pour un joueur ou non
% Parameter 1: x-axis Player
% Parameter 2: y-axis Player
% Parameter 3: x-axis Destination
% Parameter 4: y-axis Destination
% Retour : Si il peut true sinon false
movements(Xp,Y,Xd,Y) :- Xp is Xd+1; Xd is Xp+1.
movements(X,Yp,X,Yd) :- Yp is Yd+1; Yd is Yp+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%% Tools %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Function : Accessible
% Objectif : Savoir si le point est acessible pour un joueur ou non
% Retour : Si le point est égal a 'x' ou 'o' la fonction
%         retourne false sinon true
accessible(Board,X,Y) :- nth0(Y,Board,Line), nth0(X,Line,Point), not(block(Point)).


% Function : DefineBoard
% Objective : Define a specific board 
% Retour : True pour valider le changement de board
defineBoard(Board) :- assert(board(Board)).


% Function : updateListofListWithTwoFirstParameter
% Objective : Replace the first and the second value of a List, but this List must belong
% to a List
% Generic : No - Replace [X,Y] in a coordinates list. listeMaj(Index,OldList,NewList,NewX,NewT).
% Input
% Index : Index of the list for change X,Y
% FirstList : [ [X,Y,Var1,Var2], [X,Y,Var1,Var2],[X,Y,Var1,Var2],[X,Y,Var1,Var2]
% ListReturn : Return of this function
% NewX,NewY : New values
updateListofListWithTwoFirstParameter(0,[[_,_,P1,P2]|T],[[NewX,NewY,P1,P2]|T],NewX,NewY).                                                                        
updateListofListWithTwoFirstParameter(Index,[L|H],[L|H2], NewX, NewY):- N is Index-1, updateListofListWithTwoFirstParameter(N,H,H2,NewX,NewY).




% Function : updateListofListWithOneParameter
% Objective : Replace a value in the List of List
% Generic : Yes
% Input
% IndexList1 : Index to Get the first List Level
% IndexList2 : Index to Update the NewValue.
% FirstList : [ List1,List2,...ListN] with ListN = [X,Y,Z,...,ValueToUpdate,...]
% FirstListUpdated : Return of this function
% NewValue : Value to Update at the IndexList2
updateListofListWithOneParameter(IndexList1,IndexList2,FirstList,FirstListUpdated,NewValue):-
    nth0(IndexList1,FirstList,SecondList),
    updateList(IndexList2,NewValue,SecondList,SecondListUpdated),
    updateList(IndexList1,SecondListUpdated,FirstList,FirstListUpdated).




% Function : updateList
% Objective : Replace a value in a List
% Generic : Yes
% Input
% --Change Value
updateList(0,NewValue,[_|T], [NewValue|T]).
% --For Each
updateList(Index,NewValue,[Head|Tail],[Head|Tail1]):- Index > -1, N is Index-1, updateList(N, NewValue,Tail, Tail1), !.
% --Overflow : Do Nothing
updateList(_,_,L, L).

% V1.0 : Mouvement Aléatoire sur le plateau sans attaque
ia(X,Y,NewX,NewY):-repeat, random_between(0,4,Move),move(Move,X,Y,NewX,NewY),board(Board),accessible(Board,NewX,NewY),!.


% Function  Search if a case is dangerous -> see danger()	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Search for a particular bomb ... / Call by dangerParBombPlayer
dangerPerBomb(X,Y,H):- nth0(0,H,XBomb), nth0(1,H,YBomb), nth0(3,H,Puissance), ((      X=:= XBomb, (Val is  YBomb-Y, Val >= 0 ; Val is Y-YBomb, Val >=0)); (   Y=:= YBomb, (Val is  XBomb-X, Val >= 0 ; Val is X-XBomb, Val >=0))), Puissance >= Val .

% Search for a list of bomb belonging to a player ... / Call by danger 
dangerPerBombPlayer(X,Y,[H|T]):- dangerPerBomb(X,Y,H); dangerPerBombPlayer(X,Y,T).

% Search for all bombs of all players
% Return Value : true -> Dangerouse Case / false -> Safe case
% Parameter 1 : x-axis 
% Parameter 2 : y-axis
% Parameter 3 : ListBombOnGround -> list 3 dimensions
%%%%%%% [                               BombsPlayer1                                  ,  BombsPlayer2  ,  ...	] %%
%%%%%%%  [             BOMB1            ,           BOMB2              ,      BOMB3 ] , .......
%%%%%%%   [  X , Y , CountTime, Power ] , [  X , Y , CountTime , Power]
danger(X,Y,[H|T]):- dangerPerBombPlayer(X,Y,H); danger(X,Y,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%% Game Engine %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% MouvementPlayer : update PlayerList with new players coordinates.
mouvementPlayer(NumPlayer, NewX, NewY) :- playersList(List),
    updateList(NumPlayer,List, NewList, NewX,NewY),
    retract(playersList(List)),
    assert(playersList(NewList)).
                 



% A Game turn
play:- 	playersList(ListPlayer),
    	playersBeat(0, ListPlayer),
    	displayBoard,
    	writeln('PositionJoueur: '),
    	write(ListPlayer).




%Implant Bomb
% Return Value : true -> Bomb implanted / false -> Bomb not implanted
% Parameter 1 : Index of player which implant the bomb
implantBomb(PlayerIndex):-
    countTimeBomb(CountTimeBomb),
    playersList(ListPlayer),
    nth0(PlayerIndex,ListPlayer,[X,Y,NbMaxBomb,PowerPlayer]),
    bombsList(ListAllBomb),
    nth0(PlayerIndex,ListAllBomb,
         ListBombImplantByPlayer),
    length(ListBombImplantByPlayer,Length),
    Length < NbMaxBomb ,
    append(ListBombImplantByPlayer,
           [[X,Y,CountTimeBomb,PowerPlayer]],
           NewListBombImplantByPlayer),
    updateList(PlayerIndex,NewListBombImplantByPlayer,ListAllBomb,NewListAllBomb),
    retract(bombsList(ListAllBomb)),
    assert(bombsList(NewListAllBomb)).




% Instant T movement all players
% Parameter 1 : Index of player
% Parameter 2 : The list of player                    
playersBeat(_,[]).
playersBeat(PlayerIndex,[[X,Y,NbMaxBomb,Power]|T]):-ia(X,Y,NewX,NewY),
    mouvementPlayer(PlayerIndex, NewX, NewY),
    N is PlayerIndex+1, playersBeat(N,T).

% Display Board
% Aim : Affiche la carte du jeu en allant chercher la variable globale
displayBoard:- board(Board),display(Board).

% Display Line :
% Aim : Show each line of the map
displayLine([]).
displayLine([H|T]):-write(H), displayLine(T).
display([]).
display([Head|Tail]):-writeln(''),displayLine(Head),display(Tail).


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

%displayPlayerList([]).
%displayPlayerList([H|T]):-writeln(''), displayLine(H), displayPlayerList(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
