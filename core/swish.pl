
:-module(main,[wall/1,path/1,bomb/1,block/1,accessible/3,move/5,movements/4,updateList/4]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).  % new
:- use_module(library(uri)).

:- http_handler(root(init), init,[]).   % (1)
:- http_handler(root(beat), beat,[]).

%%%%%%%%%%%%%%%% Server Side %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

server(Port) :-           % (2)
http_server(http_dispatch, [port(Port)]).

init(_Request):-  createMap(Board),
      assert(board(Board)),
      assert(playersList([[1, 1, 10, 0], [7, 1, 10, 1], [1, 7, 10, 2], [7, 7, 10, 3]])).

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

destructibleBlock('o').

countTimeBomb(6).

% Function    : Move 
% Objective   : Obtain the new coordonate for the movement of an object based
%     on the three availables movement (up, right, down, left)
% Parameter 1 : Direction based on a number (0: up, 1: right, 2: down, 3:
%     left)
% Parameter 2 : Current x-axis of the object 
% Parameter 3 : Current y-axis of the object
% Parameter 4 : New x-axis of the object after the move
% Parameter 5 : New y-axis of the object after the move
move(0,X,Y,NewX,NewY):- NewX = X,NewY is Y-1.    
move(1,X,Y,NewX,NewY):- NewX is X+1,NewY = Y. 
move(2,X,Y,NewX,NewY):- NewX = X,NewY is Y+1. 
move(3,X,Y,NewX,NewY):- NewX is X-1,NewY = Y. 


% Function    : Movements
% Objective   : Know if a movemnt is available for the player
% Parameter 1 : x-axis Player
% Parameter 2 : y-axis Player
% Parameter 3 : x-axis Destination
% Parameter 4 : y-axis Destination
% Return      : False if player can't move, true if it can
movements(Xp,Y,Xd,Y) :- Xp is Xd+1; Xd is Xp+1.
movements(X,Yp,X,Yd) :- Yp is Yd+1; Yd is Yp+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%% Tools %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Function    : Accessible
% Objective   : Know if a point is accessible for a player
% Parameter 1 : Board concerned (need to be well-formed) that will be target-
%   ed
% Parameter 2 : x-axis of the targeted move  
% Parameter 3 : y-axis of the targeted move
% Return      : If the point targeted by the x-axis and y-axis is a block it
%   return false else true
accessible(Board,X,Y) :- nth0(Y,Board,Line), nth0(X,Line,Point), not(block(Point)).

% Function    : Attainable
% Objective   : Know if a point is attainable for a player
% Parameter 1 : Board concerned (need to be well-formed) that will be targeted
% Parameter 2 : x-axis of the targeted move  
% Parameter 3 : y-axis of the targeted move
% Return      : If the point targeted by the x-axis and y-axis is not attainable it
%   return false else true
attainable(Board,X,Y) :- nth0(Y,Board,Line), nth0(X,Line,Point), (destructibleBlock(Point);not(block(Point))).

% Function    : Destructible
% Objective   : Know if a case is destructible
% Parameter 1 : Board concerned (need to be well-formed) that will be target-
%   ed
% Parameter 2 : x-axis of the targeted 
% Parameter 3 : y-axis of the targeted
% Return      : If the point targeted by the x-axis and y-axis is destructi-
%   ble return true else false

destructible(Board,X,Y):- nth0(Y,Board,Line), nth0(X,Line,Point), destructibleBlock(Point). 


% Function             : DistanceManhattan
% Objective            : Find the distance of manhattan between a list of point and a target
% Parameter 1          : List which contains points represented by a list of x-axis and y-axis
% Parameter 2          : x-axis of the target
% Parameter 3          : y-axis of the target
% Parameter 4 / Return : List of distance of manhattan. The distances and the points in the
%   first parameter are linked by the index of the list.
distanceManhattan([],_,_,[]).
distanceManhattan([[XCase,YCase]|T],X,Y,[H|T2]):- distanceManhattan(T,X,Y,T2), H is abs(X-XCase)+abs(Y-YCase).

% Function             : Weigthed
% Objective            : Adjust the distance of manhattan
% Parameter 1          : The map game
% Parameter 2          : List which contains points represented by a list of x-axis and y-axis
% Parameter 3          : List of distance. Distance and points of parameter 2 are linked by their index
% Parameter 4 / Return : List of distance weighted
weighted(_,[],[],[]).
weighted(Board,[[XCase,YCase]|T],[HOldList|TOldList],[HNewList|TNewList]):- weighted(Board,T,TOldList,TNewList), (   destructible(Board,XCase,YCase) -> HNewList is HOldList+0.1 ; HNewList = HOldList ).

% Function             : LineOfFire
% Objective            : Know if the first position can reach the second for a power input
% Parameter 1          : X1
% Parameter 2          : Y1
% Parameter 3          : X2
% Parameter 4          : Y2
% Parameter 4          : Power
% Return         : True if the first position can reach the second for a power input else False
lineOfFire(X1,Y1,X2,Y2,Power):- (X1 = X2; Y1 = Y2),(distanceManhattan([[X1,Y1]],X2,Y2,[Distance|_]),Distance =< Power).

% Function    : DefineBoard
% Objective   : Define a specific board in the context 
% Parameter 1 : Board that will be set
% Return      : True pour valider le changement de board
defineBoard(Board) :- assert(board(Board)).


% Function    : updateListofListWithTwoFirstParameter
% Objective   : Replace the first and the second value of a List, but this 
%   List must belong to a List
% Parameter 1 : Replace [X,Y] in a coordinates list. listeMaj(Index,OldList,NewList,NewX,NewT).
% Parameter 2 : Index of the list for change X,Y
% Parameter 3 : List returned
% Parameter 4 : New x-axis value
% Parameter 5 : New y-axis value
updateListofListWithTwoFirstParameter(0,[[_,_,P1,P2]|T],[[NewX,NewY,P1,P2]|T],NewX,NewY).                                                                        
updateListofListWithTwoFirstParameter(Index,[L|H],[L|H2], NewX, NewY) :- N is Index-1, updateListofListWithTwoFirstParameter(N,H,H2,NewX,NewY).


% Function    : updateListofListWithOneParameter
% Objective   : Replace a value in the List of List
% Parameter 1 : Index to Get the first List Level
% Parameter 2 : Index to Update the NewValue.
% Parameter 3 : [ List1,List2,...ListN] with ListN = [X,Y,Z,...,ValueToUpdate,...]
% Parameter 4 : Return of this function
% Parameter 5 : Value to Update at the IndexList2
updateListofListWithOneParameter(IndexList1,IndexList2,FirstList,FirstListUpdated,NewValue) :-
    nth0(IndexList1,FirstList,SecondList),
    updateList(IndexList2,NewValue,SecondList,SecondListUpdated),
    updateList(IndexList1,SecondListUpdated,FirstList,FirstListUpdated).


% Function    : updateList
% Objective   : Generic function to replace a value in a List
% Parameter 1 : Index concerned by the modification 
% Parameter 2 : New value that will be set in the index
% Parameter 3 : Initial list 
% Parameter 4 : New list after modification
updateList(0,NewValue,[_|T], [NewValue|T]).
% -- For-each
updateList(Index,NewValue,[Head|Tail],[Head|Tail1]) :- Index > -1, N is Index-1, updateList(N, NewValue,Tail, Tail1), !.
% --Overflow : Do Nothing
updateList(_,_,L, L).


% Function    : Search if a case is dangerous -> see danger() 
% Objective   : Search for a particular bomb ... / Call by dangerParBombPlayer
% Parameter 1 : N/C
% Parameter 2 : N/C
% Parameter 3 : N/C
dangerPerBomb(X,Y,H):- nth0(0,H,XBomb), nth0(1,H,YBomb), nth0(3,H,Puissance), ((      X=:= XBomb, (Val is  YBomb-Y, Val >= 0 ; Val is Y-YBomb, Val >=0)); (   Y=:= YBomb, (Val is  XBomb-X, Val >= 0 ; Val is X-XBomb, Val >=0))), Puissance >= Val .


% Search for a list of bomb belonging to a player ... / Call by danger 
dangerPerBombPlayer(X,Y,[H|T]):- dangerPerBomb(X,Y,H); dangerPerBombPlayer(X,Y,T).

% Function    : Search for all bombs of all players
% Parameter 1 : x-axis 
% Parameter 2 : y-axis
% Parameter 3 : ListBombOnGround -> list 3 dimensions
% Return      : True -> Dangerous case / False -> Safe case
danger(X,Y,[H|T]):- dangerPerBombPlayer(X,Y,H); danger(X,Y,T).

% Function    : safeAndAttainable
% Objective   : Know if the target square is safe and attainable
% Parameter 1 : x-axis 
% Parameter 2 : y-axis
% Return      : True -> Safe And Attainable / False -> Unsafe or Unattainable
safeAndAttainable(X,Y):-bombsList(ListBomb),
              board(Board),
              not(danger(X,Y,ListBomb)),
              attainable(Board,X,Y).

%testSafeAndAttainable(X,Y):- assert(bombsList([[10,0,5,5]])),
%                createMap(Board),
%                assert(board(Board)),
%                safeAndAttainable(X,Y).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%% AIs of the game %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% V1.0 : Mouvement Aléatoire sur le plateau sans attaque
ia(X,Y,NewX,NewY):-repeat, random_between(0,4,Move),move(Move,X,Y,NewX,NewY),board(Board),accessible(Board,NewX,NewY),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%% Game Engine %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Function    : mouvementPlayer
% Objective   : update player's list with new players coordinates
% Parameter 1 : Number that identify the player
% Parameter 2 : New x-axis for the player
% Parameter 3 : New y-axis for the player
mouvementPlayer(NumPlayer, X, Y, NewX, NewY) :- playersList(List),
    updateListofListWithTwoFirstParameter(NumPlayer,List, NewList, NewX,NewY),
    retract(playersList(List)),
    assert(playersList(NewList)),
    updateBoard(NumPlayer,X, Y,NewX,NewY).

%
% replace a single cell in a list-of-lists
% - Board is B
% - The cell to be replaced at x coordinate (X)
%   and the y coordinate (Y)
% - The replacement value is Z
% - the transformed list-of-lists (result) is R
%
replace( B , X , Y , Z , R ) :- append(RP,[H|T],B),     % decompose the list-of-lists into a prefix, a list and a suffix
                                length(RP,X) ,                 % check the prefix length
                                append(CP,[_|CS],H) ,    % decompose that row into a prefix, a column and a suffix
                                length(CP,Y) ,                 % check the prefix length: do we have the desired column?
                                append(CP,[Z|CS],RN) , % if so, replace the column with its new value
                                append(RP,[RN|T],R).   % and assemble the transformed list-of-lists

updateBoard(NumPlayer, X, Y,NewX,NewY) :- board(Board), replace(Board,X,Y,'_',NewBoard1), replace(NewBoard1,NewX,NewY,NumPlayer,NewBoard),
retract(board(Board)),
assert(board(NewBoard)).

% A Game turn
play:-  playersList(ListPlayer),
      playersBeat(0, ListPlayer),
      displayBoard,
        sleep(1),
        play.


% Function    : implantBomb
% Objective   : Implant Bomb
% Return      : true -> Bomb implanted / false -> Bomb not implanted
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
    updateListofListWithOneParameter(PlayerIndex,NewListBombImplantByPlayer,ListAllBomb,NewListAllBomb),
   updateList(PlayerIndex,NewListBombImplantByPlayer,ListAllBomb,NewListAllBomb),
    retract(bombsList(ListAllBomb)),
    assert(bombsList(NewListAllBomb)).


% Function    : playersBeat
% Objective   : Instant T movement all players
% Parameter 1 : Index of player
% Parameter 2 : The list of player                    
playersBeat(_,[]).
playersBeat(PlayerIndex,[[X,Y,NbMaxBomb,Power]|T]):-ia(X,Y,NewX,NewY),
    mouvementPlayer(PlayerIndex,X, Y, NewX, NewY),
    N is PlayerIndex+1, playersBeat(N,T).


% Function             : backToSafePlace
% Objective            : Find the first move to execute to go in safe place
% Parameter 1          : x-axis of the point of the start
% Parameter 2          : y-axis of the point of the start
% Parameter 3          : map of the game
% Parameter 4          : bomb list of te game
% Parameter 5          : just give empty list [], it represent points already found
% Parameter 6          : Number of movement max necessary to be in safe place
% Parameter 7 / Return : 0 -> safe place not fount / 1 -> safe place found
% Parameter 8 / Return : Move the player has to do ti be in safe place
%      Undefined if safe place not found /
%        else:
%        - 0 : up
%        - 1 : right
%        - 2 : down
%        - 3 : left

backToSafePlace(X,Y,Board,ListBomb,N,DistanceLimit,Safe,Move):-
    DistanceLimit2 is DistanceLimit - 1, DistanceLimit2 >= 0,
    (   accessible(Board,X,Y), not(nth0(Index,N,[X,Y])))->
    append(N,[[X,Y]],N2),
    (   
  ( not(danger(X,Y,ListBomb)) , Safe = 1) ;
  ( Ydep is Y-1 , backToSafePlace(X,Ydep,Board,ListBomb,N2,DistanceLimit2,Safe2,Move2) , ( Safe2 =:=1 ), Safe = 1  , Move = 0 );
  ( Xdep is X-1, backToSafePlace(Xdep,Y,Board,ListBomb,N2,DistanceLimit2,Safe2,Move2) , ( Safe2 =:=1 ), Safe = 1   , Move = 3 );
  ( Ydep is Y+1, backToSafePlace(X,Ydep,Board,ListBomb,N2,DistanceLimit2,Safe2,Move2) , ( Safe2 =:=1 ), Safe = 1   , Move = 2 );
  ( Xdep is X+1, backToSafePlace(Xdep,Y,Board,ListBomb,N2,DistanceLimit2,Safe2,Move2) , ( Safe2 =:=1 ), Safe = 1   , Move = 1 )
    )
    ;   Safe = 0.

 

% Function    : displayBoard
% Objective   : Display the map that is stored in global parameter
displayBoard:- board(Board),display(Board).


% Function    : Display Line
% Objective   : Show each line of the map
% Parameter 1 : The line that will be displayed
displayLine([]).
displayLine([H|T]):-write(H), displayLine(T).
display([]).
display([Head|Tail]):-writeln(''),displayLine(Head),display(Tail).


% Function    : createMap
% Objective   : Generate a sample of a map
% Parameter 1 : The variable that will store the map
% Return      : A game map
createMap(X):- X =[
          ['x','x','x','x','x','x','x','x','x'],
          ['x','_','_','_','_','_','_','_','x'],
          ['x','_','x','_','x','_','x','_','x'],
          ['x','_','_','_','_','_','_','_','x'],
          ['x','_','x','_','x','_','x','_','x'],
          ['x','_','_','_','_','_','_','_','x'],
          ['x','_','x','_','x','_','x','_','x'],
          ['x','_','_','_','_','_','_','_','x'],
          ['x','x','x','x','x','x','x','x','x']
         ].

%displayPlayerList([]).
%displayPlayerList([H|T]):-writeln(''), displayLine(H), displayPlayerList(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%% IA_Offensive %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%ia_offensive()
