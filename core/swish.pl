%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author(s):    N.Haim, F.Chastel, C.Aparicio, A.Payan && A.Breton   	%
% Creation:     20/09/2016                                      	%
% Version :     v0.1                                           		%
% Description : AI project at INSA Lyon that aim to represent bomberman	%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%% Header %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(main,[board/1,wall/1,path/1,bomb/1,block/1,accessible/3,move/5,movements/4,updateList/4,applyMove/3,playersList/1,attainable/3,destructible/3]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).

:- http_handler(root(init), init,[]).
:- http_handler(root(beat), beat,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%% Server Side %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

server(Port) :-           % (2)
http_server(http_dispatch, [port(Port)]).

init(Request):-
		http_parameters(Request,[ playersIA(PlayersIAJSON, [])]),
		retractall(board(_)),
        retractall(playersList(_)),
        createMap(Board),
    	assert(board(Board)),
    	assert(playersList([[1, 1, 10, 0, 0, -1], [7, 1, 10, 1, 0, -1], [1, 7, 10, 2, 0, -1], [7, 7, 10, 3, 0, -1]])),
        playersList(Players),
        string_chars(PlayersIAJSON,PlayersIA),
        initPlayers(5,PlayersIA,Players,NewPlayersIA),
        reply_json(json([board=Board,players=NewPlayersIA])).        
        
initPlayers(_,[],_,[]):- !.
initPlayers(Index, [Hia|Tia],[H|T], [HTemp|TTemp]) :- updateList(Index,Hia,H,HTemp), initPlayers(Index,Tia,T,TTemp).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%% Dynamic and Static  predicats %%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic board/1.
:- dynamic playersList/1.
:- dynamic bombsList/1.

%-- Board representation of the elements
wall('x').
path('_').
bomb('b').

%- Element that are not reachable
block('x').
block('o').
block('b').

%- Bonus that can be taken by the player
bonus('p').
bonus('c').

%- Features of the bonus in function of their representation
puissance('p').
capacite('c').

%- Block that can be destroyed by a bomb
destructibleBlock('p').
destructibleBlock('c').
destructibleBlock('o').

%- Default bomb-timing
countTimeBomb(6).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%% Tools %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Function    : Move 
% Objective   : Obtain the new coordonate for the movement of an object based
%     		on the three availables movement (up, right, down, left)
% Parameter 1 : Direction based on a number (0: up, 1: right, 2: down, 3:
%     		left)
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


% Function    : applyMove
% Objective   : Save the new x-axis and y-axis  of player
% Parameter 1 : Index of player in the list
% Parameter 2 : New x-axis of the player
% Parameter 3 : New y-axis of the player
applyMove(Index,X,Y):- playersList(ListPlayers),nth0(Index, ListPlayers, InfoPlayer), nth0(2,InfoPlayer,MaxBomb),nth0(3,InfoPlayer,Power),
                        updateList(Index,[X,Y,MaxBomb,Power],ListPlayers,NewListPlayers),
			retract(playersList(ListPlayers)), assert(playersList(NewListPlayers)).


% Function    : Accessible
% Objective   : Know if a point is accessible for a player
% Parameter 1 : Board concerned (need to be well-formed) that will be targeted
% Parameter 2 : x-axis of the targeted move  
% Parameter 3 : y-axis of the targeted move
% Return      : If the point targeted by the x-axis and y-axis is a block it
%   		return false else true
accessible(Board,X,Y) :- nth0(Y,Board,Line), nth0(X,Line,Point), not(block(Point)).


% Function    :	selectSquare
% Objective   :	Recognize the type of Square
% Parameter 1 :	Board concerned (need to be well-formed) that will be target-ed
% Parameter 2 :	x-axis of the targeted move  
% Parameter 3 :	y-axis of the targeted move
% Parameter 4 :	Type of Square if Point not Initialize
selectSquare(Board,X,Y,Point) :- nth0(Y,Board,Line), nth0(X,Line,Point).


% Function    : Attainable
% Objective   : Know if a point is attainable for a player
% Parameter 1 : Board concerned (need to be well-formed) that will be targeted
% Parameter 2 : x-axis of the targeted move  
% Parameter 3 : y-axis of the targeted move
% Return      : If the point targeted by the x-axis and y-axis is not attainable it return false else true
attainable(Board,X,Y) :- nth0(Y,Board,Line), nth0(X,Line,Point), (destructibleBlock(Point);not(block(Point))).


% Function    : Destructible
% Objective   : Know if a case is destructible
% Parameter 1 : Board concerned (need to be well-formed) that will be targeted
% Parameter 2 : x-axis of the targeted 
% Parameter 3 : y-axis of the targeted
% Return      : If the point targeted by the x-axis and y-axis is destructible return true else false
destructible(Board,X,Y):- nth0(Y,Board,Line), nth0(X,Line,Point), destructibleBlock(Point). 


% Function    :	DistanceManhattan
% Objective   :	Find the distance of manhattan between a list of point and a target
% Parameter 1 :	List which contains points represented by a list of x-axis and y-axis
% Parameter 2 :	x-axis of the target
% Parameter 3 :	y-axis of the target
% Parameter 4 :	List of distance of manhattan. The distances and the points in the first parameter are linked by the index of the list.
distanceManhattan([],_,_,[]).
distanceManhattan([[XCase,YCase]|T],X,Y,[H|T2]):- distanceManhattan(T,X,Y,T2), H is abs(X-XCase)+abs(Y-YCase).


% Function    :	Weigthed
% Objective   :	Adjust the distance of manhattan
% Parameter 1 :	The map game
% Parameter 2 :	List which contains points represented by a list of x-axis and y-axis
% Parameter 3 :	List of distance. Distance and points of parameter 2 are linked by their index
% Parameter 4 :	List of distance weighted
weighted(_,[],[],[]).
weighted(Board,[[XCase,YCase]|T],[HOldList|TOldList],[HNewList|TNewList]):- weighted(Board,T,TOldList,TNewList), (   destructible(Board,XCase,YCase) -> HNewList is HOldList+0.1 ; HNewList = HOldList ).


% Function    :	LineOfFire
% Objective   :	Know if the first position can reach the second for a power input
% Parameter 1 :	X1
% Parameter 2 :	Y1
% Parameter 3 :	X2
% Parameter 4 :	Y2
% Parameter 4 :	Power
% Return      :	True if the first position can reach the second for a power input else False
lineOfFire(X1,Y1,X2,Y2,Power):- (X1 = X2; Y1 = Y2),(distanceManhattan([[X1,Y1]],X2,Y2,[Distance|_]),Distance =< Power).


% Function    : DefineBoard
% Objective   : Define a specific board in the context 
% Parameter 1 : Board that will be set
% Return      : True pour valider le changement de board
defineBoard(Board) :- assert(board(Board)).


% Function    : updateListofListWithTwoFirstParameter
% Objective   : Replace the first and the second value of a List, but this  List must belong to a List
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


% Function    : minList
% Objective   : Return the lowest value with associate index of a number list.
%               If there are two index with the same value, this function return the smaller index.
% Parameter 1         : List to analyse
% Parameter 2 /Return : Index Min Value
% Parameter 3 /Return : Value
% Parameter 4 : New list after modification
minList(List,Index,Value):- min_list(List,Value),nth0(Index,List, Value),!.


% Function    :	updateBoard
% Objective   :	update the board with new element
% Parameter 1 :	Board of origin 
% Parameter 2 :	x-axis of modification
% Parameter 3 : y-axis of modifcation 
% Parameter 4 :	new value 
% Parameter 5 :	New Board that will be returned
updateBoard(Board, Xm, Ym, NewValue, NewBoard) :- nth0(Ym, Board, TLine), updateList(Xm, NewValue, TLine, NLine),
		updateList(Ym, NLine, Board, NewBoard). 


% Function    : Search if a case is dangerous -> see danger() 
% Objective   : Search for a particular bomb ... / Call by dangerParBombPlayer
% Parameter 1 : x-axis of the case we search if it is in danger
% Parameter 2 : y-axis of the case we search if it is in danger
% Parameter 3 : list information of one bomb
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


% Function    : checkSafeAndAttainableSquareAroundPlayer
% Objective   : Know if the target square is safe and attainable
% Parameter 1 : x-axis 
% Parameter 2 : y-axis
% Return      : List of Square safe and attainable [[X,Y],...] 
%         	Ex : 	Top-Left Corner [1,1]
%        		Return : [[2, 1], [1, 2]], Possible to go at Rigth or Down
checkSafeAndAttainableSquareAroundPlayer(X,Y,SquareList):-
                    Y0 is Y-1,(safeAndAttainable(X,Y0) -> append([],[[X,Y0]], ListAfterUp);ListAfterUp = []),
                    X1 is X+1,(safeAndAttainable(X1,Y) -> append(ListAfterUp,[[X1,Y]], ListAfterRigth);ListAfterRigth = ListAfterUp),
                    Y2 is Y+1,(safeAndAttainable(X,Y2) -> append(ListAfterRigth,[[X,Y2]], ListAfterDown);ListAfterDown = ListAfterRigth),
                    X3 is X-1,(safeAndAttainable(X3,Y) -> append(ListAfterDown,[[X3,Y]], ListAfterLeft);ListAfterLeft = ListAfterDown),
                    SquareList = ListAfterLeft.


% Function    :	Direction
% Objective   :	Return x-axis and y-axis that correspond to a move in a direction
% Parameter 1 :	x-axis of origin
% Parameter 2 :	y-axis of orgin
% Parameter 3 :	Direction of the move (0: up, 1: right, 2:down, 3:left)
% Parameter 4 : x-axis of destination that will be compute
% Parameter 5 :	y-axis of destination that will be compute
direction(X, Yo, 0, X, Yd):- Yd is Yo-1.
direction(X, Yo, 2, X, Yd):- Yd is Yo+1.
direction(Xo, Y, 1, Xd, Y):- Xd is Xo+1.
direction(Xo, Y, 3, Xd, Y):- Xd is Xo-1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%% IA RANDOM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ia_random(X,Y,NewX,NewY):-repeat, random_between(0,4,Move),move(Move,X,Y,NewX,NewY),board(Board),accessible(Board,NewX,NewY),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%% IA AGGRESIVE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Function    : iaAggresive
% Objective   : IA Agressive : Defend and attack
% Parameter 1 : Index of Ia in PlayerList
% Parameter 2 : Index of Target in PlayerList
% Parameter 3 /Return : If 1 drop a bomb else no
% Parameter 4 /Return : Direction of NextMove [0,1,2,3,-1] 
%        - -1: stay
%        - 0 : up
%        - 1 : right
%        - 2 : down
%        - 3 : left
iaAggresive(IndexPlayer,IndexTarget,Bomb,NextMove):-              
  % Get Global Variable
    bombsList(BombList),
    board(Board),
    playersList(PlayerList),
    nth0(IndexPlayer,PlayerList,[X,Y,_,Power]),
    nth0(IndexTarget,PlayerList,[TargetX,TargetY,_,_]),
  % ------------------------------------
  
    (danger(X,Y,BombList) ->
      (
          %Danger
          backToSafePlace(X,Y,Board,BombList,[],5,Safe,Move),
          actionSafe(Safe,Bomb,Move,NextMove)
      );(
            % No Danger
            lineOfFire(X,Y,TargetX,TargetY,Power) ->
            (   
              % Target in line of Fire
              % For the next version implement better move after drop bomb
              Bomb = 1,
              NextMove = 0,
              writeln("Line Of Fire")
            );( 
              % No Ennemi in Line of Fire
              checkSafeAndAttainableSquareAroundPlayer(X,Y,SquareList),
              length(SquareList,LengthSquareList),
              actionAnalyseAllSquare(LengthSquareList,Board,X,Y,TargetX,TargetY,SquareList,Bomb,NextMove)
            )
        )
    ),
  !.
  
%Called By IaAggresive  
%------------------------------------------------  
actionSafe(1,Bomb,Move,NextMove):-
        % It's possible to escape 
            Bomb = 0,
            NextMove = Move,
            writeln("Try to escape").

actionSafe(0,Bomb,_,NextMove):-
      % IA is Dead --> Last Stand
            Bomb = 1,
            NextMove = -1,
            writeln("Dead").
%------------------------------------------------  

%Called By IaAggresive  
%------------------------------------------------  
actionAnalyseAllSquare(0,_,_,_,_,_,_,Bomb,NextMove):-
        Bomb = 0,
            NextMove = -1,
            writeln("I prefer don't move").

actionAnalyseAllSquare(_,Board,X,Y,TargetX,TargetY,SquareList,Bomb,NextMove):-
        distanceManhattan(SquareList,TargetX,TargetY,ListManhattan),
            weighted(Board,SquareList,ListManhattan,WheitedList),
            minList(WheitedList,Index,_),
            nth0(Index,SquareList,[NextX,NextY]),
            selectSquare(Board,NextX,NextY,TypeSquare),
            actionSquare(TypeSquare,X,Y,NextX,NextY,Bomb,NextMove).
%------------------------------------------------  
    
%Called By ActionAnalyseAllSquare  
%------------------------------------------------  
actionSquare('o',_,_,_,_,Bomb,NextMove):-
      Bomb = 1,
            %Find Safe Square next turn...
            NextMove = 0,
            writeln("I go mine a little").
actionSquare('_',X,Y,NextX,NextY,Bomb,NextMove):-
         Bomb = 0,
             move(NextMove,X,Y,NextX,NextY),
             writeln("Move").
%------------------------------------------------  

createMap(X):- X =[
          ['x','x','x','x','x','x','x','x','x'],
          ['x','_','x','_','_','_','_','_','x'],
          ['x','_','x','_','x','_','x','_','x'],
          ['x','_','_','_','_','_','_','_','x'],
          ['x','_','x','_','x','_','x','_','x'],
          ['x','_','_','_','_','_','_','_','x'],
          ['x','_','x','_','x','_','x','_','x'],
          ['x','_','_','_','_','_','_','_','x'],
          ['x','x','x','x','x','x','x','x','x']
          ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%% Game Engine %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Function    :	CheckBombs
% Parameter 1 :	Board
% Parameter 2 :	List of players
% Parameter 3 :	List of bombs
% Parameter 4 :	New board
% Parameter 5 :	New list of players
% Parameter 6 :	New list of bombs
checkBombs( Board, ListOfPlayers, [], Board, ListOfPlayers, []).
checkBombs( Board, ListOfPlayers, [Hb|Tb], NewBoard, NewListOfPlayers, [Hnb|Tnb]):-
		checkBombs( Board, ListOfPlayers, Tb, TBoard, TListOfPlayers, Tnb), 
		( nth0(2, Hb, TDecompte) is 1 )->
			( nth0(0, Hb, Xb), nth0(1, Hb, Yb), nth0(2, Hb, Eb), bombExplode( TBoard, Xb, Yb, Eb, TlistOfPlayers, NewListOfPlayer, NewBoard) );
			( NDecompte is TDecompte-1, replaceList(2, NDecompte, Hb, Hnb),
			NewBoard = Board, NewListOfPlayers = NewListOfPlayers ).


% Function    :	BombExplode
% Parameter 1 :	Board 
% Parameter 2 :	x-axis of the bomb
% Parameter 3 :	y-axis of the bomb
% Parameter 4 : effeciveness of the bomb
% Parameter 5 :	list of players
% Parameter 6 :	return of the function that is an update of the list of players
% Parameter 7 :	return of the function that is an update of the board
%- Init of the explosion
bombExplode(Board, Xb, Yb, Eb, ListOfPlayers, NewListOfPlayers, NewBoard) :- 	
						lineExplode(Board, Xb, Yb, Eb, ListOfPlayers, TPlayers0, TBoard0, 0),
						lineExplode(TBoard0, Xb, Yb, Eb, TPlayers0, TPlayers1, TBoard1, 1),
						lineExplode(TBoard1, Xb, Yb, Eb, TPlayers1, TPlayers2, TBoard2, 2),
						lineExplode(TBoard2, Xb, Yb, Eb, TPlayers2, NewListOfPlayers, NewBoard, 3).
%- Spread in a line of the explosion
lineExplode(Board, _, _, 0, Players, NewPlayers, NewBoard, _):- NewBoard = Board, NewPlayers = Players.
lineExplode(Board, Xb, Yb, Eb, Players, NewPlayers, NewBoard, Direction) :- nth0(Yb, Board, TLine), nth0(Xb, TLine, TElem),
		destructibleBlock(TElem), destroyBlock(Board, Xb, Yb, NewBoard), NewPlayers = Players.
lineExplode(Board, Xb, Yb, Eb, Players, NewPlayers, NewBoard, Direction) :- nth0(Yb, Board, TLine), nth0(Xb, TLine, TElem),
		not(block(TElem)), TEb is Eb-1,  direction(Xb, Yb, Direction, TXb, TYb), lineExplode(Board, TXb, TYb, TEb, Players, NewPlayers, NewBoard, Direction).
lineExplode(Board, Xb, Yb, Eb, Players, NewPlayers, NewBoard, Direction) :- nth0(Yb, Board, TLine), nth0(Xb, TLine, TElem),
		NewBoard = Board, killPlayers(Xb, Yb, Players, NewPlayers).
		

% Function    :	DestroyBlock
% Objective   :	destroy a destructible block with the possibility of droping 
%		a bonus with a certain probability
% Parameter 1 :	Board 
% Parameter 2 :	x-axis of the destroyed block
% Parameter 3 :	y-axis of the destroyed block
% Parameter 3 :	Output board
destroyBlock( Board, Xe, Ye, NewBoard):-
		random(0, 5, Probability), (
			 ( Probability is 0, capacite(Bonus), updateBoard(Board, Xe, Ye, Bonus, NewBoard));
			 ( Probability is 1, puissance(Bonus), updateBoard(Board, Xe, Ye, Bonus, NewBoard)); 
			 ( path(Path), updateBoard( Board, Xe, Ye, Path, NewBoard ))).


% Function    :	KillPlayers
% Objective   :	verify the lists of objects and destroy the potential object
% Parameter 1 :	x-axis of the destroyed object
% Parameter 2 :	y-axis of the destroyed object
% Parameter 3 :	ListOfPlayers
% Parameter 4 :	NewListOfPlayers
killPlayers( _, _, [], []).
killPlayers( Xd, Yd, [Hp|Tp], [Hn|Tn]):- killPlayers( Xd, Yd, Tp, Tn), 
		( nth0(0, Hp, Xd), nth0(1, Hp, Yd), killPlayer(Hp, Hn) );
		( Hn = Hp ).
		

% Function    :	killPlayer
% Objective   :	kill a player given in parameter 
% Parameter 1 : player that will be killed
% Parameter 2 :	killed player
killPlayer(Player, DeadPlayer):- updateList(4, 1, Player, DeadPlayer). 


% Function    : mouvementPlayer
% Objective   : update player's list with new players coordinates
% Parameter 1 : Number that identify the player
% Parameter 2 : New x-axis for the player
% Parameter 3 : New y-axis for the player
mouvementPlayer(NumPlayer, X, Y, NewX, NewY) :- playersList(List),
    updateListofListWithTwoFirstParameter(NumPlayer,List, NewList, NewX,NewY),
    retract(playersList(List)),
    assert(playersList(NewList)).

% Function    :	replace
% Objective   :	replace a signle celle in a matrix (list of list
% Parameter 1 :	initial matrix	
% Parameter 2 :	x-avis of the repalcement
% Parameter 3 :	y-axis of the replacement
% Parameter 4 :	value that will be set in replacement
% Parameter 5 :	modified matri
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

beat(_Request) :-   playersList(ListPlayer),
                    playersBeat(0, ListPlayer),
                    reply_json(json([list=ListPlayer])).


% Function    :	implantBomb
% Objective   :	Implant Bomb
% Return      :	true -> Bomb implanted / false -> Bomb not implanted
% Parameter 1 :	Index of player which implant the bomb
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
playersBeat(PlayerIndex,[[X,Y,NbMaxBomb,Power,Dead,Ia]|T]):-ia_random(X,Y,NewX,NewY),
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
            functionBackToSafePlace(X,Y,Board,ListBomb,N,DistanceLimit,IsSafe,IsMove) ->
            Safe = IsSafe,Move = IsMove;
          Safe = 0,Move = -1.

functionBackToSafePlace(X,Y,Board,ListBomb,N,DistanceLimit,Safe,Move):-
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
