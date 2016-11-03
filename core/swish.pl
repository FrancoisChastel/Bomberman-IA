%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
:- http_handler(root(turn), turn,[]).

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
    	assert(playersList([[1, 1, 1, 2, 0, -1], [1, 7, 1, 2, 0, -1], [7, 1, 1, 2, 0, -1], [7, 7, 1, 2, 0, -1]])),
        playersList(Players),
        string_chars(PlayersIAJSON,PlayersIAString),
	convertIAInt(PlayersIAString,PlayersIA),
        initPlayers(5,PlayersIA,Players,NewPlayersIA),
        retractall(playersList(_)),
        assert(playersList(NewPlayersIA)),
	initBombs(NewPlayersIA,Bombs),
	assert(bombsList(Bombs)),
        reply_json(json([board=Board,players=NewPlayersIA])).        
        
initPlayers(_,[],_,[]):- !.
initPlayers(Index, [Hia|Tia],[H|T], [HTemp|TTemp]) :- updateList(Index,Hia,H,HTemp), initPlayers(Index,Tia,T,TTemp).

initBombs([],[]).
initBombs([H|P], [HBomb|TBomb]) :- HBomb = [], initBombs(P,TBomb). 

convertIAInt([],[]).
convertIAInt([H|T],[HNum|TNum]):- atom_number(H,HNum), convertIAInt(T,TNum).
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
move(-1,X,Y,X,Y).

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
% Parameter 1 :	Board
% Parameter 2 : Player
% Parameter 3 : Direction
% Parameter 4 : player with new coordinates after move
applyMove([X,Y|T], Direction, NewPlayer):- 
		move(Direction, X, Y, NewX, NewY),
		updateList(0, NewX, [X,Y|T], TInfoPlayer),
		updateList(1, NewY, TInfoPlayer, NewPlayer), !.		

% Function    :	ObtainBonus
% Objective   :	Get if possible a bonus
% Parameter 1 :	Board
% Parameter 2 :	ListPlayers
% Parameter 3 :	New Board
% Parameter 4 :	New players
obtainBonus( Board, Player, NewBoard, NewPlayer):-
	nth0(0, Player, X),
	nth0(1, Player, Y),
	nth0(Y, Board, TLine),
	nth0(X, TLine, TElem),
	capacite(TElem),
	nth0(2, Player, Capacite),
	NewCapacite is Capacite+1,
	updateList(2, Player, NewCapacite).
obtainBonus( Board, Player, NewBoard, NewPlayer):-
	nth0(0, Player, X),
	nth0(1, Player, Y),
	nth0(Y, Board, TLine),
	nth0(X, TLine, TElem),
	puissance(TElem),
	nth0(3, Player, Puissance),
	NewPuissance is Puissance+1,
	updateList(3, Player, NewPuissance).
obtainBonus( NewBoard, NewPlayer, NewBoard, NewPlayer).


		
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
ia(0,IndexPlayer,PlayersList,Board,BombList,Bomb,NextMove):-
	repeat,
	nth0(IndexPlayer,PlayersList,[X,Y|T]),
    	Bomb = 0,
	random_between(0,4,TMove),
	move(NextMove,X,Y,NewX,NewY),
	accessible(Board,NewX,NewY), 
	!.

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
ia(1,IndexPlayer,PlayersList,Board,BombList,Bomb,NextMove):-
    nth0(IndexPlayer,PlayersList,[X,Y,_,Power,_,_]),
    checkNextTarget(IndexPlayer,PlayersList,[TargetX,TargetY]),
  % ------------------------------------
  
    (danger(X,Y,BombList) ->
      (
          % Danger : Move to safe place
          backToSafePlace(X,Y,Board,BombList,[],5,Safe,Move),
          actionSafe(Board,X,Y,Safe,Bomb,Move,NextMove)
      );(
            % No Danger
            lineOfFire(X,Y,TargetX,TargetY,Power) ->
            (   
              % Target in line of Fire
              % For the next version implement better move after drop bomb
              dropBomb(X,Y,Board,Bomb),
              NextMove = -1
            );( 
              % No Ennemi in Line of Fire
              checkSafeAndAttainableSquareAroundPlayer(X,Y,SquareList),
              length(SquareList,LengthSquareList),
              actionAnalyseAllSquare(LengthSquareList,Board,X,Y,TargetX,TargetY,SquareList,Bomb,NextMove)
            )
        )
    ),
  !.


%escapeBomb(X,Y,NextMove):-repeat, board(Board), random_between(0,3,NextMove),move(NextMove,X,Y,NewX,NewY),accessible(Board,NewX,NewY),!.
 
% Called By IaAggresive
%------------------------------------------------  
% Handle Overflow if IA Player is the last of PlayerList
% If isnt last Then IndexTarget = IndexTarget else IndexTarget = 0 
% Function             : checkNextTarget
% Aim          : Search next target to kick ass
% Parameter 1          : Index of start
% Parameter 2          : List
% Parameter 3 / Return : Return X Y of a player not dead

checkNextTarget(R,List,[X,Y]):- length(List,Longueur), R is Longueur -1 , checkNextTarget(-1,List,[X,Y]).
checkNextTarget(Index,List,[X,Y]):- Search is Index + 1 , nth0(Search,List,[X,Y,_,_,0,_]);
          (Search is Index + 1,checkNextTarget(Search,List,[X,Y])).
%------------------------------------------------ 

%Called By IaAggresive  
%------------------------------------------------  
actionSafe(_,_,_,1,Bomb,Move,NextMove):-
        % It's possible to escape 
            Bomb = 0,
            NextMove = Move.

actionSafe(Board,X,Y,0,Bomb,_,NextMove):-
      % IA is Dead --> Last Stand
            dropBomb(X,Y,Board,Bomb),
            NextMove = -1.
%------------------------------------------------  

%Called By IaAggresive  
%------------------------------------------------  
actionAnalyseAllSquare(0,_,_,_,_,_,_,Bomb,NextMove):-
            Bomb = 0,
            NextMove = -1.

actionAnalyseAllSquare(_,Board,X,Y,TargetX,TargetY,SquareList,Bomb,NextMove):-
            distanceManhattan(SquareList,TargetX,TargetY,ListManhattan),
            weighted(Board,SquareList,ListManhattan,WheitedList),
            minList(WheitedList,Index,_),
            nth0(Index,SquareList,[NextX,NextY]),
            selectSquare(Board,NextX,NextY,TypeSquare),
            actionSquare(Board,TypeSquare,X,Y,NextX,NextY,Bomb,NextMove).
%------------------------------------------------  
    
%Called By ActionAnalyseAllSquare  
%------------------------------------------------  
actionSquare(Board,'o',X,Y,_,_,Bomb,NextMove):-
      % There is a wall in front of Player
      % Drop Bomb and Find Safe Square next turn
      dropBomb(X,Y,Board,Bomb),
      NextMove = -1.
actionSquare(_,'_',X,Y,NextX,NextY,Bomb,NextMove):-
      % IA can move without drop a bomb
      Bomb = 0,
      move(NextMove,X,Y,NextX,NextY).
%------------------------------------------------  

dropBomb(X,Y,Board,Bomb):- nth0(Y,Board,Line),nth0(X,Line,Square),not(bomb(Square)),Bomb=1.
dropBomb(_,_,_,0).


%                  assert(bombsList([[[1,8,5,3]]])),
%                  assert(playersList([[1,1,1,5],[1,5,1,5]])),
%                  createMap(Board),
%                  assert(board(Board)), 
%                  IndexTarget is IndexPlayer+1.
%                  iaAggresive(IndexPlayer,IndexTarget,Bomb,Move).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%% IA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ia(2,IndexPlayer,PlayersList,Board,BombList,Bomb,NextMove):-
    nth0(IndexPlayer,PlayersList,[X,Y,_,_,_,_]),
    createPonderatedList(X,Y,Board,BombList,PlayersList,PonderatedList),
    max_list(PonderatedList,Max),
    nth0(Choice,PonderatedList,Max),
    NextMove is Choice-1,
    Bomb = 0,
    !.



%----------------
%Function used by IA

branch(4,X,Y,Board,PlayerList,BombList,ValueGlobal) :-
    correspondingWeightOfCoordinate(X,Y,Board,PlayerList,BombList,Value),
    ValueGlobal is Value.

branch(It,X,Y,Board,PlayerList,BombList,ValueGlobal) :-
    Index is It+1,
    branch(Index,X,Y,Board,PlayerList,BombList,Value0),
    Y0 is Y-1,branch(Index,X,Y0,Board,PlayerList,BombList,Value1),
    X0 is X+1,branch(Index,X0,Y,Board,PlayerList,BombList,Value2),
    Y1 is Y+1,branch(Index,X,Y1,Board,PlayerList,BombList,Value3),
    X1 is X-1,branch(Index,X1,Y,Board,PlayerList,BombList,Value4),
    correspondingWeightOfCoordinate(X,Y,Board,PlayerList,BombList,WheightValue),
    ValueGlobal is Value1 + Value2 + Value3 + Value4 + Value0 + WheightValue.


createPonderatedList(X,Y,Board,BombList,PlayerList,Bomb,List) :-
    branch(0,X,Y,Board,PlayerList,BombList,Value1),
    A is Y-1,branch(0,X,A,Board,PlayerList,BombList,Value2),
    B is X+1,branch(0,B,Y,Board,PlayerList,BombList,Value3),
    C is Y+1,branch(0,X,C,Board,PlayerList,BombList,Value4),
    D is X-1,branch(0,D,Y,Board,PlayerList,BombList,Value5),
    ((bombWorthIt([[X,A],[B,Y],[X,C],[D,Y]],Board,DropIt), DropIt = 1) ;(dropBomb(X,Y,Board,Bomb),Bomb = 1)),
    List = [Value1,Value2,Value3,Value4,Value5],
    Bomb = DropIt.


%Function              : bombWorthIt
%Aim                   : Should the IA drop bomb or not

bombWorthIt([[X,Y]|T],Board,1) :-
   nth0(Y, Board, Line),nth0(X, Line, Square), destructibleBlock(Square).
bombWorthIt(_,_,_,0).




% Function 			   : rapprochement
% Aim      			   : know if the player is closer the point
% Parameter 1 		   : x-axis
% Parameter 2 		   : y-axis
% Parameter 3 		   : PlayerList
% Parameter 4 / Return : Number way available
rapprochement(_,_,[],[]).

rapprochement(XTarget,YTarget,[[XTarget,YTarget|_]|T],TailRapprochement):-
rapprochement(XTarget,YTarget,T,TailRapprochement).


rapprochement(XTarget,YTarget,[[XEnnemy,YEnnemy|_]|T],[ValRapprochement|TailRapprochement]):-
	rapprochement(XTarget,YTarget,T,TailRapprochement), moreCloser(XTarget,YTarget,XEnnemy,YEnnemy,ValRapprochement).

moreCloser(X,Y,XEnnemy,YEnnemy,Val):- distanceManhattan([[X,Y]],XEnnemy,YEnnemy,[Distance]), Val is 1 - Distance/16.


% Function 			   : dangerWeight
% Aim      			   : to weight the danger of square
% Parameter 1 		   : x-axis
% Parameter 2 		   : y-axis
% Parameter 3 		   : List of bombs
% Parameter 4 / Return : Value of weight

dangerWeight(X,Y,ListBomb,0):- danger(X,Y,ListBomb).
dangerWeight(_,_,_,1).


% Function 			   : nbChoiceAvailable
% Aim      			   : Count way available
% Parameter 1 		   : x-axis
% Parameter 2 		   : y-axis
% Parameter 3 		   : Board
% Parameter 4 / Return : Number way available
nbChoiceAvailable(X,Y,Board,Val):-
	X1 is X+1, availableWeight(X1,Y,Board,Value1),
	Y1 is Y-1, availableWeight(X,Y1,Board,Value2),
	X2 is X-1, availableWeight(X2,Y,Board,Value3),
	Y2 is Y+1, availableWeight(X,Y2,Board,Value4),
	Val is Value1 + Value2 + Value3 + Value4.


% Function 			   : availableWeight
% Aim      			   : to weight available way
% Parameter 1 		   : x-axis
% Parameter 2 		   : y-axis
% Parameter 3 		   : Board
% Parameter 4 / Return : Value of weight
availableWeight(X,Y,Board,1):- nth0(Y,Board,Line), nth0(X,Line,Point), not(block(Point)).
availableWeight(_,_,_,0).


% Function    :	bonusWeight
% Aim      			   : to weight bonus
% Parameter 1 		   : x-axis
% Parameter 2 		   : y-axis
% Parameter 3 		   : Board
% Parameter 4 / Return : Value of weight
bonusWeight(X,Y,Board,1):- nth0(Y, Board, Line), nth0(X, Line, Square), bonus(Square).
bonusWeight(_,_,_,0).

%Function               : isWall
% Aim                   : avoid to take a path with a wall
% Parameter 1           : X
% Parameter 2           : Y
% Parameter 3           : Board

isWall(X,Y,Board,-10):- nth0(Y,Board,Line),nth0(X,Line,Square),wall(Square).
isWall(_,_,_,0).

correspondingWeightOfCoordinate(X,Y,Board,PlayerList,BombList,WheightValue):-
bonusWeight(X,Y,Board,Value0),
nbChoiceAvailable(X,Y,Board,Value1),
dangerWeight(X,Y,Board,Value2),
isWall(X,Y,Board,Value4),
rapprochement(X,Y,PlayerList,List),
sum_list(List,Value3),
WheightValue is Value0 + Value1 + Value2 + Value3 + Value4.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%% Game Engine %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Function    :	explosions
% Objective   :	manage the explosion on the board and for the player but
%		not on the list of bombs /!\
% Parameter 1 :	Board
% Parameter 2 :	Players
% Parameter 3 :	List of explosing bombs (coordinates)
% Parameter 4 :	New board
% Parameter 5 :	New players
explosions(NewBoard, NewPlayers, [], NewBoard, NewPlayers):- !.
explosions(Board, Players, [[X,Y,Eb]|Tb], NewBoard, NewPlayers):-
	explosions(Board, Players, Tb, TBoard, TPlayers),
	bombExplode(TBoard, X, Y, Eb, TPlayers, NewPlayers, NewBoard), !.
explosions(NewBoard, NewPlayers, [], NewBoard, NewPlayers):- !.


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
						lineExplode(TBoard2, Xb, Yb, Eb, TPlayers2, NewListOfPlayers, NewBoard, 3), !.
%- Spread in a line of the explosion
lineExplode(Board, _, _, 0, Players, NewPlayers, NewBoard, _):- NewBoard = Board, NewPlayers = Players, !.
lineExplode(Board, Xb, Yb, Eb, Players, NewPlayers, NewBoard, Direction) :- nth0(Yb, Board, TLine), nth0(Xb, TLine, TElem),
		destructibleBlock(TElem), destroyBlock(Board, Xb, Yb, NewBoard), NewPlayers = Players, !.
lineExplode(Board, Xb, Yb, Eb, Players, NewPlayers, NewBoard, Direction) :- nth0(Yb, Board, TLine), nth0(Xb, TLine, TElem),
		not(block(TElem)), TEb is Eb-1,  direction(Xb, Yb, Direction, TXb, TYb), killPlayers(Xb, Yb, Players, TPlayers) ,lineExplode(Board, TXb, TYb, TEb, TPlayers, NewPlayers, NewBoard, Direction), !.
lineExplode(NewBoard, _, _, _, NewPlayers, NewPlayers, NewBoard, _):- !.
		

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
			 ( path(Path), updateBoard( Board, Xe, Ye, Path, NewBoard ))), !.


% Function    :	KillPlayers
% Objective   :	verify the lists of objects and destroy the potential object
% Parameter 1 :	x-axis of the destroyed object
% Parameter 2 :	y-axis of the destroyed object
% Parameter 3 :	ListOfPlayers
% Parameter 4 :	NewListOfPlayers
killPlayers( _, _, [], []):- !.
killPlayers( Xd, Yd, [Hp|Tp], [Hn|Tn]):- killPlayers( Xd, Yd, Tp, Tn), 
		(( nth0(0, Hp, Xd), nth0(1, Hp, Yd), killPlayer(Hp, Hn) );
		( Hn = Hp, !)).
		

% Function    :	killPlayer
% Objective   :	kill a player given in parameter 
% Parameter 1 : player that will be killed
% Parameter 2 :	killed player
killPlayer(Player, DeadPlayer):- updateList(4, 1, Player, DeadPlayer). 


%- A game turn
turn(_Request) :-
	getModel(Board, ListPlayers, ListBombs),
	bombsManagement(Board, ListPlayers, ListBombs, NewBoard, NewListPlayers, NewListBombs),
	playersBeat(NewBoard, NewListPlayers, NewListBombs, TBoard, TListPlayers, TListBombs),
	setModel(TBoard, TListPlayers, TListBombs),
	reply_json(json([board=NewBoard,players=NewListPlayers,bombs=NewListBombs])).


%- Manage all the explosion
bombsManagement(Board, ListPlayer, ListsBombs, NewBoard, NewListPlayer, NewBombs):-
	decrementBombsOfPlayers(ListsBombs, DListsBombs),
	explodingBombsOfPlayers(DListsBombs, BombsExplosing),
	explosions(Board, ListPlayer, BombsExplosing, NewBoard, NewListPlayer),
	deleteExplodedBombs(DListsBombs, BombsExplosing, NewBombs), !.


% Function    :	decrementBombsOfPlayers
% Objective   :	decrement all the bombs of the players (counter)
% Parameter 1 :	List of bombs
% Parameter 2 :	List of bombs decremented
decrementBombsOfPlayers([], []):- !.
decrementBombsOfPlayers([H|T], [Hn|Tn]):-
	decrementBombsOfPlayer(H, Hn),
	decrementBombsOfPlayers(T, Tn), !.
decrementBombsOfPlayers([], []):- !.


% Function    :	decrementBombsOfPlayer
% Objective   :	decrement all the bombs of a player (counter)
% Parameter 1 :	List bombs of player
% Parameter 2 :	List bombs of player decremented
decrementBombsOfPlayer([],[]):- !.
decrementBombsOfPlayer([[X,Y,0,P|Tb]|T], [[X,Y,0,P|Tb]|Tn]):-
	decrementBombsOfPlayer(T, Tn), !.
decrementBombsOfPlayer([[X,Y,C,P|Tb]|T], [[X, Y, Cn, P|Tb]|Tn]):-
	Cn is C-1,
	decrementBombsOfPlayer(T, Tn), !.
decrementBombsOfPlayer([],[]):- !.


% Function    :	explodingBombsOfPlayers
% Objective   :	get exploding bombs for all the players
% Parameter 1 :	List of bombs
% Parameter 2 :	List of exploding bomb.
explodingBombsOfPlayers([H|T], NewList):-
	explodingBombsOfPlayers(T, TList),
	explodingBombsOfPlayer(H, TTList),
	append(TTList, TList, NewList), !. 
explodingBombsOfPlayers([],[]):- !.	


% Function    :	explodingBombsOfPlayer
% Objective   :	get exploding bombs for a player
% Parameter 1 :	list of bombs of a player
% Parameter 2 :	list of bombs with coordinates that will explose	
explodingBombsOfPlayer([], []):- !.
explodingBombsOfPlayer([[X,Y,0,P|_]|T], [[X,Y,P]|Te]):- explodingBombsOfPlayer(T, Te).
explodingBombsOfPlayer([_|T], NewList):- explodingBombsOfPlayer(T, NewList), !.
explodingBombsOfPlayer([], []):- !.


% Function    :	deleteExplodedBombs
% Objective   :	
% Parameter 1 :	List of bombs
% Parameter 2 :	List of exploded bombs
% Parameter 3 :	New list of bombs
deleteExplodedBombs(NewBombs, [], NewBombs):- !.
deleteExplodedBombs(ListBombs, [Hb|Tb], NewBombs):-
	deleteExplodedBombs(ListBombs, Tb, TBombs),
	deleteBombOfPlayersFromCoordinates( TBombs, Hb, NewBombs), !.
deleteExplodedBombs(NewBombs, [], NewBombs):- !.


% Function    :	deleteBombOfPlayersFromCoordinates
% Objective   :	
% Parameter 1 : 
% Parameter 2 : 
% Parameter 3 :	
deleteBombOfPlayersFromCoordinates( [], _, []):- !.
deleteBombOfPlayersFromCoordinates( [H|T], Coordinates, [Hn|Tn]):-
	deleteBombOfPlayersFromCoordinates(T, Coordinates, Tn),
	deleteBombOfAPlayerFromCoordinates(H, Coordinates, Hn), !.
deleteBombOfPlayersFromCoordinates( [], _, []):- !.


% Function    :	deleteBombOfAPlayerFromCoordinates
% Objective   :	delete a bomb of a player in function of the coordinates
% Parameter 1 :	List of bombs
% Parameter 2 :	Coordinates
% Parameter 3 :	New list of bombs
deleteBombOfAPlayerFromCoordinates( [], _, []):- !.
deleteBombOfAPlayerFromCoordinates( [[X,Y|_]|T], [X,Y,_], Tn):-
	deleteBombOfAPlayerFromCoordinates(T, [X,Y,_], Tn), !.
deleteBombOfAPlayerFromCoordinates( [H|T], [X,Y,_], [H|Tn]):-
	deleteBombOfAPlayerFromCoordinates(T, [X,Y,_], Tn), !.
deleteBombOfAPlayerFromCoordinates( [], _, []):- !.


% Function    :	GetModel
% Objective   :	Return the get model of datas that will be use in the game
% Parameter 1 :	Board 
% Parameter 2 :	List of players
% Parameter 3 :	List of bombs
getModel( Board, ListOfPlayers, ListOfBombs):-
	board(Board),
	playersList( ListOfPlayers),
	bombsList( ListOfBombs).
	

% Function    :	SetModel
% Objective   :	Change the model values
% Parameter 1 :	New board
% Parameter 2 :	New list of players 
% Parameter 3 :	New list of bombs
setModel( NewBoard, NewListOfPlayers, NewListOfBombs):-
	retract( board(_)),
	retract( playersList(_)),
	retract( bombsList(_)),
	assert( board(NewBoard)),
	assert( playersList(NewListOfPlayers)),
	assert( bombsList(NewListOfBombs)).

% Function    :	implantBomb
% Objective   :	Implant Bomb
% Roturn      :	true -> Bomb implanted / false -> Bomb not implanted
% Parameter 1 :	Index of player which implant the bomb
implantBomb(PlayerIndex, Board, [X,Y,NbMaxBomb,PowerPlayer|T], ListAllBomb, NewListAllBomb):-
    	countTimeBomb(CountTimeBomb),
    	nth0(PlayerIndex, ListAllBomb, ListBombImplantByPlayer),
    	length(ListBombImplantByPlayer,Length),
	Length < NbMaxBomb,
	plantBomb(1,ListBombImplantByPlayer,X,Y, CountTimeBomb, PowerPlayer, PlayerIndex, ListAllBomb,NewListAllBomb), !.
implantBomb(PlayerIndex, Board, [X,Y,NbMaxBomb,PowerPlayer|T], ListAllBomb, NewListAllBomb):-
    	countTimeBomb(CountTimeBomb),
    	nth0(PlayerIndex, ListAllBomb, ListBombImplantByPlayer),
    	plantBomb(0,ListBombImplantByPlayer,X,Y, CountTimeBomb, PowerPlayer, PlayerIndex,ListAllBomb,NewListAllBomb), !.

%- planBomb
plantBomb(0,_,_,_,_,_,_,ListAllBomb,ListAllBomb):- !.
plantBomb(1,ListBombImplantByPlayer,X,Y, CountTimeBomb, PowerPlayer, PlayerIndex, ListAllBomb, NewListAllBomb):- 
   	append(ListBombImplantByPlayer, [[X,Y,CountTimeBomb,PowerPlayer]], NewListBombImplantByPlayer),
	updateList(PlayerIndex,NewListBombImplantByPlayer,ListAllBomb,NewListAllBomb), !.


% Function    : playersBeat
% Objective   : Instant T movement all players
% Parameter 1 : Index of player
% Parameter 2 : The list of player

playersBeat(NewBoard, [], NewBombs, NewBoard, [], NewBombs):- ! .                   
playersBeat(Board, Players, ListBombs, NewBoard, NewPlayers, NewListBombs):- 
	playersAction(Board, Players, ListBombs, ActionsPlayers),
	applyPlayersAction(Board, Players, ListBombs, ActionsPlayers, NewBoard, NewPlayers, NewListBombs), !.


% Function    :	applyPlayersAction
% Objective   :	
applyPlayersAction(NewBoard, NewPlayers, NewBombs, [], NewBoard, NewPlayers, NewBombs):- !.
applyPlayersAction(Board, Players, Bombs, [[IndexPlayer, Direction, IsBomb]|T], NewBoard, NewPlayers, NewBombs):-
	nth0(IndexPlayer, Players, Player),
	applyAction(IndexPlayer, Board, Player, Bombs, Direction, IsBomb, TPlayer, TBombs, TBoard),
	updateList(IndexPlayer, TPlayer, Players, TPlayers),
	applyPlayersAction(TBoard, TPlayers, TBombs, T, NewBoard, NewPlayers, NewBombs), !.
applyPlayersAction(NewBoard, NewPlayers, NewBombs, [], NewBoard, NewPlayers, NewBombs):- !.

% Function    :	playersAction
% Objective   :	
% Parameter 1 :	Board of the game
% Parameter 2 :	Players 
% Parameter 3 :	List of bombs
% Parameter 4 :	Actions of players ([ [IndexPlayer, Direciton, Bomb]..])
playersAction(Board, Players, Bombs, NewActions) :-
	length(Players, Length),
	NewLength is Length-1,
	playersTAction(NewLength, Board, Players, Bombs, NewActions), !.
%- players with index.
playersTAction(-1, _, _, _, []):- !.
playersTAction(IndexPlayer, Board, Players, Bombs, [Hn|Tn]):-
	playerAction(Board, Players, Bombs, IndexPlayer, Hn),
	NewIndexPlayer is IndexPlayer-1,
	playersTAction(NewIndexPlayer, Board, Players, Bombs, Tn), !.
playersTAction(-1, _, _, _, []):- !.


% Function    :	playerAction
% Objective   :	
% Parameter 1 :	Board  of the game
% Parameter 2 :	Players
% Parameter 3 :	List of bombs
% Parameter 4 :	Index of the player
% Parameter 5 :	Actions of player [IndexPlayer, Direction , Bomb]
playerAction(Board, Players, Bombs, IndexPlayer, [IndexPlayer, Direction, IsPlanting]):-
	nth0(IndexPlayer, Players, [_, _, _, _, _, Ia]),
	ia(Ia, IndexPlayer, Players, Board, Bombs, IsPlanting, Direction), !.


% Function    :	applyAction 
% Objective   :	apply the action to the player
% Parameter 1 :	Board
% Parameter 2 :	Player
% Parameter 3 :	Direction
% Parameter 4 :	Bomb
% Parameter 5 :	New Player
% Parameter 6 :	New list of bombs
% Parameter 7 : New board
applyAction(_, Board, Player, ListBombs, Direction, 0, NewPlayer, ListBombs, Board):-
	applyMove(Player, Direction, NewPlayer), !.
applyAction(IndexPlayer, Board, Player, ListBombs, Direction, 1, NewPlayer, NewListBombs, Board):-
	implantBomb(IndexPlayer, Board, Player, ListBombs, NewListBombs),
	applyMove(Player, Direction, NewPlayer), !.


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
            initFunctionSafePlace(X,Y,Board,ListBomb,N,DistanceLimit,IsSafe,IsMove) ->
            Safe = IsSafe,Move = IsMove;
          Safe = 0,Move = -1.

functionBackToSafePlace(X,Y,Board,ListBomb,N,DistanceLimit,Safe,Move):-
    DistanceLimit2 is DistanceLimit - 1, DistanceLimit2 >= 0,
    (   accessible(Board,X,Y), not(nth0(Index,N,[X,Y])))->
    append(N,[[X,Y]],N2),
    (   
  ( not(danger(X,Y,ListBomb)) , Safe = 1) ;
  ( Ydep is Y-1 , functionBackToSafePlace(X,Ydep,Board,ListBomb,N2,DistanceLimit2,Safe2,Move2) , ( Safe2 =:=1 ), Safe = 1  , Move = 0 );
  ( Xdep is X-1, functionBackToSafePlace(Xdep,Y,Board,ListBomb,N2,DistanceLimit2,Safe2,Move2) , ( Safe2 =:=1 ), Safe = 1   , Move = 3 );
  ( Ydep is Y+1, functionBackToSafePlace(X,Ydep,Board,ListBomb,N2,DistanceLimit2,Safe2,Move2) , ( Safe2 =:=1 ), Safe = 1   , Move = 2 );
  ( Xdep is X+1, functionBackToSafePlace(Xdep,Y,Board,ListBomb,N2,DistanceLimit2,Safe2,Move2) , ( Safe2 =:=1 ), Safe = 1   , Move = 1 )
    )
    ;   Safe = 0.

initFunctionSafePlace(X,Y,Board,ListBomb,N,DistanceLimit,Safe,Move):-
    %DistanceLimit2 is DistanceLimit - 1, DistanceLimit2 > 0,
    DistanceLimit > 0,
    (  not(nth0(Index,N,[X,Y])))->
    append(N,[[X,Y]],N2),
    (   
  ( not(danger(X,Y,ListBomb)) , Safe = 1, Move = -1) ;
  ( Ydep is Y-1 , functionBackToSafePlace(X,Ydep,Board,ListBomb,N2,DistanceLimit,Safe2,Move2) , ( Safe2 =:=1 ), Safe = 1  , Move = 0 );
  ( Xdep is X-1, functionBackToSafePlace(Xdep,Y,Board,ListBomb,N2,DistanceLimit,Safe2,Move2) , ( Safe2 =:=1 ), Safe = 1   , Move = 3 );
  ( Ydep is Y+1, functionBackToSafePlace(X,Ydep,Board,ListBomb,N2,DistanceLimit,Safe2,Move2) , ( Safe2 =:=1 ), Safe = 1   , Move = 2 );
  ( Xdep is X+1, functionBackToSafePlace(Xdep,Y,Board,ListBomb,N2,DistanceLimit,Safe2,Move2) , ( Safe2 =:=1 ), Safe = 1   , Move = 1 )
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

createMap(X):- X =[
          ['x','x','x','x','x','x','x','x','x'],
          ['x','_','_','o','o','o','_','_','x'],
          ['x','_','x','o','x','o','x','_','x'],
          ['x','o','o','o','o','o','o','o','x'],
          ['x','o','x','o','x','o','x','o','x'],
          ['x','o','o','o','o','o','o','o','x'],
          ['x','_','x','o','x','o','x','_','x'],
          ['x','_','_','o','o','o','_','_','x'],
          ['x','x','x','x','x','x','x','x','x']
          ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
