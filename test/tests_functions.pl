%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author(s):	N.Haim && F.Chastel (in eXtreme Programming)    	%
% Creation: 	21/09/2016                                 		%
% Version : 	v0.1                                           		%
% Description : Test all functions of swish.pl in core dir		%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Name of module
:-module(test_module,[]).

:- use_module('../core/swish').
:- use_module(library(plunit)).

%:- initialization run_tests.

%%%%%%%%%%%%%%%%    Verify that accessible is working properly
%
:- begin_tests(accessible).

sample_board(Board) :- main:wall(W), main:path(P), main:bomb(B),
	Board=[[W,W,W,W],[W,P,B,W],[W,P,W,W],[W,W,W,W]].

% access to exterior wall accessible must return false
test(accessExteriorWall1) :- sample_board(Board), not( main:accessible(Board,0,0) ).

test(accessExteriorWall2) :- sample_board(Board), not( main:accessible(Board,4,4) ).

test(accessExteriorWall3) :- sample_board(Board), not( main:accessible(Board,4,2) ).

% access to path accessible must return true
test(accessPath) :- sample_board(Board), main:accessible(Board,1,1).

test(accessBomb) :- sample_board(Board), not( main:accessible(Board,3,2) ).

:- end_tests(accessible).

%%%%%%%%%%%%%%%%    Verify that   move is working properly 
%
:- begin_tests(move).

%sample_board(Board) :- wall(W),path(P), Board=[[W,W,W],[W,P,W],[W,P,W],[W,W,W]].

test(up):- X = 4, Y = 4, main:move(0, X, Y, NewX, NewY),
		NewX =:= 4,
		NewY =:= 3.

test(down):- X = 4, Y = 4, main:move(2, X, Y, NewX, NewY),
		NewX =:= 4,
		NewY =:= 5.


test(right):- X = 4, Y = 4, main:move(1, X, Y, NewX, NewY),
		NewX =:= 5,
		NewY =:= 4.


test(left):- X = 4, Y = 4, main:move(3, X, Y, NewX, NewY),
		NewX =:= 3,
		NewY =:= 4.

:- end_tests(move).

%%%%%%%%%%%%%%%%     Verify that movements is working properly
%
:- begin_tests(movements).

% Deplacement One Case Right/Left/Down/Up must be true
test(deplacementOneCaseRight):- Xp=1, Xd=2, Y=3, main:movements(Xp,Y,Xd,Y).

test(deplacementOneCaseLeft):- Xp=2, Xd=1, Y=3, main:movements(Xp,Y,Xd,Y).

test(deplacementOneCaseDown):- X=3, Yp=2, Yd=3, main:movements(X,Yp,X,Yd).

test(deplacementOneCaseUp):- X=3, Yp=3, Yd=2, main:movements(X,Yp,X,Yd).


% Deplaceme two cases / diagonal must be false
test(deplacementTwoCasesRight):- Xp=1, Xd=3, Y=3, not(main:movements(Xp,Y,Xd,Y)).

test(deplacementTwoCasesLeft):- Xp=3, Xd=1, Y=3, not(main:movements(Xp,Y,Xd,Y)).

test(deplacementTwoCasesDown):- X=3, Yp=2, Yd=4, not(main:movements(X,Yp,X,Yd)).

test(deplacementTwoCasesUp):- X=3, Yp=4, Yd=2, not(main:movements(X,Yp,X,Yd)).

test(deplacementDiagonalUpRight):- Xp=4, Yp=4, Xd=5, Yd=3, not(main:movements(Xp,Yp,Xd,Yd)).

test(deplacementDiagonalUpLeft):- Xp=4, Yp=4, Xd=3, Yd=3, not(main:movements(Xp,Yp,Xd,Yd)).

test(deplacementDiagonalDownRight):- Xp=4, Yp=4, Xd=5, Yd=5, not(main:movements(Xp,Yp,Xd,Yd)).

test(deplacementDiagnoalDownLeft):- Xp=4, Yp=4, Xd=3, Yd=5, not(main:movements(Xp,Yp,Xd,Yd)).

:- end_tests(movements).


%%%%%%%%%%%%%%%%     Verify that updateList is working properly
%
:- begin_tests(updateList).

% updateFirstIndex, updateNoMatterIndex and updateNoMatterIndex must be true



test(updateFirstIndex):- Index=0, HoldList=[[2,4],[5,6],[3,4],[6,2]], Val=[5,1], main:updateList(Index,Val,HoldList,NewList),
	NewList == [[5,1],[5,6],[3,4],[6,2]].


test(updateLastIndex):- Index=3, HoldList=[[2,4],[5,6],[3,4],[6,2]], Val=[1,5], main:updateList(Index,Val,HoldList,NewList),
	NewList == [[2,4],[5,6],[3,4],[1,5]].

test(updateNoMatterIndex):- Index=2, HoldList=[[2,4],[5,6],[3,4],[6,2]], Val=[1,5], main:updateList(Index,Val,HoldList,NewList),
	NewList == [[2,4],[5,6],[1,5],[6,2]].

% updateOverFlowIndex must be false

test(updateOverFlowIndex):- Index=5, HoldList=[[2,4],[5,6],[3,4],[6,2]], Val=[1,5], updateList(Index,Val,HoldList,HoldList).


:- end_tests(updateList).

%%%%%%%%%%%%%%%%%	Verify that applyMove is working prolerly
%
:- begin_tests(applyMove).


test(applyMove1,
	[
	 true(R == [3,3,8,3])
	]
    ):- main:applyMove([3,4,8,3],0,R).

test(applyMove1,
         [
          true(R == [3,5,8,3])
         ]
     ):- main:applyMove([2,5,8,3],1,R).

test(applyMove3,
         [
          true(R == [2,6,8,3])
         ]
     ):- main:applyMove([2,5,8,3],2,R).

test(applyMove4,
         [
          true(R == [1,5,8,3])
         ]
     ):- main:applyMove([2,5,8,3],3,R).
:- end_tests(applyMove).

%%%%%%%%%%%%%%%%	Verify that attainable is working properly
%
:- begin_tests(attainable).

prepareBoardAttainable:- assert(main:board([['x','x','x','x'],['x','o','_','x'],['x','b','_','x'],['x','x','x','x']])).


% attainableBlockNonDestructible and attainableBlobkBomb must be non attaible
test(attainableBlockNonDestructible, 
	[
	 setup(prepareBoardAttainable),
	 cleanup(retractall(main:board(_)))
	]
	):- main:board(Board), not(main:attainable(Board,0,1)).


test(attainableBlockBomb, 
	[
	 setup(prepareBoardAttainable),
	 cleanup(retractall(main:board(_)))
	]
	):- main:board(Board), not(main:attainable(Board,1,2)).

% attainableBlockDestructible and attainableWay must be attainable
test(attainableBlockDestructible, 
	[
	 setup(prepareBoardAttainable),
	 cleanup(retractall(main:board(_)))
	]
	):- main:board(Board), main:attainable(Board,1,1).

test(attainableWay, 
	[
	 setup(prepareBoardAttainable),
	 cleanup(retractall(main:board(_)))
	]
	):- main:board(Board), main:attainable(Board,2,1).

:- end_tests(attainable).

:- begin_tests(destructible).

prepareBoardDestructible:- assert(main:board([['x','x','x','x'],['x','o','_','x'],['x','b','_','x'],['x','x','x','x']])).


% destructibleBlockNonDestructible, destructibleBlockBomb and destructibleWay must be non destructible
test(destructibleBlockNonDestructible,
	[
	 setup(prepareBoardDestructible),
	 cleanup(retract(main:board()))
	]
	):- main:board(Board), not(destructible(Board,0,0)).


test(destructibleBlockBomb,
	[
	 setup(prepareBoardDestructible),
	 cleanup(retract(main:board()))
	]
	):- main:board(Board), not(destructible(Board,1,2)).


test(destructibleWay,
	[
	 setup(prepareBoardDestructible),
	 cleanup(retract(main:board()))
	]
	):- main:board(Board), not(destructible(Board,2,1)).

% destructibleBlockDestructible must be destructible
test(destructibleBlockDestructible,
	[
	 setup(prepareBoardDestructible),
	 cleanup(retract(main:board()))
	]
	):- main:board(Board), destructible(Board,1,1).

:- end_tests(destructible).

:- begin_tests(distanceManhattan).


test(distanceManhattan1,
	[
	 true(R == [3,5,3])
	]
	):- main:distanceManhattan([[0,3],[0,1],[2,1]],3,3,R).

:- end_tests(distanceManhattan).

:- begin_tests(weighted).

prepareBoardWeighted1:- assert(main:board([['x','x','x','x'],['x','o','_','x'],['x','b','_','x'],['x','x','x','x']])).

test(weighed1,
	[
	 setup(prepareBoardWeighted1),
	 true(R == [3,5.1,3]),
	 cleanup(retractall(main:board))
	]):- main:board(Board), main:weighted(Board,[[0,3],[1,1],[2,1]], [3,5,3],R).

:- end_tests(weighted).


:- begin_tests(lineOfFire).

test(lineOfFireTooFar,
	[
	 setup(retractall(main:board(_)))
	]):- not(main:lineOfFire(3,4,3,6,1)).

test(lineOfFireGoodDistance,
	[
	 setup(retractall(main:board(_)))
	]):- main:lineOfFire(3,4,3,6,2).


test(lineOfFireGoodDistance2,
	[
	 setup(retractall(main:board(_)))
	]):- main:lineOfFire(3,4,3,6,3).

test(lineOfFireGoodDistance3,
	[
	 setup(retractall(main:board(_)))
	]):- main:lineOfFire(3,4,3,6,4).


test(lineOfFireNotSameColumn,
	[
	 setup(retractall(main:board(_)))
	]):- not(main:lineOfFire(4,4,3,6,5)).


test(lineOfFireNotSameLine,
	[
	 setup(retractall(main:board(_)))
	]):- not(main:lineOfFire(3,4,6,6,5)).


test(lineOfFireGoodDistance4,
	[
	 setup(retractall(main:board(_)))
	]):- main:lineOfFire(4,3,6,3,5).

test(lineOfFireTooFar2,
	[
	 setup(retractall(main:board(_)))
	]):- not(main:lineOfFire(4,3,6,3,1)).


:- end_tests(lineOfFire).

:- begin_tests(minList).

test(minList1,
	[
	 setup(retractall(main:board(_)))
	]):- main:minList([3,6,2,9,0],Index,Value), Index =:= 4, Value =:= 0. 

test(minList2,
	[
	 setup(retractall(main:board(_)))
	]):- main:minList([3,6,2,9,9],Index,Value), Index =:= 2, Value =:= 2. 


test(minList3,
	[
	 setup(retractall(main:board(_)))
	]):- main:minList([3,6,2,9,0],Index,Value), Index =:= 4, Value =:= 0. 

test(minList4,
	[
	 setup(retractall(main:board(_)))
	]):- main:minList([3,1,1,9,3],Index,Value), Index =:= 1, Value =:= 1.

test(minList5,
	[
	 setup(retractall(main:board(_)))
	]):- main:minList([0,6,2,9,1],Index,Value), Index =:= 0, Value =:= 0. 
 
:- end_tests(minList).

:- begin_tests(dangerPerBomb).

% must return true
test(dangerPerBomb1):- main:dangerPerBomb(1,3,[1,2,3,1]).

% must return false
test(dangerPerBomb2):- not(main:dangerPerBomb(1,3,[1,1,3,1])).


% must return true
test(dangerPerBomb1):- main:dangerPerBomb(1,3,[4,3,3,5]).

% must return false
test(dangerPerBomb1):- not(main:dangerPerBomb(1,3,[4,3,3,2])).

:- end_tests(dangerPerBomb).

:- begin_tests(dangerPerBombPlayer).

%must return true
test(dangerPerBombPlayer1):- main:dangerPerBombPlayer(1,3,[[1,2,3,1],[1,1,3,1]]).

%must return true
test(dangerPerBombPlayer2):- main:dangerPerBombPlayer(1,3,[[1,2,3,1],[4,3,3,5]]).

%must return true
test(dangerPerBombPlayer3):- main:dangerPerBombPlayer(1,3,[[4,3,3,2],[4,3,3,5]]).

%must return false
test(dangerPerBombPlayer4):- not(main:dangerPerBombPlayer(1,3,[[4,3,3,2],[1,1,3,1]])).

:- end_tests(dangerPerBombPlayer).

:- begin_tests(danger).

%must return true
test(danger1):- main:danger(1,3,[[[1,1,3,1],[4,3,3,2]],[[8,1,3,2],[1,2,3,1]]]).

%must return false
test(danger2):- not( main:danger(1,3,[[[1,1,3,1],[4,3,3,2]],[[8,1,3,2],[1,5,3,1]]]) ).

test(danger3):- main:danger(1,3,[[[1,3,3,1]]]).

:- end_tests(danger).

:- begin_tests(safeAndAttainable).

startTestsSafeAndAttainable:- retractall(main:bombsList(_)), retractall(main:board(_)), 
assert(main:bombsList([[[1,3,5,2],[3,5,6,5]],[[7,3,5,1]]])),
assert(main:board(
 [
          ['x','x','x','x','x','x','x','x','x'],
          ['x','_','_','_','_','_','_','_','x'],
          ['x','_','x','_','x','_','x','_','x'],
          ['x','b','_','_','_','_','_','b','x'],
          ['x','_','x','_','x','_','x','_','x'],
          ['x','_','_','b','_','_','_','_','x'],
          ['x','_','x','_','x','_','x','_','x'],
          ['x','_','_','_','_','_','_','_','x'],
          ['x','x','x','x','x','x','x','x','x']
         ]

)).

terminusTestsSafeAndAttainable:- retractall(main:bombsList(_)), retractall(main:board(_)).

%must be true
test(safeAndAttainable1,
	[
	 setup(startTestsSafeAndAttainable),
	 cleanup(terminusTestsSafeAndAttainable)
	 
	]
	):- main:safeAndAttainable(2,1).

%must be true
test(safeAndAttainable2,
	[
	 setup(startTestsSafeAndAttainable),
	 cleanup(terminusTestsSafeAndAttainable)
	 
	]
	):- main:safeAndAttainable(7,1).


%must be true
test(safeAndAttainable3,
	[
	 setup(startTestsSafeAndAttainable),
	 cleanup(terminusTestsSafeAndAttainable)
	 
	]
	):- main:safeAndAttainable(5,3).

%must be true
test(safeAndAttainable4,
	[
	 setup(startTestsSafeAndAttainable),
	 cleanup(terminusTestsSafeAndAttainable)
	 
	]
	):- main:safeAndAttainable(1,6).

%must be false
test(safeAndAttainable5,
	[
	 setup(startTestsSafeAndAttainable),
	 cleanup(terminusTestsSafeAndAttainable)
	 
	]
	):- not(main:safeAndAttainable(3,3)).

%must be false
test(safeAndAttainable6,
	[
	 setup(startTestsSafeAndAttainable),
	 cleanup(terminusTestsSafeAndAttainable)
	 
	]
	):- not(main:safeAndAttainable(3,8)).


%must be false
test(safeAndAttainable7,
	[
	 setup(startTestsSafeAndAttainable),
	 cleanup(terminusTestsSafeAndAttainable)
	 
	]
	):- not(main:safeAndAttainable(2,2)).

:- end_tests(safeAndAttainable).

:- begin_tests(backToSafePlace).


test(back1):- main:initFunctionSafePlace(3,1,
                       [
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','_','_','b','_','_','_','_','x'],
                         ['x','_','x','o','x','o','x','_','x'],
                         ['x','_','o','o','o','o','o','_','x'],
                         ['x','_','x','o','x','o','x','_','x'],
                         ['x','_','o','o','o','o','o','_','x'],
                         ['x','_','x','o','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']                       ],[[[3,1,5,2]]],[],3,Safe,Move), Safe =:= 1 , Move =:= 3.

test(back2):-
	main:initFunctionSafePlace(3,1,
                      [
                      	['x','x','x','x','x','x','x','x','x'],
	        	['x','_','x','b','_','_','_','_','x'],
   	    		['x','_','x','_','x','o','x','_','x'],
   	    		['x','_','o','o','o','o','o','_','x'],
   	     		['x','_','x','o','x','o','x','_','x'],
   	       		['x','_','o','o','o','o','o','_','x'],
   	        	['x','_','x','o','x','o','x','_','x'],
   	       		['x','_','_','_','_','_','_','_','x'],
   			['x','x','x','x','x','x','x','x','x']
                      ],[[[3,1,5,2]]],[],3,Safe,Move), Safe =:= 1 , Move =:= 1.

test(back3):- main:initFunctionSafePlace(1,3,
                      [
                      	['x','x','x','x','x','x','x','x','x'],
	        	['x','b','x','_','_','_','_','_','x'],
   	    		['x','_','x','_','x','o','x','_','x'],
   	    		['x','_','_','_','b','o','o','_','x'],
   	     		['x','_','x','o','x','o','x','_','x'],
   	       		['x','_','o','o','o','o','o','_','x'],
   	        	['x','_','x','o','x','o','x','_','x'],
   	       		['x','_','b','_','_','_','_','_','x'],
   			['x','x','x','x','x','x','x','x','x']
                      ],[[[1,1,5,7],[4,3,5,2],[2,7,5,3]]],[],3,Safe,Move), Safe =:= 1 , Move =:= 1.

test(back4):- main:initFunctionSafePlace(2,3,
                      [
                      	['x','x','x','x','x','x','x','x','x'],
	        	['x','b','x','_','_','_','_','_','x'],
   	    		['x','_','x','_','x','o','x','_','x'],
   	    		['x','_','_','_','b','o','o','_','x'],
   	     		['x','_','x','o','x','o','x','_','x'],
   	       		['x','_','o','o','o','o','o','_','x'],
   	        	['x','_','x','o','x','o','x','_','x'],
   	       		['x','_','b','_','_','_','_','_','x'],
   			['x','x','x','x','x','x','x','x','x']
                      ],[[[1,1,5,7],[4,3,5,1],[2,7,5,3]]],[],3,Safe,Move), Safe =:= 1, Move =:= -1.


test(back5):- main:initFunctionSafePlace(1,3,
                      [
                      	['x','x','x','x','x','x','x','x','x'],
	        	['x','b','x','_','_','_','_','_','x'],
   	    		['x','_','x','_','x','o','x','_','x'],
   	    		['x','_','_','b','_','o','o','_','x'],
   	     		['x','_','x','o','x','o','x','_','x'],
   	       		['x','_','o','o','o','o','o','_','x'],
   	        	['x','_','x','o','x','o','x','_','x'],
   	       		['x','_','b','_','_','_','_','_','x'],
   			['x','x','x','x','x','x','x','x','x']
                      ],[[[1,1,5,7],[3,3,5,4],[2,7,5,4]]],[],3,Safe,Move), Safe = 0, Move = -1.

test(back6):- main:initFunctionSafePlace(7,2,
                      [
                      	['x','x','x','x','x','x','x','x','x'],
	        	['x','b','x','_','_','_','_','_','x'],
   	    		['x','_','x','_','x','o','x','_','x'],
   	    		['x','_','_','b','_','o','o','_','x'],
   	     		['x','_','x','o','x','o','x','_','x'],
   	       		['x','_','o','o','o','o','o','_','x'],
   	        	['x','_','x','o','x','o','x','_','x'],
   	       		['x','_','b','_','_','_','_','_','x'],
   			['x','x','x','x','x','x','x','x','x']
                      ],[[[7,3,2,2],[7,1,5,1],[1,1,5,7],[3,3,5,4],[2,7,5,4]]],[],3,Safe,Move), Safe = 0, Move = -1.


test(back7):- main:initFunctionSafePlace(5,1,
                       [
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','_','_','_','_','_','b','_','x'],
                         ['x','_','x','o','x','o','x','_','x'],
                         ['x','_','o','o','o','o','o','_','x'],
                         ['x','_','x','o','x','o','x','_','x'],
                         ['x','_','o','o','o','o','o','_','x'],
                         ['x','_','x','o','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']                       ],[[[6,1,5,2]]],[],3,Safe,Move), Safe =:= 0 , Move =:= -1.

test(back8):- main:backToSafePlace(1,2,
                      [
                      	['x','x','x','x','x','x','x','x','x'],
	        	['x','_','_','_','_','_','_','_','x'],
   	    		['x','_','x','_','x','o','x','_','x'],
   	    		['x','_','_','_','b','o','o','_','x'],
   	     		['x','_','x','o','x','o','x','_','x'],
   	       		['x','_','o','o','o','o','o','_','x'],
   	        	['x','_','x','o','x','o','x','_','x'],
   	       		['x','_','b','_','_','_','_','_','x'],
   			['x','x','x','x','x','x','x','x','x']
			],[[[1,3,5,3]]],[],3,Safe,Move), Safe =:= 1 , Move =:= 0.

:- end_tests(backToSafePlace).


:- begin_tests(rapprochement).

test(rapprochement1):- main:rapprochement(3,4,[[1,3|_],[3,4|_],[5,2|_]],List), List == [0.8125,0.75],!.

test(rapprochement2):- main:rapprochement(1,3,[[1,3|_],[3,4|_],[5,2|_]],List), List == [0.8125,0.6875],!.

test(rapprochement3):- main:rapprochement(5,2,[[1,3|_],[3,4|_],[5,2|_]],List), List == [0.6875,0.75],!.

:- end_tests(rapprochement).

:- begin_tests(dangerWeight).

test(dangerWeight1):- main:dangerWeight(1,2,[[[1,3,5,2],[3,5,6,5]],[[7,3,5,1]]],0).

test(dangerWeight2):- main:dangerWeight(3,11,[[[1,3,5,2],[3,6,6,5]],[[7,3,5,1]]],-100).

test(dangerWeight3):- main:dangerWeight(3,10,[[[1,3,5,2],[3,5,6,5]],[[7,3,5,1]]],0).

:- end_tests(dangerWeight).

:- begin_tests(nbChoiceAvailable).

test(nbChoiceAvailable1):- main:nbChoiceAvailable(3,3,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','o','o','_','x'],
                         ['x','_','x','o','x','o','x','_','x'],
                         ['x','_','o','o','o','o','o','_','x'],
                         ['x','_','x','o','x','o','x','_','x'],
                         ['x','_','b','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
                       ],R), R =:= 3.

test(nbChoiceAvailable2):- main:nbChoiceAvailable(1,7,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','o','o','_','x'],
                         ['x','_','x','o','x','o','x','_','x'],
                         ['x','_','o','o','o','o','o','_','x'],
                         ['x','_','x','o','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 2.

test(nbChoiceAvailable3):- main:nbChoiceAvailable(3,5,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','o','o','_','x'],
                         ['x','_','x','o','x','o','x','_','x'],
                         ['x','_','o','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 1.

test(nbChoiceAvailable4):- main:nbChoiceAvailable(3,5,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 3.


test(nbChoiceAvailable5):- main:nbChoiceAvailable(3,3,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 4.


test(nbChoiceAvailable6):- main:nbChoiceAvailable(4,3,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 0.

:- end_tests(nbChoiceAvailable).


:- begin_tests(availableWeight).

test(availableWeight1):- main:availableWeight(3,4,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 1.

test(availableWeight2):- main:availableWeight(3,3,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 0.

test(availableWeight3):- main:availableWeight(1,1,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 0.


test(availableWeight4):- main:availableWeight(6,3,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 0.

test(availableWeight5):- main:availableWeight(2,5,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 1.

:- end_tests(availableWeight).

:- begin_tests(bonusWeight).

test(bonusWeight1):- main:bonusWeight(1,1,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 0.

test(bonusWeight2):- main:bonusWeight(0,0,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 0.

test(bonusWeight3):- main:bonusWeight(3,4,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 1.

test(bonusWeight4):- main:bonusWeight(2,5,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 1.


test(bonusWeight5):- main:bonusWeight(6,3,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 0.

:- end_tests(bonusWeight).

:- begin_tests(isWall).


test(isWall1):- main:isWall(0,0,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 0.


test(isWall2):- main:isWall(1,8,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 0.


test(isWall3):- main:isWall(1,1,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 1.

test(isWall4):- main:isWall(2,3,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 1.

test(isWall5):- main:isWall(2,5,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 1.


test(isWall6):- main:isWall(4,5,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 1.

test(isWall7):- main:isWall(3,3,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 1.

test(isWall8):- main:isWall(3,4,[
                         ['x','x','x','x','x','x','x','x','x'],
                         ['x','b','x','_','_','_','_','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','b','_','o','o','_','x'],
                         ['x','_','x','p','x','o','x','_','x'],
                         ['x','_','c','_','o','o','o','_','x'],
                         ['x','_','x','_','x','o','x','_','x'],
                         ['x','_','_','_','_','_','_','_','x'],
                         ['x','x','x','x','x','x','x','x','x']
			],R), R =:= 1.



:- end_tests(isWall).

:- begin_tests(isBomb2).

test(isBomb2_1):- main:isBomb2(3,4,[[3,4,5,6],[1,4,6,2],[6,2,4,1]],R), R=:= 1.

test(isBomb2_2):- main:isBomb2(3,4,[[1,4,6,5],[3,4,1,3],[6,4,1,3]],R), R =:= 1.

test(isBomb2_3):- main:isBomb2(3,4,[[1,4,6,5],[2,6,1,3],[3,4,1,3]], R), R =:= 1.

test(isBomb2_4):- main:isBomb2(3,4,[[1,4,6,5],[2,1,1,3],[6,4,1,3]],R), R=:=0.

test(isBomb_1):- main:isBomb(3,4,[[[3,4,5,6],[1,4,6,2],[6,2,4,1]],[[3,3,5,6],[9,3,1,5]]],R), R=:= 1.

test(isBomb_2):- main:isBomb(3,4,[[[1,4,5,6],[1,4,6,2],[6,2,4,1]],[[3,3,5,6],[9,3,1,5]]],R), R=:= 0.

test(isBomb_3):- main:isBomb(3,4,[[[3,5,5,6],[1,4,6,2],[3,4,4,1]],[[3,3,5,6],[9,3,1,5]]],R), R=:= 1.

test(isBomb_4):- main:isBomb(3,4,[[[3,5,5,6],[1,4,6,2],[2,4,4,1]],[[3,4,5,6],[9,3,1,5]]],R), R=:= 1.

test(isBomb_5):- main:isBomb(3,4,[[[3,5,5,6],[1,4,6,2],[2,4,4,1]],[[3,3,5,6],[3,4,1,5]]],R), R=:= 1.

test(isBomb_6):- main:isBomb(3,4,[[[3,5,5,6],[3,4,6,2],[2,4,4,1]],[[3,3,5,6],[9,3,1,5]]],R), R=:= 1.


:- end_tests(isBomb2).

%%%%%%%%%%%%%%%%%       Verify that lineExplode is working properly
%
%:- begin_tests(lineExplode).

%- common use case with an explosion going to the up, killing nobody but destroying a destructible block
%test(lineExplodeNormalUseCase) :- InitBoard = [['x','x','x','x'],[,'x','o','o','x'],['x','o','o','x'],['x','_','_','x'],['x','b','_','x'],['x','x','x','x']], 
%			InitPlayers = [[3, 4, 1, 1, 0]], Direction is 0, Xb is 3, Yb is 1, Eb is 2, 
%			NewBoard = [['x','x','x','x'],[,'x','o','o','x'],['x','_','o','x'],['x','_','_','x'],['x','b','_','x'],['x','x','x','x']], NewPlayers = [[3, 4, 1, 1, 0]],
%			main:lineExplode(InitBoard, Xb, Yb, Eb, InitPlayers, NewPlayers, NewBoard, NewPlayers).

%- common use case with an explosion going to the right and a wall that stop the explosion
%test(lineExplodeNormalUseCase) :- InitBoard = [['x','x','x','x'],[,'x','o','o','x'],['x','o','o','x'],['x','_','_','x'],['x','b','_','x'],['x','x','x','x']], 
 %                       InitPlayers = [[3, 4, 1, 1, 0]], Direction is 0, Xb is 3, Yb is 1, Eb is 2, 
  %                      NewBoard = [['x','x','x','x'],[,'x','o','o','x'],['x','_','o','x'],['x','_','_','x'],['x','b','_','x'],['x','x','x','x']], NewPlayers = [[3, 4, 1, 1, 0]],
  %                      main:lineExplode(InitBoard, Xb, Yb, Eb, InitPlayers, NewPlayers, NewBoard, NewPlayers).

%:- end_tests(lineExplode).
