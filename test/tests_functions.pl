%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author(s):	N.Haim && F.Chastel (in eXtreme Programming)    %
% Creation: 	21/09/2016                                 	%
% Version : 	v0.1                                           	%
% Description : Test the move functions that apply and verify	%
%		that the move is possible			%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

preparePlayersListApplyMove:-assert(main:playersList([[3,4,8,3],[3,6,10,2],[6,4,5,4]])).

test(applyMoveFirstIndex,
	[
	 setup(preparePlayersListApplyMove),
	 cleanup(retractall(main:playersList(_))),
	 true(R == [[5,6,8,3],[3,6,10,2],[6,4,5,4]])
	]
    ):- main:applyMove(0,5,6),main:playersList(R).

test(applyMoveLastIndex,
         [
          setup(preparePlayersListApplyMove),
          cleanup(retractall(main:playersList(_))),
          true(R == [[3,4,8,3],[3,6,10,2],[5,6,5,4]])
         ]
     ):- main:applyMove(2,5,6),main:playersList(R).

test(applyMoveNoMatterIndex,
         [
          setup(preparePlayersListApplyMove),
          cleanup(retractall(main:playersList(_))),
          true(R == [[3,4,8,3],[5,6,10,2],[6,4,5,4]])
         ]
     ):- main:applyMove(1,5,6),main:playersList(R).

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


