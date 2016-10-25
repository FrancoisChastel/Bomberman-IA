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



test(updateFirstIndex):- Index=0, HoldList=[[2,4],[5,6],[3,4],[6,2]], NewX=1, NewY=5, main:updateList(Index,HoldList,NewList,NewX,NewY),
	NewList == [[1,5],[5,6],[3,4],[6,2]].


test(updateLastIndex):- Index=3, HoldList=[[2,4],[5,6],[3,4],[6,2]], NewX=1, NewY=5, main:updateList(Index,HoldList,NewList,NewX,NewY),
	NewList == [[2,4],[5,6],[3,4],[1,5]].

test(updateNoMatterIndex):- Index=2, HoldList=[[2,4],[5,6],[3,4],[6,2]], NewX=1, NewY=5, main:updateList(Index,HoldList,NewList,NewX,NewY),
	NewList == [[2,4],[5,6],[1,5],[6,2]].

% updateOverFlowIndex must be false

test(updateOverFlowIndex):- Index=5, HoldList=[[2,4],[5,6],[3,4],[6,2]], NewX=1, NewY=5, not(main:updateList(Index,HoldList,_,NewX,NewY)).


:- end_tests(updateList).
