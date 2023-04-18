:-['induction/h_muarc_lilp'].

:-use_module(library(clpfd)).

:- style_check(- discontiguous ).
output(rhs/7).
background([color_change/2,move_up/4,move_down/4,lhs/7]). %canbe_int/1,
%propositional([4,0,3,5,6,7]).
propositional([]).

:- dynamic((color_change/2,lhs/7,move_up/4,move_down/4,rhs/7)).

%stuff(Nth,X):- canbe_int(Nth), propositional(List),nth1(Nth,List,X).
%move_right(P,Q,R,Q):- maplist(canbe_int,[P,Q,R]), size(S),#=(R , P+1), #<(P,S).
%move_left(P,Q,R,Q):- maplist(canbe_int,[P,Q,R]),#>(P,1), #=(R,P-1). 
%move_down(P,_,P,_):-!.
move_down(P,Q,P,R):- maplist(canbe_int,[P,Q,R]),size(S),R#=Q+1,Q#<S.
%move_up(P,_,P,_):-!.
move_up(P,Q,P,R):- maplist(canbe_int,[P,Q,R]),Q#>1,R#=Q-1.

%begin(model(trn_0)). 
lhs(trn_0,1,1,sameR,cyan,4,sid_22).
rhs(trn_0,1,2,sameR,red,4,sid_22). 
neg(rhs(trn_0,1,1,sameR,cyan,4,sid_22)). 
%end(model(trn_0)). 

%begin(model(trn_1)). 
lhs(trn_1,2,1,sameR,cyan,1,sid_11). 
rhs(trn_1,2,2,sameR,red,1,sid_11). 
neg(rhs(trn_1,2,1,sameR,cyan,1,sid_11)). 
%end(model(trn_1)). 

%begin(model(trn_2)). 
lhs(trn_2,2,2,sameR,cyan,3,sid_13). 
rhs(trn_2,2,3,sameR,red,3,sid_13). 
neg(rhs(trn_2,2,2,sameR,cyan,3,sid_13)).
%end(model(trn_2)). 




color_change(cyan,red).

rrhs(A,B,C,D,E,F,G):-lhs(A,B,B,D,H,F,G),color_change(H,E),lhs(_I,C,B,D,H,B,_J).
rrhs(A,B,B,C,D,E,F):-lhs(A,B,E,C,G,E,F),color_change(G,D).
rrhs(A,B,C,D,E,C,F):-lhs(A,B,B,D,G,C,F),color_change(G,E).

correct_rhs(A,BI,CI,D,EI,F,G):-lhs(A,B,C,D,E,F,G),color_change(E,EI),move_down(B,C,BI,CI).

size(30). 
at_left(1,_). 
at_top(_,1). 
at_bottem(_,P):- size(P). 
at_right(P,_):-size(P). 

canbe_int(P):- integer(P),!.
canbe_int(P):- \+ var(P),!,(P=inf;P=bot).
canbe_int(P):- (attvar(P)->true;freeze(P,\+ atom(P))).
%canbe_int(P):- \+ compound(P), \+ string(P), \+ atom(P). 


test_induces(
 [lhs(tst_0, 2, 1, rot90, cyan, 4, s15307279)],
 [
  lhs(_,_,_,_,_,_,_),
  lrn_rhs(_,_,_,_,_,_,_),
  correct_rhs(_,_,_,_,_,_,_),
  lrn_rhs(tst_0,2,2,rot90,red,4,s15307279)]).

:- lilp.



