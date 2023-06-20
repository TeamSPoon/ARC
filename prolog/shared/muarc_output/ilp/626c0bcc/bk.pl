:-use_module(library(clpfd)). 
incr_nat30(P,Q):-Q#=P+1. 
color_change(_,_). 
my_geq(P,Q):-nonvar(P),nonvar(Q),!,P>=Q. 
my_leq(P,Q):-nonvar(P),nonvar(Q),!,P=<Q. 
my_add(P,Q,R):-nonvar(P),nonvar(Q),integer(P),integer(Q),R is P+Q. 
my_add(P,Q,R):-nonvar(P),nonvar(R),integer(P),integer(R),Q is R-P. 
my_add(P,Q,R):-nonvar(R),nonvar(Q),integer(Q),integer(R),P is R-Q. 
my_mult(P,Q,R):-nonvar(P),nonvar(Q),integer(P),integer(Q),R is P*Q. 
my_mult(P,Q,R):-nonvar(P),nonvar(R),integer(P),integer(R),\+P=0.0,\+P=0,Q is R/P. 
my_mult(P,Q,R):-nonvar(R),nonvar(Q),integer(Q),integer(R),\+P=0.0,\+P=0,P is R/Q. 
:-use_module(library(clpfd)). 
size(30). 
at_left(hv(1,_)). 
at_top(hv(_,1)). 
at_bottem(hv(_,P)):-size(P). 
at_right(hv(P,_)):-size(P). 
right(hv(P,Q),hv(R,Q)):-size(S),P#<S,R#=P+1. 
left(hv(P,Q),hv(R,Q)):-P#>1,R#=P-1. 
down(hv(P,Q),hv(P,R)):-size(S),Q#<S,R#=Q+1. 
up(hv(P,Q),hv(P,R)):-Q#>1,R#=Q-1. 
lhs(trn_0,hv(1,1),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_0,hv(1,1),flipD,cyan,hv(5,6),hv(6,5),17,s10866090). 
lhs(trn_0,hv(4,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(5,1),rot270,cyan,hv(4,2),hv(2,4),7,s5659926). 
lhs(trn_1,hv(3,1),flipH,black,hv(2,4),hv(2,4),6,s11680822). 
lhs(trn_1,hv(1,1),flipD,cyan,hv(6,4),hv(4,6),10,s15829814). 
lhs(trn_1,hv(1,1),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(1,1),rot180,cyan,hv(4,4),hv(4,4),13,s5612495). 
lhs(trn_2,hv(3,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
/*
lhs(tst_0,hv(1,1),flipV,cyan,hv(6,4),hv(6,4),16,s1281831). 
lhs(tst_0,hv(1,1),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(tst_0,hv(4,1),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(tst_0,hv(4,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
*/
