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
lhs(trn_0,hv(9,7),rot90,silver,hv(3,1),hv(1,3),3,sid_13). 
lhs(trn_0,hv(5,6),rot90,silver,hv(4,1),hv(1,4),4,sid_14). 
lhs(trn_0,hv(1,4),rot90,silver,hv(6,1),hv(1,6),6,s11156184). 
lhs(trn_0,hv(7,3),rot90,silver,hv(7,1),hv(1,7),7,s2476004). 
lhs(trn_0,hv(3,2),rot90,silver,hv(8,1),hv(1,8),8,s16658777). 
lhs(trn_1,hv(3,8),rot90,silver,hv(2,1),hv(1,2),2,sid_12). 
lhs(trn_1,hv(7,4),rot90,silver,hv(6,1),hv(1,6),6,s11156184). 
lhs(trn_1,hv(1,3),rot90,silver,hv(7,1),hv(1,7),7,s2476004). 
lhs(trn_1,hv(5,1),rot90,silver,hv(9,1),hv(1,9),9,s13578114). 
lhs(trn_1,hv(1,1),flipH,black,hv(4,9),hv(4,9),27,s9474680). 
lhs(trn_1,hv(6,1),flipH,black,hv(4,9),hv(4,9),30,s4370719). 
/*
lhs(tst_0,hv(6,5),rot90,silver,hv(5,1),hv(1,5),5,s1265795). 
lhs(tst_0,hv(4,3),rot90,silver,hv(7,1),hv(1,7),7,s2476004). 
lhs(tst_0,hv(8,2),rot90,silver,hv(8,1),hv(1,8),8,s16658777). 
lhs(tst_0,hv(2,9),sameR,silver,hv(1,1),hv(1,1),1,sid_11). 
*/
