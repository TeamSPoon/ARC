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
lhs(trn_0,hv(3,4),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_0,hv(7,9),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_0,hv(8,2),sameR,cyan,hv(3,3),hv(3,3),8,sid_323). 
lhs(trn_0,hv(2,3),sameR,cyan,hv(4,3),hv(4,3),10,s5461606). 
lhs(trn_0,hv(6,8),sameR,cyan,hv(4,3),hv(4,3),10,s5461606). 
lhs(trn_0,hv(9,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(7,2),flipV,cyan,hv(4,3),hv(4,3),8,s15633297). 
lhs(trn_1,hv(2,3),sameR,cyan,hv(5,3),hv(5,3),9,s16362323). 
lhs(trn_1,hv(4,7),sameR,cyan,hv(5,3),hv(5,3),9,s16362323). 
/*
lhs(tst_0,hv(2,2),rot90,cyan,hv(2,2),hv(2,2),3,s11261491). 
lhs(tst_0,hv(2,4),flipH,cyan,hv(3,2),hv(3,2),4,s10901743). 
lhs(tst_0,hv(4,7),flipV,cyan,hv(3,2),hv(3,2),4,s10901743). 
lhs(tst_0,hv(7,2),flipV,cyan,hv(3,2),hv(3,2),4,s10901743). 
lhs(tst_0,hv(4,9),flipH,cyan,hv(4,2),hv(4,2),5,s13353882). 
lhs(tst_0,hv(7,4),flipH,cyan,hv(4,2),hv(4,2),5,s13353882). 
*/