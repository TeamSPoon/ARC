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
lhs(trn_0,hv(4,2),rot90,blue,hv(8,6),hv(6,8),22,s10646592). 
lhs(trn_1,hv(7,4),sameR,black,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_1,hv(7,7),sameR,black,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_1,hv(9,9),sameR,black,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_1,hv(4,2),rot90,blue,hv(9,7),hv(7,9),37,s12778084). 
lhs(trn_2,hv(7,7),sameR,black,hv(4,4),hv(4,4),9,s8522600). 
lhs(trn_2,hv(4,2),rot90,blue,hv(9,7),hv(7,9),31,s5627827). 
lhs(trn_2,hv(7,5),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(7,7),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
/*
lhs(tst_0,hv(6,1),sameR,black,hv(5,4),hv(5,4),15,s14796172). 
lhs(tst_0,hv(7,6),rot90,black,hv(5,4),hv(4,5),15,s10537774). 
lhs(tst_0,hv(1,1),flipH,black,hv(5,10),hv(5,10),39,s12773961). 
lhs(tst_0,hv(3,1),rot90,blue,hv(10,8),hv(8,10),31,s14123544). 
*/
