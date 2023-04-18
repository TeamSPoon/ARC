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
lhs(trn_0,hv(5,2),rot90,green,hv(2,1),hv(1,2),2,sid_12). 
lhs(trn_0,hv(4,4),sameR,blue,hv(2,1),hv(2,1),2,sid_12). 
lhs(trn_0,hv(3,3),rot90,red,hv(3,1),hv(1,3),3,sid_13). 
lhs(trn_0,hv(3,19),sameR,cyan,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_0,hv(3,13),rot180,cyan,hv(8,8),hv(8,8),32,s5515589). 
lhs(trn_0,hv(6,3),sameR,yellow,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_0,hv(6,5),sameR,yellow,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(5,4),rot90,green,hv(2,1),hv(1,2),2,sid_12). 
lhs(trn_1,hv(2,17),sameR,cyan,hv(3,3),hv(3,3),9,sid_33). 
lhs(trn_1,hv(2,11),sameR,cyan,hv(9,9),hv(9,9),45,s11309857). 
lhs(trn_1,hv(4,5),sameR,yellow,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(4,6),sameR,red,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(6,4),sameR,blue,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(6,6),sameR,yellow,hv(1,1),hv(1,1),1,sid_11). 
/*
lhs(tst_0,hv(23,7),rot90,yellow,hv(2,1),hv(1,2),2,sid_12). 
lhs(tst_0,hv(24,8),rot90,red,hv(2,1),hv(1,2),2,sid_12). 
lhs(tst_0,hv(3,4),sameR,cyan,hv(4,4),hv(4,4),16,s4884971). 
lhs(tst_0,hv(3,8),flipV,cyan,hv(12,8),hv(12,8),64,s6389904). 
lhs(tst_0,hv(22,9),sameR,green,hv(1,1),hv(1,1),1,sid_11). 
*/
