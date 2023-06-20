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
lhs(trn_0,hv(4,4),rot270,red,hv(5,4),hv(4,5),16,s16297997). 
lhs(trn_0,hv(11,2),rot270,cyan,hv(5,4),hv(4,5),16,s16297997). 
lhs(trn_0,hv(17,8),rot270,yellow,hv(5,4),hv(4,5),16,s16297997). 
lhs(trn_1,hv(15,14),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_1,hv(5,7),sameR,cyan,hv(6,4),hv(6,4),16,s7114825). 
lhs(trn_1,hv(13,11),sameR,purple,hv(6,4),hv(6,4),16,s7114825). 
lhs(trn_1,hv(19,3),sameR,blue,hv(6,4),hv(6,4),16,s7114825). 
lhs(trn_1,hv(25,9),sameR,green,hv(6,4),hv(6,4),16,s7114825). 
lhs(trn_2,hv(3,6),sameR,blue,hv(4,4),hv(4,4),12,s6123374). 
lhs(trn_2,hv(8,9),sameR,yellow,hv(4,4),hv(4,4),12,s6123374). 
lhs(trn_2,hv(14,3),sameR,cyan,hv(4,4),hv(4,4),12,s6123374). 
lhs(trn_2,hv(18,8),sameR,red,hv(4,4),hv(4,4),12,s6123374). 
lhs(trn_2,hv(25,4),sameR,green,hv(4,4),hv(4,4),12,s6123374). 
/*
lhs(tst_0,hv(3,10),rot90,green,hv(5,5),hv(5,5),15,s1496004). 
lhs(tst_0,hv(10,13),rot90,blue,hv(5,5),hv(5,5),15,s1496004). 
lhs(tst_0,hv(15,3),rot90,yellow,hv(5,5),hv(5,5),15,s1496004). 
lhs(tst_0,hv(22,8),rot90,cyan,hv(5,5),hv(5,5),15,s1496004). 
*/
