:-use_module(library(clpfd)). 

color_change(cyan,red).

incr_nat30(P,Q):-Q#=P+1. 
my_geq(P,Q):-nonvar(P),nonvar(Q),!,P>=Q. 
my_leq(P,Q):-nonvar(P),nonvar(Q),!,P=<Q. 
my_add(P,Q,R):-nonvar(P),nonvar(Q),integer(P),integer(Q),R is P+Q. 
my_add(P,Q,R):-nonvar(P),nonvar(R),integer(P),integer(R),Q is R-P. 
my_add(P,Q,R):-nonvar(R),nonvar(Q),integer(Q),integer(R),P is R-Q. 
my_mult(P,Q,R):-nonvar(P),nonvar(Q),integer(P),integer(Q),R is P*Q. 
my_mult(P,Q,R):-nonvar(P),nonvar(R),integer(P),integer(R),\+P=0.0,\+P=0,Q is R/P. 
my_mult(P,Q,R):-nonvar(R),nonvar(Q),integer(Q),integer(R),\+P=0.0,\+P=0,P is R/Q. 

size(30). 
at_left(hv(1,_)). 
at_top(hv(_,1)). 
at_bottem(hv(_,P)):-size(P). 
at_right(hv(P,_)):-size(P). 

move_right(hv(P,Q),hv(R,Q)):-size(S),P#<S,R#=P+1. 
move_left(hv(P,Q),hv(R,Q)):-P#>1,R#=P-1. 
move_down(hv(P,Q),hv(P,R)):-size(S),Q#<S,R#=Q+1. 
move_up(hv(P,Q),hv(P,R)):-Q#>1,R#=Q-1.

lhs(trn_0,hv(1,1),sameR,cyan,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_1,hv(2,1),sameR,cyan,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(2,2),sameR,cyan,hv(3,1),hv(3,1),3,sid_13). 

