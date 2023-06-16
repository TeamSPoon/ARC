incr_nat30(P,Q):-nonvar(P),Q is P+1. 
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
lhs(trn_0,4,4,black,3,3,sameR,9,'[[+,-,+],[|,~,|],[+,-,+]]',[]). 
lhs(trn_0,3,3,cyan,5,5,sameR,16,'[[+,-,-,-,+],[|,0,0,0,|],[|,0,0,0,|],[|,0,0,0,|],[+,-,-,-,+]]',[]). 
lhs(trn_1,6,5,black,1,3,sameR,3,'[[|],[|],[|]]',[]). 
lhs(trn_1,5,4,purple,5,3,rot90,12,'[[+,-,-,-,+],[|,0,0,0,|],[+,-,-,-,+]]',[]). 
lhs(trn_2,8,5,black,3,3,rot90,5,'[[!,0,0],[|,>,-],[!,0,0]]',[]). 
lhs(trn_2,7,4,green,5,5,sameR,16,'[[0,+,-,+,0],[0,|,0,|,0],[/,/,0,\\,\\],[|,0,0,0,|],[+,-,-,-,+]]',[]). 
lhs(trn_3,6,5,black,4,4,rot90,10,'[[0,0,0,!],[+,-,-,|],[|,-,-,+],[!,0,0,0]]',[]). 
lhs(trn_3,5,4,red,6,6,sameR,20,'[[+,-,-,-,+,0],[|,0,0,0,|,0],[\\,\\,0,0,|,0],[0,|,0,0,\\,\\],[0,|,0,0,0,|],[0,+,-,-,-,+]]',[]). 
lhs(trn_4,2,2,cyan,3,2,sameR,6,'[[+,-,+],[+,-,+]]',[]). 
