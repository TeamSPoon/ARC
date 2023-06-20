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
lhs(trn_0,10,7,yellow,4,1,rot90,4,'[[-,-,-,-]]',[]). 
lhs(trn_0,1,8,blue,3,2,rot90,6,'[[+,-,+],[+,-,+]]',[]). 
lhs(trn_0,4,9,red,2,2,sameR,4,'[[+,+],[+,+]]',[]). 
lhs(trn_0,7,9,green,2,2,sameR,4,'[[+,+],[+,+]]',[]). 
lhs(trn_1,1,7,cyan,4,3,rot90,12,'[[+,-,-,+],[|,~,~,|],[+,-,-,+]]',[]). 
lhs(trn_1,5,9,orange,2,2,sameR,4,'[[+,+],[+,+]]',[]). 
lhs(trn_1,8,9,red,3,2,sameR,6,'[[+,-,+],[+,-,+]]',[]). 
lhs(trn_2,6,6,red,5,1,rot90,5,'[[-,-,-,-,-]]',[]). 
lhs(trn_2,1,9,yellow,4,2,sameR,8,'[[+,-,-,+],[+,-,-,+]]',[]). 
lhs(trn_2,8,8,green,3,3,sameR,9,'[[+,-,+],[|,~,|],[+,-,+]]',[]). 
