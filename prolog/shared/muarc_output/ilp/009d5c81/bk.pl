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
lhs(trn_0,5,2,cyan,9,8,sameR,30,'[[_,_,_,+,-,_,-,-,_],[_,_,_,_,|,-,|,_,_],[_,/,-,/,|,_,_,_,_],[-,-,_,|,_,_,|,-,_],[_,_,_,|,\\,-,|,_,_],[_,_,_,_,|,_,|,_,_],[_,_,+,-,-,_,-,-,+],[_,_,!,_,_,_,_,_,!]]',[]). 
lhs(trn_0,2,10,blue,3,3,sameR,5,'[[_,^,_],[<,0,>],[_,v,_]]',[]). 
lhs(trn_1,7,3,cyan,3,3,sameR,4,'[[_,^,_],[<,_,>],[_,v,_]]',[]). 
lhs(trn_1,5,3,cyan,3,2,rot270,5,'[[+,_,+],[+,-,+]]',[]). 
lhs(trn_1,10,3,cyan,3,2,rot90,5,'[[+,_,+],[+,-,+]]',[]). 
lhs(trn_1,4,12,blue,3,2,sameR,4,'[[_,^,_],[-,-,-]]',[]). 
lhs(trn_1,4,11,blue,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,6,11,blue,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,6,4,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,8,4,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,10,4,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,2,1,cyan,9,5,sameR,24,'[[_,_,|,_,_,_,|,_,_],[+,_,\\,_,-,_,\\,_,+],[|,-,0,-,0,-,0,-,|],[|,_,-,_,-,_,-,_,|],[|,_,_,_,|,_,_,_,|]]',[]). 
lhs(trn_2,8,10,blue,3,3,sameR,5,'[[_,^,_],[<,0,>],[_,v,_]]',[]). 
lhs(trn_2,5,1,black,2,3,rot270,5,'[[+,-,+],[|,_,|],[+,-,+]]',[]). 
lhs(trn_3,5,1,cyan,10,8,sameR,31,'[[_,_,_,-,\\,_,_,_,_,_],[_,_,_,_,|,-,+,_,_,_],[_,_,_,_,_,_,|,_,+,_],[_,/,-,-,\\,_,/,-,|,_],[-,/,_,_,|,-,-,_,\\,>],[_,_,_,|,/,_,_,_,|,_],[_,_,-,/,_,_,_,/,/,_],[_,_,_,_,_,-,-,/,_,_]]',[]). 
lhs(trn_3,5,11,blue,3,2,rot180,5,'[[+,_,+],[+,-,+]]',[]). 
lhs(trn_3,6,13,blue,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_3,6,12,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_4,8,5,cyan,4,3,sameR,4,'[[\\,_,_,_],[_,\\,\\,_],[_,_,_,\\]]',[]). 
lhs(trn_4,6,3,cyan,3,2,rot180,4,'[[_,^,_],[-,-,-]]',[]). 
lhs(trn_4,9,3,cyan,3,2,flipV,4,'[[-,\\,_],[_,\\,-]]',[]). 
lhs(trn_4,11,5,cyan,2,2,rot90,3,'[[_,!],[-,+]]',[]). 
lhs(trn_4,3,10,blue,3,2,sameR,4,'[[_,^,_],[-,-,-]]',[]). 
lhs(trn_4,9,2,cyan,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_4,3,9,blue,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_4,5,9,blue,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_4,9,5,black,1,2,rot90,2,'[[+,+],[+,+]]',[]). 
lhs(trn_4,9,3,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_4,8,4,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_4,11,6,black,1,1,sameR,1,'[[0]]',[]). 