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
lhs(trn_0,3,3,red,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_0,1,1,red,5,5,sameR,16,'[[+,-,-,-,+],[|,_,_,_,|],[|,_,_,_,|],[|,_,_,_,|],[+,-,-,-,+]]',[]). 
lhs(trn_0,2,2,black,3,3,sameR,8,'[[+,-,-,+],[|,_,~,|],[+,-,-,+]]',[]). 
lhs(trn_1,4,4,red,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,1,1,red,7,7,sameR,24,'[[+,-,-,-,-,-,+],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[+,-,-,-,-,-,+]]',[]). 
lhs(trn_1,2,2,black,5,5,sameR,24,'[[+,-,-,-,-,+],[|,*,~,*,~,|],[|,~,_,~,~,|],[|,*,~,*,~,|],[+,-,-,-,-,+]]',[]). 
lhs(trn_2,11,5,red,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,4,13,red,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,7,1,red,9,9,sameR,32,'[[+,-,-,-,-,-,-,-,+],[|,_,_,_,_,_,_,_,|],[|,_,_,_,_,_,_,_,|],[|,_,_,_,_,_,_,_,|],[|,_,_,_,_,_,_,_,|],[|,_,_,_,_,_,_,_,|],[|,_,_,_,_,_,_,_,|],[|,_,_,_,_,_,_,_,|],[+,-,-,-,-,-,-,-,+]]',[]). 
lhs(trn_2,2,11,red,5,5,sameR,16,'[[+,-,-,-,+],[|,_,_,_,|],[|,_,_,_,|],[|,_,_,_,|],[+,-,-,-,+]]',[]). 
lhs(trn_2,8,2,black,7,7,sameR,48,'[[+,-,-,-,-,-,-,+],[|,~,~,~,~,~,~,|],[|,~,*,~,*,~,~,|],[|,~,~,_,~,~,~,|],[|,~,*,~,*,~,~,|],[|,~,~,~,~,~,~,|],[+,-,-,-,-,-,-,+]]',[]). 
lhs(trn_2,3,12,black,3,3,sameR,8,'[[+,-,-,+],[|,_,~,|],[+,-,-,+]]',[]). 
lhs(trn_3,4,3,red,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_3,9,10,red,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_3,2,1,red,5,5,sameR,16,'[[+,-,-,-,+],[|,_,_,_,|],[|,_,_,_,|],[|,_,_,_,|],[+,-,-,-,+]]',[]). 
lhs(trn_3,6,7,red,7,7,sameR,24,'[[+,-,-,-,-,-,+],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[+,-,-,-,-,-,+]]',[]). 
lhs(trn_3,3,2,black,3,3,sameR,8,'[[+,-,-,+],[|,_,~,|],[+,-,-,+]]',[]). 
lhs(trn_3,7,8,black,5,5,sameR,24,'[[+,-,-,-,-,+],[|,*,~,*,~,|],[|,~,_,~,~,|],[|,*,~,*,~,|],[+,-,-,-,-,+]]',[]). 
