:-style_check(- (discontiguous)). 
max_body(6). 
max_vars(8). 
non_magic(4). 
head_pred(rhs,10). 
body_pred(lhs,10). 
% body_pred(child,2). 
% body_pred(incr_nat30_by,3). 
body_pred(incr_nat30,2). 
body_pred(color_change,2). 
body_pred(my_add,3). 
body_pred(my_geq,2). 
body_pred(my_leq,2). 
body_pred(my_mult,3). 
bounds(my_add,1,(0,29)). 
bounds(my_geq,1,(1,30)). 
bounds(my_leq,1,(1,30)). 
bounds(my_mult,1,(1,10)). 
% direction(color_change,(out,out)). 
% direction(incr_nat30,(out,out)). 
% direction(my_add,((in),(in),out)). 
% direction(my_geq,((in),out)). 
% direction(my_leq,((in),out)). 
% direction(my_mult,((in),out,(in))). 
type(my_add,(nat30,nat30,nat30)). 
type(my_mult,(nat30,nat30,nat30)). 
type(my_geq,(nat30,nat30)). 
type(my_leq,(nat30,nat30)). 
type(incr_nat30,(nat30,nat30)). 
type(color_change,(color,color)). 
% direction(rhs,((in),(in),(in),(in),(in),(in),(in),(in),(in),(in))). 
type(rhs,(state,nat30,nat30,color,nat30,nat30,rotation,nat900,shape,list)). 
% direction(lhs,(out,out,out,out,out,out,out,out,out,out)). 
type(lhs,(state,nat30,nat30,color,nat30,nat30,rotation,nat900,shape,list)). 
magic_type(color). 
magic_type(nat30). 
magic_value_type(color). 
magic_value_type(nat30). 
numerical_pred(my_add,3). 
numerical_pred(my_geq,2). 
numerical_pred(my_leq,2). 
numerical_pred(my_mult,3). 
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
pos(rhs(trn_0,2,2,cyan,3,3,sameR,8,'[[+,-,+],[|,_,|],[+,-,+]]',[])). 
pos(rhs(trn_0,4,4,cyan,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,3,4,cyan,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,2,4,cyan,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,4,3,cyan,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,3,3,red,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,2,3,cyan,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,4,2,cyan,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,3,2,cyan,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,2,2,cyan,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,1,1,red,5,5,sameR,16,'[[+,-,-,-,+],[|,_,_,_,|],[|,_,_,_,|],[|,_,_,_,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_0,8,1,black,1,7,sameR,7,'[[+,+],[|,|],[|,|],[|,|],[|,|],[|,|],[+,+]]',[])). 
pos(rhs(trn_1,2,2,cyan,3,3,sameR,8,'[[+,-,+],[|,_,|],[+,-,+]]',[])). 
pos(rhs(trn_1,4,4,red,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_1,2,2,yellow,5,5,sameR,24,'[[+,-,-,-,+],[|,*,~,*,|],[|,~,_,~,|],[|,*,~,*,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_1,1,1,red,7,7,sameR,24,'[[+,-,-,-,-,-,+],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[+,-,-,-,-,-,+]]',[])). 
pos(rhs(trn_1,3,3,bg,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_1,8,1,black,1,7,sameR,7,'[[+,+],[|,|],[|,|],[|,|],[|,|],[|,|],[+,+]]',[])). 
pos(rhs(trn_1,1,1,black,9,10,flipDHV,34,'[[+,-,-,-,-,-,-,-,-,+],[|,*,-,-,-,-,-,-,-,|],[|,|,_,_,_,_,_,_,_,|],[|,|,_,_,_,_,_,_,_,|],[|,|,_,_,_,_,_,_,_,|],[|,|,_,_,_,_,_,_,_,|],[|,|,_,_,_,_,_,_,_,|],[|,|,_,_,_,_,_,_,_,|],[|,|,_,_,_,_,_,_,_,|],[+,+,_,_,_,_,_,_,_,|]]',[])). 
pos(rhs(trn_2,2,2,cyan,3,3,sameR,8,'[[+,-,+],[|,_,|],[+,-,+]]',[])). 
pos(rhs(trn_2,11,5,red,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_2,4,13,red,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_2,8,2,green,7,7,sameR,48,'[[+,-,-,-,-,-,+],[|,~,~,~,~,~,|],[|,~,*,~,*,~,|],[|,~,~,_,~,~,|],[|,~,*,~,*,~,|],[|,~,~,~,~,~,|],[+,-,-,-,-,-,+]]',[])). 
pos(rhs(trn_2,7,1,red,9,9,sameR,32,'[[+,-,-,-,-,-,-,-,+],[|,_,_,_,_,_,_,_,|],[|,_,_,_,_,_,_,_,|],[|,_,_,_,_,_,_,_,|],[|,_,_,_,_,_,_,_,|],[|,_,_,_,_,_,_,_,|],[|,_,_,_,_,_,_,_,|],[|,_,_,_,_,_,_,_,|],[+,-,-,-,-,-,-,-,+]]',[])). 
pos(rhs(trn_2,3,12,cyan,3,3,sameR,8,'[[+,-,+],[|,_,|],[+,-,+]]',[])). 
pos(rhs(trn_2,2,11,red,5,5,sameR,16,'[[+,-,-,-,+],[|,_,_,_,|],[|,_,_,_,|],[|,_,_,_,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_2,3,3,bg,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_2,8,1,black,1,7,sameR,7,'[[+,+],[|,|],[|,|],[|,|],[|,|],[|,|],[+,+]]',[])). 
pos(rhs(trn_2,1,1,black,15,16,rot90,134,'[[-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,+],[_,_,_,_,_,|,~,~,~,~,~,~,~,~,~,|],[_,_,_,_,_,|,~,~,~,~,~,~,~,~,~,|],[_,_,_,_,_,|,~,~,~,~,~,~,~,~,~,|],[_,_,_,_,_,|,~,~,~,~,~,~,~,~,~,|],[_,_,_,_,_,|,-,-,-,-,-,-,-,-,-,|],[+,-,-,-,-,|,_,_,_,_,_,_,_,_,_,|],[|,~,~,~,~,|,_,_,_,_,_,_,_,_,_,|],[|,~,~,~,~,|,_,_,_,_,_,_,_,_,_,|],[|,~,~,~,~,|,_,_,_,_,_,_,_,_,_,|],[|,~,~,~,~,|,_,_,_,_,_,_,_,_,_,|],[|,~,~,~,~,|,_,_,_,_,_,_,_,_,_,|],[|,~,~,~,~,|,_,_,_,_,_,_,_,_,_,|],[|,~,~,~,~,|,_,_,_,_,_,_,_,_,_,|],[|,~,~,~,~,|,_,_,_,_,_,_,_,_,_,|],[+,-,-,-,-,-,-,-,-,-,-,-,-,-,-,+]]',[])). 
pos(rhs(trn_3,2,2,cyan,3,3,sameR,8,'[[+,-,+],[|,_,|],[+,-,+]]',[])). 
pos(rhs(trn_3,4,3,red,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_3,9,10,red,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_3,5,2,cyan,3,1,rot90,3,'[[-,-,-]]',[])). 
pos(rhs(trn_3,2,1,red,5,5,sameR,16,'[[+,-,-,-,+],[|,_,_,_,|],[|,_,_,_,|],[|,_,_,_,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_3,7,8,yellow,5,5,sameR,24,'[[+,-,-,-,+],[|,*,~,*,|],[|,~,_,~,|],[|,*,~,*,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_3,6,7,red,7,7,sameR,24,'[[+,-,-,-,-,-,+],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[|,_,_,_,_,_,|],[+,-,-,-,-,-,+]]',[])). 
pos(rhs(trn_3,3,3,cyan,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_3,8,1,black,1,7,sameR,7,'[[+,+],[|,|],[|,|],[|,|],[|,|],[|,|],[+,+]]',[])). 
pos(rhs(trn_3,1,1,black,7,13,sameR,52,'[[|,_,_,_,_,_,+,+],[|,_,_,_,_,_,|,|],[|,_,_,_,_,_,|,|],[|,_,_,_,_,_,|,|],[|,_,_,_,_,_,|,|],[|,-,-,-,-,-,-,|],[|,~,~,~,|,_,_,|],[|,~,~,~,|,_,_,|],[|,~,~,~,|,_,_,|],[|,~,~,~,|,_,_,|],[|,~,~,~,|,_,_,|],[|,~,~,~,|,_,_,|],[+,-,-,-,+,_,_,|]]',[])). 
pos(rhs(trn_3,9,1,black,6,13,flipH,50,'[[+,-,-,-,-,-,+],[|,~,~,~,~,~,|],[|,~,~,~,~,~,|],[|,~,~,~,~,~,|],[|,~,~,~,~,~,|],[|,*,-,-,-,-,|],[|,|,_,_,_,_,|],[|,|,_,_,_,_,|],[|,|,_,_,_,_,|],[|,|,_,_,_,_,|],[|,|,_,_,_,_,|],[|,|,_,_,_,_,|],[+,+,_,_,_,_,|]]',[])). 
