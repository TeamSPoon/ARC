:-use_module(library(aleph)). 
:-if(current_predicate(use_rendering/1)). 
:-use_rendering(prolog). 
:-endif. 
:-aleph. 
:-style_check(- (discontiguous)). 
:-aleph_set(verbosity,1). 
:-aleph_set(interactive,false). 
:-aleph_set(i,4). 
:-aleph_set(nodes,10000). 
:-modeh(*,rhs(+state,+nat30,+nat30,+color,+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
:-modeb(*,lhs(+state,+nat30,+nat30,#(color),+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
:-modeb(*,my_geq(+nat30,#(nat30))). 
:-modeb(*,my_leq(+nat30,#(nat30))). 
:-modeb(*,my_add(+nat30,+nat30,-nat30)). 
:-modeb(*,my_mult(+nat30,#(nat30),-nat30)). 
:-lazy_evaluate(my_add/3). 
:-lazy_evaluate(my_geq/2). 
:-lazy_evaluate(my_leq/2). 
:-lazy_evaluate(my_mult/3). 
:-determination(rhs/10,lhs/10). 
:-determination(rhs/10,color_change/2). 
:-determination(rhs/10,incr_nat30/2). 
:-determination(rhs/10,my_geq/2). 
:-determination(rhs/10,my_leq/2). 
:-determination(rhs/10,my_add/3). 
:-determination(rhs/10,my_mult/3). 
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
:-begin_bg. 
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
lhs(trn_0,9,2,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_0,3,3,cyan,3,3,sameR,8,'[[+,-,+],[|,0,|],[+,-,+]]',[]). 
lhs(trn_0,8,1,cyan,4,3,sameR,10,'[[+,-,-,+],[|,0,0,|],[+,-,-,+]]',[]). 
lhs(trn_0,7,5,cyan,5,3,rot90,13,'[[+,-,-,-,+],[|,0,|,0,|],[+,-,-,-,+]]',[]). 
lhs(trn_0,4,4,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_0,8,6,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_0,8,8,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,4,5,black,1,2,sameR,2,'[[|],[|]]',[]). 
lhs(trn_1,2,12,black,1,3,rot90,3,'[[|],[|],[|]]',[]). 
lhs(trn_1,8,11,black,2,2,sameR,4,'[[+,+],[+,+]]',[]). 
lhs(trn_1,9,5,cyan,3,3,sameR,8,'[[+,-,+],[|,0,|],[+,-,+]]',[]). 
lhs(trn_1,7,10,cyan,4,4,sameR,12,'[[+,-,-,+],[|,0,0,|],[|,0,0,|],[+,-,-,+]]',[]). 
lhs(trn_1,1,9,cyan,5,5,rot270,17,'[[0,0,+,-,+],[0,0,|,0,|],[+,-,|,0,|],[|,0,|,0,|],[+,-,-,-,+]]',[]). 
lhs(trn_1,3,2,cyan,6,5,flipDHV,20,'[[0,0,0,+,-,+],[0,0,0,|,0,|],[+,-,<,0,-,|],[|,0,0,|,0,|],[+,-,-,-,-,+]]',[]). 
lhs(trn_1,2,10,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,4,3,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,6,3,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,10,6,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,4,3,black,1,3,rot90,3,'[[|],[|],[|]]',[]). 
lhs(trn_2,7,9,black,1,3,sameR,3,'[[|],[|],[|]]',[]). 
lhs(trn_2,11,11,cyan,3,3,sameR,8,'[[+,-,+],[|,0,|],[+,-,+]]',[]). 
lhs(trn_2,3,2,cyan,5,3,sameR,12,'[[+,-,-,-,+],[|,0,0,0,|],[+,-,-,-,+]]',[]). 
lhs(trn_2,10,1,cyan,7,3,rot90,18,'[[+,-,-,-,-,-,+],[|,0,|,0,|,0,|],[+,-,-,-,-,-,+]]',[]). 
lhs(trn_2,4,8,cyan,5,5,sameR,17,'[[0,0,/,-,+],[+,-,|,0,|],[|,0,|,0,|],[+,-,|,0,|],[0,0,\\,-,+]]',[]). 
lhs(trn_2,5,10,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,11,2,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,11,4,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,11,6,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,12,12,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_3,3,8,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_3,3,10,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_3,8,3,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_3,10,13,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_3,11,8,black,2,2,sameR,4,'[[+,+],[+,+]]',[]). 
lhs(trn_3,3,13,cyan,3,3,sameR,8,'[[+,-,+],[|,0,|],[+,-,+]]',[]). 
lhs(trn_3,2,7,cyan,5,4,rot90,16,'[[+,-,-,-,+],[|,0,v,0,|],[|,0,^,0,|],[+,-,-,-,+]]',[]). 
lhs(trn_3,3,1,cyan,10,4,sameR,26,'[[+,-,\\,0,0,0,0,0,0,0],[|,0,|,-,-,-,-,-,-,+],[+,-,|,0,|,0,0,|,0,|],[0,0,\\,-,-,-,-,-,-,+]]',[]). 
lhs(trn_3,9,7,cyan,8,5,rot270,25,'[[+,-,-,+,0,0,0,0],[|,0,0,|,0,/,-,+],[|,0,0,|,-,|,0,|],[+,-,<,|,0,|,0,|],[0,0,0,\\,-,-,-,+]]',[]). 
lhs(trn_3,4,2,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_3,4,14,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_3,6,3,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_3,10,11,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_3,11,3,black,1,1,sameR,1,'[[0]]',[]). 
:-end_bg. 
:-begin_in_pos. 
pos(rhs(trn_0,9,2,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_0,3,3,blue,3,3,sameR,8,'[[+,-,+],[|,0,|],[+,-,+]]',[])). 
pos(rhs(trn_0,8,1,blue,4,3,sameR,10,'[[+,-,-,+],[|,0,0,|],[+,-,-,+]]',[])). 
pos(rhs(trn_0,8,1,black,3,4,rot90,10,'[[+,-,+],[|,0,|],[|,0,|],[+,-,+]]',[])). 
pos(rhs(trn_0,7,5,green,5,3,rot90,13,'[[+,-,-,-,+],[|,0,|,0,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_0,7,5,black,3,5,sameR,13,'[[+,-,+],[|,0,|],[|,-,|],[|,0,|],[+,-,+]]',[])). 
pos(rhs(trn_0,4,4,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,8,6,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,8,8,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_1,4,5,black,1,2,sameR,2,'[[|],[|]]',[])). 
pos(rhs(trn_1,2,12,black,1,3,rot90,3,'[[|],[|],[|]]',[])). 
pos(rhs(trn_1,8,11,black,2,2,sameR,4,'[[+,+],[+,+]]',[])). 
pos(rhs(trn_1,9,5,blue,3,3,sameR,8,'[[+,-,+],[|,0,|],[+,-,+]]',[])). 
pos(rhs(trn_1,7,10,blue,4,4,sameR,12,'[[+,-,-,+],[|,0,0,|],[|,0,0,|],[+,-,-,+]]',[])). 
pos(rhs(trn_1,1,9,green,5,5,rot270,17,'[[0,0,+,-,+],[0,0,|,0,|],[+,-,|,0,|],[|,0,|,0,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_1,1,9,black,5,5,rot90,17,'[[+,-,-,-,+],[|,0,|,0,|],[|,0,|,-,+],[|,0,|,0,0],[+,-,+,0,0]]',[])). 
pos(rhs(trn_1,3,2,red,6,5,flipDHV,20,'[[0,0,0,+,-,+],[0,0,0,|,0,|],[+,-,<,0,-,|],[|,0,0,|,0,|],[+,-,-,-,-,+]]',[])). 
pos(rhs(trn_1,3,2,black,5,6,sameR,20,'[[+,-,-,-,+],[|,0,|,0,|],[|,-,0,-,+],[|,0,v,0,0],[|,0,|,0,0],[+,-,+,0,0]]',[])). 
pos(rhs(trn_1,2,10,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_1,4,3,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_1,6,3,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_1,10,6,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_2,4,3,black,1,3,rot90,3,'[[|],[|],[|]]',[])). 
pos(rhs(trn_2,7,9,black,1,3,sameR,3,'[[|],[|],[|]]',[])). 
pos(rhs(trn_2,11,11,blue,3,3,sameR,8,'[[+,-,+],[|,0,|],[+,-,+]]',[])). 
pos(rhs(trn_2,3,2,blue,5,3,sameR,12,'[[+,-,-,-,+],[|,0,0,0,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_2,3,2,black,3,5,rot90,12,'[[+,-,+],[|,0,|],[|,0,|],[|,0,|],[+,-,+]]',[])). 
pos(rhs(trn_2,10,1,red,7,3,rot90,18,'[[+,-,-,-,-,-,+],[|,0,|,0,|,0,|],[+,-,-,-,-,-,+]]',[])). 
pos(rhs(trn_2,10,1,black,3,7,sameR,18,'[[+,-,+],[|,0,|],[|,-,|],[|,0,|],[|,-,|],[|,0,|],[+,-,+]]',[])). 
pos(rhs(trn_2,4,8,green,5,5,sameR,17,'[[0,0,/,-,+],[+,-,|,0,|],[|,0,|,0,|],[+,-,|,0,|],[0,0,\\,-,+]]',[])). 
pos(rhs(trn_2,4,8,black,5,5,rot270,17,'[[+,-,-,-,+],[|,0,0,0,|],[\\,-,-,-,/],[0,|,0,|,0],[0,+,-,+,0]]',[])). 
pos(rhs(trn_2,5,10,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_2,11,2,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_2,11,4,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_2,11,6,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_2,12,12,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_3,3,8,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_3,3,10,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_3,8,3,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_3,10,13,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_3,11,8,black,2,2,sameR,4,'[[+,+],[+,+]]',[])). 
pos(rhs(trn_3,3,13,blue,3,3,sameR,8,'[[+,-,+],[|,0,|],[+,-,+]]',[])). 
pos(rhs(trn_3,2,7,green,5,4,rot90,16,'[[+,-,-,-,+],[|,0,v,0,|],[|,0,^,0,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_3,2,7,black,4,5,sameR,16,'[[+,-,-,+],[|,0,0,|],[|,>,<,|],[|,0,0,|],[+,-,-,+]]',[])). 
pos(rhs(trn_3,3,1,yellow,10,4,sameR,26,'[[+,-,\\,0,0,0,0,0,0,0],[|,0,|,-,-,-,-,-,-,+],[+,-,|,0,|,0,0,|,0,|],[0,0,\\,-,-,-,-,-,-,+]]',[])). 
pos(rhs(trn_3,9,7,red,8,5,rot270,25,'[[+,-,-,+,0,0,0,0],[|,0,0,|,0,/,-,+],[|,0,0,|,-,|,0,|],[+,-,<,|,0,|,0,|],[0,0,0,\\,-,-,-,+]]',[])). 
pos(rhs(trn_3,9,7,black,5,8,sameR,25,'[[0,+,-,-,+],[0,|,0,0,|],[0,^,0,0,|],[/,-,-,-,+],[|,0,|,0,0],[|,-,-,\\,0],[|,0,0,|,0],[+,-,-,+,0]]',[])). 
pos(rhs(trn_3,3,1,black,4,10,rot90,26,'[[0,+,-,+],[0,|,0,|],[/,-,-,/],[|,0,|,0],[|,-,|,0],[|,0,|,0],[|,0,|,0],[|,-,|,0],[|,0,|,0],[+,-,+,0]]',[])). 
pos(rhs(trn_3,4,2,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_3,4,14,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_3,6,3,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_3,10,11,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_3,11,3,black,1,1,sameR,1,'[[0]]',[])). 
:-end_in_pos. 
