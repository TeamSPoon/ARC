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
lhs(trn_0,11,11,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_0,13,15,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_0,14,11,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_0,11,5,black,1,2,sameR,2,'[[|],[|]]',[]). 
lhs(trn_0,2,2,cyan,4,3,rot270,11,'[[+,-,-,+],[|,0,>,|],[+,-,-,+]]',[]). 
lhs(trn_0,4,11,cyan,5,4,rot90,19,'[[+,-,-,-,+],[|,<,0,>,|],[|,*,v,*,|],[+,-,-,-,+]]',[]). 
lhs(trn_0,12,14,cyan,5,4,flipD,17,'[[+,-,-,-,+],[|,0,|,0,|],[|,0,|,v,|],[+,-,-,-,+]]',[]). 
lhs(trn_0,10,10,cyan,7,3,sameR,17,'[[+,-,-,-,-,-,+],[|,0,0,|,0,0,|],[+,-,-,-,-,-,+]]',[]). 
lhs(trn_0,6,4,cyan,7,5,rot180,31,'[[+,-,-,-,-,-,+],[|,^,|,0,|,^,|],[|,0,|,v,|,0,|],[|,0,|,~,*,v,|],[+,-,-,-,-,-,+]]',[]). 
lhs(trn_0,3,3,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_0,5,13,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_0,7,6,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_0,9,7,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_0,13,17,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,7,8,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_1,13,9,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_1,15,14,black,1,2,sameR,2,'[[|],[|]]',[]). 
lhs(trn_1,4,3,black,2,2,sameR,4,'[[+,+],[+,+]]',[]). 
lhs(trn_1,12,8,cyan,4,4,sameR,14,'[[+,-,-,+],[|,0,0,|],[|,-,-,|],[+,-,-,+]]',[]). 
lhs(trn_1,2,13,cyan,5,4,sameR,18,'[[+,-,-,-,+],[|,^,|,0,|],[|,0,|,v,|],[+,-,-,-,+]]',[]). 
lhs(trn_1,6,7,cyan,5,4,rot270,17,'[[+,-,-,-,+],[|,0,|,0,|],[|,0,|,v,|],[+,-,-,-,+]]',[]). 
lhs(trn_1,3,2,cyan,6,4,sameR,19,'[[+,-,-,-,-,+],[|,0,0,|,0,|],[|,0,0,|,v,|],[+,-,-,-,-,+]]',[]). 
lhs(trn_1,12,2,cyan,5,5,sameR,22,'[[+,-,-,-,+],[|,0,|,^,|],[|,-,|,0,|],[|,0,|,v,|],[+,-,-,-,+]]',[]). 
lhs(trn_1,10,13,cyan,7,5,sameR,30,'[[+,-,-,-,-,-,+],[|,0,|,^,|,0,|],[|,-,|,0,|,0,|],[|,0,|,v,\\,v,|],[+,-,-,-,-,-,+]]',[]). 
lhs(trn_1,3,15,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,5,14,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,7,3,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,8,10,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,11,14,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,11,16,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,13,3,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,13,5,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,13,15,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_1,15,4,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,5,15,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_2,10,5,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_2,11,7,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_2,14,16,black,1,2,rot90,2,'[[|],[|]]',[]). 
lhs(trn_2,4,9,black,1,2,sameR,2,'[[|],[|]]',[]). 
lhs(trn_2,6,10,black,1,2,sameR,2,'[[|],[|]]',[]). 
lhs(trn_2,3,3,black,2,2,sameR,4,'[[+,+],[+,+]]',[]). 
lhs(trn_2,11,13,cyan,6,5,flipV,25,'[[+,-,-,-,-,+],[|,0,|,0,0,|],[|,-,0,-,-,|],[|,0,|,0,>,|],[+,-,-,-,-,+]]',[]). 
lhs(trn_2,2,2,cyan,6,5,flipH,24,'[[+,-,-,-,-,+],[|,0,|,0,0,|],[|,-,|,0,0,|],[|,0,|,-,-,|],[+,-,-,-,-,+]]',[]). 
lhs(trn_2,9,4,cyan,6,5,sameR,25,'[[+,-,-,-,-,+],[|,0,0,|,0,|],[|,-,-,-,-,|],[|,<,0,0,>,|],[+,-,-,-,-,+]]',[]). 
lhs(trn_2,3,8,cyan,9,5,rot270,38,'[[+,-,-,-,-,-,-,-,+],[|,<,0,0,>,~,|,0,|],[|,-,-,-,/,^,|,0,|],[|,0,0,>,<,0,|,v,|],[+,-,-,-,-,-,-,-,+]]',[]). 
lhs(trn_2,4,13,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,6,3,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,6,5,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,12,14,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,12,16,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,13,5,black,1,1,sameR,1,'[[0]]',[]). 
lhs(trn_2,14,14,black,1,1,sameR,1,'[[0]]',[]). 
:-end_bg. 
:-begin_in_pos. 
pos(rhs(trn_0,11,11,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_0,13,15,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_0,14,11,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_0,11,5,black,1,2,sameR,2,'[[|],[|]]',[])). 
pos(rhs(trn_0,2,2,blue,4,3,rot270,11,'[[+,-,-,+],[|,0,>,|],[+,-,-,+]]',[])). 
pos(rhs(trn_0,4,11,blue,5,4,rot90,19,'[[+,-,-,-,+],[|,<,0,>,|],[|,*,v,*,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_0,12,14,red,5,4,flipD,17,'[[+,-,-,-,+],[|,0,|,0,|],[|,0,|,v,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_0,10,10,red,7,3,sameR,17,'[[+,-,-,-,-,-,+],[|,0,0,|,0,0,|],[+,-,-,-,-,-,+]]',[])). 
pos(rhs(trn_0,6,4,green,7,5,rot180,31,'[[+,-,-,-,-,-,+],[|,^,|,0,|,^,|],[|,0,|,v,|,0,|],[|,0,|,~,*,v,|],[+,-,-,-,-,-,+]]',[])). 
pos(rhs(trn_0,3,3,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,5,13,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,7,6,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,9,7,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_0,13,17,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_1,11,11,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_1,13,15,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_1,14,11,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_1,11,5,black,1,2,sameR,2,'[[|],[|]]',[])). 
pos(rhs(trn_1,2,2,blue,4,3,rot270,11,'[[+,-,-,+],[|,0,>,|],[+,-,-,+]]',[])). 
pos(rhs(trn_1,4,11,blue,5,4,rot90,19,'[[+,-,-,-,+],[|,<,0,>,|],[|,*,v,*,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_1,12,14,red,5,4,flipD,17,'[[+,-,-,-,+],[|,0,|,0,|],[|,0,|,v,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_1,10,10,red,7,3,sameR,17,'[[+,-,-,-,-,-,+],[|,0,0,|,0,0,|],[+,-,-,-,-,-,+]]',[])). 
pos(rhs(trn_1,6,4,green,7,5,rot180,31,'[[+,-,-,-,-,-,+],[|,^,|,0,|,^,|],[|,0,|,v,|,0,|],[|,0,|,~,*,v,|],[+,-,-,-,-,-,+]]',[])). 
pos(rhs(trn_1,3,3,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_1,5,13,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_1,7,6,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_1,9,7,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_1,13,17,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_2,11,11,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_2,13,15,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_2,14,11,black,1,2,rot90,2,'[[|],[|]]',[])). 
pos(rhs(trn_2,11,5,black,1,2,sameR,2,'[[|],[|]]',[])). 
pos(rhs(trn_2,2,2,blue,4,3,rot270,11,'[[+,-,-,+],[|,0,>,|],[+,-,-,+]]',[])). 
pos(rhs(trn_2,4,11,blue,5,4,rot90,19,'[[+,-,-,-,+],[|,<,0,>,|],[|,*,v,*,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_2,12,14,red,5,4,flipD,17,'[[+,-,-,-,+],[|,0,|,0,|],[|,0,|,v,|],[+,-,-,-,+]]',[])). 
pos(rhs(trn_2,10,10,red,7,3,sameR,17,'[[+,-,-,-,-,-,+],[|,0,0,|,0,0,|],[+,-,-,-,-,-,+]]',[])). 
pos(rhs(trn_2,6,4,green,7,5,rot180,31,'[[+,-,-,-,-,-,+],[|,^,|,0,|,^,|],[|,0,|,v,|,0,|],[|,0,|,~,*,v,|],[+,-,-,-,-,-,+]]',[])). 
pos(rhs(trn_2,3,3,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_2,5,13,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_2,7,6,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_2,9,7,black,1,1,sameR,1,'[[0]]',[])). 
pos(rhs(trn_2,13,17,black,1,1,sameR,1,'[[0]]',[])). 
:-end_in_pos. 
