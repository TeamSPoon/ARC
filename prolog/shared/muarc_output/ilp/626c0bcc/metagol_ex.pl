:-style_check(- (discontiguous)). 
max_body(6). 
max_vars(8). 
non_magic(4). 
% head_pred(rhs,7). 
% body_pred(lhs,7). 
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
type(rhs,(state,loc2D,rot2D,color,vis2D,rotSize2D,nat900,shape)). 
% direction(lhs,(out,out,out,out,out,out,out,out,out,out)). 
type(lhs,(state,loc2D,rot2D,color,vis2D,rotSize2D,nat900,shape)). 
magic_type(color). 
magic_type(nat30). 
magic_value_type(color). 
magic_value_type(nat30). 
numerical_pred(my_add,3). 
numerical_pred(my_geq,2). 
numerical_pred(my_leq,2). 
numerical_pred(my_mult,3). 
/*
*/
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
lhs(trn_0,hv(1,1),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_0,hv(1,1),flipD,cyan,hv(5,6),hv(6,5),17,s10866090). 
lhs(trn_0,hv(4,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(5,1),rot270,cyan,hv(4,2),hv(2,4),7,s5659926). 
lhs(trn_1,hv(3,1),flipH,black,hv(2,4),hv(2,4),6,s11680822). 
lhs(trn_1,hv(1,1),flipD,cyan,hv(6,4),hv(4,6),10,s15829814). 
lhs(trn_1,hv(1,1),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(1,1),rot180,cyan,hv(4,4),hv(4,4),13,s5612495). 
lhs(trn_2,hv(3,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
/*
lhs(tst_0,hv(1,1),flipV,cyan,hv(6,4),hv(6,4),16,s1281831). 
lhs(tst_0,hv(1,1),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(tst_0,hv(4,1),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(tst_0,hv(4,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
*/
pos(rhs(trn_0,hv(1,1),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_0,hv(3,1),sameR,blue,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(trn_0,hv(4,4),sameR,blue,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(trn_0,hv(5,2),rot270,yellow,hv(2,2),hv(2,2),3,s11261491)). 
pos(rhs(trn_0,hv(1,2),rot90,green,hv(2,2),hv(2,2),3,s11261491)). 
pos(rhs(trn_0,hv(2,3),sameR,red,hv(2,2),hv(2,2),3,s11261491)). 
pos(rhs(trn_0,hv(4,3),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_1,hv(1,1),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_1,hv(3,1),sameR,blue,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(trn_1,hv(4,4),sameR,blue,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(trn_1,hv(5,2),rot270,yellow,hv(2,2),hv(2,2),3,s11261491)). 
pos(rhs(trn_1,hv(1,2),rot90,green,hv(2,2),hv(2,2),3,s11261491)). 
pos(rhs(trn_1,hv(2,3),sameR,red,hv(2,2),hv(2,2),3,s11261491)). 
pos(rhs(trn_1,hv(4,3),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_2,hv(1,1),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_2,hv(3,1),sameR,blue,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(trn_2,hv(4,4),sameR,blue,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(trn_2,hv(5,2),rot270,yellow,hv(2,2),hv(2,2),3,s11261491)). 
pos(rhs(trn_2,hv(1,2),rot90,green,hv(2,2),hv(2,2),3,s11261491)). 
pos(rhs(trn_2,hv(2,3),sameR,red,hv(2,2),hv(2,2),3,s11261491)). 
pos(rhs(trn_2,hv(4,3),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
/*
pos(rhs(tst_0,hv(5,1),sameR,blue,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(tst_0,hv(3,1),rot270,yellow,hv(2,2),hv(2,2),3,s11261491)). 
pos(rhs(tst_0,hv(2,3),rot90,green,hv(2,2),hv(2,2),3,s11261491)). 
pos(rhs(tst_0,hv(1,1),sameR,red,hv(2,2),hv(2,2),3,s11261491)). 
pos(rhs(tst_0,hv(4,3),sameR,red,hv(2,2),hv(2,2),3,s11261491)). 
pos(rhs(tst_0,hv(1,1),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(tst_0,hv(4,1),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(tst_0,hv(4,3),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
*/
/*
*/
