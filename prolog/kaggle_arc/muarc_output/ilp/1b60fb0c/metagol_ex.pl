:-style_check(- (discontiguous)). 
max_body(6). 
max_vars(8). 
non_magic(4). 
% head_pred(rhs,10). 
% body_pred(lhs,10). 
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
lhs(trn_0,hv(4,2),rot90,blue,hv(8,6),hv(6,8),22,s10646592). 
lhs(trn_1,hv(7,4),sameR,black,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_1,hv(7,7),sameR,black,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_1,hv(9,9),sameR,black,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_1,hv(4,2),rot90,blue,hv(9,7),hv(7,9),37,s12778084). 
lhs(trn_2,hv(7,7),sameR,black,hv(4,4),hv(4,4),9,s8522600). 
lhs(trn_2,hv(4,2),rot90,blue,hv(9,7),hv(7,9),31,s5627827). 
lhs(trn_2,hv(7,5),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(7,7),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
/*
lhs(tst_0,hv(6,1),sameR,black,hv(5,4),hv(5,4),15,s14796172). 
lhs(tst_0,hv(7,6),rot90,black,hv(5,4),hv(4,5),15,s10537774). 
lhs(tst_0,hv(1,1),flipH,black,hv(5,10),hv(5,10),39,s12773961). 
lhs(tst_0,hv(3,1),rot90,blue,hv(10,8),hv(8,10),31,s14123544). 
*/
pos(rhs(trn_0,hv(2,5),sameR,red,hv(3,3),hv(3,3),6,s10038304)). 
pos(rhs(trn_0,hv(4,2),rot90,blue,hv(8,6),hv(6,8),22,s10646592)). 
pos(rhs(trn_1,hv(2,5),sameR,red,hv(3,3),hv(3,3),6,s10038304)). 
pos(rhs(trn_1,hv(4,2),rot90,blue,hv(8,6),hv(6,8),22,s10646592)). 
pos(rhs(trn_2,hv(2,5),sameR,red,hv(3,3),hv(3,3),6,s10038304)). 
pos(rhs(trn_2,hv(4,2),rot90,blue,hv(8,6),hv(6,8),22,s10646592)). 
/*
pos(rhs(tst_0,hv(1,4),sameR,red,hv(4,5),hv(4,5),9,s2012266)). 
pos(rhs(tst_0,hv(1,1),rot90,black,hv(5,4),hv(4,5),15,s14796172)). 
pos(rhs(tst_0,hv(6,1),sameR,black,hv(5,4),hv(5,4),15,s14796172)). 
pos(rhs(tst_0,hv(7,6),rot90,black,hv(5,4),hv(4,5),15,s10537774)). 
pos(rhs(tst_0,hv(1,7),sameR,black,hv(5,4),hv(5,4),15,s10537774)). 
pos(rhs(tst_0,hv(3,1),rot90,blue,hv(10,8),hv(8,10),31,s14123544)). 
*/
/*
*/
