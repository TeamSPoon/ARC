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
lhs(trn_0,hv(5,2),rot90,green,hv(2,1),hv(1,2),2,sid_12). 
lhs(trn_0,hv(4,4),sameR,blue,hv(2,1),hv(2,1),2,sid_12). 
lhs(trn_0,hv(3,3),rot90,red,hv(3,1),hv(1,3),3,sid_13). 
lhs(trn_0,hv(3,19),sameR,cyan,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_0,hv(3,13),rot180,cyan,hv(8,8),hv(8,8),32,s5515589). 
lhs(trn_0,hv(6,3),sameR,yellow,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_0,hv(6,5),sameR,yellow,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(5,4),rot90,green,hv(2,1),hv(1,2),2,sid_12). 
lhs(trn_1,hv(2,17),sameR,cyan,hv(3,3),hv(3,3),9,sid_33). 
lhs(trn_1,hv(2,11),sameR,cyan,hv(9,9),hv(9,9),45,s11309857). 
lhs(trn_1,hv(4,5),sameR,yellow,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(4,6),sameR,red,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(6,4),sameR,blue,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(6,6),sameR,yellow,hv(1,1),hv(1,1),1,sid_11). 
/*
lhs(tst_0,hv(23,7),rot90,yellow,hv(2,1),hv(1,2),2,sid_12). 
lhs(tst_0,hv(24,8),rot90,red,hv(2,1),hv(1,2),2,sid_12). 
lhs(tst_0,hv(3,4),sameR,cyan,hv(4,4),hv(4,4),16,s4884971). 
lhs(tst_0,hv(3,8),flipV,cyan,hv(12,8),hv(12,8),64,s6389904). 
lhs(tst_0,hv(22,9),sameR,green,hv(1,1),hv(1,1),1,sid_11). 
*/
pos(rhs(trn_0,hv(7,19),sameR,yellow,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(trn_0,hv(3,19),sameR,yellow,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(trn_0,hv(5,15),rot90,blue,hv(4,2),hv(2,4),8,s13439039)). 
pos(rhs(trn_0,hv(7,17),sameR,green,hv(4,2),hv(4,2),8,s13439039)). 
pos(rhs(trn_0,hv(3,13),sameR,red,hv(6,2),hv(6,2),12,s10770502)). 
pos(rhs(trn_0,hv(3,13),rot270,black,hv(8,8),hv(8,8),32,s10038528)). 
pos(rhs(trn_1,hv(7,19),sameR,yellow,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(trn_1,hv(3,19),sameR,yellow,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(trn_1,hv(5,15),rot90,blue,hv(4,2),hv(2,4),8,s13439039)). 
pos(rhs(trn_1,hv(7,17),sameR,green,hv(4,2),hv(4,2),8,s13439039)). 
pos(rhs(trn_1,hv(3,13),sameR,red,hv(6,2),hv(6,2),12,s10770502)). 
pos(rhs(trn_1,hv(3,13),rot270,black,hv(8,8),hv(8,8),32,s10038528)). 
/*
pos(rhs(tst_0,hv(23,7),rot90,yellow,hv(2,1),hv(1,2),2,sid_12)). 
pos(rhs(tst_0,hv(24,8),rot90,red,hv(2,1),hv(1,2),2,sid_12)). 
pos(rhs(tst_0,hv(3,4),sameR,green,hv(4,4),hv(4,4),16,s4884971)). 
pos(rhs(tst_0,hv(3,12),sameR,red,hv(8,4),hv(8,4),32,s13166158)). 
pos(rhs(tst_0,hv(7,8),sameR,yellow,hv(8,4),hv(8,4),32,s13166158)). 
pos(rhs(tst_0,hv(22,9),sameR,green,hv(1,1),hv(1,1),1,sid_11)). 
*/
/*
*/
