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
lhs(trn_0,hv(9,2),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_0,hv(3,3),sameR,cyan,hv(3,3),hv(3,3),8,sid_323). 
lhs(trn_0,hv(8,1),sameR,cyan,hv(4,3),hv(4,3),10,s5461606). 
lhs(trn_0,hv(7,5),rot90,cyan,hv(5,3),hv(3,5),13,s16201681). 
lhs(trn_0,hv(4,4),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_0,hv(8,6),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_0,hv(8,8),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(4,5),sameR,black,hv(1,2),hv(1,2),2,sid_21). 
lhs(trn_1,hv(2,12),rot90,black,hv(1,3),hv(3,1),3,s4971157). 
lhs(trn_1,hv(8,11),sameR,black,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_1,hv(9,5),sameR,cyan,hv(3,3),hv(3,3),8,sid_323). 
lhs(trn_1,hv(7,10),sameR,cyan,hv(4,4),hv(4,4),12,s10058660). 
lhs(trn_1,hv(1,9),rot270,cyan,hv(5,5),hv(5,5),17,s12506459). 
lhs(trn_1,hv(3,2),flipDHV,cyan,hv(6,5),hv(5,6),20,s3125821). 
lhs(trn_1,hv(2,10),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(4,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(6,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(10,6),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(4,3),rot90,black,hv(1,3),hv(3,1),3,s4971157). 
lhs(trn_2,hv(7,9),sameR,black,hv(1,3),hv(1,3),3,s4971157). 
lhs(trn_2,hv(11,11),sameR,cyan,hv(3,3),hv(3,3),8,sid_323). 
lhs(trn_2,hv(3,2),sameR,cyan,hv(5,3),hv(5,3),12,s15623506). 
lhs(trn_2,hv(10,1),rot90,cyan,hv(7,3),hv(3,7),18,s15984027). 
lhs(trn_2,hv(4,8),sameR,cyan,hv(5,5),hv(5,5),17,s16173246). 
lhs(trn_2,hv(5,10),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(11,2),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(11,4),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(11,6),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(12,12),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_3,hv(3,8),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_3,hv(3,10),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_3,hv(8,3),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_3,hv(10,13),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_3,hv(11,8),sameR,black,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_3,hv(3,13),sameR,cyan,hv(3,3),hv(3,3),8,sid_323). 
lhs(trn_3,hv(2,7),rot90,cyan,hv(5,4),hv(4,5),16,s9927563). 
lhs(trn_3,hv(3,1),sameR,cyan,hv(10,4),hv(10,4),26,s7437625). 
lhs(trn_3,hv(9,7),rot270,cyan,hv(8,5),hv(5,8),25,s16702000). 
lhs(trn_3,hv(4,2),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_3,hv(4,14),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_3,hv(6,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_3,hv(11,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_3,hv(10,11),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
/*
lhs(tst_0,hv(4,3),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(tst_0,hv(14,8),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(tst_0,hv(10,4),sameR,black,hv(1,2),hv(1,2),2,sid_21). 
lhs(tst_0,hv(15,5),sameR,black,hv(1,2),hv(1,2),2,sid_21). 
lhs(tst_0,hv(12,13),sameR,black,hv(2,2),hv(2,2),4,sid_22). 
lhs(tst_0,hv(15,13),sameR,black,hv(2,2),hv(2,2),4,sid_22). 
lhs(tst_0,hv(4,11),rot90,black,hv(2,4),hv(4,2),8,s15848953). 
lhs(tst_0,hv(3,10),sameR,cyan,hv(6,4),hv(6,4),16,s9186898). 
lhs(tst_0,hv(11,12),sameR,cyan,hv(7,4),hv(7,4),20,s9953885). 
lhs(tst_0,hv(13,2),flipD,cyan,hv(8,4),hv(4,8),22,s3740004). 
lhs(tst_0,hv(3,2),sameR,cyan,hv(9,5),hv(9,5),28,s1838558). 
lhs(tst_0,hv(6,5),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(tst_0,hv(8,5),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(tst_0,hv(15,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
*/
pos(rhs(trn_0,hv(9,2),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_0,hv(3,3),sameR,blue,hv(3,3),hv(3,3),8,sid_323)). 
pos(rhs(trn_0,hv(8,1),sameR,blue,hv(4,3),hv(4,3),10,s5461606)). 
pos(rhs(trn_0,hv(8,1),rot90,black,hv(3,4),hv(4,3),10,s15769567)). 
pos(rhs(trn_0,hv(7,5),rot90,green,hv(5,3),hv(3,5),13,s16201681)). 
pos(rhs(trn_0,hv(7,5),sameR,black,hv(3,5),hv(3,5),13,s15446593)). 
pos(rhs(trn_0,hv(4,4),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_0,hv(8,6),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_0,hv(8,8),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_1,hv(4,5),sameR,black,hv(1,2),hv(1,2),2,sid_21)). 
pos(rhs(trn_1,hv(2,12),rot90,black,hv(1,3),hv(3,1),3,s4971157)). 
pos(rhs(trn_1,hv(8,11),sameR,black,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(trn_1,hv(9,5),sameR,blue,hv(3,3),hv(3,3),8,sid_323)). 
pos(rhs(trn_1,hv(7,10),sameR,blue,hv(4,4),hv(4,4),12,s10058660)). 
pos(rhs(trn_1,hv(1,9),rot270,green,hv(5,5),hv(5,5),17,s12506459)). 
pos(rhs(trn_1,hv(1,9),rot90,black,hv(5,5),hv(5,5),17,s3783575)). 
pos(rhs(trn_1,hv(3,2),flipDHV,red,hv(6,5),hv(5,6),20,s3125821)). 
pos(rhs(trn_1,hv(3,2),sameR,black,hv(5,6),hv(5,6),20,s14754199)). 
pos(rhs(trn_1,hv(2,10),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_1,hv(4,3),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_1,hv(6,3),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_1,hv(10,6),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_2,hv(4,3),rot90,black,hv(1,3),hv(3,1),3,s4971157)). 
pos(rhs(trn_2,hv(7,9),sameR,black,hv(1,3),hv(1,3),3,s4971157)). 
pos(rhs(trn_2,hv(11,11),sameR,blue,hv(3,3),hv(3,3),8,sid_323)). 
pos(rhs(trn_2,hv(3,2),sameR,blue,hv(5,3),hv(5,3),12,s15623506)). 
pos(rhs(trn_2,hv(3,2),rot90,black,hv(3,5),hv(5,3),12,s14431192)). 
pos(rhs(trn_2,hv(10,1),rot90,red,hv(7,3),hv(3,7),18,s15984027)). 
pos(rhs(trn_2,hv(10,1),sameR,black,hv(3,7),hv(3,7),18,s8039464)). 
pos(rhs(trn_2,hv(4,8),sameR,green,hv(5,5),hv(5,5),17,s16173246)). 
pos(rhs(trn_2,hv(4,8),rot270,black,hv(5,5),hv(5,5),17,s15559193)). 
pos(rhs(trn_2,hv(5,10),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_2,hv(11,2),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_2,hv(11,4),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_2,hv(11,6),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_2,hv(12,12),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_3,hv(3,8),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_3,hv(3,10),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_3,hv(8,3),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_3,hv(10,13),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_3,hv(11,8),sameR,black,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(trn_3,hv(3,13),sameR,blue,hv(3,3),hv(3,3),8,sid_323)). 
pos(rhs(trn_3,hv(2,7),rot90,green,hv(5,4),hv(4,5),16,s9927563)). 
pos(rhs(trn_3,hv(2,7),sameR,black,hv(4,5),hv(4,5),16,s9760239)). 
pos(rhs(trn_3,hv(3,1),sameR,yellow,hv(10,4),hv(10,4),26,s7437625)). 
pos(rhs(trn_3,hv(9,7),rot270,red,hv(8,5),hv(5,8),25,s16702000)). 
pos(rhs(trn_3,hv(9,7),sameR,black,hv(5,8),hv(5,8),25,s4001853)). 
pos(rhs(trn_3,hv(3,1),rot90,black,hv(4,10),hv(10,4),26,s13774428)). 
pos(rhs(trn_3,hv(4,2),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_3,hv(4,14),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_3,hv(6,3),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_3,hv(11,3),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_3,hv(10,11),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
/*
pos(rhs(tst_0,hv(4,3),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(tst_0,hv(14,8),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(tst_0,hv(10,4),sameR,black,hv(1,2),hv(1,2),2,sid_21)). 
pos(rhs(tst_0,hv(15,5),sameR,black,hv(1,2),hv(1,2),2,sid_21)). 
pos(rhs(tst_0,hv(12,13),sameR,black,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(tst_0,hv(15,13),sameR,black,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(tst_0,hv(4,11),rot90,black,hv(2,4),hv(4,2),8,s15848953)). 
pos(rhs(tst_0,hv(3,10),sameR,blue,hv(6,4),hv(6,4),16,s9186898)). 
pos(rhs(tst_0,hv(11,12),sameR,green,hv(7,4),hv(7,4),20,s9953885)). 
pos(rhs(tst_0,hv(13,2),flipD,red,hv(8,4),hv(4,8),22,s3740004)). 
pos(rhs(tst_0,hv(3,2),sameR,yellow,hv(9,5),hv(9,5),28,s1838558)). 
pos(rhs(tst_0,hv(6,5),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(tst_0,hv(8,5),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(tst_0,hv(15,3),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
*/
/*
*/
