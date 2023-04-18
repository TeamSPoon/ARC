:-style_check(- (discontiguous)). 
:-use_module(library(aleph)). 
:-if(current_predicate(use_rendering/1)). 
:-use_rendering(prolog). 
:-endif. 
:-aleph_set(verbosity,1). 
:-aleph_set(interactive,false). 
:-aleph_set(i,4). 
:-aleph_set(nodes,10000). 
:-aleph. 
% :- modeh(*,rhs(+state,+nat30,+nat30,+color,+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
% :- modeb(*,lhs(+state,+nat30,+nat30,#(color),+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
% :- modeb(*,my_geq(+nat30,-#(nat30))). 
% :- modeb(*,my_leq(+nat30,-#(nat30))). 
% :- modeb(*,my_add(+nat30,+nat30,-nat30)). 
% :- modeb(*,my_mult(+nat30,#(nat30),-nat30)). 
% :- lazy_evaluate(my_add/3). 
% :- lazy_evaluate(my_geq/2). 
% :- lazy_evaluate(my_leq/2). 
% :- lazy_evaluate(my_mult/3). 
% :- determination(rhs/7,lhs/7). 
% :- determination(rhs/7,color_change/2). 
% :- determination(rhs/7,incr_nat30/2). 
% :- determination(rhs/7,my_geq/2). 
% :- determination(rhs/7,my_leq/2). 
% :- determination(rhs/7,my_add/3). 
% :- determination(rhs/7,my_mult/3). 
:-output(rhs_peice/2). 
:-output(rhs_loc2D/3). 
:-output(rhs_rot2D/2). 
:-output(rhs_pen_color/2). 
:-output(rhs_rotSize2D/3). 
:-output(rhs_vis2D/3). 
:-output(rhs_mass/2). 
:-output(rhs_iz_sid/2). 
:-output(rhs/8). 
/*
*/
:-input_cw(lhs_peice/2). 
:-input_cw(lhs_loc2D/3). 
:-input_cw(lhs_rot2D/2). 
:-input_cw(lhs_pen_color/2). 
:-input_cw(lhs_rotSize2D/3). 
:-input_cw(lhs_vis2D/3). 
:-input_cw(lhs_mass/2). 
:-input_cw(lhs_iz_sid/2). 
:-input_cw(lhs/8). 
/*
*/
:-modeh(*,rhs_peice(+scope,+peice)). 
:-modeh(*,rhs_loc2D(+peice,+nat30,+nat30)). 
:-modeh(*,rhs_rot2D(+peice,+rot2D)). 
:-modeh(*,rhs_pen_color(+peice,+ #(color))). 
:-modeh(*,rhs_rotSize2D(+peice,+nat30,+nat30)). 
:-modeh(*,rhs_vis2D(+peice,+nat30,+nat30)). 
:-modeh(*,rhs_mass(+peice,nat900)). 
:-modeh(*,rhs_iz_sid(+peice,+sid)). 
:-modeh(*,rhs(+peice,+rhs,+rhs,+ #(color),+rhs,+rhs,+nat30,+rhs)). 
:-modeh(*,rhs(+peice,+rhs,+rhs,+ #(color),+rhs,+rhs,+rhs,+rhs)). 
/*
*/
:-modeb(*,lhs_peice(+scope,+peice)). 
:-modeb(*,lhs_loc2D(+peice,+nat30,+nat30)). 
:-modeb(*,lhs_rot2D(+peice,-rot2D)). 
:-modeb(*,lhs_pen_color(+peice,+ #(color))). 
:-modeb(*,lhs_rotSize2D(+peice,+nat30,+nat30)). 
:-modeb(*,lhs_vis2D(+peice,+nat30,+nat30)). 
:-modeb(*,lhs_mass(+peice,nat900)). 
:-modeb(*,lhs_iz_sid(+peice,-sid)). 
:-modeb(*,lhs(+peice,-lhs,-lhs,+ #(color),-lhs,-lhs,+nat30,-lhs)). 
:-modeb(*,lhs(+peice,-lhs,-lhs,+ #(color),-lhs,-lhs,-lhs,-lhs)). 
/*
*/
/*
*/
/*
*/
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
:-begin_bg. 
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
lhs(trn_0,hv(11,11),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_0,hv(13,15),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_0,hv(14,11),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_0,hv(11,5),sameR,black,hv(1,2),hv(1,2),2,sid_21). 
lhs(trn_0,hv(2,2),rot270,cyan,hv(4,3),hv(3,4),11,s11172463). 
lhs(trn_0,hv(4,11),rot90,cyan,hv(5,4),hv(4,5),19,s16234546). 
lhs(trn_0,hv(12,14),flipD,cyan,hv(5,4),hv(4,5),17,s1774494). 
lhs(trn_0,hv(10,10),sameR,cyan,hv(7,3),hv(7,3),17,s2817948). 
lhs(trn_0,hv(6,4),rot180,cyan,hv(7,5),hv(7,5),31,s5123897). 
lhs(trn_0,hv(3,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_0,hv(5,13),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_0,hv(7,6),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_0,hv(9,7),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_0,hv(13,17),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(7,8),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_1,hv(13,9),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_1,hv(15,14),sameR,black,hv(1,2),hv(1,2),2,sid_21). 
lhs(trn_1,hv(4,3),sameR,black,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_1,hv(12,8),sameR,cyan,hv(4,4),hv(4,4),14,s6057499). 
lhs(trn_1,hv(2,13),sameR,cyan,hv(5,4),hv(5,4),18,s3896953). 
lhs(trn_1,hv(6,7),rot270,cyan,hv(5,4),hv(4,5),17,s1774494). 
lhs(trn_1,hv(3,2),sameR,cyan,hv(6,4),hv(6,4),19,s8126788). 
lhs(trn_1,hv(12,2),sameR,cyan,hv(5,5),hv(5,5),22,s554292). 
lhs(trn_1,hv(10,13),sameR,cyan,hv(7,5),hv(7,5),30,s7751306). 
lhs(trn_1,hv(3,15),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(5,14),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(7,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(8,10),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(11,14),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(11,16),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(13,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(13,5),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(13,15),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(15,4),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(5,15),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_2,hv(10,5),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_2,hv(11,7),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_2,hv(14,16),rot90,black,hv(1,2),hv(2,1),2,sid_21). 
lhs(trn_2,hv(4,9),sameR,black,hv(1,2),hv(1,2),2,sid_21). 
lhs(trn_2,hv(6,10),sameR,black,hv(1,2),hv(1,2),2,sid_21). 
lhs(trn_2,hv(3,3),sameR,black,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_2,hv(11,13),flipV,cyan,hv(6,5),hv(6,5),25,s16667395). 
lhs(trn_2,hv(2,2),flipH,cyan,hv(6,5),hv(6,5),24,s3011901). 
lhs(trn_2,hv(9,4),sameR,cyan,hv(6,5),hv(6,5),25,s12420848). 
lhs(trn_2,hv(3,8),rot270,cyan,hv(9,5),hv(5,9),38,s7428557). 
lhs(trn_2,hv(4,13),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(6,3),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(6,5),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(12,14),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(12,16),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(13,5),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(14,14),sameR,black,hv(1,1),hv(1,1),1,sid_11). 
/*
lhs(tst_0,hv(4,8),rot90,blue,hv(1,2),hv(2,1),2,sid_21). 
lhs(tst_0,hv(10,5),rot90,blue,hv(1,2),hv(2,1),2,sid_21). 
lhs(tst_0,hv(12,9),rot90,green,hv(1,2),hv(2,1),2,sid_21). 
lhs(tst_0,hv(4,2),sameR,green,hv(1,2),hv(1,2),2,sid_21). 
lhs(tst_0,hv(12,12),sameR,green,hv(1,2),hv(1,2),2,sid_21). 
lhs(tst_0,hv(3,13),sameR,green,hv(2,2),hv(2,2),4,sid_22). 
lhs(tst_0,hv(13,3),sameR,orange,hv(2,2),hv(2,2),4,sid_22). 
lhs(tst_0,hv(14,14),sameR,orange,hv(2,2),hv(2,2),4,sid_22). 
lhs(tst_0,hv(15,10),sameR,blue,hv(2,2),hv(2,2),4,sid_22). 
lhs(tst_0,hv(3,7),sameR,blue,hv(4,3),hv(4,3),10,s5461606). 
lhs(tst_0,hv(3,1),sameR,green,hv(5,4),hv(5,4),17,s1774494). 
lhs(tst_0,hv(2,12),flipV,orange,hv(7,5),hv(7,5),30,s371978). 
lhs(tst_0,hv(9,2),sameR,green,hv(7,5),hv(7,5),28,s8137512). 
lhs(tst_0,hv(11,8),rot270,orange,hv(9,7),hv(7,9),51,s5010039). 
lhs(tst_0,hv(6,2),sameR,blue,hv(1,1),hv(1,1),1,sid_11). 
lhs(tst_0,hv(6,15),sameR,green,hv(1,1),hv(1,1),1,sid_11). 
lhs(tst_0,hv(10,3),sameR,green,hv(1,1),hv(1,1),1,sid_11). 
*/
:-end_bg. 
:-begin_in_pos. 
pos(rhs(trn_0,hv(11,11),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_0,hv(13,15),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_0,hv(14,11),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_0,hv(11,5),sameR,black,hv(1,2),hv(1,2),2,sid_21)). 
pos(rhs(trn_0,hv(2,2),rot270,blue,hv(4,3),hv(3,4),11,s11172463)). 
pos(rhs(trn_0,hv(2,2),rot180,black,hv(3,4),hv(3,4),11,s4099472)). 
pos(rhs(trn_0,hv(4,11),rot90,blue,hv(5,4),hv(4,5),19,s16234546)). 
pos(rhs(trn_0,hv(12,14),flipD,red,hv(5,4),hv(4,5),17,s1774494)). 
pos(rhs(trn_0,hv(12,14),rot180,black,hv(4,5),hv(4,5),17,s544726)). 
pos(rhs(trn_0,hv(4,11),rot180,black,hv(4,5),hv(4,5),19,s16096614)). 
pos(rhs(trn_0,hv(10,10),sameR,red,hv(7,3),hv(7,3),17,s2817948)). 
pos(rhs(trn_0,hv(10,10),rot90,black,hv(3,7),hv(7,3),17,s6445642)). 
pos(rhs(trn_0,hv(6,4),rot180,green,hv(7,5),hv(7,5),31,s5123897)). 
pos(rhs(trn_0,hv(6,4),rot90,black,hv(5,7),hv(7,5),31,s588687)). 
pos(rhs(trn_0,hv(3,3),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_0,hv(5,13),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_0,hv(7,6),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_0,hv(9,7),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_0,hv(13,17),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_1,hv(11,11),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_1,hv(13,15),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_1,hv(14,11),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_1,hv(11,5),sameR,black,hv(1,2),hv(1,2),2,sid_21)). 
pos(rhs(trn_1,hv(2,2),rot270,blue,hv(4,3),hv(3,4),11,s11172463)). 
pos(rhs(trn_1,hv(2,2),rot180,black,hv(3,4),hv(3,4),11,s4099472)). 
pos(rhs(trn_1,hv(4,11),rot90,blue,hv(5,4),hv(4,5),19,s16234546)). 
pos(rhs(trn_1,hv(12,14),flipD,red,hv(5,4),hv(4,5),17,s1774494)). 
pos(rhs(trn_1,hv(12,14),rot180,black,hv(4,5),hv(4,5),17,s544726)). 
pos(rhs(trn_1,hv(4,11),rot180,black,hv(4,5),hv(4,5),19,s16096614)). 
pos(rhs(trn_1,hv(10,10),sameR,red,hv(7,3),hv(7,3),17,s2817948)). 
pos(rhs(trn_1,hv(10,10),rot90,black,hv(3,7),hv(7,3),17,s6445642)). 
pos(rhs(trn_1,hv(6,4),rot180,green,hv(7,5),hv(7,5),31,s5123897)). 
pos(rhs(trn_1,hv(6,4),rot90,black,hv(5,7),hv(7,5),31,s588687)). 
pos(rhs(trn_1,hv(3,3),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_1,hv(5,13),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_1,hv(7,6),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_1,hv(9,7),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_1,hv(13,17),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_2,hv(11,11),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_2,hv(13,15),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_2,hv(14,11),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(trn_2,hv(11,5),sameR,black,hv(1,2),hv(1,2),2,sid_21)). 
pos(rhs(trn_2,hv(2,2),rot270,blue,hv(4,3),hv(3,4),11,s11172463)). 
pos(rhs(trn_2,hv(2,2),rot180,black,hv(3,4),hv(3,4),11,s4099472)). 
pos(rhs(trn_2,hv(4,11),rot90,blue,hv(5,4),hv(4,5),19,s16234546)). 
pos(rhs(trn_2,hv(12,14),flipD,red,hv(5,4),hv(4,5),17,s1774494)). 
pos(rhs(trn_2,hv(12,14),rot180,black,hv(4,5),hv(4,5),17,s544726)). 
pos(rhs(trn_2,hv(4,11),rot180,black,hv(4,5),hv(4,5),19,s16096614)). 
pos(rhs(trn_2,hv(10,10),sameR,red,hv(7,3),hv(7,3),17,s2817948)). 
pos(rhs(trn_2,hv(10,10),rot90,black,hv(3,7),hv(7,3),17,s6445642)). 
pos(rhs(trn_2,hv(6,4),rot180,green,hv(7,5),hv(7,5),31,s5123897)). 
pos(rhs(trn_2,hv(6,4),rot90,black,hv(5,7),hv(7,5),31,s588687)). 
pos(rhs(trn_2,hv(4,3),flipH,black,hv(11,14),hv(11,14),7,s7935261)). 
pos(rhs(trn_2,hv(3,3),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_2,hv(5,13),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_2,hv(7,6),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(trn_2,hv(9,7),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
/*
pos(rhs(tst_0,hv(4,8),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(tst_0,hv(10,5),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(tst_0,hv(12,9),rot90,black,hv(1,2),hv(2,1),2,sid_21)). 
pos(rhs(tst_0,hv(4,2),sameR,black,hv(1,2),hv(1,2),2,sid_21)). 
pos(rhs(tst_0,hv(12,12),sameR,black,hv(1,2),hv(1,2),2,sid_21)). 
pos(rhs(tst_0,hv(3,13),sameR,black,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(tst_0,hv(13,3),sameR,black,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(tst_0,hv(14,14),sameR,black,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(tst_0,hv(15,10),sameR,black,hv(2,2),hv(2,2),4,sid_22)). 
pos(rhs(tst_0,hv(3,7),sameR,blue,hv(4,3),hv(4,3),10,s5461606)). 
pos(rhs(tst_0,hv(3,1),sameR,red,hv(5,4),hv(5,4),17,s1774494)). 
pos(rhs(tst_0,hv(2,12),flipV,red,hv(7,5),hv(7,5),30,s371978)). 
pos(rhs(tst_0,hv(9,2),sameR,green,hv(7,5),hv(7,5),28,s8137512)). 
pos(rhs(tst_0,hv(11,8),rot270,orange,hv(9,7),hv(7,9),51,s5010039)). 
pos(rhs(tst_0,hv(6,2),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(tst_0,hv(6,15),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
pos(rhs(tst_0,hv(10,3),sameR,black,hv(1,1),hv(1,1),1,sid_11)). 
*/
:-end_in_pos. 
/*
*/
