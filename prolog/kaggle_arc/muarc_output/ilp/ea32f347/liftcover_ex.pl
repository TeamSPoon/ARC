:-use_module(library(slipcover)). 
:-if(current_predicate(use_rendering/1)). 
:-use_rendering(prolog). 
:-endif. 
:-sc. 
:-set_sc(verbosity,3). 
:-set_sc(depth_bound,false). 
:-set_sc(neg_ex,given). 
bg([]). 
in([]). 
input_cw(incr_nat30/2). 
input_cw(color_change/2). 
determination(P/Q,R/S):-input_cw(R/S),output(P/Q). 
fold(trn_0,[trn_0]). 
fold(trn_1,[trn_1]). 
fold(trn_2,[trn_2]). 
% modeh(*,rhs(+state,+nat30,+nat30,+color,+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
% modeb(*,lhs(+state,+nat30,+nat30,#(color),+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
% modeb(*,my_geq(+nat30,-#(nat30))). 
% modeb(*,my_leq(+nat30,-#(nat30))). 
% modeb(*,my_add(+nat30,+nat30,-nat30)). 
% modeb(*,my_mult(+nat30,#(nat30),-nat30)). 
% lazy_evaluate(my_add/3). 
% lazy_evaluate(my_geq/2). 
% lazy_evaluate(my_leq/2). 
% lazy_evaluate(my_mult/3). 
% determination(rhs/7,lhs/7). 
% determination(rhs/7,color_change/2). 
% determination(rhs/7,incr_nat30/2). 
% determination(rhs/7,my_geq/2). 
% determination(rhs/7,my_leq/2). 
% determination(rhs/7,my_add/3). 
% determination(rhs/7,my_mult/3). 
output(rhs_peice/2). 
output(rhs_loc2D/3). 
output(rhs_rot2D/2). 
output(rhs_pen_color/2). 
output(rhs_rotSize2D/3). 
output(rhs_vis2D/3). 
output(rhs_mass/2). 
output(rhs_iz_sid/2). 
output(rhs/8). 
/*
*/
input_cw(lhs_peice/2). 
input_cw(lhs_loc2D/3). 
input_cw(lhs_rot2D/2). 
input_cw(lhs_pen_color/2). 
input_cw(lhs_rotSize2D/3). 
input_cw(lhs_vis2D/3). 
input_cw(lhs_mass/2). 
input_cw(lhs_iz_sid/2). 
input_cw(lhs/8). 
/*
*/
modeh(*,rhs_peice(+scope,+peice)). 
modeh(*,rhs_loc2D(+peice,+nat30,+nat30)). 
modeh(*,rhs_rot2D(+peice,+rot2D)). 
modeh(*,rhs_pen_color(+peice,+ #(color))). 
modeh(*,rhs_rotSize2D(+peice,+nat30,+nat30)). 
modeh(*,rhs_vis2D(+peice,+nat30,+nat30)). 
modeh(*,rhs_mass(+peice,nat900)). 
modeh(*,rhs_iz_sid(+peice,+sid)). 
modeh(*,rhs(+peice,+rhs,+rhs,+ #(color),+rhs,+rhs,+nat30,+rhs)). 
/*
*/
modeb(*,lhs_peice(+scope,+peice)). 
modeb(*,lhs_loc2D(+peice,+nat30,+nat30)). 
modeb(*,lhs_rot2D(+peice,-rot2D)). 
modeb(*,lhs_pen_color(+peice,+ #(color))). 
modeb(*,lhs_rotSize2D(+peice,+nat30,+nat30)). 
modeb(*,lhs_vis2D(+peice,+nat30,+nat30)). 
modeb(*,lhs_mass(+peice,nat900)). 
modeb(*,lhs_iz_sid(+peice,-sid)). 
modeb(*,lhs(+peice,-lhs,-lhs,+ #(color),-lhs,-lhs,+nat30,-lhs)). 
/*
*/
/*
*/
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
lhs(trn_0,hv(8,5),rot90,silver,hv(3,1),hv(1,3),3,sid_13). 
lhs(trn_0,hv(5,3),rot90,silver,hv(5,1),hv(1,5),5,s1265795). 
lhs(trn_0,hv(2,2),rot90,silver,hv(6,1),hv(1,6),6,s11156184). 
lhs(trn_1,hv(8,6),rot90,silver,hv(2,1),hv(1,2),2,sid_12). 
lhs(trn_1,hv(2,4),rot90,silver,hv(4,1),hv(1,4),4,sid_14). 
lhs(trn_1,hv(5,2),rot90,silver,hv(6,1),hv(1,6),6,s11156184). 
lhs(trn_2,hv(3,8),sameR,silver,hv(3,1),hv(3,1),3,sid_13). 
lhs(trn_2,hv(8,4),rot90,silver,hv(5,1),hv(1,5),5,s1265795). 
lhs(trn_2,hv(1,3),sameR,silver,hv(6,1),hv(6,1),6,s11156184). 
lhs(trn_3,hv(2,3),sameR,silver,hv(4,1),hv(4,1),4,sid_14). 
lhs(trn_3,hv(2,6),sameR,silver,hv(5,1),hv(5,1),5,s1265795). 
lhs(trn_3,hv(8,2),rot90,silver,hv(7,1),hv(1,7),7,s2476004). 
/*
lhs(tst_0,hv(4,2),rot90,silver,hv(3,1),hv(1,3),3,sid_13). 
lhs(tst_0,hv(1,9),sameR,silver,hv(5,1),hv(5,1),5,s1265795). 
lhs(tst_0,hv(5,6),sameR,silver,hv(6,1),hv(6,1),6,s11156184). 
*/
begin(model(trn_0)). 
lhs_peice(trn_0,obj_0_44_in). 
lhs_loc2D(obj_0_44_in,8,5). 
lhs_rot2D(obj_0_44_in,rot90). 
lhs_pen_color(obj_0_44_in,silver). 
lhs_rotSize2D(obj_0_44_in,3,1). 
lhs_vis2D(obj_0_44_in,1,3). 
lhs_mass(obj_0_44_in,3). 
lhs_iz_sid(obj_0_44_in,sid_13). 
lhs_peice(trn_0,obj_0_298_in). 
lhs_loc2D(obj_0_298_in,5,3). 
lhs_rot2D(obj_0_298_in,rot90). 
lhs_pen_color(obj_0_298_in,silver). 
lhs_rotSize2D(obj_0_298_in,5,1). 
lhs_vis2D(obj_0_298_in,1,5). 
lhs_mass(obj_0_298_in,5). 
lhs_iz_sid(obj_0_298_in,s1265795). 
lhs_peice(trn_0,obj_0_172_in). 
lhs_loc2D(obj_0_172_in,2,2). 
lhs_rot2D(obj_0_172_in,rot90). 
lhs_pen_color(obj_0_172_in,silver). 
lhs_rotSize2D(obj_0_172_in,6,1). 
lhs_vis2D(obj_0_172_in,1,6). 
lhs_mass(obj_0_172_in,6). 
lhs_iz_sid(obj_0_172_in,s11156184). 
rhs_peice(trn_0,obj_0_571_out). 
rhs_loc2D(obj_0_571_out,8,5). 
rhs_rot2D(obj_0_571_out,rot90). 
rhs_pen_color(obj_0_571_out,red). 
rhs_rotSize2D(obj_0_571_out,3,1). 
rhs_vis2D(obj_0_571_out,1,3). 
rhs_mass(obj_0_571_out,3). 
rhs_iz_sid(obj_0_571_out,sid_13). 
rhs_peice(trn_0,obj_0_353_out). 
rhs_loc2D(obj_0_353_out,8,5). 
rhs_rot2D(obj_0_353_out,sameR). 
rhs_pen_color(obj_0_353_out,black). 
rhs_rotSize2D(obj_0_353_out,1,3). 
rhs_vis2D(obj_0_353_out,1,3). 
rhs_mass(obj_0_353_out,3). 
rhs_iz_sid(obj_0_353_out,s4971157). 
rhs_peice(trn_0,obj_0_147_out). 
rhs_loc2D(obj_0_147_out,5,3). 
rhs_rot2D(obj_0_147_out,rot90). 
rhs_pen_color(obj_0_147_out,yellow). 
rhs_rotSize2D(obj_0_147_out,5,1). 
rhs_vis2D(obj_0_147_out,1,5). 
rhs_mass(obj_0_147_out,5). 
rhs_iz_sid(obj_0_147_out,s1265795). 
rhs_peice(trn_0,obj_0_225_out). 
rhs_loc2D(obj_0_225_out,5,3). 
rhs_rot2D(obj_0_225_out,sameR). 
rhs_pen_color(obj_0_225_out,black). 
rhs_rotSize2D(obj_0_225_out,1,5). 
rhs_vis2D(obj_0_225_out,1,5). 
rhs_mass(obj_0_225_out,5). 
rhs_iz_sid(obj_0_225_out,s10190386). 
rhs_peice(trn_0,obj_0_432_out). 
rhs_loc2D(obj_0_432_out,2,2). 
rhs_rot2D(obj_0_432_out,rot90). 
rhs_pen_color(obj_0_432_out,blue). 
rhs_rotSize2D(obj_0_432_out,6,1). 
rhs_vis2D(obj_0_432_out,1,6). 
rhs_mass(obj_0_432_out,6). 
rhs_iz_sid(obj_0_432_out,s11156184). 
rhs_peice(trn_0,obj_0_684_out). 
rhs_loc2D(obj_0_684_out,2,2). 
rhs_rot2D(obj_0_684_out,sameR). 
rhs_pen_color(obj_0_684_out,black). 
rhs_rotSize2D(obj_0_684_out,1,6). 
rhs_vis2D(obj_0_684_out,1,6). 
rhs_mass(obj_0_684_out,6). 
rhs_iz_sid(obj_0_684_out,s11320327). 
end(model(trn_0)). 
begin(model(trn_1)). 
lhs_peice(trn_1,obj_1_200_in). 
lhs_loc2D(obj_1_200_in,8,6). 
lhs_rot2D(obj_1_200_in,rot90). 
lhs_pen_color(obj_1_200_in,silver). 
lhs_rotSize2D(obj_1_200_in,2,1). 
lhs_vis2D(obj_1_200_in,1,2). 
lhs_mass(obj_1_200_in,2). 
lhs_iz_sid(obj_1_200_in,sid_12). 
lhs_peice(trn_1,obj_1_701_in). 
lhs_loc2D(obj_1_701_in,2,4). 
lhs_rot2D(obj_1_701_in,rot90). 
lhs_pen_color(obj_1_701_in,silver). 
lhs_rotSize2D(obj_1_701_in,4,1). 
lhs_vis2D(obj_1_701_in,1,4). 
lhs_mass(obj_1_701_in,4). 
lhs_iz_sid(obj_1_701_in,sid_14). 
lhs_peice(trn_1,obj_1_427_in). 
lhs_loc2D(obj_1_427_in,5,2). 
lhs_rot2D(obj_1_427_in,rot90). 
lhs_pen_color(obj_1_427_in,silver). 
lhs_rotSize2D(obj_1_427_in,6,1). 
lhs_vis2D(obj_1_427_in,1,6). 
lhs_mass(obj_1_427_in,6). 
lhs_iz_sid(obj_1_427_in,s11156184). 
rhs_peice(trn_1,obj_1_571_out). 
rhs_loc2D(obj_1_571_out,8,5). 
rhs_rot2D(obj_1_571_out,rot90). 
rhs_pen_color(obj_1_571_out,red). 
rhs_rotSize2D(obj_1_571_out,3,1). 
rhs_vis2D(obj_1_571_out,1,3). 
rhs_mass(obj_1_571_out,3). 
rhs_iz_sid(obj_1_571_out,sid_13). 
rhs_peice(trn_1,obj_1_353_out). 
rhs_loc2D(obj_1_353_out,8,5). 
rhs_rot2D(obj_1_353_out,sameR). 
rhs_pen_color(obj_1_353_out,black). 
rhs_rotSize2D(obj_1_353_out,1,3). 
rhs_vis2D(obj_1_353_out,1,3). 
rhs_mass(obj_1_353_out,3). 
rhs_iz_sid(obj_1_353_out,s4971157). 
rhs_peice(trn_1,obj_1_147_out). 
rhs_loc2D(obj_1_147_out,5,3). 
rhs_rot2D(obj_1_147_out,rot90). 
rhs_pen_color(obj_1_147_out,yellow). 
rhs_rotSize2D(obj_1_147_out,5,1). 
rhs_vis2D(obj_1_147_out,1,5). 
rhs_mass(obj_1_147_out,5). 
rhs_iz_sid(obj_1_147_out,s1265795). 
rhs_peice(trn_1,obj_1_225_out). 
rhs_loc2D(obj_1_225_out,5,3). 
rhs_rot2D(obj_1_225_out,sameR). 
rhs_pen_color(obj_1_225_out,black). 
rhs_rotSize2D(obj_1_225_out,1,5). 
rhs_vis2D(obj_1_225_out,1,5). 
rhs_mass(obj_1_225_out,5). 
rhs_iz_sid(obj_1_225_out,s10190386). 
rhs_peice(trn_1,obj_1_432_out). 
rhs_loc2D(obj_1_432_out,2,2). 
rhs_rot2D(obj_1_432_out,rot90). 
rhs_pen_color(obj_1_432_out,blue). 
rhs_rotSize2D(obj_1_432_out,6,1). 
rhs_vis2D(obj_1_432_out,1,6). 
rhs_mass(obj_1_432_out,6). 
rhs_iz_sid(obj_1_432_out,s11156184). 
rhs_peice(trn_1,obj_1_684_out). 
rhs_loc2D(obj_1_684_out,2,2). 
rhs_rot2D(obj_1_684_out,sameR). 
rhs_pen_color(obj_1_684_out,black). 
rhs_rotSize2D(obj_1_684_out,1,6). 
rhs_vis2D(obj_1_684_out,1,6). 
rhs_mass(obj_1_684_out,6). 
rhs_iz_sid(obj_1_684_out,s11320327). 
end(model(trn_1)). 
begin(model(trn_2)). 
lhs_peice(trn_2,obj_2_536_in). 
lhs_loc2D(obj_2_536_in,3,8). 
lhs_rot2D(obj_2_536_in,sameR). 
lhs_pen_color(obj_2_536_in,silver). 
lhs_rotSize2D(obj_2_536_in,3,1). 
lhs_vis2D(obj_2_536_in,3,1). 
lhs_mass(obj_2_536_in,3). 
lhs_iz_sid(obj_2_536_in,sid_13). 
lhs_peice(trn_2,obj_2_670_in). 
lhs_loc2D(obj_2_670_in,8,4). 
lhs_rot2D(obj_2_670_in,rot90). 
lhs_pen_color(obj_2_670_in,silver). 
lhs_rotSize2D(obj_2_670_in,5,1). 
lhs_vis2D(obj_2_670_in,1,5). 
lhs_mass(obj_2_670_in,5). 
lhs_iz_sid(obj_2_670_in,s1265795). 
lhs_peice(trn_2,obj_2_127_in). 
lhs_loc2D(obj_2_127_in,1,3). 
lhs_rot2D(obj_2_127_in,sameR). 
lhs_pen_color(obj_2_127_in,silver). 
lhs_rotSize2D(obj_2_127_in,6,1). 
lhs_vis2D(obj_2_127_in,6,1). 
lhs_mass(obj_2_127_in,6). 
lhs_iz_sid(obj_2_127_in,s11156184). 
rhs_peice(trn_2,obj_2_571_out). 
rhs_loc2D(obj_2_571_out,8,5). 
rhs_rot2D(obj_2_571_out,rot90). 
rhs_pen_color(obj_2_571_out,red). 
rhs_rotSize2D(obj_2_571_out,3,1). 
rhs_vis2D(obj_2_571_out,1,3). 
rhs_mass(obj_2_571_out,3). 
rhs_iz_sid(obj_2_571_out,sid_13). 
rhs_peice(trn_2,obj_2_353_out). 
rhs_loc2D(obj_2_353_out,8,5). 
rhs_rot2D(obj_2_353_out,sameR). 
rhs_pen_color(obj_2_353_out,black). 
rhs_rotSize2D(obj_2_353_out,1,3). 
rhs_vis2D(obj_2_353_out,1,3). 
rhs_mass(obj_2_353_out,3). 
rhs_iz_sid(obj_2_353_out,s4971157). 
rhs_peice(trn_2,obj_2_147_out). 
rhs_loc2D(obj_2_147_out,5,3). 
rhs_rot2D(obj_2_147_out,rot90). 
rhs_pen_color(obj_2_147_out,yellow). 
rhs_rotSize2D(obj_2_147_out,5,1). 
rhs_vis2D(obj_2_147_out,1,5). 
rhs_mass(obj_2_147_out,5). 
rhs_iz_sid(obj_2_147_out,s1265795). 
rhs_peice(trn_2,obj_2_225_out). 
rhs_loc2D(obj_2_225_out,5,3). 
rhs_rot2D(obj_2_225_out,sameR). 
rhs_pen_color(obj_2_225_out,black). 
rhs_rotSize2D(obj_2_225_out,1,5). 
rhs_vis2D(obj_2_225_out,1,5). 
rhs_mass(obj_2_225_out,5). 
rhs_iz_sid(obj_2_225_out,s10190386). 
rhs_peice(trn_2,obj_2_432_out). 
rhs_loc2D(obj_2_432_out,2,2). 
rhs_rot2D(obj_2_432_out,rot90). 
rhs_pen_color(obj_2_432_out,blue). 
rhs_rotSize2D(obj_2_432_out,6,1). 
rhs_vis2D(obj_2_432_out,1,6). 
rhs_mass(obj_2_432_out,6). 
rhs_iz_sid(obj_2_432_out,s11156184). 
rhs_peice(trn_2,obj_2_684_out). 
rhs_loc2D(obj_2_684_out,2,2). 
rhs_rot2D(obj_2_684_out,sameR). 
rhs_pen_color(obj_2_684_out,black). 
rhs_rotSize2D(obj_2_684_out,1,6). 
rhs_vis2D(obj_2_684_out,1,6). 
rhs_mass(obj_2_684_out,6). 
rhs_iz_sid(obj_2_684_out,s11320327). 
end(model(trn_2)). 
begin(model(trn_3)). 
lhs_peice(trn_3,obj_3_13_in). 
lhs_loc2D(obj_3_13_in,2,3). 
lhs_rot2D(obj_3_13_in,sameR). 
lhs_pen_color(obj_3_13_in,silver). 
lhs_rotSize2D(obj_3_13_in,4,1). 
lhs_vis2D(obj_3_13_in,4,1). 
lhs_mass(obj_3_13_in,4). 
lhs_iz_sid(obj_3_13_in,sid_14). 
lhs_peice(trn_3,obj_3_262_in). 
lhs_loc2D(obj_3_262_in,2,6). 
lhs_rot2D(obj_3_262_in,sameR). 
lhs_pen_color(obj_3_262_in,silver). 
lhs_rotSize2D(obj_3_262_in,5,1). 
lhs_vis2D(obj_3_262_in,5,1). 
lhs_mass(obj_3_262_in,5). 
lhs_iz_sid(obj_3_262_in,s1265795). 
lhs_peice(trn_3,obj_3_602_in). 
lhs_loc2D(obj_3_602_in,8,2). 
lhs_rot2D(obj_3_602_in,rot90). 
lhs_pen_color(obj_3_602_in,silver). 
lhs_rotSize2D(obj_3_602_in,7,1). 
lhs_vis2D(obj_3_602_in,1,7). 
lhs_mass(obj_3_602_in,7). 
lhs_iz_sid(obj_3_602_in,s2476004). 
rhs_peice(trn_3,obj_3_571_out). 
rhs_loc2D(obj_3_571_out,8,5). 
rhs_rot2D(obj_3_571_out,rot90). 
rhs_pen_color(obj_3_571_out,red). 
rhs_rotSize2D(obj_3_571_out,3,1). 
rhs_vis2D(obj_3_571_out,1,3). 
rhs_mass(obj_3_571_out,3). 
rhs_iz_sid(obj_3_571_out,sid_13). 
rhs_peice(trn_3,obj_3_353_out). 
rhs_loc2D(obj_3_353_out,8,5). 
rhs_rot2D(obj_3_353_out,sameR). 
rhs_pen_color(obj_3_353_out,black). 
rhs_rotSize2D(obj_3_353_out,1,3). 
rhs_vis2D(obj_3_353_out,1,3). 
rhs_mass(obj_3_353_out,3). 
rhs_iz_sid(obj_3_353_out,s4971157). 
rhs_peice(trn_3,obj_3_147_out). 
rhs_loc2D(obj_3_147_out,5,3). 
rhs_rot2D(obj_3_147_out,rot90). 
rhs_pen_color(obj_3_147_out,yellow). 
rhs_rotSize2D(obj_3_147_out,5,1). 
rhs_vis2D(obj_3_147_out,1,5). 
rhs_mass(obj_3_147_out,5). 
rhs_iz_sid(obj_3_147_out,s1265795). 
rhs_peice(trn_3,obj_3_225_out). 
rhs_loc2D(obj_3_225_out,5,3). 
rhs_rot2D(obj_3_225_out,sameR). 
rhs_pen_color(obj_3_225_out,black). 
rhs_rotSize2D(obj_3_225_out,1,5). 
rhs_vis2D(obj_3_225_out,1,5). 
rhs_mass(obj_3_225_out,5). 
rhs_iz_sid(obj_3_225_out,s10190386). 
rhs_peice(trn_3,obj_3_432_out). 
rhs_loc2D(obj_3_432_out,2,2). 
rhs_rot2D(obj_3_432_out,rot90). 
rhs_pen_color(obj_3_432_out,blue). 
rhs_rotSize2D(obj_3_432_out,6,1). 
rhs_vis2D(obj_3_432_out,1,6). 
rhs_mass(obj_3_432_out,6). 
rhs_iz_sid(obj_3_432_out,s11156184). 
rhs_peice(trn_3,obj_3_684_out). 
rhs_loc2D(obj_3_684_out,2,2). 
rhs_rot2D(obj_3_684_out,sameR). 
rhs_pen_color(obj_3_684_out,black). 
rhs_rotSize2D(obj_3_684_out,1,6). 
rhs_vis2D(obj_3_684_out,1,6). 
rhs_mass(obj_3_684_out,6). 
rhs_iz_sid(obj_3_684_out,s11320327). 
end(model(trn_3)). 
/*
begin(model(tst_0)). 
lhs_peice(tst_0,obj_0_701_in). 
lhs_loc2D(obj_0_701_in,4,2). 
lhs_rot2D(obj_0_701_in,rot90). 
lhs_pen_color(obj_0_701_in,silver). 
lhs_rotSize2D(obj_0_701_in,3,1). 
lhs_vis2D(obj_0_701_in,1,3). 
lhs_mass(obj_0_701_in,3). 
lhs_iz_sid(obj_0_701_in,sid_13). 
lhs_peice(tst_0,obj_0_8_in). 
lhs_loc2D(obj_0_8_in,1,9). 
lhs_rot2D(obj_0_8_in,sameR). 
lhs_pen_color(obj_0_8_in,silver). 
lhs_rotSize2D(obj_0_8_in,5,1). 
lhs_vis2D(obj_0_8_in,5,1). 
lhs_mass(obj_0_8_in,5). 
lhs_iz_sid(obj_0_8_in,s1265795). 
lhs_peice(tst_0,obj_0_341_in). 
lhs_loc2D(obj_0_341_in,5,6). 
lhs_rot2D(obj_0_341_in,sameR). 
lhs_pen_color(obj_0_341_in,silver). 
lhs_rotSize2D(obj_0_341_in,6,1). 
lhs_vis2D(obj_0_341_in,6,1). 
lhs_mass(obj_0_341_in,6). 
lhs_iz_sid(obj_0_341_in,s11156184). 
rhs_peice(tst_0,obj_0_212_out). 
rhs_loc2D(obj_0_212_out,4,2). 
rhs_rot2D(obj_0_212_out,rot90). 
rhs_pen_color(obj_0_212_out,red). 
rhs_rotSize2D(obj_0_212_out,3,1). 
rhs_vis2D(obj_0_212_out,1,3). 
rhs_mass(obj_0_212_out,3). 
rhs_iz_sid(obj_0_212_out,sid_13). 
rhs_peice(tst_0,obj_0_418_out). 
rhs_loc2D(obj_0_418_out,1,9). 
rhs_rot2D(obj_0_418_out,sameR). 
rhs_pen_color(obj_0_418_out,yellow). 
rhs_rotSize2D(obj_0_418_out,5,1). 
rhs_vis2D(obj_0_418_out,5,1). 
rhs_mass(obj_0_418_out,5). 
rhs_iz_sid(obj_0_418_out,s1265795). 
rhs_peice(tst_0,obj_0_648_out). 
rhs_loc2D(obj_0_648_out,5,6). 
rhs_rot2D(obj_0_648_out,sameR). 
rhs_pen_color(obj_0_648_out,blue). 
rhs_rotSize2D(obj_0_648_out,6,1). 
rhs_vis2D(obj_0_648_out,6,1). 
rhs_mass(obj_0_648_out,6). 
rhs_iz_sid(obj_0_648_out,s11156184). 
end(model(tst_0)). 
*/
/*
*/