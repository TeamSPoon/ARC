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
modeh(*,rhs(+peice,+rhs,+rhs,+ #(color),+rhs,+rhs,+rhs,+rhs)). 
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
lhs(trn_0,hv(9,7),rot90,silver,hv(3,1),hv(1,3),3,sid_13). 
lhs(trn_0,hv(5,6),rot90,silver,hv(4,1),hv(1,4),4,sid_14). 
lhs(trn_0,hv(1,4),rot90,silver,hv(6,1),hv(1,6),6,s11156184). 
lhs(trn_0,hv(7,3),rot90,silver,hv(7,1),hv(1,7),7,s2476004). 
lhs(trn_0,hv(3,2),rot90,silver,hv(8,1),hv(1,8),8,s16658777). 
lhs(trn_1,hv(3,8),rot90,silver,hv(2,1),hv(1,2),2,sid_12). 
lhs(trn_1,hv(7,4),rot90,silver,hv(6,1),hv(1,6),6,s11156184). 
lhs(trn_1,hv(1,3),rot90,silver,hv(7,1),hv(1,7),7,s2476004). 
lhs(trn_1,hv(5,1),rot90,silver,hv(9,1),hv(1,9),9,s13578114). 
lhs(trn_1,hv(1,1),flipH,black,hv(4,9),hv(4,9),27,s9474680). 
lhs(trn_1,hv(6,1),flipH,black,hv(4,9),hv(4,9),30,s4370719). 
/*
lhs(tst_0,hv(6,5),rot90,silver,hv(5,1),hv(1,5),5,s1265795). 
lhs(tst_0,hv(4,3),rot90,silver,hv(7,1),hv(1,7),7,s2476004). 
lhs(tst_0,hv(8,2),rot90,silver,hv(8,1),hv(1,8),8,s16658777). 
lhs(tst_0,hv(2,9),sameR,silver,hv(1,1),hv(1,1),1,sid_11). 
*/
begin(model(trn_0)). 
lhs_peice(trn_0,obj_0_30_in). 
lhs_loc2D(obj_0_30_in,9,7). 
lhs_rot2D(obj_0_30_in,rot90). 
lhs_pen_color(obj_0_30_in,silver). 
lhs_rotSize2D(obj_0_30_in,3,1). 
lhs_vis2D(obj_0_30_in,1,3). 
lhs_mass(obj_0_30_in,3). 
lhs_iz_sid(obj_0_30_in,sid_13). 
lhs_peice(trn_0,obj_0_50_in). 
lhs_loc2D(obj_0_50_in,5,6). 
lhs_rot2D(obj_0_50_in,rot90). 
lhs_pen_color(obj_0_50_in,silver). 
lhs_rotSize2D(obj_0_50_in,4,1). 
lhs_vis2D(obj_0_50_in,1,4). 
lhs_mass(obj_0_50_in,4). 
lhs_iz_sid(obj_0_50_in,sid_14). 
lhs_peice(trn_0,obj_0_800_in). 
lhs_loc2D(obj_0_800_in,1,4). 
lhs_rot2D(obj_0_800_in,rot90). 
lhs_pen_color(obj_0_800_in,silver). 
lhs_rotSize2D(obj_0_800_in,6,1). 
lhs_vis2D(obj_0_800_in,1,6). 
lhs_mass(obj_0_800_in,6). 
lhs_iz_sid(obj_0_800_in,s11156184). 
lhs_peice(trn_0,obj_0_216_in). 
lhs_loc2D(obj_0_216_in,7,3). 
lhs_rot2D(obj_0_216_in,rot90). 
lhs_pen_color(obj_0_216_in,silver). 
lhs_rotSize2D(obj_0_216_in,7,1). 
lhs_vis2D(obj_0_216_in,1,7). 
lhs_mass(obj_0_216_in,7). 
lhs_iz_sid(obj_0_216_in,s2476004). 
lhs_peice(trn_0,obj_0_243_in). 
lhs_loc2D(obj_0_243_in,3,2). 
lhs_rot2D(obj_0_243_in,rot90). 
lhs_pen_color(obj_0_243_in,silver). 
lhs_rotSize2D(obj_0_243_in,8,1). 
lhs_vis2D(obj_0_243_in,1,8). 
lhs_mass(obj_0_243_in,8). 
lhs_iz_sid(obj_0_243_in,s16658777). 
rhs_peice(trn_0,obj_0_386_out). 
rhs_loc2D(obj_0_386_out,9,7). 
rhs_rot2D(obj_0_386_out,rot90). 
rhs_pen_color(obj_0_386_out,red). 
rhs_rotSize2D(obj_0_386_out,3,1). 
rhs_vis2D(obj_0_386_out,1,3). 
rhs_mass(obj_0_386_out,3). 
rhs_iz_sid(obj_0_386_out,sid_13). 
rhs_peice(trn_0,obj_0_749_out). 
rhs_loc2D(obj_0_749_out,3,2). 
rhs_rot2D(obj_0_749_out,rot90). 
rhs_pen_color(obj_0_749_out,blue). 
rhs_rotSize2D(obj_0_749_out,8,1). 
rhs_vis2D(obj_0_749_out,1,8). 
rhs_mass(obj_0_749_out,8). 
rhs_iz_sid(obj_0_749_out,s16658777). 
end(model(trn_0)). 
begin(model(trn_1)). 
lhs_peice(trn_1,obj_1_506_in). 
lhs_loc2D(obj_1_506_in,3,8). 
lhs_rot2D(obj_1_506_in,rot90). 
lhs_pen_color(obj_1_506_in,silver). 
lhs_rotSize2D(obj_1_506_in,2,1). 
lhs_vis2D(obj_1_506_in,1,2). 
lhs_mass(obj_1_506_in,2). 
lhs_iz_sid(obj_1_506_in,sid_12). 
lhs_peice(trn_1,obj_1_471_in). 
lhs_loc2D(obj_1_471_in,7,4). 
lhs_rot2D(obj_1_471_in,rot90). 
lhs_pen_color(obj_1_471_in,silver). 
lhs_rotSize2D(obj_1_471_in,6,1). 
lhs_vis2D(obj_1_471_in,1,6). 
lhs_mass(obj_1_471_in,6). 
lhs_iz_sid(obj_1_471_in,s11156184). 
lhs_peice(trn_1,obj_1_356_in). 
lhs_loc2D(obj_1_356_in,1,3). 
lhs_rot2D(obj_1_356_in,rot90). 
lhs_pen_color(obj_1_356_in,silver). 
lhs_rotSize2D(obj_1_356_in,7,1). 
lhs_vis2D(obj_1_356_in,1,7). 
lhs_mass(obj_1_356_in,7). 
lhs_iz_sid(obj_1_356_in,s2476004). 
lhs_peice(trn_1,obj_1_218_in). 
lhs_loc2D(obj_1_218_in,5,1). 
lhs_rot2D(obj_1_218_in,rot90). 
lhs_pen_color(obj_1_218_in,silver). 
lhs_rotSize2D(obj_1_218_in,9,1). 
lhs_vis2D(obj_1_218_in,1,9). 
lhs_mass(obj_1_218_in,9). 
lhs_iz_sid(obj_1_218_in,s13578114). 
lhs_peice(trn_1,obj_1_566_in). 
lhs_loc2D(obj_1_566_in,1,1). 
lhs_rot2D(obj_1_566_in,flipH). 
lhs_pen_color(obj_1_566_in,black). 
lhs_rotSize2D(obj_1_566_in,4,9). 
lhs_vis2D(obj_1_566_in,4,9). 
lhs_mass(obj_1_566_in,27). 
lhs_iz_sid(obj_1_566_in,s9474680). 
lhs_peice(trn_1,obj_1_259_in). 
lhs_loc2D(obj_1_259_in,6,1). 
lhs_rot2D(obj_1_259_in,flipH). 
lhs_pen_color(obj_1_259_in,black). 
lhs_rotSize2D(obj_1_259_in,4,9). 
lhs_vis2D(obj_1_259_in,4,9). 
lhs_mass(obj_1_259_in,30). 
lhs_iz_sid(obj_1_259_in,s4370719). 
rhs_peice(trn_1,obj_1_647_out). 
rhs_loc2D(obj_1_647_out,3,8). 
rhs_rot2D(obj_1_647_out,rot90). 
rhs_pen_color(obj_1_647_out,red). 
rhs_rotSize2D(obj_1_647_out,2,1). 
rhs_vis2D(obj_1_647_out,1,2). 
rhs_mass(obj_1_647_out,2). 
rhs_iz_sid(obj_1_647_out,sid_12). 
rhs_peice(trn_1,obj_1_90_out). 
rhs_loc2D(obj_1_90_out,5,1). 
rhs_rot2D(obj_1_90_out,rot90). 
rhs_pen_color(obj_1_90_out,blue). 
rhs_rotSize2D(obj_1_90_out,9,1). 
rhs_vis2D(obj_1_90_out,1,9). 
rhs_mass(obj_1_90_out,9). 
rhs_iz_sid(obj_1_90_out,s13578114). 
rhs_peice(trn_1,obj_1_535_out). 
rhs_loc2D(obj_1_535_out,1,1). 
rhs_rot2D(obj_1_535_out,sameR). 
rhs_pen_color(obj_1_535_out,black). 
rhs_rotSize2D(obj_1_535_out,4,9). 
rhs_vis2D(obj_1_535_out,4,9). 
rhs_mass(obj_1_535_out,34). 
rhs_iz_sid(obj_1_535_out,s13441571). 
rhs_peice(trn_1,obj_1_451_out). 
rhs_loc2D(obj_1_451_out,6,1). 
rhs_rot2D(obj_1_451_out,sameR). 
rhs_pen_color(obj_1_451_out,black). 
rhs_rotSize2D(obj_1_451_out,4,9). 
rhs_vis2D(obj_1_451_out,4,9). 
rhs_mass(obj_1_451_out,36). 
rhs_iz_sid(obj_1_451_out,s2245337). 
end(model(trn_1)). 
/*
begin(model(tst_0)). 
lhs_peice(tst_0,obj_0_435_in). 
lhs_loc2D(obj_0_435_in,6,5). 
lhs_rot2D(obj_0_435_in,rot90). 
lhs_pen_color(obj_0_435_in,silver). 
lhs_rotSize2D(obj_0_435_in,5,1). 
lhs_vis2D(obj_0_435_in,1,5). 
lhs_mass(obj_0_435_in,5). 
lhs_iz_sid(obj_0_435_in,s1265795). 
lhs_peice(tst_0,obj_0_610_in). 
lhs_loc2D(obj_0_610_in,4,3). 
lhs_rot2D(obj_0_610_in,rot90). 
lhs_pen_color(obj_0_610_in,silver). 
lhs_rotSize2D(obj_0_610_in,7,1). 
lhs_vis2D(obj_0_610_in,1,7). 
lhs_mass(obj_0_610_in,7). 
lhs_iz_sid(obj_0_610_in,s2476004). 
lhs_peice(tst_0,obj_0_420_in). 
lhs_loc2D(obj_0_420_in,8,2). 
lhs_rot2D(obj_0_420_in,rot90). 
lhs_pen_color(obj_0_420_in,silver). 
lhs_rotSize2D(obj_0_420_in,8,1). 
lhs_vis2D(obj_0_420_in,1,8). 
lhs_mass(obj_0_420_in,8). 
lhs_iz_sid(obj_0_420_in,s16658777). 
lhs_loc2D(obj_0_435_in,2,9). 
lhs_rot2D(obj_0_435_in,sameR). 
lhs_rotSize2D(obj_0_435_in,1,1). 
lhs_vis2D(obj_0_435_in,1,1). 
lhs_mass(obj_0_435_in,1). 
lhs_iz_sid(obj_0_435_in,sid_11). 
rhs_peice(tst_0,obj_0_706_out). 
rhs_loc2D(obj_0_706_out,8,2). 
rhs_rot2D(obj_0_706_out,rot90). 
rhs_pen_color(obj_0_706_out,blue). 
rhs_rotSize2D(obj_0_706_out,8,1). 
rhs_vis2D(obj_0_706_out,1,8). 
rhs_mass(obj_0_706_out,8). 
rhs_iz_sid(obj_0_706_out,s16658777). 
rhs_peice(tst_0,obj_0_75_out). 
rhs_loc2D(obj_0_75_out,2,9). 
rhs_rot2D(obj_0_75_out,sameR). 
rhs_pen_color(obj_0_75_out,red). 
rhs_rotSize2D(obj_0_75_out,1,1). 
rhs_vis2D(obj_0_75_out,1,1). 
rhs_mass(obj_0_75_out,1). 
rhs_iz_sid(obj_0_75_out,sid_11). 
end(model(tst_0)). 
*/
/*
*/
