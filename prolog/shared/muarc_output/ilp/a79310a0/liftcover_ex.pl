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
lhs(trn_0,hv(1,1),sameR,cyan,hv(2,2),hv(2,2),4,sid_22). 
lhs(trn_1,hv(2,1),sameR,cyan,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_1,hv(2,2),sameR,red,hv(1,1),hv(1,1),1,sid_11). 
lhs(trn_2,hv(2,2),sameR,cyan,hv(3,1),hv(3,1),3,sid_13). 
/*
lhs(tst_0,hv(2,1),rot90,cyan,hv(3,2),hv(2,3),4,s15307279). 
*/
begin(model(trn_0)). 
lhs_peice(trn_0,obj_0_481_in). 
lhs_loc2D(obj_0_481_in,1,1). 
lhs_rot2D(obj_0_481_in,sameR). 
lhs_pen_color(obj_0_481_in,cyan). 
lhs_rotSize2D(obj_0_481_in,2,2). 
lhs_vis2D(obj_0_481_in,2,2). 
lhs_mass(obj_0_481_in,4). 
lhs_iz_sid(obj_0_481_in,sid_22). 
rhs_peice(trn_0,obj_0_720_out). 
rhs_loc2D(obj_0_720_out,1,2). 
rhs_rot2D(obj_0_720_out,sameR). 
rhs_pen_color(obj_0_720_out,red). 
rhs_rotSize2D(obj_0_720_out,2,2). 
rhs_vis2D(obj_0_720_out,2,2). 
rhs_mass(obj_0_720_out,4). 
rhs_iz_sid(obj_0_720_out,sid_22). 
end(model(trn_0)). 
begin(model(trn_1)). 
lhs_peice(trn_1,obj_1_592_in). 
lhs_loc2D(obj_1_592_in,2,1). 
lhs_rot2D(obj_1_592_in,sameR). 
lhs_pen_color(obj_1_592_in,cyan). 
lhs_rotSize2D(obj_1_592_in,1,1). 
lhs_vis2D(obj_1_592_in,1,1). 
lhs_mass(obj_1_592_in,1). 
lhs_iz_sid(obj_1_592_in,sid_11). 
lhs_peice(trn_1,obj_0_40_in). 
lhs_loc2D(obj_0_40_in,2,2). 
lhs_rot2D(obj_0_40_in,sameR). 
lhs_pen_color(obj_0_40_in,red). 
lhs_rotSize2D(obj_0_40_in,1,1). 
lhs_vis2D(obj_0_40_in,1,1). 
lhs_mass(obj_0_40_in,1). 
lhs_iz_sid(obj_0_40_in,sid_11). 
end(model(trn_1)). 
begin(model(trn_2)). 
lhs_peice(trn_2,obj_2_414_in). 
lhs_loc2D(obj_2_414_in,2,2). 
lhs_rot2D(obj_2_414_in,sameR). 
lhs_pen_color(obj_2_414_in,cyan). 
lhs_rotSize2D(obj_2_414_in,3,1). 
lhs_vis2D(obj_2_414_in,3,1). 
lhs_mass(obj_2_414_in,3). 
lhs_iz_sid(obj_2_414_in,sid_13). 
rhs_peice(trn_2,obj_2_726_out). 
rhs_loc2D(obj_2_726_out,2,3). 
rhs_rot2D(obj_2_726_out,sameR). 
rhs_pen_color(obj_2_726_out,red). 
rhs_rotSize2D(obj_2_726_out,3,1). 
rhs_vis2D(obj_2_726_out,3,1). 
rhs_mass(obj_2_726_out,3). 
rhs_iz_sid(obj_2_726_out,sid_13). 
end(model(trn_2)). 
/*
begin(model(tst_0)). 
lhs_peice(tst_0,obj_0_673_in). 
lhs_loc2D(obj_0_673_in,2,1). 
lhs_rot2D(obj_0_673_in,rot90). 
lhs_pen_color(obj_0_673_in,cyan). 
lhs_rotSize2D(obj_0_673_in,3,2). 
lhs_vis2D(obj_0_673_in,2,3). 
lhs_mass(obj_0_673_in,4). 
lhs_iz_sid(obj_0_673_in,s15307279). 
rhs_peice(tst_0,obj_0_517_out). 
rhs_loc2D(obj_0_517_out,2,2). 
rhs_rot2D(obj_0_517_out,rot90). 
rhs_pen_color(obj_0_517_out,red). 
rhs_rotSize2D(obj_0_517_out,3,2). 
rhs_vis2D(obj_0_517_out,2,3). 
rhs_mass(obj_0_517_out,4). 
rhs_iz_sid(obj_0_517_out,s15307279). 
end(model(tst_0)). 
*/
/*
*/
