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
lhs(trn_0,hv(7,2),rot180,cyan,hv(2,2),hv(2,2),3,s11261491). 
lhs(trn_0,hv(4,10),rot270,green,hv(2,2),hv(2,2),3,s11261491). 
lhs(trn_0,hv(2,4),rot90,red,hv(2,2),hv(2,2),3,s11261491). 
lhs(trn_0,hv(8,8),sameR,blue,hv(2,2),hv(2,2),3,s11261491). 
lhs(trn_1,hv(3,4),rot180,blue,hv(2,2),hv(2,2),3,s11261491). 
lhs(trn_1,hv(5,10),rot270,yellow,hv(2,2),hv(2,2),3,s11261491). 
lhs(trn_1,hv(9,2),rot90,cyan,hv(2,2),hv(2,2),3,s11261491). 
lhs(trn_1,hv(8,6),sameR,red,hv(2,2),hv(2,2),3,s11261491). 
/*
lhs(tst_0,hv(3,10),rot180,green,hv(2,2),hv(2,2),3,s11261491). 
lhs(tst_0,hv(7,7),rot270,blue,hv(2,2),hv(2,2),3,s11261491). 
lhs(tst_0,hv(11,3),rot90,cyan,hv(2,2),hv(2,2),3,s11261491). 
lhs(tst_0,hv(3,3),sameR,purple,hv(2,2),hv(2,2),3,s11261491). 
*/
begin(model(trn_0)). 
lhs_peice(trn_0,obj_0_556_in). 
lhs_loc2D(obj_0_556_in,7,2). 
lhs_rot2D(obj_0_556_in,rot180). 
lhs_pen_color(obj_0_556_in,cyan). 
lhs_rotSize2D(obj_0_556_in,2,2). 
lhs_vis2D(obj_0_556_in,2,2). 
lhs_mass(obj_0_556_in,3). 
lhs_iz_sid(obj_0_556_in,s11261491). 
lhs_peice(trn_0,obj_0_316_in). 
lhs_loc2D(obj_0_316_in,4,10). 
lhs_rot2D(obj_0_316_in,rot270). 
lhs_pen_color(obj_0_316_in,green). 
lhs_rotSize2D(obj_0_316_in,2,2). 
lhs_vis2D(obj_0_316_in,2,2). 
lhs_mass(obj_0_316_in,3). 
lhs_iz_sid(obj_0_316_in,s11261491). 
lhs_peice(trn_0,obj_0_516_in). 
lhs_loc2D(obj_0_516_in,2,4). 
lhs_rot2D(obj_0_516_in,rot90). 
lhs_pen_color(obj_0_516_in,red). 
lhs_rotSize2D(obj_0_516_in,2,2). 
lhs_vis2D(obj_0_516_in,2,2). 
lhs_mass(obj_0_516_in,3). 
lhs_iz_sid(obj_0_516_in,s11261491). 
lhs_peice(trn_0,obj_0_271_in). 
lhs_loc2D(obj_0_271_in,8,8). 
lhs_rot2D(obj_0_271_in,sameR). 
lhs_pen_color(obj_0_271_in,blue). 
lhs_rotSize2D(obj_0_271_in,2,2). 
lhs_vis2D(obj_0_271_in,2,2). 
lhs_mass(obj_0_271_in,3). 
lhs_iz_sid(obj_0_271_in,s11261491). 
rhs_peice(trn_0,obj_0_318_out). 
rhs_loc2D(obj_0_318_out,1,1). 
rhs_rot2D(obj_0_318_out,rot180). 
rhs_pen_color(obj_0_318_out,cyan). 
rhs_rotSize2D(obj_0_318_out,2,2). 
rhs_vis2D(obj_0_318_out,2,2). 
rhs_mass(obj_0_318_out,3). 
rhs_iz_sid(obj_0_318_out,s11261491). 
rhs_peice(trn_0,obj_0_224_out). 
rhs_loc2D(obj_0_224_out,1,3). 
rhs_rot2D(obj_0_224_out,rot270). 
rhs_pen_color(obj_0_224_out,green). 
rhs_rotSize2D(obj_0_224_out,2,2). 
rhs_vis2D(obj_0_224_out,2,2). 
rhs_mass(obj_0_224_out,3). 
rhs_iz_sid(obj_0_224_out,s11261491). 
rhs_peice(trn_0,obj_0_97_out). 
rhs_loc2D(obj_0_97_out,3,1). 
rhs_rot2D(obj_0_97_out,rot90). 
rhs_pen_color(obj_0_97_out,red). 
rhs_rotSize2D(obj_0_97_out,2,2). 
rhs_vis2D(obj_0_97_out,2,2). 
rhs_mass(obj_0_97_out,3). 
rhs_iz_sid(obj_0_97_out,s11261491). 
rhs_peice(trn_0,obj_0_218_out). 
rhs_loc2D(obj_0_218_out,3,3). 
rhs_rot2D(obj_0_218_out,sameR). 
rhs_pen_color(obj_0_218_out,blue). 
rhs_rotSize2D(obj_0_218_out,2,2). 
rhs_vis2D(obj_0_218_out,2,2). 
rhs_mass(obj_0_218_out,3). 
rhs_iz_sid(obj_0_218_out,s11261491). 
end(model(trn_0)). 
begin(model(trn_1)). 
lhs_peice(trn_1,obj_1_243_in). 
lhs_loc2D(obj_1_243_in,3,4). 
lhs_rot2D(obj_1_243_in,rot180). 
lhs_pen_color(obj_1_243_in,blue). 
lhs_rotSize2D(obj_1_243_in,2,2). 
lhs_vis2D(obj_1_243_in,2,2). 
lhs_mass(obj_1_243_in,3). 
lhs_iz_sid(obj_1_243_in,s11261491). 
lhs_peice(trn_1,obj_1_717_in). 
lhs_loc2D(obj_1_717_in,5,10). 
lhs_rot2D(obj_1_717_in,rot270). 
lhs_pen_color(obj_1_717_in,yellow). 
lhs_rotSize2D(obj_1_717_in,2,2). 
lhs_vis2D(obj_1_717_in,2,2). 
lhs_mass(obj_1_717_in,3). 
lhs_iz_sid(obj_1_717_in,s11261491). 
lhs_peice(trn_1,obj_1_739_in). 
lhs_loc2D(obj_1_739_in,9,2). 
lhs_rot2D(obj_1_739_in,rot90). 
lhs_pen_color(obj_1_739_in,cyan). 
lhs_rotSize2D(obj_1_739_in,2,2). 
lhs_vis2D(obj_1_739_in,2,2). 
lhs_mass(obj_1_739_in,3). 
lhs_iz_sid(obj_1_739_in,s11261491). 
lhs_peice(trn_1,obj_1_162_in). 
lhs_loc2D(obj_1_162_in,8,6). 
lhs_rot2D(obj_1_162_in,sameR). 
lhs_pen_color(obj_1_162_in,red). 
lhs_rotSize2D(obj_1_162_in,2,2). 
lhs_vis2D(obj_1_162_in,2,2). 
lhs_mass(obj_1_162_in,3). 
lhs_iz_sid(obj_1_162_in,s11261491). 
rhs_peice(trn_1,obj_1_569_out). 
rhs_loc2D(obj_1_569_out,1,1). 
rhs_rot2D(obj_1_569_out,rot180). 
rhs_pen_color(obj_1_569_out,blue). 
rhs_rotSize2D(obj_1_569_out,2,2). 
rhs_vis2D(obj_1_569_out,2,2). 
rhs_mass(obj_1_569_out,3). 
rhs_iz_sid(obj_1_569_out,s11261491). 
rhs_peice(trn_1,obj_1_89_out). 
rhs_loc2D(obj_1_89_out,1,3). 
rhs_rot2D(obj_1_89_out,rot270). 
rhs_pen_color(obj_1_89_out,yellow). 
rhs_rotSize2D(obj_1_89_out,2,2). 
rhs_vis2D(obj_1_89_out,2,2). 
rhs_mass(obj_1_89_out,3). 
rhs_iz_sid(obj_1_89_out,s11261491). 
rhs_peice(trn_1,obj_1_491_out). 
rhs_loc2D(obj_1_491_out,3,1). 
rhs_rot2D(obj_1_491_out,rot90). 
rhs_pen_color(obj_1_491_out,cyan). 
rhs_rotSize2D(obj_1_491_out,2,2). 
rhs_vis2D(obj_1_491_out,2,2). 
rhs_mass(obj_1_491_out,3). 
rhs_iz_sid(obj_1_491_out,s11261491). 
rhs_peice(trn_1,obj_1_288_out). 
rhs_loc2D(obj_1_288_out,3,3). 
rhs_rot2D(obj_1_288_out,sameR). 
rhs_pen_color(obj_1_288_out,red). 
rhs_rotSize2D(obj_1_288_out,2,2). 
rhs_vis2D(obj_1_288_out,2,2). 
rhs_mass(obj_1_288_out,3). 
rhs_iz_sid(obj_1_288_out,s11261491). 
end(model(trn_1)). 
/*
begin(model(tst_0)). 
lhs_peice(tst_0,obj_0_750_in). 
lhs_loc2D(obj_0_750_in,3,10). 
lhs_rot2D(obj_0_750_in,rot180). 
lhs_pen_color(obj_0_750_in,green). 
lhs_rotSize2D(obj_0_750_in,2,2). 
lhs_vis2D(obj_0_750_in,2,2). 
lhs_mass(obj_0_750_in,3). 
lhs_iz_sid(obj_0_750_in,s11261491). 
lhs_peice(tst_0,obj_0_368_in). 
lhs_loc2D(obj_0_368_in,7,7). 
lhs_rot2D(obj_0_368_in,rot270). 
lhs_pen_color(obj_0_368_in,blue). 
lhs_rotSize2D(obj_0_368_in,2,2). 
lhs_vis2D(obj_0_368_in,2,2). 
lhs_mass(obj_0_368_in,3). 
lhs_iz_sid(obj_0_368_in,s11261491). 
lhs_peice(tst_0,obj_0_407_in). 
lhs_loc2D(obj_0_407_in,11,3). 
lhs_rot2D(obj_0_407_in,rot90). 
lhs_pen_color(obj_0_407_in,cyan). 
lhs_rotSize2D(obj_0_407_in,2,2). 
lhs_vis2D(obj_0_407_in,2,2). 
lhs_mass(obj_0_407_in,3). 
lhs_iz_sid(obj_0_407_in,s11261491). 
lhs_peice(tst_0,obj_0_557_in). 
lhs_loc2D(obj_0_557_in,3,3). 
lhs_rot2D(obj_0_557_in,sameR). 
lhs_pen_color(obj_0_557_in,purple). 
lhs_rotSize2D(obj_0_557_in,2,2). 
lhs_vis2D(obj_0_557_in,2,2). 
lhs_mass(obj_0_557_in,3). 
lhs_iz_sid(obj_0_557_in,s11261491). 
rhs_peice(tst_0,obj_0_646_out). 
rhs_loc2D(obj_0_646_out,1,1). 
rhs_rot2D(obj_0_646_out,rot180). 
rhs_pen_color(obj_0_646_out,green). 
rhs_rotSize2D(obj_0_646_out,2,2). 
rhs_vis2D(obj_0_646_out,2,2). 
rhs_mass(obj_0_646_out,3). 
rhs_iz_sid(obj_0_646_out,s11261491). 
rhs_peice(tst_0,obj_0_747_out). 
rhs_loc2D(obj_0_747_out,1,3). 
rhs_rot2D(obj_0_747_out,rot270). 
rhs_pen_color(obj_0_747_out,blue). 
rhs_rotSize2D(obj_0_747_out,2,2). 
rhs_vis2D(obj_0_747_out,2,2). 
rhs_mass(obj_0_747_out,3). 
rhs_iz_sid(obj_0_747_out,s11261491). 
rhs_peice(tst_0,obj_0_491_out). 
rhs_loc2D(obj_0_491_out,3,1). 
rhs_rot2D(obj_0_491_out,rot90). 
rhs_pen_color(obj_0_491_out,cyan). 
rhs_rotSize2D(obj_0_491_out,2,2). 
rhs_vis2D(obj_0_491_out,2,2). 
rhs_mass(obj_0_491_out,3). 
rhs_iz_sid(obj_0_491_out,s11261491). 
rhs_peice(tst_0,obj_0_557_out). 
rhs_loc2D(obj_0_557_out,3,3). 
rhs_rot2D(obj_0_557_out,sameR). 
rhs_pen_color(obj_0_557_out,purple). 
rhs_rotSize2D(obj_0_557_out,2,2). 
rhs_vis2D(obj_0_557_out,2,2). 
rhs_mass(obj_0_557_out,3). 
rhs_iz_sid(obj_0_557_out,s11261491). 
end(model(tst_0)). 
*/
/*
*/