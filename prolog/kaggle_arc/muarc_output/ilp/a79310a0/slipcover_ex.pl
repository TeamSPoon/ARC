:-use_module(library(slipcover)). 
:-if(current_predicate(use_rendering/1)). 
:-use_rendering(prolog). 
:-endif. 
:-sc. 

:- set_sc(verbosity,3).
:- set_sc(depth_bound,false).
:- set_sc(neg_ex,given).

bg([]).

in([]).

output(rhs/10).
input(lhs/10).
input(incr_nat30/2).
input(color_change/2).


fold(trn_0,[trn_0]).
fold(trn_1,[trn_1]).
fold(trn_2,[trn_2]).

modeh(*,rhs(+state,+nat30,+nat30,+color,+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
modeb(*,lhs(+state,+nat30,+nat30,#(color),+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
modeb(*,incr_nat30(+nat30,#(nat30))). 
modeb(*,color_change(+color,#(color))). 
/*
:-modeb(*,my_geq(+nat30,#(nat30))). 
:-modeb(*,my_leq(+nat30,#(nat30))). 
:-modeb(*,my_add(+nat30,+nat30,-nat30)). 
:-modeb(*,my_mult(+nat30,#(nat30),-nat30)). 
:-lazy_evaluate(my_add/3). 
:-lazy_evaluate(my_geq/2). 
:-lazy_evaluate(my_leq/2). 
:-lazy_evaluate(my_mult/3). */
determination(rhs/10,lhs/10). 
determination(rhs/10,color_change/2). 
determination(rhs/10,incr_nat30/2). 
%:-determination(rhs/10,my_geq/2). 
%:-determination(rhs/10,my_leq/2). 
%:-determination(rhs/10,my_add/3). 
%:-determination(rhs/10,my_mult/3). 
/*
max_body(6). 
max_vars(8). 
non_magic(4). 
head_pred(rhs,10). 
body_pred(lhs,10). 
% body_pred(child,2). 
% body_pred(incr_nat30_by,3). 
body_pred(incr_nat30,2). 
body_pred(color_change,2). 
%body_pred(my_add,3). 
%body_pred(my_geq,2). 
%body_pred(my_leq,2). 
%body_pred(my_mult,3). 
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
*/
:-use_module(library(clpfd)). 
incr_nat30(P,Q):- Q #= P+1. 
color_change(_,_). 
my_geq(P,Q):-nonvar(P),nonvar(Q),!,P>=Q. 
my_leq(P,Q):-nonvar(P),nonvar(Q),!,P=<Q. 
my_add(P,Q,R):-nonvar(P),nonvar(Q),integer(P),integer(Q),R is P+Q. 
my_add(P,Q,R):-nonvar(P),nonvar(R),integer(P),integer(R),Q is R-P. 
my_add(P,Q,R):-nonvar(R),nonvar(Q),integer(Q),integer(R),P is R-Q. 
my_mult(P,Q,R):-nonvar(P),nonvar(Q),integer(P),integer(Q),R is P*Q. 
my_mult(P,Q,R):-nonvar(P),nonvar(R),integer(P),integer(R),\+P=0.0,\+P=0,Q is R/P. 
my_mult(P,Q,R):-nonvar(R),nonvar(Q),integer(Q),integer(R),\+P=0.0,\+P=0,P is R/Q. 
size(30). 
at_left(hv(1,_)). 
at_top(hv(_,1)). 
at_bottem(hv(_,P)):-size(P). 
at_right(hv(P,_)):-size(P). 
right(hv(P,Q),hv(R,Q)):-size(S),P#<S,R#=P+1. 
left(hv(P,Q),hv(R,Q)):-P#>1,R#=P-1. 
down(hv(P,Q),hv(P,R)):-size(S),Q#<S,R#=Q+1. 
up(hv(P,Q),hv(P,R)):-Q#>1,R#=Q-1. 

begin(model(trn_0)).
lhs(trn_0,1,1,cyan,2,2,sameR,4,'[[+,+],[+,+]]',[]). 
rhs(trn_0,1,2,red,2,2,sameR,4,'[[+,+],[+,+]]',[]). 
end(model(trn_0)).


begin(model(trn_1)).
lhs(trn_1,2,1,cyan,1,1,sameR,1,'[[0]]',[]). 
rhs(trn_1,2,2,red,1,1,sameR,1,'[[0]]',[]). 
end(model(trn_1)).

begin(model(trn_2)).
lhs(trn_2,2,2,cyan,3,1,sameR,3,'[[-,-,-]]',[]). 
rhs(trn_2,2,3,red,3,1,sameR,3,'[[-,-,-]]',[]). 
end(model(trn_2)).

:- writeln('?- induce([trn_0,trn_1,trn_2],P)').