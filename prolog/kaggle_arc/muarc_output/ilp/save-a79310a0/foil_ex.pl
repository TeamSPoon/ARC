% predicates in BK and examples
:- set_prolog_flag(stack_limit,  4_294_968_296). 
:-['./ILP/mlprograms/ilp.pl'].
:-['./ILP/mlprograms/foil.pl'].
:- use_module(library(logicmoo_utils)).

output(rhs/8).
foil_predicates([(lhs/8),(move_down/4), (color_change/2)]).

%  foil_cwa(false).  % explicit negative examples are provided below
 foil_cwa(true).    % CWA

:-use_module(library(clpfd)). 


incr_nat30(P,Q):-Q#=P+1. 
my_geq(P,Q):-nonvar(P),nonvar(Q),!,P>=Q. 
my_leq(P,Q):-nonvar(P),nonvar(Q),!,P=<Q. 
my_add(P,Q,R):-nonvar(P),nonvar(Q),integer(P),integer(Q),R is P+Q. 
my_add(P,Q,R):-nonvar(P),nonvar(R),integer(P),integer(R),Q is R-P. 
my_add(P,Q,R):-nonvar(R),nonvar(Q),integer(Q),integer(R),P is R-Q. 
my_mult(P,Q,R):-nonvar(P),nonvar(Q),integer(P),integer(Q),R is P*Q. 
my_mult(P,Q,R):-nonvar(P),nonvar(R),integer(P),integer(R),\+P=0.0,\+P=0,Q is R/P. 
my_mult(P,Q,R):-nonvar(R),nonvar(Q),integer(Q),integer(R),\+P=0.0,\+P=0,P is R/Q. 

size(30). 
at_left(1,_). 
at_top(_,1). 
at_bottem(_,P):- size(P). 
at_right(P,_):-size(P). 

canbe_int(P):- integer(P),!,nat30(P).
canbe_int(P):- var(P),!,(attvar(P)->true;freeze(P,\+ atom(P))),nat30(P).

%canbe_int(P):- \+ compound(P), \+ string(P), \+ atom(P). 

nat30(P):- P#>1,size(S),P#<S.
move_right(P,Q,R,Q):- maplist(canbe_int,[P,Q,R]),R#=P+1. 
move_left(P,Q,R,Q):- maplist(canbe_int,[P,Q,R]),R#=P-1. 
move_up(P,Q,P,R):- maplist(canbe_int,[P,Q,R]),R#=Q-1.
move_down(P,Q,P,R):- maplist(canbe_int,[P,Q,R]),R#=Q+1. 
color_change(cyan,red).

lhs(trn_0,1,1,sameR,cyan,4,zid_00,sid_22). 
lhs(trn_1,2,1,sameR,cyan,1,zid_00,sid_11). 
lhs(trn_2,2,2,sameR,cyan,3,zid_00,sid_13). 

rhs(trn_0,1,2,sameR,red,4,zid_00,sid_22). 
rhs(trn_1,2,2,sameR,red,1,zid_00,sid_11). 
rhs(trn_2,2,3,sameR,red,3,zid_00,sid_13). 

output_input(F/A):- output(F/A).
output_input(F/A):- foil_predicates(L),member(F/A,L).
bg_model(E):- foil_predicates(L),findall(P,(member(F/A,L), \+ output(F/A), functor(P,F,A),call(P)),E).
examples(E):- findall(+P,(output(F/A),functor(P,F,A),call(P)),E).
foil:- output_input(F/A),foil(F/A).
ilp:- examples(E),induce_rlgg(E,Clauses).
