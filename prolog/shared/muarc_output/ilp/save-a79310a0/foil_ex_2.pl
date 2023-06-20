% predicates in BK and examples
:- set_prolog_flag(stack_limit,  4_294_967_296). 
:-['./mlprograms/foil'].
:- use_module(library(logicmoo_utils)).

foil_predicates([(rhs/6), (lhs/6), (move_down/2), (color_change/2)]).

%  foil_cwa(false).  % explicit negative examples are provided below
 foil_cwa(true).    % CWA

:-use_module(library(clpfd)). 

color_change(cyan,red).

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
at_left([1,_]). 
at_top([_,1]). 
at_bottem([_,P]):-size(P). 
at_right([P,_]):-size(P). 

move_right([P,Q],[R,Q]):-size(S),P#<S,R#=P+1. 
move_left([P,Q],[R,Q]):-P#>1,R#=P-1. 
move_down([P,Q],[P,R]):-size(S),Q#<S,R#=Q+1. 
move_up([P,Q],[P,R]):-Q#>1,R#=Q-1.

lhs(trn_0,[1,1],sameR,cyan,4,sid_22). 
lhs(trn_1,[2,1],sameR,cyan,1,sid_11). 
lhs(trn_2,[2,2],sameR,cyan,3,sid_13). 

rhs(trn_0,[1,2],sameR,red,4,sid_22). 
rhs(trn_1,[2,2],sameR,red,1,sid_11). 
rhs(trn_2,[2,3],sameR,red,3,sid_13). 

