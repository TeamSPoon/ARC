:-style_check(- (discontiguous)). 
:-use_module(library(aleph)). 
:- use_module(library(logicmoo_utils)).
:-if(current_predicate(use_rendering/1)). 
:-use_rendering(prolog). 
:-endif. 

:-aleph. 

:-aleph_set(verbosity,3). 
:-aleph_set(interactive,false). 
:-aleph_set(i,10). 
:-aleph_set(nodes,100000). 
:-aleph_set(clauselength,10).



% predicates in BK and examples
:- set_prolog_flag(stack_limit,  4_294_967_296). 
%:-['./mlprograms/foil'].

:-modeh(*,rhs(+scope,+nat30,+nat30,+rot,+(color),+nat900,+shape)). 
:-modeh(*,rhs(-scope,-nat30,-nat30,-rot,-(color),-nat900,-shape)). 
:-modeh(*,rhs(+scope,+nat30,-nat30,-rot,-(color),-nat900,-shape)). 
:-modeb(*,lhs(-scope,-nat30,-nat30,-rot,-(color),-nat900,-shape)). 
%:-modeb(*,lhs(+scope,+nat30,+nat30,+rot,+(color),+nat900,+shape)). 
:-modeb(*,lhs(#scope,-nat30,-nat30,-rot,-(color),-nat900,-shape)). 
:-modeb(*,color_change(+(color),-(color))).
:-modeb(*,color_change(#(color),-(color))).
:-modeb(*,color_change(+(color),#(color))).
:-modeb(*,color_change(#(color),#(color))).
:-modeb(*,move_up(+nat30,+nat30,-nat30,-nat30)).
%:-modeb(*,move_up(#nat30,#nat30,-nat30,-nat30)).
:-modeb(*,move_up(+nat30,+nat30,#nat30,#nat30)).
%:-modeb(*,move_up(#nat30,#nat30,#nat30,#nat30)).
%:-modeb(*,move_down(#nat30,#nat30,+nat30,+nat30)).
:-modeb(*,move_down(+nat30,+nat30,#nat30,#nat30)).
:-modeb(*,move_down(+nat30,+nat30,-nat30,-nat30)).
%:-modeb(*,move_down(#nat30,#nat30,#nat30,#nat30)).

% :- determination(rhs/7,incr_nat30/2). 
% :- determination(rhs/7,my_geq/2). 
% :- determination(rhs/7,my_leq/2). 
% :- determination(rhs/7,my_add/3). 
% :- determination(rhs/7,my_mult/3). 
:- determination(rhs/7,lhs/7). 
:- determination(rhs/7,color_change/2). 
:- determination(rhs/7,move_down/4). 
:- determination(rhs/7,move_up/4). 

:- lazy_evaluate(my_add/3). 
:- lazy_evaluate(my_geq/2). 
:- lazy_evaluate(my_leq/2). 
:- lazy_evaluate(my_mult/3). 
%:- lazy_evaluate(move_down/4). 
%:- lazy_evaluate(move_up/4). 

%foil_predicates([(rhs/7), (lhs/7), (move_down/4), (color_change/2)]).

%  foil_cwa(false).  % explicit negative examples are provided below
% foil_cwa(true).    % CWA

:-begin_bg. 

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
at_left(1,_). 
at_top(_,1). 
at_bottem(_,P):- size(P). 
at_right(P,_):-size(P). 

canbe_int(P):- integer(P),!.
canbe_int(P):- var(P),!,(attvar(P)->true;freeze(P,\+ atom(P))).

%canbe_int(P):- \+ compound(P), \+ string(P), \+ atom(P). 

move_right(P,Q,R,Q):- maplist(canbe_int,[P,Q,R]), size(S),P#<S,R#=P+1. 
move_left(P,Q,R,Q):- maplist(canbe_int,[P,Q,R]),P#>1,R#=P-1. 
move_down(P,Q,P,R):- maplist(canbe_int,[P,Q,R]),size(S),Q#<S,R#=Q+1. 
move_up(P,Q,P,R):- maplist(canbe_int,[P,Q,R]),Q#>1,R#=Q-1.


% :- modeh(*,rhs(+state,+nat30,+nat30,+color,+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
% :- modeb(*,lhs(+state,+nat30,+nat30,#(color),+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
% :- modeb(*,my_geq(+nat30,-#(nat30))). 
% :- modeb(*,my_leq(+nat30,-#(nat30))). 
% :- modeb(*,my_add(+nat30,+nat30,-nat30)). 
% :- modeb(*,my_mult(+nat30,#(nat30),-nat30)). 

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
at_left(1,_). 
at_top(_,1). 
at_bottem(_,P):- size(P). 
at_right(P,_):-size(P). 

canbe_int(P):- integer(P),!.
canbe_int(P):- atom(P),P=inf,!,fail.
canbe_int(P):- var(P),!,nop(attvar(P)->true;freeze(P,\+ atom(P))).


move_right(P,Q,R,Q):- maplist(canbe_int,[P,Q,R]), size(S),R#=P+1,P#<S.
move_left(P,Q,R,Q):- maplist(canbe_int,[P,Q,R]),P#>1,R#=P-1. 
move_down(P,_,P,_):-!.
%move_down(P,Q,P,R):- maplist(canbe_int,[P,Q,R]),size(S),R#=Q+1,Q#<S.
move_up(P,_,P,_):-!.
%move_up(P,Q,P,R):- maplist(canbe_int,[P,Q,R]),Q#>1,R#=Q-1.

lhs(trn_0,1,1,sameR,cyan,4,sid_22). 
lhs(trn_1,2,1,sameR,cyan,1,sid_11). 
lhs(trn_2,2,2,sameR,cyan,3,sid_13). 

:-end_bg. 
:-begin_in_pos. 
rhs(trn_0,1,2,sameR,red,4,sid_22). 
rhs(trn_1,2,2,sameR,red,1,sid_11). 
rhs(trn_2,2,3,sameR,red,3,sid_13). 
:-end_in_pos. 
:-begin_in_neg.
rhs(trn_0,1,1,sameR,cyan,4,sid_22). 
rhs(trn_1,2,1,sameR,cyan,1,sid_11). 
rhs(trn_2,2,2,sameR,cyan,3,sid_13). 
:-end_in_neg.


