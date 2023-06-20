/* Bongard dataset from
L. De Raedt and W. Van Laer. Inductive constraint logic.
In Klaus P. Jantke, Takeshi Shinohara, and Thomas Zeugmann, editors,
Proceedings of the Sixth International Workshop on Algorithmic
Learning Theory, volume 997 of Lecture Notes in Artificial Intelligence,
pages 80-94. SpringerVerlag, 1995.

Downloaded from
https://dtai.cs.kuleuven.be/static/ACE/doc/
*/

/** <examples>
?- induce_par_pascal([train],P),test_pascal(P,[test],LL,AUCROC,ROC,AUCPR,PR). % learn the parameteters and test the result
?- induce_pascal([train],P),test_pascal(P,[test],LL,AUCROC,ROC,AUCPR,PR). % learn the structure and the parameters and test the result
?- induce_par_pascal([all],P).
?- induce_pascal([all],P).
*/
:-use_module(library(pascal)).


:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(pic).
:- endif.

:-pascal.

%:-set_pascal(examples,keys(pos)).
:-set_pascal(default_parameters,0).
:-set_pascal(learning_algorithm,gradient_descent).
:-set_pascal(learning_rate,fixed(0.5)).
:-set_pascal(regularizing_constant,5).
:-set_pascal(regularization,2).

:-set_pascal(gd_iter,1000).
:-set_pascal(epsilon,0.0001).
:-set_pascal(epsilon_fraction,0.0001).
:-set_pascal(max_length,8).
:-set_pascal(max_lengths,[2,1,1,1]).
:-set_pascal(max_nodes,3). 
:-set_pascal(beamsize,2).
:-set_pascal(max_rules,5).
:-set_pascal(verbosity,1).

bg([]).

in([]).

:- begin_in.

ic("0.5 :: move_up(P,Q,P,R)/\\move_down(P,Q,P,R)---> false.").
%ic("0.5 :: circle(B)/\\in(B,A)---> config(A,up).").
%ic("0.5 :: circle(A)/\\in(A,B)---> config(A,up).").
%ic("0.5 :: square(A)---> config(A,up).").


 lazy_evaluate(my_add/3). 
 lazy_evaluate(my_geq/2). 
 lazy_evaluate(my_leq/2). 
 lazy_evaluate(my_mult/3). 
%:- lazy_evaluate(move_down/4). 
%:- lazy_evaluate(move_up/4). 

%foil_predicates([(rhs/7), (lhs/7), (move_down/4), (color_change/2)]).

%  foil_cwa(false).  % explicit negative examples are provided below
% foil_cwa(true).    % CWA


:-use_module(library(clpfd)). 


% :- modeh(*,rhs(+state,+nat30,+nat30,+color,+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
% :- modeb(*,lhs(+state,+nat30,+nat30,#(color),+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
% :- modeb(*,my_geq(+nat30,-#(nat30))). 
% :- modeb(*,my_leq(+nat30,-#(nat30))). 
% :- modeb(*,my_add(+nat30,+nat30,-nat30)). 
% :- modeb(*,my_mult(+nat30,#(nat30),-nat30)). 


n30(P):- integer(P),!,nat30(P).
n30(P):- var(P),!,(attvar(P)->true;freeze(P,\+ atom(P))),nat30(P).
nat30(P):- P#>=1,size(S),P#=<S.
size(30). 
my_geq(P,Q):-maplist(n30,[P,Q]),!,P#>=Q. 
my_leq(P,Q):-maplist(n30,[P,Q]),!,P#=<Q. 
my_add(P,Q,R):-n30(P),n30(Q),n30(R),R #= P+Q. 
my_mult(P,Q,R):-n30(P),n30(Q),n30(R),R #= P*Q. 
at_left(1,_).
at_top(_,1). 
at_bottem(_,P):- size(P). 
at_right(P,_):-size(P). 
move_right(P,Q,R,Q):- n30(P),n30(Q),n30(R), \+ at_right(P,_), R#=P+1. 
move_left(P,Q,R,Q):- n30(P),n30(Q),n30(R), \+ at_left(P,_), R#=P-1. 
move_up(P,Q,P,R):- n30(P),n30(Q),n30(R), Q#>1, R#=Q-1.
move_down(P,Q,P,R):- n30(P),n30(Q),n30(R),R#=Q+1. 
color_change(cyan,red).

:- end_in.

%determination(P/Q,R/S):-input(R/S),output(P/Q). 

output(rhs/7). 
input(lhs/7). 
input(color_change/2). 
input(move_down/4). 
input(move_up/4). 

fold(trn_0,[trn_0]).  fold(trn_1,[trn_1]).  fold(trn_2,[trn_2]). 
fold(train,[trn_0,trn_1,trn_2]). fold(test,[tst_0]).
fold(all,F):- fold(train,FTr), fold(test,FTe), append(FTr,FTe,F).

% predicates in BK and examples
:- set_prolog_flag(stack_limit,  4_294_967_296). 
%:-['./mlprograms/foil'].

modeh(*,rhs(+scope,+nat30,+nat30,+rot,+(color),+nat900,+shape)). 
%modeh(*,rhs(-scope,-nat30,-nat30,-rot,-(color),-nat900,-shape)). 
%modeh(*,rhs(+scope,-nat30,-nat30,-rot,-(color),-nat900,-shape)). 
%modeb(*,lhs(+scope,-nat30,-nat30,-rot,-(color),-nat900,-shape)). 
%modeb(*,lhs(+scope,+nat30,+nat30,+rot,+(color),+nat900,+shape)). 
%modeb(*,lhs(#scope,-nat30,-nat30,-rot,-(color),-nat900,-shape)). 
modeb(*,color_change(+(color),-(color))).
modeb(*,color_change(#(color),-(color))).
modeb(*,color_change(+(color),#(color))).
modeb(*,color_change(#(color),#(color))).
modeb(*,move_up(+nat30,+nat30,-nat30,-nat30)).
%modeb(*,move_up(#nat30,#nat30,-nat30,-nat30)).
modeb(*,move_up(+nat30,+nat30,#nat30,#nat30)).
%modeb(*,move_up(#nat30,#nat30,#nat30,#nat30)).
%modeb(*,move_down(#nat30,#nat30,+nat30,+nat30)).
modeb(*,move_down(+nat30,+nat30,#nat30,#nat30)).
modeb(*,move_down(+nat30,+nat30,-nat30,-nat30)).
%:-modeb(*,move_down(#nat30,#nat30,#nat30,#nat30)).

% :- determination(rhs/7,incr_nat30/2). 
% :- determination(rhs/7,my_geq/2). 
% :- determination(rhs/7,my_leq/2). 
% :- determination(rhs/7,my_add/3). 
% :- determination(rhs/7,my_mult/3). 
determination(rhs/7,lhs/7). 
determination(rhs/7,color_change/2). 
determination(rhs/7,move_down/4). 
determination(rhs/7,move_up/4). 



pos((trn_0)). 
lhs(trn_0,1,1,sameR,cyan,4,sid_22).
(rhs(trn_0,1,2,sameR,red,4,sid_22)).
neg(rhs(trn_0,1,1,sameR,cyan,4,sid_22)). 


pos((trn_1)). 
lhs(trn_1,2,1,sameR,cyan,1,sid_11). 
(rhs(trn_1,2,2,sameR,red,1,sid_11)). 
neg(rhs(trn_1,2,1,sameR,cyan,1,sid_11)). 

pos((trn_2)). 
lhs(trn_2,2,2,sameR,cyan,3,sid_13). 
(rhs(trn_2,2,3,sameR,red,3,sid_13)). 
neg(rhs(trn_2,2,2,sameR,cyan,3,sid_13)).


:- writeln('?- induce([trn_0,trn_1,trn_2],P)').
