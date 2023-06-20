/* Bongard dataset from
L. De Raedt and W. Van Laer. Inductive constraint logic. 
In Klaus P. Jantke, Takeshi Shinohara, and Thomas Zeugmann, editors, 
Proceedings of the Sixth International Workshop on Algorithmic
Learning Theory, volume 998 of Lecture Notes in Artificial Intelligence, 
pages 80-94. SpringerVerlag, 1995.

Downloaded from 
https://dtai.cs.kuleuven.be/static/ACE/doc/

     Arnaud Nguembang Fadja and Fabrizio Riguzzi. 
      Hierachical probabilistic logic programs
*/

/** <examples>
?- induce_lift([train],P),test_lift(P,[test],LL,AUCROC,ROC,AUCPR,PR). % learn the structure and the parameters and test the result
?- induce_par_lift([train],P),test_lift(P,[test],LL,AUCROC,ROC,AUCPR,PR). % learn the parameteters and test the result
?- in(P),test_lift(P,[test],LL,AUCROC,ROC,AUCPR,PR). % test the input theory
?- induce_par_lift([all],P).
?- induce_lift([all],P).
*/

:- use_module(library(slipcover)).
:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(lpad).
:- endif.

:-use_module(library(clpfd)). 
:-sc.

/*
:- set_lift(neg_ex,given).
:- set_lift(verbosity,1).
% Structure learning settings
:- set_lift(megaex_bottom,10). % max number of mega examples to considered in the generation of bottoms clauses
:- set_lift(initial_clauses_per_megaex,1).
:- set_lift(min_probability,0.00001).  % threshold value of the probability under which a clauses is dropped out

:- set_lift(verbosity, 1).

% Structure learning settings
:- set_lift(megaex_bottom,10). % max number of mega examples to considered in the generation of bottoms clauses
:- set_lift(initial_clauses_per_megaex,1).
%:- set_lift(rate,1.0). % defines the probabilityu for going from the first layer to the second layer
%:- set_lift(max_layer,-1). % Define the max number of layer: -1 for the maximum depth possible 
:- set_lift(min_probability,0.00001).  % threshold value of the probability under which a clauses is dropped out

% Parameter learning settings
%:- set_lift(algorithmType,dphil). % parameter learning algorithm dphil or emphil
% Maximun iteration and other stop conditions.
%:- set_lift(maxIter_phil,1000).  
%:- set_lift(epsilon_deep,0.0001). 
%:- set_lift(epsilon_deep_fraction,0.00001).

% regularization parameters 
%:- set_lift(regularized,no). % yes to enable regularization and no otherwise 
%:- set_lift(regularizationType,2). % 1 for L1, 2 for L2 and 3 for L3. L3 available only for emphil. If set to 0 no regularization is done
:- set_lift(gamma,10). % regularization strength
%:- set_lift(gammaCount,0). 

% Adam parameter for dphil algorithm
:- set_lift(adam_params,[0.1,0.9,0.999,1e-8]). % adam(Eta,Beta1,Beta2,Epsilon_adam_hat)
% Gradient descent strategy and the corresponding batch size
%:- set_lift(batch_strategy,minibatch(50)).
%:- set_lift(batch_strategy,stoch_minibatch(50)).
%:- set_lift(batch_strategy,batch).

*/

:-set_sc(specialization,mode).
:- set_sc(verbosity,10).

bg([]). 

:- begin_in.


color_change(cyan,red).

%incr_nat30(P,Q):-Q#=P+1. 
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

:- end_in.

fold(trn_0,[trn_0]). 
fold(trn_1,[trn_1]). 
fold(trn_2,[trn_2]). 

%determination(P/Q,R/S):-input_cw(R/S),output(P/Q). 

output(rhs/8). 
input(lhs/8). 
input(color_change/2). 
input(move_down/4). 
input(move_up/4). 

% predicates in BK and examples
:- set_prolog_flag(stack_limit,  4_294_968_296). 
%:-['./mlprograms/foil'].

% :- determination(rhs/8,incr_nat30/2). 
% :- determination(rhs/8,my_geq/2). 
% :- determination(rhs/8,my_leq/2). 
% :- determination(rhs/8,my_add/3). 
% :- determination(rhs/8,my_mult/3). 
determination(rhs/8,lhs/8). 
determination(rhs/8,color_change/2). 
determination(rhs/8,move_down/4). 
determination(rhs/8,move_up/4). 


modeh(*,rhs(+scope,+nat30,+nat30,+rot,+(color),+nat900,+shape,+shape)). 
%modeh(*,rhs(-scope,-nat30,-nat30,-rot,-(color),-nat900,-shape)). 
%modeh(*,rhs(+scope,-nat30,-nat30,-rot,-(color),-nat900,-shape)). 
modeb(*,lhs(+scope,-nat30,-nat30,-rot,-(color),-nat900,-shape,+shape)). 
%modeb(*,lhs(+scope,+nat30,+nat30,+rot,+(color),+nat900,+shape)). 
%modeb(*,lhs(#scope,-nat30,-nat30,-rot,-(color),-nat900,-shape)). 
modeb(*,color_change(+(color),-(color))).
%modeb(*,color_change(#(color),-(color))).
%modeb(*,color_change(+(color),#(color))).
%modeb(*,color_change(#(color),#(color))).
modeb(*,move_up(+nat30,+nat30,-nat30,-nat30)).
%modeb(*,move_up(#nat30,#nat30,-nat30,-nat30)).
modeb(*,move_up(+nat30,+nat30,#nat30,#nat30)).
%modeb(*,move_up(#nat30,#nat30,#nat30,#nat30)).
%modeb(*,move_down(#nat30,#nat30,+nat30,+nat30)).
modeb(*,move_down(+nat30,+nat30,#nat30,#nat30)).
modeb(*,move_down(+nat30,+nat30,-nat30,-nat30)).
%:-modeb(*,move_down(#nat30,#nat30,#nat30,#nat30)).


 lazy_evaluate(my_add/3). 
 lazy_evaluate(my_geq/2). 
 lazy_evaluate(my_leq/2). 
 lazy_evaluate(my_mult/3). 
%:- lazy_evaluate(move_down/4). 
%:- lazy_evaluate(move_up/4). 

%foil_predicates([(rhs/8), (lhs/8), (move_down/4), (color_change/2)]).

%  foil_cwa(false).  % explicit negative examples are provided below
% foil_cwa(true).    % CWA


% :- modeh(*,rhs(+state,+nat30,+nat30,+color,+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
% :- modeb(*,lhs(+state,+nat30,+nat30,#(color),+nat30,+nat30,+rotation,+nat900,+shape,+list)). 
% :- modeb(*,my_geq(+nat30,-#(nat30))). 
% :- modeb(*,my_leq(+nat30,-#(nat30))). 
% :- modeb(*,my_add(+nat30,+nat30,-nat30)). 
% :- modeb(*,my_mult(+nat30,#(nat30),-nat30)). 

begin(model(trn_0)). 
lhs(trn_0,1,1,sameR,cyan,4,zid_00,sid_22).
rhs(trn_0,1,2,sameR,red,4,zid_00,sid_22). 
neg(rhs(trn_0,1,1,sameR,cyan,4,zid_00,sid_22)). 
end(model(trn_0)). 

begin(model(trn_1)). 
lhs(trn_1,2,1,sameR,cyan,1,zid_00,sid_11). 
rhs(trn_1,2,2,sameR,red,1,zid_00,sid_11). 
neg(rhs(trn_1,2,1,sameR,cyan,1,zid_00,sid_11)). 
end(model(trn_1)). 

begin(model(trn_2)). 
lhs(trn_2,2,2,sameR,cyan,3,zid_00,sid_13). 
rhs(trn_2,2,3,sameR,red,3,zid_00,sid_13). 
neg(rhs(trn_2,2,2,sameR,cyan,3,zid_00,sid_13)).
end(model(trn_2)). 


:- writeln('?- induce([trn_0,trn_1,trn_2],P)').

