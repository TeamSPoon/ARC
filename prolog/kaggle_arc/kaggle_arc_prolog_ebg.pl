%    /*<title->
%      Prolog Explanation-Based Reasoning: Cups Example #1
%    </title->*/
 % PROLOG EBG: THE CUP EXAMPLE


/* Domain theory */

p1:cup(X):-p1:liftable(X),p1:stable(X),p1:open_vessel(X).
p1:liftable(X):-p1:light(X),p1:part(X,handle).
p1:stable(X):-p1:part(X,bottom),p1:flat(bottom).
p1:open_vessel(X):-p1:part(X,concavity),p1:points_up(concavity).
p1:light(X):-p1:small(X).
p1:light(X):-p1:made_of(X,feathers).
 
 /* Training example */
 
p1:small(obj1).
p1:part(obj1,handle).
p1:part(obj1,bottom).
p1:flat(bottom).
p1:part(obj1,concavity).
p1:points_up(concavity).
 
 /* operationality criteria */
 
p1:operational(p1:small(X)).
p1:operational(p1:part(X,Y)).
p1:operational(p1:flat(X)).
p1:operational(p1:points_up(X)).

   /*<title->
      Prolog Explanation-Based Reasoning: Cups Example #2
    </title->*/

p2:cup(X):- p2:liftable(X),  p2:holds_liquid(X).

p2:holds_liquid(Z):- p2:part(Z,W),p2:concave(W),p2:points_up(W).

p2:liftable(Y):- p2:light(Y), p2:part(Y,handle).

p2:light(A):- p2:small(A).light(A):- p2:made_of(A,feathers).

%cup(obj1).
p2:small(obj1).
p2:part(obj1,handle).
p2:owns(bob,obj1).
p2:part(obj1,bottom).
p2:part(obj1,bowl).
p2:points_up(bowl).
p2:concave(bowl).
p2:color(obj1,red).

p2:operational(p2:small(_)).
p2:operational(p2:part(_,_)).
p2:operational(p2:owns(_,_)).
p2:operational(p2:points_up(_)).
p2:operational(p2:concave(_)).
p2:operational(p2:color(_,_)).


    /*<title->
      Prolog Explanation-Based Reasoning: Goals
    </title->*/
% PROLOG EBG

ebg_1(Goal, Gen_goal, (Gen_goal :-  Premise)) :-  
	prolog_ebg_1(Goal, Gen_goal, _, Gen_proof),
	extract_support_1(Gen_proof, Premise).

prolog_ebg_1(A, GenA, A, GenA) :-  my_ebg_clause(A, true).

prolog_ebg_1((A, B), (GenA,GenB), (AProof, BProof), (GenAProof,GenBProof)) :- 
   !,prolog_ebg_1(A, GenA, AProof, GenAProof),
   prolog_ebg_1(B, GenB, BProof,GenBProof).

prolog_ebg_1(A, GenA, (A :-  Proof), (GenA :-  GenProof)) :- 
   my_ebg_clause(GenA, GenB), 
   duplicate_v1((GenA:- GenB), (A:- B)),
   prolog_ebg_1(B, GenB, Proof, GenProof).

my_ebg_clause(A, B) :-  ebg_clause(A, true), B = true.
my_ebg_clause(A, B) :-  var(B), clause(A, B).

% The purpose of copy2 is to get a new copy of an expression 
% with all new variables.

duplicate_v1(Old, New) :-  assert('$marker'(Old)), 
                      retract('$marker'(New)).

% Extract support walks down a proof tree and returns the sequence of the
% highest level operational nodes, as defined by the predicate "operational"

extract_support_1(Proof, Proof) :-  operational(Proof).
extract_support_1((A :-  _), A) :-  operational(A).
extract_support_1((AProof, BProof), (A,B)) :- 
   extract_support_1(AProof, A),
   extract_support_1(BProof, B).
extract_support_1((_ :-  Proof), B) :-  extract_support_1(Proof, B).

% A neat exercise would be to build a predicate, make_rule, that calls prolog_ebg to
% generate the generalized tree and extract_support_1 to get the operational nodes and
% constructs a rule of the form:
%     top_level_goal :-  sequence of operational nodes

    /*<title->
      Prolog Explanation-Based Reasoning
    </title->*/

% PROLOG EBG

ebg_2(Goal, Gen_goal, (Gen_goal :-  Premise)) :-  
	prolog_ebg_2(Goal, Gen_goal, _, Gen_proof),
	extract_support_v2(Gen_proof, Premise).

prolog_ebg_2(A, GenA, A, GenA) :-  not(list(A)), ebg_clause(A, [true]).

prolog_ebg_2([],[],[],[]).

prolog_ebg_2([A|B], [GenA|GenB], [AProof| BProof], [GenAProof|GenBProof]) :- 
   prolog_ebg_2(A, GenA, AProof, GenAProof),
   prolog_ebg_2(B, GenB, BProof,GenBProof).

prolog_ebg_2(A, GenA, [A :-  Proof], [GenA :-  GenProof]) :- 
   ebg_clause(GenA, GenB), 
   duplicate_2((GenA:- GenB), (A:- B)),
   prolog_ebg_2(B, GenB, Proof, GenProof).

% The purpose of copy2 is to get a new copy of an expression 
% with all new variables.
duplicate_2(Old, New) :-  assert('$marker'(Old)), retract('$marker'(New)).

% Extract support_2 walks down a proof tree and returns the sequence of the
% highest level operational nodes, as defined by the predicate "operational"

extract_support_v2(Proof, Proof) :-  operational(Proof).
extract_support_v2([Proof],(R)):- extract_support_v2(Proof,R).
extract_support_v2([AProof| BProof], (A,B)) :- 
   extract_support_v2(AProof, A),
   extract_support_v2(BProof, B).
extract_support_v2([A :-  _], A) :-  operational(A).
extract_support_v2([_ :-  Proof], B) :-  extract_support_v2(Proof, B).

% A neat exercise would be to build a predicate, make_rule, that calls prolog_ebg to
% generate the generalized tree and extract_support_2 to get the operational nodes and
% constructs a rule of the form:
%     top_level_goal :-  sequence of operational nodes

 

    /*<title->
      Prolog Explanation-Based Reasoning: Sample Run
    </title->*/

% trace of various calls to prolog ebg using the cup example.
% a top level execution predicate would compine prolog ebg and extract rule 
% plus do some syntactic stuff to build a new rule

/*example

?- prolog_ebg_1(p1:cup(obj1), p1:cup(X), P, GenP).
    X = _0,
    P = [ (cup(obj1) :-  
               [[ (liftable(obj1) :-  
                       [[ (light(obj1) :-  
                               [small(obj1)])],part(obj1,handle)])],
                [ (stable(obj1) :-  
                       [part(obj1,bottom),flat(bottom)])],
                [ (open_vessel(obj1) :-  
                       [part(obj1,concavity),points_up(concavity)])]])],
    GenP = [ (cup(_0) :-  
                [[ (liftable(_0) :-  
                       [[ (light(_0) :-  
                                  [small(_0)])],
                           part(_0,handle)])],
                   [ (stable(_0) :-  
                          [part(_0,bottom),flat(bottom)])],
                   [ (open_vessel(_0) :-  
                          [part(_0,concavity),points_up(concavity)])]])] 


?- extract_support_1([ (cup(_0) :-  
                  [[ (liftable(_0) :-  
                          [[ (light(_0) :-  
                                  [small(_0)])],
                           part(_0,handle)])],
                   [ (stable(_0) :-  
                          [part(_0,bottom),flat(bottom)])],
                   [ (open_vessel(_0) :-  
                          [part(_0,concavity),points_up(concavity)])]])], Premise).
    _0 = _0,
    Premise =  (small(_0), part(_0,handle)), 
                (part(_0,bottom), flat(bottom)), 
                part(_0,concavity), points_up(concavity) 
		
		
%%%% This conjunctive query would be the heart (plus some syntactic sugar)
% of a rule constructor.
	    
    
?- prolog_ebg(cup(obj1), cup(X), P, GenP), extract_support_1(GenP, Premise).
    X = _0,
    P = [ (cup(obj1) :-  
               [[ (liftable(obj1) :-  
                       [[ (light(obj1) :-  
                               [small(obj1)])],part(obj1,handle)])],
                [ (stable(obj1) :-  
                       [part(obj1,bottom),flat(bottom)])],
                [ (open_vessel(obj1) :-  
                       [part(obj1,concavity),points_up(concavity)])]])],
    GenP = [ (cup(_0) :-  
                  [[ (liftable(_0) :-  
                          [[ (light(_0) :- 
                                  [small(_0)])],
                           part(_0,handle)])],
                   [ (stable(_0) :-  
                          [part(_0,bottom),flat(bottom)])],
                   [ (open_vessel(_0) :-  
                          [part(_0,concavity),points_up(concavity)])]])],
    Premise =  (small(_0), part(_0,handle)), 
                (part(_0,bottom), flat(bottom)), 
                part(_0,concavity), points_up(concavity)   

*/


/*<title->
      Prolog Explanation-Based Reasoning: Suicide Example
    </title->*/

% The suicide example

/* Domain theory */

kill(A,B) :-  hate(A,B), possess(A,C), weapon(C).
hate(W,W) :-  depressed(W).
possess(U,V) :-  buy(U,V).
weapon(Z) :-  gun(Z).

/* Training example */

depressed(john).
buy(john, colt).
gun(colt).   
   
/* operationality criteria */

operational(depressed(X)).
operational(buy(X, Y)).
operational(gun(X)).


