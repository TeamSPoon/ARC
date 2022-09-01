/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.


into_baseKB_righto:-  
 locally(set_prolog_flag(access_level,system),
 ((op(200,fy,'-'),op(300,fx,'-'),
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'),
 op(600,yfx,'&'),
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-'),
 op(1199,fx,('==>'))))).
:- into_baseKB_righto.
%:- module(system).

%:- expects_dialect(pfc).
forall_assert(G,P):- forall(G,assert_if_new(P)).
%:- include(library(pfc_syntax)).
:- set_prolog_flag(pfc_term_expansion,true).

meta_argtypes(process_test_grid(oid)).

meta_argtypes(P) ==> {decl_pt(P)}.

cmem==>cmem(_,_,_).
cmem==>gid_glyph_oid(_,_,_).
cmem==>cindv(_,_,_).

tid_to_gids(T,A) :- awc,!, (clause(tid_to_gids(T,A),true)*-> true ; term_to_oid(T,A)).

startAll==>((kaggle_arc_io(TestID,ExampleNum,IO,G)/(ID=TestID*ExampleNum*IO,term_to_oid(ID,GID)))
  ==>(tid_to_gids(ID,GID),oid_to_grid(GID,G),process_oid(GID))).

%tid_to_gids(T,A) :- zwc,!, term_to_oid(T,A).

((startAll/get_why_uu(UU))==>why_startAll(UU)).

startAll ==> zwc, bwc, cwc, awc, fwc.


==> startAll.

startAll2==>(process_oid(GID)/( \+ cmem(GID,_,_))==>{assert_id_grid_cells(GID)}).


(startAll3 ==>process_test(t('27a28665'))).
((startAll4,all_arc_test_name(ID)) ==>process_test(ID)).





:- dynamic(saved_training/1).

(process_test(TestID) / (\+ saved_training(TestID))) ==> (individuate_test_grids(TestID), saved_training(TestID),{compile_and_save_test(TestID)}).

((individuate_test_grids(TestID),tid_to_gids(TestID*trn*in,GID))==> process_test_grid(GID)).
process_test_grid(GID)==>{assert_id_grid_cells(GID),individuate(complete,GID,_)}.

:- dynamic(bc_q/1).
:- dynamic(bc_p/1).

:- (ain((bc_q(N) <- bc_p(N)))).
:- listing(bc_q/1).

bc_p(a).
bc_p(b).
:- listing(bc_p/1).

%:- mpred_trace_exec.

:- mpred_test(call_u(bc_p(b))).

%= nothing cached ?
:- listing(bc_q/1).

:- mpred_test(\+ clause_u(bc_q(_),true)).

:- mpred_test((call_u(bc_q(b)))).

%= something cached
:- listing(bc_q/1).
:- mpred_test( clause_u(bc_q(_),true)).


:- mpred_test(fwc).

%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
%  cls ; kill -9 %1 ; fg ; swipl -g "ensure_loaded(pack(logicmoo_base/t/examples/base/'sanity_abc.pfc'))."


:- dynamic(meta_argtypes/1).
:- dynamic(most/1).

%:- expects_dialect(pfc).

meta_argtypes(most(ftAssertable)).

% BWD chaining
most((Q <- P))/mpred_literal(Q) ==> (Q <-(P, \+ ~(Q))).

% FWD chaining
most(P==>Q)/nonvar(Q) ==> (((P ==> most(Q)))).

% NEG chaining
most(~Q)/mpred_positive_literal(Q)  ==>  (( \+ Q ) ==> ~ Q ).

% POS chaining 1
most(Q)/(mpred_positive_literal(Q),if_missing_mask(Q,R,Test)) ==> (  ( ( \+R /Test , (\+ ~ Q)) ==> Q )).

% POS chaining 2
most(Q)/(mpred_positive_literal(Q),if_missing_mask(Q,R,Test)) ==> ( ((R/( \+(R=Q), Test)) ==> (\+ Q))).

% POS chaining 1+2
% most(Q)/(mpred_positive_literal(Q),if_missing_mask(Q,R,Test)) ==> (  ( ( \+R /Test ) ==> Q ) ,((R/( \+(R=Q), Test)) ==> (\+ Q))).

% most(Q) ==> if_missing(Q,Q).

%(most(P=>Q)/(mpred_literal_nv(Q),if_missing_mask(Q,R,Test)))  ==> ((P, \+ R/Test) => Q).
%(most(P=>Q)/nonvar(Q)) ==> (P => most(Q)).


:-dynamic((a/1,b/1,c/1)).

a(X) ==> c(X).
most(c(X) ==> ~b(X)) .
a(1).


:- listing([a/1,b/1,c/1,(==>)/2,most/1,pt,nt,bt]).

:- mpred_test(~b(1)).


(default_01a(P)/mpred_literal(P))  ==>  (~( ~P) ==> P).

(default_01a((P ==> Q))/mpred_literal(Q)) ==> (P, \+( ~Q) ==> Q).

%:- set_prolog_flag(gc,false).

%

% birds fly by default_01a.
==> default_01a((bird(X) ==> fly(X))).

% here''s one way to do an type of hierarchy.
% zisa = subclass.

zisa(C1,C2) ==>
  {P1 =.. [C1,X],
    P2 =.. [C2,X]},
  (P1 ==> P2).

==> zisa(canary,bird).
==> zisa(penguin,bird).

% penguins do not fly.
penguin(X) ==> ( ~fly(X)).

%:- mpred_trace_exec.
% chilly is a penguin.
==> penguin(chilly).


% tweety is a canary.
==> canary(tweety).

a==>b.
a.
a.
\+ a.

:- mpred_test(penguin(chilly)).

:- mpred_test(~fly(chilly)).

:- mpred_test(fly(tweety)).


:- listing(fly).
:- listing(~).

:- mpred_notrace_exec.

((cindv( Obj, localpoints, _)/(obj_to_oid(Obj,GID),globalpoints(Obj,GPS)))==> {assert_id_cells(GID,GPS)}).


arc_test_property(T, common(comp(o-o, area)), area(n(1, 1, d(0), a(0), r(1))))==> note(T,"output always size 1").
arc_test_property(T, common(comp(i-o, area)), area(n(X, X, d(0), a(0), r(1))))/var(X)==> note(T,"output size always same as input").
%arc_test_property(T, common(comp(o-o, area)), area(n(X, X, d(0), a(0), r(1))))/nonvar(X)==> note(T,"output size always same as input").

%:- forall_assert(kaggle_arc_io(TestID,ExampleNum,IO,_),some_grid_tid(TestID*ExampleNum*IO)).
:- set_prolog_flag(pfc_term_expansion,false).


:- fixup_exports.

:- add_history(pfcAddF(startAll2)).
