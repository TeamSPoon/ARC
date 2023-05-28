/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


rhs_ground(G):- ground(G),!.
rhs_ground(G):- nop(writeln(G)),!.

ac_clauses(List,Ctx,P,PSame):- is_list(List),!,member(Stuff,List),Stuff=..[_,Ctx,P,PSame].
ac_clauses(TestID,Ctx,P,PSame):- 
  ac_unit_db(TestID,Ctx,P,Same),
  include(not_debug_info,Same,PSame), 
  %Same=PSame,
  PSame\==[].

not_debug_info(P):- \+ is_debug_info(P),!.

remove_debug_info(List,NoDebug):- \+ compound(List),!,NoDebug=List.
remove_debug_info(List,NoDebug):- is_list(List), is_obj_props(List),!,include(not_debug_info,List,NoDebug).
remove_debug_info(List,NoDebug):- is_list(List), !, maplist(remove_debug_info,List,NoDebug).
remove_debug_info(List,NoDebug):- compound_name_arguments(List,F,AA),
  maplist(remove_debug_info,AA,CC),!, compound_name_arguments(NoDebug,F,CC).

ac_unit_db(TestID,Ctx,P,Same):- ac_listing(TestID,Ctx,P,Same),include(not_debug_info,Same,PSame), PSame\==[].

ac_rules(TestID,Ctx,P,PSame):- ac_listing(TestID,Ctx,P,PSame).
ac_listing(List,Ctx,P,PSame):- !, ac_unit(List,Ctx,P,PSame).
ac_listing(List,Ctx,P,PSame):- is_list(List),!,member(Stuff,List),Stuff=..[_,Ctx,P,PSame].
%ac_listing(TestID,Ctx,P->ac_unit_db,PSame):- ac_unit_db(TestID,Ctx,P,PSame).
ac_listing(TestID,Ctx,P,PSame):- ac_unit(TestID,Ctx,P,PSame)*->true;pass2_clause(TestID,Ctx,P,PSame).
%ac_listing(TestID,Ctx,P,[iz(info(prop_can))|PSame]):- prop_can(TestID,Ctx,P,PSame).
%ac_listing(TestID,Ctx,P,[pass2|PSame]):- pass2_clause(TestID,Ctx,P,PSame), \+ ac_clauses(TestID,Ctx,P,PSame).


ac_info(TestID,clauses,P->Ctx->current,LHS):- 
  member(Ctx,[in_out,in_out_out,s(_)]),
  trans_clauses_current_members(TestID,Ctx,R),
  clause_to_pcp(R,P,LHS).
ac_info(TestID,clauses,P->Ctx->combined,LHS):- fail,
  member(Ctx,[in_out,in_out_out,s(_)]),
  trans_clauses_combined_members(TestID,Ctx,R),
  clause_to_pcp(R,P,LHS).

show_time_of_failure(_TestID):- !.
show_time_of_failure(TestID):- 
    print_scene_change_clauses3(show_time_of_failure,
       ac_info,TestID).

clause_to_pcp(TestID,R,Ctx,P,LHS):-
  is_list(R),!,member(E,R),clause_to_pcp(TestID,E,Ctx,P,LHS).
clause_to_pcp(_TestID,R,Ctx,P,LHS):- 
  must_det_ll((
  find_rhs(R,P),
  find_lhs(R,Conds),
  subst(R,P,p,RR), subst(RR,Conds,conds,RRR),
  append(Conds,[iz(info(RRR))],LHS),
  ignore((sub_cmpd(ctx(Ctx),R))))).

pcp_to_clause(TestID,Ctx,P,LHS,clause(TestID,Ctx,P,LHS)).


%ac_clauses(TestID,P,PSame):- ac_clauses(TestID,_,P,PSame).

%pass2_clause(TestID,Ctx,P,PSame):- pass2_clause_old(TestID,Ctx,P,PSame).
%pass2_clause(TestID,Ctx,P,PSame):- pass2_clause_new(TestID,Ctx,P,PSame).

pass2_clause(TestID,Ctx,RHS,LHS):-   
  pass2_clause1(TestID,Ctx,RHS,LHS)*->true;
  pass2_clause2(TestID,Ctx,RHS,LHS)*->true;
  pass2_clause3(TestID,Ctx,RHS,LHS)*->true.
/*
pass2_clause(TestID,Ctx,RHS,LHS):-
  findall_vset(Ctx-RHS,(pass2_clause1(TestID,Ctx,RHS,LHS);pass2_clause2(TestID,Ctx,RHS,LHS)),List),
  member(Ctx-RHS,List),
  (pass2_clause1(TestID,Ctx,RHS,LHS)*->true;pass2_clause2(TestID,Ctx,RHS,LHS)).
*/

pass2_clause1(TestID,Ctx,RHS,LHS):- fail,
 ensure_test(TestID),
  trans_clauses_combined_members(TestID,Ctx,Clause),
  %Info = info(_Step,_IsSwapped,Ctx,_TypeO,TestID,_ExampleNum,_),
  %arg(_,Clause,Info),
  must_det_ll((
  clause_to_pcp(TestID,Clause,Ctx,RHS,LHS))).

pass2_clause2(TestID,Ctx,RHS,LHS):- 
 ensure_test(TestID),
  trans_clauses_current_members(TestID,Ctx,Clause),
  %Info = info(_Step,_IsSwapped,Ctx,_TypeO,TestID,_ExampleNum,_),
  %arg(_,Clause,Info),
  must_det_ll((
  clause_to_pcp(TestID,Clause,Ctx,RHS,LHS))).


pass2_clause3(TestID,Ctx,edit(Type,different,P),[iz(info(propcan(true,Ctx)))|PSame]):- fail,
  ensure_test(TestID), ensure_props_change(TestID,Ctx,P),
  prop_can(TestID,IO,P,PSame), once((io_to_cntx(IO,Ctx),prop_type(P,Type))).

/*pass2_clause(TestID,Ctx,RHS,[iz(info(Info))|LHS]):- 
 ensure_test(TestID),
  trans_clauses_combined(TestID,_Ctx,Combined),
  member(Clause,Combined),
  arg(_,Clause,Info),
  must_det_ll((
  arg(_,Clause,rhs(RHS)),
  arg(_,Clause,lhs(LHS)))),
  rhs_ground(RHS).
*/



trans_clauses_mappings(TestID,ExampleNum,Ctx,Mappings):- 
  var(ExampleNum),!,current_example_scope(TestID,ExampleNum),
  trans_clauses_mappings(TestID,ExampleNum,Ctx,Mappings).

trans_clauses_mappings(TestID,ExampleNum,Ctx,Mappings):- 
 ((arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,Mappings),fail)*->true;
   (obj_group_pair(TestID,ExampleNum,LHSObjs,RHSObjs),
    into_object_dependancy_r_l(TestID,ExampleNum,Ctx,RHSObjs,LHSObjs,Mappings))).
  
trans_clauses_current_members1(TestID,Ctx,Clauses):-  
  current_example_scope(TestID,ExampleNum),
  trans_clauses_mappings(TestID,ExampleNum,Ctx,Mappings),
  mappings_to_clauses(Mappings,Clauses).

mappings_to_clauses(Mappings,Clauses):- 
  member(l2r(Info,In,Out),Mappings),
  into_list(In,InL),into_list(Out,OutL),
  trans_rule(Info,InL,OutL,TransClauses), 
  member(Clauses,TransClauses).
  
trans_clauses_current_members(TestID,Ctx,Clauses):-
  ensure_test(TestID),
  ((fail,arc_cache:trans_rule_db(TestID,_ExampleNum1,Ctx,Clauses),Clauses\=l2r(_,_,_))*->true;
    trans_clauses_current_members1(TestID,Ctx,Clauses)).

trans_clauses_combined_members(TestID,Ctx,CombinedM):-
 ensure_test(TestID),
 must_det_ll((findall(Clause,trans_clauses_current_members(TestID,Ctx,Clause),Clauses),
  % must_det_ll(( \+ (member(R,[1|Clauses]), is_list(R)))),!,
  combine_trans_clauses(TestID,Clauses, Combined))),
  % must_det_ll(( \+ (member(R,[2|Combined]), is_list(R)))),
  member(CombinedM,Combined).

combine_trans_clauses( TestID,Clauses, CombinedClauses):- compute_scene_change_pass_out(TestID,Clauses, CombinedClauses),!.
combine_trans_clauses(_TestID,Clauses, CombinedClauses):- combine_trans_clauses1(Clauses, CombinedClauses).

combine_trans_clauses_textually(R1,Clauses,R,ClausesN):- 
  into_rhs(R1,RHS1),
  same_functor(R1,R2),
  select(R2,Clauses,ClausesN), % my_assertion(r1, \+ is_list(R1)), my_assertion(r2, \+ is_list(R2)),
  into_rhs(R2,RHS2),
  %into_step(R2,RHS2),
  RHS1=@=RHS2,
  merge_vals(R1,R2,R) % my_assertion(r, \+ is_list(R)),
  .
combine_trans_clauses1([],[]):-!.
combine_trans_clauses1([R],[R]):-!.
combine_trans_clauses1([R1|Clauses], CombinedClauses):-
  combine_trans_clauses_textually(R1,Clauses,R,ClausesN),!, combine_trans_clauses1([R|ClausesN], CombinedClauses).
combine_trans_clauses1([R|Clauses], [R|CombinedClauses]):- 
  combine_trans_clauses1(Clauses, CombinedClauses).




/*
pass2_clause(TestID,IO,P,OutputClause):-
  ensure_test(TestID),
  ClauseType = edit_copy(IO,ReType,P), 
  SortKey = P,
  OutputClause = clause(ClauseType,SortKey,SuperPreconds),
  KeyedClause = clause(ClauseType,SortKey,Precond),
  Clause = clause(ClauseType,P,LHS),
  findall(Clause,pass2_clause_R(TestID,Clause),Clauses),
  maplist(arg(1),Clauses,Keyz),vsr_set(Keyz,Keys),
  member(ClauseType,Keys),
  findall(KeyedClause,
    (prop_type(P,ReType),findall(LHS,member(Clause,Clauses),LHSList),flatten(LHSList,FFound),
      into_lhs(FFound,Precond)),KeyedClauses),
  maplist(arg(3),KeyedClauses,Preconds),into_lhs(Preconds,SuperPreconds),
  member(KeyedClause,KeyedClauses),
  %include(has_a_value,SuperPreconds,UsedPrconds),
  true.
*/



%map_pairs_info(TestID,Ctx,P,Step):- !, map_pairs_info_io(TestID,_ExampleNum,Ctx,Step,_TypeO,_A,_B,_USame,_InFlatProps,UPB2),member(P,UPB2),nop(ok_deduce(P)).
ensure_props_change(TestID,IO,P):-  props_change(TestID,IO,P).

map_pairs_info(TestID,IO,P,Step):-
  no_repeats_var(IOP),
  map_pairs_info2(TestID,IO,P,Step),
  ground(P),
  IOP=(IO+P).

%  ((var(P),has_propcounts(TestID))->props_change2(TestID,IO,P);true),
map_pairs_info2(TestID,IO,P,_Step):- props_change2(TestID,IO,P).
map_pairs_info2(TestID,IO,P,_Step):- 
 var(P), \+ \+ ac_unit(TestID,IO,_,_), %!,
  ac_unit(TestID,IO,P,_).
map_pairs_info2(TestID,Ctx,P,Step):- 
  arc_cache:prop_dep(TestID,_ExampleNum,Ctx,Info,_InL,_OutL,_USame,_InFlatProps,OutFlatProps),
  sub_cmpd(step(Step),Info),
  member(P,OutFlatProps).

/*

:- abolish(good_conseq/4).
:- dynamic(good_conseq/4).

map_pairs_info2(_TestID,IO,P,_Step):- nonvar(P),nonvar(IO),!.
map_pairs_info2(TestID,IO,P,_Step):- nonvar(P),nonvar(IO),good_conseq(TestID,IO,P,YN),!,YN=yes.
  
  
map_pairs_info2(TestID,IO,P,Step):- 
 ((var(P),has_propcounts(TestID))->props_change2(TestID,IO,P);true),
 no_repeats_var(IOP),
 (map_pairs_info3(TestID,IO,P,Step)*->asserta(good_conseq(TestID,IO,P,yes));(asserta(good_conseq(TestID,IO,P,no)),fail)),
 IOP=IO+P.

map_pairs_info3(_TestID,_IO,_P,_Step).
*/
/*:- nonvar(P).
map_pairs_info3(TestID,IO,P,Step):- 
  %ensure_individuals(TestID),
  %ensure_propcounts(TestID),
  %learn_object_dependancy(TestID),
  
  (var(IO)->gather_set(Ctx,pair_obj_props(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,InFlatProps,OutFlatProps));true),
  %IO_ = in, Ctx = in_out,
  %gather_set(P,(map_pairs_info_io(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,UPA2,UPB2),member(P,PB2))).
  gather_set(P,(
      %nop(gather_set(Step,(map_pairs_info_io(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,UPA2,UPB2),member(P,UPB2)))),
      map_pairs_info_io(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,InFlatProps,UPB2),member(P,UPB2),ok_deduce(P))),
  %p_to_utbs(TestID,Ctx,P,UTBLists),  
  %common_members(UTBLists,Members), 
  %member(P,Members),

  ignore(gather_set(Step,(pair_obj_props(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,InFlatProps,OutFlatProps),member(P,OutFlatProps)))),
  io_to_cntx(IO,Ctx).

map_pairs_info_io(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,UPA2,UPB2):-
 pair_obj_props(TestID,ExampleNum,Ctx,Step,TypeO,InL,OutL,USame,UPA2,UPB2).

*/



% delete
trans_rule(Info,In,[],Clauses):- listify(In,InL),
 findall(delete_object(Info,rhs(delete(In)),lhs(Preconds)),
   (member(In,InL),into_lhs(In,Preconds)),Clauses), Clauses\==[],!.

% mutiple postconds
trans_rule(Info,In,[Out,Out2|OutL],TransClause):- is_object(Out),is_object(Out2),
  maplist(trans_rule(Info,In),[Out,Out2|OutL],TransClause), TransClause\==[],!.

% create
trans_rule(Info,[],Out,Clauses):- listify(Out,OutL),
 findall(create_object(Info,rhs(create(Out)),lhs(Preconds)),
   ((member(Out,OutL),into_lhs(Out,Preconds))),Clauses), Clauses\==[],!.

% 2 preconds
%trans_rule(Info,[In1,In2],[Out],TransClause):- is_object(In1),is_object(In2),
%  TransClause = create_object2(Info,rhs(create_obj(Out)),lhs(into_new(In1,In2))),!.

% 2 preconds
trans_rule(Info,[In1,In2],[Out],TransClause):-  is_object(In1),is_object(In2), % fail,
   noteable_propdiffs(In1, Out,_Same1,_DontCare1,New1), 
   noteable_propdiffs(In2,New1,_Same2,_DontCare2,New2),
   %remove_o_giz(Out,RHSO), 
   remove_o_giz(In1,Precond1), remove_o_giz(In2,Precond2),
   %sub_comInfo = info(Step,_IsSwapped,_Ctx,TypeO,_,_,_),
   sub_cmpd(step(Step),Info), sub_cmpd(type(Type),Info),
   Type \== assumed_in_in_out,
 % append_sets(Same1,Same2,Same), append_sets(DontCare1,DontCare2,DC), append_sets(New1,New2,New),
 % append_sets(Same,New,NewObj),
  %make_common(RHSO,LHS1,NewOut1,NewLHS1),
  %make_common(NewOut1,LHS2,NewOut,NewLHS2),
  TransClause = [create_object1(Info,rhs(creation_step1(Step,Type,New1)), lhs(Precond1)),
               create_object2(Info,rhs(creation_step2(Step,Type,New2)), lhs(Precond2))],!.

% mutiple preconds
trans_rule(Info,[In,In2|InL],OutL,TransClause):- is_object(In),is_object(In2),
  trans_rule(Info,[In2|InL],OutL,TransClauseM), TransClauseM\==[],
  sub_cmpd(lhs(Precond),TransClauseM),
  noteable_propdiffs(In,OutL,Same,_L,_R),
  append_vsets([Precond,Same],NewPrecond),
  subst(TransClauseM,lhs(Precond),lhs(NewPrecond),TransClause),!.


% copy/transform 
trans_rule(Info,In,Out,Clauses):- 
  noteable_propdiffs(In,Out,_Same,_L,R), R\==[],
  into_lhs(In,LHS),  
  findall(edit_copy(Info,rhs(edit(Type,Change,P)),lhs(LHS)),
    (member(P,R),prop_pairs(In,Out,Type,Change,P),
      Change\==same,
      good_for_rhs(P)),Clauses),Clauses\==[],!.

% just copy an object
trans_rule(Info,In,Out,Clauses):- 
  noteable_propdiffs(In,Out,_Same,_L,R), R==[],
  into_lhs(In,LHS),  
  Clauses = [edit_copy(Info,rhs(edit(instance,copy,same)),lhs(LHS))],!.



trans_rule(Info,E1,E2,Clauses):-
  noteable_propdiffs(E1,E2,Same,NL,NR),
  %pp_ilp(l2r(Info,E1,E2)),
  D=1,
  prefix_spaces(D,
  ((
   pp_obj_tree(D+1,Info,E1,E2),
     dash_chars,
      if_t(how_are_differnt(E1,E2,Set),pp_ilp(how_are_differnt=Set)),
      % flat_props(E1,FP1),flat_props(E2,FP2), intersection(FP1,FP2,Same,InFlatP,OutPFlat),pp_ilp(added=OutPFlat),pp_ilp(removing=InFlatP),pp_ilp(sames=Same),
      pp_ilp(info=Info), pp_ilp(removing=NL), pp_ilp(nsames=Same), pp_ilp(adding=NR),
      dash_chars))),
  sub_cmpd(step(Step),Info), 
  sub_cmpd(type(TypeO),Info),
  Clauses = [ 
    create_object_step(Info,rhs(create3c(Step,TypeO,E2)),lhs(Same)) ],!.
    %copy_if_match(Info,rhs(copy_step(Step,TypeO)),lhs(Same)) ].

trans_rule(Info,[In],Out,Clauses):- 
  %noteable_propdiffs(In,Out,Same,L,R),
 must_det_ll((into_lhs(In,LHS))),
  must_det_ll((sub_cmpd(step(Step),Info))), 
  must_det_ll((sub_cmpd(type(TypeO),Info))),
  must_det_ll((Clauses = [edit_copy(Info,rhs(create3c(Step,TypeO,Out)),lhs(LHS))])),!.



% Specific to general induction algorithm
% run_specific():-Calls specific_to_general with both H and N initialized to []

run_specific :- specific_to_general([],[]).
% specific_to_general(List_of_hypotheses, List_of_negatives) :- 
%	This is the top level control loop. It reads in a new positive or
% 	negative instance and calls process to update List_of_hypotheses
%	and List_of_negatives. 

specific_to_general(H, N) :-
	write("H = "), write(H),nl,
	write("N = "), write(N),nl,
	write("Enter Instance "),
	read(Instance),
	process(Instance, H, N, Updated_H, Updated_N),
	specific_to_general(Updated_H, Updated_N).

% process(Instance, List_of_hypotheses, List_of_negatives, 
%	Updated_hypotheses, Updated_negatives) :-
%	updates List_of_hypotheses and List_of_negatives in response to
%	Instance.  

process(positive(Instance), [], N, [Instance], N). 
% Initialize H
process(positive(Instance), H, N, Updated_H, N) :- 
	generalize_set(H,Gen_H, Instance),
	delete(X, Gen_H, (member(Y, Gen_H), more_general(X, Y)), Pruned_H),
	delete(X, Pruned_H, (member(Y, N), covers(X, Y)), Updated_H).	

process(negative(Instance), H, N, Updated_H, [Instance|N]) :- 
		delete(X, H, covers(X, Instance), Updated_H).

process(Input, H, N, H, N):- 
	Input \= positive(_),
	Input \= negative(_),
	write("Enter either positive(Instance) or negative(Instance) "), nl.

% 

generalize_set([], [], _).

generalize_set([Hypothesis|Rest],Updated_H,Instance):-	
	not(covers(Hypothesis, Instance)),
	(bagof(X, generalize(Hypothesis, Instance, X), Updated_head); Updated_head = []),
	generalize_set(Rest,Updated_rest, Instance),
	append(Updated_head, Updated_rest, Updated_H).

generalize_set([Hypothesis|Rest],[Hypothesis|Updated_rest],Instance):-
	covers(Hypothesis, Instance),
	generalize_set(Rest,Updated_rest, Instance).
	
%

generalize([],[],[]).	
generalize([Feature|Rest], [Inst_prop|Rest_inst], [Feature|Rest_gen]) :-
	not(Feature \= Inst_prop),
	generalize(Rest, Rest_inst, Rest_gen).
generalize([Feature|Rest], [Inst_prop|Rest_inst], [_|Rest_gen]) :-
	Feature \= Inst_prop,
	generalize(Rest, Rest_inst, Rest_gen).

% more_general(Feature_vector_1, Feature_vector_2) :- succeeds if
%	Feature_vector_1 is strictly more general than Feature_vector_2
more_general(X, Y) :-  not(covers(Y, X)), covers(X, Y).

% covers(Feature_list_1, Feature_list_2) :- Succeeds if Feature_list_1
%	covers Feature_list_2.  Note that covers, unlike unification is
%	not symmetric: variables in Feature_list_2 will not match constants
%	in Feature_list_1.

covers([],[]).
covers([H1|T1], [H2|T2]) :-
	var(H1), var(H2), 
	covers(T1, T2).
covers([H1|T1], [H2|T2]) :-
	var(H1), atom(H2), 
	covers(T1, T2).	
covers([H1|T1], [H2|T2]) :-
	atom(H1), atom(H2), H1 = H2,
	covers(T1, T2).

% delete(Element, List1, Goal, List2) :- List2 contains all bindings
%	of Element to a member of List1 except those that cause 
%	Goal to succeed

delete(X, L, Goal, New_L) :-
	(bagof(X, (member(X, L), not(Goal)), New_L);New_L = []).
  










