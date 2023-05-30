/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


:- include(kaggle_arc_header).

call_list(List):-is_list(List),!,my_maplist(call_list,List).
call_list(Goal):-ignore(Goal).

:- dynamic(is_for_ilp/4).
:- abolish(ac_db_unit/4).
:- dynamic(ac_db_unit/4).
clear_scene_rules(TestID):-   
  abolish(ac_db_unit/4),dynamic(ac_db_unit/4),
  abolish(arc_cache:trans_rule_db/4),dynamic(arc_cache:trans_rule_db/4),
  %forall(ac_unit(TestID,Ctx,P,PSame),ignore(retract(ac_unit(TestID,Ctx,P,PSame)))),!,
  clear_object_dependancy(TestID).


% Define predicates that shouldn't be noticed
%dont_notice(global2G(_,_)).
dont_notice(giz(_)).
dont_notice(iz(i_o(_))).
dont_notice(iz(stype(_))).

dont_notice(simularz(_,_)).
dont_notice(global2G(_,_)).
dont_notice(iz(symmetry_type(rollD, _))).
dont_notice(link(contains,_)).
dont_notice(links_count(sees, _)).
dont_notice(occurs_in_links(contained_by,_)).
dont_notice(occurs_in_links(sees,_)).
dont_notice(oid(_)).
dont_notice(pg(_,pen(_), rankLS ,_)).
dont_notice(pg(_,iz(_),rankLS,_)).
dont_notice(pg(_,empty_area(_),rankLS,_)).

%dont_notice(pg(_, iz(_), rankLS, largest)).
%dont_notice(link(sees(_),_)).
%dont_notice(links_count(sees,_)).
%dont_notice(occurs_in_links(sees,_)).
dont_notice(P):- compound(P),arg(_,P,E),is_gridoid(E),!.
dont_notice(P):- compound(P),!,compound_name_arity(P,F,_),!,dont_notice(F).
dont_notice(P):- compound(P),arg(_,P,E),E==norm,!,fail.
dont_notice(F):- \+ atom(F),!,fail.
dont_notice(oid).
dont_notice(giz).
dont_notice(shape_rep).
dont_notice(grid_rep).

% Define predicates that should be noticed
do_notice(pg(_,_,rank1,_)).
do_notice(pg(_,_,_,_)).
do_notice(sym_counts(_,_)).

% Predicate to check if P should be noticed
ok_notice(P):- \+ \+ do_notice(P),!.
ok_notice(P):- \+ dont_notice(P).


dont_deduce(link(sees(_),_)).
%dont_deduce(giz(_)).
%dont_deduce(pg(_,_,_,_)).
dont_deduce(pg(_,iz(_),rankLS,_)).
dont_deduce(pg(_,_,rankLS,_)).
dont_deduce(size2D(_)).
%dont_deduce(global2G(_,_)).
dont_deduce(vis2D(_,_)).
dont_deduce(P):- \+ compound(P),!,fail.
dont_deduce(P):- sub_term(G,P),compound(G),is_grid(G),!.
dont_deduce(P):- sub_term(G,P),compound(G),is_object(G),!.
dont_deduce(grid(_)).
%dont_deduce(iz(_)).
dont_deduce(iz(_)).

%dont_deduce(P):- compound(P),compound_name_arguments(P,_,[X]),number(X).
dont_deduce(grid_ops(comp,_)). 
%dont_deduce(iz(stype(_))). 
dont_deduce(iz(symmetry_type(_,_))). % rot2D(rot90),grid_ops(comp,[]),changes([]),iz(fg_or_bg(iz_fg)),links_count(contained_by,0),links_count(contains,0),cc(plain_var,0),cc(bg,0),global2G(9,9),iz(sizeGX(1)),unique_colors_count(1),empty_area(0),iz(algo_sid(comp,sid_12)),iz(algo_sid(norm,sid_12)),iz(symmetry_type(flipDHV,false)),iz(symmetry_type(rot180,true)),iz(symmetry_type(flipV,true)),iz(symmetry_type(flipH,true)),iz(symmetry_type(rot270,false)),iz(symmetry_type(rot90,false)),iz(symmetry_type(sym_h_xor_v,false)),iz(symmetry_type(sym_hv,true)),iz(filltype(solid)),iz(colormass),iz(media(shaped)),iz(info(birth(colormass))),pg(_1477530,mass(_1477540),rankLS,largest),pg(_1477550,iz(sizeGX(_1477564)),rankLS,smallest),pg(_1477574,iz(sizeGY(_1477588)),rankLS,largest),pg(_1477598,iz(cenGX(_1477612)),rankLS,largest),pg(_1477622,iz(cenGY(_1477636)),rankLS,largest),pg(_1477646,unique_colors_count(_1477656),rankLS,smallest),pg(_1477666,empty_area(_1477676),rankLS,smallest).
%dont_deduce(mass(2)). % center2G(2,9),vis2D(1,2),loc2D(2,8),grid_ops(norm,[rot90]),link(sees([cc(e,2)]),o_?_459_t_08ed6ac7_trn_1_out),cc(fg,2),iz(sizeGY(2)),iz(cenGY(9)),rotSize2D(grav,2,1),area(2),iz(sid(sid_12)),\+link(sees([cc(w,2)]),o_i_109_t_08ed6ac7_trn_0_out),\+link(sees([cc(w,2)]),o_?_641_t_08ed6ac7_trn_0_out),\+link(sees([cc(w,2)]),o_?_337_t_08ed6ac7_trn_0_out),\+link(sees([cc(w,2)]),o_Z_24_t_08ed6ac7_trn_1_out),\+link(sees([cc(w,2)]),o_?_459_t_08ed6ac7_trn_1_out).
dont_deduce(oid(_)). % center2G(2,9),vis2D(1,2),loc2D(2,8),mass(2),grid_ops(norm,[rot90]),link(sees([cc(e,2)]),o_?_459_t_08ed6ac7_trn_1_out),cc(fg,2),iz(sizeGY(2)),iz(cenGY(9)),rotSize2D(grav,2,1),area(2),iz(sid(sid_12)),\+link(sees([cc(w,2)]),o_i_109_t_08ed6ac7_trn_0_out),\+link(sees([cc(w,2)]),o_?_641_t_08ed6ac7_trn_0_out),\+link(sees([cc(w,2)]),o_?_337_t_08ed6ac7_trn_0_out),\+link(sees([cc(w,2)]),o_?_532_t_08ed6ac7_trn_1_out),\+link(sees([cc(w,2)]),o_Z_24_t_08ed6ac7_trn_1_out),\+link(sees([cc(w,2)]),o_?_459_t_08ed6ac7_trn_1_out).
dont_deduce(cc(plain_var,0)).
dont_deduce(links_count(_,_)).
dont_deduce(empty_area(_)).
dont_deduce(unique_colors_count(_)).


% Define predicates that should be deduced
do_deduce(link(sees(_),_)).
do_deduce(rot2D(_)).
do_deduce(pen(_)).
do_deduce(iz(sid(_))).
do_deduce(iz(X)):- !,do_deduce(X),!.
do_deduce(P):- compound(P),compound_name_arguments(P,_,[X,Y]),number(X),number(Y).
%do_deduce(P):- compound(P),compound_name_arguments(P,_,[X,Y]),comparable_value(X),comparable_value(Y).
do_deduce(rotSize2D(grav,_,_)).
do_deduce(grid_rep(norm,_)). % pen([cc(blue,1)]),pg(_1489874,mass(_1489884),rank1,4).
do_deduce(grid_ops(norm,_)). % pen([cc(blue,1)]),pg(_1489874,mass(_1489884),rank1,4).

% Predicate to check if P should be deduced
ok_deduce(obj(L)):- nonvar(L),!.
ok_deduce(P):- \+ \+ dont_deduce(P), !, fail.
ok_deduce(P):- \+ \+ do_deduce(P),!.
ok_deduce(P):- good_for_rhs(P),!.
%ok_deduce(P):- \+ \+ dont_notice(P),!,fail.









% Check if two values have the same property names but are not equal

%into_rhs(edit(_,_,_,R),P):- !, into_rhs(R,P).
maybe_deref_value(X1,_):- \+ compound(X1),!,fail.
maybe_deref_value(edit(_,_,E1),Y1):- ensure_deref_value(E1,Y1).
maybe_deref_value(edit(_,_,_,E1),Y1):- ensure_deref_value(E1,Y1).
%maybe_deref_value(X1,Y1):- compound(X1), once(into_rhs(X1,E1)), E1\=@=X1,!,ensure_deref_value(E1,Y1).

ensure_deref_value(X1,E1):- maybe_deref_value(X1,E1),!.
ensure_deref_value(X1,X1).

other_val(X1,X2):- maybe_deref_value(X1,E1), !, other_val(E1,X2).
other_val(X2,X1):- maybe_deref_value(X1,E1), !, other_val(X2,E1).

other_val(X1,X2):- negated_s_lit(X1,P1), 
  ( negated_s_lit(X2,P2) -> other_val(P1,P2) ; other_val(X2,P1)).

other_val(X1,X2):- \+ same_prop_names(X1,X2),!,fail.
other_val(X1,X2):- X1=@=X2,!,fail.
other_val(X1,X2):- once((selfless_type(X1,V1),selfless_type(X2,V2))), \=@=(V1,V2),!,fail.
other_val(X1,X2):- is_color_prop(X1),is_color_prop(X2),once((specific_value(X1,V1),specific_value(X2,V2))), \+ other_val_same_types(V1,V2),!,fail.
other_val(_,_).

is_color_prop(X1):- fail,compound(X1),X1=pen(_).

other_val_same_types(X1,X2):- once((selfless_type(X1,V1),selfless_type(X2,V2))), \=@=(V1,V2),!,fail.
other_val_same_types(X1,X2):- X1=@=X2,!,fail.
other_val_same_types(_,_).

selfless_type(V1,T1):- data_type(V1,T),subst(T,V1,v,T1).


specific_value(X,V):- sub_term(V,X),V\=X,comparable_value(V).


same_prop_names(X1,X2):- maybe_deref_value(X1,E1), !, same_prop_names(E1,X2).
same_prop_names(X2,X1):- maybe_deref_value(X1,E1), !, same_prop_names(X2,E1).
same_prop_names(X1,X2):- 
  compound(X1),compound(X2), same_functor(X1,X2),!,
  make_unifiable_u(X1,U1), make_unifiable_u(X2,U2),!,  U1 =@= U2.

% Helper predicate to create a unifiable version of a term
make_unifiable_u(X1,X2):- maybe_deref_value(X1,E1), !, make_unifiable_u(E1,X2).
make_unifiable_u(P,U):- copy_term(P,PP),make_unifiable_u1(PP,U),!.
make_unifiable_u1(Atom,U):- is_ftVar(Atom),!,Atom=U.
make_unifiable_u1(Atom,U):- atomic(Atom),!,freeze(U,atomic(U)).
make_unifiable_u1(link(sees(L),A),link(sees(U),B)):- !, maplist(make_unifiable_u,[A|L],[B|U]),!.

make_unifiable_u1(P,U):- assume_prop(P),!,P=U.
make_unifiable_u1(X1,U1):- make_unifiable_cc(X1,U1),!.
make_unifiable_u1(X1,X1).

make_unifiable_ov(I,O):- make_unifiable_u(I,O),!.

make_unifiable_f(I,O):- make_unifiable_ov(I,O).
make_unifiable_f(I,O):- same_functor(I,O),!.


same_context(IO,Ctx):- nonvar(Ctx),!,io_to_cntx1(Out,In_Out_Out),once(Ctx==In_Out_Out;Ctx==Out),!, (once(IO=In_Out_Out;IO=Out)).
same_context(IO,Ctx):- nonvar(IO),!, io_to_cntx1(Out,In_Out_Out), once(IO==In_Out_Out;IO ==Out),!, (once(Ctx=In_Out_Out;Ctx=Out)).
same_context(IO,Ctx):- freeze(IO,same_context(IO,Ctx)),freeze(Ctx,same_context(IO,Ctx)).

io_to_cntx(IO,Ctx):- io_to_cntx1(IO,Ctx).
io_to_cntx1(in,in_out).
%io_to_cntx1(in,in_out_out).
io_to_cntx1(out,in_out_out).
io_to_cntx1(out,s(_)).
io_to_cntx1(X,X).

when_entire_suite(Goal,_Goal2):- get_pair_mode(entire_suite),!, call(Goal).
when_entire_suite(_Goal,Goal2):- call(Goal2).

repress_some_output(Goal):- 
  when_entire_suite(with_pair_mode(whole_test,repress_output(Goal)),Goal).
repress_output(Goal):- menu_or_upper('I'), !, call(Goal).
repress_output(Goal):- print_collapsed(200, wots(_,Goal)).


%repress_output(Goal):- call(Goal).


solve_via_scene_change:-  get_pair_mode(entire_suite),!, cls, 
 forall_count(all_arc_test_name(TestID),
   solve_via_scene_change(TestID)).

solve_via_scene_change:-  \+ get_pair_mode(whole_test),!,
 clsmake, ensure_test(TestID), 
 fresh_solve_via_scene_change(TestID).

solve_via_scene_change:-  clsmake, ensure_test(TestID), 
 solve_via_scene_change(TestID).

fresh_solve_via_scene_change(TestID):-  
  force_clear_test(TestID),
  %detect_pair_hints(TestID),
  %save_test_hints_now(TestID),
  clear_scene_rules(TestID),
  solve_via_scene_change(TestID).

print_informative_pairs(TestID):-
   locally(nb_setval('$grid_mode',informative_pairs),
    forall(kaggle_arc(TestID,ExampleNum,In,Out),
       print_single_pair(TestID,ExampleNum,In,Out))),!.


solve_via_scene_change(TestID):-  
 must_det_ll((
  print_test(TestID),
 % print_informative_pairs(TestID),
  print_individuals(TestID),
  learn_via_grid_change(TestID),
  %print_object_dependancy(TestID),
  %rtrace,
  ExampleNum=tst+_,
  true)),
  forall(current_example_scope(TestID,ExampleNum),
     solve_via_scene_change_rules(TestID,ExampleNum)).


learn_via_grid_change(TestID):- 
 repress_output((
  must_det_ll(( %  detect_pair_hints(TestID),   %  save_test_hints_now(TestID),
   learn_grid_size(TestID))))),
 %ensure_individuals(TestID),
 %print_individuals(TestID),
 repress_some_output((
  must_det_ll(( 
   not_warn_skip(ensure_propcounts(TestID)),   %clear_scene_rules(TestID),
  time(( %synth_one_scene_example(TestID),
    compute_rest_of_scene_change(TestID))))))).

get_each_ndividuator(IndvSMode):- get_indivs_mode(IndvSMode), IndvSMode\==complete,!.
get_each_ndividuator(IndvSMode):-
  findall(IndvSMode,(
           (toplevel_individuation(TL),IndvSMode=[TL,leftover_as_one,do_ending])
            ;get_indivs_mode(IndvSMode)),List),
  list_to_set(List,Set),!,member(IndvSMode,Set).

:- discontiguous toplevel_individuation/1. 
:- multifile toplevel_individuation/1. 
toplevel_individuation(IndvSMode):-
   member(IndvSMode,[
     leftover_as_one,
     %must_indv_omem_points,
     skip_some,
     i_pbox,
     find_hybrid_shapes]).

ensure_individuals(TestID,ExampleNum):- \+ ground(ExampleNum),!,
  forall( kaggle_arc(TestID,ExampleNum,_,_),
    ensure_individuals(TestID,ExampleNum)).
ensure_individuals(TestID,ExampleNum):- 
  \+ \+ arc_cache:individuated_cache(TestID,TestID>ExampleNum*_,_,[_,do_ending],_Out),!.
ensure_individuals(TestID,ExampleNum):- 
  time(with_individuated_cache(true,
     forall( kaggle_arc(TestID,ExampleNum,GridIn0,GridOut0),
      (duplicate_term(GridIn0+GridOut0,GridIn+GridOut),
        ensure_individuals(TestID,ExampleNum,GridIn,GridOut))))).


ensure_individuals(TestID,ExampleNum,GridIn,GridOut):- 
       name_the_pair(TestID,ExampleNum,GridIn,GridOut,_PairName),
  gid_of_tid(GID1,TestID,ExampleNum,in),
  gid_of_tid(GID2,TestID,ExampleNum,out),
       forall(get_each_ndividuator(IndvSMode),
    (with_indivs_mode(IndvSMode,(( 
              with_task_pairs(TestID,ExampleNum,GridIn,GridOut, 
           %repress_output
           (duplicate_term(GridIn+GridOut,GridIn0+GridOut0),
            individuate_pair(IndvSMode,GridIn0,GridOut0,InC,OutC),
            sformat(Title,'~q',[individuate_pair(GID1,GID2,IndvSMode)]),
            print_ss(Title,InC,OutC)))))))),
  %get_indivs_mode(IndvSMode), %ndividuator(TestID,ExampleNum,IndvSMode,GridIn,GridOut),
    FinalIndvSMode = complete,
     repress_output(individuate_pair(FinalIndvSMode,GridIn,GridOut,_,_)),!.



print_individuals(_TestID):-!.
print_individuals(TestID):-
 must_det_ll((
 ensure_test(TestID),
   ignore((never_entire_suite,set_flag(indiv,0))),%compute_and_show_test_hints(TestID),
   forall(kaggle_arc(TestID,ExampleNum,_,_), 
      ignore(ensure_individuals(TestID,ExampleNum))),
   banner_lines(orange,10),
 forall(kaggle_arc(TestID,ExampleNum,_,_),
      ignore(print_individuals(TestID,ExampleNum))),
   banner_lines(blue,10),
   forall(kaggle_arc(TestID,ExampleNum,_,_), 
      ignore(print_individual_objects(TestID,ExampleNum))),
   banner_lines(blue,10))).

print_individuals(TestID,ExampleNum):-
 must_det_ll((
              gid_of_tid(GID1,TestID,ExampleNum,in),
              gid_of_tid(GID2,TestID,ExampleNum,out),
%   ignore(ensure_individuals(TestID,ExampleNum)),
   findall(wqs([Len,ROptions,GID1])=In,
     (arc_cache:individuated_cache(TestID,TestID>ExampleNum*in,GID1,ROptions,In),
      length(In,Len)),InL),sort(InL,InS),
   findall(wqs([Len,ROptions,GID2])=Out,
      (arc_cache:individuated_cache(TestID,TestID>ExampleNum*out,GID2,ROptions,Out),
       length(Out,Len)),OutL),sort(OutL,OutS),!,
   print_individuals_paired(InS,OutS),
   dash_chars)).

print_individual_objects(TestID,ExampleNum):-
%   ignore(ensure_individuals(TestID,ExampleNum)),
   findall(Obj, objects_of(TestID,ExampleNum,in,_,_,Obj),InL),mass_sort(InL,InS),
   findall(Obj, objects_of(TestID,ExampleNum,out,_,_,Obj),OutL),mass_sort(OutL,OutS),!,
   print_ss(allobjects,InS,OutS),
   objects_into_grids(InS,GridsIn),
   objects_into_grids(OutS,GridsOut),
   print_individuals_paired(GridsIn,GridsOut),
   dash_chars.

mass_sort(L,S):- predsort(sort_on(mass_for_sort),L,S).

mass_for_sort(Obj,Mass):- mass(Obj,M),vis2D(Obj,H,V),mass_for_sort(Obj,H,V,M,Mass),!.

mass_for_sort(Obj, H,V,Mass, 4+Area+Mass):- is_bg_object(Obj),!,Area is H*V.
mass_for_sort( _,  H,V,   1, 2+   1+Area):- Area is H*V.
mass_for_sort(Obj, H,V,Mass, 1+Area+Nass):- Nass is -Mass, is_fg_object(Obj),Area is H*V.



objects_into_grids([],[]):-!.
objects_into_grids(InL,[GridObjs|GridL]):-
  gather_one_grid(InL,[],RemainingObjs,GridObjs),
  objects_into_grids(RemainingObjs,GridL),!.
objects_into_grids(Objs,Objs).

gather_one_grid([I|InR],CurGrid,RemainingObjs,[I|GridObjs]):-
   once((globalpoints(I,Ps), include(is_fg_point,Ps,FGP))),
   \+ (member(P1,FGP),member(P1,CurGrid)), !,
  append(Ps,CurGrid,MoreGrid),
  gather_one_grid(InR,MoreGrid,RemainingObjs,GridObjs).
gather_one_grid(InL,CurGrid,RemainingObjs,[I|GridObjs]):- InL\==[],
  select(I,InL,InR), once((globalpoints(I,Ps), include(is_fg_point,Ps,FGP))),
   \+ (member(P1,FGP),member(P1,CurGrid)), !,
  append(Ps,CurGrid,MoreGrid),
  gather_one_grid(InR,MoreGrid,RemainingObjs,GridObjs).
gather_one_grid(RemainingObjs,_GridPoints,RemainingObjs,[]).



objects_of(TestID,ExampleNum,IO,GID,ROptions,Obj):-
 is_why_grouped_g(TestID,_Len,individuate(GID, ROptions),OutC),
 member(Out,OutC),once(into_obj(Out,Obj)),
 \+ \+ sub_var(g(IO),Obj), \+ \+ sub_var(ExampleNum,Obj).

print_wio(I=In,O=Out):-  print_side_by_side(yellow,In,I,_,Out,O).
print_individuals_paired([In],[Out]):- !, print_wio(In,Out).
print_individuals_paired([],Out):- !, print_ss(Out).
print_individuals_paired(In,[]):-!, print_ss(In).
print_individuals_paired(InS,OutS):-
 ( member(Selector,[I1=O1,I2=O2,true]),
  select(In,InS,InR), select(Out,OutS,OutR), 
  (In = (wqs([I1,I2|_])=_I3)),
  (Out = (wqs([O1,O2|_])=_O3)),
  call(Selector)),
  print_wio(In,Out),!,
  print_individuals_paired(InR,OutR).


into_input_objects(TestID,ExampleNum,IO,In,Objs,VM):-
  must_det_ll((
        once((obj_group5(TestID,ExampleNum,IO,ROptions,TempObjs),TempObjs\==[])),
        grid_to_tid(In,TID),
        into_fti(TID,ROptions,In,VM),
        individuate(VM),
        Objs = VM.objs)).

solve_via_scene_change_rules(TestID,ExampleNum):-
 
 must_det_ll((
    kaggle_arc(TestID,ExampleNum,In,Expected),
    duplicate_term(In,InOrig),  
    into_input_objects(TestID,ExampleNum,in,In,Objs,VM),
    % predict_grid_size_now(TestID,In,PX,PY),
    print_object_dependancy(TestID))),
 must_det_ll((
    repress_some_output(print_scene_change_rules_if_different(solve_via_scene_change_rules,ac_unit,TestID)),
    print_ss(wqs(expected_answer(ExampleNum)),Objs,Expected), dash_chars)),!,

 repress_some_output( once(enter_solve_obj(VM,TestID,ExampleNum,Objs,ObjsO))),

 must_det_ll((
  dash_chars,
  print_ss(wqs(solve_via_scene_change_rules(ExampleNum)),Objs,ObjsO),
  dash_chars,
  into_solid_grid(ObjsO,OurSolution1),
  maybe_resize_our_solution(TestID,In,OurSolution1,OurSolution),
  into_solid_grid(Expected,ExpectedOut),
  count_difs(ExpectedOut,OurSolution,Errors))),
 !,
  (Errors == 0 ->  
   (nop((when_entire_suite(banner_lines(cyan,1),banner_lines(green,4)))),
    print_ss(wqs(solve_via_scene_change(TestID,ExampleNum,errors=Errors)),ExpectedOut,OurSolution),
    print_scene_change_rules(rules_at_time_of_success(green),TestID),force_report_count(1),
    true)
    ;(banner_lines(red,10),!,
      %show_time_of_failure(TestID),
      banner_lines(red,10),
      print_scene_change_rules(rules_at_time_of_failure(red),TestID),
      print_ss(wqs(input(TestID,ExampleNum,errors=Errors)),InOrig,Objs),
      print_ss(wqs(expected(TestID,ExampleNum,errors=Errors)),ExpectedOut,ObjsO),
      print_ss(wqs(objects(TestID,ExampleNum,errors=Errors)),OurSolution,ExpectedOut),
      banner_lines(red,10),
      force_report_count_plus(-1),!,
      %when_entire_suite(print_test(TestID),true),
      banner_lines(red,1),!,fail,
      %if_t((findall(_,ac_unit(_,_,_,_),L), L == []), (get_scene_change_rules(TestID,pass2_rule_new,Rules),pp_ilp(Rules))),banner_lines(red,5),
      %print_object_dependancy(TestID),
      % only really fail for tests
      (ExampleNum == tst+_) -> (!,fail); true)).
      

resize_our_solution(PX,PY,OurSolution1,OurSolution):-
  once(ground(PX+PY)
     ->resize_grid(PX,PY,OurSolution1,OurSolution)
      ;notrace(=(OurSolution1,OurSolution));notrace(trim_outside2(OurSolution1,OurSolution))).

maybe_resize_our_solution(TestID,In,OurSolution1,OurSolution):-
  predict_grid_size_now(TestID,In,PX,PY),resize_our_solution(PX,PY,OurSolution1,OurSolution),!.


enter_solve_obj(VM,TestID,ExampleNum,Objs,ObjsO):- solve_obj_group(VM,TestID,ExampleNum,in_out,Objs,ObjsO),!.

enter_solve_obj(VM,TestID,ExampleNum,Objs,ObjsO):- 
  solve_obj_group(VM,TestID,ExampleNum,in_out,Objs,ObjsM1),
  solve_obj_group(VM,TestID,ExampleNum,in_out_out,ObjsM1,ObjsM2),
  solve_obj_group(VM,TestID,ExampleNum,s(_),ObjsM2,ObjsO), ObjsO \==[],!.

score_rule(Ways,Obj,Rule,Score):- is_object(Rule), \+ is_object(Obj),!,score_rule(Ways,Rule,Obj,Score).

score_rule(Ways,Obj,Rule,Score):- 
  into_lhs(Rule,PCond), into_rhs(Rule,P), 
  % indv_props_list(Obj,Props), \+ member(P,Props), %\+ \+ ((member(E,Props),member(E,PCond))),
 %  once( ( \+ is_bg_object(Obj) ); sub_var(black,PCond)),
    score_rule(Ways,Obj,PCond,P,Score).

%score_rule(_,Obj,PCond,_P,Score):- forall(member(Prop,PCond),inv_has_prop(Obj,Prop)),!,Score=2000.
score_rule(exact,Obj,PCond,_P,Score):-  score_all_props(PCond,Obj,S0),!, ((S0>0.3)->Score=1000;Score=0).
%score_rule(_MaybeExact,_Obj,_PCond,_P,0).
score_rule(_Ways,Obj,PCond,_P,Score):- %fail,
   obj_atoms(Obj,A),
   obj_atoms(PCond,B),
     intersection(A,B,Good,_Extra,_Bad),
     length(Good,Score).

has_all_props(CanL,Obj):- maplist(inv_has_prop(Obj),CanL).
score_all_props(CanL,Obj,Score):- maplist(inv_has_prop_score(Obj),CanL,ScoreL),sumlist(ScoreL,Score),!.

assume_prop(P):- \+ \+ assume_prop1(P),!.
assume_prop(P):- \+ \+ assume_prop2(P),!.
assume_prop(P):- \+ \+ is_debug_info(P).

is_debug_info(Var):- \+ compound(Var),!,fail.
is_debug_info(info(_)).
is_debug_info(iz(P)):-!,is_debug_info(P).
%is_debug_info(was_oid(_)).
%not_assumed(P):- is_unbound_prop(P),!.
%not_assumed(P):- \+ assume_prop(P).

assume_prop1(P):- dont_notice(P).
assume_prop2(giz(_)).
assume_prop2(mv4b(_)).
assume_prop2(mv4a(_)).
assume_prop2(grid_sz(_)).
assume_prop2(global2G(_,_)).
assume_prop2(was_oid(_)).
assume_prop2(oid(_)).
assume_prop2(P):- compound(P),P=simularz(simularz(_),_).


max_prop_score(P,0.1):- assume_prop1(P),!.
max_prop_score(P,0.2):- assume_prop2(P),!.
max_prop_score(P,1.0):- ground(P),!.
max_prop_score(P,0.0):- is_unbound_prop(P),!.
max_prop_score(_,0.7).

inv_has_prop(Obj,Prop):- has_prop(Prop,Obj),!.
inv_has_prop(Obj,Prop):- inv_has_prop_score(Obj,Prop,Score),Score>0.

inv_has_prop_score(Obj,Prop, Score):- max_prop_score(Prop,Score), inv_has_prop2(Obj,Prop).

inv_has_prop2(_O,P):- \+ \+ assume_prop(P),!.
inv_has_prop2(Obj,pg(A,B,C,D)):- !, has_prop(Obj,pg(A,B,C,D)).
inv_has_prop2(Obj, \+ Prop):- !, \+ inv_has_prop(Obj,Prop).
inv_has_prop2(Obj,grid_ops(norm,Props)):- !, has_prop(grid_ops(norm,VProps),Obj),!,Props=@=VProps.
inv_has_prop2(Obj,grid_rep(norm,Props)):- !, has_prop(grid_rep(norm,VProps),Obj),!,Props=@=VProps.
inv_has_prop2(Obj,Prop):- has_prop(Prop,Obj).

match_ok(_,B):- plain_var(B),!.
match_ok(A,B):- \+ \+ A = B.

two_way_mapping(Ways,Obj,_Objs,Rules,Rule,Rules):-
  match_ok(Ways,exact),!,
  Res = res(Score,Rule),
  findall(Res,(member(Rule,Rules),score_rule(Ways,Obj,Rule,Score)),Results),
  sort(Results,ResultsSorted),
  last(ResultsSorted,Res),
  select(Rule,Rules,_RulesRest),!.
  %select(Obj,Objs,ObjsRest).

two_way_mapping(Ways,Obj,Objs,Rules,Rule,RulesRest):-
  \+ match_ok(Ways,exact),
   once((sort_by_jaccard(Obj,Rules,[Rule|RulesRest]),
   sort_by_jaccard(Rule,Objs,[PickedObj|_ObjsRest]))), 
    ((PickedObj == Obj)-> nop(match_ok(Ways,two_ways)) ; match_ok(Ways,one_way)),
  write_atoms_info(Ways,PickedObj),
  write_atoms_info(paired2,Rule),
  %maplist(write_atoms_info(leftover1),RulesRest),
  %maplist(write_atoms_info(leftover2),ObjsRest),
  !.
      
write_atoms_info(N,E):- obj_atoms(E,Atoms),!,%sort(Atoms,AE),
  nl,writeln(N=Atoms).

apply_rules_to_objects(_,_,_,[],[]):-!.
apply_rules_to_objects(_,_,[],_,[]):-!.


apply_rules_to_objects(Ways,Mapping,Rules,Objs,[apply(Rule,Obj)|More]):- 
   match_ok(Mapping,one_to_one),
   \+ match_ok(Ways,exact),

   two_way_mapping(two_way,Obj,Objs,Rules,Rule,RulesRest),

   select(Obj,Objs,ObjsRest),
   apply_rules_to_objects(Ways,Mapping,RulesRest,ObjsRest,More).

apply_rules_to_objects(Ways,Mapping,Rules,Objs,[apply(Rule,Obj)|More]):-
   match_ok(Mapping,each_object_once),
   select(Obj,Objs,ObjsRest),
  two_way_mapping(Ways,Obj,Objs,Rules,Rule,_),
   apply_rules_to_objects(Ways,Mapping,Rules,ObjsRest,More).

apply_rules_to_objects(Ways,Mapping,Rules,Objs,[apply(Rule,Obj)|More]):-
   match_ok(Mapping,each_rule_once),
   select(Rule,Rules,RulesRest),
   two_way_mapping(Ways,Rule,Rules,Objs,Obj,ObjRest),!,
   apply_rules_to_objects(Ways,Mapping,RulesRest,ObjRest,More).

apply_rules_to_objects(Ways,Mapping,[_|Rules],Objs,More):- 
 match_ok(Mapping,each_rule_once),!,
 apply_rules_to_objects(Ways,Mapping,Rules,Objs,More).

apply_rules_to_objects(Ways,Mapping,Rules,[_|Objs],More):-
 match_ok(Mapping,each_object_once),!,
 apply_rules_to_objects(Ways,Mapping,Rules,Objs,More).

apply_rules_to_objects(_Ways,_Mapping,_Rules,_Objs,[]).




solve_obj_group(VM,TestID,_ExampleNum,Ctx,ObjsIn,ObjsO):-
  my_exclude(is_bg_object_really,ObjsIn,Objs),
  (Rule = (Ctx:rhs(P):- obj_atoms(PCond))),
  %io_to_cntx(IO_,Ctx), 
  findall_vset_R(Rule,(ac_unit(TestID,Ctx,P,PCond)), Rules),
  member(Ways-Strategy,[exact-_,two_way-one_to_one,_-_]), 

  apply_rules_to_objects(Ways,Strategy,Rules,Objs,Todo), Todo\==[],
  %maplist(into_solid_grid_strings,Todo,PStr), unwonk_ansi(PStr,PPStr),pp_ilp((see_Strategy(Ways-Strategy)=PPStr)), 
  %Todo\==[],
  %maplist(pp_ilp(3),Todo),
  run_todo_output(VM,Todo,ObjsO),ObjsO\==[],!.


solve_obj_group(VM,TestID,_ExampleNum,Ctx,ObjsIn,ObjsO):-
  my_exclude(is_bg_object_really,ObjsIn,Objs),
  (Rule = (Ctx:rhs(P):- obj_atoms(PCond))),
  %io_to_cntx(IO_,Ctx), 
  findall_vset_R(Rule,(ac_unit(TestID,Ctx,P,PCond)), Rules),
  member(Ways-Strategy,[exact-_,two_way-one_to_one,_-_]), 
  apply_rules_to_objects(Ways,Strategy,Rules,Objs,Todo), Todo\==[],
  maplist(into_solid_grid_strings,Todo,PStr),
  unwonk_ansi(PStr,PPStr),
  pp_ilp((see_Strategy(Ways-Strategy)=PPStr)), Todo\==[],
  run_todo_output(VM,Todo,ObjsO),ObjsO\==[],!.

/*
solve_obj_group(_VM,TestID,_ExampleNum,Ctx,Objs,ObjsO):-
 must_det_ll((
  %PreObjsL,PostObjsL,_USame,_UPA2,_UPB2
  %map_pairs_info_io(TestID,_ExampleNum,Ctx,_Step,_TypeO,PreObjsL,PostObjsL,_USame,_UPA2,_UPB2),   
  CLS = prop_to_can(TestID,Ctx,SomeP,O,Can1,Cant,Preconds),
  findall(CLS,prop_to_can(TestID,Ctx,SomeP,O,Can1,Cant,Preconds),FwdRules),% prop_can(TestID,Ctx,SomeP,Preconds)
  maplist(apply_to_objs(Ctx,Objs),FwdRules,ObjsOFL),append(ObjsOFL,ObjsOF),
  flatten([ObjsOF],ObjsO),
  print_ss(Ctx,Objs,ObjsO))), ObjsO \==[],!.*/
solve_obj_group(_VM,_TestID,_ExampleNum,_Ctx,Objs,Objs).
/*
apply_to_objs(Ctx,Objs,CLS1,ObjsO):-
 must_det_ll((
  CLS1 = prop_to_can(_TestID,Ctx,SomeP,O,_Can1,Cant,Preconds),
  CLS2 = cl(Preconds,O,SomeP),
  include(can_cant_props(Preconds,Cant),Objs,SelectedObjects),
  %maybe_apply
  findall(NewObjs,
     (%member(CLS1,FwdRules),  
      maybe_apply(CLS2,SelectedObjects,NewObjs)), NewObjL),
  flatten([Objs,NewObjL],NewObjs),
  variant_list_to_set(NewObjs,ObjsO))),!.
*/
/*
solve_obj_group(_VM,TestID,_ExampleNum,Ctx,_ROptions,Objs,ObjsO):-
  CLS = cl(Preconds,O,Cant),
  findall(CLS,% prop_can(TestID,Ctx,SomeP,Preconds)
    (prop_to_can(TestID,IO_,P,_O,_Can1,Cant,Can),
     include(can_cant_props(Can,Cant),Objs,SelctedObjects),

   prop_to_can(TestID,Ctx,SomeP,O,_Can1,Cant,Preconds),FwdRules),
  findall(NewObjs,
     (member(CLS,FwdRules),  maybe_apply(CLS,Objs,NewObjs)),
     NewObjL),
  flatten([Objs,NewObjL],NewObjs),
  variant_list_to_set(NewObjs,ObjsO),!.
maybe_apply(CLS,Objs,NewObj):-   
  CLS = cl(Preconds,O,_SomeP),
  maplist(has_all_props(Preconds),Objs),
  NewObj=O,!.
maybe_apply(CLS,Objs,NewObj):-   
  CLS = cl(Preconds,O,_SomeP),
  select(Obj,Objs,Rest),member(Obj2,Rest),
  flat_props([Obj,Obj2],Props),
  intersection(Props,Preconds,Matched,Missing,_Extra),
  pp_ilp(matched=Matched),
  Missing==[],
  NewObj=O,!.

%copy_obj(Rules,Objs,_TestID,_ExampleNum,_IO_,_ROptions,Obj,Obj):- is_bg_object(Obj),!.
copy_obj(Rules,Objs,VM,_TestID,_EN,_Ctx,_ROptions,Obj,OObj):- 
 must_det_ll(( %print_grid(copy_obj,Obj),
    sort_by_jaccard(Obj,Rules,[Rule|_]),
    pp_ilp(Rule),
    print_grid(Obj),
    sort_by_jaccard(Rule,Objs,[_PickedObj|_]),
    edit_object(VM,Rule,Obj,OObj))).
    %print_grid(copy_obj(Rules,Objs,Ps),Obj,OObj))),!.
%copy_obj(Rules,Objs,_VM,_TestID,_ExampleNum,_Ctx,_ROptions,Obj,Obj).
   %map_pairs_info_io(TestID,_EN,Ctx,_Step,_TypeO,_A,_B,USame,UPA2,UPB2)
   

%              findall(l2r(Info,Pre,Post),arc_cache:map_pairs(TestID,_,_IO_2,Info,Pre,Post),List),
%              variant_list_to_set(List,Set)
 
copy_obj(_VM,_TestID,_ExampleNum,_IO_,_ROptions,Obj,Obj).
%solve_obj(VM,_TestID,_ExampleNum,IO_,_ROptions,Obj,OObj):-
%  edit_object(VM,pen([cc(black,1)]),Obj,OObj).
%solve_obj(VM,_TestID,_ExampleNum,IO_,_ROptions,_Obj,[]).
*/

run_todo_output(VM,[],NewObjs):- NewObjs = VM.objs,!.
run_todo_output(VM,[apply(Rule,Obj)|TODO],NewObjs):-
  edit_object(VM,Rule,Obj),
  run_todo_output(VM,TODO,NewObjs).

clone_object(I,O):- duplicate_term(I,O).

edit_object(_VM,Ps,_Obj):- Ps==[],!.
%edit_object(VM,Ps,Obj):- Ps==[],!,edit_object(VM,pen([cc(black,1)]),Obj).
edit_object(VM,[H|T],Obj):- !,edit_object(VM,H,Obj),edit_object(VM,T,Obj).
edit_object(VM,copy_step(_,perfect_in_out),Obj):- addRObjects(VM,Obj).
edit_object(VM,creation_step(_,_,Props),Obj):-
  clone_object(Obj,NewObj), edit_object(VM,Props,NewObj).
edit_object(VM,Ps,Obj):-
  must_det_ll((
   wots(SS,print(Ps)),
   override_object_1(VM,Ps,Obj,NewObj),
   remObjects(VM,Obj),
   addOGSObjects(VM,NewObj),
   addObjects(VM,NewObj),
   into_solid_grid([NewObj],SG),SG=_,
   dash_chars,
   print_ss(override_object(SS),[Obj],[NewObj]),
   nop((
   indv_props_list(Obj,PL1),
   indv_props_list(NewObj,PL2),
   intersection(PL1,PL2,_Same,Removed,Added),
    pp_ilp(([[removing=Removed],[adding=Added]])))))).

override_object_1(_VM,[],IO,IO):-!.
override_object_1(VM,[H|T],I,OO):- !, override_object_1(VM,H,I,M),!, override_object_1(VM,T,M,OO).
override_object_1(_VM,pen([cc(Red,N)]),Obj,NewObj):- pen(Obj,[cc(Was,N)]), !,
  subst001(Obj,Was,Red,NewObj),!.

% move too exterme (so we skip)
override_object_1(_VM,loc2D(X,Y),Obj,Obj):- (X>3;Y>3),!.
% move object
override_object_1(VM,loc2D(X,Y),Obj,NewObj):- loc2D(Obj,WX,WY),  
  globalpoints(Obj,WPoints),deoffset_points(WX,WY,WPoints,LPoints),  
  offset_points(X,Y,LPoints,GPoints),rebuild_from_globalpoints(VM,Obj,GPoints,NewObj).

% copy
override_object_1(_VM,Term,I,O):- sub_cmpd(edit(_,P),Term), var(P),!, I=O.
% copy + 1 edit
override_object_1(VM,Term,I,O):- sub_cmpd(edit(_,P),Term), !, override_object_1(VM,P,I,O).

% recusive fallbacks
override_object_1(VM,Term,I,O):- sub_cmpd(rhs(P),Term), !,  override_object_1(VM,P,I,O).
override_object_1(VM,Term,I,O):- sub_cmpd(edit(P),Term), !,  override_object_1(VM,P,I,O).
override_object_1(VM,Term,I,O):- sub_cmpd(edit(_,_,P),Term), !, override_object_1(VM,P,I,O).
%override_object_1(VM,Term,I,O):- sub_term(Sub,Term), compound(Sub),Sub=edit(_,_,P),  !, pp_ilp(Term), I=O,!. %override_object_1(VM,P,I,O).

override_object_1(_VM,O,I,OO):- override_object(O,I,OO),!.


mapping_step(    in_out).
mapping_step( in_in_out).
mapping_step(in_out_out).
mapping_step(   out_out).


p_of_post(P,Post):- indv_props_list(Post,Props),member(P,Props).



from_same_pair(Post,Pre):-
  has_prop(giz(example_num(trn+N)),Post),
  has_prop(giz(example_num(trn+N)),Pre).
     
     
obj_in_or_out(Pair,IO_):- is_mapping(Pair),!,
    get_mapping_info(Pair,Info,_In,_Out),arg(3,Info,IO_).
obj_in_or_out(Obj,IO_):- must_det_ll(is_object(Obj)),has_prop(giz(g(I_O)),Obj),!,I_O=IO_.
obj_in_or_out(Obj,IO_):- has_prop(iz(i_o(I_O)),Obj),!,I_O=IO_.
%obj_in_or_out(Obj,I_O):- is_input_object(Obj)-> IO_ =out ; IO_ =in.

is_pre_cond_obj(Obj,in_out):- obj_in_or_out(Obj,in).
is_pre_cond_obj(Obj,in_out_out):- obj_in_or_out(Obj,in);obj_in_or_out(Obj,out).
is_pre_cond_obj(Obj,in_in_out):- obj_in_or_out(Obj,in).
is_pre_cond_obj(Obj,s(X)):- nonvar(X), is_pre_cond_obj(Obj,out).
is_pre_cond_obj(Obj,IO_):- obj_in_or_out(Obj,IO_).
is_pre_cond_obj(Obj,in):- is_pre_cond_obj(Obj,in_out).


is_post_cond_obj(Obj,in_out):- obj_in_or_out(Obj,out).
is_post_cond_obj(Obj,in_out_out):- obj_in_or_out(Obj,out).
is_post_cond_obj(Obj,in_in_out):- obj_in_or_out(Obj,out).
is_post_cond_obj(Obj,s(X)):- nonvar(X), is_post_cond_obj(Obj,out).
is_post_cond_obj(Obj,out):- obj_in_or_out(Obj,out).
is_post_cond_obj(Obj,in):- is_post_cond_obj(Obj,in_out).







scope_training(ExampleNum):- 
  ignore((ExampleNum=(trn+_))),
  ignore((
    luser_getval(example,UExampleNum), 
      ignore(ExampleNum = UExampleNum))).
  


common_props([O|Objs],Props):-
   indv_props_list(O,List),
   findall(P,(member(P,List),\+ dont_notice(P),forall(member(E,Objs),has_prop(P,E))),Props).

current_example_scope(TestID,ExampleNum):- 
  (var(TestID)->get_current_test(TestID);true),!,
  scope_training(ExampleNum), 
  kaggle_arc(TestID,ExampleNum,_,_),
  (get_pair_mode(single_pair)->!;true).


save_how_io(HowIn,HowOut):- 
  get_current_test(TestID),save_how_io(TestID,HowIn,HowOut).
save_how_io(TestID,HowIn,HowOut):- 
  assert_test_property(TestID,common,indiv_how(in),HowIn),
  assert_test_property(TestID,common,indiv_how(out),HowOut),!.

obj_group_gg(TestID,ExampleNum,InC,OutC):- obj_group_pair(TestID,ExampleNum,InC,OutC).

obj_group_pair1(TestID,ExampleNum,InC,OutC):-
   current_example_scope(TestID,ExampleNum),
   no_repeats_var(OutC), % set_example_num(ExampleNum),
   obj_group5(TestID,ExampleNum,in,HowIn,InC), InC\==[],  length(InC,L),

   (((obj_group5(TestID,ExampleNum,out,HowOut,OOut),length(OOut,L),save_how_io(TestID,HowIn,HowOut)))
     ;obj_group5(TestID,ExampleNum,out,_,OOut)),   
   OutC = OOut.

obj_group_pair(TestID,ExampleNum,InC,OutC):-
   %findall(NV,(arc_test_property(TestID,ExampleNum,N,V),append_term(N,V,NV)),Props),
  
  obj_group_pair1(TestID,ExampleNum,InCC,OutCC),
  InC = InCC, OutC = OutCC.
  %Grid= VM.start_grid,
  %hv_point_value(1,1,Grid,PointNW),
  %hv_point_value(1,V,Grid,PointSW),
  %hv_point_value(H,1,Grid,PointNE),
  %hv_point_value(H,V,Grid,PointSE),
%%  once((kaggle_arc(TestID,ExampleNum,In,Out),grid_props(In,InProps),grid_props(Out,OutProps))),
%%  InC = [obj([giz(g(in))|InProps])|InCC], OutC = [obj([giz(g(out))|OutProps])|OutCC].
  %append(Props,[mass(0),vis2D(H,V),birth(named_grid_props),loc2D(1,1),iz(flag(always_keep)),iz(media(image)),iz(flag(hidden))],AllProps),
  %make_indiv_object(VM,AllProps,[PointNW,PointSW,PointNE,PointSE],_),!.
/*
objs_other_than_example(TestID,ExampleNum,InOut,Others):-
  findall(O,(current_example_scope(TestID,OExampleNum),
    ExampleNum\==OExampleNum,
    obj_group_io(TestID,OExampleNum,InOut,Objs),
    member(O,Objs)),Others).
*/
all_io_objs(TestID,InOut,Others):-
  findall(O,(current_example_scope(TestID,ExampleNum), 
   obj_group_io(TestID,ExampleNum,InOut,Objs), member(O,Objs)),Others).

with_individuated_cache(TF,Goal):- locally(nb_setval(use_individuated_cache,TF),Goal).

obj_group_io(TestID,ExampleNum,InOut,Objs):-
 arc_test_property(TestID,common,indiv_how(InOut),IndvSMode),!,
 current_example_scope(TestID,ExampleNum), 
 with_individuated_cache(true,
   once(obj_group5(TestID,ExampleNum,InOut,IndvSMode,Objs))).

obj_group_io(TestID,ExampleNum,InOut,Objs):- 
 current_example_scope(TestID,ExampleNum),
 with_individuated_cache(true,
   once(obj_group5(TestID,ExampleNum,InOut,_,Objs))).

obj_group5(TestID,ExampleNum,InOut,ROptions,Objs):- var(TestID),
  ensure_test(TestID),!,obj_group5(TestID,ExampleNum,InOut,ROptions,Objs).  
obj_group5(TestID,ExampleNum,InOut,ROptions,Objs):- var(ROptions),
 arc_test_property(TestID,common,indiv_how(InOut),ROptions),!,
 obj_group5(TestID,ExampleNum,InOut,ROptions,Objs).

obj_group5(TestID,ExampleNum,InOut,ROptions,Objs):-
  arc_cache:individuated_cache(TestID,TID,_GOID,ROptions,Objs), 
  once((sub_var(ExampleNum,TID),sub_var(InOut,TID))),!.

obj_group5(TestID,ExampleNum,InOut,ROptions,Objs):-
 kaggle_arc_io(TestID,ExampleNum,InOut,Grid),
  set_example_num(ExampleNum),
 other_grid(Grid,Other),
 with_other_grid(Other,
  
                 ((fail, arc_cache:individuated_cache(TestID,TID,GOID,ROptions,Objs), Objs\==[],
  once((testid_name_num_io_0(TID,_,Example,Num,InOut),
        testid_name_num_io_0(GOID,_,Example,Num,InOut))))*-> true ; grid_to_objs(Grid,ROptions,Objs))).

  
%show_object_dependancy(_TestID):-  !.
% =============================================================
show_object_dependancy(TestID):-  
% =============================================================
 ensure_test(TestID),
 %clear_object_dependancy(TestID),
 ensure_object_dependancy(TestID),
 print_object_dependancy(TestID).

% =============================================================
ensure_object_dependancy(TestID):-
% =============================================================
 ensure_test(TestID),
  must_det_ll((
  ensure_individuals(TestID),
 scope_training(ExampleNum),
 forall(current_example_scope(TestID,ExampleNum),
	  ensure_object_dependancy(TestID,ExampleNum)),
  merge_object_dependancy(TestID))).

/*
ensure_object_dependancy(TestID,ExampleNum):-
 %current_example_scope( TestID,ExampleNum),
   one_or_multiple(obj_group_pair(TestID,ExampleNum,LHSObjs,RHSObjs),
     ensure_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs)).

*/
ensure_object_dependancy(TestID,ExampleNum):-
 %current_example_scope( TestID,ExampleNum),
   forall(obj_group_pair(TestID,ExampleNum,LHSObjs,RHSObjs),
     ensure_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs)).



ensure_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs):-
  (var(TestID),var(RHSObjs),var(LHSObjs)),ensure_test(TestID),!,
  my_assertion(nonvar(TestID)),!,
  ensure_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs).

ensure_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs):-
  (var(ExampleNum),var(RHSObjs),var(LHSObjs),nonvar(TestID)),!,
  current_example_scope(TestID,ExampleNum),
  ensure_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs).

ensure_object_dependancy(TestID,ExampleNum,_RHSObjs,_LHSObjs):- % arc_cache:prop_dep(TestID,ExampleNum,_,_,_,_,_,_,_), 
 \+ \+ arc_cache:trans_rule_db(TestID,ExampleNum,_,_), !.

ensure_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs):-
  (var(RHSObjs);var(LHSObjs)), !, 
  obj_group_pair(TestID,ExampleNum,LHSObjs,RHSObjs),
  ensure_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs).

ensure_object_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs):-
  %my_assertion(((nonvar(RHSObjs),nonvar(LHSObjs)))),
  into_object_dependancy_r_l(TestID,ExampleNum,Ctx,RHSObjs,LHSObjs,Groups),
  assert_map_pairs(TestID,ExampleNum,Ctx,Groups).  

into_object_dependancy_r_l(TestID,ExampleNum,Ctx,RHSObjs,LHSObjs,Groups):-
  var(RHSObjs),var(LHSObjs),!,
  one_or_multiple(obj_group_pair(TestID,ExampleNum,LHSObjs,RHSObjs),
  into_object_dependancy_r_l(TestID,ExampleNum,Ctx,RHSObjs,LHSObjs,Groups)).

into_object_dependancy_r_l(TestID,ExampleNum,Ctx,RHSObjs,LHSObjs,Groups):-
  normalize_objects_for_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs,RHSO,LHSO),!,
  Step=0,Ctx=in_out,IsSwapped=false, 
  Info = info([step(Step),ctx(Ctx),testid(TestID),is_swapped(IsSwapped),example(ExampleNum)]),
  ((arg(_,v([],[delete],[all]),RelaxLvl),
    pairs_of_any(RelaxLvl,Info,LHSO,RHSO,[],Groups))).
 

normalize_objects_for_dependancy(TestID,ExampleNum,RHSObjs,LHSObjs,RHSObjsO,LHSObjsO):- 
  different_lengths(LHSObjs,RHSObjs),
  member(Filter,[iz(fg_or_bg(iz_fg)),   cc(bg,0)]),
  include(has_prop(Filter),LHSObjs,LHSObjsM), LHSObjsM\==[], include(has_prop(Filter),RHSObjs,RHSObjsM), RHSObjsM\==[],
  \+ different_lengths(LHSObjsM,RHSObjsM), !,
  normalize_objects_for_dependancy(TestID,ExampleNum,RHSObjsM,LHSObjsM,RHSObjsO,LHSObjsO).

normalize_objects_for_dependancy(_TestID,_ExampleNum,RHSObjs,LHSObjs,RHSO,LHSO):- fail,
  include(is_fg_object_really,LHSObjs,LHSObjsO), include(is_fg_object_really,RHSObjs,RHSObjsO),
  sort_by_jaccard(one(RHSObjsO),LHSObjsO,LHSO),
  sort_by_jaccard(one(LHSObjsO),RHSObjsO,RHSO),
  !.
normalize_objects_for_dependancy(_,_,L,R,L,R):-!.


different_lengths(LHSObjsO,RHSObjsO):-
  length(LHSObjsO,L21), length(RHSObjsO,L22), !, L21\==L22,!.

print_grid_pp(O):- global_grid(O,G),!,print_grid(G),nl,pp(O).


show_bad_objs(G,O):- is_list(O),!,maplist(show_bad_objs(G),O).
show_bad_objs(_,O):- is_object(O), \+ sub_var(black,O),!.
show_bad_objs(G,O):- fail,
  %globalpoints(O,SomeBlack),sub_var(black,SomeBlack),
  enum_fg_real_colors(FG), sub_var(FG,O),!,
  show_indiv_object(show_bad_objs,O),
  %print_ss(show_bad_objs,SomeBlack,NGrid),nl,
  print(G),nl,writeg(O),nl,writeg(FG),
  itrace,
  %bt,
  %print_ss(show_bad_objs,SomeBlack,O),nl,itrace.
  !.
show_bad_objs(_G,_O).


/*
task_id
example_task_grids
create_grid_task_object
create_grid_single_objects
task_objs
copy_input_objs_to_output_perfectly
copy_input_objs_to_output_perfectly_changing_single_props
copy_input_objs_to_output_perfectly_changing_two_props
mark_input_objs_for_delete_ignore
copy_output_objs_to_output_perfectly_changing_single_props
copy_output_objs_to_output_perfectly_changing_two_props

copy_
*/

testid_to_pairs(TestID,Start):-
  testid_to_pairs_start(TestID,Start).
testid_to_pairs(TestID,Next):-
  testid_to_pairs(TestID,Start),
  testid_to_pairs_next(TestID,Start,Next).

testid_to_pairs_start(TestID,L2R):-
   ensure_test(TestID),
   kaggle_arc(TestID,ExampleNum,LHS,RHS),
   once((Info = info([testid(TestID),example(ExampleNum)]),
   into_oid((TestID>ExampleNum*in),GID1),grid_to_single_object(GID1,LHS,In),
   into_oid((TestID>ExampleNum*out),GID2),grid_to_single_object(GID2,RHS,Out))),
   L2R=l2r(Info,[In],[Out]).

testid_to_pairs_next(_TestID,Start,Next):- pair_next(Start,Next).

pair_next( l2r(info([type(grid)]),LHSGrid,RHSGrid),
           l2r(info([type(group)]),LHSObjs,RHSObjs)):-
   individuate_pair(complete,LHSGrid,RHSGrid,LHSObjs,RHSObjs).

/*
next(l2r(Info1,LHSGrid,RHSGrid), l2r(Info2,LHSObjs,RHSObjs)):- is_grid(LHSGrid),is_grid(RHSGrid),
  merge_vals(Info1,[(TestID>ExampleNum*in)])
  
  grid_to_single_object(Info1,LHSGrid,LHSObjs),  
  grid_to_single_object(Info1,LHSGrid,RHSObjs),
  GID2 = (TestID,ExampleNum,out),
  individuate_pair(complete,LHSGrid,RHSGrid,LHSObjs,RHSObjs).

*/

  


prinnt_sbs_call([],[]):- dash_chars,!.
prinnt_sbs_call([G1|WP1],[G2|WP2]):- !,
  length(WP1,L1),length(WP2,L2),
   print_ss(blue,G1,L1,G2,L2),
   prinnt_sbs_call(WP1,WP2),!.
prinnt_sbs_call(R,[]):- !, dash_chars,!, wdmsg(input), print_side_by_side_l(1,R),!,dash_chars,dash_chars.
prinnt_sbs_call([],R):- !, dash_chars,!, wdmsg(output), print_side_by_side_l(1,R),!,dash_chars,dash_chars.
  

prinnt_sbs_call(WP1,WP2):- 
 must_det_ll((
    wots(S1,maplist(print_grid_nl,WP1)),
    wots(S2,maplist(print_grid_nl,WP2)),
    atomic_list_concat(SS10,'\n',S1),
    atomic_list_concat(SS20,'\n',S2),
    max_width(SS10,SS1,100), 
    max_width(SS20,SS2,100),
    %SS10=SS1,SS20=SS2,
    make_longest_len(SS1,SS2,SSS1,SSS2),
    print_to_string11(write,0,SSS1,SSS1A,Lad1),Lad is Lad1,
    maplist(print_to_string_using_up(Lad,''),SSS1A,SSS1B), 
    print_side_by_side0(SSS1B,_,SSS2))).

print_grid_nl(G):- nl,print_grid(G),nl.



/*
pair_obj_one_rule(TestID,Ctx,id(Ex,Step),Rule):- 
  Rule = r(Type,LHS,RHS,S,L,R, Ex, Step),
  pair_obj_props(TestID,Ex,Ctx,_Info,Step,Type,LHS,RHS,S,L,R).

trans_rules_combined(TestID,Ctx,Combined):-
  findall(Rule1,pair_obj_one_rule(TestID,Ctx,_RuleID1,Rule1),Rules),
  combine_trans_rules(Rules,CombinedRules),
  member(Combined,CombinedRules).


combine_more(Excluded,TestID,Ctx,Rule1,Combined):- 
   pair_obj_one_rule(TestID,Ctx,RuleID2,Rule2),
   \+ member(RuleID2,Excluded),
   combine_rule(Rule1,Rule2,NewRule),
   combine_more([RuleID2|Excluded],TestID,Ctx,NewRule,Combined).
combine_more(_Excluded,_TestID,_Ctx,Combined,Combined).
*/
assert_map_pairs(TestID,ExampleNum,Ctx,Group):- is_list(Group),!,
  maplist(assert_map_pairs(TestID,ExampleNum,Ctx),Group).
assert_map_pairs(_TestID,_ExampleNum,_Ctx,call(Rule)):-!,must_det_ll(Rule),!.
assert_map_pairs(TestID,ExampleNum,Ctx,TransRule):-
   assertz_new(arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,TransRule)),!.


merge_object_dependancy(TestID):-
 scope_training(ExampleNum),
  forall(arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,TransRule),
    ignore(merge_map_pairs(TestID,ExampleNum,Ctx,TransRule))).

merge_map_pairs(_TestID,_ExampleNum,_Ctx,l2r(Info,In,Out)):-

  into_list(In,InL),into_list(Out,OutL),
 must_det_ll((   
   once((trans_rule(Info,InL,OutL,TransRules), TransRules \==[])),
   
   assert_map_pairs(TestID,ExampleNum,Ctx,TransRules),
  nop((once((diff_l_r(InL,OutL,Same,InFlatP,OutPFlat),
   unnumbervars(v5('$VAR'(0),'$VAR'('_'),Same,InFlatP,OutPFlat),UNV))),
                    must_det_ll((UNV = v5(_FG1,_BG1,USame,InFlatProps,OutFlatProps))),
  %pp_ilp(l2r(Info,InL,OutL)),!,  
  assertz_new(arc_cache:prop_dep(TestID,ExampleNum,Ctx,Info,InL,OutL,USame,InFlatProps,OutFlatProps)))))).


:- dynamic(arc_cache:trans_rule_db/4).

% print the object dependencies for this test
% =============================================================
print_object_dependancy(TestID):-
% =============================================================
  ensure_test(TestID),
  /*if_t(( \+ arc_cache:map_pairs(TestID,_,_,_,_,_)),
   ( dash_chars,forall(arc_cache:map_group(TestID,_,_IO_,Group),
    once(((dash_chars,dash_chars,pp_ilp(Group),dash_chars,dash_chars)))))),
  dash_chars,*/
  %findall_vset_R(l2r(Info,Pre,Post),arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,l2r(Info,Pre,Post)),Set1),
  %pp_ilp(print_object_dependancy=Set1),!
  
% maplist(pp_ilp,Set1),
 nop((dash_chars,dash_chars,
 %pp_ilp_vset(l2r(Info,Pre,Post),pair_obj_info(TestID,_,_,Info,Pre,Post)),
 with_vset(
    arc_cache:prop_dep(TestID,ExampleNum,Ctx,Info,LHS,RHS,_USame,_InFlatProps,_OutFlatProps),
        pp_ilp(l2r(Info,LHS,RHS))))),
 dash_chars,dash_chars,
 with_vset(
    arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,Stuff),
       prefix_spaces(2,pp_ilp(Stuff))),!.

        %%pp_ilp(print_io_terms(D+7,In,Out),l2r(Info,In,Out))),

 %if_t(Set1 =@= Set2,  wdmsg('Set 2 the same')),
 %if_t(Set1 \=@= Set2,
pp_obj_tree(D,Info,In,Out):-  
  maplist(must_be(nonvar),[Info,In,Out]),
  once(into_solid_grid_strings_1(In,ITerm)),
  once(into_solid_grid_strings_1(Out,OTerm)),
  dash_chars,
  prefix_spaces(D,print(Info)),
  prefix_spaces(D+2,pp_ilp(ITerm)),
  prefix_spaces(D+10,dash_chars),
  prefix_spaces(D+2,pp_ilp(OTerm)),!,
  dash_chars.




findall_vset_R(T,G,R):- findall_vset(T,G,S), vsr_set(S,R). %,reverse(R,L).
vsr_set(L,P):- flatten([L],F),vs_set(F,R),reverse(R,P).
vs_set(L,P):- variant_list_to_set(L,S),sort(S,P).
%pp_ilp_vset(G,T):- dash_chars,with_vset(G,pp_ilp(C)).
with_vset(G,C):- term_variables(C,Vs),findall_vset_R(Vs,G,L),forall(member(Vs,L),call(C)).
%:- dynamic(arc_cache:map_pairs/6).
:- abolish(prop_dep/9).
:- abolish(arc_cache:prop_dep/9).
:- dynamic(arc_cache:prop_dep/9).
:- abolish(arc_cache:trans_rule_db/4).
:- dynamic(arc_cache:trans_rule_db/4).
%:- dynamic(arc_cache:causes/5).
/*
pair_obj_props(TestID,ExampleNum,Ctx,Info,Step,TypeO,LHS,RHS,USame,InFlatProps,OutPFlatrops):-
 ensure_test(TestID),
  Info = info(Step,_IsSwapped,Ctx,TypeO,TestID,ExampleNum,ExampleNum),
  arc_cache:prop_dep(TestID,_,_,Info,LHS,RHS,USame,InFlatProps,OutPFlatrops).

pair_obj_props(TestID,ExampleNum,Ctx,Step,TypeO,LHS,RHS,USame,InFlatProps,OutPFlatrops):-  
 ensure_test(TestID),
  Info = info(Step,_IsSwapped,Ctx,TypeO,TestID,ExampleNum,ExampleNum),
  arc_cache:prop_dep(TestID,_,_,Info,LHS,RHS,USame,InFlatProps,OutPFlatrops).

pair_obj_info(TestID,ExampleNum,Ctx,Info,LHS,RHS):-
 ensure_test(TestID),
  Info = info(_Step,_IsSwapped,Ctx,_TypeO,TestID,ExampleNum,ExampleNum),
  arc_cache:prop_dep(TestID,_,_,Info,LHS,RHS,_USame,_InFlatProps,_OutPFlatrops).
*/

ok_intersect(L1,L2):-
  member(E1,L1),member(E2,L2),
  other_val(E1,E2),!,fail.
ok_intersect(_,_).

/*
pair_obj_props54321(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R):- 
 ensure_test(TestID),
  trans_rules_combined(TestID,Ctx,Combined),
  r(Type,LHS,RHS,S,L,R, Ex, Step) = Combined,
  Info = info(Step,_IsSwapped,Ctx,Type,TestID,Ex, Ex).

pair_obj_props54321(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R):-
 ensure_test(TestID),
  (pair_obj_props5(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R)*->true;
  (pair_obj_props4(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R)*->true;
  (pair_obj_props3(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R)*->true;
  (pair_obj_props2(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R)*->true;
   pair_obj_props1(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R))))).

*/
into_solid_objs(RHS,RHSO):- flatten([RHS],RHSM),
  maplist(into_obj,RHSM,RHSO).


points_to_objects(ShapeType,Obj,Points,IndvPoints,NextScanPoints):- 
    %Points = VM.lo_points,
    %shape_min_points(VM,ShapeType,IndvPoints),
    %copy_term(ShapeType,OptionC),ShapeType=ShapeType,    
  select(C-HV,Points,Rest0), allowed_color(ShapeType,C), % non_free_fg(C), % \+ is_black(C),
  allowed_dir(ShapeType,Dir),adjacent_point_allowed(C,HV,Dir,HV2),select(C-HV2,Rest0,ScanPoints),
  all_individuals_near(_VM,Dir,ShapeType,C,[C-HV,C-HV2],ScanPoints,NextScanPoints,IndvPoints),%!,
  %make_indiv_object(VM,[iz(ShapeType),iz(media(shaped)),birth(i(ShapeType)),birth(i2(ShapeType))],IndvPoints,Obj),
   % meets_indiv_criteria(VM,ShapeType,IndvPoints),
  %set(VM.lo_points) = NextScanPoints,
  %assumeAdded(VM,Obj),
  %cycle_back_in(VM,OptionC).
  true,
  Obj = obj([globalpoints(IndvPoints)]).
/*
pair_obj_props1(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R):- fail, % fail here is since we should not allow any single example to make a rule
  pair_obj_props(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R).

pair_obj_props2(TestID,trn+Ex1,Ctx,Info,Step,Type,LHS,RHS,S,L,R):- 
  Info = info(Step1,_IsSwapped,Ctx,Type,TestID,trn+Ex1,trn+Ex1+Ex2),
  Ex1#<Ex2,
  pair_obj_props(TestID,trn+Ex1,Ctx,_Info1,Step1,Type1,LHS1,RHS1,S1,L1,R1),
  pair_obj_props(TestID,trn+Ex2,Ctx,_Info2,Step2,Type2,LHS2,RHS2,S2,L2,R2),
  combine_rule( do_requires, Step1,Type1,LHS1,RHS1,S1,L1,R1 , Step2,Type2,LHS2,RHS2,S2,L2,R2, Step,Type,LHS,RHS,S,L,R).

pair_obj_props3(TestID,trn+Ex1,Ctx,Info,Step,Type,LHS,RHS,S,L,R):-
  Info = info(Step1,_IsSwapped,Ctx,Type,TestID,trn+Ex1,trn+Ex1+Ex2+Ex3),
  Ex1#<Ex2,Ex2#<Ex3,
  pair_obj_props(TestID,trn+Ex1,Ctx,_Info1,Step1,Type1,LHS1,RHS1,S1,L1,R1),
  pair_obj_props(TestID,trn+Ex2,Ctx,_Info2,Step2,Type2,LHS2,RHS2,S2,L2,R2),
  combine_rule( do_requires, Step1,Type1,LHS1,RHS1,S1,L1,R1 , Step2,Type2,LHS2,RHS2,S2,L2,R2, Step12,Type12,LHS12,RHS12,S12,L12,R12),
  pair_obj_props(TestID,trn+Ex3,Ctx,_Info3,Step3,Type3,LHS3,RHS3,S3,L3,R3),
  combine_rule( do_requires, Step12,Type12,LHS12,RHS12,S12,L12,R12, Step3,Type3,LHS3,RHS3,S3,L3,R3, Step,Type,LHS,RHS,S,L,R).

pair_obj_props4(TestID,trn+Ex1,Ctx,Info,Step,Type,LHS,RHS,S,L,R):-
  Info = info(Step1,_IsSwapped,Ctx,Type,TestID,trn+Ex1,trn+Ex1+Ex2+Ex3+Ex4),
  Ex1#<Ex2,Ex2#<Ex3,Ex3#<Ex4,
  pair_obj_props(TestID,trn+Ex1,Ctx,_Info1,Step1,Type1,LHS1,RHS1,S1,L1,R1),
  pair_obj_props(TestID,trn+Ex2,Ctx,_Info2,Step2,Type2,LHS2,RHS2,S2,L2,R2),
  combine_rule( do_requires, Step1,Type1,LHS1,RHS1,S1,L1,R1 , Step2,Type2,LHS2,RHS2,S2,L2,R2, Step12,Type12,LHS12,RHS12,S12,L12,R12),
  pair_obj_props(TestID,trn+Ex3,Ctx,_Info3,Step3,Type3,LHS3,RHS3,S3,L3,R3),
  combine_rule( do_requires, Step12,Type12,LHS12,RHS12,S12,L12,R12, Step3,Type3,LHS3,RHS3,S3,L3,R3, Step123,Type123,LHS123,RHS123,S123,L123,R123),
  pair_obj_props(TestID,trn+Ex4,Ctx,_Info4,Step4,Type4,LHS4,RHS4,S4,L4,R4),
  combine_rule( do_requires, Step123,Type123,LHS123,RHS123,S123,L123,R123, Step4,Type4,LHS4,RHS4,S4,L4,R4, Step,Type,LHS,RHS,S,L,R).

pair_obj_props5(TestID,trn+N,Ctx,Info,Step,Type,LHS,RHS,S,L,R):-
  Info = info(Step1,_IsSwapped,Ctx,Type,TestID,trn+N,trn+0+1+2+3+N),
  pair_obj_props(TestID,trn+0,Ctx,_Info1,Step1,Type1,LHS1,RHS1,S1,L1,R1),
  pair_obj_props(TestID,trn+1,Ctx,_Info2,Step2,Type2,LHS2,RHS2,S2,L2,R2),
  combine_rule( do_requires, Step1,Type1,LHS1,RHS1,S1,L1,R1 , Step2,Type2,LHS2,RHS2,S2,L2,R2, Step12,Type12,LHS12,RHS12,S12,L12,R12),
  pair_obj_props(TestID,trn+2,Ctx,_Info3,Step3,Type3,LHS3,RHS3,S3,L3,R3),
  combine_rule( do_requires, Step12,Type12,LHS12,RHS12,S12,L12,R12, Step3,Type3,LHS3,RHS3,S3,L3,R3, Step123,Type123,LHS123,RHS123,S123,L123,R123),
  pair_obj_props(TestID,trn+3,Ctx,_Info4,Step4,Type4,LHS4,RHS4,S4,L4,R4),
  combine_rule( do_requires, Step123,Type123,LHS123,RHS123,S123,L123,R123, Step4,Type4,LHS4,RHS4,S4,L4,R4, Step1234,Type1234,LHS1234,RHS1234,S1234,L1234,R1234),
  pair_obj_props(TestID,trn+N,Ctx,_Info5,Step5,Type5,LHS5,RHS5,S5,L5,R5),
  combine_rule( do_requires, Step1234,Type1234,LHS1234,RHS1234,S1234,L1234,R1234, Step5,Type5,LHS5,RHS5,S5,L5,R5, Step,Type,LHS,RHS,S,L,R).

combine_rule(Rule1,Rule2,NewRule):-
  r(Type1,LHS1,RHS1,S1,L1,R1, Ex1, Step1) = Rule1,
  r(Type2,LHS2,RHS2,S2,L2,R2, Ex2, Step2) = Rule2,
  combine_rule(do_requires,
              Step1,Type1,LHS1,RHS1,S1,L1,R1, 
              Step2,Type2,LHS2,RHS2,S2,L2,R2,    
              Step, Type, LHS, RHS, S ,L ,R  ),!,
  r(Type,LHS,RHS,S ,L ,R ,Ex1+Ex2,Step) = NewRule.
combine_rule(Rule1,_Rule2,Rule1).

combine_rule(DoRequires,
              Step1,Type1,LHS1,RHS1,S1,L1,R1, 
              Step2,Type2,LHS2,RHS2,S2,L2,R2,   
              Step, Type, LHS, RHS, S, L, R ):-
              ignore(Step1=Step2), ignore(Step=Step2), 

              (DoRequires == do_requires -> (ok_intersect(R1,R2), something_common(R1,R2)) ; true),

              once((maplist(merge_vals,[Type1,LHS1,RHS1],[Type2,LHS2,RHS2],[Type,LHS,RHS]))),
              merge_vals(R1,R2,R),merge_vals(S1,S2,S),
                merge_vals(L1,L2,L),
                pp_ilp(merge_vals(L1,L2,L)).
*/
something_common(R1,R2):- \+ \+ ((member(E1,R1), good_for_rhs(E1),  member(E2,R2), E1=@=E2)).


%must_be_identical(step).

must_be_identical(edit).
must_be_identical(ctx).
must_be_identical(testid).

merge_list_vals(A,B,[E3|C]):- select(E1,A,AA),same_functor(E1,E2),select(E2,B,BB),!,merge_vals(E1,E2,E3),
 merge_list_vals(AA,BB,C).
merge_list_vals(A,B,C):- append_sets(A,B,C).

merge_vals(A,B,C):- atom(A),!,A==B,C=A.
merge_vals(A,B,C):- A=@=B,!,C=A.
merge_vals(A, A, A) :- !.
merge_vals(A,B,C):- A==[],!,B=C.
merge_vals(A,B,C):- B==[],!,A=C.

merge_vals(A,B,C):- is_obj_props(A),is_obj_props(B),!,merge_props(A,B,C).
merge_vals([A1,A2],[B],[C1,C2]):-  !, merge_vals(A1,B,C1),merge_vals(A2,B,C2).
merge_vals([A|AA],[B|BB],[C|CC]):- !, merge_vals(A,B,C), merge_vals(AA,BB,CC).

merge_vals(prop(Name,A),prop(Name,B),prop(Name,C)):- !, merge_vals(A, B, C).
merge_vals(prop(Name, A1, A2),prop(Name, B1, B2), prop(Name, C1, C2)):- !,
  merge_vals(A1, B1, C1),merge_vals(A2, B2, C2).

merge_vals(A,B,C):- ( \+ compound(A) ; \+ compound(B)),!, flatten_sets([A,B],C),!. 
merge_vals(T+A,T+B,C):-!,must_det_ll((C=(T+A+B))).

merge_vals(A,B,A):- functor(A,F,_),must_be_identical(F),!,A=@=B.

merge_vals(info(A),info(B),info(C)):- !, merge_list_vals(A,B,C).

merge_vals(A,B,C):- is_valid_testname(A),!,A=B,A=C.
%merge_vals(A,B,C):- good__rhs(A),!,same_rhs_operation(A,B),A=C.
%info([step(Step),is_swapped_lr(IsSwapped),ctx(Ctx),why(TypeO),testid(TestID),example(ExampleNum)])
merge_vals(A,B,C):-
  A =  ac_unit(TestID, IO, P1, PSame1),
  B =  ac_unit(TestID, IO, P2, PSame2),!,
  same_rhs_operation(P1,P2),
  merge_props(PSame1,PSame2,PSame),!,
  C =  ac_unit(TestID, IO, P1, PSame).

merge_vals(A,B,C):-
  A =  ac_unit_db(TestID, IO, P1, PSame1),
  B =  ac_unit_db(TestID, IO, P2, PSame2),!,
  same_rhs_operation(P1,P2),
  merge_props(PSame1,PSame2,PSame),!,
  C =  ac_unit_db(TestID, IO, P1, PSame).

merge_vals(A,B,C):-
  A =  ac_unit(TestID, IO, P1, PSame1),
  B =  ac_unit(TestID, IO, P2, PSame2),!,
  same_rhs_operation(P1,P2),
  merge_props(PSame1,PSame2,PSame),!,
  C =  ac_unit(TestID, IO, P1, PSame).

merge_vals(A,B,C):-
  A =  ac_listing(TestID, IO, P1, PSame1),
  B =  ac_listing(TestID, IO, P2, PSame2),!,
  same_rhs_operation(P1,P2),
  merge_props(PSame1,PSame2,PSame),!,
  C =  ac_listing(TestID, IO, P1, PSame).

/*
merge_vals(Rule1,Rule2,NewRule):-
  r(Type1,LHS1,RHS1,S1,L1,R1, Ex1, Step1) = Rule1,
  r(Type2,LHS2,RHS2,S2,L2,R2, Ex2, Step2) = Rule2,
  combine_rule(do_requires,
              Step1,Type1,LHS1,RHS1,S1,L1,R1, 
              Step2,Type2,LHS2,RHS2,S2,L2,R2,    
              Step, Type, LHS, RHS, S ,L ,R  ),!,
  r(Type,LHS,RHS,S ,L ,R ,Ex1+Ex2,Step) = NewRule.
*/

merge_vals(rhs(A),rhs(B),rhs(C)):- !, same_rhs_operation(A,B),!,merge_vals(A,B,C).

merge_vals(A,B,C):- compound(A),compound(B),var(C),
  compound_name_arguments(A,F,AA),compound_name_arguments(B,F,BB),!,
  maplist(merge_vals,AA,BB,CC),!, compound_name_arguments(C,F,CC).
%merge_vals(obj(A),obj(B),obj(C)):- is_list(A),is_list(B),!,merge_props(A,B,C).
merge_vals(A,B,C):-  flatten_sets([A,B],C),!. 

same_rhs_operation(A,B):- is_list(A),is_list(B),!.
same_rhs_operation(A,B):- (\+ compound(A) ; \+ compound(B)),!, A=@=B.
same_rhs_operation(A,B):-
  compound_name_arguments(A,F,AA),compound_name_arguments(B,F,BB),!,
  maplist(same_rhs_operation,AA,BB),!.




%good_for_rhs(iz(sid(_))).
%good_for_rhs(mass(_)).
%good_for_rhs(iz(cenGX(_))).
%good_for_rhs(iz(cenGY(_))).
%good_for_rhs(iz(sizeGX(_))).
%good_for_rhs(iz(sizeGY(_))).
/*good_for_rhs(vis2D(_, _)).
good_for_rhs(center2D(_, _)).
good_for_rhs(center2G(_, _)).
good_for_rhs(rot2D(_)).
good_for_rhs(iz(algo_sid(norm,_))).
good_for_rhs(grid_ops(norm,_)).
good_for_rhs(grid_rep(norm,_)).
*/
good_for_rhs(loc2D(_,_)).
good_for_rhs(pen(_)).

good_for_rhs(delete(_)).
good_for_rhs(edit(_)).
good_for_rhs(edit(_,_)).
good_for_rhs(edit(_,_,_)).
good_for_rhs(edit(_,_,_,_)).
good_for_rhs(create(_)).
good_for_rhs(rhs(_)).
good_for_rhs(obj(_)).

%good_for_lhs(P):- \+ ok_notice(P),!,fail.
%good_for_lhs(P):- make_unifiable(P,U),P=@=U,!,fail.
good_for_lhs(cc(bg, _)).
good_for_lhs(cc(fg, _)).
good_for_lhs(cc(_, 0)).
good_for_lhs(cc(FG, _)):- is_real_color(FG),!.
%good_for_lhs(cc(_, _)):- !, fail.
good_for_lhs(center2D(_, _)).
good_for_lhs(empty_area(_)).
good_for_lhs(global2G(_, _)).
%good_for_lhs(grid_ops(comp, _)).
%good_for_lhs(iz(fg_or_bg(_))).
%good_for_lhs(iz(filltype(_))).
good_for_lhs(iz(info(_))).
good_for_lhs(iz(sid(_))).
good_for_lhs(iz(sizeGX(_))).
good_for_lhs(iz(sizeGY(_))).
good_for_lhs(iz(stype(_))).
good_for_lhs(link(sees(_),_)):-!,fail.
good_for_lhs(link(NS, _)):- !, NS\=sees(_).
good_for_lhs(links_count(_, _)).
good_for_lhs(sym_counts(_, _)).
good_for_lhs(loc2D(_, _)).
good_for_lhs(mass(_)).
good_for_lhs(pen(_)).
good_for_lhs(rot2D(_)).
good_for_lhs(rotSize2D(grav, _, _)).
good_for_lhs(unique_colors(_)).
%good_for_lhs(unique_colors_count(_)).
good_for_lhs(vis2D(_, _)).
good_for_lhs(pg(_,_,_,_)).
good_for_lhs(\+ P):- !, good_for_lhs(P).
good_for_lhs(grid_ops(norm, _)).
good_for_lhs(iz(algo_sid(comp, _))).
good_for_lhs(iz(algo_sid(norm, _))).
good_for_lhs(iz(cenGX(_))).
good_for_lhs(iz(cenGX(_))).
good_for_lhs(iz(cenGY(_))).
good_for_lhs(_).


/*
pass2_rule_new(TestID,Ctx,RHSO,[iz(info(spawn(Info)))|PSame]):- 
  pair_obj_props54321(TestID,_Ex,Ctx,Info,_Step,_Type,LHSO,RHSO,[],[],[]),flat_props(LHSO,PSame).
pass2_rule_new(TestID,Ctx,[delete(LHSO)],[iz(info(delete(Info)))|PSame]):- 
  pair_obj_props54321(TestID,_Ex,Ctx,Info,_Step,_Type,LHSO,[],[],[],[]),flat_props(LHSO,PSame).
pass2_rule_new(TestID,Ctx,P,[iz(info(copy_edit(Info)))|PSame]):- 
  pair_obj_props54321(TestID,_Ex,Ctx,Info,_Step,_Type,_LHSO,_RHSO,PSame,_L,R),member(P,R),good_for_rhs(P).
*/

/*
pass2_rule_R(TestID,Rule):-
  Rule = rule(RuleType,P,PSame),
  RuleType = edit_copy(Ctx,ReType,P), 
  pass2_rule_1(TestID,IO,P,PSame), 
  once((good_for_rhs(P),
  prop_type(P,ReType),io_to_cntx(IO,Ctx))).
*/


has_a_value(P):- make_unifiable_u(P,U),P\=@=U.

how_are_differnt(O1,O2,Set):-
 findall(Type=Same,(prop_pairs2(O1,O2,Type,Same,_P),same\==Same),List), 
 vsr_set(List,Set).

prop_pairs(O1,O2,Type,Same,P):- must_det_ll(prop_pairs2(O1,O2,Type,Same,P)).

prop_pairs2(O1,O2,Type,Change,P):- 
  flat_props(O1,F1),flat_props(O2,F2),!,
  pred_intersection(=@=,F1,F2,_Same1,_Same2,U1,U2),
  member(P2,U2),once(make_unifiable_u(P2,P1)),
 (once((member(P1,U1),(other_val(P2,P1)->Change=different;Change=same)))-> 
   min_unifier(P2,P1,P); ((Change=adding(P),P=P2))),
 prop_type(P2,Type),
\+ignore_prop_when(Change,P).   



find_lhs(R,P):- sub_cmpd(lhs(E),R),!, into_lhs(E,P).
find_lhs(ac_unit(_Tst,_IO,_P,PConds),Out):- into_lhs(PConds,Out).
find_lhs(ac_unit(_Tst,_IO,_P,PConds),Out):- into_lhs(PConds,Out).
find_lhs(ac_unit(_Tst,_IO,_P,PConds),Out):- into_lhs(PConds,Out).
find_lhs(ac_unit(_Tst,_IO,_P,PConds),Out):- into_lhs(PConds,Out).
find_lhs(l2r(_Tst,P,_),Out):- find_lhs(P,Out).
find_lhs(R,R).

into_lhs(OID,Out):- atom(OID),!,indv_props_list(OID,In),into_lhs(In,Out),!.
into_lhs(In,Out):- \+ compound(In),!,Out=In.
into_lhs(rule(_RuleType,_SortKey,In),Out):- nonvar(In),!,into_lhs(In,Out),!.
into_lhs(obj(In),Out):- nonvar(In),!,into_lhs(In,Out),!.
into_lhs(In,Out):- \+ is_list(In),!,Out=In.
into_lhs(In,Out):- flatten([In],InF),into_lhs1(InF,LHSF),flatten(LHSF,LHSV),variant_list_to_set(LHSV,Out),!.
into_lhs1(In,Out):- m_unifiers(In,MidF),o_unifiers(MidF,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
%into_lhs1(In,Out):- is_group(In),mapgroup(into_lhs1,In,MidF),flatten(MidF,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
%into_lhs1(In,Out):- my_exclude(hide_propchange1,In,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
%into_lhs1(In,Out):-    maplist(hide_propchange,In,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
%into_lhs1(In,Out):- remove_giz(In,Out),!.
into_lhs1(In,Out):- maplist(into_lhs,In,LHSF),flatten(LHSF,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
into_lhs1(In,Out):- include(good_for_lhs,In,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
into_lhs1(Out,Out).

%m_unifiers(In,Out):- \+ is_list(In),Out=In.
   

m_unifiers(In,Out):- my_partition(assume_prop,In,Skip,DontSkip), Skip\==[],
  m_unifiers1(DontSkip,Mid), append_sets([Mid,Skip],Out),!.

%m_unifiers(In,Out):- is_list(In), select(E,In,More),is_prop1(E),is_unbound_prop(E),make_unifiable_u(E,U),select(U,More,UMore), 
%  min_unifier(U,E,S),!,m_unifiers([S|UMore],Out),!.
m_unifiers(In,Out):-m_unifiers1(In,Out).

m_unifiers1(In,Out):- is_list(In), select(E,In,More),is_prop1(E),make_unifiable_u(E,U),select(U,More,UMore), 
  min_unifier(U,E,S),!,m_unifiers1([S|UMore],Out),!.
%m_unifiers(In,Out):- select(E,In,More),is_prop1(E),make_unifiable_u(E,U),select(U,More,UMore),other_val(E,U),merge_props(U,E,S),!,m_unifiers([S|UMore],Out).
m_unifiers1(IO,IO).
%o_unifiers(In,Out):- select(E,In,More),is_prop1(E),make_unifiable(E,U),select(U,More,UMore),other_val(E,U),the_or_unifier(U,E,S),!,o_unifiers([S|UMore],Out).
o_unifiers(IO,IO). 
the_or_unifier(U,E,(U;E)).


merge_props(S1,S2,S):- my_partition(is_debug_info,S1,SP1,SO1),my_partition(is_debug_info,S2,SP2,SO2),
  the_min_unifier0(SO1,SO2,SO),append_vsets([SO,SP1,SP2],S).

the_min_unifier0(S1,S2,S):- the_min_unifier1(S1,S2,SA),
  m_unifiers(SA,SB),!,variant_list_to_set(SB,S).

the_min_unifier1(S1,S2,[E|S]):- 
   select(E1,S1,S1R),same_functor(E1,E2),select(E2,S2,S2R),

   %make_unifiable_u(E1,E2),
   other_val(E1,E2),%min_unifier(E1,E2,E),!,

   min_unifier(E1,E2,E),

   the_min_unifier1(S1R,S2R,S).
the_min_unifier1(S1,S2,S):- append(S1,S2,S),!.

propset_getter(is_group).
propset_getter(is_object).
propset_getter(is_obj_props).
two_prop_sets(TransRule,E1,E2):-
 sub_term(E1,TransRule),propset_getter(P1),call(P1,E1),subst(TransRule,E1,gone,RuleRest),
 sub_term(E2, RuleRest),propset_getter(Q1),call(Q1,E2).
/*
show_code_diff(Info,PA,[PB]):- !, show_code_diff(Info,PA,PB).
show_code_diff(Info,[PA],PB):- !, show_code_diff(Info,PA,PB).
show_code_diff(Info,[],_).
show_code_diff(Info,[P|A],PB):- !, show_code_diff(Info,P,PB),show_code_diff(Info,A,PB).
*/
show_code_diff(_Info,O1,O2):- (is_grid(O1), is_grid(O2)),!, 
  ((flat_props(O1,E1),flat_props(O2,E2), show_cp_dff_rem_keep_add(E1,E2))),!.
show_code_diff(_Info,O1,O2):- (is_grid(O1); is_grid(O2)),!, 
 ((flat_props(O1,E1),flat_props(O2,E2), show_cp_dff_rem_keep_add(E1,E2))),!.
show_code_diff(_Info,O1,O2):- (\+ is_grid(O1); \+ is_grid(O2)),!, 
 ((flat_props(O1,E1),flat_props(O2,E2), show_cp_dff_rem_keep_add(E1,E2))),!.
show_code_diff(Info,O1,O2):- (\+ is_grid(O1); \+ is_grid(O2)),!,
  into_list(O1,InL),into_list(O2,OutL), 
  trans_rule(Info,InL,OutL,TransRule),!, pp_ilp(trans_rules=TransRule).
/*
show_code_diff(Info,O1,O2):- 
 if_t((\+ is_grid(O1);\+ is_grid(O2)),
 (into_list(O1,InL),into_list(O2,OutL),
  flat_props(InL,E1),flat_props(OutL,E2), 
  show_cp_dff_rem_keep_add(E1,E2),
  trans_rule(Info,InL,OutL,TransRule),!,
  pp(TransRule))).

*/

/*
show_cp_dff_rem_keep_add([]):-!.
show_cp_dff_rem_keep_add(TransRule):-   %flat_props([B],PB), intersection(Same,PB,S,SS,_), append(S,SS,SSame),
  two_prop_sets(TransRule,E1,E2),  
  show_cp_dff_rem_keep_add(E1,E2).
*/
show_cp_dff_rem_keep_add(E1,E2):-  
  if_t(how_are_differnt(E1,E2,Set),pp_ilp(how_are_differnt=Set)),    
  noteable_propdiffs(E1,E2,Same,InFlatP,OutPFlat),
  if_t(Same==[],pp_ilp(sames=Same)),
  length(Same,LSame),
  pp_ilp(sames=LSame),
  pp_ilp(removing=InFlatP),
  pp_ilp(adding=OutPFlat),
  dash_chars.


ac_unit_visitor(AC_RULES_UNIT,_,_,_,_):- \+ compound(AC_RULES_UNIT),!,fail.
ac_unit_visitor(ac_unit(_,IO,P,PSame),IO,P,PSame).
ac_unit_visitor(ac_unit(_,IO,P,PSame),IO,P,PSame).
ac_unit_visitor(ac_unit(_,IO,P,PSame),IO,P,PSame).
ac_unit_visitor(ac_unit(_,IO,P,PSame),IO,P,PSame).
ac_unit_visitor(((IO:P):- PSame),IO,P,PSame).
ac_unit_visitor(ac_unit(_,IO,P,PSame),IO,P,PSame).


pp_ilp(Grp):- must_det_ll(pp_ilp(1,Grp)),!.

pp_ilp(D,T):-  T==[],!,prefix_spaces(D,write('[] ')),!.
pp_ilp(_,_):- format('~N'),nl,fail.
pp_ilp(D,T):-  is_ftVar(T),!,prefix_spaces(D,print(T)),!.


pp_ilp(D,AC_RULES_UNIT):- compound(AC_RULES_UNIT),
 AC_RULES_UNIT=..List, append(_,[_,IO,P,PSame],List),
 is_list(PSame),
 dash_chars,nl,
 must_det_ll((%once((nonvar(IO),io_to_cntx(IO,CTX));IO=CTX),
  Head = (IO:P), if_t(sub_cmpd(oout(OIDs),AC_RULES_UNIT),ignore(maplist(oid_to_obj,OIDs,Objs))),
   prefix_spaces(D,(print_rhs(Head),if_t(nonvar(Objs),print_grid(Objs)), 
     print_body(D+1,Head,(:- PSame)))))),!.
 %if_t(\+ sub_var(Info,PSame),pp_ilp_cmt(Info)).

pp_ilp(D,X=Y):- is_list(Y),length(Y,L),
  must_det_ll((
   prefix_spaces(D, (print(X),write('('),write(L),write(') = '),(L==0->write(' [] ');true))),
   (L==0->true;prefix_spaces(D+2,pp_ilp(Y))))).
pp_ilp(D,X=Y):- 
  must_det_ll((
   prefix_spaces(D, (print(X),write(' = '))),
   prefix_spaces(D+2,pp_ilp(Y)))).
pp_ilp(D,call(T)):- !, prefix_spaces(D,call(T)).
% pp_ilp(D,Grp):- is_mapping(Grp), prefix_spaces(D,print(Grp)),!.

pp_ilp(D,Grp):- is_mapping(Grp), !,
 must_det_ll((
  get_mapping_info(Grp,Info,In,Out),
  pp_obj_tree(D,Info,In,Out))).

pp_ilp(D,Grp):- compound(Grp), 
  (In-Out = Grp), Info=lr,!,
 \+ \+ must_det_ll((
 % get_mapping_info(Grp,Info,In,Out),
  prefix_spaces(D,(dash_chars,format('<l2r-hyphen  ~w >\n',[Info]))),
    print_io_terms(D+7,In,Out),
    %prefix_spaces(D+8,show_code_diff(Info,In,Out)),
  prefix_spaces(D,(write('</l2r-hyphen>\n'),dash_chars)))).


pp_ilp(D,apply(Rule,Obj)):- !, pp_ilp(D,l2r(apply(Rule),[],Obj)).


pp_ilp(D,A+B):-  !, prefix_spaces(D,(pp_ilp(A),nl,pp_ilp(B))).
pp_ilp(D,Grid):- is_grid(Grid),!,prefix_spaces(D,print_grid(Grid)),!,nl.
pp_ilp(D,Grid):- is_object(Grid),!,prefix_spaces(D,print_grid([Grid])),!,nl.
pp_ilp(D,G1):- is_debug_info(G1),!, prefix_spaces(D,color_print(green,call((writeg(G1))))).
pp_ilp(D,G1):- assume_prop(G1),!,prefix_spaces(D,color_print(green,call((pp_no_nl(G1))))).


pp_ilp(D,(H:-Conj)):- 
  prefix_spaces(D,(print((H)),nl,
     print_body(D+10,H,(:- Conj)))), rtrace,!.
/*

pp_ilp(D,(HH:-HConj)):- 
  must_det_ll((often_solid_str(HH,H), often_solid_str(HConj,Conj))),
  prefix_spaces(D,(print((H)),nl,
     print_body(D+15,Conj))),!.

pp_ilp(D,(H:-Conj)):-
  prefix_spaces(D,(print((H)),nl,
     prefix_spaces(D+15,
      (portray_clause(current_output, 
       (:- Conj),
       [portray_goal(portray_ilp)]))))),!.
*/

pp_ilp(D,AC_RULES_UNIT):-  fail, ac_unit_visitor(AC_RULES_UNIT,IO,P,PSame), %is_list(PSame), 
 must_det_ll((
  my_partition(not_debug_info,PSame,NoDebug,Debug),
  %once((nonvar(IO),io_to_cntx(IO,CTX));IO=CTX),
  once(list_to_conjuncts(NoDebug,Conj);NoDebug=Conj),
  pp_ilp(D,(((IO:P):- Conj))),
  pp_ilp_cmt(D,Debug))),!.


%pp_ilp(D,(H:-Conj)):- prefix_spaces(D,pp_ilp(H:-Conj)),!.


%pp_ilp(D,T):- true,!, prefix_spaces(D,print(T)),!.

%pp_ilp(D,Grid):- is_group(Grid),!,prefix_spaces(D,print_grid(Grid)),!,nl.
pp_ilp(D,Grid):- is_group(Grid),!, 
  must_det_ll((length(Grid,Len),
   prefix_spaces(D,(format('<group ~w>\n',[len=Len]))),
   prefix_spaces(D,mapgroup(pp_ilp(D+7),Grid)),!,nl,
   prefix_spaces(D,(format('</group>\n',[]))))),!.


pp_ilp(D,Grid):- is_obj_props(Grid),!,sort(Grid,R),reverse(R,S), prefix_spaces(D,pp(S)).
%pp_ilp(D,List):- is_list(List), \+ is_grid(List),write('['),maplist(pp_ilp(D+3),List),write(']').
pp_ilp(D,List):- is_list(List), !,
 must_det_ll((
  prefix_spaces(D,write('[')),
  maplist(pp_ilp(D+3),List),
  prefix_spaces(D,write(']')))).


%pp_ilp(D,T):- into_solid_grid_strings(T,G),!, prefix_spaces(D,print(G)),!.
pp_ilp(D,T):- prefix_spaces(D,pp(T)),!.

%pp_ilp_cmt(D,Debug):- is_list(Debug),!,maplist(pp_ilp_cmt(D),Debug).
%pp_ilp_cmt(D,Debug):- prefix_spaces(D+1,(color_print(green,call((write('% % '),write(Debug)))))).
pp_ilp_cmt(_,Debug):- Debug==[],!.
pp_ilp_cmt(D,[G1|Conj]):-  
        prefix_spaces(D+1, ((color_print(green,call((pp_no_nl(G1),write(','))))))),
        pp_ilp_cmt(D,Conj).

pp_ilpc(D,C,P):- 
  prefix_spaces(D, ((color_print(C,call(pp_ilp(P)))))).
  

%print_body(D,_,Conj):-   prefix_spaces(D,  portray_clause(current_output, (:- Conj), [portray_goal(user:portray_ilp)])).
print_body(D,Head, (:- Conj)):- !, conjuncts_to_list(Conj,List),  
  prefix_spaces(D, (writeln(':- '),print_body(D+2,Head,List))).

print_body(D,H,List):- select(Gps,List,Rest),is_gps(Gps,Call),!,call(Call),print_body(D,H,Rest).
print_body(_D,_,[]):- !,write('.').
print_body(D,H,[G1]):- prefix_spaces(D+1,(print_unit(D,H,G1),write('.'))).
print_body(D,H,Conj):- compound(Conj),Conj = (G1,Body),
  prefix_spaces(D+1,(print_unit(D,H,G1),write(','))), 
  print_body(D,H,Body).
print_body(D,H,[G1|Body]):-  
  prefix_spaces(D+1,(print_unit(D,H,G1),write(','))), 
  print_body(D,H,Body).
print_body(D,H,G1):- prefix_spaces(D+1,(print_unit(D,H,G1),write('.'))).

print_rhs(G1):- \+ sub_var(gps,G1),sub_term(R,G1),is_gps(R,Gps),subst(G1,R,gps,RR),G1\=@=RR,!,print_rhs(RR),call(Gps).
print_rhs(G1):- pp_no_nl(G1),!.

print_global_ngrid(OID):- oid_to_obj(OID,Obj),global_grid(Obj,Grid),into_solid_grid(Grid,SGrid),
                                                   into_ngrid(SGrid,NGrid),
                                                   mapgrid(no_ngrid_bg,NGrid,FGGrid),
                                                   print_grid(FGGrid).
no_ngrid_bg(Var,Var):- \+ compound(Var),!.
no_ngrid_bg(_-C,C):- is_bg_color(C),!.
no_ngrid_bg(C,C).

is_gps(Gps,_):- plain_var(Gps),!,fail.
is_gps(oid(OID),print_global_ngrid(OID)).
is_gps((OID),print_global_ngrid(OID)):- atom(OID),oid_to_obj(OID,_),!.
%is_gps(globalpoints(OID),print_global_ngrid(OID)).
%is_gps(globalpoints(Ps),Ps):- nonvar(Ps).
%is_gps(localpoints(Ps),Ps):- nonvar(Ps).

print_unit(_D,_H,G1):- print_unit(G1),!. 
print_unit(G1):- is_gps(G1,GPs),!,print_grid(GPs).
print_unit(G1):- is_debug_info(G1),!, color_print(green,call((writeg(G1)))).
print_unit(G1):- assume_prop(G1),!, color_print(green,call((pp_no_nl(G1)))).
print_unit(G1):- pp_no_nl(G1).


%print_body(D,H,Conj):- notrace((\+ is_list(Conj),conjuncts_to_list(Conj,List))),print_body(D,H,List).

%print_body(D,_,Conj):- prefix_spaces(D, pp(Conj)),!.

%print_body(D,H,List):- m_unifiers1(List,UList),List\==UList,!,print_body(D,H,UList).
%print_body(D,H,List):- 
%print_body(D,_,Conj):- prefix_spaces(D,  portray_clause(current_output, (Conj), [portray_goal(user:portray_ilp)])).

%portray_ilp(T,_Options):- pp_ilp(T),!.
portray_ilp(T,_):- \+ compound(T),!,pcR(T).
portray_ilp(T,_):- functor(T,F,_),upcase_atom(F,U),U==F,!,pcR(T).
portray_ilp(T,O):- is_debug_info(T),!,underline_print(user:wt(T,O)).
portray_ilp(T,O):- is_unbound_prop(T),!,underline_print(user:wt(T,O)).
portray_ilp(T,_):- pcR(T).
pcR(T):- fail, portray(T).
%portray_ilp(T,_Options):- portray(T),!.
%portray_ilp(T,_Options):- portray_clause(T),!.
wt(T,_Options):- writeq(T),!.
%wt(T,Options):- write_term(T,Options).

 
is_grid_or_group(Grid):- is_grid(Grid),!.
is_grid_or_group(Grid):- is_group(Grid),!.

pp_grp(D,Info,In,Out):- 
  prefix_spaces(D,(dash_chars,format('<l2r  ~@ >\n',[print(Info)]))),
  print_io_terms(D+7,In,Out),
  %show_cp_dff_rem_keep_add1(In,Out),
  show_code_diff(Info,In,Out),
  %prefix_spaces(D+8,show_code_diff(Info,In,Out)),
  prefix_spaces(D,(write('</l2r>\n'),dash_chars)).
  %flat_props(InL,E1),flat_props(OutL,E2),show_cp_dff_rem_keep_add(E1,E2),

type_change(ITerm,In):- first_type(ITerm,T1), first_type(In,T2),!, T1\=@=T2.
  
first_type(In,T2):- is_grid(In),!,data_type(In,T2).
first_type(In,T2):- is_object(In),!,data_type(In,T2).
first_type(In,T2):- data_type(In,DT),why_last(DT,T2),!.
first_type(In,T2):- data_type(In,T2),!.

print_io_terms(D,In,Out):-
  once(into_solid_grid_strings_1(In,ITerm)),
  once(into_solid_grid_strings_1(Out,OTerm)),
  once(ITerm\=@=In;Out\=@=OTerm),!, 
  print_io_terms(D,ITerm,OTerm),

  nop((if_t( (type_change(ITerm,In);type_change(ITerm,In)),
    nop((show_cp_dff_rem_keep_add1(ITerm,OTerm)))))).

%flat_props(InL,E1),flat_props(OutL,E2), show_cp_dff_rem_keep_add(E1,E2),

/*
print_io_terms(D,ITerm,OTerm):-  
    is_grid_or_group(ITerm),is_grid_or_group(OTerm),
    prefix_spaces(D,print_ss("",ITerm,OTerm)),!.

print_io_terms(D,loc2D(X,Y,IITerm),loc2D(OX,OY,OOTerm)):- 
    \+ is_mapping(IITerm), \+ is_mapping(OOTerm),
    into_obj(IITerm,ITerm),  into_obj(OOTerm,OTerm),
    prefix_spaces(D,print_ss("",ITerm,loc2D(X,Y),OTerm,loc2D(OX,OY))),!.
*/
print_io_terms(D,ITerm,OTerm):-
    prefix_spaces(D,pp_ilp(ITerm)),
    prefix_spaces(D+10,dash_chars),
    prefix_spaces(D,pp_ilp(OTerm)),!.
    
print_io_terms(D,ITerm,OTerm):-
  prefix_spaces(D,print_ss("",call(pp_ilp(ITerm)),call(pp_ilp(OTerm)))),!.

%prefix_spaces(D,G):- fail, DD is D, wots(Tabs,(write('\t'),print_spaces(DD),write('.\t'))), wots(SS,G),!, print_prepended(Tabs,SS).
prefix_spaces(D,G):- catch(DD is D,_,DD=2), wots(Tabs,(write('\t'),print_spaces(DD),write('\t'))),prepend_each_line(Tabs,G).

sometimes_soild_str(Obj,_):- var(Obj),!,fail.
sometimes_soild_str(Obj,_):- is_object(Obj),!,fail.%global_grid(Obj,Grid),into_solid_grid(Grid,Solid).
%sometimes_soild_str(Lst,Solid):- tersify23(Lst,Solid),!.
%sometimes_soild_str(Lst,Solid):- mostly_grids(Lst,Solid).
mostly_grids(Lst,Grid):- is_object(Lst),!,global_grid(Lst,Grid).
mostly_grids(Lst,Grid):- is_list(Lst),!,is_points_list(Lst),length(Lst,Len),Len>3,into_grid(Lst,Grid).
mostly_grids(oid(OID),oid(Grid)):- atom(OID), oid_to_obj(OID,Obj),object_ngrid(Obj,Grid).
mostly_grids(was_oid(OID),was_oid(Grid)):- atom(OID), oid_to_obj(OID,Obj),object_ngrid(Obj,Grid).

%into_solid_grid_ref(G,S):- wots(S,print_grid(G)).
into_solid_grid_ref(G,S):- as_grid_string(G,S),!.


often_solid_str(T,WithGrids):- fail,
  sub_term(Obj,T),nonvar(Obj),once(mostly_grids(Obj,Solid)),Obj\=@=Solid,
  once(subst001(T,Obj,Solid,MidTerm)),MidTerm\=@=T,!,often_solid_str(MidTerm,WithGrids).
often_solid_str(T,WithGrids):- T=WithGrids,!.

into_solid_grid_strings_2(T,WithGrids):-
  sub_term(Obj,T),once((mostly_grids(Obj,Grid),into_solid_grid(Grid,Solid),
  subst001(T,Obj,Solid,MidTerm))),MidTerm\=@=T,!,into_solid_grid_strings_1(MidTerm,WithGrids).
    
into_solid_grid_strings_2(X,Y):- into_solid_grid_strings(X,Y),!.


    
into_solid_grid_strings_1(T,WithGrids):-
  sub_term(Obj,T),is_object(Obj),global_grid(Obj,Grid),into_solid_grid(Grid,Solid),
  subst001(T,Obj,Solid,MidTerm),MidTerm\=@=T,!,into_solid_grid_strings_1(MidTerm,WithGrids).
    
into_solid_grid_strings_1(X,Y):- into_solid_grid_strings(X,Y),!.

/*into_solid_grid_strings(T,WithGrids):-
  sub_term(Obj,T),Obj\=@=T,is_mapping(Obj),
  into_solid_grid_strings(Obj,Grid),!,
  subst001(T,Obj,Grid,MidTerm),



  into_solid_grid_strings(MidTerm,WithGrids).*/
prin_to_string(T,Text):- term_contains_ansi(T),Text=T,!.
prin_to_string(T,Text):- wots(Text,print(T)). 

into_solid_grid_strings(T,Text):- is_ftVar(T),Text=T,!.
into_solid_grid_strings(A,Y):-atom(A),into_obj(A,Y),!. %,into_solid_grid_strings(X,Y).
%into_solid_grid_strings(T,Text):- \+ compound(T),T=Text,!.
%into_solid_grid_strings(T,Text):- term_contains_ansi(T),Text=T,!.
%into_solid_grid_strings(T,Text):- as_is(T),T=Text,!.
%into_solid_grid_strings(T,Text):- is_object(T),object_color_glyph_long(T,Text),!.
%into_solid_grid_strings(T,Text):- is_object(T),as_grid_string(T,Text),!.
%into_solid_grid_strings(T,Text):- is_object(T),into_solid_grid_str(T,Text),!.
%into_solid_grid_strings(g rp(T),gr p(Text)):- is_list(T), wots(Text,print_ss(T)),!.
%into_solid_grid_strings(g rp(T),g rp(Text)):- is_list(T), maplist(into_solid_grid_strings,T,Text),!.
%into_solid_grid_strings(g rp(T),g rp(Text)):- is_list(T), prin_to_string(T,Text),!.
into_solid_grid_strings([T],WithGrids):- is_grid(T), !, into_solid_grid_strings(T,WithGrids).
into_solid_grid_strings([T],WithGrids):- \+ is_grid([T]), !, into_solid_grid_strings(T,WithGrids).
into_solid_grid_strings(T,WithGrids):-
  sub_term(TObj,T), compound(TObj), \+ is_list(TObj),
  arg(_,TObj,Obj), is_object(Obj), 
  into_solid_grid_str(Obj,GridStr),Obj\=@=GridStr,!,
  subst001(T,Obj,GridStr,MidTerm),
  into_solid_grid_strings(MidTerm,WithGrids).
into_solid_grid_strings(T,WithGrids):- fail,
  sub_term(Obj,T),is_grid(Obj),
  into_solid_grid_str(Obj,GridStr),Obj\=@=GridStr,!,
  subst001(T,Obj,GridStr,MidTerm),
  into_solid_grid_strings(MidTerm,WithGrids).
/*
into_solid_grid_strings(T,WithGrids):-
  sub_term(Obj,T),is_mapping(Obj),
  into_solid_grid_str(Obj,GridStr),Obj\=@=GridStr,!,
  subst001(T,Obj,GridStr,MidTerm),
  into_solid_grid_strings(MidTerm,WithGrids).
*/
%into_solid_grid_strings(MidTerm,WithGrids):- into_solid_grid_str(MidTerm,WithGrids). 
into_solid_grid_strings(WithGrids,WithGrids).
%  \+ arc_cache:map_group(TestID,ExampleNum,IO_,LeftRight),

need_positional_context(H,V):- (H=<3;V=<3),!.
need_positional_context(H,V):- (H=<12,V=<12),!.
need_positional_context(_H,_V).


into_solid_grid_str([Obj,Obj2],SS):- fail, is_object(Obj),is_object(Obj2),
 into_solid_grid_str(Obj,Grid1),
 into_solid_grid_str(Obj2,Grid2),
 wots(SS,print_ss(Grid1,Grid2)),!.

into_solid_grid_str(Obj,SS):- is_object(Obj),global_grid(Obj,GG),!,into_solid_grid_str(GG,SS).
into_solid_grid_str(Obj,SS):- is_object(Obj),global_grid(Obj,SS),!.

into_solid_grid_str(Obj,SS):- is_object(Obj),loc2D(Obj,X,Y),
 vis2D(Obj,H,V), vis2D(Obj,H,V),has_prop(giz(g(IO_)),Obj),
 (need_positional_context(H,V)->global_grid(Obj,GG);=(Obj,GG)),
  as_grid_string(GG,Grid), =((loc2D(IO_,X-Y,Grid)),SS),!.

%into_solid_grid_str(Obj,SS):- is_object(Obj),loc2D(Obj,X,Y),into_solid_grid(Obj,Grid), =((loc2D(X-Y,Grid)),SS),!.
into_solid_grid_str(Grid,GridStr):- into_solid_grid(Grid,Solid),Solid\=@=Grid,into_solid_grid_str(Grid,GridStr). %,wots(GridStr,(nl,print_grid(Grid))).
%into_solid_grid_str(Grid,(GridStr)):- as_grid_string(Grid,GridStr),!.%print_wots(GridStr,(nl,print_grid(Grid))).
into_solid_grid_str(O,O).

% =============================================================
clear_object_dependancy(TestID):-
% =============================================================
 ensure_test(TestID),
 forall(kaggle_arc(TestID,ExampleNum,_,_),
     ignore((clear_object_dependancy(TestID,ExampleNum)))).
clear_object_dependancy(TestID,ExampleNum):-  
 forall(arc_cache:prop_dep(TestID,ExampleNum,Ctx,Info,Right,Left,A,B,C),
    retract(arc_cache:prop_dep(TestID,ExampleNum,Ctx,Info,Right,Left,A,B,C))),
 forall(arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,Info), 
   retract(arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,Info))),
 !.



/*

Rule Generation (AQ Learning): First, you would need to implement AQ learning to generate a set of rules from your data. This might involve predicates for generating potential rules, testing them against your data, and selecting the best ones. The resulting rules would be a set of Prolog rules that can classify instances based on their attributes.

Causal Inference (Mill's Methods): Once you have your rules, you could then apply Mill's methods to them. This would involve creating predicates that implement the logic of each of Mill's methods, and then applying these predicates to your rules to infer potential causal relationships.

We wrote a program that can read the output into objects 

The program is such that is is able to regenerate those grids uisng object notation

We run that program on the input and gerenate a set of objects. 

Like before, we can regerenate the orignal grids

We now write a transformation programs that maps the two object notations together

we do this for each example pair

trying to find the mappings that make the most sense per object pairs

we now poke holes onto the object notations (any time there was a source level conflict between examples) 
 this is sijmular to the the AQ-algorythem



until it becomes clear what the special transforem was for the 








pg(4,mass(_),rank1,1).
pg(4,mass(10),count,2).
cc(fg,nz).

Scene

TheWholeGridObjects
Objects

7e0986d6 is a good example why a "search" might be expensive approach


Properties

Object->Object mapping


CountOfProperty

*/
  
% sort_by_generation(Grps,SortedByGen):-predsort(sort_on(by_generation),Grps,SortedByGen).
sort_by_generation(Grps,Grps).

maybe_remove_bg(RHSObjs,RHSObjs1):- my_partition(is_fg_object,RHSObjs,RHSObjs1,Rest),RHSObjs1\==[],Rest\==[],!.
%maybe_remove_bg(RHSObjs,RHSObjs1):- include(is_fg_object,RHSObjs,RHSObjs1),RHSObjs1\=@=RHSObjs,!.
maybe_remove_bg(RHSObjs,RHSObjs).

fg_to_bgc(FG,black):- is_fg_color(FG),!.
fg_to_bgc(FG,FG):- \+ compound(FG),!.



is_mapping_list([O|GrpL]):- is_mapping(O),is_list(GrpL),maplist(is_mapping,GrpL).
is_mapping(Grp):- is_functor(l2r,Grp).

get_mapping_info(l2r(Info,In,Out),Info,In,Out):- nonvar(In).
get_mapping_info_list(GRP,Info,InOut):-
  get_mapping_info(GRP,Info,In,Out),
  into_list(In,InL),into_list(Out,OutL),!,
  append_LR(OutL,InL,InOutL),!,
  must_det_ll((InOutL=InOut)).



pairs_lr(LHS,RHS,PairsLR):- maplist(best_match_rl(RHS),LHS,PairsLR).
best_match_rl(RHS,Left,l2r(lr,Left,Right)):-
  sort_by_jaccard(Left,RHS,[Right|_RRest]).
best_match_lr(LHS,Right,l2r(lr,Left,Right)):-
  sort_by_jaccard(Right,LHS,[Left|_RRest]).
% Generate all pairs of objects from two sets
% pairs(+Set1, +Set2, -Pairs)

pairs_agree(LHS,RHS,PairsR):-
   maplist(best_match_lr(RHS),LHS,PairsR),
   maplist(best_match_rl(LHS),RHS,PairsL),!,
   PairsR=PairsL.

pairs_agree_or_select(RelaxLvl,Info,LHS,RHS,PairsR) :- 
  pairs_agree(LHS,RHS,PairsR)*->true;pairs_of_any(RelaxLvl,Info,LHS,RHS,PairsR).

n_or_more(3,[_,_,_|_]).
n_or_more(2,[_,_|_]).
n_or_more(1,[_|_]).

missing_from(SoFar,LeftOver):- \+ sub_var(LeftOver,SoFar).

member_of_relax(E,List):- \+ \+ member(E,List),!.

pairs_of_any(RelaxLvl,Info,LHS,RHS,PairsR):- pairs_of_any(RelaxLvl,Info,LHS,RHS,[],PairsR).

pairs_of_any(_RelaxLvl,_Info,[],[],SoFar,Pairs):-  !, /*append(SoFar,[l2r(balanced(Info),[],[])],Pairs);*/ 
   append(SoFar,[],Pairs),!.

pairs_of_any(RelaxLvl,Info,LeftOver,[],SoFar,Pairs):- !, member_of_relax(delete,RelaxLvl), 
   include(missing_from(SoFar),LeftOver,Delete),
   (Delete==[],Pairs=SoFar; append(SoFar,[l2r(delete(Info),Delete,[])],Pairs)),!.

pairs_of_any(RelaxLvl,Info,[],RightOver,SoFar,PairLR):- !, % member_of_relax(all,RelaxLvl),
  SoFar\==[],
  maplist(arg(2),SoFar,WasUsed), incr_cntx(Info,NInfo), 
    pairs_of_any(RelaxLvl,NInfo,WasUsed,RightOver,SoFar,PairLR).
/*
pairs_of_any(RelaxLvl,Info,LHSObjs,[],SoFar,PairLR):- fail,
  my_partition(is_mapping,LHSObjsF,Mappings,Objects),
  into_list([SoFar|Mappings],Kept),
  intersection(Kept,Objects,_,_,DeletedObjs),
  append_LR([Kept,l2r(delete,DeletedObjs,[])],PairLR).
*/
pairs_of_any(RelaxLvl,Info,LHSObjs,RHSObjs,SoFar,PairsR) :- LHSObjs\==[],RHSObjs\==[],
  %Type = perfect_combo,
  select_pair(Type,[],RHSObjs,LHSObjs,Right,Left,RestR,RestL),!,
  merge_vals(info([type(Type)]),Info,PInfo), incr_step(Info,NInfo),
  pairs_of_any(RelaxLvl,NInfo,[Left|RestL],RestR,[l2r(PInfo,Left,Right)|SoFar], PairsR),!.

pairs_of_any(RelaxLvl,Info,LHS,RHS,SoFar,PairsR) :- fail, notrace((LHS\==[],RHS\==[])),
  %member_of_relax(two_way,RelaxLvl), 
  member(Right,RHS),
  sort_by_jaccard(Right,LHS,[Left|RestL]),
  sort_by_jaccard(Left,RHS,[LeftPicked|RestR]),
  LeftPicked=@=Right,!,
  merge_vals(info([type(two_way)]),Info,PInfo), incr_step(Info,NInfo),
  pairs_of_any(RelaxLvl,NInfo,[Left|RestL],RestR,[l2r(PInfo,Left,Right)|SoFar], PairsR).


pairs_of_any(RelaxLvl,Info,LHSObjs,RHSObjs,SoFar,PairsR) :- fail, n_or_more(2,LHSObjs),n_or_more(2,RHSObjs),
  member(Filter,[iz(fg_or_bg(iz_fg)),   cc(bg,0)]),
  my_partition(has_prop(Filter),LHSObjs,LHSObjsM,LHSObjsN), n_or_more(1,LHSObjsM),n_or_more(1,LHSObjsN),
  my_partition(has_prop(Filter),RHSObjs,RHSObjsM,RHSObjsN), n_or_more(1,RHSObjsM),n_or_more(1,RHSObjsN),
  once((\+ different_lengths(LHSObjsM,RHSObjsM); \+ different_lengths(LHSObjsN,RHSObjsN))),
  pairs_of_any(RelaxLvl,Info,LHSObjsM,RHSObjsM,SoFar,SoFarMN),
  pairs_of_any(RelaxLvl,Info,LHSObjsN,RHSObjsN,SoFarMN,PairsR),!.



pairs_of_any(RelaxLvl,Info,LHS,[Right|RHS],SoFar,PairsR) :- LHS\==[], nonvar(Right),
  sort_by_jaccard(Right,LHS,[Left|RestL]),
  
  merge_vals(info([type(r_to_l_1)]),Info,PInfo), incr_step(Info,NInfo),
  pairs_of_any(RelaxLvl,NInfo,RestL, RHS,[l2r(PInfo,Left,Right)|SoFar], PairsR),!.

pairs_of_any(RelaxLvl,Info,LHS,RHS,SoFar,[l2r(Info,LHS,RHS)|SoFar]):- !, member_of_relax(all,RelaxLvl).
  


/*
pairs_of_any(RelaxLvl,Info,[LG1,LG2],RHS,SoFar,PairsR) :- 
    n_or_more(3,RHS),
    append(RG1,RG2,RHS), n_or_more(1,RG1),n_or_more(1,RG2),
    pairs_agree([[LG1],[LG2]],[RG1,RG2],WAS),
    WAS = [l2r(lr,L1,R1),l2r(lr,L2,R2)],
    pairs_of_any(RelaxLvl,Info,L1,R1,LR1), pairs_of_any(RelaxLvl,Info,L2,R2,LR2),
    append_LR([SoFar,LR1,LR2],PairsR).

pairs_of_any(RelaxLvl,Info,LHS,RHS,SoFar,PairsR) :- n_or_more(2,LHS),n_or_more(2,RHS),
    append(LG1,LG2,LHS), n_or_more(1,LG1),n_or_more(1,LG2),
    append(RG1,RG2,RHS), n_or_more(1,RG1),n_or_more(1,RG2),
    pairs_agree([LG1,LG2],[RG1,RG2],[l2r(lr,L1,R1),l2r(lr,L2,R2)]),
    pairs_of_any(RelaxLvl,Info,L1,R1,LR1), pairs_of_any(RelaxLvl,Info,L2,R2,LR2),
    append_LR([SoFar,LR1,LR2],PairsR).

pairs_of_any(RelaxLvl,Info,LHS,RHS,SoFar,PairLR):-
  pairs_agree_l_r(LHS,RHS,Agreed,RemainingL,RemainingR),
  append_LR(Agreed,SoFar,AgreedSoFar),
  pairs_of_any(RelaxLvl,Info,RemainingL,RemainingR,AgreedSoFar,PairLR).

is_adjacent_same_color(R1,R2,NewLHS,RHSObjs,RHSRest):- member(R1,NewLHS), select(R2,RHSObjs,RHSRest), is_adjacent_same_color(R1,R2,0),!.
is_adjacent_same_color(R1,R2,NewLHS,RHSObjs,RHSRest):- member(R1,NewLHS), select(R2,RHSObjs,RHSRest), is_adjacent_same_color(R1,R2,1),!.
is_adjacent_same_color(R1,R2,NewLHS,RHSObjs,RHSRest):- member(R1,NewLHS), select(R2,RHSObjs,RHSRest), is_adjacent_same_color(R1,R2,2),!.

calc_o_d_recursively(RelaxLvl,TestID,ExampleNum,TM,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,RestLR):- 
   LHSObjs==[], 
    into_list(Prev,PrevObjs),
    my_partition(is_input_object,PrevObjs,PrevLHS,PrevRHS),
    append_LR(PrevRHS,PrevLHS,NewLHS),
    is_adjacent_same_color(R1,R2,NewLHS,RHSObjs,RHSRest),
    incr_step(Step,IncrStep),
    make_pairs(TestID,ExampleNum,is_adjacent_same_color,IsSwapped,Step,Ctx,Prev,R1,R2,Pairs),
    %once((PrevRHS = [A,B|C] ; PrevLHS = [A,B|C])), %append_LR(PrevRHS,PrevLHS,NewLHS), %NewLHS=PrevLHS,    
    !, must_det_ll((calc_o_d_recursively(RelaxLvl,TestID,ExampleNum,TM,IsSwapped,IncrStep,Ctx,[Pairs|Prev],LHSObjs,RHSRest,RestLR))).


calc_o_d_recursively(RelaxLvl,TestID,ExampleNum,TM,IsSwapped,Step,Ctx,Prev,LHSObjsNil,RHSObjs,RestLR):- 
   LHSObjsNil==[], !, 
    incr_cntx(Ctx,IncrCtx),
    incr_step(Step,IncrStep), %incr_step(Step,IncrStep),
    into_list(Prev,PrevObjs),
    my_partition(is_input_object,PrevObjs,PrevLHS,PrevRHS),
    member(Type=LHSObjs,[perfect_combo=PrevLHS,perfect_combo=PrevRHS]),
      select_pair(Type,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest1,LHSRest1),
      must_det_ll((
      remove_object(RHSRest1,Right,RHSRest2), remove_object(LHSRest1,Right,LHSRest2),
      remove_object(RHSRest2, Left,RHSRest ), remove_object(LHSRest2, Left,LHSRest ),
      make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,IncrCtx,Prev,Left,Right,Pairs),
      append_LR(Prev,Pairs,NewPrev),
      
      calc_o_d_recursively(RelaxLvl,TestID,ExampleNum,TM,IsSwapped,IncrStep,IncrCtx,NewPrev,LHSRest,RHSRest,RestLR))).

new_object_splitter:-false.

calc_o_d_recursively(RelaxLvl,TestID,ExampleNum,TM,IsSwapped,Step,Ctx,Prev,LHSObjs,RHSObjs,[Pairs|RestLR]):-
 new_object_splitter,
 Type = two_way,
 select_pair(Type,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest1,LHSRest1),
 \+ has_prop(iz(info(faked(Ctx))),Right),
 must_det_ll((
  remove_object(RHSRest1,Right,RHSRest2), remove_object(LHSRest1,Right,LHSRest2),
  remove_object(RHSRest2, Left,RHSRest ), remove_object(LHSRest2, Left,LHSRest ),
  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,Prev,Left,Right,Pairs),
  append_LR(Prev,Pairs,NewPrev),
  incr_step(Step,IncrStep),


((  left_over_props(Left,Right,PropsMissing), PropsMissing=[_,_|_],
  pp_ilp(left_over_props=PropsMissing),
  obj_to_oid(Right,OID),
  obj_in_or_out(Right,IO),
  FakeObj = obj([was_oid(OID),iz(i_o(IO)),iz(info(faked(Ctx)))|PropsMissing])) -> 
      calc_o_d_recursively(RelaxLvl,TestID,ExampleNum,TM,IsSwapped,IncrStep,Ctx,NewPrev,[Right,Left|LHSRest],[FakeObj|RHSRest],RestLR);

      calc_o_d_recursively(RelaxLvl,TestID,ExampleNum,TM,IsSwapped,IncrStep,Ctx,NewPrev,[Right,Left|LHSRest],RHSRest,RestLR)))).

left_over_props(L,R,LO):- 
  noteable_propdiffs(L,R,_,_,LO).

  into_delete(_TestID,_ExampleNum,_IsSwapped,_Step,_Ctx,_Prev,_Info,Obj,Obj):- is_mapping(Obj),!.
%into_delete(_TestID,_ExampleNum,_IsSwapped,_Step,_Ctx,_Prev,_Info,Obj,Obj):-!.
into_delete(TestID,ExampleNum,IsSwapped,Step,Ctx,Prev,_Info,Obj,Pairs):- map_pred(fg_to_bgc, Obj,NewObj),
  make_pairs(TestID,ExampleNum,delete,IsSwapped,Step,Ctx,Prev,Obj,NewObj,Pairs),
  !. %edit_object(pen([cc(black,1)]))  % l2r(Info,[Obj],[])).

*/




pair_combinations([], _, []).
pair_combinations([H1|T1], List2, Pairs) :-
    pair_combinations(T1, List2, RemainingPairs),
    findall([H1, H2], (member(H2, List2), \+ member([H2, H1], RemainingPairs)), CurrentPairs),
    append(CurrentPairs, RemainingPairs, Pairs).


/*
In Prolog: I have two groups of objects where each object is denoted by `obj([center2D(2,6),mass(8),occurs_in_links(contains,1),pen([cc(blue,1)]),shape([square])])`
Each object looks at the other group of objects and keeps what its most simular to.. whenever an object from each group picks each other it forms a new pair .. remove those objects and keep going
there is no more objecs on one side any previous matches get adding back and a new set of pairs .. this goes on until there is no opbjects remaining on either side.
sometimes if there are an exact dividable number of objects on one side from the other try permutations of groups from the larger side
*/

% Predicate to find the most similar object in the other group for each object
most_similar(_, [], -1).
most_similar(Obj, [Other|Rest], MostSimilar) :-
    similarity(Obj, Other, Sim),
    most_similar(Obj, Rest, MostSimilarRest),
  (Sim > MostSimilarRest -> MostSimilar = Sim ; MostSimilar = MostSimilarRest).


%?- pair_combinations([o1, o2, o3], [o4, o5, o6], Pairs).
%Pairs = [[o1, o4], [o1, o5], [o1, o6], [o2, o4], [o2, o5], [o2, o6], [o3, o4], [o3, o5], [o3, o6]].


pairs_agree_l_r(LHS,RHS,Agreed,RemainingL,RemainingR):-
   maplist(best_match_rl(RHS),LHS,PairsR),
   maplist(best_match_lr(LHS),RHS,PairsL),
   once((
   intersection(PairsL,PairsR,Agreed,PairsLOnly,PairsROnly),
   Agreed \==[],
   %maplist(length,[Agreed,PairsLOnly,PairsROnly],[NAgreed,NPairsLOnly,NPairsROnly]),
   %NAgreed>NPairsLOnly,%NPairsLOnly>RPairsLOnly,
   %NAgreed>NPairsROnly,
   maplist(arg(1),PairsROnly,RemainingR),
   maplist(arg(1),PairsLOnly,RemainingL))).

combine_training(TestID,A,B,In012,Out012):-   
  obj_group_pair(TestID,_+A,In0,Out0),
  obj_group_pair(TestID,_+B,In1,Out1), A<B,
  pairs_agree_or_select(RelaxLvl,Info,In0,In1,In01),
  pairs_agree_or_select(RelaxLvl,Info,Out0,Out1,Out01),
  dif(C,A),dif(C,B), 
  dif(D,A),dif(D,B), dif(D,C),
  ignore((obj_group_pair(TestID,_+C,In2,_),pairs_agree_or_select(RelaxLvl,Info,In01,In2,In012))),
  ignore((obj_group_pair(TestID,_+D,_,Out2), pairs_agree_or_select(RelaxLvl,Info,Out01,Out2,Out012))),  
  ignore(In012=In01),ignore(Out012=Out01).


%  print_ss(into_object_dependancy_r_l,LHSO,RHSO),
one_or_multiple(A,B):- once(A),call(B).
%one_or_multiple(A,B):- call(A),call(B).


append_LR(Prev,Mappings,RestLR):- 
  flatten([Prev,Mappings],RestLR),!.
append_LR(Mappings,RestLR):- 
  flatten([Mappings],RestLR),!.


%incr_cntx(Ctx,NewCtx):- atom(Ctx),!, atom_concat(Ctx,'_out',NewCtx).
incr_cntx(info(Ctx),info(Next)):- is_list(Ctx),!,incr_cntx(Ctx,Next).
incr_cntx(Ctx,Next):- number(Ctx),!, plus(Ctx,1,Next).
incr_cntx(Ctx,Next):- Ctx == in_out,!, Next=in_out_out.
incr_cntx(Ctx,Next):- is_list(Ctx),select(ctx(C),Ctx,Rest),incr_cntx(C,CC),Next=[ctx(CC)|Rest],!.
incr_cntx(W+Ctx,W+Next):- incr_cntx(Ctx,Next).
incr_cntx(Ctx,Ctx):- compound(Ctx),!.
incr_cntx(Ctx,s(Ctx)).

incr_step(info(Ctx),info(Next)):- !,incr_step(Ctx,Next).
incr_step(Ctx,Next):- is_list(Ctx),select(step(C),Ctx,Rest),incr_step(C,CC),Next=[step(CC)|Rest],!.
incr_step(Ctx,Next):- incr_cntx(Ctx,Next).
swap_tf(Ctx,s(Ctx)).

%select_some(0,[],L,L).
select_some(1,[E],L,R):- select(E,L,R).  
select_some(2,[A,B],L,R):- select(A,L,R1),select(B,R1,R),A@<B.
select_some(3,[A,B,C],L,R):- select_some(2,[A,B],L,R1),select(C,R1,R),B@<C.
select_some(N,[A,B,C,D|More],L,R):- length(L,Max),between(4,Max,N),select_some(3,[A,B,C],L,R1),
  plus(M,3,N),select_some(M,[D|More],R1,R),C@<D.

in_to_ins(Ins,N,InsList):-
 findall(E,select_some(N,E,Ins,_),InsList).


%select_pair(two_way,_Prev,[A],[B],A,B,[],[]):-!.
select_pair(spatial_overlap,_Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):- fail,
  select(Right,RHSObjs,RHSRest),once(globalpoints(Right,RPs)),
  select(Left,LHSObjs,LHSRest),once(globalpoints(Left,LPs)),
  intersection(RPs,LPs,OverLap,RLO,LLO),
  (LLO==[];RLO==[];OverLap\==[]),!.

select_pair(two_way,_Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-
  (var(RHSObjs);var(LHSObjs)),!,fail.
select_pair(two_way,_Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):- 
  nonvar(RHSObjs),nonvar(LHSObjs),
  select(Left,LHSObjs,RestLeft),
  \+ is_mapping(Left),
  nonvar(Left),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),
  sort_by_jaccard(Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),
  sort_by_jaccard(Right,LHSObjs,[LeftMaybe|_]))),
  LeftMaybe = Left,!.


select_pair(perfect_w_prev,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-
  select(Left,[Prev|LHSObjs],RestLeft),
  \+ is_mapping(Left),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),
  sort_by_jaccard(Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),
  sort_by_jaccard(Right,LHSObjs,[LeftMaybe|_]))),
  LeftMaybe = Left,!.

select_pair(perfect_combo,_Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-  
  into_list(LHSObjs,LHSObjsL),variant_list_to_set(LHSObjsL,LHSObjsSet),
  in_to_ins(LHSObjsSet,2,LHSObjs_Combos),
  select(Left,LHSObjs_Combos,LHSObjs_Combos_Rest),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),  
  sort_by_jaccard(Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(LHSObjs_Combos_Rest,Right,LHSRest),
  sort_by_jaccard(Right,LHSObjs_Combos,[LeftMaybe|_]))),
  LeftMaybe = Left,!.


select_pair(need_prev,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-
  select(Left,LHSObjs,RestLeft),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),
  bonus_sort_by_jaccard(Prev,Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),
  bonus_sort_by_jaccard(Prev,Right,LHSObjs,[LeftMaybe|_]))),
  LeftMaybe = Left,!.

select_pair(from_left,Prev,RHSObjs,LHSObjs,Right,Left,RHSRest,LHSRest):-
  select(Left,LHSObjs,RestLeft),
  remove_object(RHSObjs,Left,RHSObjsMLeft),
  bonus_sort_by_jaccard(Prev,Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),!.

select_pair(from_right,Prev,LHSObjs,RHSObjs,Left,Right,LHSRest,RHSRest):-
  select(Left,LHSObjs,RestLeft),
  remove_object(RHSObjs,Left,RHSObjsMLeft),
  bonus_sort_by_jaccard(Prev,Left,RHSObjsMLeft,[Right|RHSRest]),
  remove_object(RestLeft,Right,LHSRest),!.

remove_object(RHSObjs,[Left|More],RHSObjsMI):- 
  remove_object(RHSObjs,Left,Rest),!,remove_object(Rest,More,RHSObjsMI).
remove_object(RHSObjs,Left,RHSObjsMI):- select(Left,RHSObjs,RHSObjsMI),!.
remove_object(RHSObjs,_,RHSObjs).

prime_factor(N, D) :-
    find_prime_factor(N, 2, D).

find_prime_factor(N, D, D) :- 0 is N mod D.
find_prime_factor(N, D, R) :- D < N,
    (0 is N mod D
    -> (N1 is N/D, find_prime_factor(N1, D, R))
    ;  (D1 is D + 1, find_prime_factor(N, D1, R))
    ).

split_sorted_bg(Objs,SplitLHS,SplitRHS):- 
  my_partition(is_bg_object,Objs,SplitLHS,SplitRHS), SplitLHS\==[], SplitRHS\==[].
split_sorted_bg_real(Objs,SplitLHS,SplitRHS):- 
  my_partition(split_sorted_bg_real,Objs,SplitLHS,SplitRHS), SplitLHS\==[], SplitRHS\==[].

split_sorted(Objs,SplitLHS,SplitRHS):-
 length(Objs,Len),
 prime_factor(Len,Prime),
 split_sorted_by_len(Objs,Len,Prime,SplitLHS,SplitRHS).

split_sorted_by_len(Objs,_Len,Prime,SplitLHS,SplitRHS):- 
 variance_counts(Objs,PropObjsounts),
 pp_ilp(PropObjsounts),
 findall(E,(member(E,PropObjsounts),sub_var(Prime,E)),EL),
 member(E,EL),into_prop(E,P),
 my_partition(has_prop(P),Objs,SplitLHS,SplitRHS),!.

split_sorted_by_len(Objs, Len,Prime,SplitLHS,SplitRHS):- 
 Half is Len div Prime,
 count_each_value(Objs,PropObjsounts),
 findall(E,(member(E,PropObjsounts),sub_var(Prime,Half)),EL),
 member(E,EL),into_prop(E,P),
 my_partition(has_prop(P),Objs,SplitLHS,SplitRHS),!.

into_prop(CC,P):- sub_term(E,CC),compound(E),is_prop1(E),!,E=P.

cto_aa(A,AA):- atom(A),!,AA=A.
cto_aa(s(A),AA):- nonvar(A), !, cto_aa(A,AAA),atom_concat(s_,AAA,AA).
cto_aa(A,AA):- format(atom(AA),'~w',[A]).

%make_pairs(TestID,ExampleNum,Type,s(IsSwapped),Step,Ctx,Prev,LHS,RHS,GRP):- nonvar(IsSwapped),!,
%  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,Prev,RHS,LHS,GRP).
%make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,Prev,LHS,RHS,GRP):- Prev\==[], !, 
%  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,[],Prev,LHS,NLHS),
%  make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,[],NLHS,RHS,GRP).
make_pairs(TestID,ExampleNum,Type,IsSwapped,Step,Ctx,_Prev,LHS,RHS,GRP):-
  Info = info([step(Step),is_swapped_lr(IsSwapped),ctx(Ctx),why(TypeO),testid(TestID),example(ExampleNum)]),
  must_det_ll((
 listify(LHS,LHSL),maplist(obj_in_or_out,LHSL,LCtx),maplist(cto_aa,LCtx,LCtxA),atomic_list_concat(LCtxA,'_',LP),
 listify(RHS,RHSL),maplist(obj_in_or_out,RHSL,RCtx),maplist(cto_aa,[Type,LP|RCtx],AA),atomic_list_concat(AA,'_',TypeO))),
   (Type==delete -> true ; (TypeO\==in_out_out_out, TypeO\=in_out_in_in)),
  
  %into_list(LHS,LLHS),
  %append_LR(Prev,LHS,PLHS),
  GRP = l2r(Info,LHS,RHS),
    %diff_compounds(LHS,RHS,D), (once(pp(make_pair=[info=Info,diff=D,lhs=LHS,rhs=RHS]))),
!.


saved_group(Why,IndvS):-
  is_why_grouped(_TestID,_Count,Why,IndvS).

is_why_grouped(TestID,Count,Why,IndvSO):-
  is_why_grouped_g(TestID,Count,Why,IndvSG),
  once(maplist(must_oid_to_object,IndvSG,IndvS)),
  IndvSO=IndvS.

must_oid_to_object(ID,O):- must_det_ll(oid_to_obj(ID,O)).

save_grouped(Why,G):-
  into_group(G,GS),
  get_current_test(TestID),
  length(GS,Len),
  mapgroup(register_obj,GS),
  maplist(obj_to_oid_u,GS,GGG),
  %maplist(obj_to_oid,GS,OIDs),
  my_asserta_if_new(is_why_grouped_g(TestID,Len,Why,GGG)).

into_oids(Objs,OIDs):- is_list(Objs),!, maplist(obj_to_oid,Objs,OIDs).
into_oids(Obj,[OID]):- obj_to_oid_u(Obj,OID),!. 
obj_to_oid_u(Obj,OID):- obj_to_oid(Obj,OID).

normal_group_form(Group,Group):-!.

:- dynamic(is_why_grouped_g/4).
why_grouped(Why,Group):-
  ensure_test(TestID),
  why_grouped(TestID,Why,Group).

why_grouped(TestID,Why,Group):- 
  (is_why_grouped(TestID,_,Why,Group)*->true; 
     ((is_list(Group)->length(Group,Len);true),is_why_grouped(TestID,Len,Why,Grp),same_members(=@=,Group,Grp))).

same_members(P2,G1,G2):- 
  select(E1,G1,GG1),select(E2,G2,GG2),
  call(P2,E1,E2), same_members(P2,GG1,GG2).

%select_group(TestID,Group,IndvSMode):- no_repeats(Group,select_group0(TestID,Group,IndvSMode)).
select_group(TestID,Group,IndvSMode):- select_group0(TestID,Group,IndvSMode).
select_group0(TestID,Group,IndvSMode):-
  ((is_why_grouped(TestID,_,How1,Group1), % dif(Group1,Group2), 
    is_why_grouped(TestID,_,How2,Group2),
    Group1\==[], Group2\==[],
    Group1\==Group2,
    once((sub_term(E,How1),sub_var(E,How2))),
    %length(Group1,G1), length(Group2,G2), G1>G2,
  once((sub_term(E,How1),sub_var(E,How2))),
  %member(M1,Group1),member(M2,Group2),M1=M2,
  append(Group1,Group2,GroupJ), sort_safe(GroupJ,Group),
  IndvSMode = [How1,How2])) 
    *-> true ; is_why_grouped(TestID,_,IndvSMode,Group).

select_group0(TestID,Group,obj_cache):- findall(O,obj_cache(TestID,O,_),GroupJ), GroupJ\==[], sort_safe(GroupJ,Group).



  


















compare_objects([],[]):-!.
compare_objects(Objs,Interesting):- 
  maplist(indv_props_for_noteablity,Objs,ObjProps),
  flatten(ObjProps,FlatProps),
  maplist(functorize_props,FlatProps,Functors),
  sort_safe(Functors,SortedFunctors),
  gather_props(SortedFunctors,FlatProps,ListOfLists),
  maplist(compare_values,ListOfLists,Diffs),
  include(\=([]),Diffs,Interesting).
  
functorize_props(iz(P),FA):- !, functorize_props(P,FA).
functorize_props(P,F/A):- functor(P,F,A).
gather_props([F/A|SortedFunctors],FlatProps,[(F-Candidates)|ListOfLists]):- 
  functor(Match,F,A), findall(Match,(member(Match,FlatProps);member(iz(Match),FlatProps)),Candidates),
  gather_props(SortedFunctors,FlatProps,ListOfLists).
gather_props([],_,[]).


compare_values(F-P,Notable):- predsort_using_only(number_varz,P,S),length(P,N),length(S,NS),
  is_notable(F-NS/N,Notable).

:- dynamic(repress_non_notables/0).
is_changeable_param(repress_non_notables/0).
repress_non_notables.

:- dynamic(never_noteable/1).
is_changeable_param(never_noteable/1).
never_noteable(colors_cc).
never_noteable(globalpoints).
never_noteable(P):- compound(P),functor(P,F,_),never_noteable(F).

is_prop_for_noteablity(P):- compound(P),functor(P,F,_),is_prop_for_noteablity(F),!.
is_prop_for_noteablity(P):- \+ never_noteable(P),!.

is_notable(_F-N/N,[]):- repress_non_notables, !.  
is_notable(_F-1/_,[]):- repress_non_notables, !.
is_notable(F-_,[]):- never_noteable(F),!.
is_notable(F-N/N,all_diff(F)):-!.
is_notable(F-1/_,all_same(F)):-!.
is_notable(F-S/N,notable(F,S/N)):-!.
%is_notable(F-S/N,Notable):- F-S/N = Notable.

   number_varz(I,C):- copy_term(I,C),numbervars(C,0,_,[attvar(skip)]).

:- style_check(+singleton).

found_in_w(Trait,List,L):- 
  findall(E,(member(_-Traits,List),sub_term(E,Traits),nonvar(E), \+ \+ (Trait = E) ),L).

found_in_o(Trait,List,L):- 
 findall(Obj,(member(Obj-Traits,List),sub_term(E,Traits),nonvar(E), \+ \+ (Trait =@= E)),L).


%each_1trait(Obj,self(Obj)).
each_1trait(Var,T):- var(Var),!, enum_object(Var),each_1trait(Var,T).
each_1trait(obj(L),T):- !, each_1trait(L,T).
each_1trait(iz(L),T):-  !, each_1trait(L,T).
each_1trait(L,T):- is_list(L),!,member(E,L),each_1trait(E,T).

each_1trait(T,T):- \+ too_verbose(T). 

each_trait(Obj,Obj-S):- findall(T,each_1trait(Obj,T),L),list_to_set(L,S).

get_peers(Obj,Peers):- 
  get_current_test(TestID),select_group(TestID,Group,_How), select(Obj,Group,Peers).
peerless_props(O1,Peers,PeerlessProps):-
 must_det_ll(( indv_props_list(O1,Props),
               (var(Peers)->get_peers(O1,Peers);true),
               (select(O1,Peers,PeersU)->true;PeersU=Peers),
  include(is_peerless_prop(PeersU),Props,PeerlessProps))).
not_peerless_props(O1,Peers,PeerlessProps):-
 must_det_ll(( indv_props_list(O1,Props),
               (var(Peers)->get_peers(O1,Peers);true),
               (select(O1,Peers,PeersU)->true;PeersU=Peers),
  include(not_peerless_prop(PeersU),Props,PeerlessProps))).

is_peerless_prop(Peers,P):- \+ sub_var(P,Peers).
not_peerless_prop(Peers,P):- sub_var(P,Peers).


too_unique(P):- compound(P),!,compound_name_arity(P,F,_),!,too_unique(F).
%too_unique(obj_to_oid).
too_unique(globalpoints).
%too_unique(o).
too_unique(link).
too_unique(obj_to_oid).
too_unique(/*b*/iz).
%good_overlap(colorlesspoints).

good_overlap(P):- compound(P),!,compound_name_arity(P,F,_),!,good_overlap(F).
good_overlap(localpoints).
good_overlap(rot2D).

too_non_unique(P):- compound(P),!,compound_name_arity(P,F,_),!,too_non_unique(F).
too_non_unique(grid_size).
too_non_unique(grid_sz).
%too_non_unique(/*b*/iz).
too_non_unique(grid).
too_non_unique(changes).

%too_non_unique(mass).

length_criteria(List,P):- compound(P), P=..[F,n,L],C=..[F,I,L],length(List,I),!,call(C).
length_criteria(List,P):- compound(P), P=..[F,L], C=..[F,I,L],length(List,I),!,call(C).
length_criteria(List,P):- compound(P), length(List,I), !, call(call,P,I).
length_criteria(List,N):- length(List,N).

is_fti_step(most_unique).
most_unique(symmetry_type,VM):-
  List = VM.objs,
  last(List,Obj),
  set(VM.solution)= Obj.

maplist_e(P2,A,B):- is_list(A),!,mapgroup(P2,A,B).
maplist_e(P2,A,B):- call(P2,A,B).

obj_exclude(Obj,Group,Others):- var(Obj),!,select(Obj,Group,Others).
obj_exclude(Obj,Group,Others):- select(O,Group,Others),(O==Obj *-> true; Group=Others).


  
/*

into_lst(ObjsL,[]):- ObjsL==[],!.
into_lst(ObjsL,[ObjsL]):- \+ compound(ObjsL),!.
into_lst(ObjsL,[ObjsL]):-is_gridoid(ObjsL),!.
into_lst(ObjsL,[ObjsL]):-is_grid(ObjsL),!.
into_lst(ObjsL,Lst):- is_list(ObjsL),!,maplist(into_lst,ObjsL,LstL),append(LstL,Lst).
into_lst(Grp,Lst):- is_mapping(Grp), get_mapping_info_list(Grp,_,List),!,into_lst(List,Lst).
into_lst(Grp,Lst):- arg(_,Grp,List),is_list(List),!,into_lst(List,Lst).
into_lst(ObjsL,[ObjsL]).

%solve_obj(_VM,_TestID,_ExampleNum,_IO_,_ROptions,Obj,Obj):- is_bg_object(Obj),!.

solve_obj_set([],_VM,_TestID,_ExampleNum,IO_,_ROptions,Objs,Objs):-!.
solve_obj_set([S|Set],VM,TestID,ExampleNum,IO__Start,ROptions,Objs,ObjsO):-
  solve_obj_list(S,VM,TestID,ExampleNum,IO__Start,ROptions,Objs,ObjsM),
  solve_obj_set(Set,VM,TestID,ExampleNum,IO__Start,ROptions,ObjsM,ObjsO).

solve_obj_list(_,_VM,_TestID,_ExampleNum,IO_,_ROptions,Objs,Objs):- Objs == [], !.
solve_obj_list(S,VM,TestID,ExampleNum,IO__Start,ROptions,[Obj|Objs],[NewObj|ObjsO]):-
  solve_obj(VM,TestID,ExampleNum,IO__Start,ROptions,Obj,NewObj),
  solve_obj_list(S,VM,TestID,ExampleNum,IO__Start,ROptions,Objs,ObjsO).


*/

has_individuals(TestID):- var(TestID), !, ensure_test(TestID), has_individuals_real(TestID).
has_individuals(TestID):- has_individuals_real(TestID),!.
has_individuals(TestID):- warn_skip(has_individuals(TestID)),!.
has_individuals_real(TestID):-  
 forall(current_example_scope(TestID,ExampleNum),
  (arc_cache:individuated_cache(TestID,TID,GID,_,Objs), sub_var(ExampleNum,(TID,GID)), Objs\==[])),!.
 


ensure_individuals(TestID):- var(TestID),!,ensure_test(TestID),ensure_individuals(TestID).
ensure_individuals(TestID):- has_individuals_real(TestID),!.
ensure_individuals(TestID):- load_file_dyn_pfc(TestID),has_individuals_real(TestID),!.
ensure_individuals(TestID):- 
 time((with_individuated_cache(true,
  once((with_pair_mode(whole_test, ensure_individuals1(TestID))))))), 
 save_test_hints_now(TestID).

% ensure_individuals1 tries the ensure_individuals2 
ensure_individuals1(TestID):- has_individuals_real(TestID),!.
ensure_individuals1(TestID):- 
  ensure_test(TestID),
    ignore(once((with_pair_mode(whole_test, 
          ensure_individuals2(TestID)),
    has_individuals_real(TestID)))),!.
 
ensure_individuals2(TestID):- %scope_training(ExampleNum),
  ExampleNum=(_+_),
  print_collapsed(200, 
     forall( kaggle_arc(TestID,ExampleNum,GridIn,GridOut),
       individuate_pair(complete,GridIn,GridOut,_,_))).

ensure_individuals2(TestID):- warn_skip(ensure_individuals2(TestID)),!.

ensure_individuals2(TestID):- once(with_luser(menu_key,'i',once(ndividuator(TestID)))).
ensure_individuals2(TestID):- once(with_luser(menu_key,'o',once(ndividuator(TestID)))).
ensure_individuals2(TestID):- calc_propcounts(TestID).


use_pair_info.
no_pair_info:- \+ use_pair_info.

gather_set(Ctx,Goal):-
  copy_term(Ctx+Goal,NRV+Copy),
  no_repeats_var(NRV), !, 
  call(Copy),Ctx=NRV.
/*

p_to_utbs(TestID,Ctx,P,UTBLists):-
 findall(UPB2,
  gather_set(UPB2,(map_pairs_info_io(TestID,_ExampleNum,Ctx,_Step,_TypeO,_A,_B,_USame,_UPA2,UPB2),member(P,UPB2))),UTBLists).
*/
:- use_module(library(ordsets)).

% common_members(+ListOfLists, -Common)
common_members([FirstList|Rest], Common) :-
    maplist(list_to_ord_set, [FirstList|Rest], OrdSets),
    foldl(ord_intersection, OrdSets, FirstList, Common).

% list_to_ord_set(+List, -OrdSet)
%list_to_ord_set(List, OrdSet) :- sort(List, OrdSet).

% Example query:
% ?- common_members([[1, 2, 3], [2, 3, 4], [1, 2, 3, 4, 5]], Common).
% Common = [2, 3].

%  is_post_objs(TestID,IO_,PostObjs),include(has_prop(P),PostObjs,PostObjsO).


make_common(NewOut1,LHS1,[E|NewOut],[E|NewLHS]):-
   select(O,NewOut1,NewOut2),
   make_unifiable_u(O,I),
   select(I,LHS1,LHS2),
   I=@=O, make_unifiable_u(I,E),!,
   make_common(NewOut2,LHS2,NewOut,NewLHS).
make_common(I,O,I,O).

% old code
diff_l_r_old(InL,OutL,Same,InFlatP,OutPFlat):-
 must_det_ll((
  (( \+ length(InL,1), OutL=[Out] ) -> sort_by_jaccard(Out,InL,[UseL|_]);UseL=InL),
  flat_props([UseL],PA), flat_props([OutL],PB),
  noteable_propdiffs(PA,PB,Same,InFlatP,OutPFlat))),!.



% no operation
diff_l_r([],[],[],[],[]):- !.

diff_l_r(InL,OutL,Same,InFlatP,OutPFlat):- \+ is_list(InL),!,diff_l_r([InL],OutL,Same,InFlatP,OutPFlat).
diff_l_r(InL,OutL,Same,InFlatP,OutPFlat):- \+ is_list(OutL),!,diff_l_r(InL,[OutL],Same,InFlatP,OutPFlat).

diff_l_r(InL,OutL,Same,InFlatP,OutPFlat):- fail,
 must_det_ll((
  (( \+ length(InL,1), OutL=[Out] ) -> sort_by_jaccard(Out,InL,[UseL|_]);UseL=InL),
  flat_props([UseL],PA), flat_props([OutL],PB),
  noteable_propdiffs(PA,PB,Same,InFlatP,OutPFlat))),!.

% -copy/transform  1-to-1
diff_l_r([InL],[OutL],PA,[],OutFlat):- OutL\==[],!,
  must_det_ll((flat_props([InL],PA), flat_props([OutL],PB),
  intersection(PA,PB,_Shared,_L,OutFlat))).

% -copy/transform
diff_l_r([InL],OutL,PA1,[],OutFlat):- OutL\==[],!,
  must_det_ll((flat_props([InL],PA), flat_props([OutL],PB),
  remove_o_giz(PA,PA1),remove_o_giz(PB,PB1),
  pred_intersection(propchange_unnoticable,PA1,PB1,_,_Same,_InFlatP,OutFlat))).

% create new
diff_l_r([],OutL,[],[],OutL):- OutL\==[],!.

% -delete some
diff_l_r(InL,[],Precond,[],[]):- !,
   flat_props([InL],InFlatP),
   remove_o_giz(InFlatP,Precond).

% -mutiple preconds
diff_l_r(InL,OutL,Same,InFlatP,OutPFlat):- OutL\==[],InL\==[],!,
  %pp_ilp(out=OutL), pp_ilp(in=InL),
  must_det_ll((
   sort_by_jaccard(OutL,InL,SharedInL),
   [UseL|Rest] = SharedInL,
   diff_l_r([UseL],OutL,Same1,InFlatP1,OutPFlat1),
   diff_l_r(Rest,OutL,SameR,InFlatPR,OutPFlatR),
   append_vsets([Same1,SameR],Same),
   append_vsets([InFlatP1,InFlatPR],InFlatP),
   append_vsets([OutPFlat1,OutPFlatR],OutPFlat))).

append_vsets(I,O):- flatten([I],M),variant_list_to_set(M,O),!.

ignore_prop_when(ARS,P):- compound(ARS),!,functor(ARS,F,_),!,ignore_prop_when(F,P).
ignore_prop_when(removing,P):- ignore_prop_when(adding,P).
ignore_prop_when(adding,link(contains,_)).
ignore_prop_when(adding,occurs_in_links(contains,_)).
%ignore_prop_when(adding,pg(_,_,rankLS,_)).
ignore_prop_when(adding,pg(_,_,_,_)).
ignore_prop_when(adding,grid_rep(_,_)).
ignore_prop_when(adding,simularz(_,_)).
ignore_prop_when(removing,cc(fg,_)).
ignore_prop_when(removing,mass(_)).
ignore_prop_when(removing,links_count(_,_)).
ignore_prop_when(removing,iz(info(_))).

noteable_propdiffs(E1,E2,Same,InFlatP,OutPFlat):- 
  noteable_propdiffs1(E1,E2,Same0,InFlatP0,OutPFlat0),
  my_exclude(ignore_prop_when(removing),InFlatP0,InFlatP),
  my_exclude(ignore_prop_when(adding),OutPFlat0,OutPFlat),
  my_exclude(ignore_prop_when(sames),Same0,Same),!.

noteable_propdiffs1(PA,PB,Same,InFlatP,OutPFlat):- 
  remove_o_giz(PA,PA1),remove_o_giz(PB,PB1),
  %=(PA,PA1),=(PB,PB1),
  pred_intersection(propchange_unnoticable,PA1,PB1,_,Same,InFlatP,OutPFlat),!.
/*noteable_propdiffs(PA,PB,Same,InFlatP,OutPFlat):- 
  remove_o_giz(PA,PA1),remove_o_giz(PB,PB1),
  intersection(PA1,PB1,Same,InFlatP,OutPFlat),!.*/

propchange_unnoticable(InL,OutL):- InL=@=OutL,!.
propchange_unnoticable(InL,OutL):- make_unifiable_u(InL,AU),make_unifiable_u(OutL,BU), AU\=@=BU,!,fail.
propchange_unnoticable(InL,OutL):- hide_propchange(InL,AA),hide_propchange(OutL,BB),AA=@=BB,!.


bg_into_var(Var,BG,Var):- BG ==bg,!.
bg_into_var(Var,BG,Var):- is_bg_color(BG),!.
bg_into_var(_,FG,FG).

number_fg_colors(In,Out):- sub_var('@',In),!,subst(In,'@','$VAR'(0),Out),!.
number_fg_colors(In,Out):- sub_var('fg',In),!,In=Out,!.
number_fg_colors(In,Out):- mapgrid(bg_into_var('$VAR'('_')),In,Mid),In\=@=Mid,!,number_fg_colors(Mid,Out).
number_fg_colors(In,Out):- sub_var(777,In),!,copy_term(In,Mid),subst001(Mid,'$VAR'(777),'@',Out),term_variables(Out,Vs),maplist('='('$VAR'('_')),Vs),!.
number_fg_colors(In,Out):- \+ \+ (sub_term(E,In),is_real_fg_color(E)),!,  
  copy_safe(In,InC),unique_fg_colors(InC,Cs),
  Cs\==[], % at least some colors
  subst_colors_with_vars(Cs,Vs,InC,Mid),    
  ground(Cs), % fully grounded test
  numbervars(Vs,777,_,[functor_name('$VAR'),singletons(false),attvar(skip)]),!,
  number_fg_colors(Mid,Out).
number_fg_colors(InOut,InOut).

hide_propchange2(In,Out):- \+ compound(In),!,Out=In.
hide_propchange2(link(PA,_),link(PA,_)).
hide_propchange2(pg(_,P,rank1,N),pg(_,P,rank1,N)).
%hide_propchange2(occurs_in_links(PA,_),occurs_in_links(PA,_)).
%hide_propchange2(links_count(PA,_),links_count(PA,_)).
hide_propchange2(giz(example_num(ExampleNum)),giz(example_num(ExampleNum))).
hide_propchange2(giz(gid(_)),giz(gid(_))).
hide_propchange2(giz(InL),giz(OutL)):- make_unifiable_u(InL,OutL).
hide_propchange2(oid(_),oid(_)).
hide_propchange2((i_o(_)),(i_o(_))).
hide_propchange2(In,Out):- once((sub_term(E,In),is_grid(E),number_fg_colors(E,G),subst001(In,E,G,Mid))),In\=@=Mid,!,hide_propchange(Mid,Out).
hide_propchange2(grid_rep(InL,G),grid_rep(InL,G)).
hide_propchange2(iz(X),iz(Y)):-!,hide_propchange2((X),(Y)).
hide_propchange2(IO_,IO_).

hide_propchange1(iz(symmetry_type(_,False))):- False == false.
hide_propchange1(iz(symmetry_type(_,False))):- False == true.
%hide_propchange1(pg(_,_,_,_)).
hide_propchange1(link(sees(_),_)).
hide_propchange1(pg(_,_,rankLS,_)).
hide_propchange1(iz(P)):-!,hide_propchange1(P).
%hide_propchange1(P):- \+ ok_notice(P),!.
hide_propchange1(P):- dont_notice(P),!.
%hide_propchange1(P):- make_unifiable_u(P,U),!,P=@=U,!.

hide_propchange(PA,PB):- hide_propchange2(PA,PA1),PA\=@=PA1,!,hide_propchange(PA1,PB).
hide_propchange(PA,PA).

remove_o_giz(OID,Out):- atom(OID),!,indv_props_list(OID,In),remove_o_giz(In,Out),!.
remove_o_giz(In,Out):- \+ compound(In),!,Out=In.
remove_o_giz(In,Out):- is_group(In),mapgroup(remove_o_giz,In,MidF),flatten(MidF,Mid),In\=@=Mid,!,  
  remove_o_giz(Mid,Out).
remove_o_giz(obj(In),Out):- nonvar(In),!,remove_o_giz(In,Out),!.
%flat_props(E1,FP1),flat_props(E2,FP2),

remove_o_giz(In,Out):- m_unifiers(In,MidF),o_unifiers(MidF,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
remove_o_giz(In,Out):- my_exclude(hide_propchange1,In,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
remove_o_giz(In,Out):-    maplist(hide_propchange,In,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
%remove_o_giz(In,Out):- remove_giz(In,Out),!.
remove_o_giz(Out,Out).






%is_accompany_changed_verified(TestID,IO,P,PSame):- is_accompany_changed_computed(TestID,IO,P,PSame), PSame\==[].

%is_accompany_changed_computed(TestID,IO,P,PSame):-
%   ac_unit(TestID,IO,P,PSame) *->true ; prop_can(TestID,IO,P,PSame). 
   
prop_can(TestID,IO,P,Can):-    
  props_change(TestID,IO,P),
  once((prop_cant(TestID,IO,P,Cant),
  prop_can1(TestID,IO,P,Can1),
  intersection(Can1,Cant,_,Can,_))).
  %(Can == [] -> (CanL=Can1,fail) ; CanL= Can).

prop_can1(TestID,IO,P,Can):-  
  props_change(TestID,IO,P),
  findall(O,
    ((enum_object_ext(O),has_prop(giz(g(out)),O),has_prop(cc(bg,0),O),
      has_prop(P,O))),[I|L]),
  indv_props_list(I,List),
  findall(U,(member(U,List),U\=@=P,ok_notice(U),forall(member(E,L),has_prop(U,E))),Can).


prop_cant(TestID,IO,P,Set):-
  props_change(TestID,IO,P),
  findall(Cant,
    ((enum_object(O),has_prop(giz(g(out)),O),has_prop(cc(bg,0),O),
      not_has_prop(P,O),indv_props_list(O,List),member(Cant,List),ok_notice(Cant))),Flat),
   list_to_set(Flat,Set).

enum_object_ext(O):-
  ensure_test(TestID),
  current_example_scope(TestID,ExampleNum),
  once((obj_group_io(TestID,ExampleNum,out,Objs),Objs\==[])),member(O,Objs).


contains_same([],_):- !.
contains_same([E|L],P):- sub_var(E,P),!,contains_same(L,P).

/*
find_peers_with_same(TestID,IO,P,PSame,NewSame):- select(S,PSame,Next),S=@=P,!,find_peers_with_same(TestID,IO,P,Next,NewSame).
find_peers_with_same(TestID,IO,P,PSame,NewSame):- 
   sub_term(Color,P),is_real_color(Color), sub_term(N,P),number(N),
   my_partition(contains_same([Color]),PSame,SameW,SameWO),SameW\==[], SameWO\==[],!,
   find_peers_with_same(TestID,IO,P,SameWO,NewSame).
find_peers_with_same(_,_,PSame,PSame):-!.
   
   

   

merge_xtra_props_ac1([ac1(PO)|AC3],PSame):- !, merge_xtra_props_ac1_3(PO,AC3,PSame), PSame\==[].
merge_xtra_props_ac1_3(PO,[ac1(PO2)|MORE],OUT):-
  intersection(PO,PO2,IPO),
  merge_xtra_props_ac1_3(IPO,MORE,OUT).
merge_xtra_props_ac1_3(PO,[],PO).

merge_xtra_props_ac2([ac2(_,PSame)],PSame):-!.
merge_xtra_props_ac2(AC2,PSame):-
 select(ac2(ExampleNum,PO1),AC2,AC3),
 select(ac2(ExampleNum,PO2),AC3,AC4),
 intersection(PO1,PO2,Some),Some\==[],!,
 merge_xtra_props_ac2([ac2(ExampleNum,Some)|AC4],PSame).
merge_xtra_props_ac2(AC2,PSame):-
 select(ac2(ExampleNum,PO1),AC2,AC3),
 select(ac2(ExampleNum2,PO2),AC3,AC4),
 ExampleNum \== ExampleNum2,
 intersection(PO1,PO2,Some),Some\==[],!,
 merge_xtra_props_ac2([ac2(ExampleNum,Some)|AC4],PSame).

merge_xtra_props_ac2([ac2(ExampleNum,PO1)|AC3],[ac2(ExampleNum,PO1)|PSame]):-
  merge_xtra_props_ac2(AC3,PSame),!.
merge_xtra_props_ac2(PSame,PSame):-!.
*/

changing_props(TestID,X1,X2):- 
 ensure_test(TestID),
 findall(X1-InOut,props_change(TestID,InOut,X1),X1L),
 variant_list_to_set(X1L,X1S),
 member(X1-IO,X1S),
 member(X2-IO,X1S),
% X1@>X2,
 other_val(X1,X2). 



print_scene_change_rules(TestID):- ensure_test(TestID),
  print_scene_change_rules(print_scene_change_rules,TestID).

print_scene_change_rules(Why,TestID):- 
   print_scene_change_rules3(Why,ac_unit,TestID).

print_scene_change_rules3(Why,P4db,TestID):- 
 ensure_test(TestID),
  must_det_ll((
   get_scene_change_rules(TestID,P4db,Rules),
   remove_debug_info(Rules,NoDebug),
   nb_setval('$last_rules_printed_nodebug',NoDebug),
   if_t(maybe_color_this(Why,Color),banner_lines(Color,4)),
   %trans_rules_combined(TestID,_Ctx,CombinedR),reverse(CombinedR,Combined), pp_ilp(merged(Why)=Combined),
   /*
   trans_rules_current(TestID,Ctx,Rules),
   must_det_ll(( \+ (member(R,[1|Rules]), is_list(R)))),
   combine_trans_rules(Rules, Combined),!,
   must_det_ll(( \+ (member(R,[2|Combined]), is_list(R)))).
   */
   if_t(maybe_color_this(Why,Color),banner_lines(Color,2)),
   dash_chars,pp_ilp(rules(Why,P4db)=Rules),
   if_t(maybe_color_this(Why,Color),banner_lines(Color,4)))).

print_scene_change_rules_if_different(Why,P4db,TestID):-
  (nb_current('$last_rules_printed_nodebug',Prev);Prev=[]),!,
  get_scene_change_rules(TestID,P4db,Rules),
  remove_debug_info(Rules,NoDebug),
  if_t(Prev =@= NoDebug,dash_chars),
 ignore((
   Prev \=@= NoDebug,
   nb_setval('$last_rules_printed_nodebug',NoDebug),
   banner_lines(cyan,1),
   dash_chars,
   pp_ilp(updated(Why,P4db)=Rules),
   nop(banner_lines(cyan,4)))).

maybe_color_this(Why,Color):- sub_term(Color,Why),is_color(Color),!.
get_scene_change_rules(TestID,P4db,Rules):-
 ensure_test(TestID),
  findall_vset_R(ac_unit(TestID,IO,P,PSame),
    call(P4db,TestID,IO,P,PSame),Rules).




has_propcounts(TestID):- 
 forall(current_example_scope(TestID,ExampleNum),
  ( \+ \+ (propcounts(TestID, ExampleNum, IO, count, _, _), sub_var(in,IO)),
    \+ \+ (propcounts(TestID, ExampleNum, IO, count, _, _), sub_var(out,IO)))).

ensure_propcounts(TestID):- ensure_test(TestID),ensure_propcounts1(TestID).
ensure_propcounts1(TestID):- has_propcounts(TestID),!.
ensure_propcounts1(TestID):- ensure_individuals(TestID),!.
ensure_propcounts1(TestID):- calc_propcounts(TestID),has_propcounts(TestID),!.

ensure_propcounts1(TestID):- 
  once((with_pair_mode(whole_test,
    with_luser(menu_key,'o',once(ndividuator(TestID)))))),has_propcounts(TestID),!.
ensure_propcounts1(TestID):- show_prop_counts(TestID), has_propcounts(TestID),!.
ensure_propcounts1(_).

props_change(TestID,IO,P):- map_pairs_info(TestID,IO,P,_Step),good_for_rhs(P).
props_change2(TestID,IO,P):-
% -  ensure_propcounts(TestID),
  %ensure_prop_change(E),
  findall(Q-I_or_O,counts_change(TestID,_,I_or_O,Q,_,_),L),list_to_set(L,S),!,member(P-IO,S),ok_deduce(P).
%ensure_prop_change(IO,P):- (var(P)->props_change(_TestID,IO,P);true).

in_out_atoms(in,out).

counts_change(TestID,ExampleNum,In,P,N2,N1):- in_out_atoms(In,Out),
   ensure_propcounts(TestID),
   propcounts(TestID, ExampleNum, Out, count, N1, P), ok_deduce(P),
   ExampleNum = trn+_,
   (propcounts(TestID, ExampleNum, In, count, N2, P) -> true ; N2=0), N1\==N2.

counts_change(TestID,ExampleNum,Out,P,N1,N2):- in_out_atoms(In,Out),
   ensure_propcounts(TestID),
   propcounts(TestID, ExampleNum, In, count, N1, P), ok_deduce(P),
   ExampleNum = trn+_,
   (propcounts(TestID, ExampleNum, Out, count, N2, P) -> true ; N2=0), N1\==N2.




ensure_scene_change_rules(TestID):-
 ensure_test(TestID),
 (\+ ac_unit(TestID,_,_,_) -> compute_scene_change(TestID) ; true).

compute_scene_change(TestID):-
 clear_scene_rules(TestID),  
 synth_one_scene_example(TestID),
 compute_rest_of_scene_change(TestID).


synth_one_scene_example(TestID):- 
  with_pair_mode(single_pair,
   (current_test_example(TestID,ExampleNum),
    compute_one_scene_example(TestID,ExampleNum),
    solve_via_scene_change_rules(TestID,ExampleNum))).

compute_one_scene_example(TestID,ExampleNum):-
 ensure_test(TestID),
 with_pair_mode(single_pair,
   with_example_num(ExampleNum,
     compute_scene_change_passes(TestID))).

compute_rest_of_scene_change(TestID):-
 ensure_test(TestID),
 with_pair_mode(whole_test,
   with_example_num(trn+_,
     compute_scene_change_passes(TestID))).

compute_scene_change_passes(TestID):-
 retractall(ac_db_unit(TestID,_,_,_)),
   must_det_ll((
    compute_scene_change_pass1(TestID),  
    compute_scene_change_pass2(TestID),
    compute_scene_change_pass3(TestID),
    compute_scene_change_pass4(TestID))),!.

compute_scene_change_pass1(TestID):- 
  clear_object_dependancy(TestID),
  ensure_object_dependancy(TestID),
  print_object_dependancy(TestID),!.


compute_scene_change_pass2(TestID):- 
  retractall(ac_db_unit(TestID,_,_,_)),
    forall(pass2_clause(TestID,Clause),
      assert_accompany_changed_db(TestID,Clause)),
  print_scene_change_rules(compute_scene_change_pass2,TestID).

compute_scene_change_pass3(TestID):-
 must_det_ll((
 % set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_pass2a)),
  % set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_pass2b)),
  set_of_changes(TestID,compute_scene_change_pass3a(TestID)),  
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes1)),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes2)),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes3)),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes4)),
  set_of_changes(TestID,compute_scene_change_pass3c(TestID)),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes4a)),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes4b)),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes5)),
  set_of_changes(TestID,compute_scene_change_pass3b(TestID,correct_antes6)),
  set_of_changes(TestID,compute_scene_change_pass3c(TestID)))),!.


compute_scene_change_pass3a(TestID,IO_-P):- 
   findall_vset_R(PSame,ac_unit(TestID,IO_,P,PSame),List),
   m_unifiers(List,ListR),
   update_accompany_changed_db(compute_scene_change_pass3a,TestID,IO_,P,ListR).
compute_scene_change_pass3a(_,_).


compute_scene_change_pass3b(TestID,P4,IO_-P):-
  findall_vset_R(PSame,ac_unit(TestID,IO_,P,PSame),SameS1),
  my_partition(is_debug_info,SameS1,Skip,SameS),
  call(P4,TestID,IO_,P,SameS,KeptS), KeptS\==[],!,
  if_t(SameS\=@=KeptS,
     (append(KeptS,Skip,Kept),
      update_accompany_changed_db(P4,TestID,IO_,P,Kept))).
compute_scene_change_pass3b(_,_,_). 
/*
compute_scene_change_pass3b(TestID,P4,IO_-P):-
  %findall_vset_R(PSame,ac_unit(TestID,IO_,P,PSame),SameS),
  ac_unit(TestID,IO_,P,PSame),
  call(P4,TestID,IO_,P,PSame,KeptS), KeptS\==[],!,
  if_t(PSame\=@=KeptS,     
      update_accompany_changed_db(P4,TestID,IO_,P,KeptS)).
compute_scene_change_pass3b(_,_,_). 
*/

compute_scene_change_pass3c(_,_):-!.
compute_scene_change_pass3c(TestID,IO_-P):-
  ac_unit(TestID,IO_,P,PSame1),
  my_partition(is_debug_info,PSame1,Skip,PSame),
  findall(DSame,
     (ac_unit_db(TestID,IO_,DP,DSame), 
      same_rhs_property(DP,P),at_least_one_overlap(DSame,PSame)),
   SL),  SL = [_,_|_],
  common_members(SL,Commons),
  forall((ac_unit_db(TestID,IO_,DP,DSame),same_rhs_property(DP,P)),
      (intersection(DSame,Commons,_,Kept,_),
        ignore((Kept\==[],append(Kept,Skip,Save),update_accompany_changed_db(pass3c,TestID,IO_,P,Save))))),

  print_scene_change_rules_if_different(compute_scene_change_pass3c,ac_unit,TestID),
  !.
compute_scene_change_pass3c(_,_).

compute_scene_change_pass4(TestID):-
   nop(compute_scene_change_pass3(TestID)),!.

set_of_ps(TestID,Ps):-
  ((findall_vset_R(Ctx-P1,
    ((ac_unit(TestID,IO_,P,_)
     %;ensure_props_change(TestID,IO_,P)
     %;pass2_rule(TestID,IO_,P,_)
     ),
    io_to_cntx(IO_,Ctx),into_rhs(P,P1)), Ps))).

set_of_changes(TestID,P1):-
 ((
  set_of_ps(TestID,Ps),
  why_last(P1,Why),
  %findall_vset_R(IO_-P,(ac_unit(TestID,IO_,P,_)), Ps),
  maplist(P1,Ps),
  print_scene_change_rules_if_different(Why,ac_unit,TestID))).

why_last1(A,E):- \+ compound(A),!, (atom(A);string(A)),A=E.
why_last1([H|T],E):- !, ((T\==[],why_last1(T,E));why_last1(H,E)),!.
why_last1(C,E):- compound_name_arguments(C,F,A),why_last1([F|A],E),!.
why_last(A,E):- why_last1(A,E),!.
why_last(E,E).


find_rhs(ac_unit(_Tst,_IO,P,_PConds),Out):- into_rhs(P,Out).
find_rhs(l2r(_Tst,_IO,P),Out):- into_rhs(P,Out).
find_rhs(ac_db(_Tst,_IO,P,_PConds),Out):- into_rhs(P,Out).
find_rhs(ac_unit_db(_Tst,_IO,P,_PConds),Out):- into_rhs(P,Out).
find_rhs(ac_unit(_Tst,_IO,P,_PConds),Out):- into_rhs(P,Out).
find_rhs(ac_listing(_Tst,_IO,P,_PConds),Out):- into_rhs(P,Out).
find_rhs(P,E):- sub_cmpd(rhs(E),P),!.
%into_rhs(edit(_,_,_,R),P):- !, into_rhs(R,P).
%into_rhs(edit(_,_,R),P):- !, into_rhs(R,P).
into_rhs(P,P):- \+ compound(P),!.
into_rhs(edit(R),P):- !, into_rhs(R,P).
into_rhs(create(R),P):- !, into_rhs(R,P).
into_rhs(delete(R),P):- !, into_rhs(R,P).
into_rhs(rhs(R),P):- !, into_rhs(R,P).
into_rhs([R],P):- !, into_rhs(R,P).
into_rhs(P,P).


should_replace(
               _Info1,IO1,P1,_Kept1,
               _Info2,IO2,P2,_Kept2):- P1==P2, IO1==IO2,!.
% 2-4: IO,P,Kept
should_replace(Rule,AltRule):-
  Rule=..[_,_,Info1,IO1,P1,Kept1],AltRule=..[_,_,Info2,IO2,P2,Kept2],
  \+ \+ should_replace(
               Info1,IO1,P1,Kept1,
               Info2,IO2,P2,Kept2).
nb_append(List,Rule):- member(AltRule,List),should_replace(Rule,AltRule), functor(Rule,_,A),
  forall(between(1,A,N),(arg(N,Rule,Arg),nb_setarg(N,AltRule,Arg))),!.
nb_append(List,Rule):- setarg(1,List,OH),arg(2,List,OT),NewTail=[OH|OT],nb_setarg(2,List,NewTail),nb_setarg(1,List,Rule).

assert_accompany_changed_db(TestID,List):- is_list(List),maplist(assert_accompany_changed_db(TestID),List).
assert_accompany_changed_db(List,Rule):- is_list(List),nb_append(List,Rule). % ac_unit(TestID,Ctx,P,Kept)
assert_accompany_changed_db(TestID,Rule):- Rule=..[_,_,IO,P,Kept], 
  io_to_cntx(IO,Ctx),  assert_ilp_b(ac_db_unit(TestID,Ctx,P,Kept)).

assert_accompany_changed_db(_TestID,_IO_,_P,Kept):- Kept==[],!.
assert_accompany_changed_db(TestID,IO_,P,Kept):- 
  io_to_cntx(IO_,Ctx),  
   assert_accompany_changed_db(TestID,ac_unit(TestID,Ctx,P,Kept)).


%assert_ilp_b(Term):- \+ clause_asserted(Term),!, pp_ilp(assert_ilp_b=Term), asserta_new(Term).
assert_ilp_b(Term):- asserta_new(Term).
%assert_ilp_b(Term):- pp_ilp(assert_ilp_b=Term),!, assert_if_new(Term).

show_if_changing(Why,_TestID,_Ctx,PP,Was,P,Kept):- 
 \+ \+ ignore((
        Was\=@=Kept,
        pred_intersection(=@=,Was,Kept,_,Same,MRemoved,MAdded),
        pred_intersection(about_same_property,MRemoved,MAdded,GRemoved,GAdded,Removed,Added),
        banner_lines(gold,2),
        pp_ilpc(0,yellow,added(Why,P)=Added),
        pp_ilpc(2,cyan,generalized(Why,PP)=GAdded),
        pp_ilpc(2,brown,removedG(Why,PP)=GRemoved),
        pp_ilpc(2,orange,removed(Why,PP)=Removed),
        pp_ilpc(2,blue,unchanged(Why,PP)=Same),
        !)).


update_accompany_changed_db(Why,TestID,IO_,P,Kept):- Kept\==[],       
 sort([head(P)|Kept],KeptS),

 forall(io_to_cntx(IO_,Ctx),
   (findall_vset([head(PP)|Was],(clause(ac_db_unit(TestID,Ctx,PP,Was),true,Ref),PP=@=P),PropsWasF),
    flatten(PropsWasF,PropsWas),
    sort(PropsWas,PropsWasS), 
    if_t(PropsWasS\=@=KeptS,show_if_changing(Why,TestID,Ctx,P,PropsWasS,P,KeptS)),
    forall(
      (clause(ac_db_unit(TestID,Ctx,PP,Was),true,Ref),PP=@=P),
         (nop(if_t(Was\=@=Kept,show_if_changing(Why,TestID,Ctx,PP,Was,P,Kept))),erase(Ref))))),
 assert_accompany_changed_db(TestID,IO_,P,Kept).


at_least_one_overlap(DSame,PSame):- member(S,PSame), \+ assume_prop(S),
   member(DS,DSame),
   about_same_property(DS,S),!.

about_same_property(DS,S):- \+ \+ (same_rhs_property(DS,S);( \+ DS\=S )).
same_rhs_property(DS,S):- \+ \+ (DS=@=S;other_val(S,DS)).


% Retain Overlap
correct_antes1(TestID,IO_,P,PSame,SL):- 
  %rev_in_out_atoms(OI,IO_),
  findall(S,
   (member(S,PSame),
     \+ \+ ((
       forall((ac_unit(TestID,IO_,DP,DSame),at_least_one_overlap(DSame,PSame)),
          ((P==DP)-> true; (member(DS,DSame),  
             \+ negated_s_lit(S,_), other_val(S,DS))))))),
   SL), SL\==[],!.

correct_antes1(_TestID,_IO_,_P,PSame,PSame).

is_unbound_prop(S):- make_unifiable(S,DS), S=@=DS,!.

% Make sure each arguement is transformed corretly
correct_antes2(_TestID,_IO_,_P,PSame,Kept):-  maplist(ensure_xformed,PSame,Kept),!.
ensure_xformed(pg(_,A,B,C),pg(_,A,B,C)):-!.
ensure_xformed(A,A).

% Remove Redundant Overlap
correct_antes3(_TestID,_IO_,_P,PSame,SL):- %fail,
  findall(S, ( member(S,PSame), \+ is_unbound_prop(S)), SL), SL\==[],!.
correct_antes3(_TestID,_IO_,_P,PSame,PSame).


% Remove Redundant Overlap
correct_antes4(TestID,IO_,P,PSame,SL):- %fail,
  findall(S,
   ( member(S,PSame), 
     (negated_s_lit(S,_)->true;
      \+ ((  
       forall((ac_unit(TestID,IO_,DP,DSame),
              same_rhs_property(P,DP)),          
            (member(DS,DSame), DS=@=S)))))),
   SL), 

  SL\==[],!.
correct_antes4(_TestID,_IO_,_P,PSame,PSame).

% Make sure each arguement is transformed corretly
correct_pass2a(_TestID,_IO_,_P,PSame,Kept):- 
  my_partition(is_giz_prop,PSame,Giz,NonGiz),
  append_set_level(Giz,UGiz),append(UGiz,NonGiz,Kept),!.
correct_pass2a(_TestID,_IO_,_P,Kept,Kept).

is_giz_prop(giz(_)).


% Make sure each arguement is transformed corretly
correct_pass2b(_TestID,_IO_,_P,PSame,Kept):- 
  my_partition(is_info_prop,PSame,Giz,NonGiz),
  append_set_level(Giz,UGiz),append(UGiz,NonGiz,Kept),!.
correct_pass2b(_TestID,_IO_,_P,Kept,Kept).

is_info_prop(iz(Info)):- compound(Info),Info = info(_).


% Make sure each arguement is transformed corretly
correct_pipe2c(IO,P1,Rules,Out):- trace,%mfail,
 must_det_ll((  
  my_partition(is_rule_about_same(IO,P1),Rules,AboutSame,AboutSimular),
  findall(LHS,ac_info_unit(AboutSame,IO,_,_,LHS),RulesAboutSames),flatten(RulesAboutSames,RulesAboutSamesFlat),
    sames_must_have_sames(RulesAboutSamesFlat,BetterRulesAboutSames),BetterRulesAboutSames\==[],
  findall(Info,ac_info_unit(AboutSame,IO,_,Info,_),InfoAboutSames),flatten(InfoAboutSames,InfoAboutSamesFlat),
    merge_vals(InfoAboutSamesFlat,BetterInfoAboutSames),
  append(AboutSimular,[ac_unit(_,IO,P1,[iz(info(BetterInfoAboutSames))|BetterRulesAboutSames])],Out))).
correct_pass2c(_IO_,_P,Kept,Kept).

correct_pass2d(IO,P1,Rules,Out):- %mfail, %trace,
 must_det_ll((
  my_partition(is_rule_about(IO,P1),Rules,AboutSame,AboutSimular),
  findall(LHS,ac_info_unit(AboutSimular,IO,_,_,LHS),RulesAboutSimulars),
            differents_must_differents(RulesAboutSimulars,BetterRulesAboutSimulars),BetterRulesAboutSimulars\==[],
  findall(Info,ac_info_unit(AboutSimular,IO,_,Info,_),InfoAboutSimulars),
       merge_list_values(InfoAboutSimulars,InfoAboutSimularsFlat), merge_vals(InfoAboutSimularsFlat,BetterInfoAboutSimulars),
  append(AboutSame,[ac_unit(_,IO,P1,[iz(info(BetterInfoAboutSimulars))|BetterRulesAboutSimulars])],Out))).
correct_pass2d(_IO_,_P,Kept,Kept).




% Remove Redundant Overlap
correct_antes4a(TestID,IO_,VP,PSame,SLPSame):- fail,
  %rev_in_out_atoms(OI,IO_),
  ensure_deref_value(VP,P),
  \+ \+ ((ac_unit(TestID,IO_,DP,_),other_val(P,DP))),
  findall(mv4a(info(changes_from(S,P))),
       ((member(S,PSame), \+ negated_s_lit(S,_), S\= mv4a(info(_)),
         other_val(S,P))),
     SL), SL\==[],!, 
  append(PSame,SL,SLPSame).

correct_antes4a(_TestID,_IO_,_P,PSame,PSame).



% Remove Single Chhangers
correct_antes4b(TestID,IO_,VP,PSame,SLPSame):-  fail,
  %rev_in_out_atoms(OI,IO_),
  ensure_deref_value(VP,P),
  \+ \+ ((ac_unit(TestID,IO_,DP,_),other_val(P,DP))),
  findall(mv4b(info(changes_into(S,P))),
       ((member(S,PSame), \+ negated_s_lit(S,_), S\= mv4b(info(_)), other_val(S,P),
         forall((ac_unit(TestID,IO_,DP,DSame),other_val(P,DP)),
           \+ \+ (member(DS,DSame), other_val(S,DS), \+ negated_s_lit(DS,_))))), 
     SL), SL\==[],!, 
  append(PSame,SL,SLPSame).
correct_antes4b(_TestID,_IO_,_P,PSame,PSame).



% Add Negations
correct_antes5(TestID,IO_,P,PSame,Kept):- correct_antes_neg(TestID,IO_,P,PSame,Kept),!.
correct_antes5(_TestID,_IO_,_P,PSame,Kept):- vsr_set(PSame,Kept),!.
correct_antes5(_TestID,_IO_,_P,PSame,PSame).
correct_antes_neg(TestID,IO_,P,PSame,Kept):-
  findall( ( \+ DS),
   ((member(S,PSame), \+ negated_s_lit(S,_), is_unbound_prop(S), make_unifiable(S,DS),
     ac_unit(TestID,IO_,DP,DSame),      
     other_val(P,DP), %at_least_one_overlap(DSame,PSame),
     member(DS,DSame), \+ negated_s_lit(DS,_), \+ is_unbound_prop(DS), 
       \+ member(\+ DS,PSame))), SL), SL\==[],
  append(PSame,SL,Kept),Kept\==[], !.
correct_antes_neg(_TestID,_IO_,_P,PSame,PSame).


% DISABLED not really a loops
correct_antes6(_TestID,_IO_,P,PSame,Kept):- fail,
  findall(S, (member(S,PSame), \+ same_rhs_property(P,S)), Kept), Kept\==[],!.
correct_antes6(_TestID,_IO_,_P,PSame,PSame).

 
negated_s_lit(N,P):- compound(N), N = ( \+ P ). 


/*
correct_antes5(TestID,IO_,P,PSame,Kept):-   
   make_unifiable_u(P,U),
   is_accompany_changed_computed(TestID,IO_,U,DSame),
   P\=@=U,
   maplist(make_unifiable_u,DSame,USame),
   pred_intersection(other_val,PSame,USame,Kept,_,_,_),Kept\==[].
correct_antes5(_TestID,_IO_,_P,PSame,PSame).


solve_obj_group(VM,TestID,ExampleNum,ROptions,Objs,ObjsO):-
 forall(kaggle_arc(TestID,trn+N,_,_),
  ( findall(Out,((arc_cache:map_pairs(TestID,_,trn+N,info(Step,_,in_out,perfect_in_out,_,trn+N),PreObjs,Out),
      indv_props_list(Out,PropsO),
       closest_object(Out,PreObjs,PreObj),
       rewrite_rules_for(PreObj,Out,Sames,Diffs),
       ,OutL),
    findall(PreObjs,arc_cache:map_pairs(TestID,_,trn+N,info(0,_,in_out,_,_,trn+N),PreObjs,Out),PreObjs),
  homogenize(OutL,Sames,Diffs),
*/


tesT_compare_objects:- compare_objects([
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),
      vis2D(1,1),rot2D(sameR),loc2D(4,9),changes([]),iz(type(dots)),iz(type(dot)),iz(filltype(solid)),iz(jagged(true)),center2G(4,9),% obj_to_oid(t(af902bf9)>(tst+0)*in,37),globalpoints([yellow-point_04_09]),
      grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(4,6),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(4,6),obj_to_oid(t(af902bf9)>(tst+0)*in,39),globalpoints([yellow-point_04_06]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(1,6),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(1,6),obj_to_oid(t(af902bf9)>(tst+0)*in,40),globalpoints([yellow-point_01_06]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(10,5),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(10,5),obj_to_oid(t(af902bf9)>(tst+0)*in,41),globalpoints([yellow-point_10_05]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(6,5),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(6,5),obj_to_oid(t(af902bf9)>(tst+0)*in,42),globalpoints([yellow-point_06_05]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(10,1),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(10,1),obj_to_oid(t(af902bf9)>(tst+0)*in,43),globalpoints([yellow-point_10_01]),grid_size(10,10),iz(important)]),
    obj([mass(1),shape_rep(grav,[hv(1,1)]),colors_cc([cc(yellow,1.0)]),localpoints([yellow-hv(1,1)]),vis2D(1,1),rot2D(sameR),loc2D(6,1),changes([]),iz(type(dots)),iz(shape_rep(grav,dot)),iz(filltype(solid)),iz(jagged(true)),center2G(6,1),obj_to_oid(t(af902bf9)>(tst+0)*in,44),globalpoints([yellow-point_06_01]),grid_size(10,10),iz(important)])],
    OUTPUT),
  print(OUTPUT).

compute_scene_change_pipees(TestID):-
 must_det_ll(compute_scene_change_pipe1(TestID)),
    must_det_ll(get_scene_change_rules(TestID,pipe2_clause,Rules)),
    %banner_lines(yellow,3),
    must_det_ll(print_rules(pipe2_clause=Rules)),
    must_det_ll(compute_scene_change_each_pipe(Rules,[correct_pipe2a,correct_pipe2b],NewRules)), %compute_scene_change_pipe3a(Rules,NewRules),
    must_det_ll(banner_lines(yellow,3)),
    %print_rules(pipe3_clause=NewRules),
    must_det_ll(assert_accompany_changed_db(TestID,NewRules)).


%locally(nb_setval(show_object_grids,t), 
%  print_scene_change_rules(compute_scene_change_pipe2,TestID)).

/*
compute_scene_change_pipe3b(Rules,NewRules):-
  compute_scene_change_each_pipe(Rules,
    [correct_pipe2a,correct_pipe2b,
        call(banner_lines(yellow,3)),
      %  p2(update_scene_now),
        call(banner_lines(gold,3))],NewRules).
        correct_antes1,
        correct_antes2,
        correct_antes3,
        correct_antes4,
       %  p2(update_scene_now),
        correct_antes4a,
        correct_antes4b,
        correct_antes5,
        correct_antes6],NewRules).
*/
/*
update_scene_now(In,Out):- 
  findall_vset_R(IO-P,ac_unit(In,Ref,IO,P,PSame),PList),
  my_partition(is_rule_same(IO,P),In,Rules,Rest),
  generalize(Rules,GeneralizeRules),
  append(Rest,GeneralizeRules,Out).

*/
/*
(Step,IO_-P):-
  findall_vset_R(PSame,ac_unit(TestID,Info,IO_,P,PSame),SameS1),
  my_partition(is_debug_info,SameS1,Skip,SameS),
  call(Step,TestID,IO_,P,SameS,KeptS), KeptS\==[],!,
  if_t(SameS\=@=KeptS,
     (append(KeptS,Skip,Kept),
      update_accompany_changed_db(TestID,IO_,P,Kept))).
compute_scene_change_pipe3b(_,_,_). 
*/
shared_val(P1,P2):- same_prop_names(P1,P2), \+ other_val(P1,P2).

print_rules(Rules):- pp_ilp(Rules).

compute_scene_change_pipe3a(Rules,NewRules):-
  compute_scene_change_each_pipe(Rules,[correct_pipe2a,correct_pipe2b],NewRules),!.

compute_scene_change_each_pipe(Rules,[],Rules).
compute_scene_change_each_pipe(In,[Step|More],Out):-
 must_det_ll((
  trace,
  compute_scene_change_step(In,Step,Mid),
  if_t(In\=@=Mid,((call(banner_lines(gold,3)),pp_ilp(update(Step)=Mid)))),
  compute_scene_change_each_pipe(Mid,More,Out))).

%compute_scene_change_step(In,call_p2(Step),Out):- call(Step,In,Out),!.
%compute_scene_change_step(Same,call(Step),Same):- !, ignore(Step).
%compute_scene_change_step(In,whole(Step),Out):- !,
% findall_vset_R(IO-P,ac_unit(In,IO,P,_PSame),PList),
% do_in_plist_high(PList,Step,In,Out).
compute_scene_change_step(In,Step,Out):-
 must_det_ll((findall_vset_R(IO-P,ac_unit(In,IO,P,_PSame),PList),
  do_in_plist_low(PList,Step,In,Out))).

do_in_plist_high([IO-P|PList],Step,In,Out):-
  call(Step,IO,P,In,Mid),
  do_in_plist_high(PList,Step,Mid,Out).
do_in_plist_high([_|PList],Step,In,Out):- !, do_in_plist_high(PList,Step,In,Out).
do_in_plist_high([],_,InOut,InOut).

is_rule_about_same(IO,P1,Rule):- sub_var(IO,Rule),into_rhs(Rule,P2),shared_val(P1,P2).
%is_rule_about_simular(IO,P1,Rule):- sub_var(IO,Rule),into_rhs(Rule,P2),other_val(P1,P2).
is_rule_about_simular(IO,P1,Rule):- sub_var(IO,Rule),into_rhs(Rule,P2),other_val(P1,P2).
is_rule_about(IO,P1,Rule):- sub_var(IO,Rule),into_rhs(Rule,P2), once(shared_val(P1,P2);other_val(P1,P2);same_rhs_property(P1,P2)).

do_in_plist_low([],_,InOut,InOut):-!.
do_in_plist_low(_,_,[],[]):-!.
do_in_plist_low([IO-P|PList],Step,In,Out):-
    must_det_ll((call(Step,IO,P,In,NewRules),
    do_in_plist_low(PList,Step,NewRules,Out))),!.
do_in_plist_low([IO-P|PList],Step,In,Out):-
  must_det_ll((my_partition(is_rule_about(IO,P),In,Rules,Rest),
    call(Step,IO,P,Rules,NewRules),
    append(Rest,NewRules,RestNewRules),
    do_in_plist_low(PList,Step,RestNewRules,Out))),!.
do_in_plist_low([_|PList],Step,In,Out):- !, do_in_plist_low(PList,Step,In,Out).


sames_must_have_sames(I,O):- m_unifiers1(I,O),!.

% Make sure each arguement is transformed corretly
correct_pipe2a(IO,P1,Rules,Out):- trace,%mfail,
 must_det_ll((
  
  my_partition(is_rule_about_same(IO,P1),Rules,AboutSame,AboutSimular),
  findall(LHS,ac_unit(AboutSame,_,IO,_,LHS),RulesAboutSames),flatten(RulesAboutSames,RulesAboutSamesFlat),
    sames_must_have_sames(RulesAboutSamesFlat,BetterRulesAboutSames),BetterRulesAboutSames\==[],
  findall(Info,ac_unit(AboutSame,Info,IO,_,_),InfoAboutSames),flatten(InfoAboutSames,InfoAboutSamesFlat),
    merge_vals(InfoAboutSamesFlat,BetterInfoAboutSames),
  append(AboutSimular,[ac_unit(IO,BetterInfoAboutSames,P1,BetterRulesAboutSames)],Out))).

correct_pipe2b(IO,P1,Rules,Out):- %mfail,
 trace,
 must_det_ll((
  my_partition(is_rule_about(IO,P1),Rules,AboutSame,AboutSimular),
  findall(LHS,(rtrace,ac_unit(AboutSimular,_,IO,_,LHS)),RulesAboutSimulars),
            differents_must_differents(RulesAboutSimulars,BetterRulesAboutSimulars),BetterRulesAboutSimulars\==[],
  findall(Info,ac_unit(AboutSimular,Info,IO,_,_),InfoAboutSimulars),
       merge_list_values(InfoAboutSimulars,InfoAboutSimularsFlat), merge_vals(InfoAboutSimularsFlat,BetterInfoAboutSimulars),
  append(AboutSame,[ac_unit(IO,BetterInfoAboutSimulars,P1,BetterRulesAboutSimulars)],Out))).

merge_list_values([A,B],Out):- merge_vals([A],[B],Out),!.
merge_list_values([A],[A]):-!.
merge_list_values([],[]):-!.
merge_list_values([M|InfoAboutSimulars],Out):-
  merge_vals([M],InfoAboutSimulars,Mid),merge_list_values(Mid,Out).

differents_must_differents([A],[A]).
differents_must_differents([L1|ListOfLists],O):-   
  [L1|ListOfLists]=L1ListOfLists,length(L1ListOfLists,Expected),
  which_members_vary(L1,Expected,ListOfLists,VariedMembers),
  maplist(only_these_members_or_negation(VariedMembers),L1ListOfLists,O).

only_these_members_or_negation([V|VariedMembers],I,[P|O]):- 
  select(P,I,II),  (\+ V \= V), !,
  only_these_members_or_negation(VariedMembers,II,O).
only_these_members_or_negation([V|VariedMembers],I,[ ( \+ V)| O]):- 
  only_these_members_or_negation(VariedMembers,I,O).
only_these_members_or_negation([],_,[]).

which_members_vary([HAD|L1],RRR,[UProp|VariedMembers]):-
 trace, make_unifiable_cc(HAD,UProp),
 variance_had_counts(UProp,HAD,RRR,Versions,OtherMissing,CountOfEach,Variance),
  pp([UProp,HAD,Versions,OtherMissing,CountOfEach,Variance]),!,
 which_members_vary(L1,RRR,VariedMembers).
which_members_vary(L1,[R|RR],[UProp|VariedMembers]):- 
  trace, member(HAD,R), make_unifiable_cc(HAD,UProp), \+ member(UProp,L1), RRR = [R|RR],
  variance_had_counts(UProp,HAD,RRR,Versions,OtherMissing,CountOfEach,Variance),
   pp([UProp,HAD,Versions,OtherMissing,CountOfEach,Variance]),!,
  which_members_vary(L1,RRR,VariedMembers).
which_members_vary([_|L1],RRR,VariedMembers):-
 which_members_vary(L1,RRR,VariedMembers).


/*
update_scene_now(TestID,IO,P,RulesIn,RulesOut):-
  ac_unit(TestID,Info,IO_,P,PSame1),
  my_partition(is_debug_info,PSame1,Skip,PSame),
  findall(DSame,
     (ac_db_unit(RulesIn,Ref,IO_,DP,DSame), 
      same_rhs_property(DP,P),at_least_one_overlap(DSame,PSame)),
   SL),  SL = [_,_|_],
  common_members(SL,Commons),
  forall((ac_db_unit(RulesIn,Ref,IO_,DP,DSame),same_rhs_property(DP,P)),
      (intersection(DSame,Commons,_,Kept,_),
        ignore((Kept\==[],append(Kept,Skip,Save),

  update_accompany_changed_db(TestID,IO_,P,Save))))),

  print_scene_change_rules_if_different(update_scene_now,ac_unit,TestID),
  !.
update_scene_now(_,_).
*/


