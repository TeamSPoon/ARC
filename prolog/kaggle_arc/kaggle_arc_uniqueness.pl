/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)

The code above is in Prolog and is part of a project called "ARC" which is available on Kaggle. The project is designed to test the ability of artificial intelligence to solve visual reasoning tasks. The code is divided into several sections, each of which is responsible for a specific task.

The first section of the code includes a header file and a module that defines the size of the grid used in the project. The second section of the code uses the Constraint Logic Programming over Finite Domains (CLPFD) library to solve the visual reasoning tasks.

The third section of the code defines a predicate called "solve_via_scene_change" which is used to solve the visual reasoning tasks by changing the scene. This predicate is called with a test ID which is used to identify the test to be solved. The predicate then prints the test ID and calls another predicate called "solve_via_scene_change_rules" for each example in the test.

The fourth section of the code defines a predicate called "when_entire_suite" which is used to execute a goal when the entire test suite is being run. This predicate is called with two goals, the first of which is executed when the entire test suite is being run and the second of which is ignored.

Overall, the code is designed to solve visual reasoning tasks using the CLPFD library and a scene change approach. The code is well-organized and easy to read, with each section of the code responsible for a specific task. However, some of the predicates used in the code may be difficult to understand for those who are not familiar with Prolog or the CLPFD library.

The ARC project is designed to solve a wide range of visual reasoning tasks, including pattern completion, analogy, classification, and prediction. In pattern completion tasks, the AI is presented with an incomplete pattern and must fill in the missing pieces to complete the pattern. In analogy tasks, the AI is presented with two sets of patterns and must identify the relationship between them in order to complete a third set of patterns. In classification tasks, the AI is presented with a set of patterns and must group them into categories based on their similarities. In prediction tasks, the AI is presented with a set of patterns and must predict the next pattern in the sequence. These tasks are designed to test the ability of artificial intelligence to reason visually and to identify patterns and relationships in visual data.
*/


:- include(kaggle_arc_header).
:- include(kaggle_arc_footer).

:- abolish(arc_cache:trans_rule_db/4).
:- dynamic(arc_cache:trans_rule_db/4).
:- abolish(ac_db_unit/4).
:- dynamic(ac_db_unit/4).

:- ensure_loaded(kaggle_arc_grid_size).

:- use_module(library(clpfd)).

:- multifile is_fti_step/1.
:- discontiguous is_fti_step/1.



% must_det_ll/1 is a predicate that enforces determinism and logical purity and prints a warning if it fails.
% it is loaded from the kaggle_arc_header.pl file


%dont_notice(iz(algo_sid(norm,_))).
%dont_notice(iz(sid(_))).


% Define predicates that shouldn't be noticed
%dont_notice(global2G(_,_)).
dont_notice(P):- var(P),!,fail.
dont_notice(simularz(_,_)).
dont_notice(global2G(_,_)).
dont_notice(iz(symmetry_type(rollD, _))).
dont_notice(\+ P):- !, dont_notice(P).
dont_notice(sym_counts(_,N)):- number(N), N < 10.
dont_notice(Comp):- sub_var(comp, Comp).
dont_notice(iz(algo_sid(comp, _))).
dont_notice(rotSize2D(grav, _, _)).
dont_notice(pg(_, mass(_), rank1, _)).
dont_notice(changes(_)).
dont_notice(pg(_, cc(bg, _), rank1, _)).
dont_notice(pg(_, cc(fg, _), rank1, _)).
%dont_notice(sym_counts('sym_node_*_+_@_~',_)).
dont_notice(iz(media(_))).
dont_notice(iz(type(_))).
dont_notice(pg(_, iz(info(birth(_))), _, _)).
dont_notice(pg(_, cc(bg, _), rankLS, _)).
dont_notice(pg(_, occurs_in_links(contained_by, _), _, _)).
dont_notice(pg(_, pen(_), rank1, _)).
dont_notice(iz(fg_or_bg(_))).
dont_notice(giz(_)).
dont_notice(iz(i_o(_))).
dont_notice(iz(stype(_))).
dont_notice(global2G(_,_)).
dont_notice(iz(locX(_))).
dont_notice(iz(locY(_))).
dont_notice(iz(sizeGX(_))).
dont_notice(iz(sizeGY(_))).
dont_notice(iz(symmetry_type(rollD, _))).
dont_notice(link(contains,_)).
dont_notice(links_count(sees, _)).
dont_notice(occurs_in_links(contained_by,_)).
dont_notice(occurs_in_links(sees,_)).
dont_notice(oid(_)).
dont_notice(iz(symmetry_type(_, _))).
dont_notice(pg(_,pen(_), rankLS ,_)).
dont_notice(pg(_,iz(_),rankLS,_)).
dont_notice(pg(_,iz(_), rank1,_)).
dont_notice(pg(_,empty_area(_),rankLS,_)).
dont_notice(pg(_,mass(_),rankLS,_)).
dont_notice(pg(_, occurs_in_links(contained_by, _), rank1, _)).

dont_notice(center2D(_, _)).
dont_notice(loc2D(_, _)).
dont_notice(vis2D(_, _)).
dont_notice(empty_area(_)).
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

dont_induce(P):- dont_deduce(P).

dont_deduce(link(sees(_),_)).
%dont_deduce(giz(_)).
%dont_deduce(pg(_,_,_,_)).
dont_deduce(pg(_,iz(_),rankLS,_)).
dont_deduce(pg(_,_,rankLS,_)).
dont_deduce(pg(_, _, _, _)).
%dont_deduce(size2D(_)).
%dont_deduce(global2G(_,_)).
%dont_deduce(vis2D(_, _)).
dont_deduce(P):- \+ compound(P),!,fail.
dont_deduce(P):- sub_term(G,P),compound(G),is_grid(G),!.
dont_deduce(P):- sub_term(G,P),compound(G),is_object(G),!.
dont_deduce(grid(_)).
%dont_deduce(iz(_)).
%dont_deduce(iz(_)).

%dont_deduce(P):- compound(P),compound_name_arguments(P,_,[X]),number(X).
dont_deduce(grid_ops(comp,_)). 
%dont_deduce(iz(stype(_))). 
dont_deduce(iz(symmetry_type(_, _))). % rot2D(rot90), grid_ops(comp, []), changes([]), iz(fg_or_bg(iz_fg)), links_count(contained_by, 0), links_count(contains, 0), cc(plain_var, 0), cc(bg, 0), global2G(9, 9), iz(sizeGX(1)), unique_colors_count(1), empty_area(0), iz(algo_sid(comp, sid_12)), iz(algo_sid(norm, sid_12)), iz(symmetry_type(flipDHV, false)), iz(symmetry_type(rot143, true)), iz(symmetry_type(flipV, true)), iz(symmetry_type(flipH, true)), iz(symmetry_type(rot270, false)), iz(symmetry_type(rot90, false)), iz(symmetry_type(sym_h_xor_v, false)), iz(symmetry_type(sym_hv, true)), iz(filltype(solid)), iz(colormass), iz(media(shaped)), iz(info(birth(colormass))), pg(_1477530, mass(_1477540), rankLS, largest), pg(_1477550, iz(sizeGX(_1477564)), rankLS, smallest), pg(_1477574, iz(sizeGY(_1477588)), rankLS, largest), pg(_1477598, iz(cenGX(_1477612)), rankLS, largest), pg(_1477622, iz(cenGY(_1477636)), rankLS, largest), pg(_1477646, unique_colors_count(_1477656), rankLS, smallest), pg(_1477666, empty_area(_1477676), rankLS, smallest).
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
do_deduce(iz(cenX(_))).
do_deduce(iz(cenY(_))).
do_deduce(iz(locX(_))).
do_deduce(iz(locY(_))).
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
make_unifiable_u1(X1, X2):- verbatum_unifiable(X1), !, X2=X1.
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


sometimes_when_lost(Goal):-!, fail, call(Goal).
solve_via_scene_change:-  get_pair_mode(entire_suite),!, cls, 
 forall_count(all_arc_test_name(TestID),
   solve_via_scene_change(TestID)).

solve_via_scene_change:-  clsmake, ensure_test(TestID), %make,
 solve_via_scene_change(TestID).


solve_via_scene_change(TestID):-  
 must_det_ll((
  %force_clear_test(TestID),
  clear_scene_rules(TestID),
  abolish(arc_test_property/4),
  dynamic(arc_test_property/4),
  clear_object_dependancy(TestID),
  print_test(TestID),
  %do_menu_key('i'),
  print_individuals(TestID),
  %forall(kaggle_arc(TestID, ExampleNum, _, _), ignore(print_individuals(TestID, ExampleNum))),

  %repress_some_output(learn_solve_via_grid_change(TestID)),
  ExampleNum=tst+_,
  true)),
  forall(kaggle_arc(TestID,ExampleNum,_,_),
     with_individuated_cache(true, solve_via_scene_change_rules(TestID, ExampleNum))).

when_entire_suite(Goal,_Goal2):- get_pair_mode(entire_suite),!, call(Goal).
when_entire_suite(_Goal,Goal2):- call(Goal2).

maybe_repress_output(Goal):- call(Goal).
%repress_output(Goal):- with_output_to(atom(_), Goal).
repress_output(Goal):- call(Goal).
%repress_some_output(Goal):- when_entire_suite(with_pair_mode(whole_test,repress_output(Goal)),Goal).
repress_some_output(Goal):- locally(set_prolog_flag(nogc, false), Goal).


learn_solve_via_grid_change(TestID):- 
 repress_output((
  must_det_ll((
 %  detect_pair_hints(TestID),  
 %  save_test_hints_now(TestID),
   learn_grid_size(TestID))))),
 repress_some_output((
  must_det_ll(( 
   %not_warn_skip(ensure_propcounts(TestID)),
   % % %clear_scene_rules(TestID),
   compute_scene_change(TestID))))).


% =====================================================================
is_fti_step(simple).
% =====================================================================
simple(SegOptions, VM):-
  GPoints = VM.lo_points,
  must_det_ll(once(indv_color_masses(VM.h, VM.v, VM.start_points, GPoints, SegOptions, Segs))),
  gset(VM.lo_points) = [],
  maplist(make_indiv_object(VM, [iz(birth(SegOptions))]), Segs, Objs),
  assumeAdded(VM, Objs), !.

% =====================================================================
is_fti_step(i_opts).
% =====================================================================
i_opts(Shapes, count_equ(NF), Mono, Nsew, IncludeBG, VM):-
  SegOptions = i_opts(Shapes, count_equ(NF), Mono, Nsew, IncludeBG),
  simple(SegOptions, VM).


indv_options(Opt):- nonvar(Opt), !, (Opt=simple(SOpts)->indv_options(SOpts);true).
indv_options(Opt):-
 %no_repeats_var(Opt),
 member(Opt,
     [
      i_opts(shapes(none), count_equ(NF), colors(each), dirs(nsew), incl_bg(edge)),
      %i_opts(shapes(none), count_equ(NF), colors(each), dirs(nsew), incl_bg(edge)),
      %i_opts(shapes(none), count_equ(NF), colors(mono), dirs(diags_nsew), incl_bg(edge)),

      %i_opts(shapes(none), count_equ(NF), colors(each), dirs(diags_nsew), incl_bg(false)),
      %i_opts(shapes(none), count_equ(NF), colors(each), dirs(nsew), incl_bg(false)), 
      %i_opts(shapes(none), count_equ(NF), colors(mono), dirs(diags_nsew), incl_bg(false)),
      %i_opts(shapes(none), count_equ(NF), colors(mono), dirs(nsew), incl_bg(false)),

      %i_opts(shapes(none), count_equ(NF), colors(each), dirs(nsew), incl_bg(true)),
      %i_opts(shapes(none), count_equ(false), colors(each), dirs(diags_nsew), incl_bg(true)),
      %i_opts(shapes(none), count_equ(NF), colors(each), dirs(points), incl_bg(false)),
      %i_opts(shapes(none), count_equ(false), colors(mono), dirs(diags_nsew), incl_bg(true)),
      %i_opts(shapes(none), count_equ(false), colors(mono), dirs(nsew), incl_bg(true)),
      %i_opts(shapes(none), count_equ(false), colors(each), dirs(points), incl_bg(true)),
     []]),
     compound(Opt),
     nop(dif(NF, false)).

filter_points(i_opts(_, _, _Mono, _Diag, incl_bg(false)), GPoints, Points):- !, my_partition(is_fg_point, GPoints, Points, _).
filter_points(i_opts(_, _, _Mono, _Diag, incl_bg(_)), Points, Points).

dir_ok(_, i_opts(_, _, _, dirs(points), _)):-!, fail.
dir_ok(D, i_opts(_, _, _, dirs(nsew), _)):-!, n_s_e_w(D).
dir_ok(D, i_opts(_, _, _, dirs(diags_nsew), _)):-!, (n_s_e_w(D);is_diag(D)), !.
dir_ok(_, i_opts(_, _, _, any, _)).
color_dir_kk(Dir, Color):- is_bg_color(Color), !, \+ is_diag(Dir).
color_dir_kk(_  , Color):- is_fg_color(Color), !.
colors_joinable(i_opts(_, _, colors(mono), _, _), C1, C2):- is_fg_color(C1), is_fg_color(C2).
colors_joinable(i_opts(_, _, colors(each), _, _), C1, C1):- is_fg_color(C1).
colors_joinable(i_opts(_, _, colors(black), _, _), C1, C1):- is_bg_color(C1).
%colors_joinable(i_opts(_, _, _, _, incl_bg(true)), C1, C1):- is_bg_color(C1).

%?- into_grid('37d3e8b2', G), indv_options(Opt), individuate_3(Opt, G, InC), print_grid(Opt, InC)



objs_to_spoints(InC, InPSS):-
  maplist(globalpoints, InC, InP), maplist(sort, InP, InPS), sort(InPS, InPSS).


rest_are_points([P|PointsRest], [[P]|More]):-
  rest_are_points(PointsRest, More), !.
rest_are_points([], []).

is_adjacent_to_one(_, FGPoints, _-P1):-
  is_adjacent_point(P1, Dir, P2), Dir\==c, \+ \+ member(_-P2, FGPoints), !.
is_adjacent_to_one(2, FGPoints, _-P1):- !,
  is_adjacent_point(P1, Dir1, P12), Dir1\==c, is_adjacent_point(P12, Dir2, P2), Dir2\==c, \+ \+ member(_-P2, FGPoints), !.

indv_color_masses(H, V, Orig, PointsRest, SegOptions, More):-
  var(SegOptions), !, indv_options(SegOptions), nonvar(SegOptions),
  indv_color_masses(H, V, Orig, PointsRest, SegOptions, More).

indv_color_masses(H, V, Orig, PointsRest, simple(SegOptions), More):- !,
  indv_color_masses(H, V, Orig, PointsRest, SegOptions, More).

indv_color_masses(H, V, Orig, PointsRest, SegOptions, More):- SegOptions = i_opts(A, B, C, D, incl_bg(edge)), !,
 must_det_ll((
   my_partition(is_fg_point, PointsRest, FGPoints, BGPoints0),
   include(is_adjacent_to_one(1, FGPoints), BGPoints0, BGPoints),
   BGOpts = i_opts(A, B, colors(black), dirs(nsew), incl_bg(true)),
   %subst001(BGPoints, 'black', 'wfg', BGPointsFG), print_grid(H, V, wfg, BGPointsFG),
   all_color_masses(H, V, Orig, BGPoints, BGOpts, BlackObjs),
   FGOpts = i_opts(A, B, C, D, incl_bg(false)),
   all_color_masses(H, V, Orig, FGPoints, FGOpts, FGObjs),
   append(FGObjs, BlackObjs, More))), !.

indv_color_masses(H, V, Orig, PointsRest, SegOptions, More):- SegOptions = i_opts(A, B, C, D, incl_bg(true)), !,
 must_det_ll((
   my_partition(is_fg_point, PointsRest, FGPoints, BGPoints),
   BGOpts = i_opts(A, B, colors(black), dirs(nsew), incl_bg(true)),
   %subst001(BGPoints, 'black', 'wfg', BGPointsFG), print_grid(H, V, wfg, BGPointsFG),
   all_color_masses(H, V, Orig, BGPoints, BGOpts, BlackObjs),
   FGOpts = i_opts(A, B, C, D, incl_bg(false)),
   all_color_masses(H, V, Orig, FGPoints, FGOpts, FGObjs),
   append(FGObjs, BlackObjs, More))), !.

indv_color_masses(H, V, Orig, PointsRest, SegOptions, FGObjs):- SegOptions = i_opts(A, B, C, D, incl_bg(false)), !,
   must_det_ll((
    my_partition(is_fg_point, PointsRest, FGPoints, _),
    FGOpts = i_opts(A, B, C, D, incl_bg(false)),
    all_color_masses(H, V, Orig, FGPoints, FGOpts, FGObjs))), !.

indv_color_masses(_H, _V, _Orig, PointsRest, SegOptions, More):-
  sub_var(dirs(points), SegOptions), !, rest_are_points(PointsRest, More), !.

indv_color_masses(H, V, Orig, PointsRest, SegOptions, More):-
  all_color_masses(H, V, Orig, PointsRest, SegOptions, More).

all_color_masses(H, V, Orig, FGPoints, RestOpts, Objs):-
 must_det_ll((
  color_masses(H, V, Orig, FGPoints, RestOpts, Segs, LO), !,
  rest_are_points(LO, PointSegs),
  append(Segs, PointSegs, Objs))).


color_masses(_H, _V, _Orig, Points, _SegOptions, [], []):- Points==[], !.
color_masses(H, V, Orig, Points, SegOptions, [GPoints|More], LO):-
  select(C1-HV1, Points, Points2),
  is_adjacent_point(HV1, Dir1, HV2), Dir1\==c,
  select(C2-HV2, Points2, PointsRest), colors_joinable(SegOptions, C1, C2),
  dir_ok(Dir1, SegOptions), color_dir_kk(Dir1, C1), % PointsFrom, ScanPoints, RestScanPoints, IndvPoints
  all_points_near(Orig, SegOptions, [C1-HV1, C2-HV2], PointsRest, LeftOver, GPoints), !,
  color_masses(H, V, Orig, LeftOver, SegOptions, More, LO), !.
color_masses(_H, _V, _Orig, PointsRest, _SegOptions, [], PointsRest).
/*
color_masses(H, V, Orig, [C1-HV1|PointsRest], SegOptions, [GPoints|More]):-
  all_points_near(Orig, SegOptions, [C1-HV1], PointsRest, LeftOver, GPoints), !,
  color_masses(H, V, Orig, LeftOver, SegOptions, More), !.
*/



%indv_options(in_out(I, O)):- indv_options(I, O).

indv_options(OptsIn, OptsIn):- indv_options(OptsIn).
indv_options(OptsIn, OptsOut):- indv_options(OptsIn), indv_options(OptsOut), OptsIn\=@=OptsOut.
seg_options_eio(Equation, OptsIn, OptsOut):-indv_options(OptsIn, OptsOut), arg(1, OptsIn, Equation).
ensure_objscount_equation(equals).
ensure_objscount_equation(input_times_n(_)).
ensure_objscount_equation(input_div_n(_)).
ensure_objscount_equation(input_minus_n(_)).
ensure_objscount_equation(input_plus_n(_)).
ensure_objscount_equation(becomes(_)).
ensure_objscount_equation(false ).
objscount_equation(equals, ObjsCountIn, ObjsCountOut):- ObjsCountIn=ObjsCountOut, ObjsCountIn>=2.
objscount_equation(input_times_n(N), ObjsCountIn, ObjsCountOut):- ObjsCountIn * N #= ObjsCountOut, N #>=2.
objscount_equation(input_div_n(N), ObjsCountIn, ObjsCountOut):- ObjsCountIn #= N * ObjsCountOut, N #>=2.
objscount_equation(input_minus_n(N), ObjsCountIn, ObjsCountOut):- ObjsCountIn #= N + ObjsCountOut, N #>=2.
objscount_equation(input_plus_n(N), ObjsCountIn, ObjsCountOut):- ObjsCountIn + N #= ObjsCountOut, N #>=2.
objscount_equation(becomes(2), _ObjsCountIn, 2).
objscount_equation(false, _, _).


objects_of_g(Grid, SegOptions, ObjsCount, IndvS):-
  var(SegOptions), !, indv_options(SegOptions),
  objects_of_g(Grid, SegOptions, ObjsCount, IndvS).

objects_of_g(Grid, SegOptions, ObjsCount, IndvS):-
  at_least_once(segs_of(Grid, SegOptions, ObjsCount, Segs)),
  must_det_ll(segs_to_objects(Grid, SegOptions, Segs, IndvS)).

segs_to_objects(Grid, SegOptions, ObjsCount, IndvS):-
  var(SegOptions), !, indv_options(SegOptions),
  segs_to_objects(Grid, SegOptions, ObjsCount, IndvS).
segs_to_objects(Grid, SegOptions, Segs, IndvS):-
 must_det_ll((
  into_fti(_, [SegOptions, do_ending], Grid, VM),
  gset(VM.lo_points)=[],
  gset(VM.lo_program)=[],
  gset(VM.start_options)=[],
  gset(VM.expanded_start_options)=[],
  Props = [iz(birth(SegOptions))],
  maplist(create_indiv_object(VM,Props), Segs, _),  
  post_individuate_8(VM, IndvS))).

create_indiv_object(VM, Props, Segs, Obj):-
  make_indiv_object_real(VM, Props, Segs, Obj).


textured_points_of(Grid, SegOptions, ObjsCount, Points):-
  indv_options(SegOptions),
  segs_of(Grid, SegOptions, ObjsCount, Segs),
  segs_into_points(Segs, Points).

segs_into_points(Segs, Points):-
   segs_into_n_points(1, Segs, Points).

segs_into_n_points(N, [S|Segs], PPoints):-
   segs_into_plist(N, S, P), M is N+1,
   segs_into_n_points(M, Segs, Points),
   append(P, Points, PPoints).
segs_into_n_points(_, [], []).

segs_into_plist(N, S, P):- M is N+300, int2glyph(M, G), maplist(segs_into_p(G), S, P).
segs_into_p(N, C-P, N-C-P):-!. segs_into_p(N, P, N-P).

segs_of(Grid, SegOptions, ObjsCount, IndvS):-
  var(SegOptions), !, indv_options(SegOptions),
  segs_of(Grid, SegOptions, ObjsCount, IndvS).

segs_of(In, SegOptions, ObjsCount, Objs):-
 indv_options(SegOptions),
 (var(In)->into_grid(_, In);true),
  vis2D(In, H, V),
  once(globalpoints(In, GPoints)),
  filter_points(SegOptions, GPoints, Points),
  once(indv_color_masses(H, V, GPoints, Points, SegOptions, Objs)),
  length(Objs, ObjsCount), ObjsCount=<43.

all_points_near(_Orig, _SegOptions, RestSet, [],       [],        RestSet):-!.
all_points_near( Orig, SegOptions, CurrentIndv, ScanPoints, RestScanPoints, RestSet):-
   points_near(SegOptions, CurrentIndv, ScanPoints, Found, LOScanPoints),
   (Found == [] -> (RestSet = CurrentIndv, RestScanPoints = ScanPoints)
    ; (append(CurrentIndv, Found, NewIndv),
        all_points_near(Orig, SegOptions, NewIndv, LOScanPoints, RestScanPoints, RestSet))), !.

points_near(SegOptions, Current, [E|ScanPoints], [E|Nears], RestScanPoints):-
  nearby_one_point(SegOptions, Current, E), !,
  points_near(SegOptions, [E|Current], ScanPoints, Nears, RestScanPoints).
points_near(SegOptions, From, [E|ScanPoints], Nears, [E|RestScanPoints]):- !,
    points_near(SegOptions, From, ScanPoints, Nears, RestScanPoints).
points_near(_SegOptions, _From, [], [], []):-!.

nearby_one_point(SegOptions, List, C1-E1):- is_adjacent_point(E1, Dir, E2), Dir\==c,
  dir_ok(Dir, SegOptions), color_dir_kk(Dir, C1),
  member(C2-E2, List), colors_joinable(SegOptions, C1, C2).

print_best_individuals(TestID):-
 ensure_test(TestID),
  dash_chars,
  NRVarI= io(InPSS, OutPSS),
  no_repeats_var(NRVar),
  forall(kaggle_arc(TestID, ExampleNum, _, _),
    forall(best_obj_group_pair(TestID, ExampleNum, HowIO, InC, OutC),
       (   objs_to_spoints(InC, InPSS), length(InC, CI),
           objs_to_spoints(OutC, OutPSS), length(OutC, CO),
           (NRVar=NRVarI->
           (dash_chars, print_ss(wqs(TestID >ExampleNum*ci_co(CI, CO)), InC, OutC), nl,
            write('       '), wqs(HowIO));
            (write('\n% DUP       '), wqs(HowIO)))))).

/*
print_simple_individuals(TestID):-
 ensure_test(TestID),
  dash_chars,
  NRVarI= io(InPSS, OutPSS),
  no_repeats_var(NRVar),
  forall(kaggle_arc(TestID, ExampleNum, In, Out),

    forall(indv_options(Opt),
      ignore(
      (objects_of(In, Opt, CI, InC),
       objects_of(Out, Opt, CO, OutC),
       objs_to_spoints(InC, InPSS),
       objs_to_spoints(OutC, OutPSS),
       (NRVar=NRVarI->
       (dash_chars, print_ss(wqs(TestID >ExampleNum, ci_co(CI, CO)), InC, OutC), nl,
        write('       '), wqs(Opt));
        (write('\n% DUP       '), wqs(Opt))))))).*/
simple_over_best.

/*
grid_indv_versions(TestID, ExampleNum, Dir, LHOInS):-
  (arc_cache:trans_rule_db(TestID, ExampleNum, grid_indv_versions(Dir), LHOInS)*-> true;
   (grid_indv_versions1(TestID, ExampleNum, Dir, LHOInS),
    assert_test_property(TestID, ExampleNum, grid_indv_versions(Dir), LHOInS))).
*/
:- abolish(cached_arc_test_property/4).
:- dynamic(cached_arc_test_property/4).

grid_indv_versions(TestID, ExampleNum, Dir, LHOInS):- var(ExampleNum), !,
  current_example_scope(TestID, ExampleNum),
  grid_indv_versions(TestID, ExampleNum, Dir, LHOInS).
:- abolish(arc_test_property/4).
:- dynamic(arc_test_property/4).
grid_indv_versions(TestID, ExampleNum, Dir, LHOInS):-
 cached_arc_test_property(TestID, ExampleNum, grid_indv_versions(Dir), LHOInS), LHOInS\==[], !.
grid_indv_versions(TestID, ExampleNum, Dir, LHOInS):-
 arc_cache:trans_rule_db(TestID, ExampleNum, grid_indv_versions(Dir), LHOInS), LHOInS\==[], !.
grid_indv_versions(TestID, ExampleNum, Dir, LHOInS):-
 grid_indv_versions1(TestID, ExampleNum, Dir, LHOInS), LHOInS\==[], !,
 assert(cached_arc_test_property(TestID, ExampleNum, grid_indv_versions(Dir), LHOInS)).


grid_indv_versions1(TestID, ExampleNum, Dir, LHOInS):- var(ExampleNum), !,
  current_example_scope(TestID, ExampleNum),
  grid_indv_versions1(TestID, ExampleNum, Dir, LHOInS).

grid_indv_versions1(TestID, ExampleNum, Dir, LHOInS):-
  current_example_scope(TestID, ExampleNum),
  must_det_ll((
     findall(lho(CI, InPSS, HowIn, InC),
          ((kaggle_arc(TestID, ExampleNum, _, _),
            grid_indv_version_lho(TestID, ExampleNum, Dir, CI, InPSS, HowIn, InC))),
     LHOIn),
     LHOIn\==[],
     =(LHOIn, LHOInS))).


grid_indv_version_lho(TestID, ExampleNum, Dir, CI, InPSS, HowIn, InC):-
     kaggle_arc_io(TestID, ExampleNum, Dir, In0),
     duplicate_term(In0, In),
     at_least_once(indv_options(HowIn)),
     %HowIn=simple(Opt),
     must_det_ll((
       objects_of_g(In, HowIn, CI, InC),
       InC\==[],
       %include(is_fg_object, InC, FGObjs), length(FGObjs, CI),
       objs_to_spoints(InC, InPSS))).
grid_indv_version_lho(TestID, ExampleNum, Dir, CI, InPSS, HowIn, InC):-
     kaggle_arc_io(TestID, ExampleNum, Dir, In0),
     duplicate_term(In0, In),
     at_least_once(get_each_ndividuator_extended(Dir, HowIn)),
     must_det_ll((
       individuate_3(HowIn, In, InC),
       %include(is_fg_object, InC, FGObjs),
       length(InC, CI),
       objs_to_spoints(InC, InPSS))).
grid_indv_version_lho(TestID, ExampleNum, Dir, CI, InPSS, HowIn, InC):-
     kaggle_arc_io(TestID, ExampleNum, Dir, _),
     /*at_least_once*/((
       obj_group_io_5(TestID, ExampleNum, Dir, HowIn, InC),
       %include(is_fg_object, InC, FGObjs),
       length(InC, CI),
       objs_to_spoints(InC, InPSS))).       
      

get_each_ndividuator_extended(_, complete).
%get_each_ndividuator_extended(_, i_pbox).
%get_each_ndividuator_extended(_, simple(Opt)):-indv_options(Opt).
%get_each_ndividuator_extended(Dir, HowIO):- get_each_ndividuator(Dir, HowIO).
/*
grid_indv_versions(TestID, ExampleNum, Dir, LHOInS):-
  findall(lho(CI, InPSS, HowIn, InC),
    (get_each_ndividuator(Dir, HowIn),
    obj_group_io_5(TestID, ExampleNum, Dir, HowIn, InC),
    length(InC, CI), CI=<50, objs_to_spoints(InC, InPSS)), LHOIn),
   sort(LHOIn, LHOInS), !.
best_obj_group_pair(TestID, ExampleNum, HowIO, InC, OutC):-
  OGP = best_ogp(TestID, ExampleNum, HowIO, InC, OutC),
  (arc_cache:trans_rule_db(TestID, ExampleNum, best_obj_group_pairs, OGPL)*-> true ;
   (time(findall(OGP, best_obj_group_pair1(TestID, ExampleNum, HowIO, InC, OutC), OGPL)),
     assert_test_property(TestID, ExampleNum, best_obj_group_pairs, OGPL))),

  member(OGP, OGPL).
*/
/*
calc_best_individuals(TestID):-
 current_example_scope(TestID, ExampleNum),
 forall(kaggle_arc(TestID, ExampleNum, _, _),
  (grid_indv_versions(TestID, ExampleNum, in, _LHOInS),
   grid_indv_versions(TestID, ExampleNum, out, _LHOOutS))),
 guess_io_opts(TestID, in, InOpts),
 guess_io_opts(TestID, out, OutOpts),
 print(pre_best=in_out(InOpts, OutOpts)), !.

guess_io_opts(TestID, IO, HowIn):-
 current_example_scope(TestID, ExampleNum),
 findall(hi(HowIn, s(Sizes-S)),
  (kaggle_arc(TestID, ExampleNum, _, _),
   grid_indv_versions(TestID, ExampleNum, IO, LHOInS),
   member(lho(S, InPSS, HowIn, _), LHOInS),

   maplist(length, InPSS, Sizes),
   Sizes \= [_, 1|_], Sizes\= [c_] ),
  SizeList),
 maplist(count_peers(SizeList), SizeList, SLPeers),
 reverse_sort(SLPeers, Sorted),
 pp_ilp(s(IO)=Sorted),
 Sorted=[hi(_, HowIn, s(_))|_].

reverse_sort(InputList, OutputList) :-
    sort(InputList, Sorted),
    reverse(Sorted, OutputList).

count_peers(CP, hi(HowIn, s(Sizes)), hi(N, HowIn, s(Sizes))):- findall_count(_, sub_var(s(Sizes), CP), N).
*/

best_obj_group_pair(TestID, ExampleNum, HowIO, InC, OutC):-
  NRVarI= io(InPSS, OutPSS),
  %indv_options(HowIn, HowOut),
  current_example_scope(TestID, ExampleNum),
  kaggle_arc(TestID, ExampleNum, _, _),
 wdmsg(best_obj_group_pair=howIO(HowIO)),
 with_individuated_cache(true,
  must_det_ll(((
  grid_indv_versions(TestID, ExampleNum, in, LHOInS), (var(HowIO)->LHOInS\==[];true),
  grid_indv_versions(TestID, ExampleNum, out, LHOOutS), (var(HowIO)->LHOOutS\==[];true),
  no_repeats_var(NRVar))))), !,


  ((member(lho(A, InPSS, HowIn, InC), LHOInS), member(lho(S, OutPSS, HowOut, OutC), LHOOutS), A=S, S>1, S=<43);
   (member(lho(A, InPSS, HowIn, InC), LHOInS), member(lho(S, OutPSS, HowOut, OutC), LHOOutS), A<S, A>1, S=<43);
   (member(lho(A, InPSS, HowIn, InC), LHOInS), member(lho(S, OutPSS, HowOut, OutC), LHOOutS), A<S, A>1, S=<43);
   (member(lho(A, InPSS, HowIn, InC), LHOInS), member(lho(S, OutPSS, HowOut, OutC), LHOOutS), A<S, A=1, S=<43);
   (member(lho(A, InPSS, HowIn, InC), LHOInS), member(lho(S, OutPSS, HowOut, OutC), LHOOutS), A=S, S=1);
   (member(lho(A, InPSS, HowIn, InC), LHOInS), member(lho(S, OutPSS, HowOut, OutC), LHOOutS), A>S, S=1, A=<43);
   fail),
  (Diff is abs(A-S), Diff<30),
  (NRVar=NRVarI-> nop(pp(nrVarI=NRVarI)); (wdmsg(duplicated(HowIO))), fail),
  HowIO = in_out(HowIn, HowOut).

% =============================================================
learn_object_dependancy(TestID, HowIO, RulesOut):-
% =============================================================
 ensure_test(TestID),
  at_least_once((
  %ensure_individuals(TestID),
  %scope_training(ExampleNum),
  possible_program(TestID, ActionGroupFinal, HowIO, RulesOut),
  %forall(kaggle_arc(TestID, ExampleNum, _, _),
  %   learn_object_dependancy(TestID, ExampleNum)),
  pp_ilp(possible_program(TestID, ActionGroupFinal, HowIO, RulesOut)))), !.

possible_program(TestID, ActionGroupFinal, HowIO, RulesOut):-
 must_det_ll((
 starter_narratives(TestID, Starter),

 (((fail,
   %(var(HowIn)->get_each_ndividuator(in, HowIn);true),
    ((synth_program_from_one_example(TestID, trn+0, HowIO, Starter, FirstNarrative0, Rules0), Rules0\==[])),

    synth_program_from_one_example(TestID, trn+1, HowIO, Starter, FirstNarrative1, Rules1),
    Rules1\==[],
    some_min_unifier_allow_nil([FirstNarrative0, FirstNarrative1], FirstNarrative),
    nonvar(FirstNarrative)
 ))*->true;Starter = FirstNarrative),

 ignore((ExampleNum=(trn+_))),
 %Starter = FirstNarrative,
 GNR = group_narrative_rules(ExampleNum,HowIO, ActionGroupOut, ExampleRules),
 findall(GNR,
    (kaggle_arc(TestID, ExampleNum, _, _),
      synth_program_from_one_example(TestID, ExampleNum, HowIO, FirstNarrative, ActionGroupOut, ExampleRules)), HNRL),
 pp_ilp(hNRL=HNRL),
  maplist(arg(1), HNRL, Examples),
  maplist(arg(2), HNRL, HowL), some_min_unifier_allow_nil(HowL, HowIO),
  maplist(arg(3), HNRL, AGL), some_min_unifier_allow_nil(AGL, ActionGroupFinal),
  maplist(arg(4), HNRL, RulesL),
 pp_ilp(rulesF=RulesL),
 combine_trans_rules(TestID, Examples, RulesL, RulesOut))).

some_min_unifier_allow_nil(MUL,Out):-
  include(\==([]),MUL,MULL),
  some_min_unifier(MULL,Out).

combine_trans_rules(TestID, Examples, RulesOutL, RulesOut):-
  maplist(merge_rules(TestID),Examples,RulesOutL,RulesOutLL),
  flatten(RulesOutLL, RulesOutLLF),
  merge_rules(TestID,common,RulesOutLLF, RulesOut).

% for troubshooter term expansion
%:- listing(possible_program).

synth_program_from_one_example(TestID, ExampleNum, HowIO, ActionGroup, ActionGroupOut, RulesL):-
  current_example_scope(TestID, ExampleNum),
%  HowIO = in_out(HowIn, _HowOut),
%  _GNR = group_narrative_rules(HowIO, ActionGroupOut, RulesL),
 %(var(HowIn)->get_each_ndividuator(in, HowIn);true),
 once((
  best_obj_group_pair(TestID, ExampleNum, HowIO, LHSObjs, RHSObjs), RHSObjs\==[], LHSObjs\==[],
  starter_narratives(TestID, ActionGroup),
  InfoStart=[how(HowIO)], 
  pp(how=HowIO),
  get_object_dependancy(InfoStart, TestID, ExampleNum, ActionGroup, ActionGroupOut, RHSObjs, LHSObjs, Groups),
  groups_to_rules(TestID, Groups, RulesL),
  pp_ilp([narrative_out=ActionGroupOut, rulesL=RulesL, narrative_out=ActionGroupOut]))),
  nop((merge_rules(TestID,ExampleNum,ex,TestID, RulesL, Rules), pp_ilp(synth_program_from_one_example=Rules))), !.

groups_to_rules(TestID, Groups, RulesL):-
 flatten([Groups], GroupsF),
 findall(Rule, expand_rules(TestID, GroupsF, Rule), RulesRR),
 flatten([RulesRR], RulesRF),
 filter_rules(RulesRF, RulesL).

filter_rules(RulesR, RulesL):- my_include(is_functor(ac_unit), RulesR, RulesL).

expand_rules(TestID, R, Rule):- is_list(R), !, member(E, R), expand_rules(TestID, E, Rule).
expand_rules(_TestID, exists(_), []):- !.
expand_rules(_TestID, call(G), []):- !, call(G), !.
expand_rules(TestID, group_narrative_rules(_, _, R), Out):-!, expand_rules(TestID, R, Out).
expand_rules(TestID, l2r(Info, In, Out), Rule):- !, trans_rule(Info, In, Out, Rules), expand_rules(TestID, Rules, Rule).
expand_rules(TestID, rule(Info, LHS, P), ac_unit(TestID, Ctx, P, Body)):- !, flatten([LHS, iz(info(Info))], Body), ignore(sub_cmpd(ctx(Ctx), Info)).
expand_rules(TestID, ac_unit(Ctx, P, PSame), ac_unit(TestID, Ctx, P, PSame)):-!.
expand_rules(TestID, ac_unit(P, PSame), ac_unit(TestID, _Ctx, P, PSame)):-!.
expand_rules(TestID, ac_unit(_, IO, P, PSame), ac_unit(TestID, IO, P, PSame)):-!.
expand_rules(TestID, AC_RULES_UNIT, ac_unit(TestID, IO, P, PSame)):- ac_unit_visitor(AC_RULES_UNIT, IO, P, PSame), !.
expand_rules(TestID, R, ac_unit(TestID, Ctx, P0, LHS)):-
  must_det_ll((
  find_rhs(R, P),
  find_lhs(R, Conds0), listify(Conds0, Conds),
  subst001(R, P, p, RR), subst001(RR, Conds, conds, RRR),
  append(Conds, [iz(info(RRR))], LHS),
  ignore((sub_cmpd(ctx(Ctx), R))))), !, P0=P.

expand_rules(TestID, R, ac_unit(TestID, ctx, er(R), [el(R)])).
%expand_rules(TestID, R, R).


fix_dupes(LHS,LHSO):- 
 select(A,LHS,  LHS00),
 select(B,LHS00,LHS01),
 sub_cmpd(iv(AO),A), sub_cmpd(iv(BO),B), AO=BO,
 globalpoints(A,APs), globalpoints(B,BPs),APs=@=BPs,
 !,
 show_how_dif(A,B,O),
 fix_dupes([O|LHS01],LHSO).
fix_dupes(LHS,LHS).


show_how_dif([LHS,LHSO|Objs]):-!,
  show_how_dif(LHS,LHSO,_),
  show_how_dif([LHSO|Objs]),!.
show_how_dif(_).
show_how_dif(A,B,obj(New)):- 
  sub_cmpd(iv(AO),A),
  sub_cmpd(iv(BO),B),!,
  AO==BO,
  globalpoints(A,APs),
  globalpoints(B,BPs),!,
  BPs=@=APs,
  indv_props_list(A,AL),
  indv_props_list(B,BL),
  intersection(AL,BL,S,L,R),
  %pp_ilp(sames=S),
  append_LR([L,R,S],New),
  if_t(L\==[],pp_ilp(removing=L)),
  if_t(R\==[],(pp_ilp(adding=R),trace)).


		
get_object_dependancy(InfoStart, TestID, ExampleNum, ActionGroupIn, ActionGroupOut, RHSObjs, LHSObjs, Groups):-
 ((RHSObjs\==[], LHSObjs\==[],
      VM = _{objs:LHSObjs, robjs:RHSObjs, rules:[], solution:[], testid:TestID},
      relaxed_levels(RelaxLvl),
      wots(RelaxLvlS, write([relax(RelaxLvl)|InfoStart])),
      print_ss(get_object_dependancy(RelaxLvlS), LHSObjs, RHSObjs),
      ((once((normalize_objects_for_dependancy(RelaxLvl, TestID, ExampleNum, RHSObjs, LHSObjs, RHSObjsOrdered, LHSObjsOrdered),
      InfoIn = [step(0), relax(RelaxLvl), ctx(in_out), example(ExampleNum), testid(TestID)|InfoStart],

      calc_o_d_recursive(VM, ActionGroupIn, InfoIn, [], LHSObjsOrdered, RHSObjsOrdered, ActionGroupOut, Groups))))))), !.


needs_arrange(IndvS,IndvR):- once((largest_first(mass,IndvS,Indv),reverse(Indv,IndvR))),IndvR\==[].
needs_arrange(IndvS,IndvR):- once((fix_dupes(IndvS,IndvR))),IndvR\==[].


calc_o_d_recursive(_VM, [],             _Info, PrevRules,    _,     _,            [], PrevRules):-!.
calc_o_d_recursive(VM, ActionGroupIn,    Info, PrevRules, LHSObjs, RHSObjs, ActionGroupFinal, Groups):-
    needs_arrange(LHSObjs,LHS), LHSObjs\=@=LHS,!, calc_o_d_recursive(VM, ActionGroupIn,    Info, PrevRules, LHS, RHSObjs, ActionGroupFinal, Groups).
calc_o_d_recursive(VM, ActionGroupIn,    Info, PrevRules, LHSObjs, RHSObjs, ActionGroupFinal, Groups):-
    needs_arrange(RHSObjs,RHS), RHSObjs\=@=RHS,!, calc_o_d_recursive(VM, ActionGroupIn,    Info, PrevRules, LHSObjs, RHS, ActionGroupFinal, Groups).

calc_o_d_recursive(VM, ActionGroupIn,    Info, PrevRules, LHSObjs, RHSObjs, ActionGroupFinal, Groups):-
  

 \+ \+ ignore(((
  (LHSObjs\==[];RHSObjs\==[]),
   length(PrevRules, PR), dash_chars,
   \+ \+ pp_ilp([Info, prevRules=count(PR), narrativeIn = ActionGroupIn]),
   length(LHSObjs, CI), length(RHSObjs, CO), 
   print_ss(wqs(step_ci_co(CI, CO)), LHSObjs, RHSObjs),
   if_t(CI<10,pp_ilp(in=LHSObjs)), 
   if_t(CO<10,pp_ilp(out=RHSObjs))))),
   
  %( LHSObjs==[] -> NLHSObjs=VM.objs ; NLHSObjs=LHSObjs ),
  NLHSObjs=LHSObjs,
  calc_o_d_recursively(VM, ActionGroupIn,   Info, PrevRules, NLHSObjs, RHSObjs,
                       ActionGroupOut, InfoMid, RulesOut, LHSOut, RHSOut),
  intersection(RulesOut, PrevRules, _, RulesNew, _),
  my_partition(is_functor(exists), RulesNew, _, RulesNewEww),
  reverse(RulesNewEww, RulesNewer),
  expand_rules(VM.testid,RulesNewer,RulesNewerL),
  pp_ilp(new=RulesNewerL),!,
  (Info==InfoMid-> incr_step(InfoMid, InfoOut) ; InfoMid=InfoOut),
  calc_o_d_recursive(VM, ActionGroupOut, InfoOut, RulesOut, LHSOut, RHSOut, ActionGroupFinal, Groups), !.

%calc_o_d_recursive(_VM, [Skip], _Info, PrevRules, _LHSObjs, _RHSObjs, [Skip], PrevRules):-
%  balanced_or_delete_leftovers(_, _)=Skip, !.

calc_o_d_recursive(VM, [Skip|ActionGroupIn], Info, PrevRules, LHSObjs, RHSObjs, AGF, Groups):-
 dash_chars, 
 wdmsg(no_more(Skip)), 
 Skip=..[_|ArgsIn], append(_, [Min..Max, Nth], ArgsIn), !, 
 ((Nth>=Min, Nth=<Max)-> true ; (show_last_chance(no_more(Skip),LHSObjs,RHSObjs),!,fail)),

 dash_chars,
   Skip=..[_|Args], last(Args, Was),
    (Was = 0 -> AGF = ActionGroupFinal ; AGF = [Skip|ActionGroupFinal]),
     calc_o_d_recursive(VM, ActionGroupIn,  Info, PrevRules, LHSObjs, RHSObjs, ActionGroupFinal, Groups), !.

calc_o_d_recursive(_VM, ActionGroupOut, _Info, PrevRules,   [],     [], ActionGroupOut, PrevRules).



:- discontiguous calc_o_d_recursively/11.


% Endings RHS==[]
calc_o_d_recursively(_VM, ActionGroupIn, Info, PrevRules, LHSObjs, RHSObjs,
                ActionGroupOut, InfoOut, RulesOut, [], []):- 
  RHSObjs==[],!,
  End = balanced_or_delete_leftovers(Ending, _),
  ActionGroupIn=[End|ActionGroup], !,
  ActionGroupOut= ActionGroup, !,

  ((calc_o_d_recursive_end(Ending, Info, PrevRules, LHSObjs, RHSObjs,
                InfoOut, RulesOut))), !,
  pp_ilp(calc_o_d_recursive_end=RulesOut), !.


% Endings
calc_o_d_recursively(_VM, ActionGroupIn, Info, PrevRules, LHSObjs, RHSObjs,
                ActionGroupOut, InfoOut, RulesOut, [], []):-  
  End = balanced_or_delete_leftovers(Ending, _),
  ActionGroupIn=[End|ActionGroup], !,
  ActionGroupOut= ActionGroup, !,

  ((calc_o_d_recursive_end(Ending, Info, PrevRules, LHSObjs, RHSObjs,
                InfoOut, RulesOut))), !,
  pp_ilp(calc_o_d_recursive_end=RulesOut), !.

show_last_chance(Why,LHSObjs,RHSObjs):- 
 dash_chars,dash_chars,dash_chars,dash_chars,
 wdmsg(show_last_chance(Why)),
 if_debugging((

 forall(member(Left, LHSObjs), 
     forall(member(Right, RHSObjs), print_ss(Why,Left,Right))),
 dash_chars,dash_chars,dash_chars,dash_chars,
  locally(nb_setval(without_comment,t),
   forall(member(Left, LHSObjs), 
     forall(member(Right, RHSObjs),
     ( dash_chars,dash_chars,
       wdmsg(last_chance(Why)),
       dash_chars,dash_chars,
       print_ss(Why,Left,Right),
      % pp_obj_tree(1,Why,Left,Right),
       show_cp_dff_rem_keep_add(Left,Right))))))).

if_debugging(G):- once(G),break.

calc_o_d_recursively(_VM, ActionGroupIn, InfoInOut, PrevRules, LHSObjs, RHSObjs,
                  ActionGroupOut, InfoInOut, RulesOut, LHSOut, RHSOut):-
((
  narrative_element(copy_object_perfect(Min..Max, Nth), ActionGroupIn, ActionGroupOut), !,

  (((select(Left, LHSObjs, LHSOut), select(Right, RHSObjs, RHSOut), 
    is_visible_object(Left),is_visible_object(Right),
    enum_prop_type_required(L),
    how_are_different(Left, Right, _TypeSet, PropSet), PropSet==[],
    how_are_same(Left, Right, TypeSetSame, _PropSetSame),
    intersection(TypeSetSame,L,_Satisfied,_,Missing),
    Missing==[],
    into_lhs(Left,LHS)))
  *-> true ; (fail,show_last_chance(copy_object_perfect(Min..Max, Nth),LHSObjs,RHSObjs),!,fail)),

  append_LR(PrevRules, [exists(left(Left)), rule(InfoInOut, LHS, copy_object_perfect(Min..Max, Nth))], RulesOut))).



calc_o_d_recursively(_VM, ActionGroupIn, Info, PrevRules, LHSObjs, RHSObjs,
                    ActionGroupOut, InfoOut, RulesOut, LHSOut, RHSOut):-
((
  narrative_element(copy_object_n_changes(1..1,N, Min..Max, Nth), ActionGroupIn, ActionGroupOut), !,
  %Nth<Max,
  select(Left, LHSObjs, LHSOut), select(Right, RHSObjs, RHSOut),

          % n_closest(Right,NObjs,L,LeftOverNeeds,RHSObjsX,Results),

  is_visible_object(Left),is_visible_object(Right),
  how_are_different(Left, Right, TypeOfDiffs, SetOfDiffs),
  how_are_same(Left, Right, TypeOfSames, SetOfSames),
  (var(N) -> length(TypeOfDiffs, N) ;  (number(N) -> (length(TypeOfDiffs, Len), Len = N) ; true)),!,
  dash_chars, nl,
  must_det_ll((
  %noteable_propdiffs(Left, Right, SameProps, LL, R0),
  maplist(arg(2),SetOfDiffs,SetOfDiffs2),maplist(arg(3),SetOfDiffs2,DiffProps),
  maplist(arg(2),SetOfSames,SetOfSames2),maplist(arg(3),SetOfSames2,SameProps),
  SameProps\==[], % have to have something in common
  print_ss(copy_object_n_changes(1..1,N, Min..Max, Nth), Left, Right),
  pp_ilp(3, [diffs=TypeOfDiffs, diffprops=SetOfDiffs]),
  pp_ilp(3, [sames=TypeOfSames, sameprops=SetOfSames]),
  into_lhs(Left,LHSOverSpec),
  maplist(find_relative_r(Info, Left, Right, PrevRules), DiffProps, NewRSet, PreConds, NewRules),
  append_LR([SameProps, PreConds,LHSOverSpec], NewRLHS),
  append_LR(NewRules, NewRulesF),
  subst_2L_sometimes(DiffProps, NewRSet, Left, NewLeft),
  
  RuleInfo = [diffs_sames(SetOfDiffs,SetOfSames)|Info],
  NewRRule1 = rule(RuleInfo, NewRLHS, copy_object_n_changes(1..1,N, Min..Max, TypeOfDiffs, NewRSet)),
 % (DiffProps\=@=NewRSet 
 %   -> NewRRule2 = [] % rule(RuleInfo, SameProps, copy_object_n_changes_else(N, Min..Max, TypeOfDiffs, DiffProps)) 
 %   ; NewRRule2=[]),
  NewRRule2=[],
  append_LR(PrevRules, [exists(left(NewLeft)), NewRulesF, NewRRule1, NewRRule2], RulesOut),
  InfoOut = Info,
  dash_chars, nl,
  true)))), !.


% Scenery
calc_o_d_recursively(_VM, ActionGroupIn, Info, PrevRules, LHSObjs, RHSObjs,
                ActionGroupOut, InfoOut, RulesOut, LHSObjs, RHSObjsRest):-
  narrative_element(add_dependant_scenery(NObjsL..NObjsH,MadeUpLenL..MadeupLenH,Min..Max, Nth), ActionGroupIn, ActionGroupOut),!,
   %LHSObjs==[],
    into_list(PrevRules, PrevObjs),
    my_partition(is_input_object, PrevObjs, PrevLeft, ExtraRHS),

    enum_prop_type_required(L),
    select(Right, RHSObjs, RHSObjsRest),
    
    is_visible_object(Right),    
    CodeMust = ((
      append_LR([ExtraRHS, PrevLeft, LHSObjs], RHSObjsX),
      %rtrace,
      must_det_ll((
        n_closest(Right,NObjs,L,LeftOverNeeds,RHSObjsX,Results),
        length(LeftOverNeeds,LONL),
        % into_lhs(Results,LHS),
        maplist(arg(1),Results,RHSide),
        maplist(get_type_prop(Right),LeftOverNeeds,FilledInNeeds2))),
      append_LR([RHSide,FilledInNeeds2],RuleNeeds),

      indv_link_props(Right,RuleRHS),
      %LONL=<MadeupLenH,
      Info = InfoOut,
      append_LR([iz(info(Info)),always(Results),RuleNeeds,Results],RuleLHS),
      append_LR([PrevRules,ac_unit(add_dependant_scenery(NObjsL..NObjsH,NObjs,MadeUpLenL..MadeupLenH,Min..Max,RuleRHS),
         RuleLHS)],RulesOut))),
    (Nth<Min->must_det_ll(CodeMust);must_det_ll(CodeMust)),
    nop((Nth>=Min, Nth=<Max)).

% Scenery
calc_o_d_recursively(_VM, ActionGroupIn, Info, PrevRules, LHSObjs, RHSObjs,
                  ActionGroupOut, InfoOut, RulesOut, LHSObjs, RHSOut):- fail,
  narrative_element(add_independant_scenery(_, _), ActionGroupIn, ActionGroupOut),
  select(Right,RHSObjs,RHSOut),
  enum_prop_type_required(LeftOverNeeds),
  maplist(get_type_prop(Right),LeftOverNeeds,Needs),
  indv_link_props(Right,RuleRHS),
  into_lhs(Right,LHS),
  incr_step(Info, InfoOut),
  append_LR([PrevRules,
      ac_unit(add_independant_scenery(Needs),RuleRHS),
      ac_unit(add_independant_scenery(RuleRHS),LHS)
      ],RulesOut).
  

enum_prop_type_required(L):-findall(R,prop_type_required(R),L).
get_type_prop(Right,Type,Prop):- type_prop(Type,Prop),sub_cmpd(Prop,Right).
n_closest(_Right,Z,LON,LON,_RHSObjsX,[]):- LON=[],!,ignore(Z is 0).
n_closest(_Right,0,LON,LON,_RHSObjsX,[]):-!.
n_closest(Right,NObjs,Needs,LON,RHSObjsX,[needs_are_met(SetOfSameP,LHS)|Results]):- 
 length(Needs,TotalNeedsCount),
 (var(NObjs)-> freeze(NObjs2,(NObjs is NObjs2+1)) ; (NObjs2 is NObjs -1)),
  between(0,TotalNeedsCount,AllowNeedRemaining),
  select(Left,RHSObjsX,RHSObjsXRest),
  how_are_same(Left, Right, TypeOfSames, SetOfSames),
  intersection(TypeOfSames,Needs,Overlap,_,NeedsRemaining),
  Overlap\==[], length(NeedsRemaining,NRCount), NRCount=<AllowNeedRemaining,
  n_closest(Right,NObjs2,NeedsRemaining,LON,RHSObjsXRest,Results),
  object_into_lhs(Left,LHS),
  maplist(arg(2),SetOfSames,SetOfSameL),maplist(arg(3),SetOfSameL,SetOfSameP).

n_closest(Right,NObjs,Needs,LON,RHSObjsX,[needs_are_close(SetOfCloseP,LHS)|Results]):- 
 length(Needs,TotalNeedsCount),
 (var(NObjs)-> freeze(NObjs2,(NObjs is NObjs2+1)) ; (NObjs2 is NObjs -1)),
  between(0,TotalNeedsCount,AllowNeedRemaining),
  select(Left,RHSObjsX,RHSObjsXRest),
  how_are_close(Left, Right,TypeOfCloseness, SetOfCloseness),
  intersection(TypeOfCloseness,Needs,Overlap,_,NeedsRemaining),
  Overlap\==[], length(NeedsRemaining,NRCount), NRCount=<AllowNeedRemaining,
  n_closest(Right,NObjs2,NeedsRemaining,LON,RHSObjsXRest,Results),
  object_into_lhs(Left,LHS),
  maplist(arg(2),SetOfCloseness,SetOfCloseP). %,maplist(arg(3),SetOfCloseL,SetOfCloseP).

/*
n_closest(Right,NObjs,[N|Needs],[N|LON],RHSObjsX,[needs_are_close(SetOfCloseP,LHS)|Results]):- 
 %length(Needs,TotalNeedsCount),
 (var(NObjs)-> freeze(NObjs2,(NObjs is NObjs2+1)) ; (NObjs2 is NObjs -1)),
  %between(0,TotalNeedsCount,AllowNeedRemaining),
  select(Left,RHSObjsX,RHSObjsXRest),
  how_are_close(Left, Right, [N], SetOfCloseness),
  %intersection(TypeOfCloseness,Needs,Overlap,_,NeedsRemaining),
  %Overlap\==[], length(NeedsRemaining,NRCount), NRCount=<AllowNeedRemaining,
  n_closest(Right,NObjs2,Needs,LON,RHSObjsXRest,Results),
  object_into_lhs(Left,LHS),
  maplist(arg(2),SetOfCloseness,SetOfCloseL),maplist(arg(3),SetOfCloseL,SetOfCloseP).*/
/*
n_closest(Right,NObjs,Needs,LON,RHSObjsX,[needs_are_close(SetOfCloseP,LHS)|Results]):- 
 (var(NObjs)-> freeze(NObjs2,(NObjs is NObjs2+1)) ; (NObjs2 is NObjs -1)),
  select(Left,RHSObjsX,RHSObjsXRest),
  how_are_close(Left, Right, _TypeOfCloseness, SetOfCloseness),
  %intersection(TypeOfCloseness,Needs,Overlap,_,NeedsRemaining),
  %NeedsRemaining\==Needs, Overlap\==[],
  n_closest(Right,NObjs2,Needs,LON,RHSObjsXRest,Results),
  into_lhs(Left,LHS),
  maplist(arg(2),SetOfCloseness,SetOfCloseL),maplist(arg(3),SetOfCloseL,SetOfCloseP).
*/
n_closest(Right,NObjs,[N|Needs],[N|LON],RHSObjsX,Out):-
  n_closest(Right,NObjs,Needs,LON,RHSObjsX,Out).

short_distance(Right1, Right2, 2):-
  globalpoints(Right1, GPoints1),
  globalpoints(Right2, GPoints2),
  member(_-P1, GPoints1),
  member(_-P2, GPoints2),
  is_adjacent_point(P1, Dir1, P12), Dir1\==c, is_adjacent_point(P12, Dir2, P2), Dir2\==c.


for_rhs(R0, R):-((include(good_for_rhs, R0, R), R\==[])->true;R0=R), !.

subst_2L_sometimes([], _, IO, IO):-!.
subst_2L_sometimes(_, [], IO, IO):-!.
subst_2L_sometimes([HF|TF], [HR|TR], In, Out):- !,
  subst_2L_sometimes(HF, TF, In, Mid),
  subst_2L_sometimes(HR, TR, Mid, Out).
subst_2L_sometimes(HF, TF, In, Mid):- subst0011(HF, TF, In, Mid).

verbatum_unifiable(Var):-var(Var), fail.
verbatum_unifiable(C):- sub_term(E, C), compound(E), E='$VAR'(_), !.
verbatum_unifiable(of_obj(_)).
verbatum_unifiable(of_obj(_, _)).
verbatum_unifiable(of_obj(_, _, _)).
verbatum_unifiable(of_obj(_, _, _, _)).
verbatum_unifiable(always(_)).

is_visible_object(Left):- is_object(Left),is_fg_object(Left),
  has_prop(iz(fg_or_bg(iz_fg)),Left), \+ has_prop(iz(flag(hidden)),Left).

%find_relative_r(Info, Left, Right, Possibles, loc2D(_, _), [], [], []):-!.

find_relative_sr(Same, Info, Left, Right, Possibles, R, NewRSet, PreCond, XtraRule):-
  ((make_unifiable_u(R, E), make_unifiable_u(E, GetR), make_unifiable_u(R, NewRSet),
  iz_arg(1, R, ValR), iz_arg(1, E, ValE), iz_arg(1, NewRSet, ValNewR), iz_arg(1, GetR, ValGetR),

    if_t((Same == same,number(ValR)),     (ValE#=ValR)),
    if_t((Same == diff,number(ValR)),     (ValE#\=ValR)),
    if_t((Same == same, \+ number(ValR)), (ValE=ValR)),
    if_t((Same == diff, \+ number(ValR)), (dif(ValE,ValR))),

  ((sub_term(P, Possibles),is_object(P));(fail,P=Left)), 
    P\==Right, indv_props_list(P, Props), member(E, Props),
    into_lhs(P, LHS1), subst001(LHS1, E, always(GetR), PLHS),
    %noteable_propdiffs(P,Left, _, PL, _),    
    noteable_propdiffs(P, Right, _, PR, _), subst001(PR, E, true, PRG),
    OF_OBJ=of_obj(NewRSet, Convertor /*, PRG*/),
    subst001(OF_OBJ, NewRSet, GetR, OF_OBJ_GET),
    append_LR([always(OF_OBJ_GET), PRG, iz(info(OF_OBJ_GET))], PreCond),    
    make_conversion(Same, ValE, ValR, ValGetR, ValNewR, Convertor),
    append_LR([iz(info(Info)), PRG, iz(info(OF_OBJ)),PLHS],XLHS),
    XtraRule = [ac_unit(_, OF_OBJ, XLHS)])),
   %flag('VAR_', VV, VV+0), 
     VV=27,
     numbervars(v(ValE, ValR, ValGetR, ValNewR, Convertor), VV, NEWVV, [attvar(bind)]), set_flag('VAR_', NEWVV), !.

find_relative_r(Info, Left, Right, Possibles, R, NewRSet, PreCond, XtraRule):- if_arc_expanded,
  find_relative_sr(same,Info, Left, Right, Possibles, R, NewRSet, PreCond, XtraRule),!.
find_relative_r(Info, Left, Right, Possibles, R, NewRSet, PreCond, XtraRule):- if_arc_expanded,
  find_relative_sr(diff,Info, Left, Right, Possibles, R, NewRSet, PreCond, XtraRule),!.
find_relative_r(_Info, _, _, _, R, R, [], []):-!.

if_arc_expanded:- fail.

/*

  make_unifiable_u(E, GetR),
  new_r(GetR, R, E, NewRSet, SubExpr),
  */
make_conversion(same,ValE, ValR, ValGetR, ValNewR, Convertor):- number(ValR), !,
  ignore(catch_log(ValR = ValE)),
  ignore(catch_log(ValNewR = ValGetR)),
  Convertor = true. % ( ValR#=ValE, ValNewR #= ValGetR).
  
make_conversion(diff,ValE, ValR, ValGetR, ValNewR, Convertor):- number(ValR), number(ValE),!,
  Dif is (ValR-ValE),
  Convertor = (Dif #= (ValR-ValE),ValNewR #= ValGetR + Dif).

make_conversion(diff,ValE, ValR, ValGetR, ValNewR, Convertor):- number(ValR), !,
  Convertor = (Dif #= (ValR-ValE),ValNewR #= ValGetR + Dif).

make_conversion(diff,ValE, ValR, ValGetR, ValNewR, Convertor):- 
  Convertor = relates_between(ValE = ValR, ValNewR = ValGetR).
make_conversion(diff,ValE, ValR, ValGetR, ValNewR, Convertor):-
  Convertor = (dif(ValE,ValR),dif(ValNewR,ValGetR)).

relates_between([X1],X2,Y1,Y2):- X1=X2,Y1=Y2,!.
relates_between(G1,G2):- call(G1),call(G2).

new_r(GetR, R, E, NewRSet, NewRSet=GetR):- E=@=R, NewRSet=GetR.
new_r(GetR, R, _E, NewRSet, Expr):- iz_arg(1, GetR, ValE), iz_arg(1, R, ValR), number(ValE), number(ValR), Dif is ValE - ValR,
  make_unifiable(GetR, NewRSet), iz_arg(1, NewRSet, ValNewR), Expr = (ValNewR #= ValE + Dif).

iz_arg(N, FA, A):- FA=..[_, T], compound(T), !, iz_arg(N, T, A).
iz_arg(N, T, A):- arg(N, T, A), !.


/*
out_object_splitter:-false.

calc_o_d_recursively(_VM, ActionGroupIn, Info, PrevRules, LHSObjs, RHSObjs,
                    ActionGroupIn, InfoIn, RulesOut, LHSOut, RHSOut):-
 out_object_splitter,
 Type = perfect,
 select_pair(Info, Type, PrevRules, RHSObjs, LHSObjs, Right, Left, RHSOut1, LHSOut1),
 \+ has_prop(iz(info(faked(_Ctx))), Right),
 ((
  remove_object(RHSOut1, Right, RHSOut2), remove_object(LHSOut1, Right, LHSOut2),
  remove_object(RHSOut2, Left, RHSOut ), remove_object(LHSOut2, Left, LHSOut ),
  make_pairs(InfoIn, Type, PrevRules, Left, Right, Rules),
  append_LR(PrevRules, Rules, RulesOut),
  incr_step(Info, InfoOut),


((  left_over_props(Left, Right, PropsMissing), PropsMissing=[_, _|_],
  pp_ilp(left_over_props=PropsMissing),
  obj_to_oid(Right, OID),
  obj_in_or_out(Right, IO),
  FakeObj = obj([was_oid(OID), iz(i_o(IO)), iz(info(faked(_Ctx2)))|PropsMissing])) ->
      calc_o_d_recursive(VM, ActionGroupIn, InfoIn, RulesOut, [Right, Left|LHSOut], [FakeObj|RHSOut], RulesOut);

      calc_o_d_recursive(VM, ActionGroupIn, InfoIn, RulesOut, [Right, Left|LHSOut], RHSOut, RulesOut)))).
calc_o_d_recursively(_VM, ActionGroupIn, InfoIn, PrevRules, LHSObjs, [Right], ActionGroupOut, InfoOut, RulesOut, LHSOut, RHSOut):- fail, LHSObjs=[_, _|_],
  sort_by_jaccard(Right, LHSObjs, [A, B|C]),
  make_pairs(InfoIn, assumed, [], [B, A], Right, Rules),
  append_LR(PrevRules, Rules, RulesOut),
  calc_o_d_recursive(VM, ActionGroupOut, ActionGroupOut, RelaxLvl, InfoOut, RulesOut, C, [], RulesOut), !.

calc_o_d_recursively(_VM, ActionGroupIn, InfoIn, PrevRules, LHSObjs, [Right], ActionGroupOut, InfoOut, RulesOut, LHSOut, RHSOut):- fail,
  LHSObjs == [],
  into_list(PrevRules, PrevObjs), PrevObjs\==[],
  my_partition(is_input_object, PrevObjs, PrevLHS, PrevRHS),
  once((PrevRHS = [A, B|C] ; PrevLHS = [A, B|C])),
  sort_by_jaccard(Right, [A, B|C], Stuff), !,
  reverse(Stuff, [AA, BB|_Out]),
  make_pairs(InfoIn, assumed, [], [BB, AA], Right, Rules),
  append_LR(PrevRules, Rules, RulesOut),
  calc_o_d_recursive(VM, ActionGroupOut, ActionGroupOut, RelaxLvl, InfoOut, RulesOut, [], [], RulesOut), !.


*/
left_over_props(L, R, LO):-
  noteable_propdiffs(L, R, _, _, LO).




starter_narratives(_, ActionGroup):- nonvar(ActionGroup), !.
starter_narratives(TestID, Starter):-  starter_narratives0(TestID, Starter0), 
  once(subst_1L([nth-0, lowmin-0, himax-inf], Starter0, Starter)).

starter_narratives0(TestID, ActionGroup):- starter_narrative(TestID, ActionGroup), !.
starter_narratives0(_, ActionGroup):- generic_starter_narratives(ActionGroup).

:- dynamic(starter_narrative/2).


starter_narrative(t('25d487eb'), [
   copy_object_perfect(2..2, nth), % copy two objects perfectly
   add_dependant_scenery(2..2, 2..4, 1..1, nth), % from two output objects, create one output object with up 2 to 4 made up props
   balanced_or_delete_leftovers(balanced(_), nth) ]). % there should not be any unprocessed input objects

starter_narrative(v(e41c6fd3), [
   copy_object_perfect(1..1, nth), % copy one object perfectly
   copy_object_n_changes(1..1, 1, 2..4, nth), % copy 2-4 objects that contain a single property change
   balanced_or_delete_leftovers(balanced(_), nth) ]):- % there should not be any unprocessed input objects
  true. % fail.

generic_starter_narratives(ActionGroup):-
 ActionGroup=
     [copy_object_perfect(lowmin..himax, nth),
      copy_object_n_changes(1..1,1, lowmin..himax, nth),
      copy_object_n_changes(1..1,2, lowmin..himax, nth),
      copy_object_n_changes(1..1,3, lowmin..himax, nth),
      copy_object_n_changes(1..1,4, lowmin..himax, nth),
      copy_object_n_changes(1..1,5, lowmin..himax, nth),
        add_dependant_scenery(1..3,1..5,lowmin..himax, nth),
      add_independant_scenery(lowmin..himax, nth),
      balanced_or_delete_leftovers(_, nth)].

use_adjacent_same_color(R1, R2, LHSOut, RHSObjs, RHSOut):- member(R1, LHSOut), select(R2, RHSObjs, RHSOut),
  is_adjacent_same_color(R1, R2, 0), !.
use_adjacent_same_color(R1, R2, LHSOut, RHSObjs, RHSOut):- member(R1, LHSOut), select(R2, RHSObjs, RHSOut),
  is_adjacent_same_color(R1, R2, 1), !.
use_adjacent_same_color(R1, R2, LHSOut, RHSObjs, RHSOut):- member(R1, LHSOut), select(R2, RHSObjs, RHSOut),
  is_adjacent_same_color(R1, R2, 2), !.

% Scenery
calc_o_d_recursively(_VM, ActionGroupIn, Info, PrevRules, LHSObjs, RHSObjs,
                  ActionGroupOut, InfoOut, RulesOut, LHSObjs, RHSOut):- fail,
  narrative_element(add_independant_scenery(_, _), ActionGroupIn, ActionGroupOut),
   %LHSObjs==[],
    into_list(PrevRules, PrevObjs),
    my_partition(is_input_object, PrevObjs, PrevLHS, PrevRHS),
    append_LR(PrevLHS, PrevRHS, LHSMid),
    use_adjacent_same_color(R1, R2, LHSMid, RHSObjs, RHSOut),
    incr_step(Info, InfoOut),
    make_pairs(Info, is_adjacent_same_color, PrevRules, R1, R2, RulesOut).






% ======================================
% recycles
% ======================================
calc_o_d_recursive_recycle(_VM, ActionGroupIn, Info, PrevRules, LHSObjs, RHSObjs,
                  ActionGroupOut, InfoOut, PrevRules, ['recycled'|PrevLHS], RHSObjs):-
    narrative_element(recycle(_, _), ActionGroupIn, ActionGroupOut),
   LHSObjs==[], !, ((
    into_list(PrevRules, PrevObjs),
    my_partition(is_input_object, PrevObjs, PrevLHS, _PrevRHS),
    %once((PrevRHS = [A, B|C] ; PrevLHS = [A, B|C])), %append_LR(PrevRHS, PrevLHS, LHSOut), %LHSOut=PrevLHS,
    incr_step(Info, InfoOut))).

calc_o_d_recursive_recycle(_VM, ActionGroupIn, Info, PrevRules, LHSObjsNil, RHSObjs,
                 ActionGroupOut, InfoOut, RulesOut, ['recycled'|LHSOut], RHSOut):-
  narrative_element(recycle(_, _), ActionGroupIn, ActionGroupOut),
   LHSObjsNil==[], !,
    incr_cntx(Info, PrevCurrentInfo1),
    incr_step(PrevCurrentInfo1, InfoOut), %incr_step(Info, InfoOut),
    into_list(PrevRules, PrevObjs),
    my_partition(is_input_object, PrevObjs, PrevLHS, PrevRHS),
    member(Type=LHSObjs, [perfect=PrevLHS, perfect_combo=PrevLHS, perfect_combo=PrevRHS]),
      select_pair(Type, PrevRules, RHSObjs, LHSObjs, Right, Left, RHSOut1, LHSOut1),
      must_det_ll((
      remove_object(RHSOut1, Right, RHSOut2), remove_object(LHSOut1, Right, LHSOut2),
      remove_object(RHSOut2, Left, RHSOut ), remove_object(LHSOut2, Left, LHSOut ),
      make_pairs(Info, recycle, PrevRules, Left, Right, RulesOut))).

calc_o_d_recursive_recycle(_VM, ActionGroupIn, Info, PrevRules, LHSObjs, RHSObjs,
                    ActionGroupOut, InfoOut, PrevRules, ['recycled'|LHSOut], RHSObjs):-
  narrative_element(recycle(_, _), ActionGroupIn, ActionGroupOut),
   LHSObjs==[], !, ((
    incr_cntx(Info, PrevCurrentInfo1),
    select(step(_), PrevCurrentInfo1, PrevCurrentInfo2), InfoOut=[step(30)|PrevCurrentInfo2],
    into_list(PrevRules, LHSOut))).



ensure_scene_change_rules(TestID):-
 ensure_test(TestID),
 (\+ ac_db_unit(TestID, _, _, _) -> compute_scene_change(TestID) ; true).

compute_scene_change(TestID):-
 ensure_test(TestID),
 with_pair_mode(whole_test,
 ((must_det_ll((
   learn_object_dependancy(TestID, HowIO, Rules),
   is_list(Rules),
   forall(member(R, Rules), 
     assert_ac_db(TestID, R)),
  pp_ilp(rulesList(HowIO)=Rules)))))), !.

% for troubshooting term expansion
%:- listing(compute_scene_change/1).
/*  Should look like this:

:- module_transparent compute_scene_change/1.

compute_scene_change(TestID) :-
    ensure_test(TestID),
    with_pair_mode(whole_test,
                   ( must_det_ll(learn_object_dependancy(TestID,
                                                         HowIO,
                                                         Rules)),
                     must_det_ll(is_list(Rules)),
                     must_det_ll(\+ (member(R, Rules), \+assert_ac_db(TestID, R))),
                     must_det_ll(pp_ilp(rulesList(HowIO)=Rules))
                   )),
    !.

*/



rhs_ground(G):- ground(G),!.
rhs_ground(G):- nop(writeln(G)),!.

ac_rules(List, Ctx, P, PSame):- is_list(List), !, member(Stuff, List), ctx_p_conds(Stuff, Ctx, P, PSame).
ac_rules(TestID,Ctx,P,PSame):- 
  ac_unit(TestID, Ctx, P, Same), \+ never_use_horn_rhs(P),
  include(not_debug_info,Same,PSame), 
  %Same=PSame,
  PSame\==[].

not_debug_info(P):- \+ is_debug_info(P),!.

remove_debug_info(List,NoDebug):- \+ compound(List),!,NoDebug=List.
remove_debug_info(List,NoDebug):- is_list(List), is_obj_props(List),!,include(not_debug_info,List,NoDebug).
remove_debug_info(List,NoDebug):- is_list(List), !, maplist(remove_debug_info,List,NoDebug).
remove_debug_info(List,NoDebug):- compound_name_arguments(List,F,AA),
  maplist(remove_debug_info,AA,CC),!, compound_name_arguments(NoDebug,F,CC).

ac_unit(TestID, Ctx, P, Same):- ac_listing(TestID, Ctx, P, Same).

ac_info_unit(TestID, Info, Ctx, P, NoDebug):- ac_listing(TestID, Ctx, P, List),
  my_partition(not_debug_info, List, NoDebug, Info).

ac_listing(List, Ctx, P, PSame):- is_list(List), !, member(Stuff, List), ctx_p_conds(Stuff, Ctx, P, PSame).
%ac_listing(TestID,Ctx,P->ac_db_unit,PSame):- ac_db_unit(TestID,Ctx,P,PSame).
ac_listing(TestID, Ctx, P, PSame):- (ac_db_unit(TestID, Ctx, P, PSame)*->true;pass2_rule(TestID, Ctx, P, PSame)), \+ never_use_horn_rhs(P).
%ac_listing(TestID,Ctx,P,[iz(info(prop_can))|PSame]):- prop_can(TestID,Ctx,P,PSame).
%ac_listing(TestID,Ctx,P,[pass2|PSame]):- pass2_rule(TestID,Ctx,P,PSame), \+ ac_rules(TestID,Ctx,P,PSame).
pass2_rule(List, Ctx, P, PSame):- is_list(List), !, member(Stuff, List), ctx_p_conds(Stuff, Ctx, P, PSame).

%ac_db_unit(List, Ctx, P, PSame):- is_list(List), !, member(Stuff, List), ctx_p_conds(Stuff, Ctx, P, PSame).

ctx_p_conds(Stuff, Ctx, P, PSame):- ctx_p_conds1(Stuff, CtxO, PO, PSameO), CtxO+PO+PSameO=Ctx+P+PSame.
ctx_p_conds1(Stuff, Ctx, P, PSame):- nonvar(Stuff), compound_name_arguments(Stuff, _, Args), append(_, [Ctx, P, PSame], Args), !.
ctx_p_conds1(Stuff, _, P, PSame):- nonvar(Stuff), compound_name_arguments(Stuff, _, Args), append(_, [P, PSame], Args), !.









% adds debugging to info/1
trans_rule(Info, In, Out, Rules):- sub_cmpd(info(InfoL), Info), !, trans_rule(InfoL, In, Out, Rules).
trans_rule(Info, In, Out, Clauses):- \+ is_list(In), !, trans_rule(Info, [In], Out, Clauses).
trans_rule(Info, In, Out, Clauses):- \+ is_list(Out), !, trans_rule(Info, In, [Out], Clauses).
trans_rule(Info, In, Out, Rules):-
  ( \+ sub_cmpd(oin(_), Info); \+ sub_cmpd(oout(_), Info)),
  into_oids(In, OIDIns), into_oids(Out, OIDOuts),
  append_sets(Info, [oin(OIDIns), oout(OIDOuts)], InfM), !,
  trans_rule(InfM, In, Out, Rules).

trans_rule(Info, [], [], [ac_unit(Ctx, happy_ending(Type), [e(1)|Info])]):-
   ignore((sub_cmpd(ctx(Ctx), Info))),
   ignore((sub_cmpd(type(Type), Info))).


% delete
trans_rule(Info, [In], [], [Unit]):-
 into_lhs(In, Preconds), into_rhs(Info, InfoR),
 Unit = ac_unit(delete_object(InfoR, In), [iz(info(Info))|Preconds]), !.


% create
trans_rule(Info, [], [Out], [Unit]):-
 into_lhs(Out, Preconds), into_rhs(Info, InfoR),
 Unit = ac_unit(create_object(InfoR, Out), [iz(info(Info))|Preconds]), !.

% mutiple postconds
trans_rule(Info,In,[Out,Out2|OutL],TransRule):- is_object(Out),is_object(Out2),
  maplist(trans_rule(Info,In),[Out,Out2|OutL],TransRule), TransRule\==[],!.

% 2 preconds
%trans_rule(Info,[In1,In2],[Out],TransRule):- is_object(In1),is_object(In2),
%  TransRule = create_object2(Info,rhs(create_obj(Out)),lhs(into_new(In1,In2))),!.

% 2 preconds
trans_rule(Info,[In1,In2],[Out],TransRule):- is_object(In1),is_object(In2), % fail,
   noteable_propdiffs(In1, Out,_Same1,_DontCare1,New1), 
   noteable_propdiffs(In2,New1,_Same2,_DontCare2,New2),
   %remove_o_giz(Out,RHSO), 
   remove_o_giz(In1,Precond1), remove_o_giz(In2,Precond2),
   %sub_comInfoOut =  info(Step, _IsSwapped, _Ctx, TypeO, _, _, _),
   sub_cmpd(step(Step),Info), sub_cmpd(why(Type),Info),
   Type \== assumed_in_in_out,
 % append_sets(Same1,Same2,Same), append_sets(DontCare1,DontCare2,DC), append_sets(New1,New2,New),
 % append_sets(Same,New,NewObj),
  %make_common(RHSO,LHS1,NewOut1,NewLHS1),
  %make_common(NewOut1,LHS2,NewOut,NewLHS2),
  TransRule = [create_object1(Info,rhs(creation_step1(Step,Type,New1)), lhs(Precond1)),
               create_object2(Info,rhs(creation_step2(Step,Type,New2)), lhs(Precond2))],!.

% mutiple preconds
trans_rule(Info,[In,In2|InL],OutL,TransRule):- is_object(In),is_object(In2),
  trans_rule(Info,[In2|InL],OutL,TransRuleM), TransRuleM\==[],
  sub_cmpd(lhs(Precond),TransRuleM),
  noteable_propdiffs(In,OutL,Same,_L,_R),
  append_vsets([Precond,Same],NewPrecond),
  subst(TransRuleM,lhs(Precond),lhs(NewPrecond),TransRule),!.

% just copy an object
trans_rule(Info,[In],[Out],Rules):- 
  sub_cmpd(step(Step),Info), sub_cmpd(why(TypeO),Info),
  noteable_propdiffs(In, Out, Same, _L, R), R==[],
  Rules = [ copy_if_match(Info,rhs(copy_step(Step,TypeO)),lhs(Same)) ],!.

% just copy an object
trans_rule(Info, [In], [Out], Rules):-
  how_are_different(In, Out, _TypeChanges, Diff), Diff==[],
  into_lhs(In, LHS),
  %%must_det_ll((sub_cmpd(step(Step), Info), sub_cmpd(why(TypeO), Info))),
  Rules = [ copy_if_match(Info, rhs(copy_step), lhs(LHS)) ], !.

% just copy an object
trans_rule(Info,In,Out,Rules):- 
  sub_cmpd(step(Step),Info), sub_cmpd(why(TypeO),Info),
  noteable_propdiffs(In,Out,Same,L,R),L==[],R==[],
  Rules = [ copy_if_match(Info,rhs(copy_step(Step,TypeO)),lhs(Same)) ],!.


% copy/transform 
trans_rule(Info,In,Out,Rules):- 
  noteable_propdiffs(In,Out,_Same,_L,R),
  into_lhs(In,LHS),  
  findall(edit_copy(Info,rhs(edit(Type,Change,P)),lhs(LHS)),
    (member(P,R),prop_pairs(In,Out,Type,Change,P),
      Change\==same,
      P\==pen([cc(black, 1)]),
      good_for_rhs(P)),Rules),Rules\==[],!.

trans_rule(Info, E1, E2, Rules):- print((Info, E1, E2)), !, fail,
  noteable_propdiffs(E1,E2,NSame,NL,NR),
  dash_chars,
  pp_ilp(l2r(Info, E1, E2)),
  dash_chars,
  if_t(how_are_different(E1, E2, Set), pp_ilp(how_are_different=Set)),
  flat_props(E1,FP1),flat_props(E2,FP2),
  intersection(FP1,FP2,Same,InFlatP,OutPFlat),
  pp_ilp(info=Info),
  pp_ilp(nadded=NR),
  pp_ilp(added=OutPFlat),
  pp_ilp(nremoved=NL),
  pp_ilp(removed=InFlatP),
  pp_ilp(nsames=NSame),
  pp_ilp(sames=Same),
  %itrace,
  sub_cmpd(step(Step),Info), sub_cmpd(why(TypeO),Info),
  dash_chars,
  Rules = [ 
    create_object_step(Info,rhs(create3c(Step,TypeO,E2)),lhs(Same)) ],!.
    %copy_if_match(Info,rhs(copy_step(Step,TypeO)),lhs(Same)) ].


















into_input_objects(TestID,ExampleNum,In,Objs,VM):-
  grid_vm(In, VM),
  best_obj_group_pair(TestID, ExampleNum, _, Objs, _OutC).

solve_via_scene_change_rules(TestID,ExampleNum):-
 must_det_ll((
  maybe_repress_output((ensure_scene_change_rules(TestID))),
  % predict_grid_size_now(TestID,In,PX,PY),
  ensure_scene_change_rules(TestID),
  print_object_dependancy(TestID),
  repress_some_output(print_scene_change_rules_if_different(solve_via_scene_change_rules, ac_listing, TestID)))),

  kaggle_arc(TestID, ExampleNum, In, Expected),
  into_solid_grid(Expected, ExpectedOut),

  duplicate_term(In, InOrig), !,


 banner_lines(blue, 4),
 (((into_input_objects(TestID, ExampleNum, In, Objs, VM),
 dash_chars,
 print_ss(wqs(expected_answer(ExampleNum)), Objs, ExpectedOut),
 dash_chars,
 repress_some_output( once(enter_solve_obj(VM, TestID, ExampleNum, Objs, ObjsO)))))*->true;

 (Objs=In, ObjsO=Objs)),

 must_det_ll((
  dash_chars,
  print_ss(wqs(solve_via_scene_change_rules(ExampleNum)),Objs,ObjsO),
  dash_chars,
  into_solid_grid(ObjsO,OurSolution1),
  maybe_resize_our_solution(TestID,In,OurSolution1,OurSolution),
  count_difs(ExpectedOut,OurSolution,Errors))),
 !,
  (Errors == 0 ->  
   (nop((when_entire_suite(banner_lines(cyan,1),banner_lines(green,4)))),
    print_ss(wqs(solve_via_scene_change(TestID,ExampleNum,errors=Errors)),ExpectedOut,OurSolution),
    print_scene_change_rules(rules_at_time_of_success(green),TestID),force_report_count(1))
    ;(banner_lines(red,10),!,
      %show_time_of_failure(TestID),
      banner_lines(red,10),
      print_scene_change_rules(rules_at_time_of_failure(red),TestID),
      locally(nb_setval(without_comment,t),
       print_scene_change_rules(rules_at_time_of_failure(yellow),TestID)),
      print_grid(in,InOrig),
      print_ss(wqs(solve_via_scene_change(TestID,ExampleNum,errors=Errors)),ExpectedOut,OurSolution),
      banner_lines(red,10),
      force_report_count_plus(-1),!,
      %when_entire_suite(print_test(TestID),true),
      banner_lines(red,1),!,fail,
      %if_t((findall(_,ac_rules(_,_,_,_),L), L == []), (get_scene_change_rules(TestID,pass2_rule_new,Rules),pp_ilp(Rules))),banner_lines(red,5),
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
  into_lhs(Rule, PConds), into_rhs(Rule, P),
  % indv_props_list(Obj, Props), \+ member(P, Props), %\+ \+ ((member(E, Props), member(E, PConds))),
 %  once( ( \+ is_bg_object(Obj) ); sub_var(black, PConds)),
    score_rule(Ways, Obj, PConds, P, Score).

score_rule(exact, Obj, PConds, _P, Score):-  score_all_props(PConds, Obj, S0), S0>0.3, !, Score=1000.
score_rule(_Ways, Obj, PConds, _P, Score):- %fail,
   obj_atoms(Obj,A),
   obj_atoms(PConds, B),
     intersection(A,B,Good,_Extra,_Bad),
     length(Good,Score).

has_all_props(CanL,Obj):- maplist(inv_has_prop(Obj),CanL).
score_all_props(CanL,Obj,Score):- maplist(inv_has_prop_score(Obj),CanL,ScoreL),sumlist(ScoreL,Score),!.

assume_prop(P):- \+ compound(P), !, fail.
assume_prop(P):- verbatum_unifiable(P), !, fail.
assume_prop(P):- \+ \+ assume_prop1(P),!.
assume_prop(P):- \+ \+ assume_prop2(P),!.
assume_prop(P):- \+ \+ is_debug_info(P).
assume_prop(P):- get_current_test(TestID), arc_test_property(TestID, _, assume_prop(_,_), P),!.
assume_prop(P):- get_current_test(TestID), \+ \+ arc_test_property(TestID, _,assume_prop(_,P),_).

non_assumables(LHS,NonDebug):- include(not_assume_prop,LHS,NonDebug).

not_assume_prop(P):- \+ \+ assume_prop(P).
/*
is_debug_info(Var):- \+ compound(Var),!,fail.
is_debug_info(info(_)).
is_debug_info(iz(P)):-!,is_debug_info(P).
*/
%not_assumed(P):- is_unbound_prop(P),!.
%not_assumed(P):- \+ assume_prop(P).

assume_prop1(P):- \+ compound(P), !, fail.
assume_prop1(P):- verbatum_unifiable(P), !, fail.
assume_prop1(P):- \+ \+ dont_notice(P).

assume_prop2(P):- \+ compound(P), !, fail.
assume_prop2(giz(_)).
assume_prop2(mv4b(_)).
assume_prop2(mv4a(_)).
assume_prop2(P):- verbatum_unifiable(P), !, fail.
assume_prop2(grid_sz(_)).
assume_prop2(elink(_, _)).
assume_prop2(global2G(_,_)).
assume_prop2(ctx(_)).
assume_prop2(example(_)).
assume_prop2(step(_)).
assume_prop2(relax(_)).
assume_prop2(testid(_)).
assume_prop2(was_oid(_)).
assume_prop2(oid(_)).
assume_prop2(cc(bg, 0)).
assume_prop2(unique_colors_count(1)).
assume_prop2(pg(_, empty_area(_), rank1, _)).
assume_prop2(sym_counts(_, 0)).
assume_prop2(sym_counts('sym_extend_-_/_\\_|', _)).
%assume_prop2(iz(algo_sid(comp, sid_323))).
/*
assume_prop2(iz(locX(3))).


%assume_prop2(pg(_, iz(locY(_)), rank1, _)).
assume_prop2(pg(_, iz(cenGY(_)), rank1, _)).

assume_prop2(P):- relax_level_gt(1), assume_prop3(P).
*/
relax_level_gt(1).


assume_prop3(occurs_in_links(_, _)).
assume_prop3(link(_, _)).
assume_prop3(pg(_, _, rankLS, _)).
assume_prop4(links_count(_, _)).
assume_prop4(pg(_, _, rank1, _)).

max_prop_score(P,0.1):- assume_prop1(P),!.
max_prop_score(P,0.2):- assume_prop2(P),!.
max_prop_score(P,1.0):- ground(P),!.
max_prop_score(P,0.0):- is_unbound_prop(P),!.
max_prop_score(_,0.7).

inv_has_prop(Obj,Prop):- has_prop(Prop,Obj),!.
inv_has_prop(Obj,Prop):- inv_has_prop_score(Obj,Prop,Score),Score>0.

inv_has_prop_score(Obj,Prop, Score):- max_prop_score(Prop,Score), inv_has_prop2(Obj,Prop).

inv_has_prop2(_O, P):- P==[], !.
inv_has_prop2(_O,P):- \+ \+ assume_prop(P),!.

inv_has_prop2(Obj, [P|T]):- !, inv_has_prop2(Obj, P), inv_has_prop2(Obj, T).
inv_has_prop2(Obj, pg(_, B, C, D)):- has_prop(pg(_, B, C, D), Obj), !.
%inv_has_prop2(Obj, always(of_obj(Prop))):- !, ac_call(of_obj(Prop)).
inv_has_prop2(Obj, \+ Prop):- !, \+ inv_has_prop(Obj,Prop).
inv_has_prop2(Obj,grid_ops(norm,Props)):- !, has_prop(grid_ops(norm,VProps),Obj),!,Props=@=VProps.
inv_has_prop2(Obj,grid_rep(norm,Props)):- !, has_prop(grid_rep(norm,VProps),Obj),!,Props=@=VProps.
inv_has_prop2(Obj,Prop):- has_prop(Prop,Obj).

match_ok(_,B):- plain_var(B),!.
match_ok(A,B):- \+ \+ A = B.


never_use_horn_rhs(P):- var(P), !, fail.
never_use_horn_rhs(rhs(P)):- !, never_use_horn_rhs(P).
never_use_horn_rhs(create3c(_, _, _)).

solve_obj_group(VM, TestID, ExampleNum, Ctx, ObjsIn, ObjsO):-
  my_exclude(is_bg_object_really,ObjsIn,Objs),
  findall_vset_R(Rule, (ac_rules(TestID, Ctx, P, PConds), rule_units(Rule, Ctx, P, PConds)), Rules),
  unnumbervars2a(Rules, URules),
  gset(VM.rules)=URules,
  apply_rules0(VM, TestID, ExampleNum, Ctx, URules, Objs, ObjsO), ObjsO\==[], !.
%solve_obj_group(_VM, _TestID, _ExampleNum, _Ctx, Objs, Objs).


rule_units(ac_unit(_, Ctx, P, PConds), Ctx, P, PConds).

apply_rules0(VM, _TestID, _ExampleNum, Ctx, [Rule], [Obj], [NewObj]):- !,
 must_det_ll(((
  rule_units(Rule, Ctx, P, PConds),
  ignore(vm_has_obj_prop(VM, Obj, PConds)),
  wots(SS, write((Ctx:P):-PConds)),
  must_det_ll((override_object_1(VM, P, Obj, NewObj))),
  gset(VM.robjs) = [NewObj|VM.robjs],
  print_ss(wqs([ar0(SS)]), [Obj], [NewObj])))), !.

apply_rules0(VM, TestID, ExampleNum, Ctx, Rules, Objs, ObjsO):-
  apply_rules(VM, TestID, ExampleNum, Ctx, Rules, Objs, ObjsO), !.

apply_rules(_VM, _TestID, _ExampleNum, _Ctx, _Rules, [], []):-!.
apply_rules(VM, TestID, ExampleNum, Ctx, Rules, [O|Objs], [NO|NewObjs]):- !,
  apply_rules1(VM, TestID, ExampleNum, Ctx, Rules, O, NO), !,
  apply_rules(VM, TestID, ExampleNum, Ctx, Rules, Objs, NewObjs).
apply_rules(_VM, _TestID, _ExampleNum, _Ctx, _Rules, Objs, Objs).

apply_rules1(VM, _TestID, _ExampleNum, _, Rules, Obj, NewObj):-
  member(URule, Rules),copy_term(URule,Rule),rule_units(Rule , _, P, PConds), P\=happy_ending(_),
  
  vm_has_obj_prop(VM, Obj, PConds),
  wots(S, write(P:-PConds)),
  must_det_ll((override_object_1(VM, P, Obj, NewObj))),
  gset(VM.robjs) = [NewObj|VM.robjs],
  globalpoints(NewObj, GPoints),
  print_ss(wqs([ar1(S)]), [Obj], print_grid(VM.h, VM.v, GPoints)), !.

apply_rules1(VM, _TestID, _ExampleNum, _, Rules, Obj, NewObj):-
 findall(':-'(P,PConds), 
  (member(URule, Rules),copy_term(URule,Rule),rule_units(Rule , _, P, PConds),P\=happy_ending(_)),SRules),
  print_grid(Obj),nl,
  indv_props_list(Obj,Props),
  pp_ilp(':-'(obj,Props)),
  pp_ilp(srules=SRules),
 % add_dependant_scenery
  member(':-'(P,PConds),SRules),
  trace,vm_has_obj_prop(VM, Obj, PConds),  
  must_det_ll((override_object_1(VM, P, Obj, NewObj))),
  gset(VM.robjs) = [NewObj|VM.robjs],
  globalpoints(NewObj, GPoints),
  print_ss(wqs([ar1(S)]), [Obj], print_grid(VM.h, VM.v, GPoints)), !.


apply_rules1(VM, _TestID, _ExampleNum, _Ctx, _Rules, Obj, NewObj):-
  %(rule_units(Rule , _, P, PConds)), forall(member(Rule, Rules), pp(failed(Rule))),
  %indv_props_list(Obj, Props), my_partition(assume_prop, Props, _, Needed),
  pp(skipped_obj=Obj), !, fail,
  override_object_1(VM, pen([cc(brown, 1)]), Obj, NewObj), !.
apply_rules1(_VM, _TestID, _ExampleNum, _Ctx, _Rules, Objs, Objs).


vm_has_obj_prop(VM, Obj, always(of_obj(Prop, G, LL))):-
  member(Rule, VM.rules), rule_units(Rule, _Ctx, P, PConds),
  P =@= of_obj(Prop, G, LL),
  (member(O, VM.robjs);member(O, VM.objs)),
  (O \== Obj, inv_has_prop2(O, PConds)), call(G).

vm_has_obj_prop(VM, Obj, always(of_obj(Prop, Call, _LL))):-
 copy_term(Prop, CProp),
  (member(O, VM.robjs);member(O, VM.objs)),
  (O \== Obj, inv_has_prop2(O, Prop)), Prop\=@=CProp, !,
  call(Call).

vm_has_obj_prop(VM, Obj, always(of_obj(Prop))):-
 copy_term(Prop, CProp),
  (member(O, VM.robjs);member(O, VM.objs)),
  (O \== Obj, inv_has_prop2(O, Prop)), Prop\=CProp, !.

vm_has_obj_prop(VM, Obj, [P|T]):- !, vm_has_obj_prop(VM, Obj, P), vm_has_obj_prop(VM, Obj, T).
vm_has_obj_prop(_VM, Obj, PConds):- inv_has_prop2(Obj, PConds).

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
    pp_ilp(([[removed=Removed],[added=Added]])))))).

override_object_1(_VM,[],IO,IO):-!.
override_object_1(VM,[H|T],I,OO):- !, override_object_1(VM,H,I,M),!, override_object_1(VM,T,M,OO).
override_object_1(VM, Term, I, O):- sub_compound(rhs(P), Term), !, override_object_1(VM, P, I, O).
override_object_1(VM, Term, I, O):- sub_compound(edit(P), Term), !, override_object_1(VM, P, I, O).
override_object_1(VM, Term, I, O):- sub_compound(edit(_, _, P), Term), !, override_object_1(VM, P, I, O).
override_object_1(VM, Term, I, O):- sub_compound(edit(_, _, _, P), Term), !, override_object_1(VM, P, I, O).
override_object_1(VM, Term, I, O):- sub_compound(copy_object_one_change(_, P), Term), !, override_object_1(VM, P, I, O).
override_object_1(VM, Term, I, O):- sub_compound(copy_object_n_changes(1..1,_, P), Term), !, override_object_1(VM, P, I, O).
override_object_1(VM, Term, I, O):- sub_compound(copy_object_n_changes(1..1,_, _, P), Term), !, override_object_1(VM, P, I, O).

override_object_1(_VM, Term, I, O):- compound(Term), functor(Term, copy_object_perfect, _), !, I=O, !.
override_object_1(_VM, copy_step, Obj, Obj):-!.

override_object_1(_VM, pen([cc(black, 1)]), Obj, Obj).

override_object_1(_VM,pen([cc(Red,N)]),Obj,NewObj):- pen(Obj,[cc(Was,N)]), !,
  subst001(Obj,Was,Red,NewObj),!.

%override_object_1(_VM, loc2D(X, Y), Obj, Obj):- (X>3;Y>4), !.
override_object_1(VM,loc2D(X,Y),Obj,NewObj):- loc2D(Obj,WX,WY),  
  globalpoints(Obj,WPoints),deoffset_points(WX,WY,WPoints,LPoints),  
  offset_points(X,Y,LPoints,GPoints),rebuild_from_globalpoints(VM,Obj,GPoints,NewObj).
override_object_1(VM, Pos, Obj, NewObj):- type_prop(reposition, Pos),
  must_det_ll((make_unifiable_u(Pos, UPos), indv_props(Obj, UPos),
  calc_relative_2d(UPos, Pos, OX, OY), globalpoints(Obj, OGPoints),
  offset_points(OX, OY, OGPoints, GPoints), rebuild_from_globalpoints(VM, Obj, GPoints, NewObj))).
%override_object_1(VM,Term,I,O):- sub_term(Sub,Term), compound(Sub),Sub=edit(_,_,_,P),  !, pp_ilp(Term), I=O,!. %override_object_1(VM,P,I,O).


override_object_1(VM, Term, I, O):- I=obj(List), functor(Term, F, A), functor(UTerm, F, A), \+ member(UTerm, List),
  arg(A, Term, Arg), compound(Arg), !, override_object_1(VM, Arg, I, O).

override_object_1(_VM, Term, I, O):- I=obj(List), functor(Term, F, A), functor(UTerm, F, A), member(UTerm, List),
  override_object(Term, I, O), !.

override_object_1(_VM,O,I,OO):- override_object(O,I,OO),!.

calc_relative_2d(iz(UPos), iz(Pos), OX, OY):- !, calc_relative_2d(UPos, Pos, OX, OY).
calc_relative_2d(UPos, Pos, OX, OY):- Pos=..[F, X, Y], UPos=..[F, UX, UY], OX is X-UX+1, OY is Y-UY+1.
calc_relative_2d(UPos, Pos, OX, 1):- Pos=..[F, X], UPos=..[F, UX], atom_contains(F, 'X'), OX is X-UX+1.
calc_relative_2d(UPos, Pos, 1, OY):- Pos=..[F, Y], UPos=..[F, UY], atom_contains(F, 'Y'), OY is Y-UY+1.

mapping_step(    in_out).
mapping_step( in_in_out).
mapping_step(in_out_out).
mapping_step(   out_out).


save_how_io(HowIn, HowOut):-
  get_current_test(TestID), save_how_io(TestID, HowIn, HowOut).
save_how_io(TestID, HowIn, HowOut):-
  assert_test_property(TestID, common, indiv_how(in), HowIn),
  assert_test_property(TestID, common, indiv_how(out), HowOut), !.



gather_objs_info_list(TestID, ExampleNum, Dir, Grid, IHowOutL):-
 (var(Grid)->kaggle_arc_io(TestID, ExampleNum, Dir, Grid);true),
  into_solid_grid(Grid, SolidG),
  findall(oc(how(Dir, HowOut), Info),
    (obj_group_io_5(TestID, ExampleNum, Dir, HowOut, OutC),
     into_solid_grid(OutC, SolidC),
     length(OutC, CLen),
     print_ss(how(Dir, HowOut, CLen), SolidC, SolidG),
     SolidC=@=SolidG,
     neg_fg_obj_counts(TestID, ExampleNum, Dir, Grid, HowOut, OutC, Info)), HowOutL),
  improve(HowOutL, IHowOutL).

improve([HowOutL], [HowOutL]):-!.
improve(HowOutLL, Best):-
  sort(HowOutLL, HowOutL),
  my_partition(is_bad_method, HowOutL, Bad, Good),
  improve(Good, Bad, Best), !.

improve(Good, [], Best):- !, predsort(sort_on(objects_list_quality), Good, Best), !.
improve(Good, Bad, Best):- Bad\==[], length(Good, GL), length(Bad, BL), GL>BL, !, improve(Good, [], Best).
improve([], Bad, Best):- Bad\==[], improve(Bad, [], Best), !.

is_bad_method(oc(_, List)):- sub_cmpd(mass_percent(100), List), !, fail.
is_bad_method(oc(_, List)):- \+ sub_cmpd(all_percent(-100), List), !, fail.

objects_list_quality(C, PC):- sub_cmpd(mass_percent(PC), C), !.
objects_list_quality(C, PC):- sub_cmpd([PC|_], C), !.
objects_list_quality(_, 1).

obj_group_pair(TestID, ExampleNum, HowInOut, InC, OutC):-
  \+ ground(TestID>ExampleNum), !,
  current_example_nums(TestID, ExampleNum),
  obj_group_pair(TestID, ExampleNum, HowInOut, InC, OutC).
/*
obj_group_pair(TestID, ExampleNum, in_out(HowIn, HowOut), InC, OutC):-
  guess_individuator(TestID, HowIn, HowOut),
  pp(guess_individuator(TestID, HowIn, HowOut)),
  kaggle_arc(TestID, ExampleNum, GridIn, GridOut),
  individuate_3(HowIn, GridIn, InC),
  individuate_3(HowOut, GridOut, OutC).

obj_group_pair(TestID, ExampleNum, in_out(HowIn, HowOut), InC, OutC):-
  dmsg(failed(guess_individuator(TestID))),
  each_pair_group(TestID, ExampleNum, HowIn, HowOut, InC, OutC).
*/
obj_group_pair(TestID, ExampleNum, HowIO, InC, OutC):-
  must_det_ll(in_out(HowIn, HowOut)=HowIO),
  %ensure_individuals(TestID, ExampleNum),
  (var(HowIn)->get_each_ndividuator(in, HowIn);true),
  (var(HowOut)->get_each_ndividuator(out, HowOut);true),
  obj_group_io_5(TestID, ExampleNum, in, HowIn, InC),
  obj_group_io_5(TestID, ExampleNum, out, HowOut, OutC).
/*
obj_group_pair(TestID, ExampleNum, in_out(HowIn, HowOut), InC, OutC):-
  kaggle_arc(TestID, ExampleNum, GridIn, GridOut),
  once((gather_objs_info_list(TestID, ExampleNum, in, GridIn, HowInL),
        gather_objs_info_list(TestID, ExampleNum, out, GridOut, HowOutL))),
  member(HowIn, HowInL), member(HowOut, HowOutL),
  %how_generic_simularity(TestID, ExampleNum, HowInL, HowOutL, HowIn, HowOut),
  individuate_3(HowIn, GridIn, InC),
  individuate_3(HowOut, GridOut, OutC).
*/

print_groups(TestID):-
  forall(current_example_nums(TestID, ExampleNum),
    print_groups(TestID, ExampleNum)).

print_groups(TestID, ExampleNum):-
  forall(individuated_cache_group(TestID, ExampleNum, Dir, ROptions, Info, Objs),
   (print_grid(wqs([Dir, ROptions]), Objs), nl,
    my_include(arg_not(1, is_group), Info, Stats),
    print(Stats), nl)).

arg_not(N, P1, Term):- arg(N, Term, Arg), \+ call(P1, Arg), !.


print_pair_groups(TestID):-
  forall(current_example_nums(TestID, ExampleNum),
    print_pair_groups(TestID, ExampleNum)).

print_pair_groups(TestID, ExampleNum):-
  each_pair_group(TestID, ExampleNum, HowIn, HowOut, InC, OutC),
  print_ss(HowIn=InC, HowOut=OutC).

each_pair_group(TestID, ExampleNum, HowIn, HowOut, InC, OutC):-
  current_example_nums(TestID, ExampleNum),
  kaggle_arc(TestID, ExampleNum, GridIn, GridOut),
  gather_objs_info_list(TestID, ExampleNum, in, GridIn, HowInL),
  gather_objs_info_list(TestID, ExampleNum, out, GridOut, HowOutL),
  how_generic_simularity(TestID, ExampleNum, HowInL, HowOutL, HowIn, HowOut),
  grid_to_objs(GridIn, HowIn, InC),
  grid_to_objs(GridOut, HowOut, OutC).

     
how_generic_simularity(TestID, ExampleNum, HowInL, HowOutL, HowIn, HowOut):-
 current_example_nums(TestID, ExampleNum),
 kaggle_arc(TestID, ExampleNum, GridIn, GridOut),
  (var(HowInL)-> gather_objs_info_list(TestID, ExampleNum, in, GridIn, HowInL) ; true),
  (var(HowOutL)-> gather_objs_info_list(TestID, ExampleNum, out, GridOut, HowOutL) ; true),
 must_det_ll(
    findall(Similarity-pair(Object1, Object2, ExtraInfo),
     (member(Object1, HowInL),
      member(Object2, HowOutL),
      call(generic_simularity, Object1, Object2, Similarity, ExtraInfo)),
     Pairs)),
  must_det_ll(sort(Pairs, Sorted)),
  must_det_ll(maplist(arg(2), Sorted, HowPairs)),
  %pp(howPairs=HowPairs),
  must_det_ll(best_how_pairs(HowPairs, BestPair)),
  pp(bestPair=BestPair),
  sub_cmpd(how(in, HowIn), BestPair),
  sub_cmpd(how(out, HowOut), BestPair),
  save_how_io(TestID, HowIn, HowOut), !.


%best_obj_group_pair(TestID, ExampleNum, InC, OutC):-
   %findall(NV, (arc_cache:trans_rule_db(TestID, ExampleNum, N, V), append_term(N, V, NV)), Props),
%  obj_group_pair(TestID, ExampleNum, InCC, OutCC),
%  InC = InCC, OutC = OutCC.
  %Grid= VM.start_grid,
  %hv_point_value(1, 1, Grid, PointNW),
  %hv_point_value(1, V, Grid, PointSW),
  %hv_point_value(H, 1, Grid, PointNE),
  %hv_point_value(H, V, Grid, PointSE),
%%  once((kaggle_arc(TestID, ExampleNum, In, Out), grid_props(In, InProps), grid_props(Out, OutProps))),
%%  InC = [obj([giz(g(in))|InProps])|InCC], OutC = [obj([giz(g(out))|OutProps])|OutCC].
  %append(Props, [mass(0), vis2D(H, V), birth(named_grid_props), loc2D(1, 1), iz(flag(always_keep)), iz(media(image)), iz(flag(hidden))], AllProps),
  %make_indiv_object(VM, AllProps, [PointNW, PointSW, PointNE, PointSE], _), !.
     
with_individuated_cache(TF, Goal):- locally(nb_setval(use_individuated_cache, TF), Goal).

obj_group_io(TestID, ExampleNum, IO, Objs):-
 current_example_nums(TestID, ExampleNum),
 with_individuated_cache(true, obj_group_io_5(TestID, ExampleNum, IO, _, Objs)).

obj_group_io_5(TestID, ExampleNum, Dir, ROptions, Objs):- var(TestID), !,
  ensure_test(TestID), obj_group_io_5(TestID, ExampleNum, Dir, ROptions, Objs).

obj_group_io_5(TestID, ExampleNum, Dir, ROptions, Objs):- var(ExampleNum), !,
  current_example_scope(TestID, ExampleNum),
  obj_group_io_5(TestID, ExampleNum, Dir, ROptions, Objs).

obj_group_io_5(TestID, ExampleNum, Dir, ROptions, Objs):- var(Dir), !,
  member(Dir, [out, in]),
  obj_group_io_5(TestID, ExampleNum, Dir, ROptions, Objs).

obj_group_io_5(TestID, ExampleNum, Dir, ROptions, Objs):- var(ROptions), !,
 get_each_ndividuator(Dir, ROptions),
 obj_group_io_5(TestID, ExampleNum, Dir, ROptions, Objs).

obj_group_io_5(TestID, ExampleNum, Dir, ROptions, Objs):-
 kaggle_arc_io(TestID, ExampleNum, Dir, Grid),
 kaggle_arc(TestID, ExampleNum, I, O),
 (Grid==I->Other=O;Other=I),
 set_example_num(ExampleNum),
 with_other_grid(Other,
     individuate_3(ROptions, Grid, Objs)).

/*obj_group_io_6(TestID, ExampleNum, Dir, ROptions, Grid, Objs):-
 current_example_nums(TestID, ExampleNum),
 kaggle_arc_io(TestID, ExampleNum, Dir, Grid),
 once(grid_to_objs(Grid, ROptions, Objs)).*/
obj_group_io_6(TestID, Example+Num, Dir, ROptions, Grid, Objs):-
  ((from_individuated_cache(TestID, TID, GOID, Dir, ROptions, Objs), Objs\==[],
  once((testid_name_num_io_0(TID, _, Example, Num, Dir),
        testid_name_num_io_0(GOID, _, Example, Num, Dir))))*-> true ;
    individuate_3(ROptions, Grid, Objs)).

individuated_cache_group(TestID, ExampleNum, Dir1, FROptions1, Info1, SObjs1):-
  kaggle_arc(TestID, ExampleNum, _, _),
  Template = cg_individuated_cache_group(Info, Dir, FROptions, SObjs),
  Result = cg_individuated_cache_group(Info1, Dir1, FROptions1, SObjs1),
  findall(Template,
             each_individuated_cache_group(TestID, ExampleNum, Dir, FROptions, Info, SObjs),
             List),
  sort(List, Set),
  member(Result, Set).





each_individuated_cache_group(TestID, ExampleNum, Dir, FROptions, Info, SObjs):-
 RuleInfo =  [example(ExampleNum), dir(Dir), roptions(FROptions), testid(TestID), tid(FTID)],
 from_individuated_cache(TestID, FTID, FGOID, Dir, FROptions, Objs),
 once((
  maplist(ignore,
  [ignore(testid_name_num_io(FTID, TestIDA, ExampleA, NumA, DirA)),
   ignore(testid_name_num_io(FGOID, TestIDB, ExampleB, NumB, DirB)),
   DirA=DirB, NumA=NumB, TestIDB=TestIDA, ExampleB=ExampleA,
   (ExampleNum=(ExampleA+NumA)), DirB = Dir,
   kaggle_arc_io(TestID, ExampleNum, Dir, Grid)]),
  my_partition(is_input_object, Objs, Ins, Outs))),
  member(Dir=SObjs, [in=Ins, out=Outs]),
  once((SObjs\==[],
  neg_fg_obj_counts(TestID, ExampleNum, Dir, Grid, FROptions, SObjs, SObjInfo))),
  append(SObjInfo, RuleInfo, Info).



overlapped_fg_props(FGO, OLP) :-
   maplist(indv_props_list, FGO, [F|GP]),
   include({GP}/[Prop]>>(forall(member(O, GP), member(Prop, O))), F, OLP).

if_number_negate(Info, NegInfo):- number(Info), NegInfo is - Info.
if_number_negate(Info, NegInfo):- is_list(Info), !, maplist(if_number_negate, Info, NegInfo).
if_number_negate(Info, NegInfo):- compound(Info), !, compound_name_arguments(Info, F, [A|Out]),
  if_number_negate(A, B), compound_name_arguments(NegInfo, F, [B|Out]).
if_number_negate(Info, Info).

neg_fg_obj_counts(TestID, ExampleNum, Dir, Grid, ROptions, ObjsIn, OutNegInfo):- %rtrace,
  Info =  [fgo_n(NFG), bgo_n(NBG), len(Len), mass_percent(Percent), first_mass(FMass), all_percent(TPercent),
   fake_percents(Fake), roption(ROptions), example(ExampleNum), dir(Dir), testid(TestID)],
  include(has_prop(giz(testid_example_io(TestID>ExampleNum*Dir))), ObjsIn, Objs),
  visible_order(Objs, SObjs),
  length(SObjs, Len),
  once((my_partition(is_bg_object, SObjs, BGO, FGO), length(FGO, NFG), length(BGO, NBG))),
  once(NBG>=2;NFG>=2),
   %must_det_ll((
   mass(FGO, FGMass), mass(BGO, BGMass),
  (var(Grid)->(into_solid_grid(SObjs, Grid), Fake=true);Fake=false),
   %nth0(0, SObjs, First),
   mass(Grid, FG_G_Mass), area(Grid, Area), BG_G_Mass is Area-FG_G_Mass,
  ((member(First, FGO), mass(First, FMass), FMass>1, FMass<FG_G_Mass)->true;
  ((member(First, FGO), mass(First, FMass), FMass>0, FMass<FG_G_Mass)->true;
  ((member(First, BGO), mass(First, FMass), FMass>1, FMass<FG_G_Mass)->true;
  ((member(First, BGO), mass(First, FMass), FMass>0, FMass<FG_G_Mass)->true;
   (member(First, FGO), mass(First, FMass)))))), !,


  once((must_not_error((
   if_t(FGMass>0.0, (FGMass=:=FG_G_Mass ; FGMass>9)),

   if_t(FG_G_Mass>0.2, Percent is -(FMass/FG_G_Mass*100)),
   if_t(var(Percent), Percent is -(FMass/BG_G_Mass*100)),


   if_t(FG_G_Mass>0.1, TPercent is (FGMass/FG_G_Mass*100)),
   if_t(var(TPercent), TPercent is (BGMass/BG_G_Mass*100)),


   if_t(FGMass==0, if_t( BGMass>0.3 , BGMass=:=BG_G_Mass)),

   %overlapped_fg_props(FGO, OLP), length(OLP, OLPn),
   %all_different_fg_props(FGO, DOLP), length(DOLP, DOLPn), !,

   %pp([overlap(OLPn, OLP), differnce(DOLPn, DOLP)]),
   maplist(if_number_negate, Info, NegInfo), !,
   maplist(object_oc, FGO, FGG),
   maplist(object_oc, BGO, BGG),
   append(FGG, BGG, OGG),
   %obj_atoms(FGO, Atoms), !, nop(obj_atoms(Atoms)),
   %append(NegInfo, [ogg(OGG)], OutNegInfo))
   append(NegInfo, OGG, OutNegInfo))))).

global_oid_equals_grid(Obj, OID=globalpoints(Grid)):- obj_to_oid(Obj, OID), globalpoints(Obj, Grid), !.
%global_oid_equals_mass(Obj, oid(OID, mass(Mass)):- obj_to_oid(Obj, OID), mass(Obj, Mass), iz(sid(Obj, Mass)), mass(Obj, Mass), !.

print_long_set(Set1):- is_list(Set1), length(S, 20), append(S, R, Set1), print(S), nl, !, print_long_set(R).
print_long_set(Set1):- print(Set1), nl.

%compatible_numbers(L, R, _):- (L#>0;R#>0), !, fail.
%compatible_numbers(L, R, equal):- L=R, !.
compatible_numbers(L, R, l_r_factor(0, 0, 1)):- !, L=R.
compatible_numbers(L, R, l_r_factor(OffsetL, OffsetR, Factor)):-
  L#>0, R#>0,
  OffsetL in -1..4,
  OffsetR in  OffsetL..1,
  L-OffsetL#>0,
  R-OffsetR#>0,
  %(OffsetR #=< OffsetL #\/ OffsetR #=0 #\/ OffsetR #= -1),
 (R-OffsetR #= Factor * (L-OffsetL)).

%print_term(lhs:-Set1, [nl(true), fullstop(true)]),
% Calculate the Jaccard similarity with bonuses for closer cells and same colors
generic_simularity(Object1, Object2, Similarity, ExtraInfo) :-
  oc_set(Object1, PropSet1), oc_set(Object2, PropSet2), !,
  generic_propset_simularity(PropSet1, PropSet2, Similarity, ExtraInfo), !.

generic_propset_simularity(PropSet1, PropSet2, Similarity, ExtraInfo):-
  ExtraInfo =  [how_len(HowLen), how_fg_len(HowFGLen)],
  if_t((sub_cmpd(len(Len1), PropSet1), sub_cmpd(len(Len2), PropSet2)), compatible_numbers(Len1, Len2, HowLen)),
  if_t((sub_cmpd(fgo_n(FGLen1), PropSet1), sub_cmpd(fgo_n(FGLen2), PropSet2)), compatible_numbers(FGLen1, FGLen2, HowFGLen)),
  must_det_ll((

%   wdmsg(lhs), print_long_set(PropSet1),
%   wdmsg(rhs), print_long_set(PropSet2),
    intersection(PropSet1, PropSet2, Intersection),
    union(PropSet1, PropSet2, Union),
    length(Intersection, IntersectionLen),
    length(Union, UnionLength),
    into_subcells(PropSet1, SubPropSet1),
    into_subcells(PropSet2, SubPropSet2),
    distance_bonus(SubPropSet1, SubPropSet2, DistanceBonus),
    color_bonus(SubPropSet1, SubPropSet2, ColorBonus),
    propset_name(PropSet1, PropSet1Name),
    propset_name(PropSet2, PropSet2Name),
    Similarity is (IntersectionLen / UnionLength) + DistanceBonus + ColorBonus,
    wdmsg(simularity(PropSet1Name, PropSet2Name, Similarity is (IntersectionLen / UnionLength) + DistanceBonus + ColorBonus)))).

propset_name(PropSet2, PropSet2Name):- maplist(first_atom_or_value, PropSet2, PropSet2Name).

first_atom_or_value(C, C):- \+ compound(C), !.
first_atom_or_value(C, FA):- C=..[_|Args], sub_term(A, Args), atom(A), !, FA=A, !.
first_atom_or_value(C, FA):- C=..[_, A], atomic(A), !, FA=C.
first_atom_or_value(C, A):- sub_term(A, C), number(A), !.
first_atom_or_value(C, A):- sub_term(A, C), atomic(A), !.
first_atom_or_value(C, C).

%generic_propset_simularity(_PropSet1, _PropSet2, 0, []):- !.

into_subcells(O2, Set2):- into_subcells0(O2, List2), !, list_to_set(List2, Set2).
into_subcells0(NC, Set):- atom(NC), oid_to_obj(NC, Obj), !, into_subcells0(Obj, Set).
into_subcells0(NC, Set):- atom(NC), gid_to_grid(NC, Obj), !, into_subcells0(Obj, Set).
into_subcells0(NC, []):- \+ compound(NC), !.
into_subcells0(O2, [O2]):- functor(O2, cell, _), !.
into_subcells0(oc(_, O2), Set2):- !, into_subcells0(O2, Set2).
into_subcells0(O2, Set2):- is_gridoid(O2), globalpoints(O2, Set2), !.
into_subcells0(O2, Set2):- is_list(O2), maplist(into_subcells0, O2, Set1), flatten(Set1, Set2).
into_subcells0(_, []).


best_how_pairs(HowPairs, BestPair):-
   member(BestPair, HowPairs), !.


all_different_fg_props(FGO, OLP) :-
   maplist(indv_props_list, FGO, [F|GP]),
   length(FGO, Len),
   include(include_all_different(Len, [F|GP]), F, OLP).

include_all_different(Len, FGO, Prop) :-
    make_unifiable(Prop, UHAD),
    findall(V, (
        member(R, FGO),
        (member(UHAD, R) -> V = UHAD; V = (\+ UHAD))
    ), List),
    variant_list_to_set(List, Set),
    length(Set, SetL),
    SetL = Len.


% Define cells
pointset_data(o1, [cell(1, 1, red, _), cell(2, 3, blue, _), cell(3, 4, green, _), cell(4, 5, red, _),
                   cell(6, 7, blue, _), cell(7, 8, green, _)]).

pointset_data(o2, [cell(9, 10, blue, _), cell(10, 11, green, _), cell(11, 12, red, _), cell(12, 13, blue, _),
                   cell(14, 15, red, _), cell(15, 16, blue, _), cell(16, 17, green, _), cell(17, 18, red, _)]).

pointset_data(o3, [cell(19, 20, red, _), cell(20, 21, blue, _), cell(21, 22, green, _), cell(22, 23, red, _),
                   cell(24, 25, blue, _), cell(25, 26, green, _), cell(26, 27, red, _), cell(27, 28, blue, _)]).

pointset_data(o4, [cell(28, 29, blue, _), cell(29, 30, green, _), cell(30, 1, red, _), cell(1, 2, blue, _),
                   cell(2, 3, red, _), cell(3, 4, blue, _), cell(4, 5, green, _), cell(5, 6, red, _),
                   cell(6, 7, blue, _), cell(7, 8, red, _)]).

pointset_data(o5, [cell(8, 9, red, _), cell(9, 10, blue, _), cell(10, 11, green, _),
                   cell(11, 12, blue, _), cell(12, 13, green, _), cell(13, 14, red, _)]).

pointset_data(o6, [cell(14, 15, blue, _), cell(15, 16, green, _), cell(16, 17, red, _), cell(17, 18, blue, _),
                   cell(18, 19, red, _), cell(19, 20, blue, _), cell(20, 21, green, _), cell(21, 22, red, _),
                   cell(22, 23, blue, _), cell(23, 24, green, _)]).

% Extract x, y and color from a cell
cell_x(Term, X):- arg(1, Term, X), !.
cell_y(Term, Y):- arg(2, Term, Y), !.
cell_color(Term, C):- arg(3, Term, C), !.

% Calculate average distance between cells in two sets
avg_distance(O1, O2, AvgDistance) :-
    into_subcells(O1, Set1),
    into_subcells(O2, Set2),
    findall(Distance, (
        member(Cell1, Set1),
        center2D(Cell1, X1, Y1),
        member(Cell2, Set2),
        center2D(Cell2, X2, Y2),
        %special_distance(X1, Y1, X2, Y2, Distance)
        DX is X1-X2, DY is Y1-Y2,
        Distance is sqrt(DX*DX+DY*DY)
    ), Distances),
    sort(Distances, SDistances),
    length(SDistances, Length),
    NumDistances is Length // 3 + 1,
    length(Smaller, NumDistances),
    append(Smaller, _, SDistances),
    sumlist(Smaller, FirstDistance),
    AvgDistance is FirstDistance / NumDistances.

% Calculate distance bonus. Higher for sets of cells that are closer.
distance_bonus(Set1, Set2, DistanceBonus) :-
    avg_distance(Set1, Set2, AvgDistance),
    DistanceBonus is 1 / (1 + AvgDistance).

% Calculate color bonus.
color_bonus(Set1, Set2, ColorBonus) :-
    findall(Bonus, (
         member(Cell1, Set1), once((sub_term(Color, Cell1), atom(Color))),
         (((member(Cell2, Set2), sub_var(Color, Cell2))) -> Bonus =0 ; Bonus=1)
    ), SharedColors),
    length(SharedColors, NumColors),
    sumlist(SharedColors, Shared),
    ColorBonus is Shared/NumColors.


% Calculate the Jaccard similarity with bonuses for closer cells and same colors
jaccard_similarity(OC1, OC2, Similarity) :-
    oc_set(OC1, Set1), oc_set(OC2, Set2),
    intersection(Set1, Set2, Intersection, _, _),
    union(Set1, Set2, Union),
    length(Intersection, IntersectionLen),
    length(Union, UnionLength),
    Bonus is IntersectionLen / UnionLength,
    distance_bonus(Set1, Set2, DistanceBonus),
    color_bonus(Set1, Set2, ColorBonus),
    Similarity is Bonus + DistanceBonus + ColorBonus.

% Add the oc functor to the objects
add_oc(OC, OC) :- functor(OC, oc, 2), !.
add_oc(Obj, oc(Obj, ComparableSet)) :- pointset_data(Obj, ComparableSet), !.
add_oc(Obj, oc(Obj, ComparableSet)) :- obj_atoms(Obj, ComparableSet), !.

% Extracts the cached comparable pointset
oc_set(oc(_, Set), Set).

% Pair up two sets based on best similarity
pair_up_by_best_similarity(_P4, [], _, []).
pair_up_by_best_similarity(_P4, _, [], []).
% Snipes A Right side for each left
pair_up_by_best_similarity( P4, [Object1|RemainingObjectSet2], ObjectSet2, [pair(Object1, Object2, ExtraInfo)|Groups]) :- fail, % old way
    find_best_pair(P4, Object1, ObjectSet2, Object2, ExtraInfo),
    delete(ObjectSet2, Object2, RemainingObjectSet2),
    pair_up_by_best_similarity(P4, RemainingObjectSet2, RemainingObjectSet2, Groups).

pair_up_by_best_similarity( P4, ObjectSet1, ObjectSet2, Groups) :- true, % out way
  must_det_ll((
    findall(Similarity-pair(Object1, Object2, ExtraInfo),
     (member(Object1, ObjectSet1),
      member(Object2, ObjectSet2),
      call(P4, Object1, Object2, Similarity, ExtraInfo)), Rules),
    sort(Rules, Sorted),
    maplist(arg(2), Sorted, Groups))).

% Find the best pair for a given set
find_best_pair(P4, Object1, ObjectSet2, BestObject2, ExtraInfo) :-
    findall(Similarity-pair(Object1, Object2, ExtraInfo), (
        member(Object2, ObjectSet2),
        call(P4, Object1, Object2, Similarity, ExtraInfo)
    ), Rules),
    sort(Rules, [_-pair(_, BestObject2, ExtraInfo)|_]).

% Partition objects into groups based on best similarity
partition_objects_C1(P4, Objects1, Objects2, Groups) :-
    maplist(add_oc, Objects1, OC1s),
    maplist(add_oc, Objects2, OC2s),
    pair_up_by_best_similarity(P4, OC1s, OC2s, Groups).

% Predicate to partition objects into groups based on best similarity
% Takes the unpaired sets from previous iterations, combines them into superobjects
% and then tries to pair these with the remaining sets.
partition_objects_Cn(_P4, [], Groups, Groups):- !.
partition_objects_Cn(_P4, Groups, [], Groups):- !.
partition_objects_Cn(P4, Objects1, Objects2, FinalGroups) :-
    maplist(add_oc, Objects1, OC1s),
    maplist(add_oc, Objects2, OC2s),
    pair_up_by_best_similarity(P4, OC1s, OC2s, InitialPairings),
    flatten(InitialPairings, RemoveThese),
    subtract(OC2s, RemoveThese, UnpairedElements),
    ( UnpairedElements == [] ->
     FinalGroups = InitialPairings ;
     ( maplist(combine_groups, InitialPairings, SuperObjects),
      partition_objects_Cn(P4, SuperObjects, UnpairedElements, FinalGroups))).

% Combines the sets in a group into a single set, allowing the group to be treated
% as a single object in subsequent iterations of the pairing algorithm.
combine_groups(OCList, oc(Os, SetOfCells)):-
   maplist(arg(1), OCList, Os),
   maplist(arg(2), OCList, Cs),
   flatten(Cs, List), list_to_set(List, SetOfCells).


% Test query
%?- partition_objects_Cn(P4, [o1, o2], [o3, o4, o5, o6], Groups).



/*

overlapped_fg_props(FGO, OLP):-
   maplist(indv_props_list, FGO, [F|GP]),
   include(all_have(FGO), F, OLP).

all_have(FGO, Prop):- forall(member(O, FGO), has_prop(Prop, O)).
*/


  
/*

show_pairs(InS, OutS, pair(oc(ObjsI, InfoI), oc(ObjsO, InfoO))):-
   print_ss(Closeness, ObjsI, ObjsO), nl,
   my_include(arg_not(1, is_group), InfoI, StatsI),
   my_include(arg_not(1, is_group), InfoO, StatsO),
   print(StatsI), nl,
   print(StatsO), nl.
*/



/*

best_individuated_cache(TestID, FTID, FGOID, FROptions, Nums, Objs1, Objs2):-
  best_individuated_cache_group(TestID, ExampleNum1, Dir1, FGOID1, FROptions1, NFG1+NBG1, SIObjs1, Info1),
  best_individuated_cache_group(TestID, ExampleNum2, Dir2, FGOID2, FROptions2, NFG2+NBG2, SIObjs2, Info2),
  merge_vals(TestID, ExampleNum2, Dir2, FGOID2, FROptions2, NFG2+NBG2, SIObjs2, Info2

*/
  
from_individuated_cache(TestID, FTID, FGOID, Dir, FROptions, ObjsO):-
  (arc_cache:individuated_cache(TestID, TID, GOID, ROptions, Objs)
  ;arc_cache:individuated_cache(TID, GOID, ROptions, Objs)
  ;saved_group(individuate_3(ROptions, GOID), Objs)),
  (nonvar(FTID)-> \+ \+ sub_var(FGOID, (FTID, GOID, ROptions, Objs)) ; FTID=TID),
  (nonvar(FGOID)-> \+ \+ sub_var(FGOID, (TID, GOID, ROptions, Objs)) ; FGOID=GOID),
  (nonvar(FROptions)-> \+ \+ sub_var(FGOID, (TID, GOID, ROptions, Objs)) ; ROptions=FROptions),
  ((var(FTID), nonvar(GOID))-> (testid_name_num_io_0(GOID, TestIDG, Example, Num, Dir), (FTID=(TestIDG>(Example+Num)*Dir)));true),
  ((var(TestID), nonvar(GOID))-> (testid_name_num_io_0(GOID, TestID, _Example, _Num, _Dir)) ; true),
  include(sub_var(Dir), Objs, ObjsO).


%show_object_dependancy(_TestID):-  !.
% =============================================================
show_object_dependancy(TestID):-  
% =============================================================
 ensure_test(TestID),
   %print_groups(TestID),
  %print_pair_groups(TestID),
 %learn_object_dependancy(TestID),
 print_object_dependancy(TestID).

scope_training(ExampleNum):-
  ignore((ExampleNum=(trn+_))),
  ignore((
    get_pair_mode(single_pair), luser_getval(example, UExampleNum),
      ignore(ExampleNum = UExampleNum))).


common_props([O|Objs], Props):-
   indv_props_list(O, List),
   findall(P, (member(P, List), \+ dont_notice(P), forall(member(E, Objs), has_prop(P, E))), Props).

current_example_scope(TestID, ExampleNum):-
 (var(TestID)->get_current_test(TestID);true), !,
  scope_training(ExampleNum),
  kaggle_arc(TestID, ExampleNum, _, _),
 (get_pair_mode(single_pair)->!;true).

current_example_nums(TestID, ExampleNum):-
  (var(TestID)->get_current_test(TestID);true),
  scope_training(ExampleNum),
  kaggle_arc(TestID, ExampleNum, _, _).



verify_groups(TestID, ExampleNum, RHSObjs, LHSObjs, Groups):- is_list(Groups), !,
  forall(member(Group, Groups), verify_groups(TestID, ExampleNum, RHSObjs, LHSObjs, Group)).

verify_groups(_TestID, _ExampleNum, _RHSObjs, _LHSObjs, call(_)):-!.
verify_groups(_TestID, _ExampleNum, _RHSObjs, _LHSObjs, l2r(Info, In, Out)):-
   into_list(In, InL), into_list(Out, OutL), trans_rule(Info, InL, OutL, TransRules), TransRules \==[], !.

relaxed_levels([ending(balanced(_))]).
relaxed_levels([]).
%relaxed_levels(RelaxLvl):- arg(_, v([], [delete], [all]), RelaxLvl).
member_of_relax(S, InfoOut):-
  sub_cmpd(relax(RelaxLvl), InfoOut), !,
  make_unifiable(S, P), !, \+ (( member(P, RelaxLvl), P\=S)), ignore(P=S).
member_of_relax(_S, _InfoOut).

is_object_wo_black(X):- \+ sub_var(black, X).

has_lhs_f(F):- member(F,[obj,ac_unit]).

marginalize_prop(TestID,ExampleNum,Why,P,LHS,LHS):- !,
   make_unifiable_u(P,U), 
   pp_ilp(marginalize_prop=(Why,P,LHS)),trace,
   assert_test_property(TestID, ExampleNum,assume_prop(Why,U), P).

marginalize_prop(_TestID,_ExampleNum,_,_,NC,NCO):- \+ compound(NC),!,NCO=NC.
marginalize_prop(TestID,ExampleNum,Why,P,[H|T],TT):- P==H,!,marginalize_prop(TestID,ExampleNum,Why,P,T,TT).
marginalize_prop(TestID,ExampleNum,Why,P,[H|T],TT):- P=@=H,!,marginalize_prop(TestID,ExampleNum,Why,P,T,TT).
marginalize_prop(TestID,ExampleNum,Why,P,[H|T],[HH|TT]):- !, marginalize_prop(TestID,ExampleNum,Why,P,H,HH),marginalize_prop(TestID,ExampleNum,Why,P,T,TT).
marginalize_prop(TestID,ExampleNum,Why,P,R,RL):- 
   R=..[F|Args],member(F,[obj,ac_unit]),has_lhs_f(F),append(Left,[L],Args),is_list(L),
   marginalize_prop(TestID,ExampleNum,Why,P,L,LL),!, append(Left,[LL],ArgsL),RL=..[F|ArgsL].
marginalize_prop(_TestID,_ExampleNum,_,P,RHSObjs,RHSM):- make_unifiable_u(P,U),P\=@=U,!,subst001(RHSObjs, P, shared(U), RHSM).

identical_props([Ex|ExampleObjs],P):- 
  into_lhs(Ex,Props), member(P,Props), make_unifiable_u(P,U),P\=@=U,
  P\=iz(info(_)), \+ assume_prop(P), 
  forall((member(O,ExampleObjs),into_lhs(O,OProps)), 
         (member(E,OProps),E=@=O)).


each_has_more_than_one_non_assumable(LR,P,ExampleObjs):- happy_ending(_)\=P,
  forall(member(O,ExampleObjs),has_more_than_one_non_assumable(O)),
  \+ \+ once((member(O,ExampleObjs),into_lhs(O,LHS), non_assumables(LHS,NonDebug),pp_ilp(non_assumables(LR,P)=NonDebug))),
  trace,!.


has_more_than_one_non_assumable(O):- 
 must_det_ll(( into_lhs(O,LHS), non_assumables(LHS,NonDebug))),
 % pp_ilp(non_assumables=NonDebug),
  NonDebug\=[_],NonDebug\=[],!.


  



normalize_objects_for_dependancy(RelaxLvL, TestID, ExampleNum, RHSObjs, LHSObjs, RHSO, LHSO):-
 ((append(LHSObjs,RHSObjs,ExampleObjs), LR = both),(ExampleObjs = LHSObjs, LR = lhs); (fail, ExampleObjs = RHSObjs, LR = rhs)),
  identical_props(ExampleObjs,P), \+ assume_prop(P), make_unifiable_u(P,U), P\=@=U,
  each_has_more_than_one_non_assumable(LR,P,ExampleObjs),
  marginalize_prop(TestID,ExampleNum,LR,P,ExampleObjs,_),!,
  normalize_objects_for_dependancy(RelaxLvL, TestID, ExampleNum, RHSObjs, LHSObjs, RHSO, LHSO).

normalize_objects_for_dependancy(_RelaxLvL, _TestID, _ExampleNum, RHSObjs, LHSObjs, RHSO, LHSO):-
  %member(can(fg_only), RelaxLvL),
  %include(is_fg_object_really, LHSObjs, LHSObjsO), include(is_fg_object_really, RHSObjs, RHSObjsO),
  include(is_object_wo_black, LHSObjs, LHSObjsO), include(is_object_wo_black, RHSObjs, RHSObjsO), !,
  sort_by_jaccard(one(RHSObjsO), LHSObjsO, LHSO), sort_by_jaccard(one(LHSObjsO), RHSObjsO, RHSO), !.

% maybe trim away backgroud
normalize_objects_for_dependancy(RelaxLvl, TestID, ExampleNum, RHSObjs, LHSObjs, RHSObjsO, LHSObjsO):-
  different_lengths(LHSObjs, RHSObjs),
  member(Filter, [iz(fg_or_bg(iz_fg)), cc(bg, 0)]),
  include(has_prop(Filter), LHSObjs, LHSObjsM), LHSObjsM\==[],
  include(has_prop(Filter), RHSObjs, RHSObjsM), LHSObjsM\==[],
  \+ different_lengths(LHSObjsM, RHSObjsM), !,
  normalize_objects_for_dependancy2(RelaxLvl, TestID, ExampleNum, RHSObjsM, LHSObjsM, RHSObjsO, LHSObjsO), !.
% make finding pairs faster
normalize_objects_for_dependancy(RelaxLvl, TestID, ExampleNum, RHSObjs, LHSObjs, RHSObjsO, LHSObjsO):-
  sort_by_jaccard(one(RHSObjs), LHSObjs, LHSObjsM),
  sort_by_jaccard(one(LHSObjs), RHSObjs, RHSObjsM),
  (LHSObjsM\=LHSObjs ; RHSObjsM\=RHSObjs), !,
  normalize_objects_for_dependancy2(RelaxLvl, TestID, ExampleNum, RHSObjsM, LHSObjsM, RHSObjsO, LHSObjsO), !.
normalize_objects_for_dependancy(_RelaxLvl, _, _, R, L, R, L):-!.

normalize_objects_for_dependancy2(_RelaxLvl, _TestID, _ExampleNum, RHSObjs, LHSObjs, RHSObjs, LHSObjs).

different_lengths(LHSObjsO, RHSObjsO):-
  length(LHSObjsO, L21), length(RHSObjsO, L22), !, L21\==L22, !.

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
assert_map_pairs(TestID,ExampleNum,Ctx,Group):- is_list(Group),!,maplist(assert_map_pairs(TestID,ExampleNum,Ctx),Group).
assert_map_pairs(TestID,ExampleNum,Ctx,l2r(Info,In,Out)):- fail, !, 
  assertz_new(arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,l2r(Info,In,Out))),!.
assert_map_pairs(TestID,ExampleNum,Ctx,l2r(Info,In,Out)):-!,
 must_det_ll((
 assertz_new(arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,l2r(Info,In,Out))),
 into_list(In,InL),into_list(Out,OutL),
 must_det_ll((   
   once((trans_rule(Info,InL,OutL,TransRules), TransRules \==[])),
   
   assert_map_pairs(TestID,ExampleNum,Ctx,TransRules),
  once((diff_l_r(InL,OutL,Same,InFlatP,OutPFlat),
   unnumbervars2a(v5('$VAR'(0), '$VAR'('_'), Same, InFlatP, OutPFlat), UNV))),
                    must_det_ll((UNV = v5(_FG1,_BG1,USame,InFlatProps,OutFlatProps))),
  %pp_ilp(l2r(Info,InL,OutL)),!,  
  nop((assertz_new(arc_cache:prop_dep(TestID, ExampleNum, Ctx, Info, InL, OutL, USame, InFlatProps, OutFlatProps)))))))).
assert_map_pairs(_TestID,_ExampleNum,_Ctx,call(Rule)):-!,must_det_ll(Rule),!.
assert_map_pairs(TestID,ExampleNum,Ctx,TransRule):-
   assertz_new(arc_cache:trans_rule_db(TestID,ExampleNum,Ctx,TransRule)),!.





:- abolish(arc_cache:trans_rule_db/4).
:- dynamic(arc_cache:trans_rule_db/4).

% print the object dependencies for this test
% =============================================================
print_object_dependancy(TestID):-
% =============================================================
  /*if_t(( \+ arc_cache:map_pairs(TestID,_,_,_,_,_)),
   ( dash_chars,forall(arc_cache:map_group(TestID,_,_IO_,Group),
    once(((dash_chars,dash_chars,pp_ilp(Group),dash_chars,dash_chars)))))),
  dash_chars,*/
% findall_vset_R(l2r(Info,Pre,Post),arc_cache:map_pairs(TestID,_,_IO_2,Info,Pre,Post),Set1),
% maplist(pp_ilp,Set1),
 dash_chars,dash_chars,
 %pp_ilp_vset(l2r(Info,Pre,Post),pair_obj_info(TestID,_,_,Info,Pre,Post)),
 with_vset(
   arc_cache:trans_rule_db(TestID, _ExampleNum, l2r, l2r(Info, LHS, RHS)),
       pp_obj_tree(2, Info, LHS, RHS)),

 dash_chars,dash_chars.
 %if_t(Set1 =@= Set2,  wdmsg('Set 2 the same')),
 %if_t(Set1 \=@= Set2,

findall_vset_R(T,G,R):- findall_vset(T,G,S), vsr_set(S,R). %,reverse(R,L).
vsr_set(L,P):- flatten([L],F),vs_set(F,R),reverse(R,P).
vs_set(L,P):- variant_list_to_set(L,S),sort(S,P).
%pp_ilp_vset(G,T):- dash_chars,with_vset(G,pp_ilp(C)).
with_vset(G,C):- term_variables(C,Vs),findall_vset_R(Vs,G,L),forall(member(Vs,L),call(C)).
%:- dynamic(arc_cache:map_pairs/6).
%:- abolish(prop_dep/9).
%:- abolish(arc_cache:prop_dep/9).
%:- dynamic(arc_cache:prop_dep/9).
%:- dynamic(is_for_ilp/4).
:- abolish(ac_db_unit/4).
:- dynamic(ac_db_unit/4).
%:- dynamic(arc_cache:causes/5).

ok_intersect(L1,L2):-
  member(E1,L1),member(E2,L2),
  other_val(E1,E2),!,fail.
ok_intersect(_,_).

/*
pair_obj_props54321(TestID,Ex,Ctx,Info,Step,Type,LHS,RHS,S,L,R):- 
 ensure_test(TestID),
  trans_rules_combined(TestID,Ctx,Combined),
  r(Type,LHS,RHS,S,L,R, Ex, Step) = Combined,
  InfoOut =  Step, _IsSwapped, Ctx, Type, TestID, Ex, Ex).
*/
/*
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


points_to_objects(ShapeType, Obj, Points, IndvPoints, OutScanPoints):-
    %Points = VM.lo_points,
    %shape_min_points(VM,ShapeType,IndvPoints),
    %copy_term(ShapeType,OptionC),ShapeType=ShapeType,    
  select(C-HV, Points, Out0), allowed_color(ShapeType, C), % non_free_fg(C), % \+ is_black(C),
  allowed_dir(ShapeType, Dir), adjacent_point_allowed(C, HV, Dir, HV2), select(C-HV2, Out0, ScanPoints),
  all_individuals_near(_VM, Dir, ShapeType, C, [C-HV, C-HV2], ScanPoints, OutScanPoints, IndvPoints), %!,
  %make_indiv_object(VM,[iz(ShapeType),iz(media(shaped)),birth(i(ShapeType)),birth(i2(ShapeType))],IndvPoints,Obj),
   % meets_indiv_criteria(VM,ShapeType,IndvPoints),
  %set(VM.lo_points) = OutScanPoints,
  %assumeAdded(VM,Obj),
  %cycle_back_in(VM,OptionC).
  true,
  Obj = obj([globalpoints(IndvPoints)]).


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
  A =  ac_db_unit(TestID, IO, P1, PSame1),
  B =  ac_db_unit(TestID, IO, P2, PSame2),!,
  same_rhs_operation(P1,P2),
  merge_props(PSame1,PSame2,PSame),!,
  C =  ac_db_unit(TestID, IO, P1, PSame).

merge_vals(A,B,C):-
  A =  ac_rules(TestID, IO, P1, PSame1),
  B =  ac_rules(TestID, IO, P2, PSame2),!,
  same_rhs_operation(P1,P2),
  merge_props(PSame1,PSame2,PSame),!,
  C =  ac_rules(TestID, IO, P1, PSame).

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



good_for_rhs(P):- verbatum_unifiable(P), !.
good_for_rhs(P):- dont_notice(P),!,fail.
good_for_rhs(pen(_)).
good_for_rhs(iz(sid(_))).
good_for_rhs(mass(_)).
%good_for_rhs(loc2D(_, _)).
good_for_rhs(iz(locX(_))). good_for_rhs(iz(locY(_))).
%good_for_rhs(center2D(_, _)).
%good_for_rhs(center2G(_, _)).
good_for_rhs(iz(cenX(_))).
good_for_rhs(iz(cenY(_))).
%good_for_rhs(vis2D(_, _)).
good_for_rhs(iz(sizeX(_))).
good_for_rhs(iz(sizeY(_))).

/*
good_for_rhs(vis2D(_, _)).
good_for_rhs(center2D(_, _)).
good_for_rhs(center2G(_, _)).

*/
good_for_rhs(iz(algo_sid(norm,_))).
good_for_rhs(grid_ops(norm,_)).
good_for_rhs(grid_rep(norm,_)).
good_for_rhs(loc2D(_,_)).
good_for_rhs(rot2D(_)).

%good_for_rhs(iz(sizeGX(_))).
%good_for_rhs(iz(sizeGY(_))).

good_for_rhs(prop_of(_,_,_)).
good_for_rhs(prop_of(_,_)).
good_for_rhs(prop_of(_)).
good_for_rhs(delete(_)).
good_for_rhs(edit(_)).
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
good_for_lhs(P):- verbatum_unifiable(P), !.
good_for_lhs(cc(FG, _)):- is_real_color(FG),!.
%good_for_lhs(cc(_, _)):- !, fail.
good_for_lhs(center2D(_, _)).
good_for_lhs(empty_area(_)).
good_for_lhs(global2G(_, _)).
%good_for_lhs(grid_ops(comp, _)).
%good_for_lhs(iz(fg_or_bg(_))).
%good_for_lhs(iz(filltype(_))).
good_for_lhs(oid(_)).
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
good_for_lhs(iz(locX(_))).
good_for_lhs(iz(locY(_))).
good_for_lhs(iz(cenX(_))).
good_for_lhs(iz(cenY(_))).
good_for_lhs(iz(cenGX(_))).
good_for_lhs(iz(cenGY(_))).
good_for_lhs(P):- good_for_rhs(P),!.
good_for_lhs(P):- \+ dont_notice(P),!.
%good_for_lhs(_).


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

warn_and_fail_if_empty(Skip):- Skip\==[], !.
warn_and_fail_if_empty(Skip):-
  wdmsg(warn_and_fail_if_empty(Skip)),
  Skip\==[], !.


find_lhs(R,P):- sub_cmpd(lhs(E),R),!, into_lhs(E,P).
find_lhs(ac_unit(_Tst,_IO,_P,PConds),Out):- into_lhs(PConds,Out).
find_lhs(ac_db_unit(_Tst,_IO,_P,PConds),Out):- into_lhs(PConds,Out).
find_lhs(ac_listing(_Tst,_IO,_P,PConds),Out):- into_lhs(PConds,Out).
find_lhs(ac_rules(_Tst,_IO,_P,PConds),Out):- into_lhs(PConds,Out).
find_lhs(l2r(_Tst,P,_),Out):- find_lhs(P,Out).
find_lhs(R,R).

object_into_lhs(Obj,LHS):-
  into_lhs(Obj,LHS0),
  include(is_for_object_lhs,LHS0,LHS).
is_for_object_lhs(P):- \+ \+ good_for_lhs(P),!.

into_lhs(OID, Out):- atom(OID), !, indv_props_list(OID, In), include(is_for_object_lhs,In,Out), !.
into_lhs(In,Out):- \+ compound(In),!,Out=In.
into_lhs(R, Out):- \+ is_list(R), functor(R,F,A), arg(A,R,In),has_lhs_f(F),is_list(In), !, into_lhs(In, Out), !.
into_lhs(In,Out):- \+ is_list(In),!,Out=In.
into_lhs(In,Out):- flatten([In],InF),into_lhs1(InF,LHSF),flatten(LHSF,LHSV),variant_list_to_set(LHSV,Out),!.
into_lhs1(In,Out):- m_unifiers(In,MidF),o_unifiers(MidF,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
%into_lhs1(In,Out):- is_group(In),mapgroup(into_lhs1,In,MidF),flatten(MidF,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
%into_lhs1(In,Out):- my_exclude(hide_propchange1,In,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
%into_lhs1(In,Out):-    maplist(hide_propchange,In,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
%into_lhs1(In,Out):- remove_giz(In,Out),!.
into_lhs1(In,Out):- maplist(into_lhs,In,LHSF),flatten(LHSF,Mid),In\=@=Mid,!,into_lhs1(Mid,Out).
into_lhs1(In, Out):- include(is_for_object_lhs, In, Mid), In\=@=Mid, !, into_lhs1(Mid, Out).
into_lhs1(Out,Out).

%m_unifiers(In,Out):- \+ is_list(In),Out=In.
m_unifiers(In, Out):- In==[], !, Out=[].
m_unifiers(In,Out):- my_partition(assume_prop,In,Skip,DontSkip), Skip\==[],
  m_unifiers1(DontSkip, Mid), append_sets([Mid, Skip], Out), !.

%m_unifiers(In,Out):- is_list(In), select(E,In,More),is_prop1(E),is_unbound_prop(E),make_unifiable_u(E,U),select(U,More,UMore), 
%  min_unifier(U,E,S),!,m_unifiers([S|UMore],Out),!.
m_unifiers(In, Out):-m_unifiers1(In, Out).

m_unifiers1(In, Out):- is_list(In), select(E, In, More), is_prop1(E), make_unifiable_u(E, U), select(U, More, UMore),
  min_unifier(U, E, S), !, m_unifiers([S|UMore], Out), !, warn_and_fail_if_empty(Out).
%m_unifiers(In,Out):- select(E,In,More),is_prop1(E),make_unifiable_u(E,U),select(U,More,UMore),other_val(E,U),merge_props(U,E,S),!,m_unifiers([S|UMore],Out).
m_unifiers1(IO, IO).
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
 sub_term(E1, TransRule), propset_getter(P1), call(P1, E1), subst(TransRule, E1, gone, RuleOut),
 sub_term(E2, RuleOut), propset_getter(Q1), call(Q1, E2).


%is_grid_or_group(Grid):- is_grid(Grid), !.
%is_grid_or_group(Grid):- is_group(Grid), !.

prin_to_string(T, Text):- term_contains_ansi(T), Text=T, !.
prin_to_string(T, Text):- wots(Text, print(T)).

%:- reconsult(kaggle_arc_explaination).



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
   forall(arc_cache:trans_rule_db(TestID, ExampleNum, Ctx, Rule),
     ignore((Rule = l2r(_, _, _), retract(arc_cache:trans_rule_db(TestID, ExampleNum, Ctx, Rule))))).



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

maybe_remove_bg(RHSObjs, RHSObjs1):- my_partition(is_fg_object, RHSObjs, RHSObjs1, Out), RHSObjs1\==[], Out\==[], !.
%maybe_remove_bg(RHSObjs,RHSObjs1):- include(is_fg_object,RHSObjs,RHSObjs1),RHSObjs1\=@=RHSObjs,!.
maybe_remove_bg(RHSObjs,RHSObjs).




is_mapping_list([O|GrpL]):- is_mapping(O),is_list(GrpL),maplist(is_mapping,GrpL).
is_mapping(Grp):- fail, is_functor(l2r, Grp).

get_mapping_info(l2r(Info,In,Out),Info,In,Out).
get_mapping_info_list(GRP, Info, Dir):-
  get_mapping_info(GRP,Info,In,Out),
  into_list(In,InL),into_list(Out,OutL),!,
  append_LR(OutL, InL, DirL), !,
  must_det_ll((DirL=Dir)).


n_or_more(3,[_,_,_|_]).
n_or_more(2,[_,_|_]).
n_or_more(1,[_|_]).


/*
map_pairs_info(TestID,IO,P,Step):-
  no_repeats_var(IOP),
  map_pairs_info2(TestID,IO,P,Step),
  ground(P),
  IOP=(IO+P).

%  ((var(P),has_propcounts(TestID))->props_change2(TestID,IO,P);true),
map_pairs_info2(TestID,IO,P,_Step):- props_change2(TestID,IO,P).
map_pairs_info2(TestID,IO,P,_Step):- 
 var(P), \+ \+ ac_db_unit(TestID, IO, _, _), %!,
  ac_db_unit(TestID, IO, P, _).
map_pairs_info2(TestID,Ctx,P,Step):- 
  arc_cache:prop_dep(TestID,_ExampleNum,Ctx,Info,_InL,_OutL,_USame,_InFlatProps,OutFlatProps),
  sub_cmpd(step(Step),Info),
  member(P,OutFlatProps).
*/
/*
%pairs_of_any(LHS, RHS, PairsR):-
%  pairs_of_any(LHS, RHS, [], PairsR).




%pairs_lr(LHS, RHS, PairsLR):- maplist(best_match_rl(RHS), LHS, PairsLR).




In Prolog: I have two groups of objects where each object is denoted by `obj([center2D(2,6),mass(8),occurs_in_links(contains,1),pen([cc(blue,1)]),shape([square])])`
Each object looks at the other group of objects and keeps what its most simular to.. whenever an object from each group picks each other it forms a new pair .. remove those objects and keep going
there is no more objecs on one side any previous matches get adding back and a new set of pairs .. this goes on until there is no opbjects remaining on either side.
sometimes if there are an exact dividable number of objects on one side from the other try permutations of groups from the larger side
*/

% Predicate to find the most similar object in the other group for each object
most_similar(_, [], -1).
most_similar(Obj, [Other|Out], MostSimilar) :-
    jaccard_similarity(Obj, Other, Sim),
    most_similar(Obj, Out, MostSimilarOut),
  (Sim > MostSimilarOut -> MostSimilar = Sim ; MostSimilar = MostSimilarOut).



%?- pair_combinations([o1, o2, o3], [o4, o5, o6], Pairs).
%Pairs = [[o1, o4], [o1, o5], [o1, o6], [o2, o4], [o2, o5], [o2, o6], [o3, o4], [o3, o5], [o3, o6]].

/*
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
  best_obj_group_pair(TestID, _+A, In0, Out0),
  best_obj_group_pair(TestID, _+B, In1, Out1), A<B,
  pairs_agree_or_select(In0,In1,In01),
  pairs_agree_or_select(Out0,Out1,Out01),
  dif(C,A),dif(C,B), 
  dif(D,A),dif(D,B), dif(D,C),
  ignore((best_obj_group_pair(TestID, _+C, In2, _), pairs_agree_or_select(In01, In2, In012))),
  ignore((best_obj_group_pair(TestID, _+D, _, Out2), pairs_agree_or_select(Out01, Out2, Out012))),
  ignore(In012=In01),ignore(Out012=Out01).

*/



append_LR(PrevRules, Mappings, RulesOut):- append_LR([PrevRules, Mappings], RulesOut), !.
append_LR(PrevRules, RulesSet):- must_be_free(RulesSet), flatten([PrevRules], RulesOut), !, list_to_set(RulesOut, RulesSet).


pp_w_objs(P):- into_solid_grid_strings_3(P, [is_object=object_grid], Q), !,
  pp_ilp(Q), !.






%incr_cntx(Ctx,NewCtx):- atom(Ctx),!, atom_concat(Ctx,'_out',NewCtx).
incr_cntx(info(Ctx), info(Next)):- is_list(Ctx), !, incr_cntx(Ctx, Next).
incr_cntx(Ctx,Next):- number(Ctx),!, plus(Ctx,1,Next).
incr_cntx(Ctx,Next):- Ctx == in_out,!, Next=in_out_out.
incr_cntx(Ctx, Next):- is_list(Ctx), select(ctx(C), Ctx, Out), incr_cntx(C, CC), Next=[ctx(CC)|Out], !.
incr_cntx(W+Ctx,W+Next):- incr_cntx(Ctx,Next).
incr_cntx(Ctx, Ctx):- compound(Ctx), !.
incr_cntx(Ctx,s(Ctx)).

incr_step(info(Ctx), info(Next)):- !, incr_step(Ctx, Next).
incr_step(Ctx, Next):- is_list(Ctx), select(step(C), Ctx, Out), incr_step(C, CC), Next=[step(CC)|Out], !.
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

%select_pair(perfect,_Prev,[A],[B],A,B,[],[]):-!.
select_pair(_, _Prev, RHSObjs, LHSObjs, _Right, _Left, _RHSOut, _LHSOut):- \+ (RHSObjs\=[], LHSObjs\=[]), !, fail.



select_pair(two_way(perfect), _Prev, RHSObjs, LHSObjs, Right, Left, RHSOut, LHSOut):-
  select(Left, LHSObjs, OutLeft),
  \+ is_mapping(Left),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),
  sort_by_jaccard(Left, RHSObjsMLeft, [Right|RHSOut]),
  remove_object(OutLeft, Right, LHSOut),
  sort_by_jaccard(Right,LHSObjs,[LeftMaybe|_]))),
  LeftMaybe = Left,!.

select_pair(two_way(perfect_w_prev), PrevRules, RHSObjs, LHSObjs, Right, Left, RHSOut, LHSOut):- fail,
  select(Left, [PrevRules|LHSObjs], OutLeft),
  \+ is_mapping(Left),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),
  sort_by_jaccard(Left, RHSObjsMLeft, [Right|RHSOut]),
  remove_object(OutLeft, Right, LHSOut),
  sort_by_jaccard(Right,LHSObjs,[LeftMaybe|_]))),
  LeftMaybe = Left,!.

select_pair(two_way(perfect_combo), _PrevRules, RHSObjs, LHSObjs, Right, Left, RHSOut, LHSOut):-
  into_list(LHSObjs,LHSObjsL),variant_list_to_set(LHSObjsL,LHSObjsSet),
  in_to_ins(LHSObjsSet,2,LHSObjs_Combos),
  select(Left, LHSObjs_Combos, LHSObjs_Combos_Out),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),  
  sort_by_jaccard(Left, RHSObjsMLeft, [Right|RHSOut]),
  remove_object(LHSObjs_Combos_Out, Right, LHSOut),
  sort_by_jaccard(Right,LHSObjs_Combos,[LeftMaybe|_]))),
  LeftMaybe = Left,!.


select_pair(need_prev, PrevRules, RHSObjs, LHSObjs, Right, Left, RHSOut, LHSOut):-
  select(Left, LHSObjs, OutLeft),
  once((remove_object(RHSObjs,Left,RHSObjsMLeft),
  bonus_sort_by_jaccard(PrevRules, Left, RHSObjsMLeft, [Right|RHSOut]),
  remove_object(OutLeft, Right, LHSOut),
  bonus_sort_by_jaccard(PrevRules, Right, LHSObjs, [LeftMaybe|_]))),
  LeftMaybe = Left,!.

select_pair(from_right, PrevRules, LHSObjs, RHSObjs, Left, Right, LHSOut, RHSOut):-
  select(Left, LHSObjs, OutLeft),
  remove_object(RHSObjs,Left,RHSObjsMLeft),
  bonus_sort_by_jaccard(PrevRules, Left, RHSObjsMLeft, [Right|RHSOut]),
  remove_object(OutLeft, Right, LHSOut), !.

select_pair(from_left, PrevRules, RHSObjs, LHSObjs, Right, Left, RHSOut, LHSOut):-
  select(Left, LHSObjs, OutLeft),
  remove_object(RHSObjs,Left,RHSObjsMLeft),
  bonus_sort_by_jaccard(PrevRules, Left, RHSObjsMLeft, [Right|RHSOut]),
  remove_object(OutLeft, Right, LHSOut), !.


remove_object(RHSObjs,[Left|More],RHSObjsMI):- 
  remove_object(RHSObjs, Left, Out), !, remove_object(Out, More, RHSObjsMI).
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
cto_aa(List, AA):- is_list(List), !, maplist(cto_aa, List, AAA), atomic_list_concat(AAA, '_', AA).
cto_aa(F, AA):- compound(F), F=..List, !, maplist(cto_aa, List, AAA), atomic_list_concat(AAA, '_', AA).
cto_aa(A,AA):- format(atom(AA),'~w',[A]).

%make_pairs(InfoIn, Type, s(IsSwapped), PrevRules, LHS, RHS, GRP):- nonvar(IsSwapped), !,
%  make_pairs(InfoIn, Type, PrevRules, RHS, LHS, GRP).
%make_pairs(InfoIn, Type, PrevRules, LHS, RHS, GRP):- PrevRules\==[], !,
%  make_pairs(InfoIn, Type, [], PrevRules, LHS, NLHS),
%  make_pairs(InfoIn, Type, [], NLHS, RHS, GRP).
make_pairs(InfoIn, Type, _PrevRules, LHS, RHS, GRP):-
  Info =  [type(Type), why(TypeO)|InfoIn],
  must_det_ll((
 listify(LHS,LHSL),maplist(obj_in_or_out,LHSL,LCtx),maplist(cto_aa,LCtx,LCtxA),atomic_list_concat(LCtxA,'_',LP),
 listify(RHS,RHSL),maplist(obj_in_or_out,RHSL,RCtx),maplist(cto_aa,[Type,LP|RCtx],AA),atomic_list_concat(AA,'_',TypeO))),
   (Type==delete -> true ; (TypeO\==in_out_out_out, TypeO\=in_out_in_in)),
  
  %into_list(LHS,LLHS),
  %append_LR(PrevRules, LHS, PLHS),
  GRP = l2r(Info,LHS,RHS),
  once(pp_ilp(make_pairs=GRP)).




obj_in_or_out(Rule, IO_):- is_mapping(Rule), !,
    get_mapping_info(Rule, Info, _In, _Out), arg(3, Info, IO_).
obj_in_or_out(Obj, IO_):- must_det_ll(is_object(Obj)), has_prop(giz(g(I_O)), Obj), !, I_O=IO_.
obj_in_or_out(Obj, IO_):- has_prop(iz(i_o(I_O)), Obj), !, I_O=IO_.
%obj_in_or_out(Obj, I_O):- is_input_object(Obj)-> IO_ =out ; IO_ =in.
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

obj_to_oid_u(Obj,OID):- obj_to_oid(Obj,OID).

normal_group_form(Group,Group):-!.

:- abolish(is_why_grouped_g/4).
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

%select_group(TestID, Group, HowIO):- no_repeats(Group, select_group0(TestID, Group, HowIO)).
select_group(TestID,Group,How):- select_group0(TestID,Group,How).
select_group0(TestID,Group,How):-
  ((is_why_grouped(TestID,_,How1,Group1), % dif(Group1,Group2), 
    is_why_grouped(TestID,_,How2,Group2),
    Group1\==[], Group2\==[],
    Group1\==Group2,
    once((sub_term(E,How1),sub_var(E,How2))),
    %length(Group1,G1), length(Group2,G2), G1>G2,
  once((sub_term(E,How1),sub_var(E,How2))),
  %member(M1,Group1),member(M2,Group2),M1=M2,
  append(Group1,Group2,GroupJ), sort_safe(GroupJ,Group),
  How = [How1,How2])) 
    *-> true ; is_why_grouped(TestID,_,How,Group).

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
 forall(current_example_nums(TestID,ExampleNum),
  (from_individuated_cache(TestID, TID, GID, _, _, Objs), sub_var(ExampleNum, (TID, GID)), Objs\==[])), !.
 


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
 
ensure_individuals2(TestID):- scope_training(ExampleNum),
  forall( kaggle_arc(TestID, ExampleNum, GridIn, GridOut),
           individuate_pair(complete, GridIn, GridOut, _InC, _OutC)).
%ensure_individuals2(TestID):- ignore((ExampleNum=trn+_)),
%  print_collapsed(200, forall( kaggle_arc(TestID, ExampleNum, GridIn, GridOut),
%           individuate_pair(complete, GridIn, GridOut, _InC, _OutC))).
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
   [UseL|Out] = SharedInL,
   diff_l_r([UseL],OutL,Same1,InFlatP1,OutPFlat1),
   diff_l_r(Out, OutL, SameR, InFlatPR, OutPFlatR),
   append_vsets([Same1,SameR],Same),
   append_vsets([InFlatP1,InFlatPR],InFlatP),
   append_vsets([OutPFlat1,OutPFlatR],OutPFlat))).

append_vsets(I,O):- flatten([I],M),variant_list_to_set(M,O),!.

ignore_prop_when(ARS,P):- compound(ARS),!,functor(ARS,F,_),!,ignore_prop_when(F,P).
ignore_prop_when(adding,link(contains,_)).
ignore_prop_when(adding,occurs_in_links(contains,_)).
%ignore_prop_when(adding,pg(_,_,rankLS,_)).
ignore_prop_when(adding,pg(_,_,_,_)).
ignore_prop_when(adding,grid_rep(_,_)).
ignore_prop_when(adding,simularz(_,_)).
ignore_prop_when(removing,cc(fg,_)).
ignore_prop_when(removing,mass(_)).
ignore_prop_when(adding, P):- nonvar(P), good_for_rhs(P), !, fail.
ignore_prop_when(_, P):- assume_prop(P).
ignore_prop_when(removing, P):- ignore_prop_when(adding, P).

noteable_propdiffs(Left, Right, SameProps, LeftProps, RightProps):-!,
  how_are_different(Left, Right, _TypeOfDiffs, SetOfDiffs),
  how_are_same(Left, Right, _TypeOfSames, SetOfSames),
  maplist(arg(2),SetOfDiffs,SetOfDiffs2),
  maplist(arg(2),SetOfDiffs2,LeftProps),
  maplist(arg(3),SetOfDiffs2,RightProps),
  maplist(arg(2),SetOfSames,SetOfSames2),maplist(arg(3),SetOfSames2,SameProps),!.

noteable_propdiffs(E1, E2, S, L, R):-
  indv_eprops_list(E1, FP1), indv_eprops_list(E2, FP2),
  intersection(FP1, FP2, S, L, R),!.

noteable_propdiffs(E1, E2, Same, LHS, RHS):-
  indv_eprops_list(E1, FP1), indv_eprops_list(E2, FP2),
  intersection(FP1, FP2, S, L, R),
  include(is_for_object_lhs,S,Same),
  include(is_for_object_lhs,L,LHS),
  include(is_for_object_lhs,R,RHS),!.


noteable_propdiffs(E1, E2, Same, InFlatP, OutPFlat):-
  flat_props(E1,FP1),flat_props(E2,FP2),
  noteable_propdiffs1(FP1, FP2, Same0, InFlatP0, OutPFlat0),
  my_exclude(ignore_prop_when(removing),InFlatP0,InFlatP),
  my_exclude(ignore_prop_when(adding),OutPFlat0,OutPFlat),
  my_exclude(ignore_prop_when(sames),Same0,Same),!.

noteable_propdiffs1(PA, PB, Same, InFlatP, OutPFlat):- fail,
  remove_o_giz(PA,PA1),remove_o_giz(PB,PB1),
  %=(PA,PA1),=(PB,PB1),
  pred_intersection(propchange_unnoticable,PA1,PB1,_,Same,InFlatP,OutPFlat),!.
noteable_propdiffs1(PA, PB, Same, InFlatP, OutPFlat):-
  remove_o_giz(PA,PA1),remove_o_giz(PB,PB1),
  intersection(PA1,PB1,Same,InFlatP,OutPFlat),!.

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
number_fg_colors(Dir, Dir).

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
remove_o_giz(In,Out):- is_group(In),mapgroup(remove_o_giz,In,MidF),flatten(MidF,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
remove_o_giz(obj(In),Out):- nonvar(In),!,remove_o_giz(In,Out),!.
% % % dmiles trys this commented remove_o_giz(In, Out):- m_unifiers(In, MidF), o_unifiers(MidF, Mid), In\=@=Mid, !, remove_o_giz(Mid, Out).
remove_o_giz(In,Out):- my_exclude(hide_propchange1,In,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
remove_o_giz(In,Out):-    maplist(hide_propchange,In,Mid),In\=@=Mid,!,remove_o_giz(Mid,Out).
%remove_o_giz(In,Out):- remove_giz(In,Out),!.
remove_o_giz(Out,Out).






%is_accompany_changed_verified(TestID,IO,P,PSame):- is_accompany_changed_computed(TestID,IO,P,PSame), PSame\==[].

%is_accompany_changed_computed(TestID,IO,P,PSame):-
%   ac_rules(TestID,IO,P,PSame) *->true ; prop_can(TestID,IO,P,PSame). 
   



contains_same([],_):- !.
contains_same([E|L],P):- sub_var(E,P),!,contains_same(L,P).

/*

   

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




print_scene_change_rules(TestID):- ensure_test(TestID),
  print_scene_change_rules(print_scene_change_rules,TestID).

print_scene_change_rules(Why,TestID):- 
   print_scene_change_rules3(Why,ac_listing,TestID).

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
  (nb_current('$last_rules_printed_nodebug', PrevRules);PrevRules=[]), !,
  get_scene_change_rules(TestID,P4db,Rules),
  remove_debug_info(Rules,NoDebug),
  if_t(PrevRules =@= NoDebug, dash_chars),
 ignore((
   PrevRules \=@= NoDebug,
   length(PrevRules, PrevLenth),
   length(NoDebug, Lenth),
   nb_setval('$last_rules_printed_nodebug',NoDebug),
   banner_lines(cyan, 4),
   pp(updated(Why, PrevLenth->Lenth)),
   pp_ilp(updated(Why,P4db)=Rules),
   nop(banner_lines(cyan,4)))).

maybe_color_this(Why,Color):- sub_term(Color,Why),is_color(Color),!.
get_scene_change_rules(TestID,P4db,Rules):-
 ensure_test(TestID),
  findall_vset_R(ac_rules(TestID,IO,P,PSame),
    call(P4db,TestID,IO,P,PSame),Rules).




has_propcounts(TestID):- 
 forall(current_example_nums(TestID,ExampleNum),
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

/*

changing_props(TestID,X1,X2):- 
 ensure_test(TestID),
 findall(X1-Dir, props_change(TestID, Dir, X1), X1L),
 variant_list_to_set(X1L,X1S),
 member(X1-IO,X1S),
 member(X2-IO,X1S),
% X1@>X2,
 other_val(X1,X2). 
prop_can(TestID, IO, P, Can):-
  props_change(TestID, IO, P),
  once((prop_cant(TestID, IO, P, Cant),
  prop_can1(TestID, IO, P, Can1),
  intersection(Can1, Cant, _, Can, _))).
  %(Can == [] -> (CanL=Can1, fail) ; CanL= Can).
prop_can1(TestID, IO, P, Can):-
  props_change(TestID, IO, P),
  findall(O,
    ((enum_object_ext(O), has_prop(giz(g(out)), O), has_prop(cc(bg, 0), O),
      has_prop(P, O))), [I|L]),
  indv_props_list(I, List),
  findall(U, (member(U, List), U\=@=P, ok_notice(U), forall(member(E, L), has_prop(U, E))), Can).

prop_cant(TestID, IO, P, Set):-
  props_change(TestID, IO, P),
  findall(Cant,
    ((enum_object(O), has_prop(giz(g(out)), O), has_prop(cc(bg, 0), O),
      not_has_prop(P, O), indv_props_list(O, List), member(Cant, List), ok_notice(Cant))), Flat),
   list_to_set(Flat, Set).

enum_object_ext(O):-
  ensure_test(TestID),
  current_example_nums(TestID, ExampleNum),
  once((obj_group_io(TestID, ExampleNum, out, Objs), Objs\==[])), member(O, Objs).
find_peers_with_same(TestID, IO, P, PSame, NewSame):- select(S, PSame, Next), S=@=P, !, find_peers_with_same(TestID, IO, P, Next, NewSame).
find_peers_with_same(TestID, IO, P, PSame, NewSame):-
   sub_term(Color, P), is_real_color(Color), sub_term(N, P), number(N),
   my_partition(contains_same([Color]), PSame, SameW, SameWO), SameW\==[], SameWO\==[], !,
   find_peers_with_same(TestID, IO, P, SameWO, NewSame).
find_peers_with_same(_, _, PSame, PSame):-!.


props_change(TestID,IO,P):- map_pairs_info(TestID,IO,P,_Step),good_for_rhs(P).
props_change2(TestID,IO,P):-
% -  ensure_propcounts(TestID),
  %ensure_prop_change(E),
  findall(Q-I_or_O,counts_change(TestID,_,I_or_O,Q,_,_),L),list_to_set(L,S),!,member(P-IO,S),ok_deduce(P).
%ensure_prop_change(IO,P):- (var(P)->props_change(_TestID,IO,P);true).
*/

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


% bd14c3bf
   /*

   group the ones that change form the ones that dont
   if you cant find a good reason then



   */


merge_rules(TestID,ExampleNum, RulesList, NewRulesList):-
  identical_props(RulesList,P),
  LR = lhs, 
  each_has_more_than_one_non_assumable(LR,P,RulesList),!,
  marginalize_prop(TestID,ExampleNum,LR,P,RulesList, RulesListM),!,
  merge_rules(TestID,ExampleNum, RulesListM, NewRulesList).  

merge_rules(_TestID,ExampleNum, RulesList, RulesList):- ExampleNum \==common,!.

merge_rules(TestID,common, RulesList, NewRulesList):-
 must_det_ll((
  %unnumbervars2a(RulesList0,RulesList),
  var(NewRulesList),
  is_list(RulesList),
  gensym(newTestID, GenSym),
  forall(member(R, RulesList), assert_ac_db(Gensym, R)),
  set_of_changes(GenSym, compute_scene_change_pass3a(GenSym)),
  set_of_changes(GenSym, compute_scene_change_pass3b(GenSym, correct_antes1)),
  set_of_changes(GenSym, compute_scene_change_pass3b(GenSym, correct_antes2)),
  set_of_changes(GenSym, compute_scene_change_pass3b(GenSym, correct_antes3)),
  set_of_changes(GenSym, compute_scene_change_pass3b(GenSym, correct_antes4)),
  set_of_changes(GenSym, compute_scene_change_pass3c(GenSym)),
  set_of_changes(GenSym, compute_scene_change_pass3b(GenSym, correct_antes4a)),
  set_of_changes(GenSym, compute_scene_change_pass3b(GenSym, correct_antes4b)),
  set_of_changes(GenSym, compute_scene_change_pass3b(GenSym, correct_antes5)),
  set_of_changes(GenSym, compute_scene_change_pass3b(GenSym, correct_antes6)),
  set_of_changes(GenSym, compute_scene_change_pass3c(GenSym)))), !,
  R = ac_unit(TestID, Ctx, P, PSame),
  findall(R, retract(ac_db_unit(Gensym, Ctx, P, PSame)), NewRulesList).

assert_ac_db(Gensym, R):-
  ctx_p_conds(R, Ctx, P, PSame), assert_ilp_b(ac_db_unit(Gensym, Ctx, P, PSame)).

compute_scene_change_pass3a(RulesList, IO_-P):-
  Why = pass3a,
   findall(PSame, ac_db_unit(RulesList, IO_, P, PSame), ListF),
   append(ListF, List),
   dash_chars,
   pp_ilp(IO_-P=ListF),
   m_unifiers(List,ListR),
   warn_and_fail_if_empty(ListR),
   pred_intersection(=@=, List, ListR, _, _, Missing, New),
   pp_ilp(listR(IO_-P)=ListR),
   update_accompany_changed_db_now(Why, RulesList, IO_, P, iz(info(reduced(Why, Missing->New))), ListR).
compute_scene_change_pass3a(_,_).

compute_scene_change_pass3b(RulesList, P4, IO_-P):-
  findall_vset_R(PSame, ac_db_unit(RulesList, IO_, P, PSame), SameS1),
  Why = P4-pass3,
  my_partition(is_debug_info,SameS1,Skip,SameS),
  call(P4, RulesList, IO_, P, SameS, KeptS), warn_and_fail_if_empty(KeptS), !,
  if_t(SameS\=@=KeptS,
     (append(KeptS,Skip,Kept),
      intersection(SameS, KeptS, _, Missing, New),
      update_accompany_changed_db_now(Why, RulesList, IO_, P, iz(info(reduced(Why, Missing->New))), Kept))).
compute_scene_change_pass3b(_,_,_). 

update_accompany_changed_db_now(P4, RulesList, IO_, P, More, Kept):- More\=iz(info(reduced(_, ([]->[])))),
  append_LR([More, Kept], MoreKept),
  update_accompany_changed_db(P4, RulesList, IO_, P, MoreKept), !.
update_accompany_changed_db_now(P4, RulesList, IO_, P, _, Kept):- update_accompany_changed_db(P4, RulesList, IO_, P, Kept).

compute_scene_change_pass3c(_,_):-!.
compute_scene_change_pass3c(RulesList, IO_-P):-
  ac_db_unit(RulesList, IO_, P, PSame1),
  my_partition(is_debug_info,PSame1,Skip,PSame),
  findall(DSame,
     (ac_db_unit(RulesList, IO_, DP, DSame),
      same_rhs_property(DP,P),at_least_one_overlap(DSame,PSame)),
   SL),  SL = [_,_|_],
  common_members(SL,Commons),
  forall((ac_db_unit(RulesList, IO_, DP, DSame), same_rhs_property(DP, P)),
      (intersection(DSame,Commons,_,Kept,_),
        ignore((warn_and_fail_if_empty(Kept), append(Kept, Skip, Save), update_accompany_changed_db(pass3c, RulesList, IO_, P, Save))))),

  print_scene_change_rules_if_different(pass3c, ac_db_unit, RulesList),
  !.
compute_scene_change_pass3c(_,_).


compute_scene_change_pass4(RulesList):-
   nop(compute_scene_change_pass3(RulesList)), !.

set_of_ps(RulesList, Ps):-
  ((findall_vset_R(Ctx-P1,
    ((ac_db_unit(RulesList, IO_, P, _)
     %;ensure_props_change(RulesList, IO_, P)
     ;( pass2_rule(RulesList, IO_, P, _))),
    io_to_cntx(IO_,Ctx),into_rhs(P,P1)), Ps))).

set_of_changes(RulesList, P1):-
 ((
  set_of_ps(RulesList, Ps),  
  why_last(P1,Why),
  my_exclude(is_functor(add_dependant_scenery),Ps,PsP1),
  %findall_vset_R(IO_-P, (ac_rules(RulesList, IO_, P, _)), Ps),
  maplist(P1, PsP1),
  print_scene_change_rules_if_different(Why, ac_db_unit, RulesList))).





why_last1(A,E):- \+ compound(A),!, (atom(A);string(A)),A=E.
why_last1([H|T],E):- !, ((T\==[],why_last1(T,E));why_last1(H,E)),!.
why_last1(C,E):- compound_name_arguments(C,F,A),why_last1([F|A],E),!.
why_last(A,E):- why_last1(A,E),!.
why_last(E,E).


find_rhs(ac_unit(_Tst,_IO,P,_PConds),Out):- into_rhs(P,Out).
find_rhs(l2r(_Tst,_IO,P),Out):- into_rhs(P,Out).
find_rhs(ac_db(_Tst,_IO,P,_PConds),Out):- into_rhs(P,Out).
find_rhs(call(P), call(P)):- !.
find_rhs(ac_db_unit(_Tst,_IO,P,_PConds),Out):- into_rhs(P,Out).
find_rhs(ac_rules(_Tst,_IO,P,_PConds),Out):- into_rhs(P,Out).
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

/*
update_accompany_changed_db(TestID, IO_, P, Kept):- warn_and_fail_if_empty(Kept),
 forall(io_to_cntx(IO_, Ctx), forall(retract(ac_db_unit(TestID, Ctx, P, _)), true)),
 assert_accompany_changed_db(TestID,IO_,P,Kept).
   
assert_accompany_changed_db(_TestID,_IO_,_P,Kept):- Kept==[],!.
assert_accompany_changed_db(TestID,IO_,P,Kept):- 
  io_to_cntx(IO_,Ctx),  
   assert_ilp_b(ac_db_unit(TestID,Ctx,P,Kept)).

%assert_ilp_b(Term):- \+ clause_asserted(Term), !, pp_ilp(assert_ilp_b=Term), asserta_out(Term).
assert_ilp_b(Term):- asserta_out(Term).
%assert_ilp_b(Term):- pp_ilp(assert_ilp_b=Term), !, assert_if_out(Term).
*/

at_least_one_overlap(DSame,PSame):-
  member(DS,DSame),member(S,PSame),
  about_same_property(DS,S),!.

about_same_property(DS,S):- \+ \+ (same_rhs_property(DS,S);( \+ DS\=S )).
same_rhs_property(DS,S):- \+ \+ (DS=@=S;other_val(S,DS)).



has_a_value(P):- make_unifiable_u(P, U), P\=@=U.


/*
ac_db_unit_unv(TestID, Ctx, P, PSame):-
  ac_db_unit(TestID, Ctx, PU, PSameU),
  unnumbervars2a(PU+PSameU, P+PSame).
*/


% Retain Overlap
correct_antes1(RulesList, IO_, P, PSame, SL):-
  %make_unifiable_u(P, DP),
  %rev_in_out_atoms(OI,IO_),
  findall(S,
   (member(S,PSame),
     \+ \+ (verbatum_unifiable(S); negated_s_lit(S, _); sub_var(S, P) ; ((
       forall((ac_rules(RulesList, IO_, DP, DSame), %same_rhs_property(P, DP),
          at_least_one_overlap(DSame, PSame)),
          ((P==DP)-> true;
            (make_unifiable_u(S, DS), member(DS, DSame), other_val(S, DS))
            )))))),
   SL), warn_and_fail_if_empty(SL), !.
correct_antes1(_RulesList, _IO_, _P, PSame, PSame).

is_unbound_prop(S):- var(S), !, fail.
is_unbound_prop(S):- verbatum_unifiable(S), !, fail.
is_unbound_prop(S):- make_unifiable(S,DS), S=@=DS,!.


correct_antes2a(TestID, IN_OUT, P, PSame, Kept):-   fail,
   make_unifiable_u(P, U),
   ac_rules(TestID, IN_OUT, U, DSame),
   P\=@=U,
   maplist(make_unifiable_u, DSame, USame),
   intersection(PSame, USame, Kept, _, _), warn_and_fail_if_empty(Kept).
correct_antes2a(_TestID, _IN_OUT, _P, PSame, PSame).

% Make sure each arguement is transformed corretly
correct_antes2(_RulesList, _IO_, _P, PSame, Kept):-  maplist(ensure_xformed, PSame, Kept), !.
ensure_xformed(pg(_,A,B,C),pg(_,A,B,C)):-!.
ensure_xformed(A,A).

% Remove Duped Simularz
correct_antes3(RulesList, IO_, P, PSame, SLO):-
  select(simularz(N, V1), PSame, PSame1),
  member(simularz(N, V2), PSame1),
  V1\==V2,
  findall(S, ( member(S, PSame1), \+ (S = simularz(N, _))), SL),
  correct_antes3(RulesList, IO_, P, SL, SLO).
correct_antes3(_RulesList, _IO_, _P, PSame, PSame).
% Remove Unbound Props
correct_antes3u(_RulesList, _IO_, _P, PSame, SL):- fail,
  findall(S, ( member(S, PSame), \+ is_unbound_prop(S)), SL), warn_and_fail_if_empty(SL), !.
correct_antes3u(_RulesList, _IO_, _P, PSame, PSame).


% Remove Redundant Overlap
correct_antes4(RulesList, IO_, P, PSame, SL):-
  findall(S,
   ( member(S,PSame), 
     (negated_s_lit(S,_)->true;
      \+ ((  
       forall((ac_rules(RulesList, IO_, DP, DSame),
              same_rhs_property(P,DP)),          
            (member(DS,DSame), DS=@=S)))))),
   SL), 

  warn_and_fail_if_empty(SL), !.
correct_antes4(_RulesList, _IO_, _P, PSame, PSame).





% Remove Redundant Overlap
correct_antes4a(RulesList, IO_, VP, PSame, SLPSame):- fail,
  %rev_in_out_atoms(OI,IO_),
  ensure_deref_value(VP,P),
  \+ \+ ((ac_rules(RulesList, IO_, DP, _), other_val(P, DP))),
  findall(mv4a(info(changes_from(S,P))),
       ((member(S,PSame), \+ negated_s_lit(S,_), S\= mv4a(info(_)),
         other_val(S,P))),
     SL), warn_and_fail_if_empty(SL), !,
  append(PSame,SL,SLPSame).

correct_antes4a(_RulesList, _IO_, _P, PSame, PSame).



% Remove Single Chhangers
correct_antes4b(RulesList, IO_, VP, PSame, SLPSame):-  fail,
  %rev_in_out_atoms(OI,IO_),
  ensure_deref_value(VP,P),
  \+ \+ ((ac_rules(RulesList, IO_, DP, _), other_val(P, DP))),
  findall(mv4b(info(changes_into(S,P))),
       ((member(S,PSame), \+ negated_s_lit(S,_), S\= mv4b(info(_)), other_val(S,P),
         forall((ac_rules(RulesList, IO_, DP, DSame), other_val(P, DP)),
           \+ \+ (member(DS,DSame), other_val(S,DS), \+ negated_s_lit(DS,_))))), 
     SL), warn_and_fail_if_empty(SL), !,
  append(PSame,SL,SLPSame).
correct_antes4b(_RulesList, _IO_, _P, PSame, PSame).



% Add Negations
correct_antes5(RulesList, IO_, P, PSame, Kept):- correct_antes_neg(RulesList, IO_, P, PSame, Kept), !.
correct_antes5(_RulesList, _IO_, _P, PSame, Kept):- vsr_set(PSame, Kept), !.
correct_antes5(_RulesList, _IO_, _P, PSame, PSame).
correct_antes_neg(RulesList, IO_, P, PSame, Kept):-
  findall( ( \+ DS),
   ((member(S,PSame), \+ negated_s_lit(S,_), is_unbound_prop(S), make_unifiable(S,DS),
     ac_rules(RulesList, IO_, DP, DSame),
     other_val(P,DP), %at_least_one_overlap(DSame,PSame),
     member(DS,DSame), \+ negated_s_lit(DS,_), \+ is_unbound_prop(DS), 
       \+ member(\+ DS, PSame))), SL),
  append(PSame, SL, Kept),
  warn_and_fail_if_empty(Kept), !.
correct_antes_neg(_RulesList, _IO_, _P, PSame, PSame).


% DISABLED not really a loops
correct_antes6(_RulesList, _IO_, P, PSame, Kept):- fail,
  findall(S, (member(S, PSame), \+ same_rhs_property(P, S)), Kept), warn_and_fail_if_empty(Kept), !.
correct_antes6(_RulesList, _IO_, _P, PSame, PSame).


negated_s_lit(N,P):- compound(N), N = ( \+ P ). 
how_are_different(O1, O2, PropSet):-
  how_are_different(O1, O2, _TypeSet, PropSet).
how_are_different(O1, O2, TypeSet, PropSet):-
  findall(Type=Same, (prop_pairs2(O1, O2, Type, Same, _P), Same\=same(_,_,_), Type\==reorder), List),
  maplist(arg(1), List, TypeL), vsr_set(TypeL, TypeSet),
  vsr_set(List, PropSet).

how_are_same(O1, O2, PropSet):-
  how_are_same(O1, O2, _TypeSet, PropSet).
how_are_same(O1, O2, TypeSet, PropSet):-
  findall(Type=Same, (prop_pairs2(O1, O2, Type, Same, _P), Same\=different(_,_,_), Type\==reorder), List),
  maplist(arg(1), List, TypeL), vsr_set(TypeL, TypeSet),
  vsr_set(List, PropSet).

how_are_close(O1, O2, PropSet):-
  how_are_close(O1, O2, _TypeSet, PropSet).
how_are_close(O1, O2, TypeSet, PropSet):-
  findall(Type=Close, (prop_pairs2(O1, O2, Type, Close, _P), true, Type\==reorder), List),
  maplist(arg(1), List, TypeL), vsr_set(TypeL, TypeSet),
  vsr_set(List, PropSet).


prop_pairs(O1, O2, Type, Same, P):- prop_pairs2(O1, O2, Type, Same, P).

prop_pairs2(O1, O2, Type, Change, P):-
  flat_props(O1, F1), flat_props(O2, F2), !,
  %intersection(FF1, FF2, _, F1, F2),
  prop_type(P1, Type), member(P1, F1), make_unifiable_u(P1, P2),
 (once((member(P2, F2), 
       (other_val(P1, P2)->Change=different(P, P1, P2);Change=same(P, P1, P2))))->
   min_unifier(P1, P2, P); ((fail,Change=adding(P), P=P1))),
 nop((\+ ignore_prop_when(Change, P))).

narrative_element_once(Ele, ActionGroupIn, ActionGroupOut):-
  ActionGroupIn = [Ele|ActionGroupOut], !.

narrative_element(Ele, ActionGroupIn, ActionGroupOut):-
  ActionGroupIn = [EleIn|ActionGroup],
   EleIn=..[F|ArgsIn], append(LArgs, [Min..Max, Was], ArgsIn),
   Ele=..[F|Args], append(LArgs, [Min..Max, WasOut], Args),
   ((number(Was), number(Max))-> (Max>Was) ; true),
    (number(Was)-> WasOut is Was +1 ; WasOut=Was),
   (Max==WasOut
      -> ActionGroupOut= ActionGroup
      ; must_det_ll((
       append(LArgs, [Min..Max, WasOut], ArgsOut), EleOut =.. [F|ArgsOut],
      ActionGroupOut = [EleOut|ActionGroup]))).




clear_scene_rules(TestID):-
  abolish(ac_db_unit/4), dynamic(ac_db_unit/4),
  abolish(arc_cache:trans_rule_db/4), dynamic(arc_cache:trans_rule_db/4),

  forall(ac_db_unit(TestID, Ctx, P, PSame), ignore(retract(ac_db_unit(TestID, Ctx, P, PSame)))),
  forall(arc_cache:trans_rule_db(TestID, ExampleNum, Ctx, Rule),
     ignore((/*Rule \= l2r(_, _, _), */retract(arc_cache:trans_rule_db(TestID, ExampleNum, Ctx, Rule))))).

out_is_one_in:- arc_common_property(rev(comp(cbg(black),o-i,X))),!,X=ogs.

calc_o_d_recursive_end(Ending, InfoIn, PrevRules, MLHSObjs, RHSObjs,
                    InfoOut, RulesOut):- out_is_one_in,!,
    
  my_partition(is_mapping, MLHSObjs, _Mappings, LHSObjsFGBG),
  my_partition(is_bg_object, LHSObjsFGBG, _LBGObjs, LFGObjs),
  length(LFGObjs, N),
  RHSObjs == [], LFGObjs\==[], !, member_of_relax(ending(Ending), InfoIn), !, Ending = leftover(delete(N)),
  InfoOut =  [type(ending(Ending))|InfoIn], sub_cmpd(testid(TestID), InfoIn), sub_cmpd(example(ExampleNum), InfoIn),
  append_LR(PrevRules, [l2r(Info, LFGObjs, []), l2r(Info, [], []),
  call(assert_test_property(TestID, ExampleNum, ending, ending(Ending)))], RulesOut).

% HAPPY ENDINGS
calc_o_d_recursive_end(Ending, InfoIn, PrevRules, MLHSObjs, RHSObjs,
                    InfoOut, RulesOut):-

  my_partition(is_mapping, MLHSObjs, _Mappings, LHSObjs),
  RHSObjs == [], LHSObjs==[], !,
  member_of_relax(ending(Ending), InfoIn), Ending = balanced(perfect),
  must_det_ll((
  InfoOut =  [type(ending(Ending))|InfoIn],
  sub_cmpd(testid(TestID), InfoIn), sub_cmpd(example(ExampleNum), InfoIn),
  append_LR(PrevRules,
     [l2r(InfoOut, [], []), call(assert_test_property(TestID, ExampleNum, ending, ending(Ending)))],
     RulesOut))).


calc_o_d_recursive_end(Ending, InfoIn, PrevRules, MLHSObjs, RHSObjs,
                    InfoOut, RulesOut):-

  my_partition(is_mapping, MLHSObjs, _Mappings, LHSObjsFGBG),
  my_partition(is_bg_object, LHSObjsFGBG, LBGObjs, LFGObjs),
  RHSObjs == [], LFGObjs==[], LBGObjs\==[], !, member_of_relax(ending(Ending), InfoIn), Ending = balanced(with_bg_l),
  InfoOut =  [type(ending(Ending))|InfoIn], sub_cmpd(testid(TestID), InfoIn), sub_cmpd(example(ExampleNum), InfoIn),
  append_LR(PrevRules, [l2r(Info, LBGObjs, []), l2r(Info, [], []), call(assert_test_property(TestID, ExampleNum, ending, ending(Ending)))], RulesOut).

calc_o_d_recursive_end(Ending, InfoIn, PrevRules, LHSObjs, MRHSObjs,
                   InfoOut, RulesOut):-

  my_partition(is_bg_object, MRHSObjs, RBGObjs, RFGObjs),
  RFGObjs == [], LHSObjs==[], !, member_of_relax(ending(Ending), InfoIn), Ending = balanced(with_bg_r),
  InfoOut =  [type(ending(Ending))|InfoIn], sub_cmpd(testid(TestID), InfoIn), sub_cmpd(example(ExampleNum), InfoIn),
  append_LR(PrevRules, [l2r(Info, [], RBGObjs), l2r(Info, [], []), call(assert_test_property(TestID, ExampleNum, ending, ending(Ending)))], RulesOut).

calc_o_d_recursive_end(Ending, InfoIn, PrevRules, MLHSObjs, MRHSObjs,
                     InfoOut, RulesOut):-

  my_partition(is_bg_object, MRHSObjs, RBGObjs, RFGObjs), my_partition(is_bg_object, MLHSObjs, LBGObjs, LFGObjs),
  RFGObjs == [], LFGObjs==[], !, member_of_relax(ending(Ending), InfoIn), Ending = balanced(with_bg_l_r),
  InfoOut =  [type(ending(Ending))|InfoIn], sub_cmpd(testid(TestID), InfoIn), sub_cmpd(example(ExampleNum), InfoIn),
  append_LR(PrevRules, [l2r(Info, LBGObjs, []), l2r(Info, [], RBGObjs), call(assert_test_property(TestID, ExampleNum, ending, ending(Ending)))], RulesOut).

calc_o_d_recursive_end(Ending, InfoIn, PrevRules, LHSObjs, RHSObjs,
                     InfoOut, RulesOut):-

  LHSObjs==[], RHSObjs == [], !,
  member_of_relax(ending(Ending), InfoIn), Ending = balanced(perfect_balance),
  must_det_ll((
  sub_cmpd(testid(TestID), InfoIn), sub_cmpd(example(ExampleNum), InfoIn),
  InfoOut =  [type(ending(Ending))|InfoIn],
  append_LR(PrevRules, [call(assert_test_property(TestID, ExampleNum, ending, ending(Ending)))], RulesOut))), !.

calc_o_d_recursive_end(Ending, InfoIn, PrevRules, MLHSObjs, RHSObjs,
                   InfoOut, RulesOut):-

  RHSObjs==[], !,
  my_partition(is_mapping, MLHSObjs, _Mappings, LHSObjs),
  member_of_relax(ending(Ending), InfoIn), Ending = ending(left_over),
  must_det_ll((
  InfoOut =  [type(ending(Ending)), why(Ending)|InfoIn], sub_cmpd(testid(TestID), InfoIn), sub_cmpd(example(ExampleNum), InfoIn),
    must_det_ll((maplist(into_delete(TestID, ExampleNum, PrevRules, InfoOut), LHSObjs, Mappings),
    append_LR(PrevRules, [call(assert_test_property(TestID, ExampleNum, ending, ending(Ending))), Mappings], RulesOut))))), !.

fg_to_bgc(FG, black):- is_fg_color(FG), !.
fg_to_bgc(FG, FG):- \+ compound(FG), !.

into_delete(_TestID, _Ctx, _Prev, _Info, Obj, Obj):- is_mapping(Obj), !.
%into_delete(_TestID, _Ctx, _Prev, _Info, Obj, Obj):-!.
into_delete(_TestID, _Ctx, PrevRules, Info, Obj, Rules):- map_pred(fg_to_bgc, Obj, NewObj),
  make_pairs(Info, delete, PrevRules, Obj, NewObj, Rules),
  !. %edit_object(pen([cc(black, 1)]))  % l2r(Info, [Obj], [])).


/*
correct_antes5(TestID,IO_,P,PSame,Kept):-   
   make_unifiable_u(P,U),
   is_accompany_changed_computed(TestID,IO_,U,DSame),
   P\=@=U,
   maplist(make_unifiable_u,DSame,USame),
   pred_intersection(other_val, PSame, USame, Kept, _, _, _), warn_and_fail_if_empty(Kept).
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
  
%:- reconsult(kaggle_arc_explaination).

