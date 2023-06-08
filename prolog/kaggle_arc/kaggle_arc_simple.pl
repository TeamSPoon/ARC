/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

:- include(kaggle_arc_header).

:- ensure_loaded(kaggle_arc_grid_size).


:- use_module(library(clpfd)).




vm_opts_some(VM,Shapes,NF,Mono,Nsew,IncludeBG):-
  SegOptions = vm_opts(Shapes,NF,Mono,Nsew,IncludeBG),
  GPoints = VM.lo_points,
  filter_points(SegOptions,GPoints,Points),
  once(color_masses(VM.h,VM.v,VM.start_points,Points,SegOptions,Segs)),
  maplist(make_indiv_object(VM,[iz(birth(SegOptions))]),Segs,Objs),
  assumeAdded(VM,Objs),!.

vm_opts(VM,Shapes,NF,Mono,Nsew,IncludeBG):-
  vm_opts_some(VM,Shapes,NF,Mono,Nsew,IncludeBG),
  run_fti(VM,[do_ending]),
  post_individuate_8(VM,IndvS),
  gset(VM.objs) = IndvS.

seg_options(Opt):-
 member(Opt,
     [vm_opts(no_shapes,NF,mono,diag_ok,false),
      vm_opts(no_shapes,NF,mono,nsew,false),
      vm_opts(no_shapes,NF,color,nsew,false),      
      vm_opts(no_shapes,NF,color,diag_ok,false),
      vm_opts(no_shapes,NF,color,nsew,true),
      %vm_opts(no_shapes,false,color,diag_ok,true),
      vm_opts(no_shapes,NF,color,points,false),    
      vm_opts(no_shapes,false,mono,diag_ok,true),
      vm_opts(no_shapes,false,mono,nsew,true),
      vm_opts(no_shapes,false,color,points,true)]),dif(NF,false).
filter_points(vm_opts(no_shapes,_,_Mono,_Diag,true),Points,Points).
filter_points(vm_opts(no_shapes,_,_Mono,_Diag,false),GPoints,Points):- my_partition(is_fg_point,GPoints,Points,_).
dir_ok(_,vm_opts(no_shapes,_,_,points,_)):-!,fail.
dir_ok(D,vm_opts(no_shapes,_,_,nsew,_)):-!, n_s_e_w(D).
dir_ok(D,vm_opts(no_shapes,_,_,diag_ok,_)):-!, (n_s_e_w(D);is_diag(D)).
dir_ok(_,vm_opts(no_shapes,_,_,any,_)).
colors_joinable(vm_opts(no_shapes,_,mono,_,_),C1,C2):- is_fg_color(C1),is_fg_color(C2).
colors_joinable(vm_opts(no_shapes,_,color,_,_),C1,C1):- is_fg_color(C1).
colors_joinable(vm_opts(no_shapes,_,_,_,true),C1,C1):- is_bg_color(C1).
seg_options(OptsIn, OptsIn):- seg_options(OptsIn).
seg_options(OptsIn,OptsOut):- seg_options(OptsIn),seg_options(OptsOut), OptsIn\=@=OptsOut.
seg_options(Equation,OptsIn,OptsOut):-seg_options(OptsIn,OptsOut), arg(1,OptsIn,Equation).
ensure_objscount_equation(equals).
ensure_objscount_equation(input_times_n(_)).
ensure_objscount_equation(input_div_n(_)).
ensure_objscount_equation(input_minus_n(_)).
ensure_objscount_equation(input_plus_n(_)).
ensure_objscount_equation(becomes(_)).
ensure_objscount_equation(false).
objscount_equation(equals,ObjsCountIn,ObjsCountOut):- ObjsCountIn=ObjsCountOut,ObjsCountIn>=2.
objscount_equation(input_times_n(N),ObjsCountIn,ObjsCountOut):- ObjsCountIn * N #= ObjsCountOut, N #>=2.
objscount_equation(input_div_n(N),ObjsCountIn,ObjsCountOut):- ObjsCountIn #= N * ObjsCountOut, N #>=2.
objscount_equation(input_minus_n(N),ObjsCountIn,ObjsCountOut):- ObjsCountIn #= N + ObjsCountOut, N #>=2.
objscount_equation(input_plus_n(N),ObjsCountIn,ObjsCountOut):- ObjsCountIn + N #= ObjsCountOut, N #>=2.
objscount_equation(becomes(2),_ObjsCountIn,2).
objscount_equation(false,_,_).

segs_overlap(TestID,FAO):-
 ensure_test(TestID),
 seg_options(OptsIn,OptsOut),
 findall(oc(ex_opts_in_out(ExampleNum,ObjsCount,OptsIn,OptsOut),Overlap),
  (kaggle_arc(TestID,ExampleNum,In,Out),
    co_segs_in_out(In,Out,OptsIn,OptsOut,ObjsCount,Overlap)), FAO),
 pp(fao=FAO).


pairs_of_common_seg(TestID,IO,OptsIn,Simularity):-
 ensure_test(TestID),
  member(IO,[in,out]),
 freeze(OptsIn,(vm_opts(no_shapes,_,_,points,_)\=OptsIn)),  
  findall(Simularity-seg_options(OptsIn), 
     (seg_options(OptsIn),pairs_of_common_seg_1(TestID,IO,OptsIn,Simularity)), SD),
 sort(SD,SSD), nop(print_tt(TestID,IO,textured_points_of,SSD)),
 member(Simularity-seg_options(OptsIn),SSD).

print_tt(TestID,IO,P4,SSD):- fail,
  !,
  append([S1-seg_options(OptsInA)|_],[S2-seg_options(OptsInB)],SSD),
  pairs_of_common_seg_3(P4,TestID,IO,OptsInA,InSegsA1,InSegsA2), print_ss(wqs(low(S1,OptsInA)),InSegsA1,InSegsA2),
  pairs_of_common_seg_3(P4,TestID,IO,OptsInB,InSegsB1,InSegsB2), print_ss(wqs(high(S2,OptsInB)),InSegsB1,InSegsB2),!.

print_tt(TestID,IO,P4,SSD):-
  maplist(print_ttt(TestID,IO,P4),SSD).

print_ttt(TestID,IO,P4,S1-seg_options(OptsInA)):-
  pairs_of_common_seg_3(P4,TestID,IO,OptsInA,InSegsA1,InSegsA2), 
    print_ss(wqs(each(S1,OptsInA)),InSegsA1,InSegsA2).


deduce_individuator(TestID):- 
 ensure_test(TestID),
 arc_test_property(TestID, common, indiv_how(in), OptsIn),
 arc_test_property(TestID, common, indiv_how(out), OptsOut),!,
 show_individuators(TestID,OptsIn,OptsOut),!.

deduce_individuator(TestID):- 
 ensure_test(TestID), 
 time(guess_individuator(TestID,OptsIn,OptsOut)),
 assert_test_property(TestID, common, indiv_how(in), OptsIn),
 assert_test_property(TestID, common, indiv_how(out), OptsOut),!,
 show_individuators(TestID,OptsIn,OptsOut).
deduce_individuator(TestID):-deduce_individuator2(TestID).


show_individuators(TestID,OptsIn,OptsOut):-
 forall(kaggle_arc(TestID,ExampleNum,In,Out),
     with_individuated_cache(true,once(
      (individuate_3(OptsIn,In,InC),
       individuate_3(OptsOut,Out,OutC),
       length(InC,CI),length(OutC,CO),
       print_ss(wqs([TestID,ExampleNum,in_out(OptsIn,OptsOut,CI,CO)]),InC,OutC))))).

guess_individuator(TestID,OptsIn,OptsOut):- 
  seg_options(ObjsCountEquation,OptsIn,OptsOut), 
  ensure_objscount_equation(ObjsCountEquation),
  ((kaggle_arc(TestID,trn+N,In0,Out0),co_segs_in_out(In0,Out0,OptsIn,OptsOut,ObjsCountEquation,_Overlap))),
  forall((kaggle_arc(TestID,trn+M,In,Out),N\=M), 
     co_segs_in_out(In,Out,OptsIn,OptsOut,ObjsCountEquation,_Overlap2)),
  nop((forall(kaggle_arc(TestID,ExampleNum,In,Out),
     once(
      (textured_points_of(In,OptsIn,CI,PointsIn),
       textured_points_of(Out,OptsOut,CO,PointsOut),
       print_ss(wqs([TestID,ExampleNum,in_out(ObjsCountEquation,OptsIn,OptsOut,CI,CO)]),PointsIn,PointsOut)))))).


deduce_individuator2(TestID):- 
 ensure_test(TestID),
  pairs_of_common_seg(TestID,in,OptsIn,SI),
  pairs_of_common_seg(TestID,out,OptsOut,SO),
  forall(kaggle_arc(TestID,ExampleNum,In,Out),
  once(
   (textured_points_of(In,OptsIn,CI,PointsIn),
    textured_points_of(Out,OptsOut,CO,PointsOut),
    print_ss(wqs([TestID,ExampleNum,in_out(SI-OptsIn,SO-OptsOut,CI,CO)]),PointsIn,PointsOut)))).



 
pairs_of_common_seg_1(TestID,IO,OptsIn,Simularity):-
 ensure_test(TestID),
  member(IO,[in,out]),
 findall(Simularity, pairs_of_common_seg_2(TestID,IO,OptsIn,Simularity), SD),
 sumlist(SD,Simularity).
  
pairs_of_common_seg_2(TestID,IO,OptsIn,Simularity):-  
  pairs_of_common_seg_3(co_segs,TestID,IO,OptsIn,InSegs1,InSegs2),
  iu_simularity(InSegs1,InSegs2,Simularity).


pairs_of_common_seg_3(P4,TestID,IO,OptsIn,InSegs1,InSegs2):-
  seg_options(OptsIn),
  kaggle_arc_io(TestID,ExampleNum1,IO,In1),
  kaggle_arc_io(TestID,ExampleNum2,IO,In2),
  ExampleNum1@<ExampleNum2,
  call(P4,In1, OptsIn, _, InSegs1),
  call(P4,In2, OptsIn, _, InSegs2).

iu_simularity(Set1,Set2,Simularity):-
    once((intersection(Set1, Set2, Intersection),
    union(Set1, Set2, Union),
    length(Intersection, IntersectionLen),
    length(Union, UnionLength),
    Simularity is IntersectionLen / UnionLength)).

co_segs_in_out(In,Out,OptsIn,OptsOut,ObjsCountEquation,Overlap):-
  seg_options(OptsIn,OptsOut),
  co_segs(In, OptsIn,  ObjsCountIn,  InSegs), 
  co_segs(Out,OptsOut, ObjsCountOut, OutSegs),
  objscount_equation(ObjsCountEquation,ObjsCountIn,ObjsCountOut),
  intersection(InSegs,OutSegs,Overlap).

co_segs(Grid,OptsIn,ObjsCount,Out):- 
   sobjs_of(Grid,OptsIn,ObjsCount,Objs),
   sort(Objs,SObjs),maplist(arg(1),SObjs,Props),flatten(Props,Segs),
   variant_list_to_set(Segs,SegsSet),
   NObjsCount is - ObjsCount,
   length(Segs,SegsLen),length(SegsSet,SegsSetLen),
   Dist is (SegsLen/SegsSetLen*100)/ObjsCount, Out = [obj_count(NObjsCount),distribution(Dist)|Segs].

sobjs_of(Grid,SegOptions,ObjsCount,Objs):-
  seg_options(SegOptions),
  segs_of(Grid,SegOptions,ObjsCount,Segs),
  maplist(into_sobjs,Segs,Objs).

textured_points_of(Grid,SegOptions,ObjsCount,Points):-
  seg_options(SegOptions),
  segs_of(Grid,SegOptions,ObjsCount,Segs),
  segs_into_points(Segs,Points).

segs_into_points(Segs,Points):-
   segs_into_n_points(1,Segs,Points).
 
segs_into_n_points(N,[S|Segs],PPoints):-
   segs_into_plist(N,S,P), M is N+1,
   segs_into_n_points(M,Segs,Points),
   append(P,Points,PPoints).
segs_into_n_points(_,[],[]).

segs_into_plist(N,S,P):- M is N+300, int2glyph(M,G), maplist(segs_into_p(G),S,P).
segs_into_p(N,C-P,N-C-P):-!. segs_into_p(N,P,N-P).
 
segs_of(In,SegOptions,ObjsCount,Objs):- 
 seg_options(SegOptions),
 (var(In)->into_grid(_,In);true),
  vis2D(In,H,V),
  once(globalpoints(In,GPoints)),
  filter_points(SegOptions,GPoints,Points),
  once(color_masses(H,V,GPoints,Points,SegOptions,Objs)),
  length(Objs,ObjsCount).



into_sobjs(Points,[mc(Mass,Empty),center2D(CX,CY),vis2D(SX,SY),loc2D(Xmin,Ymin),color_cc(CC),globalpoints(Points)]):-
  length(Points,Mass),  
  maplist(point_to_hvc,Points,XX,YY,Pixels),
  pixels_to_cc(Pixels,CC),
  sumlist(XX,TX),
  sumlist(YY,TY),
  CX is floor(TX/Mass)+1,
  CY is floor(TY/Mass)+1,
  min_max(XX,Xmin,Xmax),
  min_max(YY,Ymin,Ymax),
  SX is Xmax - Xmin+1,
  SY is Ymax - Ymin+1,
  Area is SX * SY,
  Empty is Area-Mass.


min_max([H|T], Min, Max) :- min_max(T, H, H, Min, Max).

min_max([], Min, Max, Min, Max).
min_max([H|T], MinSoFar, MaxSoFar, Min, Max) :-
    NewMin is min(H, MinSoFar),
    NewMax is max(H, MaxSoFar),
    min_max(T, NewMin, NewMax, Min, Max).
  

color_masses(_H,_V,_Orig,Points,_SegOptions,[]):- Points==[],!.

color_masses(H,V,Orig,Points,SegOptions,[GPoints|More]):-   
  select(C1-HV1,Points,Points2),
  is_adjacent_point(HV1,Dir1,HV2),Dir1\==c,
  select(C2-HV2,Points2,PointsRest),colors_joinable(SegOptions,C1,C2),
  dir_ok(Dir1,SegOptions),color_dir_kk(Dir1,C1), % PointsFrom,ScanPoints,NextScanPoints,IndvPoints
  all_points_near(Orig,SegOptions,[C1-HV1,C2-HV2],PointsRest,LeftOver,GPoints),!,
  color_masses(H,V,Orig,LeftOver,SegOptions,More).

color_masses(H,V,Orig,Points,SegOptions,[GPoints|More]):-   
  select(C1-HV1,Points,PointsRest), 
  all_points_near(Orig,SegOptions,[C1-HV1],PointsRest,LeftOver,GPoints),
  color_masses(H,V,Orig,LeftOver,SegOptions,More).
color_masses(H,V,Orig,[P|PointsRest],SegOptions,[[P]|More]):-
  color_masses(H,V,Orig,PointsRest,SegOptions,  More),!.

all_points_near(_Orig,_SegOptions,NewSet,[],        [],         NewSet):-!.
all_points_near( Orig,SegOptions,Indv, ScanPoints,NewScanPoints,NewSet):-
   points_near(SegOptions,Indv,ScanPoints,New,NextScanPoints),
   (New == [] -> (NewSet = Indv, NewScanPoints = NextScanPoints)
    ; (append(Indv,New,IndvNew),
        all_points_near(Orig,SegOptions,IndvNew,NextScanPoints,NewScanPoints,NewSet))),!.

points_near(_SegOptions,_From,[],[],[]):-!.
points_near(SegOptions,From,[E|ScanPoints],[E|Nears],NextScanPoints):- 
  nearby_one(SegOptions,E,From),
  points_near(SegOptions,[E|From],ScanPoints,Nears,NextScanPoints).
points_near(SegOptions,From,[E|ScanPoints],Nears,[E|NextScanPoints]):- 
    points_near(SegOptions,From,ScanPoints,Nears,NextScanPoints).

nearby_one(SegOptions,C1-E1,List):- is_adjacent_point(E1,Dir,E2),Dir\==c,
  dir_ok(Dir,SegOptions),color_dir_kk(Dir,C1),
  member(C2-E2,List),colors_joinable(SegOptions,C1,C2).

color_dir_kk(_  ,Color):- is_fg_color(Color),!.
color_dir_kk(Dir,Color):- is_bg_color(Color),!, \+ is_diag(Dir).
 




solve_easy:- get_current_test(Name),solve_easy(Name).

solve_easy(Name):- 
  fix_test_name(Name,TestID,ExampleNum),
  ignore(some_current_example_num(ExampleNum)),
  forall(kaggle_arc(TestID,ExampleNum,In,Out),try_easy_io(TestID>ExampleNum,In,Out)),
  ((ExampleNum\=tst+_)-> 
    forall(kaggle_arc(TestID,tst+N,In,Out),try_easy_io(TestID>tst+N,In,Out))).

try_something_easy(rot180).
try_something_easy(=).
try_something_easy(run_dsl(E)):- fail, test_info(_,human(E)).

maybe_try_something_easy(I,M,P2):-  try_something_easy(P2), call(P2,I,M).
maybe_try_something_easy(I,M,Did):- fail_over_time(4,try_something(Did,I,M),fail),!.

try_easy_io(Name,I,O):-
 ignore((
  Template = try_something(W,Did,I,M,SS),
  findall(Template,
    (wots(SS,arc_weto(maybe_try_something_easy(I,M,Did))),count_changes(M,O,1,W),(W==1->!;true)),
     List),
  sort_safe(List,[Template|_]),
  %ignore((call(P2,I,II),call(P2,O,OO),
  %reduce_grid(GridIn+GridOut,IOps,II+OO),!,
  (W==1 -> Color=green; Color = yellow),
  must_det_ll(print_side_by_side(Color,M,easyResult(Name,Did),_,O,easyExpected(Name=W))))), !. 



grid_w_obj(Grid,Why,Objs):-
  (var(Grid)->arc_grid(Grid);true),
  ROptions = complete,
  individuate_3(ROptions,Grid,_IndvS),  
  why_grouped(Why,GS),
  member(Objs,GS).

grid_grouped(Grid,Why,Objs):- 
 (var(Grid)->arc_grid(Grid);true),
  ROptions = complete,
  individuate_3(ROptions,Grid,IndvS),
  regroups(IndvS,Why,Objs).

group_same_props(IndvS0,Ps):-  guard_invs(IndvS0,IndvS),
  group_props(IndvS,PropsSet),
  findall(Prop,(member(Prop,PropsSet),my_maplist(has_prop(Prop),IndvS)),Ps).


group_same_props(IndvS0,P1N,GsOO):-  guard_invs(IndvS0,IndvS),
   findall(Have-Prop,(group_same_prop(IndvS,Prop,Have,HN),HN\==[],length(Have,HH),call(P1N,HH)),Gs),
   sort_safe(Gs,GsO),combine_keys(GsO,GsOO).

combine_keys([],[]):-!.
combine_keys([K1-V1|GsO],[K1-Props|GsOO]):- my_partition(=(K1-_),[K1-V1|GsO],G1,G2),
 my_maplist(arg(2),G1,Props),combine_keys(G2,GsOO).

group_same_prop(IndvS,Prop,Have,HaveNots):-
  group_props(IndvS,PropsSet),
  member(Prop,PropsSet),
  my_partition(has_prop(Prop),IndvS,Have,HaveNots).

member_prop(Prop,Obj,Actual):-
  member_prop(Prop,Obj,_Template,Actual).

member_prop(Prop,Obj,Template,Actual):-
  indv_props_list(Obj,List),generalize(Prop,Template),nonvar(Template),copy_term(Template,Actual),member(Actual,List).

group_at_least_1_diff_props(IndvS0,Prop,Obj,HaveNots,Actuals):- guard_invs(IndvS0,IndvS),
  group_props(IndvS,PropsSet),
  member(Prop,PropsSet),
  once((my_partition(has_prop(Prop),IndvS,Have,HaveNots),
  Have=[Obj])),
  my_maplist(member_prop(Prop),HaveNots,Actuals).

group_all_diff_props(IndvS0,Prop,Obj,HaveNots,OtherProp):- guard_invs(IndvS0,IndvS),
  group_at_least_1_diff_props(IndvS,Prop,Obj,HaveNots,Actuals),
  list_to_set(Actuals,AS),AS = [OtherProp].

guard_invs(IndvS0,IndvS):- is_group(IndvS0),!,IndvS0=IndvS.
guard_invs(IndvS0,IndvS):- var(IndvS0), !, no_repeats(IndvS,gen_group(IndvS)),IndvS0=IndvS.
guard_invs(IndvS0,IndvS):- into_group(IndvS0,IndvS).

gen_group(IndvS):-
arc_grid(_,Grid), \+ \+ individuate_3(complete,Grid,_),
  why_grouped(_Why,IndvS),IndvS\==[].

group_diff_props(IndvS0,Ps):- guard_invs(IndvS0,IndvS),
  group_props(IndvS,PropsSet),
  findall(Prop,(member(Prop,PropsSet),\+ my_maplist(has_prop(Prop),IndvS)),Ps).

group_props(IndvS,PropsSet):- 
  findall(Props,(member(Obj,IndvS),indv_props_list(Obj,Props)),PropsL),
  append(PropsL,PropsF),list_to_set(PropsF,PropsSet).

group_uprops(IndvS0,UPropsSet):- guard_invs(IndvS0,IndvS),
  group_props(IndvS,PropsSet),simplify_props(IndvS,PropsSet,L),list_to_set(L,UPropsSet).

not_has_prop(Prop,Obj):- \+ has_prop(Prop,Obj).

relax_prop(iz(S1),iz(R1)):- !, relax_prop(S1,R1).
relax_prop(S1,R1):- compound(S1),ground(S1),!,relax_prop1(S1,R1),\+ ground(R1),!.
relax_prop(S1,R1):- R1 = S1.

relax_prop1(S1,R1):- relax_prop2(S1,R1)*->true;generalize(S1,R1).

relax_prop2(pg(OG,X,Y,_),pg(OG,X,Y,_)).
relax_prop2(loc2D(X,_),loc2D(X,_)).
relax_prop2(loc2D(_,Y),loc2D(_,Y)).


simplify_props(IndvS,[R1|Props],SPropsF):- never_group_on(R1), !,simplify_props(IndvS,Props,SPropsF).
simplify_props(IndvS,[R1|Props],SPropsF):- my_maplist(haz_prop(R1),IndvS), !,simplify_props(IndvS,Props,SPropsF).
simplify_props(IndvS,Props,[R1|SPropsF]):- 
  select(S1,Props,More),\+ never_group_on(S1),
  select(S2,More,More2),\+ never_group_on(S2),
  relax_prop(S1,R1),relax_prop(S2,R2),R1=@=R2,
  %ground(S1),ground(S2),
  \+ my_maplist(haz_prop(R1),IndvS),
  my_partition(=(R2),More2,_Remove,Keep),!,
  simplify_props(IndvS,Keep,SPropsF).
simplify_props(_,A,A).

pregroup1(iz(media(shaped))).
pregroup1(iz(media(image))).
pregroup1(iz(chromatic(N,BGN))):- between(1,10,N),between(0,2,BGN).
pregroup1(pg(_OG,_,_,How)):- dif(How,i_repair_patterns).


never_uprop(localpoints(_)).
never_group_on(pg(_,I,_,_)):- I == i_repair_patterns.
never_group_on(P):- never_uprop(P).

regroups(IndvS,[Why1,Why2],[Obj|Grp]):-
  group_uprops(IndvS,PropsSet),
  propset_indivs(PropsSet,OtherProps,IndvS,Why1,Grp1),
  length(Grp1,N1),N1>2,
  propset_indivs(OtherProps,_,Grp1,Why2,Grp2),
  length(Grp2,N2),N2=1,Grp2 = [Obj],
  select(Obj,Grp1,Grp).

regroups(IndvS,[pairs,Why1],Grp1):-
  group_props(IndvS,PropsSet),
  propset_indivs(PropsSet,_OtherProps,IndvS,Why1,Grp1),
  length(Grp1,N1),N1=2.

propset_indivs(PropsSet,OtherProps,IndvS,Why,Grp):- 
  select(Why,PropsSet,OtherProps),
  include(haz_prop(Why),IndvS,Grp).

haz_prop(P,O):- has_prop(P,O).

:- export(grid_part/2).
grid_part(Grid,Info):- var(Grid), get_current_test(TestID), some_current_example_num(ExampleNum),!,
  kaggle_arc_io(TestID,ExampleNum,_,Grid),
  grid_part(Grid,Info).

%grid_part(Grid,InfoR):- nth1(X,Grid,Info),VInfo=..[v|Info],InfoR=..[row,X,VInfo].
%grid_part(Grid,InfoR):- rot90(Grid,Grid90),nth1(X,Grid90,Info),VInfo=..[v|Info],InfoR=..[col,X,VInfo].
%grid_part(Grid,NObjs):- wno(individuate_3(complete,Grid,Objs)), maplist_n(1,number_obj,Objs,NObjs).

%cheapest_desc(Grid

number_obj(N,obj(List),obj([ord(N)|List])).
/*
  Obj = obj(List),
  loc2D(Obj,X,Y),obj_to_oid(Obj,_,MyID),
 % atomic_list_concat([obj,X,Y],'_',Key),
  localpoints_include_bg(Obj,LocalPoints),
  points_to_grid(X,Y,LocalPoints,Grid),mapgrid(sometimes_assume(=,bg),Grid),
  select(shape_rep(grav,Shape),List,Rest2),mapgrid(sometimes_assume(=,bg),Shape),
  Rest3 = Rest2,
  must_det_ll((remove_too_verbose(MyID,Rest3,TV00))),flatten([TV00],TV0),
  must_det_ll((include(not_too_verbose,TV0,TV1),my_maplist(fix_iz,TV1,TV))),!,
  member(MrT,[oform(Shape),ogrid(Grid)|TV])*/

%grid_part(Grid,P):- globalpoints(Grid,Points),member(P,Points).

%object_info(obj(List)

%grid(Type,ConstructorData,[rot270]),CacheOfCalls).

%is_graid(TestID>ExampleNum*IO,TestID,ExampleNum,IO).

is_graid((TestID > (ExampleNum*IO)),G):- kaggle_arc_io(TestID,ExampleNum,IO,G).

:- export(is_graid/2).
%grid_aid(ID,TestID>ExampleNum*IO):- is_graid(Grid,TestID,ExampleNum,IO),format(ID,).

point(Grid,Color,X,Y):- is_graid(Grid,G),nth1(Y,G,R),nth1(X,R,Color).

%g_size(Grid,X,Y):- is_graid(Grid,G),grid_size(G,X,Y).

grid_points(Grid,Points):-  is_graid(Grid,G),globalpoints(G,Points).
grid_point(Grid,point(X,Y,Color)):- point(Grid,Color,X,Y).

grid_object(Grid,mass(1),Point):- grid_point(Grid,Point).
grid_object(Grid,mass(2),point2(Dir,[(HV1)-(HV2)],Color)):- 
  globalpoints(Grid,Ps),select(Color-HV1,Ps,Pss),select(Color-HV2,Pss,_), 
  is_adjacent_point(HV1,Dir,HV2).

grid_object(Grid,mass(N),object(Points,Color)):- 
  is_graid(Grid,G),enum_colors(Color), \+ \+ grid_point(Grid,point(_,_,Color)),
  length(Points,N),Points = [HV1,HV2,HV3|AdjRest],
  globalpoints(G,Ps),select(Color-HV1,Ps,Pss),select(Color-HV2,Pss,Psss),select(Color-HV3,Psss,Rest), 
  HV1 @< HV2,
  is_adjacent_point(HV1,_Dir1,HV2),is_adjacent_point(HV2,_Dir2,HV3),
  ColorHV4 = Color-HV4,
  findall(HV4,(member(ColorHV4,Rest),is_adjacent_point(HV1,_,HV5),is_adjacent_point(HV5,_,HV4)),AdjRest).





:- include(kaggle_arc_footer).

