/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
/*

Rule types

i1 to o1
i2 to o1
o1 to o1


*/
:- encoding(iso_latin_1).
:- include(kaggle_arc_header).
:- thread_local(t_l:objs_others/4).
show_interesting_props(Named,OutC,InC):-
 extend_grp_proplist(InC,ObjsI),
 extend_grp_proplist(OutC,ObjsO),
  banner_lines(cyan,4),
  w_section('INPUT PROPS',
    locally(t_l:objs_others(inputs,ObjsI,ObjsO,outputs),
      show_interesting_named_props(input(Named),ObjsI))),
  banner_lines(white,2),
  w_section('OUTPUT PROPS',
    locally(t_l:objs_others(outputs,ObjsO,ObjsI,inputs),
      show_interesting_named_props(output(Named),ObjsO))),    
  banner_lines(white,2),
  show_interesting_props_next(Named,ObjsI,ObjsO),
  banner_lines(cyan,4).

show_interesting_props_next(Named,ObjsI,ObjsO):- 
  append(ObjsO,ObjsI,ObjsAll),
  w_section('BOTH PROPS',
    locally(t_l:objs_others(both,ObjsAll,ObjsAll,both),
      show_interesting_named_props(both(Named),ObjsAll))),!.

show_interesting_props_next(_Named,ObjsI,ObjsO):- 
   append(ObjsO,ObjsI,Objs),
   show_interesting_props_gojs(Objs).

show_interesting_props_gojs(Objs):- u_dmsg(show_interesting_props_gojs(Objs)).
  %8731374e

print_treeified_props(Objs):-
  print_treeified_props(treeified_props,Objs).
print_treeified_props(Named,Objs):-
  maplist(into_obj_plist,Objs,RawPropLists),
  maplist(treed_props_list,RawPropLists,PropLists),
  print_ptree(Named,PropLists).

treed_props_list(RawPropLists,PropLists):-
  include(p1_not(p1_arg(_,is_gridoid)),RawPropLists,RawPropLists0),
  care_to_count(RawPropLists0,RawPropLists1),
  include(p1_not(p1_subterm(skip_ku)),RawPropLists1,PropLists),!.

print_ptree(Named,RRR):- 
  treeify_props(RRR,Tree),
  remember_tree(Named,Tree),
  tersify_gridoids(Tree,TTree),
  with_pre(pp(Named=TTree)).

tersify_gridoids(Tree,TTree):- map_pred1(replace_gridoids,Tree,TTree).
replace_gridoids(Tree,TTree):- is_points_list(Tree),length(Tree,F),F>4,!,length(Four,4),append(Four,_,Tree),append(Four,'...',TTree),!.
replace_gridoids(G,O):- is_grid(G),!,tersify(G,O).
replace_gridoids(Tree,TTree):- is_grid(Tree),length(Tree,F),F>4,!,length(Four,4),append(Four,_,Tree),append(Four,'...',TTree),!.
%tersify_gridoids(Tree,TTree):- is_grid(Tree),length_s(Tree,F),F>4,!,length_s(Tree,F),append(Four,_,Tree),append(Four,'...',TTree),!.
replace_gridoids(G,O):- is_gridoid(G),!,tersify(G,O).


show_interesting_named_props(Named,In):-
   extend_grp_proplist(In,ObjsG),!,
   consider_for_rank(ObjsG,Objs,_),
   w_section(print_treeified_props(Named),print_treeified_props(Named,Objs)),
   banner_lines(green,2),banner_lines(green,2),
   hack_prop_groups(Named,Objs),
   banner_lines(green,2),banner_lines(green,2),!.
   

show_interesting_named_props(Named,In):-
 must_det_ll((
   extend_grp_proplist(In,ObjsG),
   consider_for_rank(ObjsG,Objs,_),
   hack_prop_groups(Named,Objs),
   show_three_interesting_groups(Named,Objs,Groups),
   %banner_lines(cyan,3),
   groups_to_groupsets(Groups,GroupSets),
   forall(member(GObjs,GroupSets),
    (findall(Title,member(Title-GObjs,Groups),Titles),
      show_interesting_group(Named,Titles-GObjs))))).

groups_to_groupsets(Groups,GroupSets):-
  maplist(arg(2),Groups,GroupsList), predsort(variants_equal,GroupsList,GroupSets).

show_interesting_group(Named,Title-Objs):- 
  length_s(Objs,Len),
  mass(Objs,Mass),
  sformat(SGroup,'~w Object(s) ~w mass (~q)',[Len,Mass,Named]), 
  w_section(title(SGroup),
    (pp(Title),nl,print_ss(Objs))).

show_three_interesting_groups(Named,Objs,Groups):-
  findall(Prop,(member(obj(O),Objs),member(Prop,O), not_skip_ku(Prop) ),Props),
  sort_safe(Props,SProps),
  print_interesting_named_groups(props(Named),SProps),
  maplist(make_unifiable_cc,SProps,UProps), predsort(using_compare(numbered_vars),UProps,SUProps),  
  print_interesting_named_groups(suprops(Named),SUProps),
  %count_each(SProps,Props,GroupsWithCounts),
  length_s(Objs,L),
  group_quals(SUProps,SProps,L,KUProps),
  print_interesting_named_groups(kuprops(Named),KUProps),
  objs_with_props(KUProps,Objs,L,Groups),
  nop(print_ss(groups=Groups)).

print_interesting_named_groups(Named,KUProps):- 
   w_section(title(Named),pp(KUProps)).

numbered_vars(A,B):- copy_term(A,B),numbervars(B,0,_,[attvars(skip)]).

%skip_ku(pg(_,_,FL,_)):- !, FL \==firstOF, FL \==lastOF. 

skip_ku(Var):- var(Var),!,fail.
%skip_ku(pg(_,_,_,_)).
skip_ku(link(sees([_,_|_]),_)).
skip_ku(link(sees(_),_)).
skip_ku(links_count(sees,_)).
skip_ku(occurs_in_links(sees,_)).
skip_ku(iz(media(_))).
skip_ku(shape_rep( _,_)).
skip_ku(points_rep( _,_)).
skip_ku(globalpoints(_)).
skip_ku(center2G(_,_)).
skip_ku(changes(_)).
skip_ku(o(_,_,_,_)).
skip_ku(cc(C,_)):- is_real_color(C),!.
skip_ku(giz(_)).
skip_ku(iz(KU)):- skip_ku(KU),!.
skip_ku(S):- priority_prop(S),!,fail.
%skip_ku(iz(info(_))).
%skip_ku(iz(_)).
skip_ku(_-KU):- skip_ku(KU),!.


priority_prop(Var):- var(Var),!,fail.
priority_prop((algo_sid(norm,_))).
priority_prop((stype(_))).
priority_prop(iz(P)):- priority_prop(P),!.
priority_prop(giz(P)):- priority_prop(P),!.
priority_prop(_-P):- priority_prop(P),!.
priority_prop(pen(_)).
priority_prop(iv(_)).
priority_prop(sid(_)).
priority_prop(cc(fg,_)).
priority_prop(cc(bg,_)).
priority_prop(occurs_in_links(contained_by,_)).

ku_rewrite_props(Var,Var):- var(Var),!.
ku_rewrite_props(List0,List9):- is_grid(List0),!,List9=List0.
ku_rewrite_props(List0,List9):- is_group(List0),!,mapgroup(ku_rewrite_props,List0,List9).
ku_rewrite_props(obj(List0),obj(List9)):-!,ku_rewrite_props(List0,List9).
ku_rewrite_props(List0,List9):- is_list(List0),!,
  include(not_skip_ku,List0,List1),
  maplist(ku_rewrite_props,List1,List2),
  variant_list_to_set(List2,List9).
ku_rewrite_props(link(sees([cc(S,_)]),_),link(sees([cc(S,_)]),_)).
ku_rewrite_props(link(S,_),link(S,_)):-!.
ku_rewrite_props(S-A,S-B):- ku_rewrite_props(A,B),!.
ku_rewrite_props(A,A).

objs_with_props([KU-_|Props],Objs,OL,GO):- skip_ku(KU),!,objs_with_props(Props,Objs,OL,GO).

objs_with_props([KU-_|Props],Objs,OL,GO):-
  findall(O,(member(O,Objs),has_prop(KU,O)),KL), length_s(KL,KLL), 
  KLL==OL,!,objs_with_props(Props,Objs,OL,GO).
objs_with_props([_-List|Props],Objs,OL,GO):-
  findall(Prop-OO,(member(Prop,List),
     (findall(O,(member(O,Objs),has_prop(Prop,O)),OO))),         G),
  objs_with_props(Props,Objs,OL,Groups),
  append(G,Groups,GO).
objs_with_props([],_,_,[]).

group_quals([U|SUProps],SProps,L,[U-ListUS|KUProps]):- findall(U,member(U,SProps),ListU),
  length_s(ListU,LUL),LUL\==L, sort_safe(ListU,ListUS),
   ListUS=[_,_|_],!,group_quals(SUProps,SProps,L,KUProps).
group_quals([_|SUProps],SProps,L,KUProps):-  group_quals(SUProps,SProps,L,KUProps).
group_quals([],_SProps,_,[]).



dont_change(Var):- copy_term(Var,VarC),freeze(VarC,fail), freeze(Var,Var=@=VarC).


select_subgroup(Objs,GroupName,Count,SubObjs):- 
  into_group(Objs,GObjs),
  findall(O-GroupName,(member(O,GObjs),is_in_subgroup(Objs,O,GroupName)),Memberships),
  maplist(arg(2),Memberships,GroupNames),
  get_ccs(GroupNames,GroupNameCC),
  member(cc(GroupName,Count),GroupNameCC),
  findall(O,member(O-GroupName,Memberships),SubObjs).

object_prop(O,Prop):- indv_props_list(O,Props),member(Prop,Props).

objects_props(SubObjs,Props):-
  findall(Prop,(member(O,SubObjs),object_prop(O,Prop)),Props).
objects_names_props(SubObjs,Props):-
  findall(F-Prop,(member(O,SubObjs),object_prop(O,Prop),prop_name(Prop,F)),Props).

prop_name(Prop,F):- \+ compound(Prop),!,F=Prop.
prop_name(Prop,Named):- compound_name_arguments(Prop,F,[A|_]),
   (number(A)-> Named =F ; compound_name_arguments(Named,F,[A])).


mostly_fg_objs(OutCR,OutCR):-!.
mostly_fg_objs(ObjsG,FG):- my_partition(is_fg_object,ObjsG,FG,BG),length(FG,FGL),length(BG,BGL),FGL>BGL,!.
%mostly_fg_objs(InCRFGBG,OutCR):- include(is_fg_object,InCRFGBG,OutCR),!.
mostly_fg_objs(OutCR,OutCR):-!.

object_group_cc(Objs,GroupName,SubObjs,Count,NamesCC,ValuesCC):-
  select_subgroup(Objs,GroupName,Count,SubObjs),
  objects_names_props(SubObjs,Props),
  maplist(arg(1),Props,Names),get_ccs(Names,NamesCC),
  maplist(arg(2),Props,Values),get_ccs(Values,ValuesCC).

group_prop(P):- group_prop(P,_).
group_prop(giz(g(in)),i_o(input)).
group_prop(giz(g(out)),i_o(output)).
group_prop(cc(fg,0),fg_or_bg(iz_bg)).
group_prop(cc(bg,0),fg_or_bg(iz_fg)).
group_prop(Pof2,Extra):- compound(Pof2),compound_name_arguments(Pof2,F,List),append(Left,[A],List),
  classify_n(A,Plural),
  append([F|Left],[Plural],AmmountL),
  compound_name_arguments(Extra,ammount,AmmountL),!.
classify_n(Var,any):- var(Var),!.
classify_n(0,zero).
classify_n(1,zero).
classify_n(Two,any):- number(Two),!, Two>=2.


link_rel(sees(_)).
link_rel(overlaps(_,_)).
link_rel(overlapped_by(_,_)).
link_rel(contains).
link_rel(contained_by).
link_rel(P,F):- link_rel(P),functor(P,F,_).

/*
is_in_subgroup_c(Contained_by,Functor,_Grp,Obj,count_of_bg_links(Functor,Count)):- findall(Y, (indv_props(Obj,link(Contained_by,Y)),is_bg_object(Y)), L), length_s(L,Count).
is_in_subgroup_c(Contained_by,Functor,_Grp,Obj,count_of_fg_links(Functor,Count)):- findall(Y, (indv_props(Obj,link(Contained_by,Y)),is_fg_object(Y)), L), length_s(L,Count).
is_in_subgroup_c(Contained_by,Functor,Grp,YObj,links_from_bg_count(Functor,Count)):- obj_to_oid(YObj,Y), link_rel(Contained_by,Functor),findall(Obj, (member(Obj,Grp),indv_props(Obj,link(Contained_by,Y)),is_bg_object(Obj)), L), length_s(L,Count).
is_in_subgroup_c(Contained_by,Functor,Grp,YObj,links_from_fg_count(Functor,Count)):- obj_to_oid(YObj,Y), link_rel(Contained_by,Functor),findall(Obj, (member(Obj,Grp),indv_props(Obj,link(Contained_by,Y)),is_fg_object(Obj)), L), length_s(L,Count).
*/

fyl_functor_count(FYL,Functor,Count):-
  maplist(arg(1),FYL,Fs),list_to_set(Fs,FSet),
  member(Functor,FSet),
  findall(_,member(Functor-_,FYL),L),length_s(L,Count).

link_functor(contains,contains).
link_functor(sees(_),sees).
link_functor(contained_by,contained_by).


%is_in_subgroup(Grp,Obj,Prop):- var(Obj),!, enum_object(Obj),is_in_subgroup(Grp,Obj,Prop).
is_in_subgroup(Grp,Obj,Prop):- nonvar(Grp),var(Obj),!,member(Obj,Grp),is_in_subgroup(Grp,Obj,Prop).
is_in_subgroup(Grp,Obj,Prop):- var(Grp),var(Obj),!,findall(Obj,enum_object(Obj),Grp),is_in_subgroup(Grp,Obj,Prop).
is_in_subgroup(_Grp,Obj,links_count(Functor,Count)):- 
  link_functor(Contained_by,Functor),
  findall(_, (indv_props(Obj,link(Contained_by,_))), FYL), 
  length_s(FYL,Count).

is_in_subgroup(Grp,YObj,occurs_in_links(Functor,Count)):- 
  obj_to_oid(YObj,Y), findall(Functor-Obj, 
    (member(Obj,Grp),indv_props_list(Obj,PL),member(link(Contained_by,YY),PL),YY=Y,functor(Contained_by,Functor,_)), FYL), 
  fyl_functor_count(FYL,Functor,Count).

is_in_subgroup(_Grp,Obj,iz(IZ)):- group_prop(Prop,IZ), has_prop(Prop,Obj).
is_in_subgroup(_Grp,Obj,nth_fg_color(Nth,Color)):- unique_fg_colors(Obj,List),
 sort_color_by_mass(Obj,List,Sorted),nth1(Nth,Sorted,Color).


%is_in_subgroup(Grp,Obj,ansestors(N,Set)):-transitive_sets(ansestor,Obj,Set,N).
%is_in_subgroup(Grp,Obj,descendants(N,Set)):-transitive_sets(descendant,Obj,Set,N).
%is_in_subgroup(Grp,Obj,tiouching(N,Set)):- nontransitive_set(touching,Obj,Set,N).
%is_in_subgroup(Grp,Obj,seeing(N,Set)):- nontransitive_set(seeing,Obj,Set,N).
%is_in_subgroup(Grp,Obj,insideOf(N,Set)):-transitive_sets(insideOf,Obj,Set,N).
%is_in_subgroup(Grp,Obj,contained_by(N,Set)):-transitive_sets(contained_by,Obj,Set,N).
%is_in_subgroup(Grp,Obj,Prop):- has_prop(Prop,Obj).
%is_in_subgroup(Grp,_,all).
not_skip_ku(P):- \+ skip_ku(P).

indv_eprops_list(Indv,List9):- 
  indv_props_list(Indv,List0),
  ku_rewrite_props(List0,List9).


variant_list_to_set(I,O):- list_to_set(I,M),list_to_set_variants_equal2(M,O).

list_to_set_variants_equal2([E|List],[E|Set]):- !,
  ((select(S,List,Rest),shall_count_as_same(E,S))
    ->list_to_set_variants_equal2(Rest,Set);list_to_set_variants_equal2(List,Set)).
list_to_set_variants_equal2(H,H).

determine_elists(Objs,EList):-
  maplist(indv_eprops_list,Objs,PropLists),
  flatten(PropLists,List), %list_to_set(List,Set),
  include(not_skip_ku,List,EList).

hack_prop_groups(Named,Objs):-
 must_det_ll((
  determine_elists(Objs,EList),
  w_section(print_elists(Named),print_elists_hack_objs(Named,EList,Objs,HackedObjs)),
  banner_lines(orange,2),
  nop((maplist(arg(1),HackedObjs,RRR),
    w_section(hack_prop_groups(Named), (print_ptree(hacked(Named),RRR), banner_lines(yellow,2))))),  
  ignore(skip_if_ansi(print_propset_groups(Named,Objs,EList))))).



var_to_underscore(Var,_):- plain_var(Var),!.
print_elists_hack_objs(Named,Props,Objs,HackedObjs):-
 HackedObjs = Hacked,
 % HackedObjs = Splits,
 must_det_ll((
  length_s(Objs,BaseSize),
  variant_list_to_set(Props,PropsSet),
  count_each(PropsSet,Props,CountOfEachL),
  predsort(sort_on(arg(2)),CountOfEachL,CountOfEach0),
  sort(CountOfEach0,CountOfEach),
  %mpp(countOfEach=CountOfEach),
  ignore(maplist(remember_propcounts(Named,count),CountOfEach)),
  maplist(make_unifiable_cc,PropsSet,UPropsSet),
  map_pred(var_to_underscore,UPropsSet,UPropsSetG),
  variant_list_to_set(UPropsSetG,UPropsSetGSet),
  count_each(UPropsSetGSet,UPropsSetG,GroupsWithCountsL),
  variant_list_to_set(GroupsWithCountsL,GroupsWithCountsLVS),
  predsort(sort_on(arg(2)),GroupsWithCountsLVS,GroupsWithCounts),!,
  variant_list_to_set(GroupsWithCounts,GroupsWithCountsW),
  sort(GroupsWithCountsW,GroupsWithCountsWP),
  variant_list_to_set(GroupsWithCountsWP,GroupsWithCountsWPO),
  make_splitter(GroupsWithCountsWPO,CountOfEach,SSplits),sort(SSplits,CSplits),
  store_splits(Named,BaseSize,CSplits,Splits),
  print_ptree(countOfEachU(Named),Splits),
  ignore(maplist(remember_propcounts(Named,diversity),GroupsWithCountsWPO)),
  replace_props_with_stats(GroupsWithCountsWPO,CountOfEach,Objs,HackedObjsM),
  maplist(ku_rewrite_props,HackedObjsM,Hacked),
  nop(pp(hackedObjs(Named)=HackedObjs)))).

store_splits(Named,BaseSize,CSplits,Splits):-
  ignore((maplist(save_1split(Named,BaseSize),CSplits))),
  clean_splits(BaseSize,CSplits,Splits).

%save_1split(Named,Six,(1-_)->[Six-CSplits]):- maplist(remember_propcounts(Named,nth(_)),[CSplits]).
%save_1split(Named,Six,(Six-_)->List):- length_s(List,Six),maplist(p1_call(chk(=(1-_))),List),
save_1split(Named,_,(N-Div)->CSplits):- maplist(remember_propcounts(Named,Div,N),CSplits).  
%save_1split(Named,_,(1-_)->[1-CSplits]):- !.
%save_1split(Named,_,(N)->List):- predsort(sort_on(last_type_is_value),List,Sort).
%save_1split(Named,_,_).

clean_splits(BaseSize,CSplits,SplitsO):- is_list(CSplits),!,
  maplist(clean_1split(BaseSize),CSplits,Splits),
  include(\==(''),Splits,SplitsO).
clean_splits(_,I,I).

clean_1split(Six,(1-_)->[Six-_CSplits],'').
clean_1split(Six,(1-_)->[Six-CSplits],ignore(CSplits)).
clean_1split(_,(1-_)->[1-CSplits],1-CSplits).
%clean_1split(Six,(Six-_)->List,Sort):- length_s(List,Six),maplist(arg(2),List,Last),predsort(sort_on(last_type_is_value),Last,Sort).
clean_1split(Six,(Six-_)->List,(Six)->Sort):- length_s(List,Six),maplist(arg(2),List,Last),predsort(sort_on(last_type_is_value),Last,Sort).
clean_1split(_,(N)->List,(N)->Sort):- predsort(sort_on(last_type_is_value),List,Sort).
clean_1split(_,X,X).

last_type_is_value(I,O):- last_type_is(number,I,O).
last_type_is_value(I,O):- last_type_is(is_list,I,O).
last_type_is_value(I,O):- last_type_is(is_color,I,O).
last_type_is_value(I,I).

last_type_is(P1,List,Last):- is_list(List),reverse(List,Term),member(E,Term),last_type_is(P1,E,Last).
last_type_is(P1,Numb,Numb):- p1_call(P1,Numb),!.
last_type_is(P1,Term,Last):- compound(Term),compound_name_arguments(Term,_,Args),reverse(Args,List),!,member(E,List),last_type_is(P1,E,Last).

make_splitter([N-UProp|WithCountsWPO],CountOfEach,OutSplits):-
  my_partition(sameps(UProp),CountOfEach,Used,Unused),
  made_split(N,UProp,Used,Out),
  (Out==[]->OutSplits=Splits;OutSplits=[Out|Splits]),
  make_splitter(WithCountsWPO,Unused,Splits).
make_splitter([],_,[]).

made_split(_N,_UProp,[],[]).
made_split(N,UProp,List,Out):-variant_list_to_set(List,Set),List\=@=Set,!,made_split(N,UProp,Set,Out).
made_split(N,UProp,List,Out):-sort(List,Set),List\=@=Set,!,made_split(N,UProp,Set,Out).
made_split(_,UProp,List,((Len-UProp)->List)):- length_s(List,Len).
sameps(UProp,_-Prop):- \+ Prop \= UProp.

into_test_id_io(input(TestID>ExampleNum),TestID,ExampleNum,in).
into_test_id_io(both(TestID>ExampleNum),TestID,ExampleNum,in_out).
into_test_id_io(output(TestID>ExampleNum),TestID,ExampleNum,out).
into_test_id_io(Named,TestID,ExampleNum,IO):- Named=..[IO,TestID>ExampleNum],!.
into_test_id_io(Named,TestID,ExampleNum,IO):- Named=..[IO,TestID,ExampleNum],!.
remember_propcounts(Named,Diversity,N-Prop):- into_test_id_io(Named,TestID,ExampleNum,IO),
  arc_assert(propcounts(TestID,ExampleNum,IO,Diversity,N,Prop)).
remember_propcounts(Named,Diversity,B,N-Prop):- into_test_id_io(Named,TestID,ExampleNum,IO),
  arc_assert(propcounts(TestID,ExampleNum,IO,Diversity,B,N,Prop)).


sort_obj_props(How,obj(Props),obj(Sorted)):-
  predsort(How,Props,Sorted).

replace_props_with_stats(SortedWithCounts,CountOfEach,obj(Objs),obj(HackedObjs)):- 
  !,maplist(replace_props_with_stats(SortedWithCounts,CountOfEach),Objs,Hacked),
  sort(Hacked,HackedObjsR),reverse(HackedObjsR,List0),!,
  include(not_skip_ku,List0,List1),
  maplist(ku_rewrite_props,List1,HackedObjs).

replace_props_with_stats(SortedWithCounts,CountOfEach,Objs,HackedObjs):-
  is_list(Objs),!,maplist(replace_props_with_stats(SortedWithCounts,CountOfEach),Objs,HackedObjs).



replace_props_with_stats(SortedWithCounts,CountOfEach,Prop,OProp):-  by_freq(SortedWithCounts,CountOfEach,Prop,OProp),!.


by_freq(_,_,N-P,N-P):-!.
by_freq(SortedWithCounts,CountOfEach,Prop,OProp):-
   ((member(N1-UProp,SortedWithCounts), \+ \+ (UProp=Prop))),!,
   ((member(N2-CProp,CountOfEach),CProp=@=Prop)
      ->
        (OProp=((N1/N2)-Prop))
      ;
        (OProp=u(N1)-Prop)).
by_freq(_SortedWithCounts,CountOfEach,Prop,(c(N2)-Prop)):-
   ((member(N2-CProp,CountOfEach),CProp=@=Prop)),!.
by_freq(_,_,P,0-P):-!.

print_propset_groups(Named,Objs,EList):- 
  w_section(print_propset_groups(Named),print_propset_groups_now(Named,Objs,EList)).

print_propset_groups_now(Named,Objs,EList):-
  maplist(has_props_set(Objs),EList,GrpList),
  predsort(sort_on(length_s),GrpList,GrpSetR),reverse(GrpSetR,GrpSet),
  with_tag_ats(table,class("tblo rows_distinct"),
   (html_table_row([th(props),td('&nbsp;'),th(objects),td('&nbsp')]),
    maplist(print_prop_groups(Named,EList,Objs),GrpSet),
    forall(select(O1,Objs,Rest),print_prop_groups(Named,EList,Rest,O1,[])))).

has_props_set(Objs,Prop,Set):- include(has_prop(Prop),Objs,Set).
print_prop_groups(_Named,_ESet,_,[]):-!.
print_prop_groups(_Named,_ESet,_,[_]):-!.
print_prop_groups(Named,EList,All,Group):-
  include(not_in(Group),All,Except),
  [O1|Rest]=Group,
  print_prop_groups(Named,EList,Except,O1,Rest).

print_prop_groups(Named,_ESet,Except,O1,Rest):-
  [O1|Rest]=Group,
  indv_props_list(O1,PropsIKU), include(not_skip_ku,PropsIKU,Props),

  findall(Prop,(member(Prop,Props), \+ any_have_prop(Except,Prop), 
                               forall(member(O,Rest),has_prop(Prop,O))),UniqueToGroup),

  t_l:objs_others(_,_These,Those,_),
  findall(O2,(member(O2,Those), forall(member(UProp,UniqueToGroup),has_prop(UProp,O2))),OtherObjs),

  asserta_if_new(prop_groups(Named,Group,UniqueToGroup,Except,OtherObjs)),

  maplist(into_gui_item,Group,Grids),
  maplist(into_gui_item,OtherObjs,OGrids),
  %OGrids=[],
  print_prop_grids_list(UniqueToGroup,Grids,OGrids),!.

remember_tree(_,_).

print_prop_grids_list(UniqueToGroup,Grids,OGrids):- 
 html_table_row([
  th(with_tag_style(pre,"width: 225px; font-size: 14px",write_tall(UniqueToGroup))), 
  td('&nbsp;'),
  td(write_scrollable(print_table_rows_2([Grids,OGrids ]))),
  td('&nbsp;')]),!.
%print_prop_grids_list(UniqueToGroup,Grids,OGrids):- print_table([[with_tag(pre,write_tall(UniqueToGroup)),print_table([[print_ss(Grids)],[print_ss(OGrids)]])]]).

%print_table_rows_2([RowA,RowB]):- !, print_side_by_side(RowA),!,print_side_by_side(RowB),!.
print_table_rows_2([RowA,[]]):- 
  t_l:objs_others(These,_,_,_),
  with_tag_style('table','border: 2px solid blue;', maplist(html_table_row,[[th(These)|RowA]])).
print_table_rows_2([RowA,RowB]):- 
  make_rows_same_length([RowA,[],RowB],[Row1,Spaces,Row2]),
  Spacer = [td(" ")|Spaces],
  t_l:objs_others(These,_,_,Those),
  with_tag_style('table','border: 2px solid blue;', maplist(html_table_row,[Spacer,Spacer,[th(These)|Row1],Spacer,[th(Those)|Row2],Spacer,Spacer])).

write_scrollable(Goal):- with_tag_class(div,scrollable,Goal).

any_have_prop(Except,Prop):- member(O,Except),has_prop(Prop,O),!.

transitive_sets(P2,Obj,Set,N):- findall(n(P,List),(trans(P2,Obj,List),List\==[],length_s(List,P)),Lists),sort_safe(Lists,Set),length_s(Set,N).
nontransitive_set(P2,Obj,Set,N):- findall(Other,p2_call(P2,Obj,Other),List),sort_safe(List,Set),length_s(Set,N).

trans(P2,Obj,Out):- obj_to_oid(Obj,OID),trans_no_loop(P2,[OID],Obj,Out).

trans_no_loop(P2,Skip,Obj,Out):- 
  ((call_oid_objs(P2,Obj,Other),v_obj(Other,That), \+ member(That,Skip))
    *->(Out=[Other|Others],trans_no_loop(P2,[That|Skip],That,Others))
    ;Out=[]).

v_obj(v(OID,_Info),OID):-!.
v_obj(Obj,Obj).

call_oid_objs(P2,OID,Other):- atom(OID),!,oid_to_obj(OID,Obj),call(P2,Obj,Other).
call_oid_objs(P2,Obj,Other):- call(P2,Obj,Other).

ansestor(Obj,Other):- has_prop(link(subsume,Other,subsumed_by(_,_)),Obj).
descendant(Obj,Other):- has_prop(link(subsume,Other,subsuming(_,_)),Obj).
touching(Obj,v(Other,Info)):- has_prop(link(dir_touching,Other,Info),Obj).
seeing(Obj,v(Other,Info)):- has_prop(link(dir_seeing,Other,Info),Obj).
insideOf(Obj,Other):- has_prop(link(insideOf,Other,_),Obj).
contained_by(Obj,Other):- has_prop(link(contained_by,Other,_),Obj).


% =====================================================================
is_fti_step(extend_obj_proplists).
% =====================================================================
%really_group_vm_priors(_VM):-!.
extend_obj_proplists(VM):- extend_grp_proplist(VM.objs,set(VM.objs)).


% =====================================================================
is_fti_step(group_vm_priors).
% =====================================================================
group_vm_priors(VM):-
 if_arc_option(group_vm_priors,
  really_group_vm_priors(VM)).

% =====================================================================
is_fti_step(really_group_vm_priors).
% =====================================================================
really_group_vm_priors(_VM):-!.
really_group_vm_priors(VM):-
 must_det_ll((
  ObjsG = VM.objs,
  %print_list_of(ppnl,ObjsG),
  TID_GID=tid_gid(VM.id,VM.gid),
  check_tid_gid(TID_GID,VM.start_grid),
  consider_for_rank(ObjsG,FG,BG),
  group_prior_objs(TID_GID,FG,Objs),  
  append(Objs,BG,FGBG),
  gset(VM.objs) = FGBG)).

consider_for_rank(ObjsG,FG,BG):- my_partition(is_fg_object,ObjsG,FG,BG).

% bg from fg
relivant_divide(is_fg_object).
relivant_group(G):- relivant_divide(G).
relivant_group(not_has_prop(mass(1))).
%relivant_divide(has_prop(cc(bg,0))).
% seperate input from output
%relivant_divide(has_prop(giz(g(in)))).
%relivant_divide(unique_fg_color_count_eq_1).

unique_fg_color_count_eq_1(Obj):- unique_fg_colors(Obj,II),II=1.

group_prior_objs(Why,ObjsIn,WithPriors):- fail,
 relivant_divide(RelivantDivide),
 my_partition(RelivantDivide,ObjsIn,FG,BG),
 ((FG\==[],BG\==[])), 
 fail,
 !,
 print_ss([splitting_group(RelivantDivide)=FG,splitting_group(RelivantDivide)=BG]), 
 group_prior_objs(Why,FG,FGWithPriors),
 group_prior_objs(Why,BG,BGWithPriors),
 append(FGWithPriors,BGWithPriors,WithPriors),!. 

group_prior_objs(Why,ObjsIn,WithPriors):- fail,
 once(combine_same_globalpoints(ObjsIn,Objs)),
 ObjsIn\=@=Objs,!,
 group_prior_objs(Why,ObjsIn,WithPriors).

group_prior_objs(Why,Objs,WithPriors):- 
 must_det_ll((
 %print_list_of(show_indiv,group_prior_objs,Objs),!,
 get_prior_labels(Objs,Lbls),
 keysort(Lbls,N),
 length_s(N,Len),
 Title = Why+Len,
 nop(noisey_debug(print_premuted_objects(Title))),
 w_section(title(add_priors(Title)),
  %print_tree(groupPriors=Lbls,[max_depth(200)]),
  with_tag_class(div,nonpre,add_priors(Lbls,Objs,WithPriors))))).

print_premuted_objects(Why,Objs):- Objs==[],!,dmsg(no_objs(Why)).
print_premuted_objects(Why,Objs):- 
  must_det_ll((
  orule_first(Objs,ORF), reverse(ORF,RORF),
  visible_first(ORF,VF), reverse(VF,RVF),
  largest_first(ORF,LF),
  smallest_first(ORF,SF),
  %sort_by_vertical_size(Objs,Sorted),
  grid_size(Objs,IH,IV),!,
  create_vis_layers([],IH,IV,0,RORF,Sets),!,
  %nop(maplist(title_objs,ORF,TitledObjs)),
  %maplist(obj_short_props,ORF,TitledObjs),
  pp(breakdown_of(Why)),
  append([
     orule_first=ORF,
     rev_orule_first=RORF,
     visible_first=VF,
     rev_visible_first=RVF,
     largest_first=LF,
     smallest_first=SF],Sets,SS),
  print_ss(SS),
  print_obj_short_props(ORF))).


create_vis_layers(_Fallback,_IH,_IV,_LayerNum,[],[]):-!.
create_vis_layers(Fallback,IH,IV,LayerNum,InC,[layer(LayerNum2)=BestOrder|Rest]):- 
  filter_shown(IH,IV,InC,InShown,InHidden),!,
  LayerNum2 is LayerNum+1,
  append(InShown,Fallback,Fallback2),
  list_to_set(Fallback2,BestOrder),
  ((InHidden==[];equal_sets(InHidden,InC))-> Rest=[] ; create_vis_layers(Fallback2,IH,IV,LayerNum2,InHidden,Rest)).

equal_sets(A,B):- sort_safe(A,AA),sort_safe(B,BB),AA=@=BB.


% sprop_piority(Class,Priority).

%sprop_piority(pg(OG,_,_,0),0).
%sprop_piority(birth(i3(_)),0).
%sprop_piority(birth(i2(_)),0).
sprop_piority(iz(flag(hidden)),9).
sprop_piority(iz(media(shaped)),0).
sprop_piority(iz(info(combined)),1).
sprop_piority(iz(media(image)),2).
sprop_piority(iz(info(by_color)),3).
sprop_piority(iz(info(glyphic)),2).


resize_inf(X,N):- is_list(X),!,length_s(X,N).
resize_inf(X,X).

member_or_iz(Prop,Ps):- member(Prop,Ps).
member_or_iz(Prop,Ps):- member(iz(Prop),Ps).
member_or_iz(Prop,Ps):- member(birth(Prop),Ps).
member_or_iz(Prop,Ps):- member(giz(Prop),Ps).

%combine_number(_F1,O1,O2,O):- O is ((abs(O1-O2)+1)*O1)+(O2*30).
combine_number(_F1,O1,O2,O):- O is sqrt(O1*O1+O2*O2).

length_s(L,S):- (is_list(L);var(L)),!,length(L,S).
length_s(L,S):- itrace,fail,length(L,S).

visible_first(IndvS0,IndvO):- predsort_two_p2(visible_priority,visible_first_order,IndvS0,IndvO).
%visible_pred(F1,I,O):- ranking_pred(F1,I,O),!.
visible_first_order(I,VArea):-  vis_area(I,Area), globalpoints(I,Ps),length_s(Ps,L), VArea is Area/L.
visible_priority(Indv,Priority):- mass(Indv,1),!,Priority=7.
visible_priority(Indv,Priority):- mass(Indv,0),!,Priority=8.
visible_priority(Indv,Priority):- sprop_piority(Prop,Priority), has_prop(Prop,Indv),!.
visible_priority(_,1):-!.

predsort_two_p2(P2a,P2b,IndvS,IndvO):-
  findall((SortOn1+SortOn2)-Indv,(member(Indv,IndvS),call(P2a,Indv,SortOn1),call(P2b,Indv,MSize),resize_inf(MSize,SortOn2)),All),
  keysort(All,AllK),
  maplist(arg(2),AllK,IndvO).  

orule_first(IndvS0,IndvO):- predsort_two_p2(orule_priority,orule_first_order,IndvS0,IndvO).
%orule_pred(F1,I,O):- ranking_pred(F1,I,O),!.
orule_first_order(I,VArea):-  hybrid_order(I,VArea).
orule_priority(Indv,Priority):- is_bg_object(Indv),!,Priority=9.
orule_priority(Indv,Priority):- area(Indv,1),!,Priority=6.
orule_priority(Indv,Priority):- has_prop(cc(fg,1),Indv),!,Priority=5.
orule_priority(Indv,Priority):- sub_var(iz(i()),Indv),Priority=1.
%orule_priority(Indv,Priority):- sprop_piority(Prop,Priority), has_prop(Prop,Indv),!.
orule_priority(_,3).



smallest_first(IndvS0,IndvO):- smallest_first(mass,IndvS0,IndvO).
smallest_pred(F1,I,O):- ranking_pred(F1,I,O),!.
smallest_pred(_,I,O):- mass(I,O).
smallest_priority(Indv,Priority):- sprop_piority(Prop,Priority), has_prop(Prop,Indv),!.
smallest_priority(_,1).
smallest_first(P2,IndvS0,IndvO):- predsort_two_p2(P2,smallest_priority,IndvS0,IndvO).

largest_first(IndvS0,IndvO):- largest_first(mass,IndvS0,IndvO).
largest_pred(F1,I,O):- ranking_pred(F1,I,O),!.
largest_pred(_,I,O):- mass(I,O).
largest_priority(Indv,Priority):- sprop_piority(Prop,Priority), has_prop(Prop,Indv),!.
largest_priority(_,1).
largest_first(P2,IndvS0,IndvR):-   
 sort_safe(IndvS0,IndvS),
 %must_det_ll
 ((
  findall((Priority+Size)-Indv,(member(Indv,IndvS),largest_priority(Indv,NPriority),call(P2,Indv,Size),Priority is - NPriority),All),
  keysort(All,AllK),
  maplist(arg(2),AllK,IndvO),
  reverse(IndvO,IndvR))).

largest_first_nonbg(IndvS,IndvOB):-  
  largest_first(mass,IndvS,IndvO),
  remove_bgs(IndvO,IndvL,BGIndvS),
  my_append(IndvL,BGIndvS,IndvOB).

care_to_count(GSS,GS):- is_list(GSS),include(is_care_to_count,GSS,GS).


not_care_to_count(Cmpd):- arg(1,Cmpd,E),is_gridoid(E),!.
not_care_to_count(Cmpd):- arg(1,Cmpd,E),is_points_list(E),!.
%not_care_to_count(iz(info(_))).
not_care_to_count(iz(HasNumber)):- sub_term(N,HasNumber),number(N),!.
is_care_to_count(P):- not_care_to_count(P),!,fail.
is_care_to_count(_).

variance_had(UHAD,HAD,RRR,Versions,Missing,Variance):-
  make_unifiable_cc(HAD,UHAD),
  findall(R,(member(R,RRR), \+ member(UHAD,R)),Missing),
  findall(UHAD,(member(R,RRR),member(UHAD,R)),VersionL),
  sort(VersionL,Versions),
  (Missing==[]->length(Versions,Variance);length([_|Versions],Variance)).

variance_had(HAD,RRR,Versions,Variance):-
  make_unifiable_cc(HAD,UHAD),
  findall(UHAD,(member(R,RRR),member(UHAD,R)),VersionL),
  sort(VersionL,Versions),
  length(Versions,Variance).

is_length(N,L):- length_s(L,N).
treeify_props([],[]):-!.
%treeify_props(One,[One]):- \+ is_list(One), !.
%treeify_props(RRR,R):- sort_vertically(RRR,R),!.
treeify_props(One,One):- \+ is_list(One), !.
treeify_props([One],One):-!.
treeify_props(One,One):- length_s(One,1), !.
treeify_props(One,One):- maplist(is_length(1),One), !.
treeify_props(RRR,R):- sort_vertically(RRR,RR),RRR\=@=RR,!,treeify_props(RR,R).
%treeify_props(L,LM):- maplist(simpl_ogs,L,MM),L\=@=MM,!,treeify_props(MM,LM).
%treeify_props(RRR,R):- sort_vertically(RRR,RR),RRR\=@=RR,!,treeify_props(RR,R).
treeify_props(RRR,HAD->RO):- member(R,RRR),member(HAD,R),
   maplist(variant_select(HAD),RRR,RR),treeify_props(RR,RO).
/*treeify_props(RRR,HAD->RO):- member(R,RRR),member(HAS,R),
  make_unifible(HAS,HAD), maplist(p1_not_not(member(HAD)),RRR),
  maplist(p1_not_not(member(HAD)),RRR),
  findall(H->RObjL,(member(H,HHH),
    findall(RObj,(member(Obj,RRR),select(H,Obj,RObj)),RObjL),
  treeify_props(RR,RO). %,RR\=[[]|_],!.*/
treeify_props(RRR, OUTPUT):- is_list(RRR), fail,
  flatten(RRR,Props),
  ku_rewrite_props(Props,GSS), 
  care_to_count(GSS,PropsSet),
  %count_each(PropsSet,Props,CountOfEachL),
  %predsort(sort_on(arg(2)),CountOfEachL,CountOfEach),
  %pp(countOfEach=CountOfEach),  
  maplist(make_unifiable_cc,PropsSet,UPropsSet),
  map_pred(var_to_underscore,UPropsSet,UPropsSetG),
  variant_list_to_set(UPropsSetG,UPropsSetGSet),
  count_each(UPropsSetGSet,UPropsSetG,GroupsWithCountsL),
  variant_list_to_set(GroupsWithCountsL,GroupsWithCountsLVS),
  predsort(sort_on(arg(2)),GroupsWithCountsLVS,GroupsWithCounts),!,
  variant_list_to_set(GroupsWithCounts,GroupsWithCountsW),
  sort(GroupsWithCountsW,KS), [_N-HAD|_] = KS,
  findall(HAD,member(HAD,PropsSet),HHH),!,
  treeify_props_these_next(HAD,HHH,RRR,OUTPUT).

treeify_props(RRR,OUT):- flatten(RRR,GF),
  once((sort_safe(GF,GSS),
  length(RRR,DontDivOnThisNumber),
  care_to_count(GSS,GS),
  count_each(GS,GF,UC),keysort(UC,KS),member(N-HAD,KS),N\==1,N\==DontDivOnThisNumber, \+ dont_divide_on(HAD),
  my_partition(member(HAD),RRR,HAVES,HAVENOTS),
  length_s(HAVES,HL),length_s(HAVENOTS,NL))), HL>0, NL>0,
  variance_had(HAD,RRR,_Versions,Variance), Variance == 1,
  maplist(variant_select(HAD),HAVES,HAVESS),%HAVESS\=[[]|_],
  treeify_props(HAVESS,FHAVES),
  treeify_props(HAVENOTS,FHAVENOTS),
  FHAVENOTS\=[],
 (HL>NL -> OUT = [ (not5(HAD)=NL/HL/Variance)->FHAVENOTS , HAD->FHAVES ] ;
    OUT = [ (yes51(HAD)=HL/NL/Variance)->FHAVES ,  (not51(HAD)=NL/HL/Variance)->FHAVENOTS ]),!.

treeify_props(RRR,[ (yes0(HAD)=HL/NL/Variance)->FHAVES ,  (not0(HAD)=NL/HL/Variance)->FHAVENOTS ]):- flatten(RRR,GF),
  once((sort_safe(GF,GSS),
  length(RRR,DontDivOnThisNumber),
  care_to_count(GSS,GS),
  count_each(GS,GF,UC),keysort(UC,KS),member(N-HAD,KS),N\==DontDivOnThisNumber, \+ dont_divide_on(HAD),
  my_partition(member(HAD),RRR,HAVES,HAVENOTS), 
  length_s(HAVES,HL),length_s(HAVENOTS,NL))), HL>1, NL>1,
  variance_had(HAD,RRR,_Versions,Variance), 
  maplist(variant_select(HAD),HAVES,HAVESS),%HAVESS\=[[]|_],
  treeify_props(HAVESS,FHAVES),
  treeify_props(HAVENOTS,FHAVENOTS),
  FHAVENOTS\=[].

treeify_props(RRR,[ (not11(HAD)=NL/HL)->FHAVENOTS ,  (yes11(HAD)=HL/NL)->FHAVES ]):- flatten(RRR,GF),
  sort_safe(GF,GSS), 
  length(RRR,DontDivOnThisNumber),
  care_to_count(GSS,GS),
  count_each(GS,GF,UC),keysort(UC,KS),member(N-HAD,KS),N\==DontDivOnThisNumber,N\==1, \+ dont_divide_on(HAD),
  my_partition(member(HAD),RRR,HAVES,HAVENOTS), 
  length_s(HAVES,HL),length_s(HAVENOTS,NL), HL>1, NL==1,
  maplist(variant_select(HAD),HAVES,HAVESS), 
  treeify_props(HAVESS,FHAVES),
  treeify_props(HAVENOTS,FHAVENOTS),
  FHAVENOTS\=[].

treeify_props(RRR,[ (yes1(HAD)=HL/NL)->FHAVES ,  (not1(HAD)=NL/HL)->FHAVENOTS ]):- flatten(RRR,GF),
  sort_safe(GF,GSS), 
  length(RRR,DontDivOnThisNumber),
  care_to_count(GSS,GS),
  count_each(GS,GF,UC),keysort(UC,KS),member(N-HAD,KS),N\==DontDivOnThisNumber, \+ dont_divide_on(HAD),
  my_partition(member(HAD),RRR,HAVES,HAVENOTS), 
  length_s(HAVES,HL),length_s(HAVENOTS,NL), HL==1, NL>1,
  variance_had(HAD,RRR,_Versions,Variance), Variance==2,
  maplist(variant_select(HAD),HAVES,HAVESS), 
  treeify_props(HAVESS,FHAVES),
  treeify_props(HAVENOTS,FHAVENOTS),
  FHAVENOTS\=[].

treeify_props(RRR, OUT):- member(R,RRR), member(HAD,R), \+ dont_divide_on(HAD),
  length(RRR,DontDivOnThisNumber), variance_had(UProp,HAD,RRR,Versions,Missing,Variance),
  Variance\==DontDivOnThisNumber,!,
  must_det_ll(treeify_versions(UProp,Versions,RRR,Missing,OUT)).



/*
%treeify_props(RRR,R):- predsort(using_compare(sort),RRR,RR),RRR\=@=RR,!,treeify_props(RR,R).
treeify_props(RRR,[One|R]):- select(One,RRR,RRRR),treeify_props(RRRR,R).
*/
treeify_props(RRR,RR):- attempt_min_unifier(RRR,R),RRR\=@=R,!,treeify_props(R,RR).
treeify_props(RRR,RR):- sort_vertically(RRR,R), always_attempt_min_unifier(R,RR).

variant_member(E,List):- member(V,List),E=@=V,!.
%make_unifiable_cc(cc(C,N),cc(C,N)):-!.
make_unifiable_cc(O,U):- make_unifiable(O,U).
treeify_versions(UProp,[Prop|Versions],RRR,Missing,[(Prop->HAVESSTREE)|OUTS]):-
  my_partition(variant_member(Prop),RRR,HAVES,HAVENOTS),
  maplist(variant_select(Prop),HAVES,HAVESS),
  treeify_props(HAVESS,HAVESSTREE),
  treeify_versions(UProp,Versions,HAVENOTS,Missing,OUTS).
treeify_versions(_,[],_,[],[]):-!.
treeify_versions(UProp,[],_,Missing,[missing(UProp)->MissingTREE]):-
  treeify_props(Missing,MissingTREE).

always_attempt_min_unifier(R,RRRR):- attempt_min_unifier(R,RRRR),!.
always_attempt_min_unifier(R,R).

attempt_min_unifier(R,UProp-UProps->RRRR):-
  member(O,R),member(Prop,O),
  make_unifiable_cc(Prop,UProp),
  findall(UProp,(member(RO,R),member(UProp,RO)),UProps),
  some_min_unifier(UProps,Common),nonvar(Common),
  attempt_min_unifier_select(Common,UProps,R,RRRR).

attempt_min_unifier_select(Common,_UProps,RR,Common-Actuals->RRRR):-
  maplist(select_variant_or_unify,Common,Actuals,RR,RRR),!,
  treeify_props(RRR,RRRR).

attempt_min_unifier_select(Common,UProps,RR,Common-UCommons->RRRR):-
  maplist(select_safe_always(UProps),Common,UCommonsL,RR,RRR),append(UCommonsL,UCommons),
  treeify_props(RRR,RRRR).

select_safe_always(_UProps,E,[O],L,R):- select_variant_or_unify(E,O,L,R),!.
select_safe_always( UProps,_,[O],L,R):- member(E,UProps), select_variant_or_unify(E,O,L,R),!.
select_safe_always( _,_,[],L,L).

variant_select(HAD,R,RR):- select(H,R,RR), H=@=HAD.

variant_select_always(HAD,R,RR):- select(H,R,RR), H=@=HAD,!.
variant_select_always(_,R,R).

select_variant_or_unify(HAD,U,R,RR):- variant_select(HAD,R,RR),!,U=HAD.
select_variant_or_unify(HAD,H,R,RR):- select(H,R,RR), shall_count_as_same(HAD,H). 


treeify_props_these_next(HAD,HHH,RRR,HAD->OUTPUT):-
  findall(H->RObjLO,(member(H,HHH),h_for(H,RRR,RObjLO)),OUTPUT).

dont_divide_on(links_count(sees,_)).
dont_divide_on(occurs_in_links(sees,_)).
%dont_divide_on(grid_ops(comp,_)).
%dont_divide_on(grid_ops(norm,_)).
dont_divide_on(iz(i_o(_))).
dont_divide_on(giz(_)).
dont_divide_on(oid(_)).
dont_divide_on(was_oid(_)).

h_for(H,RRR,RObjLO):- 
  findall(RObj,(member(Obj,RRR),select(H,Obj,RObj)),RObjL),
  treeify_props(RObjL,RObjLO).

/*
  my_partition(member(HAD),RRR,HAVES,HAVENOTS), 
  length_s(HAVES,HL),length_s(HAVENOTS,NL),
  maplist(variant_select(HAD),HAVES,HAVESS),%HAVESS\=[[]|_],
  treeify_props(HAVESS,FHAVES),
  treeify_props(HAVENOTS,FHAVENOTS),
  FHAVENOTS\=[].
*/

%:- functor(VV,vv,9009),nb_linkval(vv9009,VV).
:- length(VV,9009),nb_linkval(vv9009,VV).
variant_copy(V,C):- ground(V),!,V=C.
variant_copy(V,C):- copy_term(V,C),term_variables(C,CTV),append(CTV,_,OVV),nb_current(vv9009,VV),OVV=VV,!.

sort_vertically(RRR,RR):- \+ is_list(RRR),!,RR=RRR.
sort_vertically(RRR,RRO):- maplist(sort_vertically,RRR,RRR0),list_to_set(RRR0,R),predsort(first_equals(variant,sort_on(get_loc2D)),R,RR),reverse(RR,RRO),!.
get_loc2D(List,loc2D(V,H)):- sub_term(E,List),compound(E),E=loc2D(H,V),!. 
get_loc2D(P,loc2D(inf,P)). 



/*
prior_name_by_size(VM,IndvS0,Named):- 
  Title = save_as_obj_group(Named),
  if_t((true;number(_R)),
   (length_s(IndvS0,Len),
    if_t(Len>0,
     must_det_ll((
      %add_prior_placeholder(Named,IndvS0,IndvS1),
      override_object(birth(Named),IndvS0,IndvS1)
      gset(VM.objs)=IndvS1,
     /*
      %rank_priors(Named,IndvS1,IndvSL),
      gset(VM.objs)=IndvSL,
      nop((( Named == i_nsew ) -> DebugThis = full ; luser_getval(debug,DebugThis)->true; DebugThis=true)),
      nop(( DebugThis\==false -> Feedback = show_indiv(Title) ; Feedback = print_info)),
      nop(ignore((DebugThis\==false, print_grid(VM.h,VM.v,Title,IndvSL)))),     
      %dash_chars,
      */
      addObjectOverlap(VM,IndvSL)))))),!.
*/
/*
%add_prior_placeholder(_,I,I):-!.
add_prior_placeholder(Named,IndvS0,IndvS9):- is_list(IndvS0),!,
  length_s(IndvS0,Len),
  maplist(add_prior_placeholder(Len,Named),IndvS0,IndvS9).

add_prior_placeholder(_Len,Named,IndvS0,IndvS9):- 
  override_object(birth(Named),IndvS0,IndvS9),!.

add_prior_placeholder(Len,Named,IndvS0,IndvS9):- 
  (has_prop(pg(OG,Len,_,Named),IndvS0)-> IndvS0=IndvS9 ; 
    ((has_prop(pg(OG,Was,Other,Named),IndvS0)-> delq(IndvS0,pg(OG,Was,Other,Named),IndvS1) ; IndvS0=IndvS1),
     override_object(pg(OG,Len,nil,Named),IndvS1,IndvS9))),!.
*/

object_get_priors(X,S):- var(X),!, enum_object(X), object_get_priors(X,S).
object_get_priors(X,S):- is_object(X), !, must_det_ll((indv_props_list(X,Ps),
  findall(I,(member(P,Ps),props_object_prior(P,I)),L),L\==[],list_to_set(L,S))).

get_prior_labels(Objs,PriorsWithCounts):- must_det_ll((is_list(Objs),
  findall(Named,(member(Obj,Objs),object_get_priors(Obj,Named)),AllPriorsL),
  append(AllPriorsL,AllPriors),
  sort_safe(AllPriors,PriorsSet),
  my_partition(never_prior,PriorsSet,_Unused,PriorsSetClean),
  count_each(PriorsSetClean,AllPriors,PriorsWithCounts))).

never_prior(giz(_)).
never_prior(oid(_)).
%never_prior(pg(_,_,_,_)).


ranking_pred(rank1(F1),I,Value):- Prop=..[F1,Value], indv_props_list(I,Ps),member_or_iz(Prop,Ps),!.
ranking_pred(rank1(F1),I,Value):- !, catch(call(F1,I,Value),_,fail),!.
ranking_pred(rankA(F1),I,Value):- append_term(F1,Value,Prop), indv_props_list(I,Ps),member_or_iz(Prop,Ps),!.
ranking_pred(rankA(F1),I,Value):- !, catch(call(F1,I,Value),_,fail),!.
ranking_pred(rank2(F1),I,Value):- Prop=..[F1,O1,O2], indv_props_list(I,Ps),member_or_iz(Prop,Ps),!,combine_number(F1,O1,O2,Value).
%ranking_pred(rank2(F1),I,Value):- !, catch(call(F1,I,O1,O2),_,fail),!,combine_number(F1,O1,O2,Value).
ranking_pred(_F1,I,Value):- mass(I,Value).

has_prop(Prop,Obj):- var(Obj),!, enum_object(Obj),has_prop(Prop,Obj).
has_prop(Prop,Obj):- is_grid(Obj),!,grid_props(Obj,Props),!,member(Prop,Props).
has_prop(Prop,Objs):- is_list(Objs),!,forall(member(Obj,Objs),has_prop(Prop,Obj)).
has_prop(_Props,Obj):- is_cons(Obj),!,ds,fail.

has_prop(Prop,ObjRef):- \+ is_object(ObjRef),!,atom(ObjRef),into_obj(ObjRef,Obj),!,has_prop(Prop,Obj).




has_prop(Prop,Obj):- var(Prop), !, indv_props_list(Obj,L), member(Prop,L).
%has_prop(Var,_Obj):- var(Var),!, fail.
has_prop(Prop,Obj):- indv_props_list(Obj,L), member(Q,L), (Q=@=Prop -> true ; ( Q = Prop)).
has_prop(lbl(Lbl),Obj):- !, is_prior_prop(Lbl,Obj).

has_prop(Lbl ,Obj):- atom(Lbl),!, is_prior_prop(Lbl,Obj),!.
has_prop(and(A,B),Obj):- !, has_prop(A,Obj),has_prop(B,Obj).
has_prop(call_1(A),Obj):- !, has_prop(AA,Obj),call(A,AA).
has_prop(not(A),Obj):- !, \+ has_prop(A,Obj).
has_prop(or(A,B),Obj):- !, (has_prop(A,Obj);has_prop(B,Obj)).
has_prop(rank1(Lbl),Obj):- atom(Lbl),!, is_prior_prop(rank1(Lbl),Obj),!.
%has_prop(rank2(Lbl),Obj):- atom(Lbl),!, is_prior_prop(rank2(Lbl),Obj),!.
has_prop(rankA(Lbl),Obj):- nonvar(Lbl),!, is_prior_prop(rankA(Lbl),Obj),!.

never_a_prior(P):- var(P),!,fail.
never_a_prior(link).
never_a_prior(info).
never_a_prior(by_color).
never_a_prior(nsew).
never_a_prior(colormass).

never_a_prior(giz(X)):- !, fail, never_a_prior(X).
never_a_prior(iz(X)):- !, never_a_prior(X).
never_a_prior(P):- compound(P),functor(P,What,_),never_a_prior(What).
never_a_prior(P):- rankNever(P).

props_object_prior(V,_):- var(V),!,fail.
props_object_prior(Prop,_):- never_a_prior(Prop),!,fail.
%props_object_prior(pg(OG,_,_,L),O):- props_object_prior(L,O)
props_object_prior(pg(_OG,L,_,_),L):-!.
props_object_prior(mass(_),rank1(mass)).


props_object_prior(cc(Color,_Value),rankA(cc(Color))):-!.
props_object_prior(birth(S),L):- !, fail, first_atom_value(S,L), L\= indiv(_), \+ never_a_prior(L).
props_object_prior(iz(S),iz(L)):- !, props_object_prior(S,L).
%props_object_prior(iz(S),L):- !, props_object_prior(S,L).
props_object_prior(S,L):- S\=info(_), first_atom_value(S,L), \+ rankOnly(L), \+ never_a_prior(L).

first_atom_value(S,S):- atom(S),!.
first_atom_value(S,_):- \+ compound(S),!,fail.
first_atom_value(S,rank1(F)):- S=..[F,A],comparable_value(A).
%first_atom_value(S,rank2(F)):- S=..[F,A,B],F\=='/', comparable_value(A),comparable_value(B).
first_atom_value(S,O):- arg(1,S,E),atomic(E),E\==[],!,O=S, O \= (_/_).
first_atom_value(S,O):- arg(2,S,E),first_atom_value(E,O),O \= (_/_).

%comparable_value(A):- compound(A),!,A=(_/_).
comparable_value(A):- is_color(A),!.
comparable_value(A):- number(A),!.
%comparable_value(A):- atom(A),!.


rankOnly(center2G(_,_)).
rankOnly(loc2D(_,_)).

rankNever(vis2G(_,_)).



is_prior_prop(Lbl,Obj):- object_prior(Obj,L),L=Lbl,!.
%is_prior_prop(Lbl,Obj):- has_prop(Lbl,Obj),!.
object_prior(Obj,E):- object_get_priors(Obj,L),member(E,L).

add_priors([Lbl-Count|PriorsWithCounts],Objs,LF):- number(Count), !, add_prior(Count,Lbl,Objs,Mid),!, add_priors(PriorsWithCounts,Mid,LF).
add_priors([Count-Lbl|PriorsWithCounts],Objs,LF):- number(Count), !, add_prior(Count,Lbl,Objs,Mid),!, add_priors(PriorsWithCounts,Mid,LF).
add_priors([Lbl|PriorsWithCounts],Objs,LF):- !, add_prior(_,Lbl,Objs,Mid),!, add_priors(PriorsWithCounts,Mid,LF).
add_priors(_,IO,IO).

add_prior(N,Lbl,Objs,ObjsWithPrior):- 
  %is_list(Objs),
  %print_list_of(ppnl,Title,N),
  my_partition(is_prior_prop(Lbl),Objs,Lbld,Unlbl),  
  %add_prior_placeholder(Lbl,Lbld,RLbld),
  length_s(Lbld,LL),  
  rank_priors(Lbl,Lbld,RLbldR),
  nop(print_grid(Lbl->N/LL,RLbldR)),
  % write('\t '), writeq(Lbl->N/LL),write(' <p/>\t'),
  append([Unlbl,RLbldR],ObjsWithPrior).  


/*
prior_name_by_size(_VM,[],_Name):-!.
prior_name_by_size(VM,IndvS0,Named):-  
  rank_priors(Named,IndvS0,IndvSL),
  addObjectOverlap(VM,IndvSL).
*/
rank_priors(_RelivantDivide,GType,FG,SFO1):-
  rank_priors(GType,FG,SFO1).

rank_priors(GType,Objs,SFO):-
 relivant_divide(RelivantDivide),
 my_partition(RelivantDivide,Objs,FG,BG),
 (FG\==[],BG\==[]),!,
 rank_priors(RelivantDivide,GType,FG,SFO1),
 rank_priors(inv(RelivantDivide),GType,BG,SFO2),
 append(SFO1,SFO2,SFO).

rank_priors(GType,Objs,SFO):-
 must_det_ll((
   %add_prior_placeholder(GType,Group,Objs),
   smallest_first(smallest_pred(GType),Objs,SF),
   ignore((relivant_group(OG), maplist(OG,Objs))),
   length_s(SF,SFLen),
   nop(SFLen < 2 -> pp(red,rank_priors(GType,SFLen)); pp(green,rank_priors(GType,SFLen))),  
   reverse(SF,SFR),
   add_rank(OG,GType,SFLen,SFR,SFO),
   nop(maybe_show_ranking(GType,SFO)))).


maybe_show_ranking(GType,SFO):-
 dash_chars,dash_chars,dash_chars,dash_chars,
 maplist(object_grid,SFO,G),
  length_s(SFO,Len),
  print_list_of(pp,setOF(GType),SFO),
  print_ss(G),
  u_dmsg(setOF(GType)=Len),atrace.


%has_order(O,P1, Code ):- 
% Code = ((
add_rank(OG,GType,ZType,IndvS,IndvSO):- add_rank(OG,GType,ZType,1,IndvS,IndvSO).

add_rank(_OG,_GType,_ZType,_,[],[]):-!.
%add_rank(OG,_GType,_ZType,1,IO,IO):-!.

add_rank(_OG,_GType,_ZType,N,[L],[L]):-  N==1, !.
%add_rank(OG,GType,ZType,N,[L],[Obj]):-   N>1, !, value_sym(firstOF,FO),set_rank(OG,GType,ZType,L,FO,Obj).
%add_rank(OG,GType,ZType,N,[L],[Obj]):-  N==1, !, set_rank(OG,GType,ZType,L,N,Obj),!. %firstAndLastOF
%add_rank(OG,GType,ZType,N,[L|IndvS],[Obj|IndvSO]):- value_sym(lastOF,_LO),set_rank(OG,GType,ZType,L,1,Obj), 
% N2 is N+1,!, add_rank(OG,GType,ZType,N2,IndvS,IndvSO).
add_rank(OG,GType,ZType,N,[L|IndvS],[Obj|IndvSO]):- % (GType = rank1(_);GType = rank2(_)),!, 
 set_rank(OG,GType,ZType,L,/*nthOF**/(N),Obj), 
 N2 is N+1,!, add_rank(OG,GType,ZType,N2,IndvS,IndvSO).
add_rank(OG,GType,ZType,N,[L|IndvS],  [L|IndvSO]):- 
 N2 is N+1,!, add_rank(OG,GType,ZType,N2,IndvS,IndvSO).

value_sym(firstOF,1).
value_sym(lastOF,1).
set_rank(OG,GType,ZType,L,N,Obj):-
  get_setarg_p1(nb_setarg,I,L,P1), 
  compound(I), I = pg(OG,GType,ZType,_),
  II = pg(OG,GType,ZType,N), 
  call(P1 ,II),!, Obj = L.

set_rank(OG,GType,ZType,L,N,Obj):- 
   II = pg(OG,GType,ZType,N), 
   override_object([II],L,Obj),!.


