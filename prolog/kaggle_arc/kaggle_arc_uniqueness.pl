/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

/*
Props [Not] Shared Between

  - All Input Grids
  - All Output Grids

  - All Grids

  - Pair #1 Input
  - Pair #1 Output
  - Pair #1 Input and Output

  - Pair #2 Input
  - Pair #2 Output
  - Pair #2 Input and Output

  - Pair #1 Input and Pair #2 Output

*/
interesting_group_name('All Input Grids',all,[input]).
interesting_group_name('All Output Grids',all,[output]).
interesting_group_name('All Grid I/O',all,[]).
interesting_group_name('Pair #1 Input', [trn+0],[input]).
interesting_group_name('Pair #1 Output',[trn+0],[output]).
interesting_group_name('Pair #1 I/O',   [(_>(trn+0)*in),(_>(trn+0)*out)],[]).
interesting_group_name('Pair #2 Input', [(_>(trn+1)*in)],[]).
interesting_group_name('Pair #2 Output',[(_>(trn+1)*out)],[]).
interesting_group_name('Pair #2 Input vs Pair #2 Output',[(_>(trn+1)*in),(_>(trn+1)*out)],[]).
interesting_group_name('Pair #1 Input vs Pair #2 Output',[(_>(trn+0)*in),(_>(trn+1)*out)],[]).
interesting_group_name('Pair #1 Input vs Pair #2 Input', [(_>(trn+0)*in),(_>(trn+1)*in)],[]).
interesting_group_name('Test #1 Input vs Pair #1 Input', [(_>(trn+0)*in),(_>(tst+0)*in)],[]).
interesting_group_name('Test #1 Input', [tst+0],[input]).

select_some_objects(TestID,Trn,Num,IO,Filter,Objects):-
  test_grouped_io(TestID,[(TestID>(Trn+Num)*IO)],[],Objs),
  reorganize_objs(Objs,Shared,PropsUnique,PropsDistibuted),
  named_filter_proptyple(Filter,Objs,Shared,PropsUnique,PropsDistibuted,Objects).


pair_two_groups(TestID,Name1+Filter1,Name2+Filter2,Objs1,Objs2):-
  select_group(TestID,Name1,Filter1,Objs1),
  select_group(TestID,Name2,Filter2,Objs2).

select_group(TestID,Name,Filter,Objects):-  
  interesting_selectors(Name,Trn,Num,IO),
  select_some_objects(TestID,Trn,Num,IO,Filter,Objects).
interesting_selectors('All Input',trn,_,in).
interesting_selectors('All Output',trn,_,out).
interesting_selectors('Pair #1 Out',trn,0,in).
interesting_selectors('Pair #1 In',trn,0,out).
interesting_selectors('Pair #1 I/O',trn,0,_).
interesting_selectors('Pair #2 Out',trn,1,in).
interesting_selectors('Pair #2 In',trn,1,out).
interesting_selectors('Pair #2 I/O',trn,1,_).
interesting_selectors('Test #1 In',tst,0,in).

%named_filter_proptyple(Filter,Objs,Shared,PropsUnique,PropsDistibuted,Objects).
named_filter_proptyple(whole,Objs,_,_,_,Objs).
named_filter_proptyple(shared,_,Objs,_,_,Objs).
named_filter_proptyple(distribted,_,_,_,Objs,Objs).
named_filter_proptyple(Else,_,_,L,_,[Else|Objs]):- select(Else,L,Rest), findall(Else,member(Else,Rest),Objs).
  
reorganize_objs([obj(O)|Objects],Shared,PropsUnique,PropsDistibuted):-!,
  maplist(into_obj_plist,[obj(O)|Objects],RawPropLists),
  maplist(treed_props_list,RawPropLists,PropLists),!,
  reorganize_objs(PropLists,Shared,PropsUnique,PropsDistibuted).

reorganize_objs(Objects,[Prop|Shared],PropsUnique,PropsDistibuted):-
  maplist(variant_select,Prop,Objects,Next),
  sort(Objects,SObjects),sort(Next,SNext), SObjects\=@=SNext,!,
  reorganize_objs(Next,Shared,PropsUnique,PropsDistibuted).
reorganize_objs(Objects,Shared,[alone(OID,AloneProps)|PropsUnique],PropsDistibuted):-
  select(Obj,Objects,Rest),
  member(oid(OID),Obj),
  findall(Prop,
    (member(Prop,Obj), make_unifiable(Prop,UProp), \+ ((member(RO,Rest),member(UProp,RO)))),
     AloneProps),
  include(not_in(AloneProps),Obj,NewObj),
  Next = [NewObj|Rest],
  sort(Objects,SObjects),sort(Next,SNext), SObjects\=@=SNext,!,
  reorganize_objs(Next,Shared,PropsUnique,PropsDistibuted).

reorganize_objs(Objects,Shared,[unique(OID,NewObj,Prop)|PropsUnique],PropsDistibuted):-
  select(Obj,Objects,Rest),
  maplist(variant_select,Prop,Rest,NewRest),
  member(oid(OID),Obj),
  make_unifiable(Prop,UProp), 
  select(Obj,UProp,NewObj),
  Next = [NewObj|NewRest],
  sort(Objects,SObjects),sort(Next,SNext), SObjects\=@=SNext,!,
  reorganize_objs(Next,Shared,PropsUnique,PropsDistibuted).
reorganize_objs(Objects,Shared,[missing(OID,Prop)|PropsUnique],PropsDistibuted):-
  select(Obj,Objects,Rest),
  maplist(variant_select,Prop,Rest,NewRest),
  member(oid(OID),Obj),
  Next = [Obj|NewRest],
  sort(Objects,SObjects),sort(Next,SNext), SObjects\=@=SNext,!,
  reorganize_objs(Next,Shared,PropsUnique,PropsDistibuted).
/*
reorganize_objs(Objects,Shared,PropsUnique,[Versions|PropsDistibuted]):-
  length(Objects,Len),member(Obj,Objects),member(Prop,Obj),  
  variance_had(Prop,Objects,Versions,Variance), Variance = Len,
  reorganize_objs([RestProps|Rest],Shared,PropsUnique,PropsDistibuted).
*/
reorganize_objs(Objects,[],[],Objects).



show_groups(TestID):- ensure_test(TestID),
  forall(interesting_group_name(Name,Ors,Ands),
   once((show_info_about_objects(TestID,Name,Ors,Ands)))),
  show_pair_groups(TestID).
  

show_pair_groups(TestID):-
  forall(pair_two_groups(TestID,Name1+Filter1,Name2+Filter2,Objs1,Objs2),
    ignore((
      Objs1\==[],Objs2\==[],
      Objs1\==Objs2,            
      Filter1\==whole,Filter2\==whole,
      (Name1==Name2->Filter1\==Filter2;true),
      (Filter1==Filter2->Name1\==Name2;true),
      functor(Filter1,F1,_),functor(Filter2,F2,_), %F1==F2,  
      F1\==alone,F2\==alone,
      append(Objs1,Objs2,OBJS),
      pp(Name1+Filter1-Name2+Filter2
         = Objs1->Objs2),
      show_interesting_named_props(Name1+Filter1-Name2+Filter2,OBJS)
      ))).

rules_from(Objs1,Objs2,Objects):- Objects=(Objs1->Objs2).

show_info_about_objects(TestID,Name,Ors,Ands):-
  test_grouped_io(TestID,Ors,Ands,IO),
  w_section(Name,show_interesting_named_props(Name,IO)).


test_grouped_io(TestID,ExampleNum,IO,Objs):-
  findall(Indv,
   (is_why_grouped_g(TestID,_Count,_Why,IndvSG),
    maplist(must_oid_to_object,IndvSG,IndvS),     
     member(Indv,IndvS),
    (is_list(ExampleNum)->once((member(OR,ExampleNum),sub_term(E,Indv),nonvar(E),E=OR));true),
    \+ \+ ((forall(member(Trn,IO),sub_var(Trn,Indv))))),
   IndvSIO),
  list_to_set(IndvSIO,IndvSet),
   Objs = IndvSet.
/*
test_grouped_io(TestID,ExampleNum,IO,Objs):-
  Example+Num = ExampleNum,
  is_why_grouped_g(TestID,_Count, individuate(complete,two(ID,_)),IndvSG),
  once(testid_name_num_io(ID,TestID,Example,Num,_)),
   maplist(must_oid_to_object,IndvSG,IndvS),
   (var(IO)->member(IO,[in,out]);true),
   once(include(sub_var(IO),IndvS,IndvSIO)),
   Objs = IndvSIO.
*/

test_grouped(TestID,ExampleNum,I,O):-  
  test_grouped_io(TestID,ExampleNum,in,I),
  once(test_grouped_io(TestID,ExampleNum,out,O)).


saved_group(Why,IndvS):-
  is_why_grouped(_TestID,_Count,Why,IndvS).

is_why_grouped(TestID,Count,Why,IndvSO):-
  is_why_grouped_g(TestID,Count,Why,IndvSG),
  maplist(must_oid_to_object,IndvSG,IndvS),!,
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

%select_group(TestID,Group,How):- no_repeats(Group,select_group0(TestID,Group,How)).
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
  my_append(Group1,Group2,GroupJ), sort_safe(GroupJ,Group),
  How = [How1,How2])) 
    *-> true ; is_why_grouped(TestID,_,How,Group).

select_group0(TestID,Group,obj_cache):- findall(O,obj_cache(TestID,O,_),GroupJ), GroupJ\==[], sort_safe(GroupJ,Group).

:- arc_history(test_what_unique).
test_what_unique:- get_current_test(TestID), what_unique(TestID,n=0,n>10).


:- arc_history((get_current_test(TestID),what_unique(TestID,n=0,n>10))).
get_new_uniq_dict(Dict):- 
    ArgDict = _{sharedWith:_SharedWith,object:_Obj,trait:_Trait,groupSizeMask:_GroupSizeMask,
  actualGroupSize:_ActualGroupSize,countMask:_CountMask,
  actualCount:_ActualCount,otherL:_OtherL,slistL:_ListL,
  setL:_SetL,others:_TraitCountSets,how:_How,group:_Group},
  (var(Dict)->Dict=ArgDict ; Dict >:< ArgDict).

is_fti_step(most_unique).
most_unique(symmetry_type,VM):-
  List = VM.objs,
  last(List,Obj),
  set(VM.solution)= Obj.


  

what_unique:- get_current_test(TestID),what_unique(TestID).

what_unique(TestID):- 
   get_vm(VM),
   ((VM.id \= (TestID > _ * _)), ndividuator),
   get_vm(VM2), explain_uniqueness(VM2.objs).

what_unique(TestID,Dict):- is_vm_map(Dict),!,what_unique_dict(TestID,Dict).
what_unique(TestID,Obj):- get_current_test(TestID),select_group(TestID,Group,_How), member(Obj,Group), must_det_ll(what_unique(TestID,Obj,Group)).
what_unique(TestID,Obj,Group):- (is_group(Group);is_object(Obj)),!,what_unique_obj(TestID,Obj,Group).
what_unique(TestID,CountMask,GroupSizeMask):-
  get_new_uniq_dict(Dict),
  Dict.groupSizeMask = GroupSizeMask,
  Dict.countMask = CountMask,!,
  what_unique_dict(TestID,Dict),
  report_unique(Dict).

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

is_peerless_prop(Peers,Prop):- \+ sub_var(Prop,Peers).
not_peerless_prop(Peers,Prop):- sub_var(Prop,Peers).

what_unique_obj:- get_current_test(TestID),what_unique_obj(TestID,_,_).
what_unique_obj(TestID,Obj,Group):- 
  get_new_uniq_dict(Dict),
  Dict.group = Group,
  Dict.object = Obj,
  what_unique_dict(TestID,Dict),
  report_unique(Dict).

/*what_unique(TestID,CountMask,GroupSizeMask):-
 what_unique(TestID,SharedWith,Obj,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,TraitCounts,How),
 report_unique(SharedWith,Obj,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,TraitCounts,How).
*/
report_unique(Dict):- var(Dict),get_new_uniq_dict(Dict),!,report_unique(Dict).
report_unique(Dict):- var(Dict.actualCount),!,get_current_test(TestID), what_unique_dict(TestID,Dict),report_unique(Dict).
report_unique(Dict):-
 must_det_ll((
  ArgDict = _{sharedWith:SharedWith,object:Obj,trait:Trait,groupSizeMask:GroupSizeMask,
  actualGroupSize:ActualGroupSize,countMask:CountMask,
  actualCount:ActualCount,otherL:OtherL,listL:ListL,
  setL:SetL,others:TraitCountSets,how:How,group:Group},
  (var(Dict)->Dict=ArgDict ; Dict >:< ArgDict),
  maplist_e(tersify,TraitCountSets,HTraitSetO),
  maplist_e(tersify,SharedWith,SharedWithO),
  maplist_e(tersify,Group,GroupO),
  maplist_e(tersify,Obj,ObjO),
  %(Obj\==[] -> ignore(print_grid(Obj)) ; true),
  format('~N'), pp(what_unique(ObjO=[ActualCount/ActualGroupSize-Trait],sharedWith=SharedWithO,
  setL/listL=SetL/ListL,others=HTraitSetO,how=How,
  groupSizeMask=GroupSizeMask,group:GroupO,countMask=CountMask,otherL=OtherL)))).

maplist_e(P2,A,B):- is_list(A),!,mapgroup(P2,A,B).
maplist_e(P2,A,B):- call(P2,A,B).

:- style_check(-singleton).
%:- arc_history(what_unique(TestID,SharedWith,Obj,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,Others,_How)).
:- style_check(+singleton).


obj_exclude(Obj,Group,Others):- var(Obj),!,select(Obj,Group,Others).
obj_exclude(Obj,Group,Others):- select(O,Group,Others),(O==Obj *-> true; Group=Others).


what_unique_dict(TestID,Dict):- 
  ArgDict = _{sharedWith:SharedWith,object:Obj,trait:Trait,groupSizeMask:GroupSizeMask,
  actualGroupSize:ActualGroupSize,countMask:CountMask,
  actualCount:ActualCount,otherL:OtherL,listL:ListL,
  setL:SetL,others:TraitCountSets,how:How,group:Group},
  (var(Dict)->Dict=ArgDict ; Dict >:< ArgDict),
  (var(Group)->(select_group(TestID,Group,How));true),
  obj_exclude(Obj,Group,Others),  
  length_criteria(Group,GroupSizeMask),
  length(Group,ActualGroupSize),
  mapgroup(each_trait,[Obj|Others],[_-ObjT|TraitList]),
  member(Trait,ObjT),
  \+ too_non_unique(Trait),
  \+ too_unique(Trait),
  found_in_o(Trait,TraitList,SharedWith),
  length_criteria(SharedWith,CountMask),
  length(SharedWith,ActualCount),
   freeze(B,\+ \+ (member(E,SharedWith), E==B)),
   my_partition(=(B-_),TraitList,_Mine,NotMine),
   length(NotMine,OtherL),   
   %dif(WTrait,Trait),
   functor(Trait,F,A),functor(WTrait,F,A),
   found_in_w(WTrait,NotMine,HTraitList),length(HTraitList,ListL),
   sort_safe(HTraitList,HTraitSet),length(HTraitSet,SetL),
   findall(C-HTrait,(member(HTrait,HTraitSet),found_in_w(HTrait,NotMine,LS),length(LS,C)),TraitCounts),
   sort_safe(TraitCounts,TraitCountSets),
   \+ filter_what_unique(TestID,SharedWith,Obj,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,How).


explain_uniqueness(GroupWhole):-
  object_printables(GroupWhole,Group,GroupPP),
  get_current_test(TestID),!,
  forall(member(Obj,Group),
   (dash_chars,
    object_glyph(Obj,G), object_color_glyph_short(Obj,GC), object_grid(Obj,OG), 
    locally(nb_setval(color_index,[Obj|GroupPP]),print_side_by_side(GC,GroupPP,'explain_uniqueness',_,OG,G)),
    dmsg(uobj=Obj),!,
    forall(what_unique_obj(TestID,Obj,Group),true))),
  dash_chars.


% touching vs each dir
% size2D



:- style_check(-singleton).
filter_what_unique(TestID,SharedWith,Obj,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,How):-
  OtherL=<1.

filter_what_unique(TestID,SharedWith,Obj,Trait,GroupSizeMask,ActualGroupSize,CountMask,ActualCount,OtherL,ListL,SetL,How):-
 ListL=SetL, SetL>1.


/*

With each type of example we can have...

values_all_same|values_all_dif
values_where_1_stand_otherwise
values_where_2_stand_otherwise

*/



compare_objects([],[]):-!.
compare_objects(Objs,Interesting):- 
  maplist(indv_props_for_noteablity,Objs,ObjProps),
  flatten(ObjProps,FlatProps),
  maplist(functorize_props,FlatProps,Functors),
  sort_safe(Functors,SortedFunctors),
  gather_props(SortedFunctors,FlatProps,ListOfLists),
  maplist(compare_values,ListOfLists,Diffs),
  include(\=([]),Diffs,Interesting).
  
functorize_props(iz(Prop),FA):- !, functorize_props(Prop,FA).
functorize_props(Prop,F/A):- functor(Prop,F,A).
gather_props([F/A|SortedFunctors],FlatProps,[(F-Candidates)|ListOfLists]):- 
  functor(Match,F,A), findall(Match,(member(Match,FlatProps);member(iz(Match),FlatProps)),Candidates),
  gather_props(SortedFunctors,FlatProps,ListOfLists).
gather_props([],_,[]).


compare_values(F-X,Notable):- predsort_using_only(number_varz,X,S),length(X,N),length(S,NS),
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
is_prop_for_noteablity(X):- \+ never_noteable(X),!.

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
