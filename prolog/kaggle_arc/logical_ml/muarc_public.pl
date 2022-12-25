/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


dont_change(Var):- copy_term(Var,VarC),freeze(VarC,fail), freeze(Var,Var=@=VarC).


select_subgroup(Objs,GroupName,Count,SubObjs):- 
  into_group(Objs,GObjs),
  findall(O-GroupName,(member(O,GObjs),is_in_subgroup(O,GroupName)),Memberships),
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
prop_name(Prop,Name):- compound_name_arguments(Prop,F,[A|_]),
   (number(A)-> Name =F ; compound_name_arguments(Name,F,[A])).

object_group_cc(Objs,GroupName,SubObjs,Count,NamesCC,ValuesCC):-
  select_subgroup(Objs,GroupName,Count,SubObjs),
  objects_names_props(SubObjs,Props),
  maplist(arg(1),Props,Names),get_ccs(Names,NamesCC),
  maplist(arg(2),Props,Values),get_ccs(Values,ValuesCC).

is_in_subgroup(Obj,Prop):- var(Obj),!, enum_object(Obj),is_in_subgroup(Obj,Prop).
is_in_subgroup(Obj,iz(bg_obj)):- has_prop(cc(fg,0),Obj).
is_in_subgroup(Obj,iz(fg_obj)):- has_prop(cc(bg,0),Obj).
is_in_subgroup(Obj,iz(single_fg_color)):- has_prop(fg_colors_count(1),Obj).
is_in_subgroup(Obj,iz(multicolor)):- has_prop(fg_colors_count(Two),Obj),Two>1.
is_in_subgroup(Obj,ansestors(N,Set)):-transitive_sets(ansestor,Obj,Set,N).
is_in_subgroup(Obj,descendants(N,Set)):-transitive_sets(descendant,Obj,Set,N).
%is_in_subgroup(Obj,tiouching(N,Set)):- nontransitive_set(touching,Obj,Set,N).
%is_in_subgroup(Obj,seeing(N,Set)):- nontransitive_set(seeing,Obj,Set,N).
is_in_subgroup(Obj,insideOf(N,Set)):-transitive_sets(insideOf,Obj,Set,N).
is_in_subgroup(Obj,contains(N,Set)):-transitive_sets(contains,Obj,Set,N).
%is_in_subgroup(Obj,Prop):- has_prop(Prop,Obj).
%is_in_subgroup(_,all).




transitive_sets(P2,Obj,Set,N):- findall(n(P,List),(trans(P2,Obj,List),List\==[],length(List,P)),Lists),sort(Lists,Set),length(Set,N).
nontransitive_set(P2,Obj,Set,N):- findall(Other,call(P2,Obj,Other),List),sort(List,Set),length(Set,N).

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
contains(Obj,Other):- has_prop(link(contains,Other,_),Obj).

%show_indiv(_Why,Obj):- is_bg_object(Obj),!.
show_indiv(Why, Obj):-  
  format('~N'),dash_chars(45),dash_chars(45),
  must_det_ll((
  vis2D(Obj,H,V),   

  DoFF = false,

  findall(SubGroup,is_in_subgroup(Obj,SubGroup),SubGroupS), 
  pp(subGroupS=SubGroupS),

  %writeg(obj=Obj),

  if_t((H\==1;V\==1;true),
    must_det_ll((     
     object_or_global_grid(Obj,LG+Type,Grid),      
     Title = show_indiv(Why,objFn(Glyph),LG+Type,loc2D(OH,OV),center2G(CX,CY),size2D(H,V)),
             loc2D(Obj,OH,OV), ignore(center2G(Obj,CX,CY)), object_glyph(Obj,Glyph),
     Grids = [Title=Grid|_],     

     copy_term(Obj,CObj),
     nop((object_ngrid(CObj,NGrid), append(_,["NGrid"=NGrid|_],Grids))),

     ShowQ=_,


     term_variables(Grid,GV1),
     normalize_grid(NOps,Grid,Normalized), % if_t(Normalized\=@=Grid,append(_,["NORMALIZED!!!"=Normalized|_],Grids)),
     term_variables(Normalized,RV1),
     nop(((((GV1\=@=RV1 ; (Normalized\=@=Grid,Normalized=[[_]])) -> ShowQ = true ; ShowQ = _)))),

     compress_grid(COps,Grid,Compressed), if_t(Compressed\=@=Normalized,append(_,["Compressed!!!"=Compressed|_],Grids)),

     if_t(DoFF,((constrain_grid(f,_TrigF,Grid,GridFF), if_t(GridFF\=@=Grid,append(_,["Find"=GridFF|_],Grids)),
       copy_term(Grid+GridFF,GG1+GridFFNV,GoalsFF), numbervars(GG1+GridFFNV+GoalsFF,10,_,[attvar(bind),singletons(false)])))),

     append(_,[],Grids),
     wots(SS, print_side_by_side(Grids)), replace_in_string(['®'=Glyph,'@'=Glyph],SS,S),     
     HH is (OH - 1) * 2, print_w_pad(HH,S),

     if_t(has_goals(GridFFNV),writeg(gridFF=GridFFNV)),

     if_t((nonvar(COps),COps\==[]), 
       (writeg(cops=COps), 
        nop((unreduce_grid(Compressed,COps,Uncompressed), 
        if_t(Uncompressed\=@=Grid, (ShowQ=true,writeg("Bad Uncompressed"=Uncompressed))))))),

     if_t((nonvar(NOps),NOps\==[]), 
       (writeg(nops=NOps),
        if_t((ShowQ==true;Normalized\=@=Grid;has_goals(Normalized);true), writeg(normalized=Normalized)),
        nop((unreduce_grid(Normalized,NOps,Unnormalized), 
        if_t(Unnormalized\=@=Grid, (ShowQ=true,writeg("Bad Unnormalized"=Unnormalized))))))),
     
     if_t((ShowQ==true;has_goals(Grid)),    writeg(grid=Grid)),
     

     %writeg("NGrid"=NGrid),
   true))),
    
  if_t(is_object(Obj),
    (format('~N~n'),
     locally(nb_setval(debug_as_grid,nil),underline_print(debug_indiv(Obj))))),
     format('~N'),dash_chars(15))),!.

has_goals(G):- term_attvars(G,AV),AV\==[].
has_goals(G):- term_variables(G,TV),term_singletons(G,SV),TV\==SV.

writeg(Term):- \+ \+ writeg0(Term).

writeg0(Term):- 
  term_attvars(Term,Attvars), Attvars\==[],!,
  term_variables(Term,Vars),
  include(not_in(Attvars),Vars,PlainVars),   
  copy_term((Attvars+PlainVars+Term),(AttvarsC+PlainVarsC+TermC),Goals),
  numbervars(PlainVarsC,10,Ten1,[singletons(true)]),
  numbervars(AttvarsC+Goals,Ten1,_Ten,[attvar(bind),singletons(false)]),
  writeg0(TermC), wots(S,writeg0(Goals)), print_w_pad(2,S).

writeg0(N=V):- format('~N'),nonvar(N), pp_no_nl(N),writeln(' = '), !, wots(S,writeg0(V)), print_w_pad(2,S).
writeg0(V):- is_gridoid(V),!,print_grid(V),wots(S,(maplist(writeg1,V))), print_w_pad(2,S).
writeg0(O):- compound(O),compound_name_arguments(O,F,[A]),!,wots(S,((writeq(F),write('('),writeg3(A),write(')')))), print_w_pad(1,S).
writeg0([H|T]):- compound(H),H=(_=_), maplist(writeg0,[H|T]).
writeg0([H|T]):- is_list(T),wots(S,((write('['),writeg2(H),maplist(writeg0,T),write(']')))), print_w_pad(1,S).
%writeg0(Term):- \+ ground(Term),!, \+ \+ (numbervars(Term,99799,_,[singletons(true)]),
%   subst(Term,'$VAR'('_'),'$VAR'('_____'),TermO), writeg0(TermO)).
%writeg0(V):- \+ is_list(V),!,writeq(V),nl.
writeg0(V):- \+ is_list(V),!,pp(V).
writeg0(X):- wots(S,pp(X)), print_w_pad(2,S).

writeg1(X):- format('~N'),writeg2(X),!,write(' '),!.
writeg2(X):- write_term(X,[quoted(true),quote_non_ascii(true),portrayed(false),nl(false),numbervars(false)]),!.
%writeg1(X):- format('~N'),writeg(X).
writeg2(X):- writeq(X),!.
writeg3(X):- is_list(X),X\==[],X=[_,_|_],!,writeg(X).
writeg3(X):- writeg2(X).

