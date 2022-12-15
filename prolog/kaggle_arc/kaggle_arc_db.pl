/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- include(kaggle_arc_header).

%:- dynamic(row_mem/34).
:- dynamic(oindv/3).
:- dynamic(oindv/4).
:- dynamic(oindv/5).
:- dynamic(cindv/3).
:- dynamic(cindv/4).
:- dynamic(cindv/5).
:- dynamic(gid_glyph_oid/3).

%show_time_gt_duration(Goal):- show_time_gt_duration(0.23,Goal,true). 
% 793,184,238 inferences, 160.894 CPU in 160.897 seconds (100% CPU, 4929859 Lips)
show_time_gt_duration(Timespan,Goal):- show_time_gt_duration(Timespan,Goal,true). 
show_time_gt_duration(Timespan,Goal,Goal2):- 
  get_time(Now),setup_call_cleanup(true,Goal,maybe_report_time(Timespan,Now,Goal,Goal2)).

maybe_report_time(Timespan, Was,Goal,Goal2):- get_time(Now), Elapse is Now - Was,
  ignore((Elapse>Timespan , 
    pp(show_time_gt_duration(Elapse>Timespan,Goal)),
    once(Goal2 ),
    pp(yellow,maybe_report_time(Elapse>Timespan,Goal)))).

neg_h_v_area(size2D(H,V),VAL):- NArea is - (H * V),  max_min(H,V,Hi,_Lo), DifHV is - abs(H-V)*Hi, VAL is NArea+DifHV.

:- abolish(l_s_4sides,2).
:- abolish(s_l_4sides,2).

:- dynamic(l_s_4sides/2).
:- dynamic(s_l_4sides/2).

predsort_using_only(P2,List,Sorted):- predsort(using_compare(P2),List,Sorted).
using_compare(C,R,A,B):- (A==B-> R=(=) ; must_det_ll((call(C,A,AA),call(C,B,BB),!,compare(R,AA,BB)))).
predsort_on(P2,List,Sorted):- predsort(sort_on(P2),List,Sorted).
sort_on(C,R,A,B):- (A==B-> R= (=) ; must_det_ll((call(C,A,AA),call(C,B,BB),!,compare(R,AA+A,BB+B)))).

:- 
   findall(size2D(HV,HV),between(1,32,HV),SizesSquareS),
   findall(size2D(H,V),(between(1,32,H),between(1,32,V),H\=V),SizesRect),
   predsort_on(neg_h_v_area,SizesRect,SizesRectS),
   %reverse(SizesSquareS,SizesSquareR), reverse(SizesRectS,SizesRectR),
  % list_to_set([size2D(3,3),size2D(2,2)|SizesSquareS],Sizes_L_S),
   append(SizesSquareS,SizesRectS,AllSizes),   
   predsort_on(neg_h_v_area,AllSizes,Sizes_L_S),

   forall(member(size2D(H,V),Sizes_L_S),assertz(l_s_4sides(H,V))),
   reverse(Sizes_L_S,Sizes_S_L),
   forall(member(size2D(H,V),Sizes_S_L),assertz(s_l_4sides(H,V))),
   !.

erase_grid(GID):- 
  %id_grid_cells(GID,Grid)
  pfc_retractall(cmem(GID,_,_)), 
  forall(pfc_retract(gid_glyph_oid(GID,_,OID)), erase_obj(OID)).
erase_obj(OID):-
   pfc_retractall(oindv(OID,_,_)),
   pfc_retractall(oindv(OID,_,_,_)),
   pfc_retractall(oindv(OID,_,_,_,_)).

assert_id_cells(ID,Points):- maplist(assert_id_cell(ID),Points).
assert_id_cell(ID,-(C,HV)):- assert(cmem(ID,HV,C)).


:- dynamic(is_grid_size/3).
% Grid to_fast_workspace


%assert_id_grid_cells(Grid):- is_grid(Grid),grid_to_gid(Grid,GID),!,assert_id_grid_cells(GID,Grid).
%assert_id_grid_cells(GID):- oid_to_gridoid(GID,Grid), assert_id_grid_cells(GID,Grid).
ensure_gid(Grid,GID):- atom(Grid),!,GID=Grid.
ensure_gid(Grid,GID):- assert_id_grid_cells(GID,Grid). 
/*
assert_id_grid_cells2(ID,_SH,_SV,Grid):- is_grid(Grid),!,
 forall(nth1(N,Grid,Row),
   forall(list_to_row_mem(ID,N,Row,Mem),arc_assert(Mem))).
*/
assert_id_grid_cells(GID, Grid):- var(GID),grid_to_gid(Grid,GID),!,assert_id_grid_cells(GID,Grid).
assert_id_grid_cells(GID,_Grid):- nonvar(GID), is_grid_size(GID,_,_),!.
assert_id_grid_cells(GID, Grid):- var(Grid),!,oid_to_gridoid(GID,Grid),!,assert_id_grid_cells(GID,Grid).
assert_id_grid_cells(GID, Grid):-
 %throw(all_in_emem(assert_id_grid_cells(GID,Grid))),
   grid_size(Grid,SH,SV),
   %((cmem(GID,_,_);gid_glyph_oid(GID,_,_))-> erase_grid(GID) ; true),
   retractall(is_grid_size(GID,_,_)),
   %retractall(is_cmem(GID,_,_)),retractall(is_cmem_hv(GID,_,_,_)),
   retractall(cmem(GID,_,_)),retractall(cmem_hv(GID,_,_,_)),
   assert(is_grid_size(GID,SH,SV)),
   assert_id_grid_cells2(GID,SH,SV,Grid),
   cache_grid_objs(GID),
   nop(show_grid_objs(GID)).

assert_id_grid_cells2(ID,SH,SV,Grid):-
   forall(between(1,SH,H),
    forall(between(1,SV,V),
     ignore((hv_c_value(Grid,C,H,V),
       assert_hvc_cell(ID,H,V,C))))).
assert_hvc_cell(_,_,_,C):- plain_var(C). % free_cell(C),!.
%assert_hvc_cell(ID,H,V,C):- hv_point(H,V,HV),assert(is_cmem(ID,HV,C)),assert(is_cmem_hv(ID,H,V,C)).
assert_hvc_cell(ID,H,V,C):- hv_point(H,V,HV),assert(cmem(ID,HV,C)),assert(cmem_hv(ID,H,V,C)).

%:- dynamic(is_cmem/3).
%cmem(GID,HV,C):- is_cmem(GID,HV,C),C\==black.
:- dynamic(smem/3).

get_color_at(H,V,Grid,C):-
  ((nth1(V,Grid,Row),nth1(H,Row,C))*->true;C=_).

get_color_at_point(Grid,Point,C):- hv_point(H,V,Point), get_color_at(H,V,Grid,C).
  
:- dynamic(tid_to_gids/2).
tid_to_gids(TID,GID) :- awc,!, (clause(tid_to_gids(TID,GID),true)*-> true ; term_to_oid(TID,GID)).

find_test_gids(TestID,Type,GID) :- awc,!, find_test_grids(TestID,Type,Grid), grid_to_gid(Grid,GID).

find_test_grids(TestID,visible,Grid):- test_grids(TestID,Grid).
find_test_grids(TestID,train,Grid):- kaggle_arc_io(TestID,trn,_,Grid).
find_test_grids(TestID,test_input,Grid):- kaggle_arc_io(TestID,tst,in,Grid).
find_test_grids(TestID,train_input,Grid):- kaggle_arc_io(TestID,trn,in,Grid).
find_test_grids(TestID,train_output,Grid):- kaggle_arc_io(TestID,trn,out,Grid).

term_to_oid(v(A)>(B+C)*D,Atom):- maplist(atomic,[A,B,C,D]),atomic_list_concat([v,A,B,C,D],'_',Atom),!.
term_to_oid(t(A)>(B+C)*D,Atom):- maplist(atomic,[A,B,C,D]),atomic_list_concat([t,A,B,C,D],'_',Atom),!.
term_to_oid(T,A):- (compound(T)->term_to_atom(T,A);(atom(T)->T=A;term_to_atom(T,A))).

point_to_hvc(Var,_,_,_):- var(Var),!,fail.
point_to_hvc(Point,  H,V,fg):- atomic(Point),!, hv_point(H,V,Point).
%point_to_hvc(CD-Point,H,V,C):- var_or_color_data(CD,C),must(hv_point(H,V,Point)),!.
point_to_hvc(CD-Point,H,V,CD):- atomic(Point),!, hv_point(H,V,Point),!.
%point_ to_hvc(H,V,_,H,V).
%point_ to_hvc(Inf,Inf,offset_ranges(_,_,_,_)).
var_or_color_data(CD,C):- only_color_data(CD,C),!.
var_or_color_data(C,C).


make_grid(H,V,Grid):- (H<1;V<1),!,wdmsg(make_grid(H,V,Grid)),break,!,fail.
make_grid(H,V,Grid):- between(1,40,H),between(1,40,V),  % max_min(H,0,HH,_), max_min(V,0,VV,_), %max_min(HH,32,_,HHH),max_min(VV,32,_,VVV),!,    
   ensure_make_grid(H,V,G),G=Grid.

ensure_make_grid(H,V,Grid):- make_grid_cache(H,V,Grid),!. 
ensure_make_grid(H,V,Grid):- make_fresh_grid(H,V,Grid), assertz(make_grid_cache(H,V,Grid)).

make_fresh_grid(1,1,[[_]]):-!.
make_fresh_grid(_,0,[]):- !.
make_fresh_grid(0,1,[[]]):-!.
make_fresh_grid(0,N,Grid):- N>1,!, make_list([],N,Grid).
make_fresh_grid(H,V,Grid):- length(Grid,V), maplist(make_lengths(H),Grid),!.


% Grid vis2D/resize
make_lengths(N,L):- length(L,N).

:- dynamic(make_grid_cache/3).
make_grid_cache:-
 my_time((forall(s_l_4sides(GH,GV), make_grid(GH,GV,_)))).

insert_row(N,Row,Grid,NewGrid):- grid_size(Grid,H,V), insert_row(N,Row,Grid,H,V,NewGrid).
insert_row(N,Row,Grid,H,V,NewGrid):- N<0, NewN is V + N+1,!,insert_row(NewN,Row,Grid,H,V,NewGrid).
insert_row(N,Row,Grid,H,_,NewGrid):- length(Row,H),length(Left,N),append(Left,Right,Grid),append(Left,[Row|Right],NewGrid).

insert_col(N,Col,Grid,NewGrid):- grid_size(Grid,H,V), insert_col(N,Col,Grid,H,V,NewGrid).
insert_col(N,Col,Grid,H,V,NewGrid):- N<0, NewN is H + N+1,!,insert_col(NewN,Col,Grid,H,V,NewGrid).
insert_col(N,Col,Grid,_,V,NewGrid):- length(Col,V),maplist(insert_col_at(N),Col,Grid,NewGrid).


insert_col_at(N,Col,Row,NewRow):- length(Left,N),append(Left,Right,Row),append(Left,[Col|Right],NewRow).

insert_ele(N,V,L,NL):- length(Left,N),append(Left,Right,L),append(Left,[V|Right],NL).

delete_row(N,Grid,NewGrid):- N < 0, length(Grid,L), DR is L+N+1,delete_row(DR,Grid,NewGrid).
delete_row(N,Grid,NewGrid):- length(Left,N),append(Left,[_|Right],Grid),append(Left,Right,NewGrid).

delete_col(N,Grid,NewGrid):- maplist(delete_row(N),Grid,NewGrid).

map_nth(P,N,Grid):- nth1(N,Grid,E),call(P,E).
map_row(P,N,Grid):- map_nth(maplist(P),N,Grid).
map_col(P,N,Grid):- maplist(map_nth(P,N),Grid).


maybe_glyph(G,_,Glyph):- is_object(G), object_glyph(G,Glyph), !.
maybe_glyph(_G,N,Code):- i_glyph(N,Code),!.
maybe_glyph(G,_,Glyph):- is_grid(G),grid_dot(Glyph),!.
maybe_glyph(_,N,N).

is_visible(Obj):- \+ has_prop(iz(flag(hidden)),Obj).

is_pred_sorted_object_grid(O):- last(O,I), \+ is_visible(I).

object_printables(Objs,GroupVis,GroupPP):- 
 %is_pred_sorted_object_grid(Objs),
 !,GroupVis=GroupPP,GroupVis=Objs.
  
object_printables(Objs,GroupVis,GroupPP):-
  visible_first(Objs,SF),
  my_partition(is_visible,SF,GroupVis,GroupInv),
  append([GroupVis,GroupInv,Objs],GroupP),list_to_set(GroupP,GroupPP),!.

grid_color_and_glyph(Points,C,GN,H,V):- %is_object_group(Points), 
  object_printables(Points,_,ObjList),
  gridoid_color(ObjList,C,H,V),
  gridoid_glyph(ObjList,GN,H,V),!.
grid_color_and_glyph(Points,C,GN,H,V):- %is_object_group(Points), 
  gridoid_color(Points,C,H,V),
  gridoid_glyph(Points,GN,H,V),!.

gridoid_color(Points,C,H,V):-
 ((nb_current(color_index,ObjList),ObjList\==[]) -> ObjList=List ; Points=List),
  from_gridoid(List,C,_N,H,V,_G),!.

gridoid_glyph(Points,GN,H,V):-
 ((nb_current(color_index,ObjList),ObjList\==[]) -> ObjList=List ; Points=List),
  from_gridoid(List,_C,N,H,V,G), maybe_glyph(G,N,GN).

%from_gridoid(Points,C,GN,H,V):- from_gridoid(Points,C,N,H,V,G), maybe_glyph(G,N,GN).
%from_gridoid(Points,C,N,H,V,G):- nth1(N,Points,G),hv_c_value(G,C,H,V),nonvar_or_ci(C), \+ is_bg_color(C), \+ bg_sym(C), !.
%from_gridoid(Points,C,N,H,V,G):- nth1(N,Points,G),hv_c_value(G,C,H,V),nonvar_or_ci(C),!.
from_gridoid(Points,C,N,H,V,G):- nth1(N,Points,G), \+ cant_use(G),hv_c_value(G,C,H,V).

cant_use(G):- is_object(G), has_prop(G,iz(bfc(bg))),!.

%hv_c_value(O,_Color,_H,_V):- is_object(O), iz(O,iz(info(combined))), !, fail.
hv_c_value(O,_Color,_H,_V):-  plain_var(O),!,fail.
hv_c_value(O,_Color,_H,_V):-  is_ftVar(O),!,fail.
hv_c_value(O,Color,H,V):- is_cpoint(O),!,O=(Color-Point),hv_point(H,V,Point),!.
hv_c_value(In,C,H,V):- compound(In),In=in(O),!,hv_c_value(O,C,H,V).
hv_c_value(O,Color,H,V):- is_grid(O),!,nth1(V,O,Row),nth1(H,Row,Color),!.
hv_c_value([],_Color,_H,_V):-  !,fail.
hv_c_value(diff(_-> New),C,H,V):-!,hv_c_value(New,C,H,V).
hv_c_value(diff(_),_C,_H,_V):-!, fail.
hv_c_value(O,C,_H,_V):- is_colorish(O),!,C=O.
hv_c_value(O,GN,H,V):- is_map(O),O.objs\==[],!,hv_c_value(O.objs,GN,H,V).
hv_c_value(O,GN,H,V):- is_map(O),!,hv_c_value(O.grid,GN,H,V).

hv_c_value(ID,C,H,V):- (var(H);var(V)),!,arcST,trace, hv_point(H,V,_),hv_c_value(ID,CC,H,V),CC=C.
hv_c_value(O,Color,H,V):- is_object(O),!,globalpoints(O,Ps),hv_c_value(Ps,Color,H,V).
hv_c_value(O,Color,H,V):- is_list(O), is_cpoints_list(  O),!,hv_point(H,V,Point),member(Color-Point,O).
hv_c_value(O,FGL   ,H,V):- is_list(O), maplist(is_nc_point,O),!,hv_point(H,V,Point),member(Point,O),get_fg_label(FGL).
hv_c_value(O,FGL   ,H,V):- is_nc_point(O),!,O=Point,hv_point(H,V,Point),!,get_fg_label(FGL).

%hv_c_value(G,Color,H,V):- is_group(G),!,into_list(G,L),member(E,L),hv_c_value(E,Color,H,V),!.
%hv_c_value(O,Color,H,V):- known_gridoid(O,G),!,hv_c_value(G,Color,H,V).
hv_c_value(G,Color,H,V):- my_assertion(into_list(G,L)),!,member(E,L),hv_c_value(E,Color,H,V),!.
%hv_c_value(O,Color,H,V):- is_object(O),localpoints(O,Ps),hv_c_value(Ps,Color,H,V).
%hv_c_value(L,Color,H,V):- is_list(L), member(E,L),hv_c_value(E,Color,H,V),!.

point_c_value(Point,C,Grid):- hv_point(Point,H,V),hv_c_value(Grid,C,H,V).


hv_cg_value(O,_Color,_H,_V):-  var(O),!,fail.
hv_cg_value(ID,C,H,V):- (var(H);var(V)),!, hv_point(H,V,_),hv_cg_value(ID,CC,H,V),CC=C.
hv_cg_value(Grid,Color,H,V):- is_grid(Grid),!,nth1(V,Grid,Row),nth1(H,Row,Color).
hv_cg_value(O,GN,H,V):- is_map(O),O.objs\==[],!,hv_cg_value(O.objs,GN,H,V).
hv_cg_value(O,GN,H,V):- is_map(O),!,hv_cg_value(O.grid,GN,H,V).
hv_cg_value(O,Color-GN,H,V):- is_object(O), hv_c_value(O,Color,H,V),obj_to_oid(O,GN),nonvar_or_ci(GN),!.

hv_cg_value([G|Points],CN,H,V):- quietly(( is_list(Points), is_object_or_grid(G))), 
   grid_color_and_glyph([G|Points],C,N,H,V),CN=(C-N),!.
%hv_cg_value(O,CN,H,V):- (has_index(color_index);has_index(glyph_index)),
%   grid_color_and_glyph(O,C,N,H,V),CN=(C-N),!.


%hv_cg_value(ID,C,H,V):- nonvar(H) -> (hv_point(H,V,HV),cmem(ID,HV,C)); (cmem(ID,HV,C),hv_point(H,V,HV)).

%hv_cg_value(Points,C,H,V):- is_list(Points),member(C-HV,Points),hv_point(H,V,HV).

hv_cg_value_or(Grid,C,_H,_V,Else):- (Grid==[];var(Grid)),!,Else=C.
hv_cg_value_or(Grid,C,H,V,Else):- hv_cg_value(Grid,C,H,V)*->true;C=Else.

hv_c_value_or(Grid,C,_H,_V,Else):- (Grid==[];var(Grid)),!,Else=C.
hv_c_value_or(Grid,C,H,V,Else):- hv_c_value(Grid,C,H,V)*->true;C=Else.

pgt:- clsmake,!,pgt(Obj),once(print_grid(Obj)).

pgt(Obj):- pgt1(Obj).
pgt(Obj):- pgt2(Obj).
pgt(Obj1-Obj2):- pgt1(Obj1),pgt2(Obj2).
pgt([Obj1]-[Obj2]):- pgt1(Obj1),pgt2(Obj2).
pgt([Obj1,Obj2]):- pgt1(Obj1),pgt2(Obj2).
pgt1(Obj):-
  Obj = obj( [ amass(536),
         colorless_points( [ point_01_01, point_02_01]),
         colors( [ cc(red, 190.0), cc(silver, 132.0), cc(green, 55.0), cc(cyan, 53.0),
                   cc(blue, 45.0), cc(yellow, 36.0), cc(orange, 25.0)]),
         localpoints( [ red-point_01_01, silver-point_02_01]), vis2D(3, 1), rot2L(sameR), loc2D(3, 1),
         changes([]), iz(info(combined)),
         iz(shape(rectangle)), iz(multicolored),
         iz(shape(polygon)), %obj _to_oid(v('0ad4ef5')>(trn+0)*in, 21),
       %  globalpoints( [ red-point_01_01, silver-point_02_01]),
         grid_size(8, 8)]).

pgt2(Obj):- Obj = 
      obj( [ amass(536),
         colorless_points( [ point_01_01, point_02_01]),
         colors( [ cc(red, 190.0), cc(silver, 132.0), cc(green, 55.0), cc(cyan, 53.0),
                   cc(blue, 45.0), cc(yellow, 36.0), cc(orange, 25.0)]),
         localpoints( [ red-point_01_01, silver-point_02_01]), vis2D(3, 1), rot2L(sameR), loc2D(2, 1),
         changes([]), iz(info(combined)),
         iz(shape(rectangle)), iz(multicolored),
         iz(shape(polygon)), %obj _to_oid(v('a1d4ef5')>(trn+0)*in, 66),
        %  globalpoints( [ red-point_01_01, silver-point_02_01]),
         grid_size(8, 8)]).

%hv_cg_value(ID,C,H,V):- row_mem_nth(H,ID,V,C).

has_index(CI):-  luser_getval(CI,List),List\==[],is_list(List),!.


hv_member(HV,C,O):- is_grid(O),!,fail,globalpoints(O,Ps),hv_member(Ps,C,HV),!.
hv_member(HV,C,O):- is_object(O),!,globalpoints(O,Ps),hv_member(Ps,C,HV),!.
hv_member(HV,C,Points):- member(C-HV,Points),!.
% hv_member(HV,C,Points):- sub_term(C-HV,Points),!.
/*b_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),set_nth1(H,Row,C).
nb_set_hv_value(Grid,C,H,V):- nth1(V,Grid,Row),nb_set_nth1(H,Row,C).
b_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),rplc_nth1(H,Row,OldC,NewC).
nb_rplc_hv_value(Grid,OldC,NewC,H,V):- nth1(V,Grid,Row),nb_rplc_nth1(H,Row,OldC,NewC).

*/

my_assertion_is_color(_):-!.
my_assertion_is_color(X):- my_assertion(is_color(X)).
%replace_local_hvcpoint(_H,_V,NewC,OldC,Point,G,GO):-nonvar_or_ci(G),!,G=obj(L),is_list(L),
replace_global_hvc_point(H,V,NewC,OldC,Grid,GridO):- is_grid(Grid),!, my_assertion_is_color((NewC)), replace_grid_point(H,V,NewC,OldC,Grid,GridO).
replace_global_hvc_point(H,V,NewC,OldC,G,GO):- hv_point(H,V,Point), must_det_ll(replace_global_point_color(Point,NewC,OldC,G,GO)).

%replace_global_hvc_point(Point,OldC,G,GO):- trace_or_throw(unknown_target_type(replace_global_hvc_point(Point,OldC,G,GO))).

replace_global_point_color(Point,NewC,OldC,G,GO):- is_object(G), !,
    globalpoints(G,Points),
    replace_in_points(Point,NewC,OldC,Points,RPoints),
    setq(G,[globalpoints(RPoints)],GO).
replace_global_point_color(Point,NewC,OldC,G,GO):- is_grid(G),!, point_to_hvc(Point,H,V,_),my_assertion_is_color((NewC)), 
  replace_grid_point(H,V,NewC,OldC,G,GO),!.
replace_global_point_color(Point,NewC,OldC,G,GO):- is_list(G),!, maplist(replace_global_point_color(Point,NewC,OldC),G,GO).
replace_global_point_color(Point,NewC,OldC,G,GO):- replace_local_point_color(Point,NewC,OldC,G,GO).


replace_in_points(Point,NewC,OldC,G,GO):- (var(NewC);is_bg_color(NewC)),!, ((select(OldC-Point,G,GO), \+ is_bg_color(OldC))->true; GO=G).
replace_in_points(Point,NewC,OldC,G,GO):- select(C-Point,G,Rest), !, ((\+ \+ OldC = C )-> GO= [NewC-Point|Rest] ; GO=G).
replace_in_points(Point,NewC,OldC,G,GO):- attvar(OldC),!, (\+ (OldC \= noexisting(Point,NewC))-> GO= [NewC-Point|G] ; G=GO).
replace_in_points(Point,NewC,OldC,G,GO):- (var(OldC);is_bg_color(OldC)),!, GO= [NewC-Point|G].



%replace_local_points(Obj,Grid,GridO):- is_group(Obj), localpoints(Obj,Points),replace_local_points(Points,Grid,GridO).
%replace_local_points([H|T],Grid,GridO):- is_points_list([H|T]), !, replace_local_points([H|T],Grid,GridO).
%replace_local_points([H|T],Grid,GridO):- !, replace_local_points(H,Grid,GridM),replace_local_points(T,GridM,GridO).

%replace_grid_points([],_,Grid,Grid):-!.
replace_grid_points(List,Grid,GridO):- replace_local_points(List,_OldC,Grid,GridO).
replace_grid_points(List,OldC,Grid,GridO):- replace_local_points(List,OldC,Grid,GridO).

replace_local_points(Nil,_OldC,G,G):- Nil ==[], !.
replace_local_points(Obj,OldC,Grid,GridO):- is_grid(Obj),!, localpoints_include_bg(Obj,Points),replace_local_points(Points,OldC,Grid,GridO).
replace_local_points([H|T],OldC,G,GO):- !,  replace_local_points(H,OldC,G,MGO), replace_local_points(T,OldC,MGO,GO). 
replace_local_points(Obj,OldC,Grid,GridO):- is_object(Obj), localpoints(Obj,Points),replace_local_points(Points,OldC,Grid,GridO).

replace_local_points(Point,OldC,G,GO):- is_grid(G),!, point_to_hvc(Point,H,V,NewC),my_assertion_is_color((NewC)), 
  replace_grid_point(H,V,NewC,OldC,G,GO),!.
replace_local_points(Point,OldC,G,GO):- point_to_hvc(Point,H,V,NewC),nop(my_assertion_is_color((NewC))), 
  hv_point(H,V,Colorless),
  must_det_ll(replace_local_point_color(Colorless,NewC,OldC,G,GO)),!.
replace_local_points(Point,OldC,G,GO):- trace_or_throw(unknown_target_type(replace_local_points(Point,OldC,G,GO))).

replace_local_point_color(Point,NewC,OldC,G,GO):- is_points_list(G),!, replace_in_points(Point,NewC,OldC,G,GO).
replace_local_point_color(Point,NewC,OldC,G,GO):- is_list(G),!, maplist(replace_local_point_color(Point,NewC,OldC),G,GO).
replace_local_point_color(Point,NewC,OldC,G,GO):- is_object(G), !,
    localpoints(G,Points),     
    replace_in_points(Point,NewC,OldC,Points,RPoints),
    %loc2D(G,OH,OV),offset_point(OH,OV,Point,LPoint),colorless_points(G,NCPoints), maplist(replace_in_points(Point,NewC,OldC),NCPoints,RNCPoints),,colorless_points(RNCPoints)
    setq(G,localpoints(RPoints),GO).
replace_local_point_color(Point,NewC,OldC,G,GO):- trace_or_throw(unknown_target_type(replace_local_point_color(Point,NewC,OldC,G,GO))).





create_movements:- 
 show_time_gt_duration(0.22,(
 forall( between(1,30,H),
  forall(between(1,30,V),
  calc_movement(H,V))))).

:- initialization(create_movements).

:- dynamic(hv_point/3).
:- dynamic(is_adjacent_point/3).
:- dynamic(is_adjacent_hv/5).
:- export(hv_point/3).
:- export(is_adjacent_point/3).
:- export(is_adjacent_hv/5).

hv(H,V,hv(H,V)).

calc_movement(H,V):- forall((nav(Dir,HO,VO),Dir\==c), save_calc_movement(H,V,Dir,HO,VO)).

save_calc_movement(H,V,Dir,HO,VO):- H2 is HO+H, V2 is VO+V,
  muarc_mod(M),
  ignore((between(1,32,H2), between(1,32,V2), 
     format(atom(HV),'point_~|~`0t~d~2+_~|~`0t~d~2+',  [H,V]),
     format(atom(HV2),'point_~|~`0t~d~2+_~|~`0t~d~2+', [H2,V2]),
     %hv_point(H,V,HV),
     %hv_point(H2,V2,HV2),
    assert_if_new(M:is_adjacent_point(HV,Dir,HV2)),
    assert_if_new(M:hv_point(H,V,HV)),
    assert_if_new(M:is_adjacent_hv(H,V,Dir,H2,V2)))).
  
is_adjacent_2points(HV,Dir,HV2,HV3):-  is_adjacent_point(HV,Dir,HV2),is_adjacent_point(HV2,Dir,HV3).

create_points_plus:- show_time_gt_duration(0.3,create_points_plus_now).
create_points_plus_now:- 
 forall(
  (between(1,30,H1),between(1,30,V1),
   between(1,30,H2),between(1,30,V2)),
  (H is H1 + H2 -1,
   V is V1 + V2 -1,
   ignore(
    (hv_point(H, V, HV),
     hv_point(H1,V1,HV1),
     hv_point(H2,V2,HV2),
     assert_point_plus_if_needed(HV1,HV2,HV))))).


:- dynamic(is_point_plus/3). % number_of_clauses(214426)

assert_point_plus_if_needed(HV1,HV2,HV):- point_plus(HV1,HV2,HV),!.
assert_point_plus_if_needed(HV1,HV2,HV):- assert(is_point_plus(HV1,HV2,HV)).

point_plus(point_01_01,HV,HV):- hv_point(_,_,HV).
point_plus(HV,point_01_01,HV):- hv_point(_,_,HV).
point_plus(HV1,HV2,HV):- is_point_plus(HV1,HV2,HV).

point_minus(HV,HV2,HV1):- point_plus(HV1,HV2,HV).
%point_minus(HV,HV2,HV1):-point_plus(HV2,HV1,HV),!.

:- initialization(create_points_plus).

is_adj_point_es(HV1,HV2):- is_adjacent_point(HV1,s,HV2).
is_adj_point_es(HV1,HV2):- is_adjacent_point(HV1,e,HV2).
is_adj_point_es_d(HV1,HV2):- is_adj_point_es(HV1,HV2).
is_adj_point_es_d(HV1,HV2):- is_adjacent_point(HV1,se,HV2).
is_adj_point_es_d(HV1,HV2):- is_adjacent_point(HV1,sw,HV2).

is_adj_point_wn(HV1,HV2):- is_adjacent_point(HV1,n,HV2).
is_adj_point_wn(HV1,HV2):- is_adjacent_point(HV1,w,HV2).
is_adj_point_wn_d(HV1,HV2):- is_adj_point_wn(HV1,HV2).
is_adj_point_wn_d(HV1,HV2):- is_adjacent_point(HV1,nw,HV2).
is_adj_point_wn_d(HV1,HV2):- is_adjacent_point(HV1,ne,HV2).

is_adj_point_nsew(HV1,HV2):- is_adj_point_es(HV1,HV2).
is_adj_point_nsew(HV1,HV2):- is_adj_point_wn(HV1,HV2).
is_adj_point_d(HV1,HV2):- is_adjacent_point(HV1,ne,HV2).
is_adj_point_d(HV1,HV2):- is_adjacent_point(HV1,nw,HV2).
is_adj_point_d(HV1,HV2):- is_adjacent_point(HV1,se,HV2).
is_adj_point_d(HV1,HV2):- is_adjacent_point(HV1,sw,HV2).
is_adj_point_colormass(C1,HV1,HV2):- is_adjacent_point(HV1,Dir,HV2), freeze(C1,(C1\==black ->  true ; \+ is_diag(Dir))).

is_adj_point_type(C1,colormass,HV1,HV2):- is_adj_point_colormass(C1,HV1,HV2).
%is_adj_point_type(C1,v_line,HV1,HV2):- (is_adjacent_point(HV1,n,HV2);is_adjacent_point(HV1,n,HV2)).
%is_adj_point_type(C1,h_line,HV1,HV2):- (is_adjacent_point(HV1,w,HV2);is_adjacent_point(HV1,e,HV2)).
is_adj_point_type(_C1,nsew,HV1,HV2):- is_adj_point_nsew(HV1,HV2).
is_adj_point_type(_C1,diamonds,HV1,HV2):- is_adj_point_d(HV1,HV2).

:- dynamic(gid_type_oid/3).



grid_type_oid(Grid,Type,OID):- ensure_gid(Grid,GID), cache_grid_objs_for(GID,Type), gid_type_oid(GID,Type,OID).

:- dynamic(is_gridmass/2).
mmass(Grid,Mass):- ensure_gid(Grid,GID), Grid\==GID,!,mmass(GID,Mass).
mmass(GID,Mass):- is_gridmass(GID,Mass),!.
mmass(GID,Mass):- 
  findall(_,(cmem(GID,_,C),C\==black),L),length(L,Mass),
  assert(is_gridmass(GID,Mass)).

mgrid_size(Grid,H,V):- ensure_gid(Grid,GID), is_grid_size(GID,H,V).

/*
is_nsew_same_as_colormass(Grid):-
  ensure_gid(Grid,GID),
  grid_object_points(GID,nswe,Nswe),
  grid_object_points(GID,colormass,Colormass),!,
  sort(Colormass,S1),sort(Nswe,S2),
  S1\=@=S2.

is_nsew_same_as_colormass_count(Grid):-
  ensure_gid(Grid,GID),
  grid_object_count(GID,nswe,Nswe),
  grid_object_count(GID,colormass,Colormass),!,
  Nswe\=@=Colormass.
*/

ensure_obj(OID,Obj):- into_obj(OID,Obj).

show_oid(OID):-
 (var(OID)->gid_type_oid(_GID,Type,OID);true),
 oid_to_points(OID,Points),
 ignore(gid_type_oid(_,Type,OID)),
 print_grid(oid(Type,OID),Points).


oid_to_points(OID,Points):- findall(C-HV,(omem(GID,HV,OID),cmem(GID,HV,C)),Points).
oid_to_glyph_points(OID,Points):- oid_to_glyph(OID,Glyph),
  findall(GC-HV,(omem(GID,HV,OID),cmem(GID,HV,C),(smem(GID,HV,OID)->GC=C;(GC=(Glyph-C)))),Points).

oid_to_glyph(OID,Glyph):- atom_chars(OID,[_,_,Glyph|_]).

tmem(GID,HV2,Type):- omem(GID,HV2,OID), gid_type_oid(GID,Type,OID).

%ensure_indv_type(Type):- member(Type,[nsew,colormass,diamonds]).
%ensure_indv_type(Type):- member(Type,[v_line,h_line,nsew,diamonds,colormass]).
%ensure_indv_type(Type):- member(Type,[nsew,colormass]).
ensure_indv_type(Type):- member(Type,[colormass,diamonds,nsew]).
%ensure_indv_type(Type):- member(Type,[colormass]).
%cant_reuse(diamonds,nsew).
%cant_reuse(X,X):-!.
cant_reuse(A,B):- can_reuse(A,B),!,fail.
cant_reuse(_,_):-!.
can_reuse(X,colormass):- X \== colormass.
can_reuse(X,diamonds):- X \== diamonds.
can_reuse(nsew,_).

test_show_grid_objs(TestID):- ensure_test(TestID), every_grid(show_grid_objs,TestID).

show_grid_objs(Grid):- true, 
  ensure_gid(Grid,GID),cache_grid_objs(GID),
  findall(Type=G,(ensure_indv_type(Type),grid_object_glyph_points(GID,Type,Group),flatten(Group,G),G\==[]),Grids),
  maplist(arg(2),Grids,Flatme),flatten(Flatme,All),
  show_grid_objs_list(GID,All,Grids).
  %print_ss([all=All|Grids]),!.
  
  
  %print_grid((GID),All).
show_grid_objs_list(GID,_All,Grids):- Grids=[N=V],!,dash_chars,print_grid(GID=N,V),dash_chars.
show_grid_objs_list(GID,All,Grids):- dash_chars,print_ss([GID=All|Grids]),dash_chars,!.

grid_object_points(Grid,Type,Groups):-
  ensure_gid(Grid,GID),
  cache_grid_objs_for(Type,GID),
  findall(Points,(gid_type_oid(GID,Type,OID),oid_to_points(OID,Points)),Groups).

grid_object_glyph_points(Grid,Type,Groups):-
  ensure_gid(Grid,GID),
  cache_grid_objs_for(Type,GID),
  findall(Points,(gid_type_oid(GID,Type,OID),oid_to_glyph_points(OID,Points)),Groups).

%grid_object_defs(Grid,Type,Groups):-
%  ensure_gid(Grid,GID), vm_for_gid(GID,VM),!.

grid_object_count(Grid,Type,Len):-
  ensure_gid(Grid,GID),
  grid_object_points(GID,Type,Groups),length(Groups,Len).

% each_o(Grid,Type,OID),show_oid(OID).

grid_type_obj(Grid,Type,OBJ):- ensure_gid(Grid,GID), 
  cache_grid_objs(GID),!,gid_type_oid(GID,Type,OID),ensure_obj(OID,OBJ).


precache_all_grid_objs:- 
  time(forall(all_arc_test_name_unordered(TestID),cache_grid_objs_for_test(TestID))).

cache_grid_objs_for_test(TestID):- 
 forall(kaggle_arc(TestID,_Example,I,O),
    (ignore(cache_grid_objs(I)),ignore(cache_grid_objs(O)))).

% 810,000
cache_grid_objs_for(Grid,_Type):- ensure_gid(Grid,GID),cache_grid_objs(GID).

cache_grid_objs(TestID):- is_valid_testname(TestID),!, cache_grid_objs_for_test(TestID).
cache_grid_objs(Grid):- \+ atom(Grid),!, ensure_gid(Grid,GID),cache_grid_objs(GID).
cache_grid_objs(GID):- gid_type_oid(GID,_,_),!.
cache_grid_objs(GID):-
  Shown = _,
  show_time_gt_duration(0.3, 
   (forall(ensure_indv_type(Type),cache_grid_objs_now(Type,GID)),
    forall(how_count(_,How,_),grid_obj_count_1(GID,How,_))),
    (Shown = 1,show_grid_objs(GID))),
  ignore(((   var(Shown),is_grid_obj_count(GID,Type,Count),Count>=40,show_grid_objs(GID),show_obj_counts(GID)))),
  ignore(((nonvar(Shown),is_grid_obj_count(GID,Type,Count),Count>=40,                    show_obj_counts(GID)))),!.

show_obj_counts(GID):- findall(Type=Count,grid_obj_count(GID,Type,Count),Out),pp(GID=Out),!.

:- dynamic(is_grid_obj_count/3).

grid_obj_counts(Grid,Counts):-
   findall(Type=Count,grid_obj_count(Grid,Type,Count),Counts).

grid_obj_count(Grid,Type,Count):- ensure_gid(Grid,GID),
  cache_grid_objs(GID),!,
  grid_obj_count_1(GID,Type,Count).

grid_obj_count_1(GID,Type,Count):- var(Type),!,how_count(_,Type,_),grid_obj_count_1(GID,Type,Count).
grid_obj_count_1(GID,Type,Count):- is_grid_obj_count(GID,Type,Count),!.
grid_obj_count_1(GID,Type,Count):- 
   how_count(GID,Type1,Goal),Type=@=Type1, !, 
   findall(_,Goal,L),length(L,Count), assert(is_grid_obj_count(GID,Type,Count)).

how_count(GID,all,Goal):- Goal = gid_type_oid(GID,_,_).
how_count(GID,fg,Goal):- Goal = ((gid_type_oid(GID,_,OID),\+ oindv(color,GID,OID,black))).
how_count(GID,bg,Goal):- Goal = ((gid_type_oid(GID,_,OID), oindv(color,GID,OID,black))).
how_count(GID,fg(Type),Goal):- ensure_indv_type(Type), Goal = (gid_type_oid(GID,Type,OID),\+ oindv(color,GID,OID,black)).
how_count(GID,bg(Type),Goal):- ensure_indv_type(Type), Goal = (gid_type_oid(GID,Type,OID), oindv(color,GID,OID,black)).
%how_count(GID,Color,Goal):- enum_fg_colors(Color), Goal = (oindv(color,GID,_,Color)).
%how_count(GID,Type,Goal):- ensure_indv_type(Type), Goal = (gid_type_oid(GID,Type,_)).

%cache_gid_objs(GID):- show_time_gt_duration(0.4, forall(ensure_indv_type(Type),cache_grid_objs_now(Type,GID)), show_grid_objs(GID)).

is_obj_count_all_lt_15(Grid):- ensure_gid(Grid,GID), grid_obj_count(GID,all,Count),Count<15.
is_obj_count_all_gt_40(Grid):- ensure_gid(Grid,GID), grid_obj_count(GID,all,Count),Count>40.
is_obj_count_all_gt_200(Grid):- ensure_gid(Grid,GID), grid_obj_count(GID,all,Count),Count>200.
is_obj_count_nsew_zero(Grid):- ensure_gid(Grid,GID), grid_obj_count(GID,fg(nsew),C),C==0.
is_obj_count_diamonds_not_zero(Grid):- ensure_gid(Grid,GID), grid_obj_count(GID,fg(diamonds),C),C>0.
is_obj_count_colormass_zero(Grid):- ensure_gid(Grid,GID), grid_obj_count(GID,fg(colormass),C),C==0.


:- dynamic(o_color/3).

cache_grid_objs_now(Type,GID):- gid_type_oid(GID,Type,_),!.
%cache_grid_objs_now(Type,GID):- var(Type),!,ensure_indv_type(Type),cache_grid_objs(Type,GID).
cache_grid_objs_now(Type,GID):- cache_grid_objs1(Type,GID).

type_may_have(GID,Type,HV1):- \+ (omem(GID,HV1,OOID), gid_type_oid(GID,OType,OOID), cant_reuse(Type,OType)).

%cache_grid_objs1(diamonds,_GID):-!.
cache_grid_objs1(Type,GID):- 
  type_min_len(Type,Minlen),
  cmem(GID,HV1,C1), type_may_have(GID,Type,HV1),
  continue_obj(GID,Type,[HV1],Points,Len), Len >= Minlen,!,
  new_obj_points(GID,Type,C1,Points,Len),
  cache_grid_objs1(Type,GID).

cache_grid_objs1(Type,GID):- Type == colormass,
  cmem(GID,HV1,C1), type_may_have(GID,Type,HV1),
  continue_obj(GID,Type,[HV1],Points,Len), Len>=2,!,
  new_obj_points(GID,Type,C1,Points,Len),
  cache_grid_objs1(Type,GID).

cache_grid_objs1(Type,GID):- Type == colormass,
  cmem(GID,HV1,C1), type_may_have(GID,Type,HV1),!,
  new_obj_points(GID,Type,C1,[HV1],1),
  cache_grid_objs1(Type,GID).

cache_grid_objs1(_,_).

continue_obj(GID,Type,Points,Out,Len):- 
  cont_obj_execpt(GID,Type,Points,Points,Out),
  length(Out,Len).

cont_obj_execpt(_GID,_Type,[],_Already,[]):-!.
cont_obj_execpt(GID,Type,[HV1|Points],Already,[HV1|Out]):-   
 (cmem(GID,HV1,C1),is_adj_point_type(C1,Type,HV1,HV2), \+ member(HV2,Already), cmem(GID,HV2,C1)),
  type_may_have(GID,Type,HV2),
  cont_obj_execpt(GID,Type,[HV2,HV1|Points],[HV2|Already],Out).
cont_obj_execpt(GID,Type,[HV1|Points],Already,[HV1|Out]):-  !,
  cont_obj_execpt(GID,Type,Points,Already,Out).

type_min_len(diamonds,3).
type_min_len(nsew,4).
type_min_len(colormass,1).




assert_omem_points(GID,OID,[HV1]):-  !, assert_omem_point(GID,OID,HV1).
assert_omem_points(GID,OID,[HV1|Points]):- assert_omem_point(GID,OID,HV1), assert(omem(GID,HV1,OID)), assert_omem_points(GID,OID,Points).

assert_omem_point(GID,OID,HV1):- 
 ignore((retract(omem(GID,HV1,OOID)),update_object(OOID))),
   assert(omem(GID,HV1,OID)).

new_obj_points(GID,Type,OID,C1,Points,Len):-
  new_omem(GID,Type,OID),
  assert_omem_points(GID,OID,Points),
  assert(oindv(color,GID,OID,C1)),
  assert(oindv(size,GID,OID,Len)).

update_object(OID):- 
   must_det_ll((gid_type_oid(GID,Type,OID), 
   update_object(GID,Type,OID))).

update_object(GID,_Type,OID):-
     \+ \+ omem(_,_,OID),!,     
     findall(_,omem(GID,_,OID),L),length(L,Len),
     retractall(oindv(size,GID,OID,_)),
     assert(oindv(size,GID,OID,Len)).

update_object(GID,Type,OID):-
  ignore(( 
    \+ omem(GID,_,OID),
    gid_type_oid(GID,Type,OID),
    retract_object(GID,OID,_),
    retractall(oindv(_,GID,OID,_)),
    retractall(gid_type_oid(GID,Type,OID)),
    retractall(is_grid_obj_count(GID,_,_)))).
    


new_omem(GID,Type,OID):- 
  flag(GID,X,X+1),int2glyph(X,Glyph),
  name(Glyph,[ID]),!,
  atomic_list_concat(['o_',Glyph,'_',ID,'_',GID],OID),
  assert(gid_type_oid(GID,Type,OID)).

:- include(kaggle_arc_footer).

