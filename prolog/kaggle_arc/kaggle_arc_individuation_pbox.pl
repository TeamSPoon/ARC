/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


% =====================================================================
is_fti_step(pbox_vm).
% =====================================================================
:- luser_setval(individuated_cache,false).


neg_h_v_area(size2D(H,V),sort(NArea,DifHV)):- NArea is - (H * V),  DifHV is - abs(H-V).

:- abolish(l_s_4sides,2).
:- abolish(s_l_4sides,2).

:- dynamic(l_s_4sides/2).
:- dynamic(s_l_4sides/2).

:- 
   findall(size2D(HV,HV),between(1,32,HV),SizesSquareS),
   findall(size2D(H,V),(between(1,32,H),between(1,32,V),H\=V),SizesRect),
   predsort(sort_on(neg_h_v_area),SizesRect,SizesRectS),
   %reverse(SizesSquareS,SizesSquareR), reverse(SizesRectS,SizesRectR),
  % list_to_set([size2D(3,3),size2D(2,2)|SizesSquareS],Sizes_L_S),
   append(SizesSquareS,SizesRectS,AllSizes),   
   predsort(sort_on(neg_h_v_area),AllSizes,Sizes_L_S),

   forall(member(size2D(H,V),Sizes_L_S),assertz(l_s_4sides(H,V))),
   reverse(Sizes_L_S,Sizes_S_L),
   forall(member(size2D(H,V),Sizes_S_L),assertz(s_l_4sides(H,V))),

   nop(i_pbox(VM,l_s(2),SizesRectS)), nop(i_pbox(VM,s_l(2),SizesSquareR)), nop(i_pbox(VM,s_l(2),SizesRectR)).


pbox_vm(VM):- !,
   %GH is round(VM.h*2/3), GV is round(VM.v*2/3),
   GH is round(VM.h - 1), GV is round(VM.v - 1),
   findall(size2D(H,V),(l_s_4sides(H,V),H=<GH),Sizes_L_S),
   findall(size2D(H,V),(s_l_4sides(H,V),V=<GV),Sizes_S_L),
   GridI = VM.grid_o,
   get_black(Black),
   mapgrid(assign_plain_var_with(Black),GridI,Grid),
   localpoints_include_bg(Grid,Points),
   add_top_bot_left_right(Top,_T,Grid,_B,Bot,LLeft,_LL,_RR,RRight,XSG),
   maplist(=(N),Top), maplist(=(S),Bot), maplist(=(W),LLeft), maplist(=(E),RRight),
   NSEW = [N,S,E,W],
  i_pbox_l(NSEW,XSG,Points,VM,s_1(1),Sizes_L_S),!,
  i_pbox_l(NSEW,XSG,Points,VM,l_s(1),Sizes_S_L),!,
  i_pbox_l(NSEW,XSG,Points,VM,s_l(2),Sizes_S_L),!,
  i_pbox_l(NSEW,XSG,Points,VM,l_s(2),Sizes_L_S),!,
  i_pbox_l(NSEW,XSG,Points,VM,l_s(3),Sizes_L_S),!,
  i_pbox_l(NSEW,XSG,Points,VM,s_l(3),Sizes_S_L),!,
   true.
/*
X-y-X
|   |
y   y
|   |
X-y-X
*/
quick_test_menu(pbox).

pbox :-  clsmake, fail.
/*
pbox :-  pbox(t('05f2a901')).
pbox :-  pbox(t('06df4c85')).
pbox :-  pbox(t('60b61512')).
pbox :-  pbox(t('0b148d64')).
pbox :-  pbox(t('09629e4f')).*/
pbox :-  pbox(_).
pbox(Name):-   
  update_changed_files,
  (var(Name)-> true; testid_name_num_io(Name,TestID,Example,Num,IO)),
  ExampleNum = Example+Num,
  (nonvar(IO) 
   -> forall(kaggle_arc_io(TestID,ExampleNum,IO,G),ignore(pbox_io(TestID,ExampleNum,IO,G))) 
    ; forall(kaggle_arc(TestID,ExampleNum,I,O),ignore(pbox_pair(TestID,ExampleNum,I,O)))).

is_bg_color_or_var(C):- is_bg_color(C) ; \+ is_fg_color(C).
is_fg_color_or_var(C):- is_fg_color(C) ; \+ is_bg_color(C).

test_pbox:- test_p2(pbox_pair(_TestID,_What)).
quick_test_menu(test_pbox).


pbox_indivs:- 
  with_test_grids(TestID,Grid,
    (pbox_io(TestID,_ExampleNum,_IO,Grid))).

pbox_pair(TestID,ExampleNum,I,O):-
   wdmsg(?- test_p2(pbox_pair(TestID,ExampleNum))),
   pbox_io(TestID,ExampleNum,in,I), pbox_io(TestID,ExampleNum,out,O).

pbox_io(TestID,ExampleNum,IO,G0):-
  kaggle_arc_io(TestID,ExampleNum,IO,_),
  (into_grid(G0,G)->true;into_grid((TestID>ExampleNum*IO),G)), 
  duplicate_term(G,GG),
  ignore(kaggle_arc_io(TestID,ExampleNum,IO,GG)),
  set_current_test(TestID),
  wdmsg(?- pbox_io(TestID,ExampleNum,IO)),
  my_time((i_pbox(GG,Objs),
  pbox_io_result(TestID,ExampleNum,IO,GG,Objs))).

pbox_io_result(TestID,ExampleNum,IO,G,[]):- !,
 print_grid(wqs(red,(?-pbox(TestID>ExampleNum*IO))),G).

pbox_io_result(TestID,ExampleNum,IO,G,[Objs]):- !,
 print_side_by_side(orange,G,(?-pbox(TestID>ExampleNum*IO)),_,print_grid(Objs),(TestID>ExampleNum*IO)),!.

pbox_io_result(TestID,ExampleNum,IO,G,Objs):- !,
 once((maplist(obj_global_grid,Objs,OGG), print_side_by_side(OGG))),!,
 print_side_by_side(cyan,G,(?-pbox(TestID>ExampleNum*IO)),_,print_grid(Objs),(TestID>ExampleNum*IO)),!.

i_pbox(GridIn,Objs):- 
  ROptions = i_pbox,
  do_ig(ROptions,GridIn,IndvS),
  into_grid(GridIn,Grid),
  locally(nb_setval(debug_as_grid,t),show_ig(igo,ROptions,GridIn,Grid,IndvS)),
  maybe_subdiv(IndvS,Objs).

maybe_subdiv([OO],Objs):- object_grid(OO,G),i(i_pbox,G,Objs),!.
maybe_subdiv(Objs,Objs).

obj_global_grid(X,G-wqs(DSC)):- get_black(Black), global_grid(X,Grid),subst(Grid,Black,wbg,G), v_hv(X,VH,VV), loc2D(X,OH,OV),!,
  DSC =[loc2D(OH,OV),v_hv(VH,VV)].

not_in_eq(Set,V):- \+ (member(VV,Set),VV == V).

no_rule(_,_).
dif_fg(X,Y):- nop((freeze(X,freeze(Y,((is_fg_color(X);is_fg_color(Y))->dif(X,Y)))))).


add_top_bot(Top,T,[],B,Bot,[Top]):- B=Bot,T=Top,B=T.
add_top_bot(Top,T,[],B,Bot,[Top,Bot]):- !, B=Bot,T=Top.
add_top_bot(Top,T,In,B,Bot,TInB):-
 must_det_ll((  
  last(In,B), [T|_]=In,
  length(T,H), length(Top,H), length(Bot,H),
  append([Top|In],[Bot],TInB))).

make_squarish(BorderRule,In,NewSearch,Inside,OBorder):-
 must_det_ll((
  add_top_bot_left_right(Top,T,In,B,Bot,LLeft,LL,RR,RRight,NewSearch),
  maplist(BorderRule,Top,T), maplist(BorderRule,Bot,B),  maplist(BorderRule,LL,LLeft), maplist(BorderRule,RRight,RR),
  term_variables(NewSearch,FNewSearch), term_variables(In,Inside),
  include(not_in_eq(Inside),FNewSearch,OBorder))),!.

add_top_bot_left_right(Top,T,In,B,Bot,LLeft,LL,RR,RRight,NewSearch):-
 must_det_ll((
  add_top_bot(Top,T,In,B,Bot,TInB),
  rot90(TInB,TInB90),  
  add_top_bot(Left,L,TInB90,R,Right,Find),
  rot270(Find,NewSearch),  
  append([_|LL],[_],L),
  append([_|LLeft],[_],Left),
  append([_|RR],[_],R),
  append([_|RRight],[_],Right))),!.



:- abolish(cached_make_search_box,9).
:- dynamic(cached_make_search_box/9).

make_search_box(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder):- 
  % between(1,32,H),between(1,32,V),
  %s_l_4sides(H,V),
  HH is H,VV is V, once(make_search_box_m1(HH,VV,Center,In,Find,Cents,Inside,IBorder,OBorder)).

make_search_box_m1(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder):- cached_make_search_box(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder),!.
make_search_box_m1(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder):-
  make_search_box_fresh(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder),
  asserta(cached_make_search_box(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder)).  

make_search_box_fresh(1,1,Center,In,Find,Cents,Inside,IBorder,OBorder):- 
  Center = [], Cents = [],  [[C]] = In,  IBorder = [C],  make_squarish(no_rule,In,  Find, Inside,OBorder).
make_search_box_fresh(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder):- 
  (H<3;V<3),!,make_grid(H,V,In), flatten(In,IBorder), 
  Center = In, Cents = IBorder, !,
  make_squarish(no_rule,In,  Find, Inside,OBorder).
make_search_box_fresh(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder):-
     HH is H-2 ,VV is V-2, make_grid(HH,VV,Center),
     make_squarish(no_rule,Center,  In, Cents, IBorder),
  make_squarish(no_rule,In,  Find, Inside,OBorder).


/*
i_pbox_detect(L_S,H,V,XSG,[n,s,e,w],In,OH,OV):- 
 H>=2,V>=2,
 once((
   make_grid(H,V,In),
   make_squarish(no_rule,In,Find,Inside,OBorder))),
  % print_grid(in,In),ptv(in=In), print_grid(ns,Find),ptv(ns=Find), print_grid(s,XSG),ptv(s=XSG),
  ogs_11(OH,OV,Find,XSG),
  \+ \+ different_enough(L_S,In,Find,Inside,OBorder).
*/
trim_ends(T,TT):- append([_|TT],[_],T).

not_irow(C,T):- trim_ends(T,TT), \+ maplist(=(C),TT).
not_edge(C,Find):- append([T|_],[B],Find), not_irow(C,T),  not_irow(C,B),
  rot90(Find,Find90),append([R|_],[L],Find90), not_irow(C,R), not_irow(C,L).

not_whole_row_or_col(C,Center):- 
  \+ (member(Row,Center), maplist(=(C),Row)),
  \+ (Center\==[], Center\==[[]],rot90(Center,Cols),member(Col,Cols), maplist(=(C),Col)).

count_irows(Find,C,N):-
  append([N|_],[S],Find), rot90(Find,Find90),append([W|_],[E],Find90), 
  maplist(trim_ends,[N,S,E,W],Rows),
  findall(E,(member(E,Rows),not_irow(C,E)),N).

i_pbox_l(_NSEW,_XSG,_Points,_VM,L_S,[]):- !, wdmsg(complete(L_S)).

i_pbox_l(NSEW,XSG,Points,VM,L_S,[size2D(H,V)|Sizes]):- L_S == l_s(1),
  H>3,V>3,
  make_search_box(H,V,_Center,In,Find,_Cents,Inside,_IBorder,_OBorder),
  maplist(=(C),Inside),
  ogs_11(OH,OV,Find,XSG),
  count_irows(Find,C,N), length(N,NN),
  OBJ = In, Why = find_ogs(NN),
  cont_pbox_l(OH,OV,NSEW,XSG,Points,VM,L_S,size2D(H,V),Sizes, OBJ,Why).

/* ___________
  | T T T T T |
  | T t t t T |
  | T t   t T |
  | T t t t T |
  | T T T T T |
   ¯¯¯¯¯¯¯¯¯¯¯
*/

i_pbox_l(NSEW,XSG,Points,VM,L_S,[size2D(H,V)|Sizes]):-  L_S == l_s(2),

   (H>=2;V>=2),
   (H>=2,V>=2),
    %B = black,
    %NSEW =[B,B,B,B],
    make_search_box(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder),    
    %Black = _,
    %Center \= [[_]],
    ogs_11(OH,OV,Find,XSG),
    once(( found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,   OBJ,Why))),
    cont_pbox_l(OH,OV,NSEW,XSG,Points,VM,L_S,size2D(H,V),Sizes, OBJ,Why).


i_pbox_l(NSEW,XSG,Points,VM,L_S,[_|Sizes]):- i_pbox_l(NSEW,XSG,Points,VM,L_S,Sizes).


cont_pbox_l(OH,OV,NSEW,XSG,Points,VM,L_S,size2D(H,V),Sizes, OBJ,Why):-
  print_grid(Why,OBJ),
  once(( localpoints_include_bg(OBJ,OPoints), offset_points(OH,OV,OPoints,GOPoints) )),
  intersection(Points,GOPoints,Intersection,LeftOver,Unknown), Unknown==[],!,
  must_det_ll((
  %indv_props(Obj,Props),my_partition(is_point_or_colored,Props,_,PropsRetained),
  make_indiv_object(VM,[],GOPoints,_Obj),
  %offset_grid(OH,OV,In,OffsetGrid),!, is_grid(OffsetGrid),
  %OffsetGrid = In,
  %as_debug(1,((dash_chars,Info=ogs_1(OH,OV), print_side_by_side(Grid-Info,[Obj]-Info)))), % trace,
  %print_ss([Obj|Grid]-wqs(maybe_ogs_color(R,OH,OV))), %  trace,  
  %print_grid(maybe_ogs_color(R,OH,OV),[Obj|Grid]), %  trace,  
  remCPoints(VM,GOPoints), % =GOPoints,
  remGPoints(VM,Intersection),!,
  i_pbox_l(NSEW,XSG,LeftOver,VM,L_S,[size2D(H,V)|Sizes]))). 

existingObject(VM,GOPoints):- 
  member(O,VM.objs),globalpoints_include_bg(O,Ps),
  GOPoints==Ps,!.

:- style_check(-singleton).


p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  Center, unRibs(L_S,C,H,V)):- 
   H>3,V>3,s_l(1)==L_S,  C\==black,  not_whole_row_or_col(C,Center), \+ ((member(Row,Center),append(_,[C,C,C|_],Row))).

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  Center, ring(L_S,C,H,V)):- H>4,V>4, 
   \+ \+ (member(NB,Cents), NB \= C), not_whole_row_or_col(C,Center), C==black.

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  Center, innerThang(L_S,C,H,V)):- 
   maplist(=(C),OBorder),!, C\==black, l_s(2)==L_S, \+ member(C,Cents).

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  Center, innerThang(C)):- 
   maplist(=(C),OBorder),!, C\==black, \+ member(C,Cents).

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  Ans, midThang(L_S,C,H,V)):- 
   %l_s(2)==L_S,
   \+ maplist(=(C),OBorder), not_whole_row_or_col(C,Center), \+ is_black(C),  
   (is_black(C)->  Ans = Center ; (Ans = In, not_edge(C,Find))).


p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  In, midThang1(C)):- 
  (s_l(2) == L_S, \+ \+ ([B,B,B,B]=NSEW, maplist(=(C),OBorder), \+ maplist(==(C),IBorder), nop( \+ (member(Row,In), maplist(=(C),Row) )))).          

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  In, midThang2(L_S,C,H,V)):- 
        %  fail,
  (l_s(2) == L_S, \+ \+ ([n,s,e,w]=NSEW, different_enough(L_S,In,Find,Inside,OBorder))).

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  In, outerThing(BC)):-
  is_black(C), 
  maplist(=(BC),OBorder), BC\==C,!.

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  In, outerThing2n(L_S,BC,C,H,V)):- 
   % l_s(2)==L_S,
  maplist(=(BC),OBorder), BC\==C,!.

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  Center, twang):- fail,

    \+ \+ fif((maplist(==(C),Inside), \+ maplist(=(C),OBorder)),
       \+ \+ fif(not_edge(C,Find), fif(\+ append(_,[C,C,C|_],OBorder), nop(print_grid(ogs_in0(L_S,C,OH,OV,H,V,OBorder),In))))),
    %fif( \+ mass(In,0),print_grid(ogs_in(L_S,C,OH,OV,H,V),In)),

    % ( \+ member(C,OBorder) ; C==black),
/*
    (L_S == l_s(2) ->  \+member(C,OBorder) ; ( \+member(C,OBorder) ; C==black)),
      fif(L_S == l_s(2),(
       \+ (member(Row,Center), maplist(=(C),Row)),

       \+ (Center\==[], Center\==[[]],rot90(Center,Cols),member(Col,Cols), maplist(=(C),Col)) )),
*/

(
    fif( \+ \+ (is_black(C),not_whole_row_or_col(C,Center)),
      fif(Center\=[[_]],print_grid(inside1(L_S,C,OH,OV,H,V),Center))),
 

    (  ( fail, \+ member(C, Cents)) -> print_grid(inside(L_S,C,OH,OV,H,V),Center);
         \+ \+  fif(Center\==In, 
               \+ \+  fif(not_whole_row_or_col(C,In),print_grid(found1(L_S,C,OH,OV,H,V),In)))),
   \+ \+  fif(( \+ member(C,OBorder), \+ maplist(=(Black),OBorder)),
       ((print_grid(inGood(L_S,C,OH,OV,H,V),In),nop((print_grid(found2(L_S,C,OH,OV,H,V),Find))))))).
    %fif(maplist(==(C),Inside),print_grid(find1(L_S,C,OH,OV,H,V),Find)),
    %maplist(=(C),Center,Cents) -> make_obj(In,solid_rect);
    %-> make_obj(In,solid_rect);


  %  (maplist(=(C),Inside), \+ member(C,OBorder)) -> ObjG = In ;
  %  (maplist(=(C),Inside), \+ member(C,OBorder))

is_compass(A):- atom(A),member(A,[n,s,e,w]).

found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  Center, 
                       dottedBoxCenter(border1(C1,C2),NSEW,L_S,H,V)):- H>3,V>3, 
    s_l(1)==L_S,
    ((my_partition(=(C),IBorder,C1s,C2s),length(C1s,N1),length(C2s,N2),max_min(N1,N2,Max,Min),Min==4)),!.
%    \+ member(C,OBorder), \+ member(C,Cents))).


found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  In, largeInBlack(NSEW,L_S,H,V)):-  
  l_s(2)==L_S,
   \+ \+ ((NSEW=[B,B,B,B], B =black,
         maplist(=(B),OBorder), (member(C,IBorder), \+ is_black(C)))),!, not_whole_row_or_col(C,In).

/*

found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  Center, boxCenter(NSEW,L_S,H,V)):-
  l_s(2)==L_S,
   \+ \+ ((maplist(=(C),IBorder), \+ member(C,OBorder), \+ member(C,Cents))).

*/
found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  OBJ, Why):-
  maplist(=(C),IBorder),!,p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  OBJ, Why).

/*
found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  In, different_enough(L_S,NSEW)):-  
  [n,s,e,w]=NSEW,
  !, fail,
  not_whole_row_or_col(black,In),
  different_enough(L_S,In,Find,Inside,OBorder),!.
*/

found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,  In, different_enough(NSEW)):- 
  %[n,s,e,w]=NSEW,
  !, fail,
  different_enough(L_S,In,Find,Inside,OBorder).



different_enough(L_S,In,Find,Inside,OBorder):-
  a_portion_colors(Inside,InsideC,InsideV,InsideBG,InsideFG),
  a_portion_colors(OBorder,BorderC,BorderV,BorderBG,BorderFG),
  maplist(sort,[BorderC,BorderV,BorderBG,BorderFG,InsideC,InsideV,InsideBG,InsideFG],
               [SBorderC,SBorderV,SBorderBG,SBorderFG,SInsideC,SInsideV,SInsideBG,SInsideFG]),
  different_enough_c(In,Find,L_S,SBorderC,SBorderV,SBorderBG,SBorderFG,
                                               SInsideC,SInsideV,SInsideBG,SInsideFG),!.

a_portion_colors(IA,Compass,Vars,BG,FG):- 
  my_partition(is_compass,IA,Compass,Colors),
  my_partition(is_bg_color,Colors,BG,VFG),my_partition(is_fg_color,VFG,FG,Vars).


  %\+ not_different_enough_ii(BorderV,BorderBG,BorderFG,InsideV,InsideBG,InsideFG).

%        different_enough_c(_I,_F,BorderV,BorderBG,BorderFG,   InsideV,InsideBG,InsideFG):-!.

has_blank_rows(I,N):- findall(Row,(append([_|_],[Row,_|_],I), maplist(cmatch(is_bg_color),Row)),N).
%has_blank_rows(I,N):- findall(Row,(member(Row,I), maplist(cmatch(is_bg_color),Row)),N).
has_blank_cols(C,N):- rot90(C,I),has_blank_rows(I,N).

different_enough_c(_I,_F,l_s(2),   _, _,           [_],        [],    
                               _, _,            [],    [_,_|_]):- !, % 0b148d64
  true.
/*
different_enough_c(I,_F,l_s(2),  [N,W], _,        [_],        [],    
                               _,   [],        [_],       [_]):- w_in_90(N,W),!, % 0b148d64
  \+ has_blank_rows_or_cols(I).
*/


different_enough_c(_I,_F,_,  _,_,      [_],      [],    
                              _,_,     [_],      []):- !,fail.

different_enough_c(I,_F,l_s(2),   _, _,           [_],        [],    
                               _,   [],        _,       [_]):- !, % 0b148d64
  max_blanks(I,1).




different_enough_c(_I,_F,l_s(2),  _, _,            [],        [FG],    
                              _, [],          _ ,         [H|T]):- \+ member(FG,[H|T]).  % 09629e4f

different_enough_c(I,_F,_S_l,  [],    _,        [_],        [],    
                                _,   [],        [_],       [_]):- !, % 05f2a901
                                 max_blanks(I,1).

different_enough_c(_I,_F,l_s(2),  _,    _,         [],        [_],    
                                _,   [],        [_],       []):- !, % 06df4c85
   true.

/*
*/

different_enough_c(I,_F,l_s(2),   _, _,           [_],        [],    
                               _,   [],        _,       NN):- NN\==[], !, % 0b148d64
                                  length(NN,Max),
                                  max_blanks(I,Max).


/*

different_enough_c(_I,_F,_,  _,_,       _,       [],    
                                 _,_,       _,       []):- !,fail.


different_enough_c(_I,_F,_,   _,_,        [],          [],    _,_,       _,     _):-!,fail.
different_enough_c(_I,_F,_,   _,_,        [],          [SC],      _,_,         [],    InsideFG):- InsideFG==[SC],!,fail.
different_enough_c(_I,_F,_,   _,_,        [_],         [],    _,InsideV,       _,     InsideFG):-  InsideV\==[];InsideFG==[_]. 

%different_enough_c(_I,_F,_,_,        [_],        [],    _,[],       [_],     [_]). 
different_enough_c(_I,_F,_,   [_,_,_],_,        [_],        [],    _,[],       [_],     [_,_|_]). % 0b148d64
different_enough_c(_I,_F,_,   _,_,        _,         [_],    _,[],       [_],     []).
*/
/*
%different_enough_c(_I,_F, _,        [],          [_],      _,          _,        _).
%different_enough_ii(BorderV,BorderBG,BorderFG,InsideV,InsideBG,InsideFG).
%not_different_enough_ii(_BorderV,_BorderBG,_BorderFG,_InsideV,_InsideBG,_InsideFG):- fail.
*/
max_blanks(I,Max):-
  \+ (has_blank_rows(I,N), length(N,L), L>=Max), 
  \+ (has_blank_cols(I,N), length(N,L), L>=Max).

