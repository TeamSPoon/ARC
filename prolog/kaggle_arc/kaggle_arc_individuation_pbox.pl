/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/

:- dynamic(gclip_cache/10).
make_gclip_cache_0:-
 forall((
   make_grid_cache(GH,GV,G),
   between(1,GH,OH),between(1,GV,OV),
   MH is GH-OH, MV is GV-OV,MH>=1,MV>=1,
   between(1,MH,SGH),between(1,MV,SGV), 
   EH is OH+SGH-1, EV is OV+SGV-1),
   ignore(once((clip(OH,OV,EH,EV,G,SG),writeq(gclip_cache(GH,GV,SGH,SGV,EH,EV,OH,OV,G,SG)),writeln('.'))))).

make_gclip_cache_file:- exists_file('make_gclip_cache.pl'),!.
make_gclip_cache_file:- 
 tell('make_gclip_cache.pl'),

 format(':- style_check(-singletons). ~n'),
 make_gclip_cache_0,
 told.

make_gclip_cache:- 
 my_time((
 make_gclip_cache_file,
 consult('make_gclip_cache.pl'))),
 functor(G,gclip_cache,10),
 predicate_property(G,number_of_clauses(NC)),
 wdmsg(gclip_cache = NC).

% =====================================================================
is_fti_step(maybe_pbox_vm).
% =====================================================================
%:- luser_setval(individuated_cache,false).

maybe_pbox_vm(VM):- VM.option_pboxes==false,!.
maybe_pbox_vm(VM):- var(VM.option_pboxes), !, 
 (need_pboxes(VM.grid_o) -> (set(VM.option_pboxes) = true) ; (set(VM.option_pboxes) = false)),
  maybe_pbox_vm(VM).
maybe_pbox_vm(VM):- pbox_vm(VM),!.


need_pboxes(VM):- 
  testid_name_num_io(VM.id,TestID,_Example,_Num,IO),!,
  forall(kaggle_arc_io(TestID,trn+_,IO,G), \+ \+ (whole_row_or_col(C,G),C\==black)). 

whole_row_or_col(C,Center):- member(Row,Center), maplist(=(C),Row).
whole_row_or_col(C,Center):- Center\==[], Center\==[[]], rot90(Center,Cols),member(Col,Cols), maplist(=(C),Col).
not_whole_row_or_col(C,Center):- \+ whole_row_or_col(C,Center).



% =====================================================================
is_fti_step(pbox_vm).
% =====================================================================
pbox_vm(VM):- !,
   %GH is round(VM.h*2/3), GV is round(VM.v*2/3),
   GH is round(VM.h + 0), GV is round(VM.v + 0),
   findall(size2D(H,V),(l_s_4sides(H,V),H=<GH),Sizes_L_S),
   findall(size2D(H,V),(s_l_4sides(H,V),V=<GV),Sizes_S_L),
   GridI = VM.grid_o,
   get_black(Black),
   mapgrid(assign_plain_var_with(Black),GridI,GridM),
   shoot_lines_on_black_grid(GridM,Grid),
   add_top_bot_left_right(Top,_T,Grid,_B,Bot,LLeft,_LL,_RR,RRight,XSG),
   add_top_bot_left_right(Top2,_T2,XSG,_B2,Bot2,LLeft2,_LL2,_RR2,RRight2,_XSG2),
   %list_to_set(T,TColors),list_to_set(B,BColors),list_to_set(RR,RColors),list_to_set(LL,LColors),
   %intersection(TColors,BColors,BlackAndBorderV), intersection(LColors,RColors,BlackAndBorderH),
   %writeln(blackAndBorderHV=[BlackAndBorderH,BlackAndBorderV]),   
   maplist(=(N),Top), maplist(=(S),Bot), maplist(=(W),LLeft), maplist(=(E),RRight),
   maplist(=(N),Top2), maplist(=(S),Bot2), maplist(=(W),LLeft2), maplist(=(E),RRight2),
   % nth1(1,XSG,RTop),maplist(=(N),RTop), last(XSG,RBot),maplist(=(S),RBot),
   if_t( \+ (GH<4,GV<4),if_t(find_gridline_color(Grid,C),if_t(C\==black,NSEW=[C,C,C,C]))),
   NSEW = [N,S,E,W],
   if_t(nonvar(C),((nth1(1,XSG,RTop),maplist(=(N),RTop), last(XSG,RBot),maplist(=(S),RBot)))),
   if_t((GH<4,GV<4),NSEW=[n,s,e,w]),
   %if_t(var(N),maplist(ignore_equal(black),NSEW)),
   \+ \+ ((maplist(ignore_equal,NSEW,['N','S','E','W']),print_side_by_side(XSG-xsg,GridI-grid))),
  localpoints_include_bg(Grid,Points),!,
  begin_i_pbox_l(Grid,NSEW,XSG,Points,  Points1,VM,s_l(1),Sizes_S_L),
  begin_i_pbox_l(Grid,NSEW,XSG,Points1, Points2,VM,l_s(2),Sizes_L_S),
  if_t(Points\==Points2,set(VM.points) = Points2),
  begin_i_pbox_l(Grid,NSEW,XSG,Points,  Points3,VM,l_s(1),Sizes_L_S),
  begin_i_pbox_l(Grid,NSEW,XSG,Points3, Points9,VM,s_l(2),Sizes_S_L),
  if_t(Points\==Points9,set(VM.points) = Points9),
  !.

begin_i_pbox_l(Grid,NSEW,XSG,Points5,Points9,VM,S_L,Sizes_S_L):-
  copy_term(NSEW+XSG,CNSEW+CXSG),
  dmsg(begin(S_L)), my_time(must_det_ll((i_pbox_l(Grid,CNSEW,CXSG,Points5,Points9,VM,S_L,Sizes_S_L)))),!.



   %nop(i_pbox(VM,l_s(2),SizesRectS)), nop(i_pbox(VM,s_l(2),SizesSquareR)), nop(i_pbox(VM,s_l(2),SizesRectR)).

colors_of(T,TColors):- list_to_set(T,TColors).

side_ways(P2,I,O):- rot90(I,G),mapgrid(=,G,T), call(P2,T,M), rot270(M,O).
both_ways(P2,I,O):- mapgrid(=,I,T), call(P2,T,M),side_ways(P2,M,O).

shoot_lines_on_black_grid(Grid,Grid):-!.
shoot_lines_on_black_grid(Grid,GridO):- find_gridline_color(Grid,C), \+ is_black(C),!,GridO=Grid.
shoot_lines_on_black_grid(Grid,GridO):- Color = black,
  both_ways(shoot_lines_on_colored_row(LC,Color),Grid,GridO),
  LC=wbg.

% back half contains middle if odd V
grid_halves(Grid,Left,Odd,Right):- length(Grid,L), (is_odd(L)->Odd=odd;Odd=even), M is floor(L/2), length(Left,M), append(Left,Right,Grid).

find_gridline_color(Grid,C):- find_rowline_color(Grid,C) -> true ; (rot90(Grid,Grid90), find_rowline_color(Grid90,C)).
find_rowline_color(Grid,C):- grid_halves(Grid,_,OddEven,GridM), !, find_halfline_color(OddEven,GridM,C),!.

is_odd(N):- number(N), 1 is N rem 2.
find_halfline_color(odd,[[C|Row]|_],C):- maplist(==(C),Row).
find_halfline_color(_,Grid,C):- member([C|Row],Grid),maplist(==(C),Row), \+ is_black(C),!.
find_halfline_color(_,Grid,C):- member([C|Row],Grid),maplist(==(C),Row), is_black(C),!.



shoot_lines_on_colored_row(LC,C,Grid,GridO):- maplist(shoot_lines_on_rows(LC,C),Grid,GridO).

shoot_lines_on_rows(LC,C,Row,NewRow):- maplist(=(C),Row),!,length(Row,L),make_list(LC,L,NewRow).
shoot_lines_on_rows(_,_,Row,Row).

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
 print_grid(wqs(red,no_result_for(?-pbox(TestID>ExampleNum*IO))),G).

/*
pbox_io_result(TestID,ExampleNum,IO,G,[Objs]):- !,
 obj_global_grid(Obj,OGrid),
 print_side_by_side(orange,G,one_result_for(?-pbox(TestID>ExampleNum*IO)),_,OGrid,(TestID>ExampleNum*IO)),!.
*/

pbox_io_result(TestID,ExampleNum,IO,G,Objs):- !,
 once((maplist(obj_global_grid,Objs,OGG), print_side_by_side(OGG))),!,
 print_side_by_side(cyan,G,(?-pbox(TestID>ExampleNum*IO)),_,print_grid(Objs),(TestID>ExampleNum*IO)),!.

i_pbox(GridIn,Objs):- 
  ROptions = pbox_vm,
  do_ig(ROptions,GridIn,IndvS),
  into_grid(GridIn,Grid),
  locally(nb_setval(debug_as_grid,t),
   locally(nb_setval(individuated_cache,false),
    show_ig(igo,ROptions,GridIn,Grid,IndvS))),
  maybe_subdiv(IndvS,Objs).

maybe_subdiv([OO],Objs):- object_grid(OO,G),i(i_pbox,G,Objs),!.
maybe_subdiv(Objs,Objs).

obj_global_grid(X,G-wqs(DSC)):- get_black(Black), localpoints(X,Grid),subst(Grid,Black,wbg,G), vis2D(X,VH,VV), loc2D(X,OH,OV),!,
  DSC =[loc2D(OH,OV),vis2D(VH,VV)].
obj_global_grid(X,G-wqs(DSC)):- get_black(Black), global_grid(X,Grid),subst(Grid,Black,wbg,G), vis2D(X,VH,VV), loc2D(X,OH,OV),!,
  DSC =[loc2D(OH,OV),vis2D(VH,VV)].

not_in_eq(Set,V):- \+ member_eq(V, Set).
member_eq(V,Set):- member(VV,Set),VV == V,!.

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
  term_variables(In,Inside),
  append([Top,Bot,LLeft,RRight],OBorder))),!.

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

%pbox_phase_check(A, B):- A=s_l(_)-> B=s_l(_); B=l_s(_).
pbox_phase_check(A, B):- A==B.
%pbox_phase_check(_, _).

:- abolish(cached_make_search_box,11).
:- dynamic(cached_make_search_box/11).

make_search_box(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs ):- 
  % between(1,32,H),between(1,32,V),
  %s_l_4sides(H,V),
  HH is H,VV is V, once(make_search_box_m1(HH,VV,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs)).

make_search_box_m1(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs):- cached_make_search_box(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs),!.
make_search_box_m1(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs):-
  make_search_box_fresh(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs),
  asserta(cached_make_search_box(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs)).  

make_search_box_fresh(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs):-
  make_search_box_fresh(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder),
  nsew_edges_trimed(H,V,In,IBorderDivs),
  HH is H+1,VV is V+1,
  nsew_edges_trimed(HH,VV,Find,OBorderDivs),!.

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
trim_ends(T,TT):- append([_|TT],[_],T),!.
trim_ends(_,[]):-!.

not_irow(C,T):- trim_ends(T,TT), \+ maplist(=(C),TT).
not_edge(C,Find):- append([T|_],[B],Find), not_irow(C,T),  not_irow(C,B),
  rot90(Find,Find90),append([R|_],[L],Find90), not_irow(C,R), not_irow(C,L).

nsew_edges(H,V,_,[[],[],[],[]]):- (H<3;V<3),!.
nsew_edges(_H,_V,Find,Rows):- must_det_ll((append([T|_],[B],Find),rot90(Find,Find90),append([R|_],[L],Find90), Rows = [T,B,R,L])).
nsew_edges_trimed(H,V,Find,Trims):- nsew_edges(H,V,Find,Rows),maplist(trim_ends,Rows,Trims).

member_e(List,E):- nonvar(E), member(E,List),!.
maybe_swap(C1,C2,C1,C2). maybe_swap(C1,C2,C2,C1).


i_pbox_l(_Grid,_NSEW,_XSG,Points,Points,_VM,L_S,_):- Points==[], !, wdmsg(pointless(L_S)).
i_pbox_l(_Grid,_NSEW,_XSG,Points,Points,_VM,L_S,[]):- !, wdmsg(complete(L_S)).
i_pbox_l(Grid,NSEW,XSG,Points,Points9,VM,L_S,[size2D(H,V)|Sizes]):- 
  Which = _,
  make_search_box(H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs),
  ogs_11(FH,FV,Find,XSG),
  found_box(L_S,NSEW,FH,FV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs, OBJ,WHY),
  OBJ \=@= Grid,
  OBJ \== [],
  (OBJ=@=Find -> (break, OH is FH-1, OV is FV-1) ;
   (OBJ=@=Center -> (Which = center,OH is FH, OV is FV) ;  
    (OBJ=@=In -> (Which = in,OH is FH, OV is FV) 
     ; (break, OH is FH-0, OV is FV-0)))),
  obj_gpoints(OBJ,OH,OV,GOPoints),
  intersection(Points,GOPoints,Intersection,LeftOver,Unknown), 
  Intersection\==[],!,
  must_det_ll((
    functor(L_S,F,_),functor(WHY,Y,_),
    nop(Unknown==[]),  
  ((\+ \+ (maplist(ignore_equal,NSEW,['N','S','E','W']),
    %grid_size(OBJ,HH,VV), EH is OH+HH-1,EV is OV+VV-1, clip(OH,OV,EH,EV,Grid,OGrid), print_side_by_side(cyan,OGrid,Y,_,OBJ,F),
    print_side_by_side(yellow,Find,w(Which, WHY),_,OBJ,cpmt(o=loc2D(OH,OV),size2D(H,V),NSEW,L_S))))),

  make_indiv_object(VM,[birth(pbox(Y,F))],GOPoints,_Obj),
  OHM1 is OH -1,OVM1 is OV -1,
  grid_size(OBJ,HH,VV), EHP1 is OH+HH, EVP1 is OV+VV,  clip(OHM1,OVM1,EHP1,EVP1,Grid,SGrid),
  (( L_S=s_l(_),
     OBJ =@= Center,
     fail,
     iz_symmetry(SGrid,_H,_V,_N,R), R \= symmetry(none),   
     obj_gpoints(SGrid,OHM1,OVM1,SOPoints),
     intersection(LeftOver,SOPoints,SIntersect,LeftOver2,_),
     SIntersect\==[],
     make_indiv_object(VM,[birth(pbox2(Y,F))],SIntersect,Obj2),
     print_side_by_side(cyan,SGrid,Y,_,[Obj2],F))-> true
   ; (LeftOver2=LeftOver)),
  i_pbox_l(Grid,NSEW,XSG,LeftOver2,Points9,VM,L_S,[size2D(H,V)|Sizes]))). 

i_pbox_l(Grid,NSEW,XSG,Points,Points9,VM,L_S,[_|Sizes]):- i_pbox_l(Grid,NSEW,XSG,Points,Points9,VM,L_S,Sizes).

existingObject(VM,GOPoints):- 
  member(O,VM.objs),globalpoints_include_bg(O,Ps),
  GOPoints==Ps,!.

obj_gpoints(OBJ,OH,OV,GOPoints):- localpoints_include_bg(OBJ,OPoints), offset_points(OH,OV,OPoints,GOPoints).

:- style_check(-singleton).

/* ___________
  | T T T T T |
  | T t t t T |
  | T t   t T |
  | T t t t T |
  | T T T T T |
   ¯¯¯¯¯¯¯¯¯¯¯
*/

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   In, insideSolid(C)):-  fail,
  pbox_phase_check(L_S,s_l(1)),
   (H>1,V>1), maplist(==(C),Inside), \+ maplist(==(C),OBorder), maplist(=(B),OBorder).

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   Center, unRibs(L_S,C,H,V)):- 
   H>3,V>3, pbox_phase_check(L_S,s_l(1)),  C\==black,  not_whole_row_or_col(C,Center), \+ ((member(Row,Center),append(_,[C,C,C|_],Row))).

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   Center, ring(L_S,C,H,V)):- H>4,V>4, fail,
   \+ \+ (member(NB,Cents), NB \= C), not_whole_row_or_col(C,Center), C==black.

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   Center, innerThang(L_S,C,H,V)):- 
   maplist(=(C),OBorder),!, C\==black, pbox_phase_check(L_S,l_s(2)), \+ member(C,Cents).

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   Center, innerThang(C)):- 
   maplist(=(C),OBorder),!, C\==black, \+ member(C,Cents).

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   Ans, midThang(L_S,C,H,V)):- 
   pbox_phase_check(L_S,l_s(2)),   
   \+ maplist(=(C),OBorder), 
   not_whole_row_or_col(C,Center),
   member(C,Center),
   \+ is_black(C),  
   (is_black(C)->  Ans = Center ; (Ans = In, not_edge(C,Find))).


p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   In, midThang1(C)):- 
  (pbox_phase_check(L_S,s_l(2)), \+ \+ ([B,B,B,B]=NSEW, maplist(=(C),OBorder), \+ maplist(==(C),IBorder), 
    nop( \+ (member(Row,In), maplist(=(C),Row) )))).          

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   In, midThang2(L_S,C,H,V)):- 
        %  fail,
  (pbox_phase_check(L_S,l_s(2)), \+ \+ ([n,s,e,w]=NSEW, different_enough(L_S,In,Find,Inside,OBorder))).

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   In, outerThing(BC)):-
  pbox_phase_check(L_S,l_s(2)),
  is_black(C), 
  maplist(=(BC),OBorder), BC\==C,!.

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   In, outerThing2n(L_S,BC,C,H,V)):- 
  pbox_phase_check(L_S,l_s(2)),
  maplist(=(BC),OBorder), BC\==C,!.

p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   Center, twang):- fail,

    \+ \+ if_t((maplist(==(C),Inside), \+ maplist(=(C),OBorder)),
       \+ \+ if_t(not_edge(C,Find), if_t(\+ append(_,[C,C,C|_],OBorder), nop(print_grid(ogs_in0(L_S,C,OH,OV,H,V,OBorder),In))))),
    %if_t( \+ mass(In,0),print_grid(ogs_in(L_S,C,OH,OV,H,V),In)),

    % ( \+ member(C,OBorder) ; C==black),
/*
    (L_S == l_s(2) ->  \+member(C,OBorder) ; ( \+member(C,OBorder) ; C==black)),
      if_t(L_S == l_s(2),(
       \+ (member(Row,Center), maplist(=(C),Row)),

       \+ (Center\==[], Center\==[[]],rot90(Center,Cols),member(Col,Cols), maplist(=(C),Col)) )),
*/

(
    if_t( \+ \+ (is_black(C),not_whole_row_or_col(C,Center)),
      if_t(Center\=[[_]],print_grid(inside1(L_S,C,OH,OV,H,V),Center))),
 

    (  ( fail, \+ member(C, Cents)) -> print_grid(inside(L_S,C,OH,OV,H,V),Center);
         \+ \+  if_t(Center\==In, 
               \+ \+  if_t(not_whole_row_or_col(C,In),print_grid(found1(L_S,C,OH,OV,H,V),In)))),
   \+ \+  if_t(( \+ member(C,OBorder), \+ maplist(=(Black),OBorder)),
       ((print_grid(inGood(L_S,C,OH,OV,H,V),In),nop((print_grid(found2(L_S,C,OH,OV,H,V),Find))))))).
    %if_t(maplist(==(C),Inside),print_grid(find1(L_S,C,OH,OV,H,V),Find)),
    %maplist(=(C),Center,Cents) -> make_obj(In,solid_rect);
    %-> make_obj(In,solid_rect);


  %  (maplist(=(C),Inside), \+ member(C,OBorder)) -> ObjG = In ;
  %  (maplist(=(C),Inside), \+ member(C,OBorder))

is_compass(A):- atom(A),member(A,[n,s,e,w]).

black_and(Colors,C):- select(C,Colors,Rest),Rest==[black],!.

member1(C,N):- select(CC,N,R), C==CC, \+ (member(C2,R),C2==C).

found_box_s(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs, CentS,InsideS,IBorderS,OBorderS,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  maplist(=(C),Inside), CentS=[_|_], nonvar(C),
  IBorderS\==[C],
  \+ member(C,OBorderS), OBJ=Center,
  WHY = solidCenter(CentS,C).

found_box_s(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs, CentS,InsideS,IBorderS,OBorderS,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  maplist(=(C),Inside), nonvar(C),
  \+ member(C,OBorderS), OBJ=In,
  WHY = solidCenterIn(CentS,C).

found_box_s(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs, CentS,InsideS,IBorderS,OBorderS,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  IBorderDivs = [N,S,E,W],
  member1(C,N),member1(C,S),member1(C,E),member1(C,W),
  \+ member(C,Cents),
  \+ member(C,OBorder),
  %restOfGrid(XSG,OH,OV,H,V,Rest), \+ member(C,Rest).
  OBJ=In,
  WHY = fourDots(CentS,C).


found_box_s(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs, CentS,InsideS,IBorderS,OBorderS,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  InsideS==[_,_|_],
  OBorderS==[_,_|_],
  \+ member(black,InsideS), 
  member(black,OBorderS), 
  OBJ=Center,
  WHY = solidCenter1(InsideS,OBorderS).


found_box_s(L_S,NSEW,OH,OV,Find,XSG,H,V,_Center,Center,Find,Cents,Inside,_IBorder,IBorder,IBorderDivs,OBorderDivs,_CentS,CentS,_IBorderS,IBorderS,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  maplist(=(C),IBorderS),
  %\+ maplist(=(I),IBorderS),
  %if_t(maplist(=(B),OBorderS),(C\==B;C==black)),
  if_t(nonvar(C),CentS\==[C]),
  CentS=[_|_],
  if_t(nonvar(C),not_whole_row_or_col(C,Center)),
  %(),
  %if_t(C\==black, \+ member(C,CentS)),
  %nonvar(C),CentS\==[C],CentS=[_|_],
  OBJ=Center,
  WHY = solidInnerBorderI_O(CentS,C).


found_box_s(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs, CentS,InsideS,IBorderS,OBorderS,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  maplist(=(C),IBorderS),
  C\==black,
  %if_t(maplist(=(B),OBorderS),(C\==B;C==black)),
  if_t(nonvar(C),CentS\==[C]),
  CentS=[_|_],
  if_t(nonvar(C),not_whole_row_or_col(C,Center)),
  %(),
  %if_t(C\==black, \+ member(C,CentS)),
  %nonvar(C),CentS\==[C],CentS=[_|_],
  %(C==black -> OBJ=In; OBJ=In),
  %OBJ=Center,
  \+ member(InsideS,Black),
  OBJ=In,
  WHY = solidInnerBorder0(CentS,C).





found_box_s(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs, CentS,InsideS,IBorderS,OBorderS,  OBJ,WHY):- 
 % (pbox_phase_check(L_S,s_l(2));pbox_phase_check(L_S,l_s(1))),
  fail,
  InsideS=[C],C\==black,
  OBorderS = [_,_|_],
  \+ member(black,OBorderS),
  \+ member(C,NSEW),
  OBJ=In,
  WHY = dashedOuterBorder(OBorderS,C).



found_box_s(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs, CentS,InsideS,IBorderS,OBorderS,  OBJ,WHY):- fail,
  pbox_phase_check(L_S,l_s(2)),
  maplist(=(C),IBorderS),
  if_t(maplist(=(B),OBorderS),(C\==B;C==black)),
  if_t(nonvar(C),CentS\==[C]),
  CentS=[_|_],
  if_t(C\==black,not_whole_row_or_col(C,Center)),
  %(),
  %if_t(C\==black, \+ member(C,CentS)),
  %nonvar(C),CentS\==[C],CentS=[_|_],
  OBJ=Center, WHY = solidInnerBorder1(CentS,C).


found_box_s(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs, CentS,InsideS,IBorderS,OBorderS,  OBJ,WHY):- fail,
  V>3,H>3,
  pbox_phase_check(L_S,l_s(1)),
  black_and(IBorderS,C),maplist(\==(C),OBorderS),
  (black_and(CentS,C); \+ member(C,CentS)),
  OBJ=Center,!,WHY = solidInnerBorder2(CentS).

found_box_s(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs, CentS,InsideS,IBorderS,OBorderS,  OBJ,WHY):- fail,
  pbox_phase_check(L_S,l_s(1)),
  InsideS=[C], \+ maplist(\==(C),OBorderS),
  OBJ=In,!,WHY = solidIn(C).

found_box_s(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs, CentS,InsideS,IBorderS,OBorderS,  OBJ,WHY):-
  pbox_phase_check(L_S,l_s(1)),
  IBorderS=[C],maplist(\==(C),InsideS),
  OBJ=Center,!,WHY = solidInnerBorder2(C).



found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   _, _):- fail,
  member(C,Inside),is_real_color(C),maplist(==(C),Inside), maplist(=(C),OBorder),!,fail.

found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   OBJ,WHY):- 
 % pbox_phase_check(L_S,l_s(2)),
  H>1,V>1,
  once((list_to_set(Cents,CentS),list_to_set(Inside,InsideS),list_to_set(IBorder,IBorderS),list_to_set(OBorder, OBorderS))),
  found_box_s(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs, CentS,InsideS,IBorderS,OBorderS,  OBJ,WHY),
  nop((MOBJ\==In -> MOBJ=OBJ ; ( (maplist(=(C),IBorder), nop((\+ member(C,Cents))) ) -> OBJ=Center ; OBJ=MOBJ))).


found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   Center, 
                       dottedBoxCenter(border1(C1,C2),NSEW,L_S,H,V)):- H>3,V>3, 
  pbox_phase_check(L_S,s_l(1)),
    ((my_partition(=(C),IBorder,C1s,C2s),length(C1s,N1),length(C2s,N2),max_min(N1,N2,Max,Min),Min==4)),!.
%    \+ member(C,OBorder), \+ member(C,Cents))).


found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   In, largeInBlack(NSEW,L_S,H,V)):-  
  pbox_phase_check(L_S,l_s(2)),
  CNSEW=[B,B,B,B], B =black,
   \+ \+ ((NSEW=CNSEW,
         maplist(=(B),OBorder), (member(C,IBorder), \+ is_black(C)))),!, not_whole_row_or_col(C,In).


found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   In, find_pbox(L_S,C1-C2=C3-C4)):-  
  pbox_phase_check(L_S,l_s(2)),
  H>3,V>3,
  once((list_to_set(Find,FindS),
  list_to_set(OBorder, OBorderS),
  pred_intersection(==,FindS,OBorderS,Common,_,FindU,OBorderU))),
  [Common,FindU,OBorderU] = All,
  ground(All),
  [Common,FindU,OBorderU]=[[_],[_],[_]].


found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   In, find_ogs(L_S,C,NN,OBorderC)):- 
  pbox_phase_check(L_S, l_s(1)),
  H>3,V>3,
  maplist(=(C),Inside),
  get_edges(In,Top,Bottem,Left,Right),
  maplist(trim_ends,[Top,Bottem,Left,Right],TrimmedRows),
  findall(E,(member(E,TrimmedRows),maplist(=(C),E)),Borders),
  length(Borders,NN),
  \+ maplist(==(black),Inside),
  list_to_set(OBorder,OBorderC).

/*

found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   Center, boxCenter(NSEW,L_S,H,V)):-
  pbox_phase_check(L_S,l_s(2)),
   \+ \+ ((maplist(=(C),IBorder), \+ member(C,OBorder), \+ member(C,Cents))).

*/
found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   OBJ, WHY):-
  maplist(=(C),IBorder),!,p1_found_box(L_S,NSEW,OH,OV,C,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   OBJ, WHY).

found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   In, different_enough(L_S,CNSEW)):-  
  [n,s,e,w] = CNSEW,
 % !, fail,    
  \+ \+ ((CNSEW=NSEW,different_enough(L_S,In,Find,Inside,OBorder))),!,
  if_t((member(C,OBorder),nonvar(C)),
       not_whole_row_or_col(C,In)).

/*
found_box(L_S,NSEW,OH,OV,Find,XSG,H,V,Center,In,Find,Cents,Inside,IBorder,OBorder,IBorderDivs,OBorderDivs,   In, different_enough(NSEW)):- 
  %[n,s,e,w]=NSEW,
  !, fail,
  different_enough(L_S,In,Find,Inside,OBorder).
*/


different_enough(L_S,In,Find,Inside,OBorder):-
  a_portion_colors(Inside,InsideC,InsideV,InsideBG,InsideFG),
  a_portion_colors(OBorder, BorderC,BorderV,BorderBG,BorderFG),
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

