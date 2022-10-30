/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


% =====================================================================
is_fti_step(pbox_vm).
% =====================================================================
:- luser_setval(individuated_cache,false).

pbox_vm(VM):- !,
   %GH is round(VM.h*2/3), GV is round(VM.v*2/3),
   GH is round(VM.h - 1), GV is round(VM.v - 1),
   findall(size(H,V),(between(1,GH,H),between(1,GV,V),H=V),SizesSquare),
   findall(size(H,V),(between(1,GH,H),between(1,GV,V),H\=V),SizesRect),
   predsort(sort_on(neg_h_v_area),SizesSquare,SizesSquareS),
   predsort(sort_on(neg_h_v_area),SizesRect,SizesRectS),
   reverse(SizesSquareS,SizesSquareR),
   reverse(SizesRectS,SizesRectR),
  % list_to_set([size(3,3),size(2,2)|SizesSquareS],Sizes_L_S),
   append(SizesSquareS,SizesRectS,Sizes),
   predsort(sort_on(neg_h_v_area),Sizes,Sizes_L_S),
   reverse(Sizes_L_S,Sizes_S_L),
   i_pbox(VM,l_s,Sizes_L_S),
   nop(i_pbox(VM,l_s,SizesRectS)),
   nop(i_pbox(VM,s_l,SizesSquareR)),
   nop(i_pbox(VM,s_l,SizesRectR)),
   i_pbox(VM,s_l,Sizes_S_L),!.



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

pbox_pair(TestID,ExampleNum,I,O):-
   wdmsg(?- test_p2(pbox_pair(TestID,ExampleNum))),
   pbox_io(TestID,ExampleNum,in,I), pbox_io(TestID,ExampleNum,out,O).

pbox_io(TestID,ExampleNum,IO,G0):-
  wdmsg(?- pbox_io(TestID,ExampleNum,IO)),
  (into_grid(G0,G)->true;into_grid((TestID>ExampleNum*IO),G)), 
  duplicate_term(G,GG),
  ignore(kaggle_arc(TestID,ExampleNum,IO,GG)),
  set_current_test(TestID),
  my_time((i_pbox(GG,Objs),
  pbox_io_result(TestID,ExampleNum,IO,GG,Objs))).

pbox_io_result(TestID,ExampleNum,IO,G,[]):- !,
 print_grid(wqs(red,(?-pbox(TestID>ExampleNum*IO))),G).

pbox_io_result(TestID,ExampleNum,IO,G,[Objs]):- !,
 print_side_by_side(orange,G,(?-pbox(TestID>ExampleNum*IO)),_,print_grid(Objs),(TestID>ExampleNum*IO)),!.

pbox_io_result(TestID,ExampleNum,IO,G,Objs):- !,
 once((maplist(obj_global_grid,Objs,OGG), print_side_by_side(OGG))),!,
 print_side_by_side(cyan,G,(?-pbox(TestID>ExampleNum*IO)),_,print_grid(Objs),(TestID>ExampleNum*IO)),!.

i_pbox(GG,Objs):- ((i(i_pbox,GG,OO),maybe_subdiv(OO,Objs))).

maybe_subdiv([OO],Objs):- object_grid(OO,G),i(i_pbox,G,Objs),!.
maybe_subdiv(Objs,Objs).

obj_global_grid(X,G-wqs(DSC)):- get_black(Black), global_grid(X,Grid),subst(Grid,Black,wbg,G), v_hv(X,VH,VV), loc(X,OH,OV),!,
  DSC =[loc(OH,OV),v_hv(VH,VV)].

not_in_eq(Set,V):- \+ (member(VV,Set),VV == V).

no_rule(_,_).
dif_fg(X,Y):- nop((freeze(X,freeze(Y,((is_fg_color(X);is_fg_color(Y))->dif(X,Y)))))).

is_compass(A):- atom(A),member(A,[n,s,e,w]).
a_portion_colors(IA,Compass,Vars,BG,FG):- 
  my_partition(is_compass,IA,Compass,Colors),
  my_partition(is_bg_color,Colors,BG,VFG),my_partition(is_fg_color,VFG,FG,Vars).


different_enough(L_S,In,Find,Inside,Border):-
  a_portion_colors(Inside,InsideC,InsideV,InsideBG,InsideFG),
  a_portion_colors(Border,BorderC,BorderV,BorderBG,BorderFG),
  maplist(sort,[BorderC,BorderV,BorderBG,BorderFG,InsideC,InsideV,InsideBG,InsideFG],
               [SBorderC,SBorderV,SBorderBG,SBorderFG,SInsideC,SInsideV,SInsideBG,SInsideFG]),
  different_enough_c(In,Find,L_S,SBorderC,SBorderV,SBorderBG,SBorderFG,
                                               SInsideC,SInsideV,SInsideBG,SInsideFG),!.
  %\+ not_different_enough_ii(BorderV,BorderBG,BorderFG,InsideV,InsideBG,InsideFG).

%        different_enough_c(_I,_F,BorderV,BorderBG,BorderFG,   InsideV,InsideBG,InsideFG):-!.

has_blank_rows(I,N):- findall(Row,(append([_|_],[Row,_|_],I), maplist(cmatch(is_bg_color),Row)),N).
%has_blank_rows(I,N):- findall(Row,(member(Row,I), maplist(cmatch(is_bg_color),Row)),N).
has_blank_cols(C,N):- rot90(C,I),has_blank_rows(I,N).

different_enough_c(_I,_F,l_s,   _, _,           [_],        [],    
                               _, _,            [],    [_,_|_]):- !, % 0b148d64
  true.
/*
different_enough_c(I,_F,l_s,  [N,W], _,        [_],        [],    
                               _,   [],        [_],       [_]):- w_in_90(N,W),!, % 0b148d64
  \+ has_blank_rows_or_cols(I).
*/


different_enough_c(_I,_F,_,  _,_,      [_],      [],    
                              _,_,     [_],      []):- !,fail.

different_enough_c(I,_F,l_s,   _, _,           [_],        [],    
                               _,   [],        _,       [_]):- !, % 0b148d64
  max_blanks(I,1).




different_enough_c(_I,_F,l_s,  _, _,            [],        [FG],    
                              _, [],          _ ,         [H|T]):- \+ member(FG,[H|T]).  % 09629e4f

different_enough_c(I,_F,_S_l,  [],    _,        [_],        [],    
                                _,   [],        [_],       [_]):- !, % 05f2a901
                                 max_blanks(I,1).

different_enough_c(_I,_F,l_s,  _,    _,         [],        [_],    
                                _,   [],        [_],       []):- !, % 06df4c85
   true.

/*
*/

different_enough_c(I,_F,l_s,   _, _,           [_],        [],    
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

add_top_bot(Top,T,In,B,Bot,TInB):-
 must_det_ll((
  last(In,B), [T|_]=In,
  length(T,H), length(Top,H), length(Bot,H),
  append([Top|In],[Bot],TInB))).

make_squarish(BorderRule,In,NewSearch,Inside,Border):-
 must_det_ll((
  add_top_bot_left_right(Top,T,In,B,Bot,LLeft,LL,RR,RRight,NewSearch),
  maplist(BorderRule,Top,T), maplist(BorderRule,Bot,B),  maplist(BorderRule,LL,LLeft), maplist(BorderRule,RRight,RR),
  term_variables(NewSearch,FNewSearch), term_variables(In,Inside),
  include(not_in_eq(Inside),FNewSearch,Border))),!.

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


neg_h_v_area(size(H,V),NArea):- NArea is H * (- V).


i_pbox_detect(L_S,H,V,XSG,NSEW,In,OH,OV):- 
  H>=2,V>=2,
  B = blue,
 once((
    HH is H -1,VV is V -1,
    make_grid(HH,VV,InM1),
    make_squarish(no_rule,InM1,In,     _, BorderM1),
    make_squarish(no_rule,In,Find, Inside,Border))),
    ogs_11(OH,OV,Find,XSG),
    once( 
          (s_l == L_S, \+ \+ ([B,B,B,B]=NSEW, maplist(=(C),Border), \+ maplist(==(C),BorderM1), nop( \+ (member(Row,In), maplist(=(C),Row) ))))
          ;
          (/*s_l == L_S,*/ \+ \+ ([B,B,B,B]=NSEW, maplist(=(C),BorderM1), \+ maplist(==(C),Border), nop( \+ (member(Row,In), maplist(=(C),Row) ))))
          ;
          fail
          ;
          (l_s == L_S, \+ \+ ([n,s,e,w]=NSEW, different_enough(L_S,In,Find,Inside,Border)))).
/*
i_pbox_detect(L_S,H,V,XSG,[n,s,e,w],In,OH,OV):- 
 H>=2,V>=2,
 once((
   make_grid(H,V,In),
   make_squarish(no_rule,In,Find,Inside,Border))),
  % print_grid(in,In),ptv(in=In), print_grid(ns,Find),ptv(ns=Find), print_grid(s,XSG),ptv(s=XSG),
  ogs_11(OH,OV,Find,XSG),
  \+ \+ different_enough(L_S,In,Find,Inside,Border).
*/

i_pbox(_VM,_L_S,[]):-!.
i_pbox(VM,L_S,[size(H,V)|Sizes]):-
  once((
   GridO= VM.grid,  
   duplicate_term(GridO,GridI),
   duplicate_term(GridO,GridM),
   set(VM.grid) = GridM,
   get_black(Black),
   mapgrid(assign_plain_var_with(Black),GridI,Grid),
   add_top_bot_left_right(Top,_T,Grid,_B,Bot,LLeft,_LL,_RR,RRight,XSG),
   NSEW = [N,S,E,W],
   maplist(=(N),Top), maplist(=(S),Bot), maplist(=(W),LLeft), maplist(=(E),RRight))),   
   i_pbox_detect(L_S,H,V,XSG,NSEW,In,OH,OV),

  %pp(ogs_1(OH,OV,Inside,Border,Find)),   
  once((localpoints_include_bg(In,OPoints),offset_points(OH,OV,OPoints,GOPoints))),
  % \+ existingObject(VM,GOPoints),
  intersection(VM.points,GOPoints,Intersection), Intersection\==[],!,
  must_det_ll((
  %indv_props(Obj,Props),my_partition(is_point_or_colored,Props,_,PropsRetained),
  make_indiv_object(VM,[],GOPoints,_Obj),
  %offset_grid(OH,OV,In,OffsetGrid),!, is_grid(OffsetGrid),
  %OffsetGrid = In,
  %as_debug(1,((dash_chars,Info=ogs_1(OH,OV), print_side_by_side(Grid-Info,[Obj]-Info)))), % trace,
  %print_ss([Obj|Grid]-wqs(maybe_ogs_color(R,OH,OV))), %  trace,  
  %print_grid(maybe_ogs_color(R,OH,OV),[Obj|Grid]), %  trace,  
  remCPoints(VM,GOPoints), % =GOPoints,
  remGPoints(VM,Intersection),
  i_pbox(VM,L_S,[size(H,V)|Sizes]))). 

i_pbox(VM,L_S,[_|Sizes]):-
 i_pbox(VM,L_S,Sizes).


existingObject(VM,GOPoints):- 
  member(O,VM.objs),globalpoints_include_bg(O,Ps),
  GOPoints==Ps,!.

