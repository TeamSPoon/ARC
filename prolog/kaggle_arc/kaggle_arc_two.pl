
/*
luser_getval(ID,N,V):- 
 (arc_user_prop(ID,N,V)*->true;
  (nb_current(N,V))*->true;arc_user_prop(global,N,V)).
*/
:- luser_default(example,trn+0).


:- luser_default(no_diags,false).

%c:- forall(clause(fav(A,B),true),arc_history1((fav(A,B)))).
:- arc_history1(fav2).
:- arc_history1(arc2).
:- arc_history1(arc).
:- arc_history1(arc1).
:- arc_history1(fav).
:- arc_history1(fav1).
:- arc_history1(fav3).

:- multifile(mregression_test/0).
:- dynamic(mregression_test/0).

:- include(kaggle_arc_footer).

%:- forall((fav(_,P),flatten([P],Flat),member(E,Flat)), assert_if_new(fav_trait(E))).


run_nb(G):- call(G).
%run_nb(G):- setup_call_cleanup(G,true,notrace).

arc:- forall(arc11,true).
arc1:- clsmake, test_names_by_hard(X), whole_test(X).
arc2:- clsmake, test_names_by_hard_rev(X), whole_test(X).
arc11:- clsmake, test_names_by_hard(X), arc1(X).
arc22:- clsmake, test_names_by_hard_rev(X), arc1(X).
arc3:- clsmake, arc1(v('009d5c81')).
arc4:- clsmake, arc1(t('25d487eb')).
arc5:- clsmake, arc1(v('1d398264')).

fav3:- clsmake, arc1(t('3631a71a')>(_+_)),!.
fav:- clsmake,forall(fav11,true).
favr:- clsmake,forall(fav22,true).
fav1:- clsmake, test_names_by_hard_rev(X), whole_test(X).
fav2:- clsmake, test_names_by_fav_rev(X), whole_test(X).
fav11:- clsmake, test_names_by_fav(X), arc1(X).
fav22:- clsmake, test_names_by_fav_rev(X), arc1(X).
favL:- clsmake, get_current_test(X),!,whole_test(X).
favC:- clsmake, set_current_test(Y), UT=until_test(Y),!,
  test_names_by_hard(X),until_test(X)=UT,nb_setarg(1,UT,_),whole_test(X).

whole_test(X):- cls1, with_tty_raw(interactive_test(X)).
%whole_test(X):- cls1, noninteractive_test(X).

fav(X):- nonvar(X),!, clsmake, arc1(X).
fav(X):- clause(fav(X,_),true).

arc(TestID):- time(forall(arc1(true,TestID),true)).

arc1(TName):- arc1(true,TName).
%arc1(G,TName):- arc2(G,TName,(_+0)).


arc1(G,TName):-
 set_current_test(TName),
 fix_test_name(TName,TestID,_UExampleNum), 
 locally(set_prolog_flag(gc,true),
  (clear_shape_lib(TestID),
   nb_delete('$training_vm'),
   % choice point created here purposely
  forall(kaggle_arc(TestID,ExampleNum,_In,_Out),
  ignore((catch((call(G),
    run_arc_io(TestID,ExampleNum)),'$aborted',true)))))).


is_detatched_thread:- arc_webui,!.
is_detatched_thread:- \+ (thread_self(Main) -> Main == main ; main==0),!.

cls_z:- is_detatched_thread,!,flush_tee.
cls_z:- catch(cls,_,true), flush_tee, nop((clear_tee,clear_test_html)).
cls1:- nop(catch(cls_z,_,true)).

list_to_rbtree_safe(I,O):- must_be_free(O), list_to_rbtree(I,M),!,M=O.
:- dynamic(is_buggy_pair/2).
%is_buggy_pair(v(fd096ab6)>(trn+0), "BUG: System Crash").
%is_buggy_pair(t('3631a71a')>(tst+0),"segv").
%is_buggy_pair(t('27a28665')>(tst+2), "BUG: Re-Searcher gets stuck!").

run_arc_io(TestID,ExampleNum):- Pair = (TestID>ExampleNum), is_buggy_pair(Pair,Why),!,format("~N1 % Skipping ~q because: ~w ~n~n",[Pair,Why]).
run_arc_io(TestID,ExampleNum):- 
  time(train_test(TestID)),
  time(solve_test(TestID,ExampleNum)).

get_training(Training):- luser_getval('$training_vm',Training),compound(Training),!.
get_training(Tree):- list_to_rbtree([p-q],T),!,ignore(Tree=T),!.
get_training(Training):- must_det_ll(((
  get_current_test(TestID), make_training(TestID,Training), !,
  luser_linkval('$training_vm',Training)))),!.
set_training(Training):- luser_linkval('$training_vm',Training).
set_training(Prop,Value):- get_training(Training), gset(Training.Prop)=Value.
get_training(Prop,Value):- get_training(Training), get_kov(Prop,Training,Value).
set_vm(VM):- luser_linkval('$grid_vm',VM).
get_vm(VM):- luser_getval('$grid_vm',VM),!.
get_vm(VM):- ndividuator,!,luser_getval('$grid_vm',VM),!.

peek_vm(VM):- luser_getval('$grid_vm',VM),!.
peek_vm(Key,Value):- luser_getval('$grid_vm',VM)->get_kov(Key,VM,Value);luser_getval(Key,Value).

set_vm(Prop,Value):- ignore(luser_getval('$grid_vm',VM)),
 luser_set_vm(VM,Prop,Value).

luser_set_vm(VM,Prop,Value):- var(VM), !, print_grid(luser_set_vm(Prop),Value).
luser_set_vm(VM,Prop,Value):-
 (get_kov1(Prop,VM,_) -> gset(VM.Prop) = Value ; 
  (get_kov1(props,VM,Hashmap) -> 
    (var(Hashmap)->(list_to_rbtree([Prop-Value],Hashmap),gset(VM.props)=Hashmap); must_not_error( gset(Hashmap.Prop)=Value));
      (list_to_rbtree([Prop-Value],Hashmap),gset(VM.props)=Hashmap))).

set_vm_obj(Prop,Or,Value):- set_vm(Prop,Value),ignore(set_vm_obj1(Prop,Or,Value)),!.

set_vm_obj1(Prop,Or,Value):- is_grid(Value),!,
  localpoints_include_bg(Value,IndvPoints),
  grid_size(Value,H,V),
  if_t(IndvPoints\==[],
    (get_vm(VM),
          make_indiv_object(VM,[iz(Prop),vis2D(H,V),birth(set_vm(Prop))|Or],IndvPoints,_Obj),
          %addObjects(VM,Obj),
          make_bg_visible(Value,VValue),
          print_grid(H,V,Prop,VValue))),!.

set_vm_obj1(Prop,Or,IndvPoints):- is_points_list(IndvPoints),!,
  if_t(IndvPoints\==[],
    (get_vm(VM),          
      make_indiv_object(VM,[iz(Prop),birth(set_vm(Prop))|Or],IndvPoints,_Obj),
      %addObjects(VM,Obj),
      print_grid(VM.h,VM.v,Prop,IndvPoints))),!.


set_vm_obj1(Prop,Or,Value):- is_object(Value),!,
  get_vm(VM),
  remObjects(VM,Value),
  override_object([iz(Prop),birth(set_vm(Prop))|Or],Value,NewObj),
  addObjects(VM,NewObj),
  object_grid(NewObj,Grid),
  print_grid(Prop,Grid),!.



get_vm(Key,Value):-  get_vm(VM), get_kov(Key,VM,Value).
  
get_kov(K,O,V):- get_kov1(K,O,V),!.
get_kov(K,O,V):- get_kov1(props,O,VV),!,get_kov1(K,VV,V).
% (get_kov(Prop,VM,Value) -> true ; (get_kov(props,VM,Hashmap),nonvar(Hashmap),must_not_error(nb_get_value(Hashmap,Prop,ValueOOV)),get_oov_value(ValueOOV,Value))).
get_kov1(K,O,V):- is_dict(O),!,get_dict(K,O,OOV),get_oov_value(OOV,V).

get_kov1(K,O,V):- nonvar(K),is_rbtree(O),!,rb_lookup(K,V,O).
get_kov1(K,O,V):- is_rbtree(O),!,rb_in(K,V,OOV),get_oov_value(OOV,V).
%get_kov(K,O,V):- is_rbtree(O),!,nb_rb_get_node(K,O,Node),nb_rb_node_value(Node,V).

get_oov_value(ValueOOV,Value):- compound(ValueOOV),ValueOOV=oov(Value),!.
get_oov_value(Value,Value).



make_training(TestID,VMO):- 
 %make_fti(_GH,_GV,TestID,_Grid,_Sofar,_Reserved,_Options,_Points,ArgVM),
 must_det_ll((
    WAZ = _{
      %program:[],
      %pairs:_, %datatree:_, 
      %current:_,
      test_id:TestID},
    make_training_hints(TestID,WAZ,VMO))).
    /*
     test:ID,mappings:_,
     pre_in:_, pre_out:_,
     inC:_InC,outC:_OutC,
     removed:_,added:_, kept:_,   
     grid_in:_,grid_target:_,
   set(VM.mappings) =[map])), !. % pp(VM),nl.
  */


  

%show_arc_pair_progress(TestID,ExampleNum,In,Out):- show_arc_pair_progress_sol(TestID,ExampleNum,In,Out),!.
train_test:- notrace(get_current_test(TestID)), once(train_test(TestID)).
train_test(TestID):- 
  clear_training(TestID),
  compile_and_save_test(TestID),
  train_test(TestID,train_using_oo_ii_io).
train_test(TestID,P2):-   
  print_testinfo(TestID),
  flag(indiv,_,0),
  %get_training(PrevPairEnv),
  %luser_setval(prev_pairEnv,PrevPairEnv),
  %nb_delete('$training_vm'),
  %get_training(Training),
  %my_time(make_training_hints(TestID,Training,_HIDE_Dictation)),
  %Dictation = Training,
  %set_training(Dictation),
  rb_new(Dictation),
  my_time(call(P2,TestID,Dictation,DictOut)),
  set_training(DictOut),!.


train_using_oo_ii_io(TestID,DictIn,DictOut):- 
  train_using_oo_ii_io(TestID,trn,0,DictIn,DictOut).

train_using_oo_ii_io(TestID,Trn,N1,DictIn,DictOut):-
 (kaggle_arc(TestID,(Trn+N1),In1,Out1), N2 is N1 + 1),

 (kaggle_arc(TestID,(Trn+N2),_In2,Out2)
   -> 
    (with_other_grid(Out2,train_for_objects_from_pair_with_mono(DictIn,TestID,[Trn,'o',N1,'o',N2],Out1,Out2,Dict1)),
     %nop((train_for_objects_from_pair_with_mono(Dict0,TestID,[Trn,'i',N1,'i',N2],In1,In2,Dict1))),
     train_using_oo_ii_io(TestID,Trn,N2,Dict1,DictM))
     ; (DictM = DictIn)),
  !,
  with_other_grid(Out1,train_for_objects_from_pair_with_mono(DictM,TestID,[Trn,'i',N1,'o',N1],In1,Out1,DictOut)),!.

train_using_oo_ii_io(_TestID,_Trn,_N1,DictInOut,DictInOut).

train_only_from_pairs:- notrace(get_current_test(TestID)), train_only_from_pairs(TestID).

train_only_from_pairs(TestID):- clear_training(TestID), train_test(TestID,train_using_io).

train_using_io(TestID,DictIn,DictOut):- train_using_io(TestID,trn,0,DictIn,DictOut).
train_using_io(TestID,Trn,N1,DictIn,DictOut):- 
  kaggle_arc(TestID,(Trn+N1),In,Out),!,
  detect_pair_hints(TestID,(Trn+N1),In,Out),
  with_other_grid(Out,train_for_objects_from_1pair(DictIn,TestID,[Trn,'i',N1,'o',N1],In,Out,DictMid)),
  N2 is N1 + 1,
  train_using_io(TestID,Trn,N2,DictMid,DictOut).
train_using_io(_TestID,_Trn,_,DictInOut,DictInOut).

%:- thread_local(keep_going/0).

which_io0(i,in). which_io0(o,out).
which_io(I,In):- which_io0(I,In),!.
which_io(In,In):- which_io0(_,In),!.


train_for_objects_from_pair_with_mono(Dict0,TestID,Desc,In,Out,Dict9):- 
 must_det_ll((
  into_monochrome(In,MonoIn0), into_monochrome(Out,MonoOut0),
  ensure_other_grid(MonoIn0,MonoOut0),
  copy_term(MonoIn0,MonoIn),copy_term(MonoOut0,MonoOut),
 Desc = [_Trn,IsIO1,N1,IsIO2,N2], 
 MonoDesc = ['train_mono',IsIO1,N1,IsIO2,N2], 
  with_other_grid(Out,train_for_objects_from_1pair(Dict0,TestID,Desc,In,Out,Dict1)),!,
  nop(train_for_objects_from_1pair(Dict1,TestID,MonoDesc,MonoIn,MonoOut,Dict9)),!,
   ignore(Dict1=Dict9))),!.

train_for_objects_from_1pair(Dict0,TestID,Desc,InA,OutA,Dict1):-
  locally(set_prolog_flag(gc,true),
    train_for_objects_from_1pair1(Dict0,TestID,Desc,InA,OutA,Dict1)).

train_for_objects_from_1pair1(Dict0,_TestID,Desc,_InA,_OutA,Dict0):- Desc = [_Trn,'o',_N1,'o',_N2], !.

train_for_objects_from_1pair1(Dict0,TestID,Desc,InA,OutA,Dict1):-
 collapsible_section(debug,train_for_objects_from_1pair1,true,
(maplist(must_det_ll,[
 Desc = [Trn,IsIO1,N1,IsIO2,N2], 
 which_io(IsIO1,IO1),
 which_io(IsIO2,IO2),
 atomic_list_concat([IO1,IO2],'_',ModeIn),
 atomic_list_concat([IO2,IO1],'_',ModeOut),
 atom_concat(IO1,N1,ION1),
 atom_concat(IO2,N2,ION2),
 atomic_list_concat([ION1,ION2],'_',ExampleNum),
 pp([train_for_objects_from_1pair1=ExampleNum,left=ION1,right=ION2]),
 garbage_collect,
  Dict0=Dict1,
   format('~N dict= '), pp(Dict0),

   %get_map_pairs(Dict0,_Type,Pairs),
   %list_to_rbtree_safe(Pairs,InVM),
   into_grid(InA,In), into_grid(OutA,Out),!,
   name_the_pair(TestID,ExampleNum,In,Out,PairName),
 	 grid_size(In,IH,IV), grid_size(Out,OH,OV),
	 ignore((IH+IV \== OH+OV , writeln(io(size2D(IH,IV)->size2D(OH,OV))))),
   
   into_fti(TestID>(Trn+N1)*IO1,ModeIn,In,InVM),!,
   into_fti(TestID>(Trn+N2)*IO2,ModeOut,Out,OutVM)]),!,

   %InVM.compare=OutVM, 
   set(InVM.grid_target)=Out,
   %OutVM.compare=InVM, 
   set(OutVM.grid_target)=In,
  maplist(must_det_ll,[
   show_pair_grid(yellow,IH,IV,OH,OV,original(InVM.id),original(OutVM.id),PairName,In,Out),!,  
  individuate_c(InVM),!,
  individuate_c(OutVM)]),!,

  InC = InVM.objs,
  OutC = OutVM.objs,
  %print_info(InC),
  %print_info(OutC),
  %wdmsg(InC=OutC),
  maplist(must_det_ll,[
  pred_intersection(overlap_same_obj,InC,OutC,RetainedIn,RetainedOut,Removed,Added),
  /*add_shape_lib(pair,RetainedIn),
  % add_shape_lib(pair,RetainedOut),
  add_shape_lib(removed(PairName),Removed),
  add_shape_lib(added(PairName),Added),*/
  
  dash_chars,dash_chars,dash_chars,dash_chars,
  show_pair_grid(cyan,IH,IV,OH,OV,original(InVM.id),original(OutVM.id),PairName,In,Out),!,
  max_min(IH,OH,IOH,_), max_min(IV,OV,IOV,_),
  luser_setval(no_rdot,true),
  ((Removed==Added, Removed==[]) -> pp(yellow,nothing_removed_added(PairName)) ;
    show_pair_diff_code(IOH,IOV,IOH,IOV,removed(PairName),added(PairName),PairName,Removed,Added)),
  ((RetainedIn==RetainedOut, RetainedIn==[]) -> pp(yellow,nothing_retained(PairName)) ;
    show_pair_diff_code(IH,IV,   OH, OV,retained(ION1),retained(ION2),PairName,RetainedIn,RetainedOut)),
  ((InC==OutC, InC==[]) -> pp(yellow,nothing_individuated(PairName)) ;
    show_pair_diff_code(IH,IV,   OH, OV,individuated1(ION1),individuated1(ION2),PairName,InC,OutC)),!, 
  luser_setval(no_rdot,false),
   % pp(OutC=InC),

   ignore(( learn_rule_o(ModeIn,InVM,OutVM))),

   ignore(( ModeIn == in_out, Trn == trn,  
            train_io_from_hint(TestID,Trn+N1,InVM))),

  dash_chars,dash_chars,dash_chars,dash_chars,
  print_testinfo(TestID)]))).

show_pair_diff_code(IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out):-
  show_pair_diff(IH,IV,OH,OV,NameIn,NameOut,PairName,In,Out),
  dash_chars,dash_chars,
  nop(show_pair_code(In,Out)),!.

show_pair_code(In,Out):- 
  pp(purple,show_objs_as_code),
  dash_chars,
  show_objs_as_code(In),
  dash_chars,
  show_objs_as_code(Out),
  dash_chars,dash_chars.

print_testinfo(TestID):-
  forall(test_info_recache(TestID,F),pp(fav(TestID,F))).
  %forall(test_info(TestID,F),forall(member(I,F),pp(test_info=I))).

% trials(learn). trials(clue).   
trials(human). trials(sol).
trials(dsl). trials(runDSL).
trial_non_human(sol).

sols_for(TestID,Trial,TrialSol):- trials(Trial),once((compound_name_arguments(Entry,Trial,[Sol]), test_info(TestID,Sols),member(Entry,Sols))),
  append_trial(Trial,Sol,TrialSol).

append_trial(Trial,Sol,TrialSol):- listify(Sol,SolL),
  ((appended_trial(Trial,TrialAppend), \+ append(_,TrialAppend,SolL)) -> append(SolL,TrialAppend,TrialSol) ;
    TrialSol = SolL).

appended_trial(human,[learn_rule]).



solve_test:- forall(trial_non_human(Trial),solve_test_trial(Trial)).

solve_test_trial(Trial):- mmake, with_test_pairs(TestID,ExampleNum,I,O,solve_test_trial_pair(Trial,TestID,ExampleNum,I,O)).

solve_test_trial_pair(Trial,TestID,ExampleNum,_I,_O):- 
 my_time((my_menu_call((catch(solve_test_trial(Trial,TestID,ExampleNum),E,
   wdmsg(E=solve_test_trial(Trial,TestID,(ExampleNum)))))))),!.

solve_test_training_too:- 
 solve_test,
 my_menu_call((get_current_test(TestID), catch(solve_test_trial(Trial,TestID,(trn+A)),E,wdmsg(E=solve_test_trial(Trial,TestID,(trn+A)))))),!.


solve_test(Name):- forall(trial_non_human(Trial),solve_test_trial(Trial,Name)).

solve_test_trial(Trial,Name):- 
  fix_test_name(Name,TestID,ExampleNum),!, 
  solve_test_trial(Trial,TestID,ExampleNum).

solve_test(TestID,ExampleNum):-
  forall(trial_non_human(Trial),solve_test_trial(Trial,TestID,ExampleNum)).

solve_test_trial(Trial,TestID,ExampleNum):-
 forall(kaggle_arc(TestID,ExampleNum,TestIn,ExpectedOut),
   ignore(solve_test_trial(Trial,TestID,ExampleNum,TestIn,ExpectedOut))).

solve_test(TestID,ExampleNum,TestIn,ExpectedOut):-
  forall(trial_non_human(Trial),solve_test_trial(Trial,TestID,ExampleNum,TestIn,ExpectedOut)).

  
solve_test_trial(Trial,TestID,ExampleNum,TestIn,ExpectedOut):-
   must_det_ll((    
    name_the_pair(TestID,ExampleNum,TestIn,ExpectedOut,PairName))),
   must_det_ll((       
    grid_size(TestIn,IH,IV), grid_size(ExpectedOut,OH,OV),
    ignore((IH+IV \== OH+OV , writeln(io(size2D(IH,IV)->size2D(OH,OV))))),
    print_testinfo(TestID))), 
   must_det_ll((
   try_easy_io(TestID>ExampleNum,TestIn,ExpectedOut),
    dash_chars, dash_chars,
    show_pair_grid(green,IH,IV,OH,OV,'Test TestIn','Solution ExpectedOut (Not computed by us)',PairName,TestIn,ExpectedOut),!,  
    get_training(Training))),
    flag(indiv,_,0),    
    into_fti(TestID>ExampleNum*in,in,TestIn,InVM),!,
    set(InVM.objs) = [],
    %set(InVM.points) = [],
    %set(InVM.training) = Training,
    set_training(Training),
    maybe_set_vm(InVM),    
    gset(InVM.grid_target) = _,
    must_det_ll((
    %print(training(Training)),nl,
    %ppt(InVM),
    dash_chars, dash_chars,    
    %print_testinfo(TestID),
    do_sols_for(Trial,"Taking Test",InVM,TestID,ExampleNum))).

    % find indiviuation one each side that creates the equal number of changes

set_target_grid(ExpectedOut):-
    luser_setval(other_grid,ExpectedOut),
    grid_size(ExpectedOut,GOH,GOV),
    luser_setval(other_grid_size,size2D(GOH,GOV)).


do_sols_for(Trial,Why,InVM,TestID,ExampleNum) :-
 must_det_ll(( ppt("BEGIN!!!"+Why+TestID>ExampleNum), 
    kaggle_arc_io(TestID,ExampleNum,out,ExpectedOut),
    set_target_grid(ExpectedOut),
    forall(sols_for(TestID,Trial,SolutionProgram),
     ignore(((
      once((pp(cyan,trial=Trial),
       ppt(cyan,run_dsl(TestID>ExampleNum,Trial,SolutionProgram)),!,
       (my_time((
              maybe_set_vm(InVM),
              kaggle_arc_io(TestID,ExampleNum,in,TestIn),
              gset(InVM.grid) = TestIn,
              maybe_set_vm(InVM),
              run_dsl(InVM,SolutionProgram,InVM,GridOut)))*->!;GridOut=InVM.grid),
       into_pipe(GridOut,Solution)))
       *->    
      ignore((count_difs(ExpectedOut,Solution,Errors),
       print_side_by_side(blue,Solution,"Our Ran Solution",_,ExpectedOut,"Expected Solution"),
          (Errors==0 -> 
             arcdbg_info(green,pass(Why,TestID,ExampleNum,SolutionProgram))
             ; (banner_lines(red), arcdbg(fail(Why,Errors,TestID,ExampleNum,SolutionProgram)),
                test_info(TestID,InfoF),wqnl(fav(TestID>ExampleNum,InfoF)),
                banner_lines(red)))))

     

       ;arcdbg(warn(unrunable(TestID,ExampleNum,SolutionProgram))))))),
    print_side_by_side("our grid", InVM.grid, InVM.objs),!,
    print_list_of("our objs",InVM.objs),
    ppt("END!!!"+Why+TestID+ExampleNum))),!.


:- luser_linkval(test_rules,[rules]).
:- luser_linkval(pair_rules,[rules]).
  

reuse_indivs(IndvA,IndvB,BetterA,BetterB):-
  smallest_first(IndvA,IndvAS),
  smallest_first(IndvB,IndvBS),
  my_append(IndvAS,IndvBS,IndvCC), list_to_set(IndvCC,IndvC),
  smallest_first(IndvC,IndvCS),
  reuse_indivs_cleanup(IndvAS,IndvBS,IndvCS,BetterA,BetterB,_BetterC),!.

reuse_indivs_cleanup(IndvA,IndvB,IndvC,_,_,_):-
  maplist(length,[IndvA,IndvB,IndvC],Rest),
  wdmsg(len=Rest),fail.
reuse_indivs_cleanup(IndvA,IndvB,IndvC,BetterAO,BetterBO,BetterCO):-
  select(A,IndvC,IndvCRest), member(B,IndvCRest),
  select(A,IndvA,IndvARest),
  select(A,IndvB,IndvBRest),
  reuse_a_b(A,B,AA),
  my_append(IndvARest,[AA],BetterA),
  my_append(IndvBRest,[B],BetterB),
  my_append(IndvCRest,[AA],BetterC),
  reuse_indivs_cleanup(BetterA,BetterB,BetterC,BetterAO,BetterBO,BetterCO),!.
reuse_indivs_cleanup(A,B,C,A,B,C).

%same_object(D)
reuse_a_b(A,B,AA):-
  findall(H,compare_objs1(H,A,B),How),
  obj_to_oid(B,BOID),
  obj_to_oid(A,_AOID),
  setq(A,oid(BOID),AA),
  object_glyph(A,GlyphA),
  object_glyph(B,GlyphB),
  ignore((How ==[]-> nop(pp(shared_object(GlyphB->GlyphA))); 
    (pp(same_object(GlyphA,GlyphB,How))))).

test_regressions:- make, forall((clause(mregression_test,Body),ppt(Body)),must_det_ll(Body)).
:- arc_history1(test_regressions).

:- dynamic(muarc_2_mods/2).
:- strip_module(_,M,_), prolog_load_context(module,MM), retractall(muarc_2_mods(_,_)), asserta(muarc_2_mods(M,MM)).

%:- forall(ping_indiv_grid(X),atom_concat(X,Y
:- fixup_exports.
%:- initialization(demo,program).
%:- initialization(demo,restore_state).
%:- initialization(demo,main).
%:- initialization(demo,after_load).
:- muarc_mod(M), arc_history1((module(M))).

%:- muarc_mod(M), M:show_tests.
:- load_last_test_name.

%:- muarc_mod(M), M:listing((addOptions)/2).
%:- xlisting((.)/3).
%:- xlisting(user:'.'(_, _, _)).

:- ensure_loaded(kaggle_arc_simple).

:- dynamic(saved_training/1).
saved_training(TestID):- call_u('~'(saved_training(TestID))), !, fail. % explictly always assume unsaved?
saved_training(TestID):- test_name_output_file(TestID,File),exists_file(File).



:- set_prolog_flag(arc_term_expansion, true).

%:- ensure_loaded('kaggle_arc_fwd.pfc').

%:- set_prolog_flag(arc_term_expansion, false).

%:- if(prolog_load_context(reload,false)).
%:- fixup_module_exports_into_from(system,muarc).
%:- endif.

%:- fixup_module_exports_now.  
user:portray(Grid):- fail, 
   current_prolog_flag(debug,false),
    \+ tracing,
   \+ nb_current(arc_can_portray,nil),
   current_predicate(bfly_startup/0), \+ \+ catch(quietly(arc_portray(Grid)),_,fail),!, flush_output.


%:- ignore(check_dot_spacing).

   
:- add_history((print_test)).
:- add_history((webui_tests)).
:- add_history((bfly_test(a1))).
:- add_history((bfly_tests)).
:- add_history((test_pp)).
:- add_history((bfly_startup)).
%:- add_history1((cls_z,make,demo)).
:- add_history1((demo)).


:- nb_setval(arc_can_portray,nil).
:- nb_setval(arc_can_portray,t).
:- nb_setval(arc_can_expand_query,nil).
:- nb_setval(arc_can_expand_query,t).
%:- \+ nb_current(arc_can_portray,nil).

:- fixup_module_exports_into(baseKB).
:- fixup_module_exports_into(system).

:- catch_log(set_stream(current_output,encoding(utf8))).

:- (current_prolog_flag(load_arc_webui,true)->catch_log(logicmoo_webui) ; true).
:- current_prolog_flag(load_arc_webui,true) -> catch_log(start_arc_server) ; true.

:- catch_log(set_long_message_server('https://logicmoo.org:17771')).

bfly_startup:-
   set_toplevel_pp(bfly),
   asserta(was_inline_to_bfly),inline_to_bfly_html,
   bfly,
   catch_log(webui_tests),
   catch_log(print_test),
   catch_log(menu),
   %with_pp(bfly,catch_log(menu)),
   nop((next_test,previous_test)),!,
   ansi.


ansi_startup:- 
   ansi,
   catch_log(webui_tests),
   catch_log(print_test),
   catch_log(menu),
   %with_pp(bfly,catch_log(menu)),
   nop((next_test,previous_test)),!.

:- luser_setval(cmd,test_easy_solve_by).
:- luser_setval(individuated_cache,false).
:- luser_default(extreme_caching,false).
:- gen_gids.

