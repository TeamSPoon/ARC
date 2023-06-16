
end_of_file.

run_todo_output(VM, [], OutObjs):- OutObjs = VM.objs, !.
run_todo_output(VM, [apply(Rule, Obj)|TODO], OutObjs):-
  edit_object(VM, Rule, Obj),
  run_todo_output(VM, TODO, OutObjs).

two_way_mapping(Ways, Obj, _Objs, Rules, Rule, Rules):-
  match_ok(Ways, exact), !,
  Res = res(Score, Rule),
  findall(Res, (member(Rule, Rules), score_rule(Ways, Obj, Rule, Score)), Results),
  sort(Results, ResultsSorted),
  last(ResultsSorted, Res),
  select(Rule, Rules, _RulesOut), !.
  %select(Obj, Objs, ObjsOut).

two_way_mapping(Ways, Obj, Objs, Rules, Rule, RulesOut):-
  \+ match_ok(Ways, exact),
   once((sort_by_jaccard(Obj, Rules, [Rule|RulesOut]),
   sort_by_jaccard(Rule, Objs, [PickedObj|_ObjsOut]))),
    ((PickedObj == Obj)-> nop(match_ok(Ways, two_ways)) ; match_ok(Ways, one_way)),
  write_atoms_info(Ways, PickedObj),
  write_atoms_info(paired2, Rule),
  %maplist(write_atoms_info(leftover1), RulesOut),
  %maplist(write_atoms_info(leftover2), ObjsOut),
  !.



apply_rules_to_objects(_, _, _, [], []):-!.
apply_rules_to_objects(_, _, [], _, []):-!.


apply_rules_to_objects(Ways, Mapping, Rules, Objs, [apply(Rule, Obj)|More]):-
   match_ok(Mapping, one_to_one),
   \+ match_ok(Ways, exact),

   two_way_mapping(two_way, Obj, Objs, Rules, Rule, RulesOut),

   select(Obj, Objs, ObjsOut),
   apply_rules_to_objects(Ways, Mapping, RulesOut, ObjsOut, More).

apply_rules_to_objects(Ways, Mapping, Rules, Objs, [apply(Rule, Obj)|More]):-
   match_ok(Mapping, each_object_once),
   select(Obj, Objs, ObjsOut),
  two_way_mapping(Ways, Obj, Objs, Rules, Rule, _),
   apply_rules_to_objects(Ways, Mapping, Rules, ObjsOut, More).

apply_rules_to_objects(Ways, Mapping, Rules, Objs, [apply(Rule, Obj)|More]):-
   match_ok(Mapping, each_rule_once),
   select(Rule, Rules, RulesOut),
   two_way_mapping(Ways, Rule, Rules, Objs, Obj, ObjOut), !,
   apply_rules_to_objects(Ways, Mapping, RulesOut, ObjOut, More).

apply_rules_to_objects(Ways, Mapping, [_|Rules], Objs, More):-
 match_ok(Mapping, each_rule_once), !,
 apply_rules_to_objects(Ways, Mapping, Rules, Objs, More).

apply_rules_to_objects(Ways, Mapping, Rules, [_|Objs], More):-
 match_ok(Mapping, each_object_once), !,
 apply_rules_to_objects(Ways, Mapping, Rules, Objs, More).

apply_rules_to_objects(_Ways, _Mapping, _Rules, _Objs, []).

    

write_atoms_info(N, E):- obj_atoms(E, Atoms), !, %sort(Atoms, AE),
  nl, writeln(N=Atoms).

p_of_post(P, Post):- indv_props_list(Post, Props), member(P, Props).



from_same_pair(Post, Pre):-
  has_prop(giz(example_num(trn+N)), Post),
  has_prop(giz(example_num(trn+N)), Pre).



obj_in_or_out(Rule, IO_):- is_mapping(Rule), !,
    get_mapping_info(Rule, Info, _In, _Out), arg(3, Info, IO_).
obj_in_or_out(Obj, IO_):- must_det_ll(is_object(Obj)), has_prop(giz(g(I_O)), Obj), !, I_O=IO_.
obj_in_or_out(Obj, IO_):- has_prop(iz(i_o(I_O)), Obj), !, I_O=IO_.
%obj_in_or_out(Obj, I_O):- is_input_object(Obj)-> IO_ =out ; IO_ =in.

is_pre_cond_obj(Obj, in_out):- obj_in_or_out(Obj, in).
is_pre_cond_obj(Obj, in_out_out):- obj_in_or_out(Obj, in);obj_in_or_out(Obj, out).
is_pre_cond_obj(Obj, in_in_out):- obj_in_or_out(Obj, in).
is_pre_cond_obj(Obj, s(X)):- nonvar(X), is_pre_cond_obj(Obj, out).
is_pre_cond_obj(Obj, IO_):- obj_in_or_out(Obj, IO_).
is_pre_cond_obj(Obj, in):- is_pre_cond_obj(Obj, in_out).


is_post_cond_obj(Obj, in_out):- obj_in_or_out(Obj, out).
is_post_cond_obj(Obj, in_out_out):- obj_in_or_out(Obj, out).
is_post_cond_obj(Obj, in_in_out):- obj_in_or_out(Obj, out).
is_post_cond_obj(Obj, s(X)):- nonvar(X), is_post_cond_obj(Obj, out).
is_post_cond_obj(Obj, out):- obj_in_or_out(Obj, out).
is_post_cond_obj(Obj, in):- is_post_cond_obj(Obj, in_out).





