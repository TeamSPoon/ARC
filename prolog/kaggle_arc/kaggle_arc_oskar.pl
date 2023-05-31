% Background knowledge
background_knowledge(color/1).
background_knowledge(shape/1).
background_knowledge(size/1).
background_knowledge(texture/1).
background_knowledge(location/2).
background_knowledge(scale/1).
background_knowledge(hollow/1).
background_knowledge(subobject/1).

% Positive examples
positive_example(p1, [color:red, shape:circle, size:small, texture:smooth, location:(2,3), scale:1.0, hollow:no, subobject(sub1: [color:red, shape:circle, size:small])]).
positive_example(p2, [color:green, shape:circle, size:small, texture:rough, location:(-1,5), scale:1.2, hollow:no, subobject(sub2: [color:green, shape:circle, size:large])]).
positive_example(p3, [color:red, shape:square, size:small, texture:rough, location:(3,2), scale:0.8, hollow:yes, subobject(sub3: [color:blue, shape:square, size:large])]).
positive_example(p4, [color:blue, shape:circle, size:small, texture:smooth, location:(0,0), scale:1.5, hollow:yes, subobject(sub4: [color:red, shape:square, size:small])]).
positive_example(p5, [color:blue, shape:square, size:large, texture:rough, location:(-2,-4), scale:2.0, hollow:yes, subobject(sub5: [color:green, shape:circle, size:small])]).
positive_example(p6, [color:green, shape:circle, size:large, texture:smooth, location:(-3,1), scale:1.0, hollow:no, subobject(sub6: [color:blue, shape:square, size:large])]).
positive_example(p7, [color:red, shape:square, size:large, texture:smooth, location:(1,-2), scale:1.2, hollow:no, subobject(sub7: [color:red, shape:circle, size:large])]).

% Negative examples
negative_example(n1, [color:green, shape:circle, size:small, texture:smooth, location:(-2,-3), scale:0.9, hollow:no, subobject(sub8: [color:green, shape:square, size:large])]).
negative_example(n2, [color:red, shape:circle, size:small, texture:rough, location:(4,1), scale:1.1, hollow:yes, subobject(sub9: [color:red, shape:square, size:small])]).
negative_example(n3, [color:blue, shape:square, size:small, texture:smooth, location:(-1,4), scale:1.3, hollow:yes, subobject(sub10: [color:green, shape:circle, size:large])]).
negative_example(n4, [color:red, shape:circle, size:large, texture:rough, location:(0,0), scale:1.5, hollow:yes, subobject(sub11: [color:blue, shape:square, size:small])]).
negative_example(n5, [color:green, shape:square, size:small, texture:smooth, location:(-2,-4), scale:2.0, hollow:yes, subobject(sub12: [color:red, shape:circle, size:large])]).

% Find minimal explanatory background
find_meb(Positive, Negative, MEBClauses) :-
  findall(Clause, (
    positive_example(_, PosExample),
    minimal_explanation(PosExample, Positive, Negative, Clause)
  ), MEBClauses).

% Check if a clause is a minimal explanation
minimal_explanation(Example, Positive, Negative, Clause) :-
  background_knowledge(Clause),
  \+ member(Clause, Positive),
  \+ (member(ExampleNeg, Negative), rule_covered(Clause, ExampleNeg)),
  explain(Example, Positive, Negative, [Clause]).

% Explain positive example using clauses
explain(_, [], _, _).
explain(Example, [Positive|RestPos], Negative, Clauses) :-
  member(Positive, Clauses),
  \+ (member(ExampleNeg, Negative), rule_covered(Positive, ExampleNeg)),
  explain(Example, RestPos, Negative, Clauses).

% ?- find_meb([p1, p2, p3, p4, p5, p6, p7], [n1, n2, n3, n4, n5], MEBClauses).



end_of_file.







% Background knowledge
background_knowledge(color/1).
background_knowledge(shape/1).
background_knowledge(size/1).
background_knowledge(texture/1).
background_knowledge(location/2).
background_knowledge(scale/1).
background_knowledge(hollow/1).
background_knowledge(subobject/1).

% Positive examples
positive_example(p1, [color:red, shape:circle, size:small, texture:smooth, location:(2,3), scale:1.0, hollow:no, subobject(sub1: [color:red, shape:circle, size:small])]).
positive_example(p2, [color:green, shape:circle, size:small, texture:rough, location:(-1,5), scale:1.2, hollow:no, subobject(sub2: [color:green, shape:circle, size:large])]).
positive_example(p3, [color:red, shape:square, size:small, texture:rough, location:(3,2), scale:0.8, hollow:yes, subobject(sub3: [color:blue, shape:square, size:large])]).
positive_example(p4, [color:blue, shape:circle, size:small, texture:smooth, location:(0,0), scale:1.5, hollow:yes, subobject(sub4: [color:red, shape:square, size:small])]).
positive_example(p5, [color:blue, shape:square, size:large, texture:rough, location:(-2,-4), scale:2.0, hollow:yes, subobject(sub5: [color:green, shape:circle, size:small])]).
positive_example(p6, [color:green, shape:circle, size:large, texture:smooth, location:(-3,1), scale:1.0, hollow:no, subobject(sub6: [color:blue, shape:square, size:large])]).
positive_example(p7, [color:red, shape:square, size:large, texture:smooth, location:(1,-2), scale:1.2, hollow:no, subobject(sub7: [color:red, shape:circle, size:large])]).

% Negative examples
negative_example(n1, [color:green, shape:circle, size:small, texture:smooth, location:(-2,-3), scale:0.9, hollow:no, subobject(sub8: [color:green, shape:square, size:large])]).
negative_example(n2, [color:red, shape:circle, size:small, texture:rough, location:(4,1), scale:1.1, hollow:yes, subobject(sub9: [color:red, shape:square, size:small])]).
negative_example(n3, [color:blue, shape:square, size:small, texture:smooth, location:(-1,4), scale:1.3, hollow:yes, subobject(sub10: [color:green, shape:circle, size:large])]).
negative_example(n4, [color:red, shape:circle, size:large, texture:rough, location:(0,0), scale:1.5, hollow:yes, subobject(sub11: [color:blue, shape:square, size:small])]).
negative_example(n5, [color:green, shape:square, size:small, texture:smooth, location:(-2,-4), scale:2.0, hollow:yes, subobject(sub12: [color:red, shape:circle, size:large])]).

% Find minimal explanatory background
find_meb(Positive, Negative, MEBClauses) :-
  findall(Clause, (
    positive_example(_, PosExample),
    generalize_example(PosExample, Positive, Negative, Clause)
  ), MEBClauses).

% Generalize an example by replacing specific values with minimal generalizations
generalize_example(Example, Positive, Negative, GeneralizedExample) :-
  findall(GenExample, (
    member(Pos, Positive),
    generalize_single_example(Example, Pos, GenExample),
    \+ (member(Neg, Negative), rule_covered(GenExample, Neg))
  ), GeneralizedExamples),
  minimal_example_set(GeneralizedExamples, GeneralizedExample).

% Generalize a single example by replacing specific values with minimal generalizations
generalize_single_example([], [], []).
generalize_single_example([Key:Value|T1], [Key:Value|T2], [Key:Value|T3]) :-
  ground(Value),
  generalize_single_example(T1, T2, T3).
generalize_single_example([Key:Value|T1], [_:Value|T2], [Key:Variable|T3]) :-
  \+ ground(Value),
  Variable = _,
  generalize_single_example(T1, T2, T3).

% Check if a rule covers an example
rule_covered(Rule, Example) :-
  subset(Rule, Example).

% Subset relation for lists
subset([], _).
subset([H|T], List) :-
  member(H, List),
  subset(T, List).

% Find the minimal set of examples for generalization
minimal_example_set(Examples, MinimalSet) :-
  length(MinimalSet, _),
  subset(MinimalSet, Examples),
  \+ (subset(OtherSet, Examples), subset(MinimalSet, OtherSet)).
























% Positive examples
positive_example(p1, [color:red, shape:circle, size:small, texture:smooth, location:(2,3), scale:1.0, hollow:no]).
positive_example(p2, [color:green, shape:circle, size:small, texture:rough, location:(-1,5), scale:1.2, hollow:no]).
positive_example(p3, [color:red, shape:square, size:small, texture:rough, location:(3,2), scale:0.8, hollow:yes]).
positive_example(p4, [color:blue, shape:circle, size:small, texture:smooth, location:(0,0), scale:1.5, hollow:yes]).
positive_example(p5, [color:blue, shape:square, size:large, texture:rough, location:(-2,-4), scale:2.0, hollow:yes]).
positive_example(p6, [color:green, shape:circle, size:large, texture:smooth, location:(-3,1), scale:1.0, hollow:no]).
positive_example(p7, [color:red, shape:square, size:large, texture:smooth, location:(1,-2), scale:1.2, hollow:no]).

% Negative examples
negative_example(n1, [color:green, shape:circle, size:small, texture:smooth, location:(-2,-3), scale:0.9, hollow:no]).
negative_example(n2, [color:red, shape:circle, size:small, texture:rough, location:(4,1), scale:1.1, hollow:yes]).
negative_example(n3, [color:blue, shape:square, size:small, texture:smooth, location:(-1,4), scale:1.3, hollow:yes]).
negative_example(n4, [color:red, shape:circle, size:large, texture:rough, location:(0,0), scale:1.5, hollow:yes]).
negative_example(n5, [color:green, shape:square, size:small, texture:smooth, location:(-2,-4), scale:2.0, hollow:yes]).

% Background knowledge
attribute(color, [red, green, blue]).
attribute(shape, [circle, square]).
attribute(size, [small, large]).
attribute(texture, [smooth, rough]).
attribute(location, [(X,Y) | _]) :- between(-5, 5, X), between(-5, 5, Y).
attribute(scale, [0.8, 1.0, 1.2, 1.5, 2.0]).
attribute(hollow, [yes, no]).

% AQ Algorithm with Version Space Search and LGG
aq(Feature) :-
  positive_example(_, Positive),
  negative_example(_, Negative),
  initialize_boundaries(Positive, Negative, GeneralBoundary, SpecificBoundary),
  version_space_search(Positive, Negative, GeneralBoundary, SpecificBoundary, Feature, Hypotheses),
  write('Hypotheses: '), write(Hypotheses),
  lgg(Hypotheses, Features).

% Initialize the general and specific boundaries
initialize_boundaries(Positive, Negative, GeneralBoundary, SpecificBoundary) :-
  create_rule([], GeneralRule),
  generalize_rule(GeneralRule, GeneralBoundary),
  create_rule(Positive, SpecificRule),
  specialize_rule(SpecificRule, SpecificBoundary),
  remove_inconsistent(SpecificBoundary, Positive, Negative).

% Version Space Search
version_space_search(_, _, GeneralBoundary, [], Feature, Hypotheses) :-
  generalize_rule(GeneralBoundary, Hypotheses),
  satisfy_feature(Feature, Hypotheses).
% Version Space Search
version_space_search(_, _, GeneralBoundary, [], Feature, Hypotheses) :-
  generalize_rule(GeneralBoundary, Hypotheses),
  satisfy_feature(Feature, Hypotheses).
version_space_search(Positive, Negative, GeneralBoundary, [Specific|Rest], Feature, Hypotheses) :-
  remove_inconsistent([Specific|Rest], Positive, Negative, Pruned),
  generalize_rule(GeneralBoundary, GeneralizedBoundary),
  specialize_rule(Specific, Specialized),
  version_space_search(Positive, Negative, GeneralizedBoundary, Specialized, Feature, Hypotheses).

% Remove inconsistent hypotheses
remove_inconsistent([], _, []).
remove_inconsistent([H|T], Positive, Negative, [H|Pruned]) :-
  consistent_with_examples(H, Positive, Negative),
  remove_inconsistent(T, Positive, Negative, Pruned).
remove_inconsistent([_|T], Positive, Negative, Pruned) :-
  remove_inconsistent(T, Positive, Negative, Pruned).

% Check if a hypothesis is consistent with the examples
consistent_with_examples(Hypothesis, Positive, Negative) :-
  all_positive_covered(Hypothesis, Positive),
  no_negative_covered(Hypothesis, Negative).

% Check if all positive examples are covered by the hypothesis
all_positive_covered(_, []).
all_positive_covered(Hypothesis, [Example|Rest]) :-
  rule_covered(Hypothesis, Example),
  all_positive_covered(Hypothesis, Rest).

% Check if a rule is covered by an example
rule_covered(Rule, Example) :-
  Rule =.. [Example, Attributes],
  Example =.. [_, ExampleAttributes],
  subset(Attributes, ExampleAttributes).

% Check if a rule covers an example
%rule_covered(Rule, Example) :-
%  maplist(example_matches_rule(Example), Rule).

% Check if an example matches a rule
%example_matches_rule(Example, Rule) :-
%  member(Rule, Example).


% Check if no negative examples are covered by the hypothesis
no_negative_covered(_, []).
no_negative_covered(Hypothesis, [Example|Rest]) :-
  \+ rule_covered(Hypothesis, Example),
  no_negative_covered(Hypothesis, Rest).

% Create a rule from a list of attributes
create_rule(Attributes, Rule) :-
  Rule =.. [rule, Attributes].

% Generalize a rule using LGG
generalize_rule(Rule, GeneralizedRule) :-
  Rule =.. [_, Attributes],
  lgg(Attributes, GeneralizedAttributes),
  GeneralizedRule =.. [rule, GeneralizedAttributes].

% Specialize a rule by adding possible attributes
specialize_rule(Rule, SpecializedRule) :-
  Rule =.. [_, Attributes],
  findall(Attribute, possible_attribute(Attribute), PossibleAttributes),
  append(Attributes, PossibleAttributes, NewAttributes),
  SpecializedRule =.. [rule, NewAttributes].

% Possible attributes for specialization
possible_attribute(color:red).
possible_attribute(color:green).
possible_attribute(color:blue).
possible_attribute(shape:circle).
possible_attribute(shape:square).
possible_attribute(size:small).
possible_attribute(size:large).
possible_attribute(texture:smooth).
possible_attribute(texture:rough).
possible_attribute(location:(X,Y)) :- between(-5, 5, X), between(-5, 5, Y).
possible_attribute(scale:0.8).
possible_attribute(scale:1.0).
possible_attribute(scale:1.2).
possible_attribute(scale:1.5).
possible_attribute(scale:2.0).
possible_attribute(hollow:yes).
possible_attribute(hollow:no).

% LGG Algorithm
lgg([], []).
lgg([List1|Rest], [List2|Result]) :-
  lgg_lists(List1, List2, LGGList),
  lgg(Rest, Result),
  Result = [LGGList|_].

% LGG of two lists
lgg_lists([], [], []).
lgg_lists([H1|T1], [H2|T2], [H|LGGT]) :-
  (H1 = H2 -> H = H1 ; H = feature(_)),
  lgg_lists(T1, T2, LGGT).


% Check if a feature is satisfied by a hypothesis
satisfy_feature(Feature, Hypothesis) :-
  Hypothesis =.. [_, Attributes],
  member(Feature, Attributes).


% Background knowledge
background_knowledge(color/2).
background_knowledge(shape/2).
background_knowledge(size/2).
background_knowledge(texture/2).
background_knowledge(location/3).
background_knowledge(scale/2).
background_knowledge(hollow/2).

% Find minimal explanatory background
find_meb(Positive, MEBClauses) :-
  findall(Clause, (
    positive_example(_, Example),
    minimal_explanation(Example, Positive, Clause)
  ), MEBClauses).

% Check if a clause is a minimal explanation
minimal_explanation(Example, Positive, Clause) :-
  background_knowledge(Clause),
  \+ member(Clause, Positive),
  explain(Example, Positive, [Clause]).

% Explain positive example using clauses
explain(_, [], _).
explain(Example, [Positive|Rest], Clauses) :-
  member(Positive, Clauses),
  explain(Example, Rest, Clauses).

% Example usage
/* ?- find_meb([p1, p2, p3, p4, p5, p6, p7], MEBClauses).*/





end_of_file.

call_w_time_limit(Time, Goal) :-
    call_w_time_limit(Time, Goal, '$no_ctx').

    %call_w_time_limit(6, current_alarm(Time, Goal, Id, Status)).

call_w_time_limit(Time, Goal, Ctx) :-
  %current_alarm(Time, Goal, Id, Status)->uninstall_alarm(Id)
  setup_call_cleanup(alarm(Time, maybe_yield(Time,Id,Goal,overtime), Id, [install(false),remove(false)]),
        run_alarm_goal(Time, Id, Goal), alarm_unneeded(Id)).

alarm_unneeded(Id):- current_alarm(Time, Goal, Id, Status), uninstall_alarm(Id).
alarm_unneeded(_).
maybe_yield(Time,Id,Goal,overtime):- engine_self(_),engine_yield(overtime(Time,Id,Goal)).
maybe_yield(_,_,_,_).

run_alarm_goal(Time, Id, Goal) :- install_alarm( Id, Time), Goal,!,alarm_unneeded(Id).






%change_of_position
