% ./Popper/arc/magicpopper/magicpopper/popper.py Popper/arc/ilp/save-a79310a0/ --stats

max_body(3).
max_vars(11).
max_magic(4).
% bias file contents ---------------------------------------------------
head_pred(rhs,8).
body_pred(lhs,8).
body_pred(move_down,2).
body_pred(color_change,2). 
% body_pred(incr_nat30,2).
% body_pred(at_top,1). body_pred(at_bottom,1). body_pred(at_left,1). 
% body_pred(at_right,1). 
% body_pred(move_left,2). body_pred(move_right,2). body_pred(move_up,2).


type(rhs,(state,point2D,rot2D,color,vis2D,rotSize2D,nat900,shape)). 
type(lhs,(state,point2D,rot2D,color,vis2D,rotSize2D,nat900,shape)). 
type(color_change,(color,color)).
% type(incr_nat30,(nat30,nat30)). type(move_left,(point2D,point2D)). type(move_right,(point2D,point2D)). type(move_up,(point2D,point2D)).
type(move_down,(point2D,point2D)).
% type(at_top,(point2D)). type(at_bottom,(point2D)). type(at_left,(point2D)). type(at_right,(point2D)).

  % direction(lhs,(in,out,out,out,out,out,out,out)). 
  % direction(rhs,(in,in,in,in,in,in,in,in)). 
  % direction(at_top,in). direction(at_bottom,in). direction(at_left,in). direction(at_right,in).
  % direction(incr_nat30,(in,out)).
  % direction(move_left,(in,out)). direction(move_right,(in,out)). direction(move_up,(in,out)).
direction(move_down,(in,out)).
direction(color_change,(in,out)). 

magic_type(point2D).
magic_type(color).
% magic_type(nat30).

magic_value_type(point2D).
magic_value_type(color). 
% magic_value_type(nat30). 

% direction(color_change,(out,out)). 
% direction(incr_nat30,(out,out)). 
% direction(my_add,(in,in,out)). 
% direction(my_geq,(in,out)). 
% direction(my_leq,(in,out)). 
% direction(my_mult,(in,out,in)). 


% bounds(my_add,1,(0,29)). 
% bounds(my_geq,1,(1,30)). 
% bounds(my_leq,1,(1,30)). 
% bounds(my_mult,1,(1,10)). 
% numerical_pred(my_add,3). 
% numerical_pred(my_geq,2). 
% numerical_pred(my_leq,2). 
% numerical_pred(my_mult,3). 

%% P(A,B):-Q(A,C),R(C,B).
% meta_clause(C):- 
%    head_literal(C,P,2,hv(0,1)), body_literal(C,Q,2,hv(0,2)), body_literal(C,R,2,hv(2,1)),
%    meta_lower(P,Q), meta_lower(P,R), body_size(C,2).
% :- clause(C), not meta_clause(C).

% meta_lower(P,Q):- lower(P,Q).
% meta_lower(P,Q):- head_aux(P,_), body_pred(Q,_), not head_aux(Q,_).

