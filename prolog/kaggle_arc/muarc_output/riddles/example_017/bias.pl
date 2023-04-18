
%:-style_check(-discontiguous).

%body_pred(zendo,1).
%head_pred(rhs,1).

head_pred(zendo,1).

body_pred(rhs,1).
body_pred(piece,2).
body_pred(child,2).
body_pred(cenGX,2).
body_pred(cenGY,2).
body_pred(size,2).
body_pred(color,2).

body_pred(lhs,1).
body_pred(rhs,1).

body_pred(medium,1).
body_pred(large,1).
body_pred(upright,1).
body_pred(strange,1).


type(zendo,(state,)).
type(piece,(state,piece)).
type(child,(piece,piece)).
type(cenGX,(piece,real)).
type(cenGY,(piece,real)).
type(size,(piece,real)).
type(small,(real,)).
type(medium,(real,)).
type(large,(real,)).
type(upright,(piece,)).
type(lhs,(piece,)).
type(rhs,(piece,)).
type(strange,(piece,)).

direction(zendo,(in,)).
direction(piece,(in,out)).
direction(color,(in,out)).
direction(child,(in,out)).
direction(cenGX,(in,out)).
direction(cenGY,(in,out)).
direction(size,(in,out)).
direction(small,(in,)).
direction(medium,(in,)).
direction(large,(in,)).
direction(upright,(in,)).
direction(lhs,(in,)).
direction(rhs,(in,)).
direction(strange,(in,)).