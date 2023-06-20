%./Popper/arc/magicpopper/magicpopper/popper.py Popper/arc/ilp/save-a79310a0/ --stats

%********** SOLUTION **********
%Precision:1.00 Recall:1.00 TP:3 FN:0 TN:0 FP:0 Size:4
%rhs(A,B,C,D,E,F):- lhs(A,B,G,H,E,F),color_change(H,D),incr_nat30(G,C).
%******************************
%Total programs: 208
%Total operation time: 0.24s
%Total execution time: 27.89s

max_vars(8).
max_magic(4).
% bias file contents ---------------------------------------------------
head_pred(rhs,6). 
body_pred(lhs,6). 
body_pred(color_change,2). 
body_pred(incr_nat30,2).
type(rhs,(obj,nat30,nat30,color,nat900,shape)).
type(lhs,(obj,nat30,nat30,color,nat900,shape)).
type(color_change,(color,color)).
type(incr_nat30,(nat30,nat30)).
direction(lhs,(in,out,out,out,out,out)). 
direction(rhs,(in,in,in,in,in,in)). 
direction(color_change,(in,out)). 
direction(incr_nat30,(in,out)).
magic_type(color).
magic_type(nat30).
