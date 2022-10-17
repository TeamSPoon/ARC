/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- if(current_module(trill)).
:- set_prolog_flag_until_eof(trill_term_expansion,false).
:- endif.

:- use_module(library(nb_set)).
:- use_module(library(lists)).


:- decl_pt(unreduce_grid(infoR,grid)).
unreduce_grid(gridOpFn(GridR,OP),GridO):- !, unreduce_grid(GridR,OP,GridO).
unreduce_grid(GridO,GridO).

:- decl_pt(unreduce_grid(grid,list,grid)).
unreduce_grid(G,OP,GO):- maybe_into_grid_io(G,GG),!,unreduce_grid(GG,OP,GO).
unreduce_grid(G,[OP|List],GO):- !, unreduce_grid(G,OP,M),unreduce_grid(M,List,GO).
unreduce_grid(G,[],G):-!.
unreduce_grid(G,OP,GO):- call(OP,G,GO).

/*
reduce_op1_1(PassNo,Grid,blur(flipD),Left):- flipD(Grid,GridR), GridR==Grid,!,keep_flipD(Grid,Left).
*/
%reduce_op1_1(PassNo,Grid,blur(A,flipV),Left):- length(Grid,L),LS is floor(L/2),length(Left,LS),reverse(Left,Right), RS is L-LS, 
%  (RS==LS->  (append(Left,Right,Grid),A=[]) ; (append(Left,[E|Right],Grid),A=[E])).

reorder_cbody(Rev,Left,Right):- (call(Rev) -> (Left,Right); (Right,Left)).


%same_reduction(O,O):-!.

same_reduction(OPA,OPB):- var(OPA), \+ attvar(OPA),!,freeze(OPA,same_reduction(OPA,OPB)).
same_reduction([A|OPA],[B|OPB]):- !, same_reduction(A,B),same_reduction(OPA,OPB).
same_reduction(A,B):- \+ compound(A), !, freeze(B,A=B).
same_reduction(OPA,OPB):- functor(OPA,F,N),functor(OPB,F,N),arg(1,OPA,A),arg(1,OPB,B),!,same_reduction(A,B).

  /*(length(E,L),length(EB,L),arg(1,OPB,EB))),!.*/

combin_pair_op(OPA,OPB,OP):- (OPA=OPB->OP=OPA;OP=op(OPA,OPB)).

too_small_reduce(Grid,N):- grid_size(Grid,X,Y),X=<N,Y=<N,!.

/*
reduce_op2(PassNo,A+B,OPA+OPB,AA+BB):- reduce_op1(PassNo,B,OPB,BB), same_reduction(OPB,OPA),reduce_op1(PassNo,A,OPA,AA),!.
reduce_op1(PassNo,A+B,OPA+OPB,AA+BB):- reduce_op1(PassNo,A,OPA,AA), reduce_op1(PassNo,B,OPB,BB),same_reduction(OPA,OPB).
reduce_op1(PassNo,A+B,OPA+OPB,AA+BB):- reduce_op2(PassNo,A+B,OPA+OPB,AA+BB).
reduce_op1(PassNo,A+B,OPA+OPB,AA+BB):- nth1(N,A,EA,A0),maplist(=(_),EA),nth1(N,B,EB,B0),EA=@=EB,reduce_op2(PassNo,A0+B0,OPA+OPB,AA+BB).
reduce_op1(PassNo,A+B,OPA+OPB,AA+BB):- !, reduce_op2(PassNo,B+A,OPB+OPA,BB+AA).
*/

reduce_op1(PassNo,A+B,OP,AA+BB):- 
  reduce_op1_1(PassNo,A,OPA,AA),is_grid(AA),
  same_reduction(OPA,OPB),
  reduce_op1_1(PassNo,B,OPB,BB),is_grid(BB),
  combin_pair_op(OPA,OPB,OP).


%reduce_op1_1(_,Grid,call(=),Grid):- too_small_reduce(Grid,3),!.

/*reduce_op1_1(_,Grid,copy_row(N1),GridR):- number(N1),!,N2 is N1+1,
   length(Left,N2),append(Left,[RowA,RowB|Right],Grid),RowA=@=RowB,length(Left,N2),append(Left,[RowA|Right],GridR).
*/
% reduce_op1_1(_,Grid,copy_row(N1,N3),GridR):- fail, append(Left,[RowA,G,RowB|Right],Grid),RowA=@=RowB,length([_,_|Left],N1),append(Left,[RowA,G|Right],GridR),N3 is N1+2,!.
reduce_op1_1(_,Grid,copy_row(N1,N2),GridR):-  \+ too_small_reduce(Grid,2),
  append(Left,[RowA,RowB|Right],Grid),RowA=@=RowB,length([_,_|Left],N1),append(Left,[RowA|Right],GridR),N2 is N1+1,!.
reduce_op1_1(_,Grid,left_right(Left,Reduced),GridR):- fail, \+ too_small_reduce(Grid,3),
   length(Grid,L), nth1(1,Grid,A), nth1(L,Grid,B), A=@=B,
   LS is floor(L/2),
   between(1,LS,LR),LRR is LS-LR,LRR>0, length(Left,LRR),reverse(Left,Right), 
   append([Left,GridR,Right],Grid),
   reduce_grid(Left+Left,Reduced).
reduce_op1_1(_,Grid,remove_row(Row),GridR):- fail, 
  \+ too_small_reduce(Grid,3),
  nth1(Row,Grid,Same,GridR),maplist(==(black),Same),!.

%reduce_1pair_op(PassNo,Grid,RotR,GridR):- grav_rot(Grid,RotG,GridR), unrotate(RotG,RotR).

%reduce_1pair_op(PassNo,G,M,O):- maybe_into_grid_io(G,GG),!,reduce_1pair_op(PassNo,GG,M,O).
reduce_1pair_op(PassNo,Grid, Op,GridR):- reduce_op1(PassNo,Grid,Op,GridR).
%reduce_1pair_op(PassNo,GridL,as_rot(RotG,UnRotG,Op),GridRR):- grav_rot(GridL,RotG,Grid), unrotate(RotG,UnRotG), reduce_op1(PassNo,Grid,Op,GridR),call(UnRotG,GridR,GridRR).
reduce_1pair_op(PassNo,A+B,as_rot(Rot90,Rot270,Op),AA+BB):- 
  rot_pair(Rot90,Rot270),
  call(Rot90,A,AL),call(Rot90,B,BL),  
  (A\==AL;B\==BL),
  reduce_op1(PassNo,AL+BL,Op,AR+BR),
  call(Rot270,AR,AA),call(Rot270,BR,BB).

  
%reduce_1pair_op(PassNo,GridL,as_rot(rot270,rot90,Op),GridRR):- rot90(GridL,Grid),reduce_op1(PassNo,Grid,Op,GridR),rot270(GridR,GridRR).
rot_pair(rot90,rot270).
rot_pair(flipD,inverseRot(flipD)).

inverseRot(Rot,X,Y):- call(Rot,X,Y).

as_rot(L,R,Op,A+B,AA+BB):-!, as_rot(L,R,Op,A,AA),as_rot(L,R,Op,B,BB).
as_rot(L,R,Op,X,Y):- call(L,X,Grid),unreduce_grid(Grid,Op,GridR),call(R,GridR,Y).
left_right(LR,G,GOO):- into_grid_io(LR,Left), reverse(Left,Right),append([Left,G,Right],GO),mat_grid(GO,GOO).
copy_row(N1,N2,G,GOO):- nth1(N1,G,Row),N12 is N2-1,length(Left,N12), append(Left,Right,G),append(Left,[Row|Right],GO),mat_grid(GO,GOO).
mat_grid(A+B,AA+BB):-!, mat_grid(A,AA),mat_grid(B,BB).
mat_grid(GO,GOO):- mapgrid(=,GO,GOO).

into_grid_io(A+B,AA+BB):- !, into_grid_io(A,AA),!,into_grid_io(B,BB).
into_grid_io(A,B):-A=[],!,B=[].
into_grid_io(A,B):- into_grid(A,B).

maybe_into_grid_io(A,B):- into_grid_io(A,B),!,A\=@=B.

lpoints_to_norm(Width,Height,LPoints,IOps,LPointsNorm):- 
   points_to_grid(Width,Height,LPoints,LGrid), grid_to_norm(LGrid,IOps,LPointsNorm).
grid_to_norm(LGrid,IOps,LPointsNorm):- reduce_grid(LGrid+LGrid,IOps,LPointsNorm+_).


:- decl_pt(reduce_grid(grid,infoR)).
reduce_grid(G,O):- maybe_into_grid_io(G,GG),!,reduce_grid(GG,O).
reduce_grid(Grid,gridOpFn(GridR,OP)):- reduce_grid(Grid,OP,GridR),OP\==[],!.
reduce_grid(Grid,Grid).

%ungrav_rot(G,sameR,G):-!.
ungrav_rot(G,sameR,G):- too_small_reduce(G,2),!.
ungrav_rot(G,UnRotG,GG):- grav_rot(G,RotG,GG),(G==GG->UnRotG=sameR;unrotate(RotG,UnRotG)).

:- decl_pt(reduce_grid(grid,list,grid)).

%reduce_grid(A+B,ROP,AA+BB):- reduce_grid(A+A,ROP,AA+AA),reduce_grid(B+B,ROP,BB+BB),!.
reduce_grid(A+B,OP,AAO+BBO):- reduce_grid_pair(A+B,ROP,AAO+BBO),reverse(ROP,OP),!.
reduce_grid(A+B,[],A+B).

reduce_grid_pair1(A+B,[g(perfect)|ROPA],AR+BR):-
  once((reduce_grid_pass(1,A+A,[A+A],ROPA,AR+AR),
        reduce_grid_pass(1,B+B,[B+B],ROPB,BR+BR))),
  ROPA\==[],
  ROPB= ROPA,!.

reduce_grid_pair1(A+B,[g(ok)|ROPA],AR+BR):-
  once((reduce_grid_pass(1,A+A,[A+A],ROPA,AR+AR))),ROPA\==[], reduce_grid_pass(1,B+B,[B+B],ROPA,BR+BR).

reduce_grid_pair1(A+B,ROP,AAO+BBO):-
  reduce_grid_pass(1,A+B,[A+B],OP,AR+BR), 
  grav_rot(BR,UnRot,BB),
  call(UnRot,AR,AA),
  reduce_grid_pair2(ROP,OP,UnRot+UnRot,AR+BR,AA+BB,AAO+BBO).

%reduce_grid_pair2(ROP,OP,UnRotGA+UnRotGB,AR+BR,AA+BB,AAO+BBO):- 
reduce_grid_pair2(ROP,OP,UnRotGA+UnRotGB,AR+BR,AA+BB,AAO+BBO):- 
  ((AA==AR, BB==BR) -> (ROP = OP,AAO=AA,BBO=BB)
   ;( RotOP = unrotate(UnRotGA+UnRotGB),
      reduce_grid_pair(AA+BB,ROPL,AAO+BBO),
      append([OP,[RotOP],ROPL],ROP))),!.

reduce_grid_pair(A+B,ROP,AAO+BBO):-
 reduce_grid_pair1(A+B,ROP1,AR+BR),
 (AR\==A;BR\==B),
 reduce_grid_pair(AR+BR,ROP2,AAO+BBO),
 append(ROP1,ROP2,ROP).

reduce_grid_pair(A+B,[],A+B).

%reduce_grid(A+B,OPA+OPB,AA+BB):- A\=@=B,nth1(N,A,EA,A0),maplist(=(_),EA),nth1(N,B,EB,B0),EA=@=EB,reduce_grid(A0+B0,OPA+OPB,AA+BB).

reduce_grid_pass(PassNo,Grid,NBC,OP,GridR):- reduce_pair_op(PassNo,Grid,NBC,OP,GridR),!.
reduce_grid_pass(PassNo,Grid,NBC,OP,GridR):- PassNo<4,plus(PassNo,1,PassNo2), reduce_grid_pass(PassNo2,Grid,NBC,OP,GridR),!.
reduce_grid_pass(_PassNo,Grid,_,[],Grid).

reduce_pair_op(PassNo,Grid,NBC,[OP|More],Reduced):- 
    reduce_1pair_op(PassNo,Grid,OP,GridR),
    Grid\==GridR, \+ (member(E,NBC), E==GridR),
    reduce_grid_pass(PassNo,GridR,[GridR|NBC],More,Reduced),!.
reduce_pair_op(_,NR,_,[],NR):-!.

test_reduce_grid(Grid,GridO):- reduce_grid(Grid,Ops,GridO),unreduce_grid(GridO,Ops,Unred),show_sf_same(Ops,Unred,Grid).


  

test_reduce_grid:- test_p2(test_reduce_grid).

show_sf_same(Info,Solution,ExpectedOut):- 
       count_difs(ExpectedOut,Solution,Errors),
        (Errors\==0 -> 
          (banner_lines(red),print_side_by_side(red,Solution,'Our Solution'(Errors),_,ExpectedOut,"Expected Solution"),
          pp(Info), banner_lines(red));

          (banner_lines(green), pp(Info),banner_lines(green))),!. 
  


%reduce_grid(PassNo,Grid,res(Opers,Result)):- reduce_grid(PassNo,Grid,Opers,res(Opers,Result)),!.

