%% The system that 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Two components for learning                                 %%%
%%- 1) Learning up to 100                                      %%%
%%- 2) Beyond 100 up to 1000                                   %%%
%% show how you can solve the sparsity problem.                %%%
%% patterns of errors                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- expand_environment('$GIJOE_ROOT/prism/train.pl', P), 
   cl(P).

:- expand_environment('$GIJOE_ROOT/prism/assoc_list.pl', P), 
   cl(P).

:- ['numbers.pl'].

succ_goal(N, prove('Succ_2'-[Y, X])) :- 
    number_word(N, X), 
    N1 is N + 1, 
    number_word(N1, Y).

number_goal(N, prove('Number_1'-[X])) :- 
    number_word(N, X).

goals(Goals) :- 
    Numbers @= [G : I in 1..99, [G], number_goal(I, G)], 
    Succs @= [G : I in 1..99, [G], succ_goal(I, G)], 
    append(Numbers, Succs, Goals).

counts([], []) :- !.
counts(Vs, Assoc) :- counts(Vs, [], Assoc).

counts([], A, A). 
counts([V|Vs], AssocIn, AssocOut) :- 
    (get_assoc(AssocIn, V, C) -> 
         C1 is C+1, 
         set_assoc(AssocIn, V, C1, Assoc1),
         counts(Vs, Assoc1, AssocOut);
         set_assoc(AssocIn, V, 1, Assoc1), 
         counts(Vs, Assoc1, AssocOut)).
