%% Modeling the early structure of counting sequence 
%% as described in Fuson et al. 1982 section "Overall structure 
%% of the sequence"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% We model just the successor relation on numbers 1 - 20
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
     

%% training 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_sys_psm :- expand_environment('$GIJOE_ROOT/experiments/cogsci2014/number_5_5_3_lpn.sys.psm', 
                                   P), prism([load], P).

init :- 
    load_sys_psm, 
    set_training_flags, 
    set_prism_flag(epsilon, 1), 
    set_prism_flag(default_sw_a, uniform), 
    set_prism_flag(std_ratio, 1000.0), 
    set_prism_flag(reset_hparams, off), 
    set_prism_flag(restart, 10). 


go :- 
    init, 
    goals(Goals),
    learn(Goals).
      
         
        
    

              
    

    
    
    
    
    

