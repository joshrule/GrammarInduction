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

%% Alpha is probability of skipping a number in the successor
succ_goal_noisy(N, Alpha, Goal) :- 
    succ_goal_noisy(N, Alpha, 1, Goal).

succ_goal_noisy(N, Alpha, Skip, prove('Succ_2'-[Y, X])) :- 
    number_word(N, X), 
    random_uniform(V), 
    ((V < Alpha) -> 
        Skip1 is Skip + 1,
        succ_goal_noisy(N, Alpha, Skip1, prove('Succ_2'-[Y, X]));
        N1 is N + Skip,
        number_word(N1, Y)).

%% elements 1/A^1, ..., 1/A^N normalized to sum to one
power_law_range(A, N, Vs, Ps) :- 
    Xs @= [X : I in 1..N, [X], 
           X is 1/(A**I)], 
    Vs @= [I:I in 1..N],
    normalize(Xs, Ps).
    
sample(A, Hi, Alpha, Goal) :- 
    power_law_range(A, Hi, Vs, Ps), 
    random_select(Vs, Ps, V),
    succ_goal_noisy(V, Alpha, Goal).

sample(A, Hi, Alpha, N, Goals) :- 
    Goals0 @= [Goal : I in 1..N, [Goal],
             sample(A, Hi, Alpha, Goal)], 
    counts(Goals0, Counts), 
    Goals @= [count(G, C) : G\C in Counts].


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
    set_prism_flag(epsilon, 0.1), 
    set_prism_flag(default_sw_a, uniform(0.1)), 
    set_prism_flag(std_ratio, 1000.0), 
    set_prism_flag(reset_hparams, off), 
    set_prism_flag(restart, 2). 


go :- 
    init, 
    sample(1.5, 20, 0.3, 4000, Goals), 
    learn(Goals).
      
         
        
    

              
    

    
    
    
    
    

