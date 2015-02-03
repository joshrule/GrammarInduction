%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% show how you can solve the sparsity problem.                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- expand_environment('$GIJOE_ROOT/prism/train.pl', P), 
   cl(P).

%% load .sys.psm file.
load_sys_psm :- expand_environment('$GIJOE_ROOT/experiments/cogsci2014/number_5_5_2_lpn.sys.psm', 
                                   P),
                prism([load], P).

:- ['numbers.pl'].
:- ['test.pl'].

%% what language? 
:- dynamic number_language/1.
number_language(english).

set_language(Lang) :- 
    retractall(number_language(_)), 
    asserta(number_language(Lang)).

number(N, W) :- 
    number_language(Lang), 
    number_word(N, W, Lang).


%% generating goals

number_goal(Ns, Ps, prove('Number_1'-[X])) :- 
    random_select(Ns,Ps,N),
    number(N, X).

succ_goal(Ns, Ps, prove('Succ_2'-[X, Y])) :- 
    random_select(Ns,Ps,N),
    N1 is N + 1, 
    number(N,  X),
    number(N1, Y).

succ10_goal(Ns, Ps, prove('Succ10_2'-[X, Y])) :-
    random_select(Ns,Ps,N),
    0 is N mod 10,
    N1 is N + 10,
    number(N,  X),
    number(N1, Y).

succ100_goal(Ns, Ps, prove('Succ100_2'-[X, Y])) :-
    random_select(Ns,Ps,N),
    0 is N mod 100,
    N1 is N + 100,
    number(N,  X),
    number(N1, Y).

more_goal(Ns, Ps, prove('More_2'-[X, Y])) :-
    more_goal(Ns, Ps, X, Y).

more_goal(Ns, Ps, X, Y) :-
    random_select(Ns,Ps,N1),
    random_select(Ns,Ps,N2),
    (N1 > N2 -> number(N1, X),
                number(N2, Y);
     (N2 > N1 -> number(N2, X),
                 number(N1, Y);
      more_goal(Ns, Ps, X, Y))).

%% This function takes a list of Predicate information and generates a
%% flattened list of goals from it.
generate_data(PCNPs,Goals) :-
    Gs @= [G : PCNP in PCNPs, [G], sample_relations(PCNP,G)],
    flatten(Gs,Gs2),
    countlist(Gs2,Gs3),
    create_counts(Gs3,Goals).

sample_relations([P,C,Ns,Ps],Gs) :-
    Gs @= [G : I in 1..C, [G], call(P,Ns,Ps,G)].

create_counts([],[]). 
create_counts([T=N|Xs],[count(T,N)|Rest]) :-
    create_counts(Xs,Rest).

%% generating lists for sampling?
%%
%% We want to take a list of numbers, along with a distribution over
%% how likely they are to be in our list, walk along and flip a coin
%% for each. That gives us our list of Ns. We also have a geometric
%% distribution over the list from 1 to Max(Ns). After selecting our
%% subset, we renormalize that, giving our our Ns and Ps.
ns_and_ps(A,Ns1,M,PFlip,Ns,Ps) :-
    M =< max(Ns1),
    Bs @= [B : I in Ns1, [B], (I =< M -> B = 1; B = PFlip)],
    accum_by_flip(Ns1,Bs,Ns),
    Ps1 @= [P : N in Ns, [P], (P is 1 / (A**N))],
    Ps @= [P : N in Ps1, [P], (P is N/sum(Ps1))].

accum_by_flip([],[],[]).
accum_by_flip([N1|Ns1],[B|Bs],Ns) :-
    NotB is 1-B,
    random_select([[],[N1]],[NotB,B],Z),
    accum_by_flip(Ns1,Bs,Rest),
    append(Z,Rest,Ns).
    
%% Each sublist contains, in this order:
%% - predicate name
%% - total count
%% - geometric alpha
%% - possible Ns in relation
%% - numbers less than this guaranteed
%% - probability of non-guaranteed numbers
predicates_to_test(Ps) :-
    N1000 @= [I : I in 1..1000],
    N999  @= [I : I in 1..999],
    N10s  @= [I : I in 10..10..990],
    N100s @= [I : I in 100..100..900],
    Ps = [[number_goal,  1000,1.02,N1000,20,0.8],
          [succ_goal,    1000,1.02,N999 ,20,0.8],
          [succ10_goal,  0,1.02,N10s ,20,0.8],
          [succ100_goal, 0,1.02,N100s,20,0.8],
          [more_goal,    0,1.02,N1000,20,0.8]].

%% PCNPs is a list of lists. Each sub-list contains [P,C,Ns,Ps], where
%% P  = the predicate
%% C  = the count
%% Ns = the numbers available for sampling
%% Ps = the probabilities with which Ns will be sampled
generate_pcnps([],[]).
generate_pcnps([[P,C,A,Ns1,M,PFlip]|Xs],[[P,C,Ns,Ps]|Rest]) :-
    ns_and_ps(A,Ns1,M,PFlip,Ns,Ps),
    generate_pcnps(Xs,Rest).


%% Threshold goals
threshold_goal(count(Goal, C), Thresh, Goal) :- 
    !, 
    C >= Thresh. 
threshold_goal(Goal, Thresh, Goal) :- 
    threshold_goal(count(Goal, 1), Thresh, Goal).

threshold_goals([], Thresh, []).
threshold_goals([G|Gs], Thresh, [G1|Gs1]) :- 
    threshold_goal(G, Thresh, G1), !, 
    threshold_goals(Gs, Thresh, Gs1).
threshold_goals([G|Gs], Thresh, Gs1) :- 
    threshold_goals(Gs, Thresh, Gs1).

%% Goals to numbers
goal_number(count(G, _), N) :- 
    !, 
    goal_number(G, N). 
goal_number(prove(_-[X|_]), N) :- 
    number(N, X).
                               

%% Generate goal sequences 
%% The count for each goal decays as a power law
%% i.e. Count(i) = CountBase/(i^Exponent)
sequence_goals(CountBase, Exponent, Lo, Hi, Goals) :-
    findall(G, (between(Lo, Hi, N), 
                I is N - Lo + 1,
               sequence_goals_sub(CountBase, Exponent, N, I, G)), 
            Goals).
    
sequence_goals_sub(CountBase, Exponent, Number, Index, Goal) :- 
    number(Number, Word), 
    NextNumber is Number + 1, 
    number(NextNumber, NextWord), 
    Count is round(CountBase / (Index ** Exponent)), 
    Count > 0, 
    Goal = count(prove('Succ_2'-[NextWord, Word]), Count).
        

decade_goals(0, []) :- !.
decade_goals(Count, Goals) :- 
    findall(Goal, 
            (decade_goal(Goal0), 
             Goal = count(Goal0, Count)), Goals).

decade_goal(Goal) :- 
    between(1, 9, I), 
    N is I * 10, 
    number(N, W), 
    N1 is N + 10, 
    number(N1, W), 
    Goal = prove('Succ10_2'-[W1, W]).

predecade_goal(Goal) :- 
    between(3, 9, I), 
    N is (I * 10) -1, 
    N1 is N + 1, 
    number(N, W), 
    number(N1, W1),
    Goal = prove('Succ_2'-[W1, W]).

predecade_goals(0, []) :- !.
predecade_goals(Count, Goals) :- 
    findall(Goal, 
            (predecade_goal(Goal0),
             Goal=count(Goal0, Count)), Goals).
                           
    

%% what you should execute

init :- 
    set_prism_flag(search_progress, 1), 
    set_prism_flag(em_progress, 1),
    set_prism_flag(max_iterate, 200),
    set_prism_flag(viterbi_mode,vb),
    set_prism_flag(log_scale,on), 
    set_prism_flag(learn_message, all),
    set_prism_flag(epsilon, 0.00001), 
    set_prism_flag(learn_mode, vb),
    set_prism_flag(std_ratio, 1), 
    set_prism_flag(reset_hparams, off), 
    set_prism_flag(restart, 1), 
    set_prism_flag(verb, em), 
    set_prism_flag(clean_table, on), 
    set_prism_flag(init, random). 

init_sw_a(Sw, Mu, Sigma) :- 
    get_sw_a(Sw, [_, Vs, As0]),
    As @= [A: A0 in As0, [A],
           random_gaussian(Mu, Sigma, A)],
    set_sw_a(Sw, As).

init_sw_a(Mu, Sigma) :- 
    get_sw_a(Sw, _), 
    init_sw_a(Sw, Mu, Sigma), 
    fail. 
init_sw_a(_, _) :- true.
    

mk_data(Lo, Hi, CountBase, Decay, DecadeCount, PredecadeCount, Data) :- 
    sequence_goals(CountBase, Decay, Lo, Hi, SequenceData), 
    decade_goals(DecadeCount, DecadeData), 
    predecade_goals(PredecadeCount, PredecadeData),
    append(DecadeData, SequenceData, Data0),
    append(PredecadeData, Data0, Data).


go(Lo, Hi, CountBase, Decay, DecadeCount, PredecadeCount, SaveDir) :- 
    load_sys_psm,
    init, 
    sequence_goals(CountBase, Decay, Lo, Hi, SequenceData), 
    decade_goals(DecadeCount, DecadeData), 
    predecade_goals(PredecadeCount, PredecadeData),
    append(DecadeData, SequenceData, Data0),
    append(PredecadeData, Data0, Data),
    write(Data),
    show_prism_flags,
    init_sw_a(0.1, 0.01),
    learn(Data),
    go_file(SaveDir, CountBase, [F1, F2]), 
    save_sw_pa(F1, F2), 
    pruneAll(0.5).

go_file(SaveDir, N, [F, Fa]) :- 
    number_chars(N,X), 
    atom_concats([SaveDir, '/', sw_p_|X], F),
    atom_concats([SaveDir, '/', sw_a_|X], Fa).

run(Lo, Hi, Decay, Counts, DecadeRatio, PredecadeCounts, ListProbabilities, SaveDir) :-
    zip(Counts, PredecadeCounts, CPairs), 
    ListProbabilities 
        @= [Ps : C\PredecadeCount in CPairs, [Ps, C, PredecadeCount],
            run1(Lo, Hi, Decay, C, DecadeRatio, PredecadeCount, Ps, SaveDir)].
            
                          
run1(Lo, Hi, Decay, Count, DecadeRatio, PredecadeCount, Ps, SaveDir) :- 
    DecadeCount is round(Count * DecadeRatio),
    go(Lo, Hi, Count, Decay, DecadeCount, PredecadeCount, SaveDir),
    count_list_transition_probabilities(Lo, Hi, Ps, 5, 2, 2).

    
    
