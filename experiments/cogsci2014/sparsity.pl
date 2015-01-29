%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% show how you can solve the sparsity problem.                %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- expand_environment('$GIJOE_ROOT/prism/train.pl', P), 
   cl(P).

:- ['numbers.pl'].

%% generating goals

number_goal(Ns, Ps, prove('Number_1'-[X])) :- 
    random_select(Ns,Ps,N),
    number_word(N, X).

succ_goal(Ns, Ps, prove('Succ_2'-[X, Y])) :- 
    random_select(Ns,Ps,N),
    N1 is N + 1, 
    number_word(N,  X),
    number_word(N1, Y).

succ10_goal(Ns, Ps, prove('Succ10_2'-[X, Y])) :-
    random_select(Ns,Ps,N),
    0 is N mod 10,
    N1 is N + 10,
    number_word(N,  X),
    number_word(N1, Y).

succ100_goal(Ns, Ps, prove('Succ100_2'-[X, Y])) :-
    random_select(Ns,Ps,N),
    0 is N mod 100,
    N1 is N + 100,
    number_word(N,  X),
    number_word(N1, Y).

more_goal(Ns, Ps, prove('More_2'-[X, Y])) :-
    more_goal(Ns, Ps, X, Y).

more_goal(Ns, Ps, X, Y) :-
    random_select(Ns,Ps,N1),
    random_select(Ns,Ps,N2),
    (N1 > N2 -> number_word(N1, X),
                number_word(N2, Y);
     (N2 > N1 -> number_word(N2, X),
                 number_word(N1, Y);
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
          [succ10_goal,  1000,1.02,N10s ,20,0.8],
          [succ100_goal, 1000,1.02,N100s,20,0.8],
          [more_goal,    1000,1.02,N1000,20,0.8]].

%% PCNPs is a list of lists. Each sub-list contains [P,C,Ns,Ps], where
%% P  = the predicate
%% C  = the count
%% Ns = the numbers available for sampling
%% Ps = the probabilities with which Ns will be sampled
generate_pcnps([],[]).
generate_pcnps([[P,C,A,Ns1,M,PFlip]|Xs],[[P,C,Ns,Ps]|Rest]) :-
    ns_and_ps(A,Ns1,M,PFlip,Ns,Ps),
    generate_pcnps(Xs,Rest).

%% what you should execute

init :- 
    set_training_flags, 
    set_prism_flag(epsilon, 0.1), 
    set_prism_flag(default_sw_a, uniform(0.1)), 
    set_prism_flag(std_ratio, 1000.0), 
    set_prism_flag(reset_hparams, off), 
    set_prism_flag(restart, 2). 

go(PCNPS,Goals) :-
    init,
    predicates_to_test(Ps),
    generate_pcnps(Ps,PCNPs),
    generate_data(PCNPs,Goals).
