
log_prob_test(TestSet, LogProbData) :- 
    LogProbData @= [Ex-Label\LogP : Ex-Label in TestSet, [LogP, Ex, Label, Ex1, Y], 
                 (
                     (Ex=count(Ex1, Y) -> true; Ex1=Ex),
                     prob(Ex1, LogP), 
                     format("~w - ~w: ~w\n", [Ex1, Label, LogP])
                )].

hit_falsealarm(TestSet, NThreshs, Out) :- 
    log_prob_test(TestSet, LogProbData), 
    LogPsLabeled @= [P\L:_-L\P in LogProbData], 
    LogPs @= [P: P\L in LogPsLabeled], 
    Labels @= [L: P\L in LogPsLabeled], 
    PosLabels @= [1 : L in Labels, [], L=positive],
    NegLabels @= [1 : L in Labels, [], L=negative],
    scale_to_01(LogPs, ScaledLogPs), 
    zip(ScaledLogPs, Labels, ScaledLogPsLabeled),
    length(ScaledLogPsLabeled, N),
    length(PosLabels, NPos), 
    length(NegLabels, NNeg),
    linspace(0, 1, NThreshs, Thresholds),
    Out @= [Thresh\HitRate\FalseAlarmRate : 
            Thresh in Thresholds, 
            [Thresh, HitRate, FalseAlarmRate, Label, V, X, Hits, FalseAlarms, NHits, NFAs],
            (
                Hits @= [X : V\Label in ScaledLogPsLabeled, 
                         [X, V, Label], 
                         (V > Thresh, Label=positive -> X = 1; X = 0)], 
                FalseAlarms @= [X : V\Label in ScaledLogPsLabeled, 
                                [X, V, Label], 
                                (V > Thresh, Label=negative -> X = 1; X = 0)], 
                NHits is sum(Hits),
                NFAs is sum(FalseAlarms),
                HitRate is NHits/NPos, 
                FalseAlarmRate is NFAs/NNeg
            )].
    

write_labeled_scores(TestSet) :- 
    log_prob_test(TestSet, LogProbData), 
    foreach(Ex-Label\LogP in LogProbData,
            [Label, L, LogP],
            ((Label=positive -> L=1; L=0), 
             format("[~w, ~w],\n", [L, LogP]))).

%%% Test counting %%%%%%
    
viterbi_succ(MaxLength, X, Y\P) :-
    findall(L, between(1, MaxLength, L), Lengths),
    viterbi_succ_sub(Lengths, X, Y\P), 
    write(Y), nl, 
    viterbi_succ(MaxLength, Y, _).

viterbi_succ_sub([], X, _\negInfty) :- !.
viterbi_succ_sub([Length|Ls], X, YHat\PHat) :- !, 
    viterbi_succ_sub1(Length, X, Y\P),
    viterbi_succ_sub(Ls, X, YHat0\PHat0), 
    (gt_negInfty(P, PHat0) -> 
            YHat = Y, PHat=P; 
            YHat = YHat0, PHat = PHat0).
viterbi_succ_sub1(Length, X, Y\P) :- 
    length(Y, Length),
    viterbig(prove('Succ_2'-[Y, X]), P), !.
viterbi_succ_sub1(Length, X, _\negInfty).

gt_negInfty(A, negInfty).
gt_negInfty(A, B) :- 
    B \= negInfty, 
    A \= negInfty, 
    A > B.



succ_word(U, W) :- 
    number_word(N, U), 
    number_word(N1, W), 
    N1 is N + 1.



%% Exp: softmax behavior; pure sampling Exponent=1; maximizing Exponent -> infty
get_successor_distribution(X, Distr1, N, MaxWordLength, Exp) :-
    findall(Y\LogP, 
            (between(1, MaxWordLength, L), 
             get_successor_distribution_sub(X, D, N, L), 
             member(Y\LogP0, D), 
             %% succ_word(U, X), 
             %% Y\=U, 
             LogP is Exp*LogP0
            ), Distr), 
    assoc_to_lists(Distr, Ys, LogPs),
    maplist_math(exp, LogPs, Ws), 
    normalize(Ws, Ps), 
    assoc_to_lists(Distr1, Ys, Ps).

%% NOTE: we don't actually need this because viterbi functions seem to 
%% always return log probabilities (?)
%% prismScoreToLogP(S, S) :- get_prism_flag(log_scale, on). 
%% prismScoreToLogP(S, LogP) :- 
%%     get_prism_flag(log_scale, off),
%%     LogP is log(S).

get_successor_distribution_sub(X, Distr, N, Length) :- 
    length(Y, Length),
    findall(Y\LogP, 
            (n_viterbig(N, prove('Succ_2'-[Y, X]), LogP)),             
            Distr).

sample_successor(X, Y, N, MaxWordLength, Exp) :- 
    get_successor_distribution(X, Distr, N, MaxWordLength, Exp), 
    assoc_to_lists(Distr, Ys, Ps), 
    random_select(Ys, Ps, Y).

succ_probability(W,Succ, P, N, MaxWordLength, Exp) :-
    get_successor_distribution(W, Distr, N, MaxWordLength, Exp), 
    findall(P, (member(Succ\P, Distr)), Ps), 
    P is sum(Ps).

successor_distribution(W, Distr, N, MaxWordLength) :- 
    get_successor_distribution(W, Distr0, N, MaxWordLength, 1), 
    sum_over_keys(Distr0, Distr).

successor_distribution(W, N, MaxWordLength) :- 
    successor_distribution(W, Distr, N, MaxWordLength),
    write_assoc(Distr).

successor_distributions(Ws, Distrs, N, MaxWordLength) :- 
    findall(W\D, (member(W, Ws), 
                  successor_distribution(W, D, N, MaxWordLength)), 
            Distrs).

successor_distributions(Ws, N, MaxWordLength) :- 
    successor_distributions(Ws, Distrs, N, MaxWordLength),
    member(W\Distr, Distrs), 
    format("==== Successor of ~w ==== \n", W), 
    write_assoc(Distr), 
    fail.
successor_distributions(_, _, _) :- true.



count_list_transition_probabilities(Lo, Hi, Ps, N, MaxWordLength, Exp) :- 
    Ps @= [P: I in Lo..Hi, [P, I1, W, W1], 
           (number(I, W), 
            I1 is I+1, 
            number(I1, W1),
            succ_probability(W, W1, P, N, MaxWordLength, Exp))], 
    write('Count list transition probabilities:'), nl, 
    write(count_list_transition_probabilities(Lo, Hi, Ps, N, MaxWordLength, Exp)), nl.
            
count_list_probability(Lo, Hi, Ps, N, MaxWordLength, Exp) :- 
    count_list_transition_probabilities(Lo, Hi, Ts, N, MaxWordLength, Exp), 
    write('Count list transition probabilities:'), nl, 
    write(Ts), nl, 
    inits(Ts, Tss),
    maplist(prodlist, Tss, Ps).
    



    
    
    

    


%%%%%%%%%%%%%%%%%%%%%

%% utilties
scale_to_01(Xs, Ys) :- 
    Max=max(Xs), 
    Min=min(Xs), 
    Ys @= [Y :X in Xs,  [Y], Y is (X-Min)/(Max-Min)].


linspace(Lo, Hi, N, Xs) :-
    Delta is (Hi-Lo)/(N-1), 
    Xs @= [X : I in 0..(N-1), [I, X],
           X is I*Delta + Lo].

tails([], []).
tails([X], [[X]]) :- !.
tails([X|Xs], [T, S|Ts]) :- 
    T = [X|S], 
    tails(Xs, [S|Ts]).

inits(Xs, Ys) :- 
    reverse(Xs, Xs1), 
    tails(Xs1, Ys1),
    reverse(Ys1, Ys2), 
    maplist(reverse, Ys2, Ys).

prodlist(Xs, Y) :- 
    prodlist_go(Xs, 1, Y).
prodlist_go([], Acc, Acc).
prodlist_go([X|Xs], Acc, Y) :- 
    Acc1 is Acc*X, 
    prodlist_go(Xs, Acc1, Y).

nub([], []). 
nub([X|Xs], Ys) :- 
    nub(Xs, Ys0),
    (member(X, Ys0) -> 
         Ys=Ys0;Ys=[X|Ys0]).
    
sum_over_key(AssocIn, Key, Sum) :- 
    findall(V, member(Key\V, AssocIn), Vs), 
    Sum is sum(Vs). 
    
sum_over_keys(AssocIn, AssocOut) :- 
    assoc_keys(AssocIn, Ks0), 
    nub(Ks0, Ks), 
    findall(K\S, 
            (member(K, Ks), 
             sum_over_key(AssocIn, K, S)), 
            AssocOut).
             
                
