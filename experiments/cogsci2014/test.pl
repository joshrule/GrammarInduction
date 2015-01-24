
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
viterbi_succ(X, Y) :-
    viterbi_succ(X, Y, 1), 
    write(Y), nl, 
    viterbi_succ(Y, Z).

viterbi_succ(X, Y, Length) :- 
    length(Y, Length),
    viterbig(prove('Succ_2'-[Y, X])).


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


