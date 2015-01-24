:- expand_environment('$GIJOE_ROOT/prism/train.pl', P), 
   cl(P).

%% load .sys.psm file.
load_sys_psm :- expand_environment('$GIJOE_ROOT/experiments/cogsci2014/number_5_5_3_lpn.sys.psm', 
                                   P),
                prism([load], P).

%% load data set
%% this includes before and after sentences with 20 held out test sentences
:- expand_environment('$GIJOE_ROOT/experiments/cogsci2014/phase2_data.pl', P),
   cl(P).

:- expand_environment('$GIJOE_ROOT/experiments/cogsci2014/phase1_data.pl', P),
   cl(P).

%% load data set
%% this includes before and after sentences with 20 held out test sentences
:- expand_environment('$GIJOE_ROOT/experiments/cogsci2014/phase3_data.pl', P),
   cl(P).


init :- 
    load_sys_psm, 
    set_training_flags, 
    set_prism_flag(epsilon, 0.01), 
    set_prism_flag(default_sw_a, uniform), 
    set_prism_flag(std_ratio, 1000.0), 
    set_prism_flag(reset_hparams, off), 
    set_prism_flag(restart, 10). 

phase1_data(Train, Test) :- 
    Cs @= [40000, 10000, 4444, 2500, 1600, 1111, 816, 625, 493, 400], 
    findall(X, phase1_train(count(X, Y)), Xs), 
    zip(Xs, Cs, XCs), 
    findall(count(X, C), member(X\C, XCs), Train).
    

phase2_data(Train, Test) :- 
    findall(count(X, Y), phase2_train(count(X, Y)), Train), 
    findall(X-Label, phase2_test(X, Label), Test).

phase2_data(Pred, Train, Test):- 
    findall(count(X, Y), (phase2_train(count(X, Y)), X=prove(Pred-_)), Train), 
    findall(X-Label, (phase2_test(X, Label), X=count(prove(Pred-_), _)) , Test).

phase3_data(Train, Test) :- 
    findall(count(X, Y), phase3_train(count(X, Y)), Train), 
    findall(X-Label, phase3_test(X, Label), Test).

go_phase1 :- 
    phase1_data(Train, Test),
    learn(Train).

go_phase2 :- 
    phase2_data(Train, Test),
    learn(Train).

go_phase3 :- 
    phase2_data(Train2, _),
    phase3_data(Train3, _),
    append(Train2, Train3, Train),
    learn(Train).


go_phase123 :- 
    go_phase1, 
    go_phase2, 
    go_phase3.

go :- 
    init, 
    go_phase123.


test_phase2(Pred) :- 
    phase2_data(Pred, _, Test), 
    write_labeled_scores(Test).

test_phase2 :- 
    phase2_data(_, Test), 
    write_labeled_scores(Test).

test_phase3(Pred) :- 
    phase3_data(Pred, _, Test), 
    write_labeled_scores(Test).

test_phase3 :- 
    phase3_data(_, Test), 
    write_labeled_scores(Test).
       
