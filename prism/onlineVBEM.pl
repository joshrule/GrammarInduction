%% imports

:- expand_environment('$GIJOE_ROOT/prism/util.pl', P), 
   cl(P).
:- expand_environment('$GIJOE_ROOT/prism/vis.pl', P), 
   cl(P).

%% a global record structure for storing data associated with oVBEM
set_ovbem_rec(Flag, Val) :- 
    retractall(ovbem_rec(Flag, _)),
    assert(ovbem_rec(Flag, Val)).

get_ovbem_rec(Flag, Val) :- ovbem_rec(Flag, Val).

ovbem_rec_assoc(Assoc) :- 
    findall(K\V, ovbem_rec(K, V), Assoc).

:- dynamic ovbem_rec/2. 
ovbem_rec(niter, 100).
ovbem_rec(iter, 0).
ovbem_rec(batch_size, 10).
ovbem_rec(data_set, []).
ovbem_rec(sw_a, []).
ovbem_rec(logfile, '.ovbem_log').
ovbem_rec(outdir, '.ovbem_out').
ovbem_rec(learning_rate_type, constant(0.1)).

initialize_outdir :- 
    get_ovbem_rec(outdir, Dir), 
    directory_exists(Dir), !, 
    throw(outdir_already_exists(Dir)). 

initialize_outdir :- 
    get_ovbem_rec(outdir, Dir), 
    make_directory(Dir).

save_ovbem_rec :- 
    get_ovbem_rec(outdir, Dir),
    get_ovbem_rec(iter, Iter),
    number_atom(Iter, Iter_A),
    ovbem_rec_assoc(Assoc), 
    atom_concat('data.', Iter_A, File), 
    atom_concats([Dir, '/', File], Path),
    save_clauses(Path, [Assoc], []). 

set_ovbem_alpha_0 :- 
    get_reg_sw_list(Switches), 
    SwitchAlphas @= [Sw\As: Sw in Switches, [As, _1, _2], 
                     get_sw_a(Sw, _1, _2, As)], 
    set_ovbem_rec(alpha_0, SwitchAlphas). 
             
get_ovbem_alpha_0(Sw, As) :- 
    get_ovbem_rec(alpha_0, SwitchAlphas), 
    get_assoc(SwitchAlphas, Sw, As).


random_init_alpha :- 
    random_init_alpha(1, _S).
random_init_alpha(K) :- 
    random_init_alpha(K, _S).
random_init_alpha(K, SwitchAlphas) :- 
    get_reg_sw_list(Switches), 
    SwitchAlphas @= [Sw\As: Sw in Switches, 
                     [Vs, N, As],
                     (get_values(Sw, Vs), 
                      length(Vs, N), 
                      As @=[A: I in 1..N, [A, A1], 
                            (random_uniform(A1),
                             A is K * A1)],
                      set_sw_a(Sw, As))
                    ].
    

set_ovbem_flags :- 
    set_prism_flag(search_progress, 1), 
    set_prism_flag(em_progress, 1),
    set_prism_flag(max_iterate, 50),
    set_prism_flag(restart,1), 
    set_prism_flag(std_ratio, 10),  
    set_prism_flag(epsilon, 1.0e-6),
    set_prism_flag(learn_mode,vb),
    set_prism_flag(viterbi_mode,vb),
    set_prism_flag(reset_hparams, off), %% we want this off so that
                                        %% the statistics we learn are
                                        %% calculated with respect to
                                        %% the new alphas at each iteration
    set_prism_flag(log_scale,on), 
    set_prism_flag(learn_message, all), 
    set_prism_flag(verb, em),
    set_prism_flag(default_sw_a, uniform(1)), 
    set_prism_flag(daem, off), 
    set_prism_flag(itemp_init,0.001), 
    set_prism_flag(itemp_rate, 1.9), 
    set_prism_flag(show_itemp, on).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_ovbem :- 
    date(Start), %% get start time
    set_ovbem_rec(start_date, Start),
    set_ovbem_alpha_0, 
    ovbem_rec_assoc(Assoc), 
    get_assocs(Assoc, 
               [niter, batch_size, data_set], 
               [NIter, BatchSize, DataSet]), 
    set_ovbem_flags,
    ovbem(NIter, BatchSize, DataSet), %% actually run oVBEM
    date(End), 
    set_ovbem_rec(end_date, End).

ovbem(NIter, BatchSize, DataSet) :- 
    ovbem(NIter, BatchSize, DataSet, 1).

ovbem(NIter, BatchSize, DataSet, StartIter) :-
    % get number of latent parameters
    findall(Info, get_sw_a(Info), PInit),
    findall(X, member(switch(_,_,_,X),PInit), Gs),
    flatten(Gs,G),
    length(G,N),
    % initialize rate
    Tau_0 = 10, %% moving average window size
    GBar_0 @= [0 : _X in 1..N],
    HBar_0 = 0,
    Rho_0 = 1.0,
    % randomly initialize alphas
    random_init_alpha,
    % run the online VB-EM
    ovbem(NIter, BatchSize, DataSet, rate(Tau_0,GBar_0,HBar_0,Rho_0), 
          StartIter, StartIter). 

ovbem(NIter, _BatchSize, _DataSet, _RateInit, StartIter, Iter) :- 
    (Iter-StartIter+1) > NIter, !.
ovbem(NIter, BatchSize, DataSet, RateInit, StartIter, Iter) :- 
    format("ovbem iter# ~w\n", [Iter]),
    %% debug: report data set expected log likelihood
    %% findall(X, member(count(X,_), DataSet), Xs), 
    %% free_energy(Xs, F, L0, L1, L2),
    %% format("~nDEBUG: dataset expected loglike: ~w~n", [L2]), 
    %%%
    ovbem1(DataSet, BatchSize, RateInit, RateFinal), 
    %% save data
    findall(Sw\As, get_sw_a(Sw, _, _, As), SwitchAlphas),
    set_ovbem_rec(sw_a, SwitchAlphas),
    save_ovbem_rec,
    %% show_lpn, 
    %%%%%%
    NextIter is Iter+1, 
    set_ovbem_rec(iter, NextIter),
    ovbem(NIter, BatchSize, DataSet, RateFinal, StartIter, NextIter).

ovbem1(DataSet, BatchSize, RateInit, RateFinal) :- 
    %% format("running: ~w\n", [ovbem1(DataSet, BatchSize, RateInit, RateFinal)]), 
    length(DataSet, N),
    % get current alphas
    findall(Sw\As, get_sw_a(Sw, _, _, As), SwitchAlpha),
    % sample a data point from the dataset
    random_selects(DataSet, BatchSize, Batch),
    M is round(N/BatchSize),
    BatchOut @= [D1: D in Batch, [D1], multiply_datum_count(D, M, D1)],
    % run vbem
    write(BatchOut), nl,
    learn(BatchOut),
    % get output alphas:
    % use current alpha to compute expected counts for datum;
    % add those counts to initial alpha hyperparams
    SwitchAlphaHat @= [Sw\AlphaHat : Sw\Alpha in SwitchAlpha,
                        [Counts, Alphas_0, AlphaHat, _1, _2, _3], 
                        (get_sw_a(Sw, _1, _2, _3, Counts), 
                         get_ovbem_alpha_0(Sw, Alphas_0), 
                         maplist_math(+, Alphas_0, Counts, AlphaHat))],
    % update learning rate
    get_ovbem_rec(learning_rate_type, LearningRateType), 
    update_learning_rate(LearningRateType, 
                         SwitchAlpha, SwitchAlphaHat,RateInit,RateFinal),
    RateFinal = rate(_,_,_,StepSize),
    format("~nDEBUG: StepSize: ~w~n", [StepSize]),
    %% print_rate(RateFinal),
    % interpolate SwitchAlpha and SwitchAlphaHat, setting new alphas values
    !, %% failure driven loop to update each switch alphas
    (
    member(Sw\As0, SwitchAlpha), 
    member(Sw\As1, SwitchAlphaHat), 
    update_vb_params(StepSize, As0, As1, AsOut),
    %% format("StepSize: ~w\n, As0: ~w\n, As1: ~w\n, AsOut: ~w\n\n", [StepSize, As0, As1, AsOut]),
    set_sw_a(Sw, AsOut), 
    fail; 
    true).
    
print_rate(rate(T,G,H,R)) :-
    meanlist(G,GSum),
    format("Tau:~2f HBar:~2f Rho:~10f ~n",[T,H,R]).
    %% format("GBar: ~w ~n", [G]).

%% We use an adaptive learning rate requiring no tuning. Essentially,
%% we are trying to minimize the expected error between the batch and
%% stochastic (online) update. We do so via the following update rule:
%%
%% \lambda_{t+1} = (1-\rho_t)\lambda_t + \rho_t \hat{lambda_t} where,
%%
%% lambda_t is the set of estimated hyperparameters from iteration t
%% \hat{lambda_t} is the intermediate estimate for iteration t+1
%%
%% \rho_t = \bar{g}^{\trans}_t \bar{g}_t / \bar{h}_t \bar{g}_t =
%% (1-1/\tau_t)\bar{g}_{t-1} + g_t/\tau_t \bar{h}_t =
%% (1-1/\tau_t)\bar{h}_{t-1} + g_t^{\trans}g_t/\tau_t
%% \tau_{t+1} = \tau_t(1-\rho_t)+1

update_learning_rate(constant(Rho), _, _, 
                     _,
                     rate(_, _, _, Rho)). 
update_learning_rate(hoffman(T0, Kappa), _, _, _, rate(_, _, _, Rho)) :-
    get_ovbem_rec(iter, Iter),
    Rho is (T0**Kappa)*(T0 + Iter)**(-Kappa).
update_learning_rate(adaptive, 
                     SwAlphasInit,
                     SwAlphasPrime,
                     rate(TauT,GBarTMinus1,HBarTMinus1,_),
                     rate(TauTPlus1,GBarT,HBarT,RhoT)) :-
    compute_g(SwAlphasInit,SwAlphasPrime,Gt),
    update_bar_g(TauT, GBarTMinus1, Gt, GBarT),
    update_bar_h(TauT, HBarTMinus1, Gt, HBarT),
    update_rho(GBarT, HBarT, RhoT),
    update_tau(TauT,  RhoT,  TauTPlus1).
    
compute_g(SwAlphasInit,SwAlphasFinal,G) :-
    findall(X, (member(Sw\AI,SwAlphasInit),
                 member(Sw\AF,SwAlphasFinal),
                 maplist_math(-,AF,AI,X)
               ),Gs),
    flatten(Gs,G).

%% dot product of two lists (should we use bprolog arrays for this?)
dot([],[],0).
dot([X|Xs],[Y|Ys],N) :- dot(Xs,Ys,Rest), N is X*Y + Rest.

update_bar_g(TauT,GBarTMinus1,Gt,GBarT) :-
    length(Gt,N),
    Co1 is 1-(1/TauT),
    Co2 is 1/TauT,
    Co1s @= [Co1 : X in 1..N],
    Co2s @= [Co2 : X in 1..N],
    maplist_math(*,Co1s,GBarTMinus1,Summand1),
    maplist_math(*,Co2s,Gt,Summand2),
    maplist_math(+,Summand1,Summand2,GBarT).

update_bar_h(TauT,HBarTMinus1,Gt,HBarT) :-
    dot(Gt,Gt,GDot),
    HBarT is (1-(1/TauT))*HBarTMinus1 + GDot/TauT.

update_rho(GBarT,HBarT,Rho) :-
    dot(GBarT,GBarT,GDot),
    Rho is GDot/HBarT.

update_tau(TauT,RhoT,TauTPlus1) :- 
    TauTPlus1 is TauT*(1-RhoT) + 1.

update_vb_params(_, [], [], []).
update_vb_params(StepSize, [A0|As0], [A1|As1], [AOut|AsOut]) :-
    AOut is (1-StepSize)*A0 + StepSize*A1,
    update_vb_params(StepSize, As0, As1, AsOut).

multiply_datum_count(count(X, C), N, count(X, C1)) :- !, C1 is C * N.
multiply_datum_count(X, N, count(X, N)).

%%%%%%%% Loading ovbem data %%%%%%
load_ovbem_data(PATH, Assoc) :- 
    load_clauses(PATH, [Assoc], []).

load_ovbem_psm(PATH) :- 
    load_ovbem_data(PATH, Assoc), 
    get_assoc(Assoc, sys_psm_file, Psm), 
    prism([load], Psm), 
    get_assoc(Assoc, sw_a, Sws),
    foreach(Sw\As in Sws, 
            (write(Sw), nl, 
             set_sw_a(Sw, As))).
    
    
%%%% for evaluating held out data %%%%%

run_held_out(HeldOut, DataDir, [0\LL0|LLs], ReloadPsm) :- 
    directory_files(DataDir, AllFiles),
    Paths @= [Path : File in AllFiles, [File, Path],
              (
                  atom_concats([DataDir, '/', File], Path), 
                  file_property(Path, type(regular)))], 
    Paths = [P0|_],
    load_ovbem_data(P0, Rec0), 
    get_assoc(Rec0, sys_psm_file, Psm0), 
    (ReloadPsm = yes -> prism([load], Psm0); true), 

    set_training_flags, 
    set_prism_flag(clean_table, off),
    free_energy(HeldOut, _, _, _, LL0),
    LLs @= [Iter\LL : Path in Paths,  
            [_A, _B, _C, Paths, LL, V,Rec,Iter, Sw, As, Sws],
            (
                write(Path), nl, 
                load_ovbem_data(Path, Rec), 
                get_assoc(Rec, iter, Iter), 
                get_assoc(Rec, sw_a, Sws),
                foreach(switch(Sw, _, _, As) in Sws, 
                        (write(Sw), nl, 
                         set_sw_a(Sw, As))),
                set_training_flags, 
                set_prism_flag(clean_table, off),
                free_energy(HeldOut, _A, _B, _C, LL), 
                format("Held Out Exp LL: Iter #~w: ~w\n", [Iter, LL])
           )]. 
    
ovbem_held_out(NIter, Iter, 
               BatchSize, DataSet, HeldOut, IterBetween) :- 
    (((NIter-Iter) > IterBetween) -> 
        IterBetween1 = IterBetween;
        IterBetween1 = NIter-Iter),
    ovbem(IterBetween1, BatchSize, DataSet, Iter), 
    Iter1 is Iter + IterBetween1,
    set_ovbem_rec(iter, Iter1), 
    run_held_out(HeldOut),
    write('NIter: '), write(NIter), nl,
    write('Iter1: '), write(Iter1), nl,
    (Iter < NIter -> 
        ovbem_held_out(NIter, Iter1, BatchSize, DataSet, HeldOut, IterBetween);
     true).
