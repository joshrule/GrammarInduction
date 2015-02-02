%% run_sparsity.py
%%
%% script for collecting multiple trials of data from sparsity.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ['sparsity.pl'].

%% ---------------------------
%% Execution Options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- set_option(language, english). 
:- set_option(default_sw_a, 0.1).
:- set_option(sw_a_std, 0.01).
:- set_option(save_directory, '.').
:- set_option(prune_threshold, 0.5).
:- set_option(epsilon, 0.0001). 
:- set_option(max_iterate, 200).
:- set_option(search_progress, 1). 
:- set_option(em_progress, 1). 
:- set_option(restart, 1).
:- set_option(max_word_length, default). 
:- set_option(n_viterbi_bound, 5).
:- set_option(soft_max_exponent, 2). 
:- set_option(lo, 1). 
:- set_option(hi, 99).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%% ---------------------------
%% Set prism flags from options
set_prism_flags_from_options :- 
    PrismOptions = [search_progress, 
                     em_progress, 
                     max_iterate, 
                     epsilon], 
    foreach(Opt in PrismOptions, [V], 
            (get_option(Opt, V), 
             set_prism_flag(Opt, V))), 
    set_prism_flag(viterbi_mode, vb), 
    set_prism_flag(learn_mode, vb), 
    set_prism_flag(learn_message, all), 
    set_prism_flag(reset_hparams, off), 
    set_prism_flag(log_scale, on), 
    set_prism_flag(verb, em).

%% run learning for a single set of data
run_trial(DataSetId, Rep) :- 
    TrialId = trialId(DataSetId, Rep),
    load_sys_psm, 
    set_prism_flags_from_options, 
    get_dataset(DataSetId, Data), 
    initialize_sw_a, 
    learn(Data), 
    save_switch_alphas(TrialId).

%% run all trials
run_trials(TrialIds) :-
    set_prism_flag(clean_table, on), 
    foreach(trialId(DataSetId, I) in TrialIds, [],
            (run_trial(DataSetId, I), 
             set_prism_flag(clean_table, off))).

%% get successor distribution for a single number
run_successor_distribution(I, Distr) :- 
    get_option(n_viterbi_bound, N), 
    get_option(max_word_length, MaxWordLength),
    number(I, Word), 
    successor_distribution(Word, Distr, N, MaxWordLength). 
    
run_transition_probability(I, Distr, P) :- 
    I1 is I + 1, 
    number(I1, Succ),
    findall(P, member(Succ\P, Distr), Ps), 
    P is sum(Ps).
    

%% get count list transition probability
run_trial_analysis(TrialId) :- 
    get_option(lo, Lo), 
    get_option(hi, Hi), 
    get_option(prune_threshold, Thresh), 
    load_sys_psm, 
    set_prism_flags_from_options,
    restore_switch_alphas(TrialId), 
    pruneAll(Thresh),
    foreach(I in Lo..Hi, [Distr, Trans],
            (run_successor_distribution(I, Distr), 
             run_transition_probability(I, Distr, Trans),
             save_analysis_data(TrialId, I\Distr, I\Trans))
            ).

run_trial_analyses(TrialIds) :- 
    set_prism_flag(clean_table, on),
    foreach(TrialId in TrialIds, 
            (run_trial_analysis(TrialId), 
             set_prism_flag(clean_table, off))).

%% run whole thing
run_simulation(TrialIds) :- 
    init_simulation, 
    run_trials(TrialIds), 
    run_trial_analyses(TrialIds).

run_simulation :- 
    gen_trial_ids(TrialIds), 
    run_simulation(TrialIds).    

init_simulation :- 
    set_prism_flags_from_options,
    retractall(saved_switch_alphas(_, _)), 
    retractall(saved_analysis_data(_, _, _)).


save_switch_alphas(TrialId) :- 
    findall(Sw\Alpha, get_sw_a(Sw, _, _, Alpha), SwitchInfo), 
    asserta(saved_switch_alpha(TrialId, SwitchInfo)), 
    get_option(save_directory, SaveDir), 
    atom_concats([SaveDir, '/', 'switch_alpha_data'], Path), 
    findall(switch_alpha(T, Info), 
            saved_switch_alpha(T, Info), 
            Clauses), 
    save_clauses(Path, Clauses, []). 

save_analysis_data(TrialId, Distr, TransitionProbs) :-   
    asserta(saved_analysis_data(TrialId, Distr, TransitionProbs)), 
    get_option(save_directory, SaveDir), 
    atom_concats([SaveDir, '/', 'analysis_data'], Path), 
    findall(analysis_data(T, D, P), 
            saved_analysis_data(T, D, P), 
            Clauses), 
    save_clauses(Path, Clauses, []). 


restore_switch_alphas(TrialId) :- 
    saved_switch_alpha(TrialId, SwitchInfo), 
    foreach(Sw\Alpha in SwitchInfo, [], 
            set_sw_a(Sw, Alpha)).
    

initialize_sw_a :- 
    get_option(default_sw_a, Mu), 
    get_option(sw_a_std, Sigma), 
    init_sw_a(Mu, Sigma).

%% dataset utils
get_dataset(DataSetId, Data) :- 
    dataset(DataSetId, Data), !.

set_dataset(DataSetId, Data) :- 
    retractall(dataset(DataSetId, _)), 
    ?? assert(dataset(DataSetId, Data)).





%% ---------------------------
%% Option Defaults
get_option_default(max_word_length, V) :- 
    (get_option(language, english), !, V = 2);
    (get_option(langauge, chinese), !, V = 3).


%% ---------------------------
%% Utilities
set_option(Option, Value) :- 
    atom(Option), !, 
    T0 = option(Option, X),
    retractall(T0), 
    T1 = option(Option, Value),
    assert(T1).
set_option(Option, _) :- 
    Err = invalid_option(option(Option)), 
    throw(Err).
get_option(Option, Value) :- 
    option(Option, V),
    (V=default -> get_option_default(Option, Value);
                  Value = V).
    
show_options:- 
    option(Option, Value), 
    format("~w: ~w\n", [Option, Value]), 
    fail.
show_options:- true.
    
