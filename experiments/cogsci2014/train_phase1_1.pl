:- expand_environment('$GIJOE_ROOT/prism/train.pl', P), 
   cl(P).

%% load .sys.psm file.
load_sys_psm :- expand_environment('$GIJOE_ROOT/experiments/cogsci2014/number_2_2_2_lpn.sys.psm', 
                                   P),
                prism(P).

%% load data set
%% this includes before and after sentences with 20 held out test sentences
:- expand_environment('$GIJOE_ROOT/experiments/cogsci2014/phase1_data.pl', P),
   cl(P).

:- load_sys_psm.
:- set_training_flags.
:- set_prism_flag(epsilon, 0.01). 
:- set_prism_flag(default_sw_a, uniform(0.01)).
:- set_prism_flag(std_ratio, 100.0). 
:- set_prism_flag(restart, 10).

:- findall(X, phase1(X), DataSet), 
   set_ovbem_rec(data_set, DataSet).

go_batch :- 
    get_ovbem_rec(data_set, Ds), 
    learn(Ds).

    
          
