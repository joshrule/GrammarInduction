%% util.py
%% Author: Eyal Dechter 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gijoe_root(DIR) :- 
    expand_environment('$GIJOE_ROOT', DIR).
gijoe_bin(DIR) :- 
    gijoe_root(B), 
    atom_concat(B, '/bin', DIR).
prismify(SysFile, PsmFile) :- 
    gijoe_bin(BINDIR), 
    write(BINDIR), nl,
    formatAtom("~a/prismify ~a ~a", [BINDIR, SysFile, PsmFile], A),
    format("Prismifying LPN ~a ... ~1n", [SysFile]),
    system(A),
    format("Done.", []).

%% load_local(PATH): load from ROOT/prism/...
load_local(REL) :- load_local([], REL).
load_local(OPTIONS, REL) :- 
    gijoe_root(ROOT), 
    formatAtom("~a/prism/~a", [ROOT, REL], PATH), 
    cl(OPTIONS, PATH).
                   

% load LPN .sys file at path. By default, load precompiled code found at
% <path>.out. 
load_lpn(Path) :- 
    load_lpn([load], Path).

% load LPN path with supplied options. Options are the same as the
% those to prism(options, path).
load_lpn(Options, Path) :-
    format("Loading LPN in file ~a...~1n", [Path]),
    format("Options: ~w~1n", [Options]),
    prism(Options, Path),
    format("Done").


:- dynamic lpn_value_pred/2. 
lpn_value_pred(Switch, _) :- throw(lpn_value_pred_not_found(Switch)).
assert_lpn_value_pred(Switch, Pred) :- 
    asserta(lpn_value_pred(Switch, Pred)).

%% lpn_set_sw_... 
%% change the values, probs, and alpha values of lpn switch
lpn_set_sw_v(Switch, Vs) :- 
    lpn_value_pred(Switch, P), 
    RetractTerm =.. [P, K], 
    AssertTerm =.. [P, Vs], 
    retract(RetractTerm), 
    assert(AssertTerm).

lpn_set_sw_a(Switch, As) :- 
    set_sw_a(Switch, As).

lpn_set_sw_vp(Switch, Vs, Ps) :- 
    lpn_set_sw_v(Switch, Vs), 
    normalize(Ps, Ps1),
    set_sw(Switch, Ps1).

lpn_set_sw_va(Switch, Vs, As) :- 
    lpn_set_sw_v(Switch, Vs), 
    set_sw_a(Switch, As).

lpn_set_sw_vpa(Switch, Vs, Ps, As) :- 
    lpn_set_sw_v(Switch, Vs), 
    set_sw_p(Switch, Ps),
    set_sw_a(Switch, As).

%%%%%%%%%%%%%%%%%%%%

:- dynamic lpn_data/1. 
set_lpn_data(Ds) :- 
    retractall(lpn_data), 
    assert(lpn_data(Ds)).

get_lpn_data(Ds) :- 
    lpn_data(Ds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

formatString(Format, Ls, Codes) :- 
    open('/tmp/stream', write, SOut),
    format(SOut, Format, Ls),
    flush_output(SOut),
    close(SOut),
    readFile('/tmp/stream', Codes).



    
formatAtom(Format, Ls, A) :- 
    open('/tmp/stream', write, SOut),
    format(SOut, "term('", []), 
    format(SOut, Format, Ls),
    format(SOut, "').", []),
    flush_output(SOut),
    close(SOut),
    %% readFile('/tmp/stream', In),
    open('/tmp/stream', read, SIn),
    read(SIn, In), 
    %% close(SIn),
    %% parse_string(A), 
    (In == end_of_file -> close(SIn), 
                          open('/tmp/stream', read, SIn1),
                          read(SIn1, term(A)),
                          close(SIn1);
     In=term(A), close(SIn)).



atom_concats([], '') :- !.
atom_concats([A|Rest], Out) :- atom_concats(Rest, B), 
                               atom_concat(A, B, Out).

number_atom(N,A) :- number_chars(N, Cs), atom_concats(Cs, A).

concat([], []). 
concat([X|Xs], Y) :- concat(Xs, Y1), append(X, Y1, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
take(_, [], []) :- !. 
take(0, _, []) :- !.
take(N, [X|Xs], [X|Ys]) :- N1 is N - 1, 
                           take(N1, Xs, Ys). 


%%%%% Date
date(D) :- 
    system('date > /tmp/date', A), 
    sleep(100), 
    readFile('/tmp/date', D), atom_codes(T, D).

%%%%% random choices %%%%%%
% multiple independent choices from a list
random_selects(Values, N, Vs) :- 
    Vs @= [V : I in 1..N, [V],
           random_select(Values, V)].
random_selects(Values, Dist, N, Vs) :- 
    Vs @= [V : I in 1..N, [V],
           random_select(Values, Dist, V)].
    

normalize(Vs, Ps) :- 
    sumlist(Vs, Z), 
    Ps @= [P : V in Vs, [P], P is V/Z].

%% sampling from lpn given alphas
%% sample_lpn(X) :- 

%% sample from gamma distribution
random_gamma1(Alpha, Lambda, X) :- 
     Alpha > 1, !, 
     D is Alpha - 1/3, 
     C is 1/sqrt(9 * D), !, 
     random_gaussian(Z), 
     random_uniform(U), (
         V is (1 + C * Z) ** 3,
         log(U) > (0.5 * + D - D * V + D * log(V)),
         X is (D * V / Lambda); 
         random_gamma1(Alpha, Lambda, X)
    ), !.
random_gamma1(Alpha, Lambda, X) :- 
    Alpha1 is Alpha + 1, 
    random_gamma1(Alpha1, Lambda, X1), !, 
    random_uniform(R), 
    X is X1 * (R ** (1/Alpha)), !.

random_gamma(Alpha, Lambda, X) :- random_gamma1(Alpha, Lambda, X). 
random_gamma(Alpha, Lambda, N, Xs) :- 
    Xs @= [X:I in 1..N, [X], random_gamma(Alpha, Lambda, X)].

%% sample from dirichlet distribution
random_dirichlet(Alphas, Dirichlet) :- random_dirichlet(Alphas, 1, [Dirichlet]).
random_dirichlet(Alphas, N, Dirichlets) :-
    Dirichlets @= [Vs : I in 1..N, [Gammas, D, Vs, V, G, A], 
                   (Gammas @= [G:A in Alphas, [G], random_gamma(A, 1, G)], 
                    sumlist(Gammas, D), 
                    Vs @= [V: G in Gammas, [V], V is G/D])].

%% sample and set switch probabilities given the alpha values for the switch
sample_switch_probs_from_sw_a :- 
    get_reg_sw_list(Switches), 
    foreach(Sw in Switches, [_1, _2, As, Ps],
            (get_sw_a(Sw, [_1, _2, As]), 
             random_dirichlet(As, Ps), 
             set_sw(Sw, Ps))).

set_switch_probs_from_sw_a_mean :- 
    get_reg_sw_list(Switches), 
    foreach(Sw in Switches, [_1, _2, As, Ps],
            (get_sw_a(Sw, [_1, _2, As]), 
             normalize(As, Ps), 
             set_sw(Sw, Ps))).
               
                 
               
%%% help function for loading prism files
load_prism(PATH) :- 
    formatAtom("~w.out", [PATH], PATH_OUT), 
    (file_exists(PATH_OUT) -> prism([load], PATH); prism(PATH)).


    
    
    
    
