%% association list
%% an Association List is a list of key value pairs K\V. 

get_assoc([K\V|_], K, V) :- !.
get_assoc([_|Rest], K, V) :- get_assoc(Rest, K, V). 

get_assoc_default(Assoc, K, V, _) :- 
    get_assoc(Assoc, K, V), !.
get_assoc_default(_, _, Default, Default).

get_assocs(Assoc, Ks, Vs) :- 
    Vs @= [V: K in Ks, [V], get_assoc(Assoc, K, V)].

assoc_to_lists([], [], []). 
assoc_to_lists([K\V|Assoc], [K|Ks], [V|Vs]) :- 
    assoc_to_lists(Assoc, Ks, Vs).



set_assoc([], K, V, [K\V]).
set_assoc([K\_|AssocIn], K, V, [K\V|AssocIn]) :- !.
set_assoc([P|AssocIn], K, V, [P|AssocOut]) :- set_assoc(AssocIn, K, V, AssocOut).


assoc_keys([], []).
assoc_keys([K\_|Rest], [K|Ks]) :- assoc_keys(Rest, Ks).


write_assoc(Assoc) :- 
    member(K\V, Assoc), 
    format("~w: ~w\n", [K, V]), 
    fail.
write_assoc(Assoc) :- true.

