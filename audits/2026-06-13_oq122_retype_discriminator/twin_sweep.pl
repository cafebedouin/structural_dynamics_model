% Twin-corpus sweep for OQ-122. Run once per corpus via:
%   CORPUS_DIR=testsets_haiku swipl -q -g true -t halt twin_sweep.pl
%   CORPUS_DIR=testsets_flash swipl -q -g true -t halt twin_sweep.pl
% Overlay uses asserta (NOT assertz — default param(corpus_path,testsets) is first
% clause; plain assertz is silently ignored, loads the default 57). corpus_constraint
% count printed first = overlay-took-effect witness (must read 960, not 57).
:- initialization(main).
:- [stack].

nvic(C, N) :- findall(V, narrative_ontology:constraint_victim(C, V), L), sort(L,Ls), length(Ls,N).
mountain(C) :- narrative_ontology:constraint_claim(C, mountain).
fsm(C) :- signature_detection:false_summit_mountain(C, _).
eps(C,E) :- ( domain_priors:base_extractiveness(C,E) -> true ; E = na ).
supp(C,S) :- ( drl_core:get_raw_suppression(C,S0), number(S0) -> S=S0 ; S=na ).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)),
    asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets,
    findall(C, corpus_loader:corpus_constraint(C), Cs), length(Cs, NC),
    format("~n=== CORPUS=~w  loaded corpus_constraint count = ~w ===~n", [Dir, NC]),
    findall(C, (member(C,Cs), mountain(C)), Ms), length(Ms, NM),
    format("mountain-claimers: ~w~n", [NM]),
    findall(C, (member(C,Ms), fsm(C)), Fs), length(Fs, NF),
    findall(C, (member(C,Fs), nvic(C,V), V =:= 0), F0), length(F0, NF0),
    findall(C, (member(C,Fs), nvic(C,V), V > 0),  F1), length(F1, NF1),
    format("FSM fires on ~w mountain-claimers: vic=0 -> ~w (false-positive candidates), vic>0 -> ~w (swap RETAINS, remove FORFEITS)~n",
           [NF, NF0, NF1]),
    ( NF1 > 0
    -> format("  *** FSM-fires WITH victim (the decisive swap!=remove cases): ***~n"),
       forall(member(C,F1), (eps(C,E),supp(C,S),nvic(C,V),
              format("    ~w  eps=~w supp=~w vic=~w~n",[C,E,S,V])))
    ;  format("  (no FSM-fires-with-victim case in this corpus)~n") ),
    findall(C, (member(C,Ms), eps(C,E),number(E),E=<0.25, supp(C,S),number(S),S=<0.05, nvic(C,V),V>0), Adv),
    length(Adv, NAdv),
    format("ADVERSARIAL CELL {mountain, eps<=0.25, supp<=0.05, victim!=[]}: ~w member(s)~n",[NAdv]),
    forall(member(C,Adv), (eps(C,E),supp(C,S),nvic(C,V),
           format("    ~w  eps=~w supp=~w vic=~w~n",[C,E,S,V]))),
    halt.
main :- format("TWIN SWEEP FAILED~n"), halt(1).
