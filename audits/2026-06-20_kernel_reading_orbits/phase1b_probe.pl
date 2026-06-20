% Phase 1b read-only probe — axiom-grounding-profile + kernel-obstruction orbit keys.
% Loads ONE twin (corpus_path overlaid via asserta — the silent-fork gotcha) and prints:
%   LOAD\t<N>
%   GROUND\t<C>\t<sorted-grounding-multiset-atom>     (one per corpus constraint with a UID)
%   OBSTRUCT\t<K>\t<status>                            (one per kernel)
%   CTRL_GROUND_FACTS\t<count>                         (non-vacuity control)
% Run:  swipl -g "twin(testsets_haiku), halt" -t "halt(1)" phase1b_probe.pl   (from prolog/)
:- initialization(true).

:- use_module(library(lists)).

twin(Dir) :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, Dir)),
    consult(stack),
    corpus_loader:load_all_testsets,
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    format("LOAD\t~w~n", [N]),
    % non-vacuity control: total cs_axiom_grounding facts
    aggregate_all(count, narrative_ontology:cs_axiom_grounding(_,_,_), GF),
    format("CTRL_GROUND_FACTS\t~w~n", [GF]),
    % per-reading axiom-grounding profile
    forall(corpus_loader:corpus_constraint(C),
           ( ( narrative_ontology:cs_story_uid(C, UID),
               findall(G, narrative_ontology:cs_axiom_grounding(UID,_,G), Gs0),
               Gs0 \== []
             ->  msort(Gs0, Gs), term_to_atom(Gs, Prof)
             ;   Prof = none ),
             format("GROUND\t~w\t~w~n", [C, Prof]) )),
    % per-kernel obstruction status
    findall(K, narrative_ontology:cs_kernel_id(_, K), Ks0),
    sort(Ks0, Ks),
    forall(member(K, Ks),
           ( ( cs_kernel_registry:cs_kernel_obstruction_status(K, St) -> true ; St = error ),
             format("OBSTRUCT\t~w\t~w~n", [K, St]) )).
