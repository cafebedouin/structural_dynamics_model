% OQ-128 — does Boltzmann compliance separate genuine physics from social-naturalization
% where the structural metrics (ac/su/re/ts) could NOT? (read-only, no engine change.)
%
% The delta-exactly-two control proved metric relaxation over-restores: radiative/actinide
% are metrically identical to price_formation__naturalist et al. The open question (operator,
% 2026-06-16): is there a NON-speculative diagnostic that adds the separation — specifically
% Boltzmann compliance (ACTIVE, already feeds constraint_signature; OQ-117 false-foundational
% gate)? If genuine physics is `compliant` and the social-naturalization twins are
% `non_compliant`, natural_law can be gated on observable structure with ZERO intent authoring.
%
% Reports, per corpus, every metric-NL-eligible mountain-claimer (passes ac/su/re/ts — the
% restoration-eligible set the metrics could not narrow) with its Boltzmann verdict.
:- initialization(main).
:- [stack].
:- use_module(cache_registry).
:- use_module(boltzmann_compliance).

mountain(C) :- narrative_ontology:constraint_claim(C, mountain).
nvic(C,N) :- findall(V, narrative_ontology:constraint_victim(C,V), L), sort(L,Ls), length(Ls,N).
fld(ac,AC) :- config:param(natural_law_collapse_min,M), number(AC), AC>=M.
fld(su,Su) :- config:param(natural_law_suppression_max,M), number(Su), Su=<M.
fld(re,Re) :- config:param(natural_law_resistance_max,M), number(Re), Re=<M.
metric_nl(C) :-
    signature_detection:get_constraint_profile(C, profile(AC,Su,Re,_,_,TS,_)),
    fld(ac,AC), fld(su,Su), fld(re,Re), TS==stable.

bverdict(C, V) :-
    ( catch(boltzmann_compliance:boltzmann_compliant(C, R), _, R = error) -> true ; R = failed ),
    ( R = compliant(_)         -> V = 'COMPLIANT'
    ; R = non_compliant(_,_)   -> V = 'non_compliant'
    ; R = inconclusive(_)      -> V = 'inconclusive'
    ; V = R ).

main :-
    getenv('CORPUS_DIR', Dir),
    retractall(config:param(corpus_path,_)), asserta(config:param(corpus_path, Dir)),
    corpus_loader:load_all_testsets, cache_registry:clear_all_caches,
    findall(C, corpus_loader:corpus_constraint(C), Cs0), sort(Cs0, Cs),
    findall(C, (member(C,Cs), mountain(C), metric_nl(C)), Ms0), sort(Ms0, Ms),
    length(Ms, N),
    format("~n==== ~w : Boltzmann verdict over ~w metric-NL-eligible mountain-claimers ====~n", [Dir, N]),
    forall(member(C, Ms),
        ( nvic(C,Vic), bverdict(C, V),
          format("  ~w~t~60|vic=~w  boltzmann=~w~n", [C, Vic, V]) )),
    % tally
    findall(1, (member(C,Ms), bverdict(C,'COMPLIANT')), Cmp), length(Cmp,NC),
    findall(1, (member(C,Ms), bverdict(C,'non_compliant')), Ncp), length(Ncp,NN),
    findall(1, (member(C,Ms), bverdict(C,'inconclusive')), Inc), length(Inc,NI),
    format("  -- tally: COMPLIANT=~w  non_compliant=~w  inconclusive=~w~n", [NC,NN,NI]),
    halt.
main :- format("BOLTZMANN SEPARATION PROBE FAILED~n"), halt(1).
