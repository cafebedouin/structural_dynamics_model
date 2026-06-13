% OQ-122 discriminating probe: is the RED cap on radiative_levitation driven by
% the type-CLAIM (Web C's re-type test) or by agent-beneficiary presence vs extraction?
:- initialization(main).

:- [stack].
:- use_module(probe_harness).
:- use_module(cache_registry).

ctx_power(context(agent_power(P), _, _, _), P).

report_dr_types(C) :-
    forall(drl_core:standard_context(Ctx),
        ( ctx_power(Ctx, P),
          ( catch(drl_core:dr_type(C, Ctx, T), E, (T = throw(E))) -> true ; T = '<fail>' ),
          format("    dr_type @ ~w = ~w~n", [P, T]) )).

t1_mismatch(C, Ctx) :- drl_core:dr_claim_mismatch(C, Ctx, type_1_false_summit, _).

report_type1(C) :-
    (   setof(Ctx, t1_mismatch(C, Ctx), Ms)
    ->  length(Ms, N), format("    type_1_false_summit FIRES at ~w context(s)~n", [N])
    ;   format("    type_1_false_summit does NOT fire~n")
    ).

report_claim(C) :-
    findall(K, narrative_ontology:constraint_claim(C, K), Ks),
    format("    constraint_claim = ~w~n", [Ks]).

report_sig(C) :-
    ( signature_detection:constraint_signature(C, S) -> true ; S = none ),
    format("    constraint_signature = ~w~n", [S]).

report_benef(C) :-
    findall(B, narrative_ontology:agent_beneficiary(C, B), Bs),
    format("    agent_beneficiaries = ~w~n", [Bs]).

main :-
    corpus_loader:ensure_corpus_loaded,
    C = radiative_levitation_stratification,
    cache_registry:clear_all_caches,

    format("~n================ BASELINE (probe positive control) ================~n"),
    ( drl_core:base_extractiveness(C, Eps) -> true ; Eps = none ),
    config:param(mountain_extractiveness_max, MaxX),
    format("    base_extractiveness = ~w   (mountain_extractiveness_max = ~w)~n", [Eps, MaxX]),
    report_claim(C), report_benef(C), report_sig(C),
    report_dr_types(C), report_type1(C),

    format("~n========= INTERVENTION A: retract beneficiaries, claim STILL mountain =========~n"),
    cache_registry:clear_all_caches,
    probe_harness:with_retracted(
        [ narrative_ontology:constraint_beneficiary(C, _) ],
        ( report_claim(C), report_benef(C), report_sig(C),
          report_dr_types(C), report_type1(C) )),

    format("~n========= INTERVENTION B (Web C): re-type claim mountain -> tangled_rope =========~n"),
    cache_registry:clear_all_caches,
    probe_harness:with_overlay(
        [ narrative_ontology:constraint_claim(C, mountain) ],
        [ narrative_ontology:constraint_claim(C, tangled_rope) ],
        ( report_claim(C), report_benef(C), report_sig(C),
          report_dr_types(C), report_type1(C) )),

    format("~n========= POST-RESTORE SANITY (caches cleared, baseline must return) =========~n"),
    cache_registry:clear_all_caches,
    report_claim(C), report_type1(C),
    halt.
main :- format("PROBE FAILED~n"), halt(1).
