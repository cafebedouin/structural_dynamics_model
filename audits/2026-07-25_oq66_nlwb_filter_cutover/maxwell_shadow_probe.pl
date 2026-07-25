% maxwell_shadow_probe.pl — OQ-66 step 2 rider: the maxwell measurement.
%
% kernel_v1's sole raw-vs-filtered divergence candidate is
% maxwell_demon_impossibility — the reference constraint both gate-two items
% were declared metric-identical against. The MaxEnt arm of the original
% tripwire was never actually measured (unfitted model -> every read failed ->
% no_top compared against no_top). This probe is the FIRST read of that
% constraint under a properly-recomputed model, and its output is the opening
% datum for the shadow-separability OQ.
%
% Usage: swipl -q -l stack.pl -l maxwell_shadow_probe.pl -g "run, halt" -t 'halt(1)'

run :-
    retractall(config:param(corpus_path, _)),
    asserta(config:param(corpus_path, 'archives/datasets/kernel_v1')),
    corpus_loader:load_all_testsets,
    constraint_indexing:site_contexts_canonical(Ctxs),
    cache_registry:clear_all_caches,
    maxent_classifier:maxent_cleanup,
    maxent_classifier:maxent_multi_run(Ctxs, _),
    aggregate_all(count, maxent_classifier:maxent_dist(_, _, _), ND),
    ( ND > 0 -> true ; throw(maxent_unfitted) ),
    format("MAXWELL maxent_dist_facts=~w (non-vacuity control)~n", [ND]),
    C = maxwell_demon_impossibility,
    ( drl_core:natural_law_without_beneficiary(C) -> R = true ; R = false ),
    format("MAXWELL raw_nlwb=~w~n", [R]),
    findall(B, narrative_ontology:constraint_beneficiary(C, B), Bs),
    findall(A, narrative_ontology:agent_beneficiary(C, A), As),
    format("MAXWELL beneficiaries=~w agent_beneficiaries=~w~n", [Bs, As]),
    ( signature_detection:constraint_signature(C, Sig) -> true ; Sig = none ),
    format("MAXWELL signature=~w~n", [Sig]),
    forall(member(Cx, Ctxs),
           ( ( drl_core:dr_type(C, Cx, T) -> true ; T = no_type ),
             ( maxent_classifier:maxent_top_type(C, Cx, M) -> true ; M = no_top ),
             ( maxent_classifier:maxent_entropy(C, Cx, H) -> true ; H = no_entropy ),
             ( maxent_classifier:maxent_dist(C, Cx, D) -> true ; D = no_dist ),
             format("MAXWELL ctx=~w dr_type=~w shadow_top=~w entropy=~w~n", [Cx, T, M, H]),
             format("MAXWELL   dist=~w~n", [D]) )).
