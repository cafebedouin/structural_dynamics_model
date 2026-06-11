% ============================================================================
% stage_d_controls.pl — OQ-92 Stage D two-sided controls (OQ-94 rows 1-3 gates)
% Run from prolog/:
%   swipl -g run_stage_d_controls -t halt ../audits/2026-06-10_oq92_step3_preregistration/stage_d_controls.pl
% Expected (pre-registered in PREREGISTRATION.md Q2 block):
%   scaffold gate: uncaptured-shaped -> scaffold; captured twin -> NOT scaffold
%   CI_Rope gate: live cert + asserted gain_flow -> certification OFF; retract -> ON
%   pure_coordination: captured -> not pure_coordination subtype
%   absent side at scale: the suite (run separately) stays green, corpus unchanged
% ============================================================================

:- [stack].

scaffold_shape(C) :-
    assertz(narrative_ontology:constraint_metric(C, extractiveness, 0.20)),
    assertz(narrative_ontology:constraint_metric(C, suppression_requirement, 0.10)),
    assertz(narrative_ontology:constraint_metric(C, theater_ratio, 0.10)),
    assertz(narrative_ontology:constraint_beneficiary(C, group_x)),      % -> coordination fn
    assertz(narrative_ontology:has_sunset_clause(C)),                    % -> temporality
    assertz(narrative_ontology:constraint_stakeholder(C, seat_a, beneficiary,
                                                      institutional, biographical, mobile, national)).

run_stage_d_controls :-
    % --- Scaffold gate, two-sided (constructed twins) ---
    scaffold_shape(scaf_uncap),
    scaffold_shape(scaf_cap),
    assertz(narrative_ontology:stakeholder_gain_flow(scaf_cap, seat_a)),
    cache_registry:clear_all_caches,
    Ctx = context(agent_power(institutional), time_horizon(biographical),
                  exit_options(mobile), spatial_scope(national)),
    drl_core:base_extractiveness(scaf_uncap, E1),
    constraint_indexing:extractiveness_for_agent_d(scaf_uncap, Ctx, 0.25, Chi1),
    drl_core:get_raw_suppression(scaf_uncap, S1),
    drl_core:classify_from_metrics(scaf_uncap, E1, Chi1, S1, Ctx, T1),
    format('SCAFFOLD-GATE uncaptured: type=~w (expect scaffold)~n', [T1]),
    drl_core:base_extractiveness(scaf_cap, E2),
    constraint_indexing:extractiveness_for_agent_d(scaf_cap, Ctx, 0.25, Chi2),
    drl_core:get_raw_suppression(scaf_cap, S2),
    drl_core:classify_from_metrics(scaf_cap, E2, Chi2, S2, Ctx, T2),
    format('SCAFFOLD-GATE captured:   type=~w (expect NOT scaffold)~n', [T2]),

    % --- pure_coordination subtype (row 3) on the same twins ---
    ( signature_detection:determine_pure_subtype(scaf_uncap, Sub1) -> true ; Sub1 = none ),
    ( signature_detection:determine_pure_subtype(scaf_cap,   Sub2) -> true ; Sub2 = none ),
    format('PURE-SUBTYPE uncaptured=~w (expect pure_coordination) captured=~w (expect NOT pure_coordination)~n', [Sub1, Sub2]),

    % --- CI_Rope gate: deterministic intervention on a LIVE certification ---
    corpus_loader:load_all_testsets,
    cache_registry:clear_all_caches,
    C = demographic_skill_mismatch,
    ( signature_detection:constraint_signature(C, Sig0) -> true ; Sig0 = none ),
    format('CIROPE-GATE baseline: ~w = ~w (expect coupling_invariant_rope)~n', [C, Sig0]),
    once(narrative_ontology:constraint_stakeholder(C, Seat, _, _, _, _, _)),
    assertz(narrative_ontology:stakeholder_gain_flow(C, Seat)),
    cache_registry:clear_all_caches,
    ( signature_detection:constraint_signature(C, Sig1) -> true ; Sig1 = none ),
    format('CIROPE-GATE captured (gain_flow -> ~w): ~w (expect NOT coupling_invariant_rope)~n', [Seat, Sig1]),
    retract(narrative_ontology:stakeholder_gain_flow(C, Seat)),
    cache_registry:clear_all_caches,
    ( signature_detection:constraint_signature(C, Sig2) -> true ; Sig2 = none ),
    format('CIROPE-GATE restored: ~w (expect coupling_invariant_rope again)~n', [Sig2]).
