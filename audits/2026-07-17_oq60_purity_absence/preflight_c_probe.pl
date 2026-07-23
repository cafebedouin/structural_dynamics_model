% Preflight (c) scratch probe — injected `unknown` subscore end-to-end.
% Run: cd prolog && swipl -g "[stack], corpus_loader:ensure_corpus_loaded, consult('<this>'), run_probe, halt" -t "halt(1)"

:- dynamic oq60_inject_target/1.

original_ex_clause((purity_scoring:excess_extraction_subscore(C, EX) :-
    (   boltzmann_compliance:excess_extraction(C, Excess)
    ->  EX is max(0.0, 1.0 - min(1.0, Excess * 2.0))
    ;   EX = 1.0
    ))).

swap_in :-
    abolish(purity_scoring:excess_extraction_subscore/2),
    assertz((purity_scoring:excess_extraction_subscore(C, EX) :-
        user:oq60_inject_target(C), !, EX = unknown)),
    original_ex_clause(Orig),
    assertz(Orig),
    cache_registry:clear_all_caches.

swap_out :-
    abolish(purity_scoring:excess_extraction_subscore/2),
    original_ex_clause(Orig),
    assertz(Orig),
    cache_registry:clear_all_caches.

% bare synthetic constraint per census_oq60.pl positive-control template
assert_bare(C) :-
    retractall(constraint_indexing:constraint_classification(C, _, _)),
    Ctx1 = context(agent_power(powerless), time_horizon(generational), exit_options(trapped), spatial_scope(national)),
    Ctx2 = context(agent_power(moderate), time_horizon(generational), exit_options(mobile), spatial_scope(national)),
    Ctx3 = context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx1)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx2)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx3)),
    cache_registry:clear_all_caches.

retract_bare(C) :-
    retractall(constraint_indexing:constraint_classification(C, _, _)),
    cache_registry:clear_all_caches.

run_probe :-
    Bare = oq60_inject_bare,
    Golden = alignment_constraint_narrowing,
    assert_bare(Bare),
    % --- PRE controls (original predicate) ---
    purity_scoring:purity_score(Bare, PBare0),
    purity_scoring:purity_score(Golden, PGold0),
    purity_scoring:excess_extraction_subscore(Bare, EX0),
    format(user_error, '[pre ] bare purity=~w (EX=~w)  golden purity=~w~n', [PBare0, EX0, PGold0]),
    % --- SWAP: inject unknown for BOTH targets ---
    assertz(oq60_inject_target(Bare)),
    assertz(oq60_inject_target(Golden)),
    swap_in,
    % mid dispatch control: visible at the consumer's call site
    purity_scoring:excess_extraction_subscore(Bare, EXm),
    purity_scoring:excess_extraction_subscore(Golden, EXg),
    format(user_error, '[mid ] EX subscore: bare=~w golden=~w (expect unknown/unknown)~n', [EXm, EXg]),
    % non-target still original (fallback clause dispatches)
    purity_scoring:excess_extraction_subscore(epistemic_collapse, EXo),
    format(user_error, '[mid ] non-target epistemic_collapse EX=~w (expect number)~n', [EXo]),
    % 1. scalar
    purity_scoring:purity_score(Bare, P1), purity_scoring:purity_score(Golden, P2),
    format(user_error, '[step1] purity_score: bare=~w golden=~w (expect unknown/unknown)~n', [P1, P2]),
    % 2. zone
    logical_fingerprint:purity_zone(P2, Z),
    format(user_error, '[step2] purity_zone(~w) = ~w (expect unknown)~n', [P2, Z]),
    % 3. JSON emitter — REAL assembled per-constraint entry for the golden constraint
    constraint_indexing:default_context(MaxEntCtx),
    with_output_to(string(JSON),
        json_report:write_per_constraint_entry(current_output, Golden, false, MaxEntCtx)),
    (   sub_string(JSON, _, _, _, "\"purity_score\": null")
    ->  format(user_error, '[step3] emitter: "purity_score": null PRESENT~n', [])
    ;   format(user_error, '[step3] *** emitter purity_score NOT null~n', [])
    ),
    (   sub_string(JSON, Ix, _, _, "\"purity_score\""),
        Len is 60, sub_string(JSON, Ix, Len, _, Frag)
    ->  format(user_error, '[step3] emitter fragment: ~w~n', [Frag])
    ;   true
    ),
    % 4. effective_purity (bare: no-neighbor branch; golden: real neighbor path)
    drl_purity_network:effective_purity(Bare, EPb, CompB),
    drl_purity_network:effective_purity(Golden, EPg, _),
    format(user_error, '[step4] effective_purity: bare=~w (~w) golden=~w (expect unknown/unknown)~n', [EPb, CompB, EPg]),
    % --- RESTORE + post controls ---
    swap_out,
    retractall(oq60_inject_target(_)),
    purity_scoring:purity_score(Bare, PBare9),
    purity_scoring:purity_score(Golden, PGold9),
    format(user_error, '[post] bare purity=~w golden purity=~w (expect ~w / ~w)~n', [PBare9, PGold9, PBare0, PGold0]),
    (   PBare9 == PBare0, PGold9 == PGold0
    ->  format(user_error, '[post] RESTORE VERIFIED~n', [])
    ;   format(user_error, '[post] *** RESTORE MISMATCH~n', [])
    ),
    retract_bare(Bare).
