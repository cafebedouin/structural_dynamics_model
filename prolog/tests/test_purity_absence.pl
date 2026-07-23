% ============================================================================
% test_purity_absence.pl — OQ-60 purity absence-reward regression tests.
%
% A constraint with no purity evidence must not score pristine: no-data is
% `unknown` (JSON null), distinct from the -1.0 epistemic-gate-fail sentinel
% (rulings R1-R4, audits/2026-07-17_oq60_purity_absence/).
%
% Layers:
%   (a) Preflight retro-witness for Commit 0a/0a.2 (both landed with only a
%       null-diff witness): a synthetic `unknown` injected at the EX subscore
%       (gotchas doc section 3 swap, three dispatch controls) must propagate
%       purity_score -> purity_zone -> JSON emitter -> effective_purity.
%   (b) Two-sided golden: alignment_constraint_narrowing scores exactly the
%       census value; a gate-fail constraint scores exactly -1.0 (precedence:
%       sentinel XOR unknown, never both).
%   (c) Token totality: purity_zone and the JSON writers handle `unknown`
%       without throwing.
%   Producer-commit units (C-LATENT mechanisms, C-FLOOR) are appended by their
%   own commits — each RED at the HEAD it lands on, GREEN with its fix.
%
%   (d) Ordering-boundary ingest (item-0 audit, ORDERING_AUDIT_2026-07-23.md):
%       atoms sort BEFORE numbers in the standard order, so an `unknown`
%       reaching msort/max_member-based stats would silently head the list.
%       The two cache boundaries that feed ordering predicates (drl_fpn
%       precompute, giant_component precompute) must collapse `unknown` to the
%       -1.0 sentinel their downstream `>= 0.0` filters already exclude.
%
% Run (needs the PIPELINE load chain — json_report/giant_component are not
% loaded by [stack]; tests fail loudly, not silently, if the chain is short):
%   cd prolog && swipl -l stack.pl -l covering_analysis.pl -l maxent_classifier.pl \
%     -l dirac_classification.pl -l diagnostic_summary.pl -l post_synthesis.pl \
%     -l json_report.pl -l giant_component_analysis.pl \
%     -g "[tests/test_purity_absence], run_tests(purity_absence), halt" -t "halt(1)"
% ============================================================================

:- corpus_loader:ensure_corpus_loaded.

% Injection target registry for the test-local EX-subscore swap.
:- dynamic oq60_inject_target/1.

% Original excess_extraction_subscore/2 body (purity_scoring.pl) — re-asserted
% on restore. If the source clause changes, update this copy or the post-restore
% golden check fails loudly.
oq60_original_ex_clause((purity_scoring:excess_extraction_subscore(C, EX) :-
    (   boltzmann_compliance:excess_extraction(C, Excess)
    ->  EX is max(0.0, 1.0 - min(1.0, Excess * 2.0))
    ;   EX = 1.0
    ))).

oq60_swap_in :-
    abolish(purity_scoring:excess_extraction_subscore/2),
    assertz((purity_scoring:excess_extraction_subscore(C, EX) :-
        user:oq60_inject_target(C), !, EX = unknown)),
    oq60_original_ex_clause(Orig),
    assertz(Orig),
    cache_registry:clear_all_caches.

oq60_swap_out :-
    abolish(purity_scoring:excess_extraction_subscore/2),
    oq60_original_ex_clause(Orig),
    assertz(Orig),
    retractall(user:oq60_inject_target(_)),
    cache_registry:clear_all_caches.

% Bare gate-passing constraint — the oq60_control_bare template from
% census_oq60.pl (3 authored classifications, no grid/coupling/extraction/
% coordination data).
oq60_assert_bare(C) :-
    retractall(constraint_indexing:constraint_classification(C, _, _)),
    Ctx1 = context(agent_power(powerless), time_horizon(generational), exit_options(trapped), spatial_scope(national)),
    Ctx2 = context(agent_power(moderate), time_horizon(generational), exit_options(mobile), spatial_scope(national)),
    Ctx3 = context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx1)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx2)),
    assertz(constraint_indexing:constraint_classification(C, snare, Ctx3)),
    cache_registry:clear_all_caches.

oq60_retract_bare(C) :-
    retractall(constraint_indexing:constraint_classification(C, _, _)),
    cache_registry:clear_all_caches.

:- begin_tests(purity_absence).

% ----------------------------------------------------------------------------
% (b) Two-sided golden + precedence
% ----------------------------------------------------------------------------

% Census value (census_testsets_2026-07-23.tsv, clean HEAD). Byte-identical
% pre/post the latent producer commits.
test(golden_scored_value) :-
    purity_scoring:purity_score(alignment_constraint_narrowing, P),
    P == 0.3541666666666667.

% Gate-fail precedence: -1.0 sentinel XOR unknown, never both, never null.
test(gate_fail_sentinel_precedence) :-
    purity_scoring:purity_score(actinide_replenishment_mechanism_contradictions, P),
    P == -1.0,
    P \== unknown.

% ----------------------------------------------------------------------------
% (c) Token totality — `unknown` never throws downstream
% ----------------------------------------------------------------------------

test(purity_zone_total_on_unknown) :-
    logical_fingerprint:purity_zone(unknown, Z),
    Z == unknown.

test(json_writers_total_on_unknown) :-
    with_output_to(string(N), json_report:write_json_number(current_output, unknown)),
    N == "null",
    with_output_to(string(S), json_report:write_json_string(current_output, null)),
    S == "null".

% ----------------------------------------------------------------------------
% (a) Preflight retro-witness: injected unknown end-to-end (0a + 0a.2 live)
% ----------------------------------------------------------------------------
% Values are CAPTURED, not hard-coded, so the unit survives the producer
% commits (post-C-LATENT a bare constraint's pre-swap purity is legitimately
% `unknown`; the golden constraint keeps numeric subscores either way).

test(injected_unknown_end_to_end, [
        setup(oq60_assert_bare(oq60_inject_bare)),
        cleanup(oq60_retract_bare(oq60_inject_bare))
    ]) :-
    Bare = oq60_inject_bare,
    Golden = alignment_constraint_narrowing,
    % pre dispatch control: original behavior captured
    purity_scoring:purity_score(Bare, PBare0),
    purity_scoring:purity_score(Golden, PGold0),
    assertz(user:oq60_inject_target(Bare)),
    assertz(user:oq60_inject_target(Golden)),
    % once/1 is LOAD-BEARING (gotchas doc section 2): sub_string/5 leaves
    % choicepoints, which would defer the cleanup (restore) past the post
    % checks below — witnessed 2026-07-23: without once/1 both post reads
    % returned `unknown` because the swap was still active.
    setup_call_cleanup(
        oq60_swap_in,
        once((
            % mid dispatch control: swap visible at the consumer's call site,
            % non-target untouched
            purity_scoring:excess_extraction_subscore(Bare, EXm), EXm == unknown,
            purity_scoring:excess_extraction_subscore(Golden, EXg), EXg == unknown,
            purity_scoring:excess_extraction_subscore(epistemic_collapse, EXo), number(EXo),
            % scalar: 0a propagation guard (purity_scoring.pl:54-55) fires
            purity_scoring:purity_score(Bare, P1), P1 == unknown,
            purity_scoring:purity_score(Golden, P2), P2 == unknown,
            % zone total
            logical_fingerprint:purity_zone(P2, Zi), Zi == unknown,
            % REAL assembled emitter: unknown serializes as null (engine half
            % of the token-serialization pin)
            constraint_indexing:default_context(MaxEntCtx),
            with_output_to(string(JSON),
                json_report:write_per_constraint_entry(current_output, Golden, false, MaxEntCtx)),
            sub_string(JSON, _, _, _, "\"purity_score\": null"),
            sub_string(JSON, _, _, _, "\"purity_band\": null"),
            % 0a.2 consumer guard: unknown intrinsic -> unknown effective
            drl_purity_network:effective_purity(Bare, EPb, _), EPb == unknown,
            drl_purity_network:effective_purity(Golden, EPg, _), EPg == unknown
        )),
        oq60_swap_out
    ),
    % post dispatch control: restore verified against the captured values
    purity_scoring:purity_score(Bare, PBare9), PBare9 == PBare0,
    purity_scoring:purity_score(Golden, PGold9), PGold9 == PGold0.

% ----------------------------------------------------------------------------
% (d) Ordering-boundary ingest: unknown collapses to -1.0 BEFORE any sort
% ----------------------------------------------------------------------------

test(fpn_ingest_collapses_unknown_to_sentinel, [
        setup(oq60_assert_bare(oq60_inject_bare)),
        cleanup(oq60_retract_bare(oq60_inject_bare))
    ]) :-
    assertz(user:oq60_inject_target(oq60_inject_bare)),
    setup_call_cleanup(
        oq60_swap_in,
        once((
            purity_scoring:purity_score(oq60_inject_bare, P), P == unknown,
            constraint_indexing:default_context(Ctx),
            drl_fpn:fpn_precompute_constraints([oq60_inject_bare], Ctx),
            drl_fpn:fpn_intrinsic(oq60_inject_bare, IP),
            IP == -1.0
        )),
        oq60_swap_out
    ).

test(gc_ingest_collapses_unknown_to_sentinel, [
        setup(oq60_assert_bare(oq60_inject_bare)),
        cleanup((
            oq60_retract_bare(oq60_inject_bare),
            retractall(giant_component_analysis:gc_node_purity(oq60_inject_bare, _, _)),
            retractall(giant_component_analysis:gc_node_type(oq60_inject_bare, _, _))
        ))
    ]) :-
    retractall(giant_component_analysis:gc_node_purity(oq60_inject_bare, _, _)),
    assertz(user:oq60_inject_target(oq60_inject_bare)),
    setup_call_cleanup(
        oq60_swap_in,
        once((
            purity_scoring:purity_score(oq60_inject_bare, P), P == unknown,
            constraint_indexing:default_context(Ctx),
            giant_component_analysis:precompute_props_loop([oq60_inject_bare], Ctx, 0, 1),
            giant_component_analysis:gc_node_purity(oq60_inject_bare, IP, EP),
            IP == -1.0,
            EP == -1.0,
            % and the downstream distribution filter excludes the sentinel
            findall(V, ( member(V, [IP, EP]), V >= 0.0 ), Kept),
            Kept == []
        )),
        oq60_swap_out
    ).

:- end_tests(purity_absence).
