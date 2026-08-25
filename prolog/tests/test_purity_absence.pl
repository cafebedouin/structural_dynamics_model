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
    ;   EX = unknown
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

% ============================================================================
% OQ-356 — the THIRD ingest of effective_purity/4 in giant_component_analysis.
%
% SITED next to gc_ingest_collapses_unknown_to_sentinel below: same value
% (`unknown` out of purity_scoring), same hazard (arithmetic on an atom), same
% module, two INGESTS. That test pins the guarded one (precompute_props_loop/4,
% giant_component_analysis.pl:362-369). These pin the one OQ-60 missed, at
% :1278 inside count_by_action_band/8, which killed the whole Phase-3
% contamination block on 17 of 20 corpora.
%
% WHY THE INJECTION TARGET IS excess_extraction_subscore/2 AND NOT
% purity_score/2 OR effective_purity/4: this file's swap idiom (abolish +
% re-assert a COPY of the original clause) forks whatever it targets. The EX
% subscore's copy already exists above — oq60_original_ex_clause/1, 5 lines.
% Targeting purity_score/2 or effective_purity/4 would copy 18 or 25 lines of
% engine logic into a test file (including effective_purity's
% CONVERGENCE-CRITICAL comment), a canonical fork with a larger blast radius
% than the thing it would witness. Every population these tests need is
% reachable through the existing 5-line target.
%
% THREE-WAY DISPATCH, and the three cases are the point. The guard's rejecting
% conjunction is `catch(effective_purity(...), _, fail), number(EP), EP >= 0.0`,
% and it drops THREE silently different populations:
%   (a) effective_purity SUCCEEDS with a non-number  -> the OQ-60 defect class
%   (b) effective_purity THROWS                      -> dropped by catch/3
%   (c) effective_purity FAILS                       -> the conjunct fails
% A hand-written "count the unknowns" gets (b) and (c) wrong, and the resulting
% conservation identity then breaks as a FALSE ALARM attributed to the guard.
% Asserting that all three land in NExcluded is the two-sided half of that.
% ============================================================================

:- dynamic oq356_unknown_target/1.
:- dynamic oq356_throw_target/1.
:- dynamic oq356_fail_target/1.

oq356_swap_in :-
    abolish(purity_scoring:excess_extraction_subscore/2),
    assertz((purity_scoring:excess_extraction_subscore(C, EX) :-
        user:oq356_unknown_target(C), !, EX = unknown)),
    assertz((purity_scoring:excess_extraction_subscore(C, _) :-
        user:oq356_throw_target(C), !, throw(oq356_synthetic_producer_error))),
    assertz((purity_scoring:excess_extraction_subscore(C, _) :-
        user:oq356_fail_target(C), !, fail)),
    oq60_original_ex_clause(Orig),
    assertz(Orig),
    cache_registry:clear_all_caches.

oq356_swap_out :-
    abolish(purity_scoring:excess_extraction_subscore/2),
    oq60_original_ex_clause(Orig),
    assertz(Orig),
    retractall(user:oq356_unknown_target(_)),
    retractall(user:oq356_throw_target(_)),
    retractall(user:oq356_fail_target(_)),
    cache_registry:clear_all_caches.

% The three synthetic members, one per exclusion cause. Each needs the bare
% gate-passing template so purity_score/2's clause 1 (and therefore the EX
% subscore) is actually reached rather than short-circuiting to the -1.0
% epistemic-gate-fail sentinel.
oq356_fixture_in :-
    oq60_assert_bare(oq356_member_unknown),
    oq60_assert_bare(oq356_member_throw),
    oq60_assert_bare(oq356_member_fail),
    assertz(user:oq356_unknown_target(oq356_member_unknown)),
    assertz(user:oq356_throw_target(oq356_member_throw)),
    assertz(user:oq356_fail_target(oq356_member_fail)),
    oq356_swap_in.

oq356_fixture_out :-
    oq356_swap_out,
    oq60_retract_bare(oq356_member_unknown),
    oq60_retract_bare(oq356_member_throw),
    oq60_retract_bare(oq356_member_fail).

% N real corpus members whose effective purity is a number >= 0.0, i.e. members
% the guard must KEEP. Selected by PROPERTY, not by name: a name-pinned list
% rots as the live leg grows (`prolog/testsets/` carries no count by ruling),
% and the point of these members is only that they are real and scorable.
oq356_scorable_members(N, Ms) :-
    constraint_indexing:default_context(Ctx),
    findall(C, ( corpus_loader:corpus_constraint(C),
                 catch(drl_purity_network:effective_purity(C, Ctx, EP, _), _, fail),
                 number(EP), EP >= 0.0 ),
            All),
    length(All, NAll),
    (   NAll >= N
    ->  length(Ms, N), append(Ms, _, All)
    ;   throw(error(oq356_fixture_too_small(NAll, N),
                    'the live corpus has too few scorable members for this fixture'))
    ).

% The fixture member list: 4 real scorable members + the three synthetic
% exclusion causes. |Members| = 7, NKept = 4, NExcluded = 3.
oq356_fixture_members(Ms) :-
    oq356_scorable_members(4, Keep),
    append(Keep, [oq356_member_unknown, oq356_member_throw, oq356_member_fail], Ms).


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
        setup((
            oq60_assert_bare(oq60_inject_bare),
            % synthetic SCORED control (corpus-independent): authored
            % extractiveness + coordination_type ⇒ EX computes numerically.
            % (Originally used corpus row epistemic_collapse — an m5 flip row,
            % legitimately unknown post-C-FLOOR; witnessed 2026-07-23.)
            oq60_assert_bare(oq60_scored_ctl),
            assertz(narrative_ontology:constraint_metric(oq60_scored_ctl, extractiveness, 0.5)),
            assertz(narrative_ontology:coordination_type(oq60_scored_ctl, information_standard)),
            cache_registry:clear_all_caches
        )),
        cleanup((
            oq60_retract_bare(oq60_inject_bare),
            retractall(narrative_ontology:constraint_metric(oq60_scored_ctl, extractiveness, _)),
            retractall(narrative_ontology:coordination_type(oq60_scored_ctl, _)),
            oq60_retract_bare(oq60_scored_ctl)
        ))
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
            purity_scoring:excess_extraction_subscore(oq60_scored_ctl, EXo), number(EXo),
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

% ----------------------------------------------------------------------------
% (e) OQ-356 — count_by_action_band's purity guard
% ----------------------------------------------------------------------------

% THE THROW WITNESS. RED at pre-fix HEAD with
%   type_error(evaluable, unknown/0)
% raised by the bare `EP >= 0.0` at giant_component_analysis.pl:1278 — the
% catch/3 one line above intercepts NOTHING, because `unknown` is a RETURN
% VALUE (OQ-60 path 0a), not an exception. GREEN once number(EP) lands ahead of
% the comparison.
%
% CHECK THE ERROR TERM, NOT THE COLOUR: an existence_error here would mean the
% test was written against a predicate arity that does not exist yet, and the
% reading would be worthless.
test(oq356_count_by_action_band_survives_unknown_purity, [
        setup(oq356_fixture_in),
        cleanup(oq356_fixture_out)
    ]) :-
    constraint_indexing:default_context(Ctx),
    config:param(purity_action_sound_floor, SF),
    config:param(purity_action_degraded_floor, DF),
    oq356_fixture_members(Ms),
    giant_component_analysis:count_by_action_band(Ms, Ctx, SF, DF, NS, NB, NW, ND, _, _),
    integer(NS), integer(NB), integer(NW), integer(ND).

% CRITERION 1 — THE HAND-COMPUTED ROW, at the two edges where a hand
% computation and the code most easily disagree.
%
% count_in_zone/4 -> in_float_range/3 is HALF-OPEN [Lo, Hi), so a member at
% exactly the sound floor lands in Sound (not Borderline) and a member at
% exactly the degraded floor lands in Warning (not Degraded). Both sides are
% asserted: it is not enough that the value lands where expected, it must also
% NOT land in the neighbouring band. Floors are read from config:param/2 at test
% time, never hardcoded — config.pl is the single source of truth and these are
% tunable.
%
% This is a literal, drift-proof hand computation. Deviation from the plan,
% stated rather than absorbed (WRITEUP.md "deviation D1"): the plan sited the
% hand-computed row inside a count_by_action_band call over members with
% injected exact purities. Pinning exact effective purities through the real
% chain would require abolishing and re-copying purity_score/2's or
% effective_purity/4's body into this file — a canonical fork with a larger
% blast radius than the thing it would witness. The row is therefore computed
% over a literal EP list through the same count_in_zone/4 the predicate uses,
% and the guard/partition half is carried by the tests below on the real
% predicate. Nothing the plan asked the row to establish is dropped; only the
% injection point differs.
test(oq356_band_edges_are_half_open) :-
    config:param(purity_action_sound_floor, SF),
    config:param(purity_action_escalation_floor, EF),
    config:param(purity_action_degraded_floor, DF),
    % exactly the sound floor -> Sound, and NOT Borderline
    giant_component_analysis:count_in_zone([SF], SF, 1.01, S1), S1 =:= 1,
    giant_component_analysis:count_in_zone([SF], EF, SF,   S2), S2 =:= 0,
    % exactly the degraded floor -> Warning, and NOT Degraded
    giant_component_analysis:count_in_zone([DF], DF, EF,    W1), W1 =:= 1,
    giant_component_analysis:count_in_zone([DF], -0.01, DF, W2), W2 =:= 0.

% CRITERION 1 (cont.) — the full hand-computed row over a literal EP list.
% Hand computation, from config.pl:467-469 + the literal 1.01/-0.01 bounds:
%   0.70 -> Sound      [0.70, 1.01)   (exactly the floor; half-open at Lo)
%   1.00 -> Sound      [0.70, 1.01)
%   0.55 -> Borderline [0.50, 0.70)
%   0.30 -> Warning    [0.30, 0.50)   (exactly the floor)
%   0.05 -> Degraded   [-0.01, 0.30)
%   0.00 -> Degraded   [-0.01, 0.30)  (exactly 0.0 is a real score, bands worst)
% => NS=2, NB=1, NW=1, ND=2, and the four bands are TOTAL over the list (6).
test(oq356_hand_computed_band_row) :-
    config:param(purity_action_sound_floor, SF),
    config:param(purity_action_escalation_floor, EF),
    config:param(purity_action_degraded_floor, DF),
    EPs = [0.70, 1.00, 0.55, 0.30, 0.05, 0.00],
    giant_component_analysis:count_in_zone(EPs, SF, 1.01, NS), NS =:= 2,
    giant_component_analysis:count_in_zone(EPs, EF, SF,   NB), NB =:= 1,
    giant_component_analysis:count_in_zone(EPs, DF, EF,   NW), NW =:= 1,
    giant_component_analysis:count_in_zone(EPs, -0.01, DF, ND), ND =:= 2,
    Total is NS + NB + NW + ND,
    length(EPs, Total).

% CRITERION 2 — CONSERVATION, AS TWO IDENTITIES, with a NON-ZERO subtrahend.
%   (1) NS + NB + NW + ND == NKept        band coverage of the filtered domain
%   (2) NKept + NExcluded == |Members|    the guard's partition is TOTAL
% Identity (2) is only meaningful because NKept is accumulated independently of
% NExcluded in partition_scorable_purity/4. Were NKept derived as
% |Members| - NExcluded, (2) would be true by construction and would test
% nothing. As written it fires if a member falls through both branches or is
% counted twice.
% Two-sided: it fails if the guard drops numeric members as well as if it keeps
% atoms; and a band-coverage bug fails (1) rather than masquerading as a guard
% bug.
test(oq356_conservation_two_identities, [
        setup(oq356_fixture_in),
        cleanup(oq356_fixture_out)
    ]) :-
    constraint_indexing:default_context(Ctx),
    config:param(purity_action_sound_floor, SF),
    config:param(purity_action_degraded_floor, DF),
    oq356_fixture_members(Ms),
    length(Ms, NMembers),
    giant_component_analysis:count_by_action_band(Ms, Ctx, SF, DF,
                                                  NS, NB, NW, ND, NKept, NExcluded),
    Bands is NS + NB + NW + ND,
    Bands =:= NKept,                    % identity (1)
    NKept + NExcluded =:= NMembers,     % identity (2)
    NExcluded > 0,                      % NOT a degenerate pass (criterion 5)
    NExcluded =:= 3,                    % exactly the three synthetic members
    NKept =:= 4.                        % exactly the four real scorable ones

% THE EQUIVALENCE REQUIREMENT, MADE TESTABLE PER CAUSE. The excluded count must
% be produced by the SAME conjunction the guard rejects, which drops three
% different populations. Each cause is asserted on its own so a regression names
% which one broke: a run where only (a) is counted passes a bulk conservation
% check on a corpus with no throwing or failing member, and fails here.
test(oq356_all_three_exclusion_causes_land_in_nexcluded, [
        setup(oq356_fixture_in),
        cleanup(oq356_fixture_out)
    ]) :-
    constraint_indexing:default_context(Ctx),
    config:param(purity_action_sound_floor, SF),
    config:param(purity_action_degraded_floor, DF),
    % (a) SUCCEEDS with a non-number -- and the precondition is witnessed, so a
    %     fixture that silently stopped producing `unknown` cannot pass here.
    purity_scoring:purity_score(oq356_member_unknown, PU), PU == unknown,
    drl_purity_network:effective_purity(oq356_member_unknown, Ctx, EPU, _),
    EPU == unknown,
    giant_component_analysis:count_by_action_band([oq356_member_unknown], Ctx,
                                                  SF, DF, _, _, _, _, KA, XA),
    KA =:= 0, XA =:= 1,
    % (b) THROWS -- dropped by the catch/3, must still be counted
    catch(purity_scoring:purity_score(oq356_member_throw, _), ET, true),
    ET == oq356_synthetic_producer_error,
    giant_component_analysis:count_by_action_band([oq356_member_throw], Ctx,
                                                  SF, DF, _, _, _, _, KB, XB),
    KB =:= 0, XB =:= 1,
    % (c) FAILS -- the conjunct fails, must still be counted
    \+ drl_purity_network:effective_purity(oq356_member_fail, Ctx, _, _),
    giant_component_analysis:count_by_action_band([oq356_member_fail], Ctx,
                                                  SF, DF, _, _, _, _, KC, XC),
    KC =:= 0, XC =:= 1.

% The guard must EXCLUDE NON-NUMBERS AND NOTHING ELSE. Two-sided against the
% test above: on a member list with no absence at all, NExcluded must be 0 and
% every member kept. A guard that over-rejects passes every conservation check
% in this file and fails here. This is the unit-level twin of the haiku2/haiku3
% leg invariance control (V6c).
test(oq356_guard_excludes_nothing_when_all_scorable) :-
    constraint_indexing:default_context(Ctx),
    config:param(purity_action_sound_floor, SF),
    config:param(purity_action_degraded_floor, DF),
    oq356_scorable_members(6, Ms),
    length(Ms, NMembers),
    giant_component_analysis:count_by_action_band(Ms, Ctx, SF, DF,
                                                  NS, NB, NW, ND, NKept, NExcluded),
    NExcluded =:= 0,
    NKept =:= NMembers,
    Bands is NS + NB + NW + ND,
    Bands =:= NKept.

:- end_tests(purity_absence).

% ============================================================================
% C-LATENT producer tests — one per mechanism terminus (m1-m4), plus the
% top-level bare-constraint claim and the R3 aggregation polarity.
% Written RED at pre-fix HEAD; land together with the producer edits (GREEN).
% All use the bare gate-passing template: no grid, no scope types, no
% extraction data — every no-data branch fires at once, but each test pins
% its OWN terminus, giving per-mechanism attribution without four commits.
% ============================================================================

:- begin_tests(purity_absence_producers).

% mech 1 (SI): empty type list is no_data, not variant([]) — and the subscore
% is unknown, not the N=0 overshoot 1.25.
test(m1_scope_invariance_no_data, [
        setup(oq60_assert_bare(oq60_inject_bare)),
        cleanup(oq60_retract_bare(oq60_inject_bare))
    ]) :-
    boltzmann_compliance:scope_invariance_test(oq60_inject_bare, R),
    R == no_data,
    purity_scoring:scope_invariance_subscore(oq60_inject_bare, SI),
    SI == unknown.

% mech 2 (coupling): a <2-point grid is no data — cross_index_coupling FAILS
% (never a fabricated 0.0, and failure is not cached), and the factorization
% subscore reports unknown.
test(m2_coupling_fails_not_zero, [
        setup(oq60_assert_bare(oq60_inject_bare)),
        cleanup(oq60_retract_bare(oq60_inject_bare))
    ]) :-
    \+ boltzmann_compliance:cross_index_coupling(oq60_inject_bare, _),
    \+ boltzmann_compliance:cached_coupling(oq60_inject_bare, _),
    purity_scoring:factorization_subscore(oq60_inject_bare, F),
    F == unknown.

% mech 3 (CC): empty grid → coupling-cleanliness unknown, never "clean 1.0".
test(m3_coupling_cleanliness_unknown, [
        setup(oq60_assert_bare(oq60_inject_bare)),
        cleanup(oq60_retract_bare(oq60_inject_bare))
    ]) :-
    purity_scoring:coupling_cleanliness_subscore(oq60_inject_bare, CC),
    CC == unknown.

% mech 4 (EX): no extraction data → unknown, never "clean 1.0".
test(m4_excess_extraction_unknown, [
        setup(oq60_assert_bare(oq60_inject_bare)),
        cleanup(oq60_retract_bare(oq60_inject_bare))
    ]) :-
    purity_scoring:excess_extraction_subscore(oq60_inject_bare, EX),
    EX == unknown.

% The OQ-60 headline: a bare gate-passing constraint must NOT score pristine.
test(bare_constraint_purity_unknown_never_1, [
        setup(oq60_assert_bare(oq60_inject_bare)),
        cleanup(oq60_retract_bare(oq60_inject_bare))
    ]) :-
    purity_scoring:purity_score(oq60_inject_bare, P),
    P == unknown,
    P \== 1.0.

% R3 aggregation polarity at the structural_purity verdict:
% clean aggregate over a set with an unknown member → distinct abstention.
test(structural_purity_abstains_on_no_data, [
        setup(oq60_assert_bare(oq60_inject_bare)),
        cleanup(oq60_retract_bare(oq60_inject_bare))
    ]) :-
    signature_detection:structural_purity(oq60_inject_bare, PC),
    PC == inconclusive(no_data).

% R3 polarity, both directions, at the pure aggregation helper:
% a witnessed failure fires THROUGH unknown members (existential); a clean
% aggregate with an unknown member abstains; all-pass delegates.
test(aggregation_polarity) :-
    signature_detection:aggregate_purity_tests(
        [pass(a), fail(scope_invariance, variant([x, y])), unknown(no_extraction_data)],
        V1),
    V1 == purity_fail([fail(scope_invariance, variant([x, y]))]),   % OQ-62 rename
    signature_detection:aggregate_purity_tests(
        [pass(a), pass(b), unknown(no_extraction_data)],
        V2),
    V2 == inconclusive(no_data),
    signature_detection:aggregate_purity_tests([pass(a), pass(b)], V3),
    V3 == all_pass.

:- end_tests(purity_absence_producers).

% ============================================================================
% C-FLOOR producer tests — mechanism 5, the LIVE commit.
% boltzmann_floor_for/2 clause 3 fabricated boltzmann_floor_default=0.05 when
% coordination_type was absent, letting 93 constraints (11/2/80/2 per leg at
% the 2026-07-23 census) score purity off a floor nobody authored. Post-fix:
% no override + no coordination_type ⇒ boltzmann_floor_for FAILS ⇒
% excess_extraction fails ⇒ EX subscore unknown ⇒ purity unknown (C-LATENT
% wiring). The authored paths (override, coordination type) are unchanged.
% ============================================================================

:- begin_tests(purity_absence_floor).

% Gate-passing constraint WITH an extractiveness metric but NO
% coordination_type and NO override — the m5 victim shape.
oq60_assert_floor_probe(C) :-
    oq60_assert_bare(C),
    assertz(narrative_ontology:constraint_metric(C, extractiveness, 0.5)),
    cache_registry:clear_all_caches.

oq60_retract_floor_probe(C) :-
    retractall(narrative_ontology:constraint_metric(C, extractiveness, _)),
    retractall(narrative_ontology:coordination_type(C, _)),
    retractall(narrative_ontology:boltzmann_floor_override(C, _)),
    oq60_retract_bare(C).

% RED at pre-fix HEAD (floor succeeded with the fabricated 0.05 default).
test(m5_absent_coordination_type_floor_fails, [
        setup(oq60_assert_floor_probe(oq60_floor_probe)),
        cleanup(oq60_retract_floor_probe(oq60_floor_probe))
    ]) :-
    \+ boltzmann_compliance:boltzmann_floor_for(oq60_floor_probe, _),
    \+ boltzmann_compliance:excess_extraction(oq60_floor_probe, _),
    purity_scoring:excess_extraction_subscore(oq60_floor_probe, EX),
    EX == unknown,
    purity_scoring:purity_score(oq60_floor_probe, P),
    P == unknown.

% Authored paths unchanged: coordination type routes to its typed floor param.
test(m5_coordination_type_floor_unchanged, [
        setup(( oq60_assert_floor_probe(oq60_floor_probe),
                assertz(narrative_ontology:coordination_type(oq60_floor_probe, information_standard)),
                cache_registry:clear_all_caches )),
        cleanup(oq60_retract_floor_probe(oq60_floor_probe))
    ]) :-
    boltzmann_compliance:boltzmann_floor_for(oq60_floor_probe, F),
    config:param(boltzmann_floor_information_standard, Expected),
    F == Expected,
    boltzmann_compliance:excess_extraction(oq60_floor_probe, _).

% Authored paths unchanged: per-constraint override wins outright.
test(m5_override_floor_unchanged, [
        setup(( oq60_assert_floor_probe(oq60_floor_probe),
                assertz(narrative_ontology:boltzmann_floor_override(oq60_floor_probe, 0.2)),
                cache_registry:clear_all_caches )),
        cleanup(oq60_retract_floor_probe(oq60_floor_probe))
    ]) :-
    boltzmann_compliance:boltzmann_floor_for(oq60_floor_probe, F),
    F == 0.2.

:- end_tests(purity_absence_floor).
