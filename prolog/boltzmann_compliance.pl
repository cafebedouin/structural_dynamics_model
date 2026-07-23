:- module(boltzmann_compliance, [
    boltzmann_compliant/2,
    boltzmann_shadow_audit/2,
    cross_index_coupling/2,
    coupling_liveness/3,
    detect_nonsensical_coupling/3,
    complexity_adjusted_threshold/2,
    epistemic_access_check/2,
    excess_extraction/2,
    boltzmann_floor_for/2,
    boltzmann_invariant_mountain/2,
    scope_invariance_test/2,
    ib_adjusted_threshold/2,
    coupling_test_powers/1,
    coupling_test_scopes/1,
    coupling_test_context/3,
    classify_at_context/3,
    clear_classification_cache/0
]).

:- use_module(library(lists)).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(constraint_indexing).

% --- Classification memoization cache ---
% Avoids redundant classify_at_context/3 and cross_index_coupling/2
% calls. classify_at_context is called 12 times per constraint
% (4 powers × 3 scopes), and the same constraint may be classified
% 100+ times per test run across Boltzmann, purity, FNL, FCR, etc.
:- dynamic cached_classification/3.   % cached_classification(C, Context, Type)
:- dynamic cached_coupling/2.         % cached_coupling(C, CouplingScore)

%% clear_classification_cache/0
%  Invalidates all memoized classifications and coupling scores.
%  Called by scenario_manager:clear_kb/0 between test intervals.
clear_classification_cache :-
    retractall(cached_classification(_, _, _)),
    retractall(cached_coupling(_, _)).

% Registered with the central invalidation surface (cache_registry.pl) so
% overlay probes can clear every memo without knowing this module's caches.
:- multifile cache_registry:clear_hook/0.
cache_registry:clear_hook :- clear_classification_cache.

/* ================================================================
   BOLTZMANN COMPLIANCE ENGINE v5.0

   Based on Tamuz & Sandomirskiy (2025), "On the origin of the
   Boltzmann distribution," Mathematische Annalen.

   Core theorem: The Boltzmann distribution is the ONLY distribution
   that correctly describes unrelated (uncoupled) systems.

   Application to DR: A Natural Law (Mountain) must show Boltzmann-
   compliant independence across index dimensions. Any constraint
   that couples independent dimensions is necessarily Constructed,
   not Natural.

   ACTIVE: Boltzmann compliance drives classification overrides via
   integrate_signature_with_modal/3 (called from drl_core:dr_type/3).
   Override rules NL, FNL, CI_Rope, and FCR are all live in the
   classification pipeline. The original shadow-mode calibration
   period ended when the override rules were wired into
   resolve_modal_signature_conflict (v5.1).

   Edge Cases Handled:
   1. Complexity Offset — high-complexity coordination types have
      inherently higher coupling (global infrastructure vs naming)
   2. Epistemic Access — insufficient indexed classifications make
      the test inconclusive rather than rejecting
   3. Moving Boltzmann Floor — the minimum necessary extraction
      can increase with system complexity over time
   ================================================================ */

/* ----------------------------------------------------------------
   BOLTZMANN COMPLIANCE TEST
   ---------------------------------------------------------------- */

% Categorical: Naturality condition [STRICT] — tests factorizability of classification across index dimensions
%% boltzmann_compliant(+Constraint, -Result)
%  Tests whether a constraint's classification across index
%  dimensions is consistent with Boltzmann independence.
%
%  Result is one of:
%    compliant(CouplingScore)
%    non_compliant(CouplingScore, Threshold)
%    inconclusive(Reason)
%
%  ACTIVE: This predicate feeds constraint_signature/2, which feeds
%  integrate_signature_with_modal/3, which overrides dr_type/3.
%  Use boltzmann_shadow_audit/2 for full diagnostic output.

boltzmann_compliant(C, Result) :-
    (   epistemic_access_check(C, true)
    ->  cross_index_coupling(C, CouplingScore),
        complexity_adjusted_threshold(C, Threshold),
        (   CouplingScore =< Threshold
        ->  Result = compliant(CouplingScore)
        ;   Result = non_compliant(CouplingScore, Threshold)
        )
    ;   Result = inconclusive(insufficient_classifications)
    ).

%% boltzmann_shadow_audit(+Constraint, -AuditReport)
%  Full diagnostic report for Boltzmann compliance.
%  Designed for logging in test_harness.pl without triggering
%  classification changes.
%
%  AuditReport = boltzmann_audit(
%      Constraint,
%      ComplianceResult,
%      CouplingScore,
%      Threshold,
%      CoupledPairs,
%      ExcessExtraction,
%      InvariantResult
%  )

boltzmann_shadow_audit(C, boltzmann_audit(C, Compliance, Coupling, Threshold,
                                          CoupledPairs, Excess, Invariant)) :-
    boltzmann_compliant(C, Compliance),
    (   cross_index_coupling(C, Coupling)
    ->  true
    ;   Coupling = unknown
    ),
    (   complexity_adjusted_threshold(C, Threshold)
    ->  true
    ;   Threshold = unknown
    ),
    (   detect_nonsensical_coupling(C, CoupledPairs, _)
    ->  true
    ;   CoupledPairs = []
    ),
    (   excess_extraction(C, Excess)
    ->  true
    ;   Excess = unknown
    ),
    (   boltzmann_invariant_mountain(C, Invariant)
    ->  true
    ;   Invariant = unknown
    ).

/* ----------------------------------------------------------------
   CROSS-INDEX COUPLING DETECTION
   ----------------------------------------------------------------
   The "Sicherman Dice" test.

   For each constraint, compute classification across a grid of
   (Power, Scope) combinations. If the classification map factorizes
   — i.e., changing Power has the same effect at all Scope levels
   and vice versa — the constraint is Boltzmann-compliant.

   If a scope change flips classification at ONE power level but
   not another, there's a coupling that violates independence.
   ---------------------------------------------------------------- */

% Categorical: Naturality square test [STRICT] — checks commutativity of classification on Power x Scope grid
%% cross_index_coupling(+Constraint, -CouplingScore)
%  Computes coupling score from 0.0 (fully independent) to 1.0
%  (maximally coupled) by testing classification factorizability
%  across Power × Scope grid.

cross_index_coupling(C, CouplingScore) :-
    (   cached_coupling(C, CachedScore)
    ->  CouplingScore = CachedScore
    ;   compute_cross_index_coupling(C, ComputedScore),
        assertz(cached_coupling(C, ComputedScore)),
        CouplingScore = ComputedScore
    ).

%% compute_cross_index_coupling(+Constraint, -CouplingScore)
%  Implementation body for cross_index_coupling/2.
compute_cross_index_coupling(C, CouplingScore) :-
    coupling_test_powers(Powers),
    coupling_test_scopes(Scopes),
    findall(
        classified(P, S, Type),
        (   member(P, Powers),
            member(S, Scopes),
            coupling_test_context(P, S, Ctx),
            classify_at_context(C, Ctx, Type)
        ),
        Grid
    ),
    length(Grid, GridSize),
    % OQ-60 mech 2: a <2-point grid is NO DATA, not independence — FAIL rather
    % than fabricate 0.0 (absence must not present as a measurement). Failure
    % is not cached (cross_index_coupling asserts cached_coupling only on
    % success), so a later grid-bearing state recomputes.
    GridSize >= 2,
    (   count_coupling_violations(Grid, Powers, Scopes, Violations),
        length(Powers, NP),
        length(Scopes, NS),
        MaxViolations is NP * (NS - 1),
        (   MaxViolations > 0
        ->  CouplingScore is min(1.0, Violations / MaxViolations)
        ;   CouplingScore = 0.0
        )
    ).

%% coupling_test_powers(-Powers)
%  The power levels used for coupling grid test.
coupling_test_powers([powerless, moderate, institutional, analytical]).

%% coupling_test_scopes(-Scopes)
%  The scope levels used for coupling grid test.
coupling_test_scopes([local, national, global]).

%% coupling_test_context(+Power, +Scope, -Context)
%  Builds a canonical context for coupling grid test.
%  Uses standard time horizon and exit options per power level.
coupling_test_context(powerless, Scope, context(
    agent_power(powerless), time_horizon(biographical),
    exit_options(trapped), spatial_scope(Scope))).
coupling_test_context(moderate, Scope, context(
    agent_power(moderate), time_horizon(biographical),
    exit_options(mobile), spatial_scope(Scope))).
coupling_test_context(institutional, Scope, context(
    agent_power(institutional), time_horizon(generational),
    exit_options(arbitrage), spatial_scope(Scope))).
coupling_test_context(analytical, Scope, context(
    agent_power(analytical), time_horizon(civilizational),
    exit_options(analytical), spatial_scope(Scope))).

%% classify_at_context(+C, +Context, -Type)
%  Memoizing wrapper around classify_at_context_impl/3.
%  Checks cached_classification/3 first; on miss, delegates to impl
%  and caches the result. This avoids redundant metric lookups and
%  classification calls — classify_at_context is invoked 12 times per
%  constraint per Boltzmann test, and the same constraint participates
%  in multiple tests (coupling, purity, FNL, FCR, scope invariance).
classify_at_context(C, Context, Type) :-
    (   cached_classification(C, Context, CachedType)
    ->  Type = CachedType
    ;   classify_at_context_impl(C, Context, ComputedType),
        assertz(cached_classification(C, Context, ComputedType)),
        Type = ComputedType
    ).

%% classify_at_context_impl(+C, +Context, -Type)
%  Computes metrics and delegates to drl_core:classify_from_metrics/6
%  (the single source of truth for threshold classification).
%  Uses explicit module qualification to avoid circular use_module
%  dependency — drl_core imports structural_signatures, so we cannot
%  import drl_core, but runtime-qualified calls work fine since both
%  modules are loaded by the time any coupling test runs.
%  Also uses extractiveness_for_agent (v6.0 directionality chain)
%  instead of the legacy power_modifier * scope_modifier calculation.
%  OQ-205 (spec §3, fix 2): a missing ε FAILS the classification — never the
%  fabricated BaseEps = 0.5 (the OQ-89 neutral-default class, silently above
%  snare_epsilon_floor 0.46) or Supp = 0. Reads mirror the canonical is_X/3
%  sources exactly: drl_core:base_extractiveness/2 (constraint_metric via
%  constraint_data + direct multifile facts, fail-closed) and
%  drl_core:get_raw_suppression/2 (`unknown` token on absence, which
%  classify_from_metrics/6 already handles from the is_X path). Failure — not
%  a Type = unknown token — so the coupling grid stays EMPTY for a no-ε story
%  and downstream reads null/inconclusive (didn't-compute), not a
%  computed-looking 0-violations over a grid of unknowns (Pattern 6).
classify_at_context_impl(C, Context, Type) :-
    drl_core:base_extractiveness(C, BaseEps),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    drl_core:get_raw_suppression(C, Supp),
    drl_core:classify_from_metrics(C, BaseEps, Chi, Supp, Context, Type).

%% count_coupling_violations(+Grid, +Powers, +Scopes, -Violations)
%  Counts how many (Power, Scope) pairs show classification that
%  doesn't factorize. For each power level, checks if the type
%  is invariant across scopes. Each scope-level change in type
%  counts as a violation.
count_coupling_violations(Grid, Powers, Scopes, Violations) :-
    coupling_violation_components(Grid, Powers, Scopes,
                                  ScopeViolations, PowerViolations),
    Violations is ScopeViolations + PowerViolations.

%% coupling_violation_components(+Grid, +Powers, +Scopes, -ScopeViolations, -PowerViolations)
%  Splits the violation count into its two independent channels, instead of
%  only returning the sum:
%    ScopeViolations — classification changes across scope at a FIXED power
%      (the ±σ(S) multiplier crossing a threshold band — scope is a live index).
%    PowerViolations — classification changes across power at a FIXED scope,
%      EXCLUDING the divergences DR treats as expected indexical relativity
%      (power is a live index beyond what the framework already licenses).
%  count_coupling_violations/4 sums these for the coupling score; surfacing the
%  components separately lets a consumer read WHICH index is live — the "seat"
%  structure — rather than only the aggregate coupling magnitude. This is the
%  SOLE source of the violation logic (the score path and coupling_liveness/3
%  both route through it), so the two cannot silently drift apart.
coupling_violation_components(Grid, Powers, Scopes,
                              ScopeViolations, PowerViolations) :-
    findall(1,
        (   member(P, Powers),
            member(S1, Scopes),
            member(S2, Scopes),
            S1 @< S2,
            member(classified(P, S1, T1), Grid),
            member(classified(P, S2, T2), Grid),
            T1 \= T2
        ),
        ScopeViolationList
    ),
    length(ScopeViolationList, ScopeViolations),
    % Also check power invariance at each scope
    findall(1,
        (   member(S, Scopes),
            member(P1, Powers),
            member(P2, Powers),
            P1 @< P2,
            member(classified(P1, S, T1), Grid),
            member(classified(P2, S, T2), Grid),
            T1 \= T2,
            % Power-driven variance is EXPECTED (indexical relativity).
            % Only count as violation if the PATTERN of power-variance
            % differs across scopes (i.e., power shifts type at one scope
            % but not another in a way that isn't explained by σ scaling).
            \+ expected_power_divergence(P1, P2, T1, T2)
        ),
        PowerViolationList
    ),
    length(PowerViolationList, PowerViolations).

%% coupling_liveness(+Constraint, -ScopeViolations, -PowerViolations)
%  Per-constraint liveness profile: rebuilds the Power×Scope classification
%  grid and reports the two violation channels separately (via
%  coupling_violation_components/5). A constraint with (0, 0) is index-invariant
%  on this grid — seat-free with respect to the observer index, which is
%  consistent with a genuine Mountain (Boltzmann invariance is a PARTIAL test
%  for Mountain-ness, not a pathology flag). Non-zero counts say which index
%  moves the verdict: scope, power, or both. Fails (to be caught by callers)
%  when the grid cannot be built — e.g. epistemic access is unavailable.
coupling_liveness(C, ScopeViolations, PowerViolations) :-
    coupling_test_powers(Powers),
    coupling_test_scopes(Scopes),
    findall(classified(P, S, T),
        (   member(P, Powers),
            member(S, Scopes),
            coupling_test_context(P, S, Ctx),
            classify_at_context(C, Ctx, T)
        ),
        Grid),
    Grid \= [],
    coupling_violation_components(Grid, Powers, Scopes,
                                  ScopeViolations, PowerViolations).

%% expected_power_divergence(+P1, +P2, +T1, +T2)
%  Power-driven classification divergence is EXPECTED in DR.
%  A powerless agent seeing snare while institutional sees rope
%  is not a coupling violation — it's indexical relativity working
%  correctly. This predicate identifies expected divergence patterns.
expected_power_divergence(powerless, institutional, _, _) :- !.
expected_power_divergence(institutional, powerless, _, _) :- !.
expected_power_divergence(powerless, analytical, _, _) :- !.
expected_power_divergence(analytical, powerless, _, _) :- !.
% Moderate-analytical divergence is expected (π = 1.0 vs 1.15)
expected_power_divergence(moderate, analytical, _, _) :- !.
expected_power_divergence(analytical, moderate, _, _) :- !.
% Mountain-rope divergence at moderate/powerless and analytical/institutional
% is expected: the immutability gate returns rope for (biographical, mobile)
% and (generational, arbitrage) contexts. A natural constraint doesn't become
% changeable because the observer has mobile exit options.
% Type-conditioned: only suppresses divergence when one side is mountain.
expected_power_divergence(moderate, powerless, _, mountain) :- !.
expected_power_divergence(powerless, moderate, mountain, _) :- !.
expected_power_divergence(analytical, institutional, mountain, _) :- !.
expected_power_divergence(institutional, analytical, _, mountain) :- !.
% Naturalized transitions: legitimate waypoint on rope → {tangled_rope, snare} path
% As d increases from institutional, extraction becomes visible before consent activates.
expected_power_divergence(_, _, rope, naturalized) :- !.
expected_power_divergence(_, _, naturalized, rope) :- !.
expected_power_divergence(_, _, naturalized, tangled_rope) :- !.
expected_power_divergence(_, _, tangled_rope, naturalized) :- !.
expected_power_divergence(_, _, naturalized, snare) :- !.
expected_power_divergence(_, _, snare, naturalized) :- !.

/* ----------------------------------------------------------------
   NONSENSICAL COUPLING DETECTION
   ----------------------------------------------------------------
   Identifies WHICH specific dimension pairs show coupling that
   violates Boltzmann independence. This is the "Sicherman Dice"
   diagnostic — it tells you exactly which "dice" are "crazy."
   ---------------------------------------------------------------- */

%% detect_nonsensical_coupling(+Constraint, -CoupledPairs, -Strength)
%  Returns list of coupled dimension pairs and overall coupling strength.
%  CoupledPairs = [coupled(Dim1, Dim2, Score), ...]
%  Strength = aggregate coupling strength in [0, 1]

detect_nonsensical_coupling(C, CoupledPairs, Strength) :-
    coupling_test_powers(Powers),
    coupling_test_scopes(Scopes),
    findall(
        classified(P, S, Type),
        (   member(P, Powers), member(S, Scopes),
            coupling_test_context(P, S, Ctx),
            classify_at_context(C, Ctx, Type)
        ),
        Grid
    ),
    % OQ-60 mech 3: an EMPTY grid is no data — fail rather than report a
    % vacuously clean Pairs=[]/Strength=0.0. A nonempty grid with no coupled
    % pairs still succeeds ([], 0.0): that is a measured-clean, not absence.
    Grid \== [],
    findall(
        coupled(power_scope, P, ScopePair, Score),
        (   member(P, Powers),
            member(S1, Scopes), member(S2, Scopes),
            S1 @< S2,
            member(classified(P, S1, T1), Grid),
            member(classified(P, S2, T2), Grid),
            T1 \= T2,
            ScopePair = S1-S2,
            Score = 1.0
        ),
        CoupledPairs
    ),
    (   CoupledPairs = []
    ->  Strength = 0.0
    ;   length(CoupledPairs, N),
        length(Powers, NP), length(Scopes, NS),
        MaxPairs is NP * (NS * (NS - 1)) // 2,
        (MaxPairs > 0 -> Strength is min(1.0, N / MaxPairs) ; Strength = 0.0)
    ).

/* ----------------------------------------------------------------
   COMPLEXITY-ADJUSTED THRESHOLD
   ----------------------------------------------------------------
   Edge Case #1: A global power grid MUST couple dimensions that
   a simple naming convention does not. The Boltzmann coupling
   threshold should be higher for inherently complex coordination.
   ---------------------------------------------------------------- */

%% complexity_adjusted_threshold(+Constraint, -Threshold)
%  Returns the effective Boltzmann coupling threshold after applying
%  the complexity offset for the constraint's coordination type.

complexity_adjusted_threshold(C, Threshold) :-
    config:param(boltzmann_coupling_threshold, BaseThreshold),
    coordination_type_offset(C, Offset),
    Threshold is BaseThreshold + Offset.

%% coordination_type_offset(+Constraint, -Offset)
%  Looks up the complexity offset for a constraint's coordination type.
%  Falls back to default if no coordination type is declared.
coordination_type_offset(C, Offset) :-
    narrative_ontology:coordination_type(C, Type),
    coordination_type_to_offset_param(Type, ParamName),
    config:param(ParamName, Offset), !.
coordination_type_offset(_, Offset) :-
    config:param(complexity_offset_default, Offset).

coordination_type_to_offset_param(information_standard,    complexity_offset_information_standard).
coordination_type_to_offset_param(attachment_coordination, complexity_offset_attachment_coordination).
coordination_type_to_offset_param(resource_allocation,     complexity_offset_resource_allocation).
coordination_type_to_offset_param(identity_coordination,   complexity_offset_identity_coordination).
coordination_type_to_offset_param(enforcement_mechanism,   complexity_offset_enforcement_mechanism).
coordination_type_to_offset_param(global_infrastructure,   complexity_offset_global_infrastructure).

/* ----------------------------------------------------------------
   EPISTEMIC ACCESS CHECK
   ----------------------------------------------------------------
   Edge Case #2: If an agent's Markov Blanket prevents them from
   seeing enough of the constraint, the Boltzmann test is
   inconclusive rather than rejecting.
   ---------------------------------------------------------------- */

%% epistemic_access_check(+Constraint, -Sufficient)
%  Returns true if enough indexed classifications exist for a
%  reliable Boltzmann compliance test, false otherwise.
%
%  WARNING — call with an UNBOUND second argument and test the result.
%  Calling epistemic_access_check(C, false) with `false` bound ALWAYS
%  succeeds: clause 1's head can't unify with false, so its guard+cut
%  never run and the catch-all matches every constraint (bound-probe
%  bypasses clause-order). This made structural_purity/2 unconditionally
%  `inconclusive` for the whole corpus until 2026-06-03; see
%  audits/2026-06-03_purity/purity_audit_20260603.md §2. Bound-`true` calls are safe
%  (clause-1 failure falls through to no solution, not a wrong one).
epistemic_access_check(C, true) :-
    config:param(boltzmann_min_classifications, MinN),
    findall(Ctx,
        constraint_indexing:constraint_classification(C, _, Ctx),
        Ctxs
    ),
    % OQ-109 Phase B / AUDIT OPEN-2 (2026-06-12): authored stakeholder seats
    % are epistemic positions — counted alongside authored classification
    % contexts so the gate measures position coverage rather than the legacy
    % perspectives[] table (which Phase C retires). Output-preserving on the
    % live corpus at change time (witnessed): every story either carries >=3
    % authored classifications or 0 of both surfaces.
    findall(Name,
        narrative_ontology:constraint_stakeholder(C, Name, _, _, _, _, _),
        Names
    ),
    length(Ctxs, NC),
    length(Names, NS),
    N is NC + NS,
    N >= MinN, !.
epistemic_access_check(_, false).

/* ----------------------------------------------------------------
   PRICE OF ANARCHY / EXCESS EXTRACTION
   ----------------------------------------------------------------
   The Boltzmann floor is the minimum extraction inherent to the
   coordination type. Extraction above the floor is "extractive
   overhead" — the Price of Anarchy excess.

   Edge Case #3: The floor can move. Technology changes can lower
   the floor (reform pressure) or raise it (necessary complexity
   increase). Testsets can override via boltzmann_floor_override/2.
   ---------------------------------------------------------------- */

%% excess_extraction(+Constraint, -ExcessEps)
%  Computes how much extraction exceeds the Boltzmann floor.
%  ExcessEps = max(0, ε(C) - floor(coordination_type(C)))
%  This is the "extractive overhead" — the PoA excess.
excess_extraction(C, ExcessEps) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    % Use temporal ε threaded by classify_at_time/4 if available; else static fact.
    (   catch(nb_getval(classify_at_time_eps, eps(C, Eps)), _, fail)
    ->  true
    ;   narrative_ontology:constraint_metric(C, ExtMetricName, Eps)
    ),
    boltzmann_floor_for(C, Floor),
    ExcessEps is max(0.0, Eps - Floor).

%% boltzmann_floor_for(+Constraint, -Floor)
%  Returns the Boltzmann floor for a constraint.
%  Priority: per-constraint override > coordination type; FAILS when neither
%  is authored. OQ-60 mech 5 (C-FLOOR, 2026-07-23): the former clause 3
%  fabricated boltzmann_floor_default=0.05 on absent coordination_type,
%  letting 93 constraints (11/2/80/2 across the four legs) score purity off a
%  floor nobody authored (e.g. conceptual_framework_reading 0.972
%  near-pristine). An unauthored floor is no data: this fails, excess
%  extraction fails, the EX subscore reports `unknown`, and purity_score
%  propagates `unknown` (JSON null). Spec: docs/logic_extensions.md
%  (fabricated-default pattern, formerly flagged OQ-41).
boltzmann_floor_for(C, Floor) :-
    narrative_ontology:boltzmann_floor_override(C, Floor), !.
boltzmann_floor_for(C, Floor) :-
    narrative_ontology:coordination_type(C, Type),
    coordination_type_to_floor_param(Type, ParamName),
    config:param(ParamName, Floor), !.

coordination_type_to_floor_param(information_standard,    boltzmann_floor_information_standard).
coordination_type_to_floor_param(attachment_coordination, boltzmann_floor_attachment_coordination).
coordination_type_to_floor_param(resource_allocation,     boltzmann_floor_resource_allocation).
coordination_type_to_floor_param(identity_coordination,   boltzmann_floor_identity_coordination).
coordination_type_to_floor_param(enforcement_mechanism,   boltzmann_floor_enforcement_mechanism).
coordination_type_to_floor_param(global_infrastructure,   boltzmann_floor_global_infrastructure).

/* ----------------------------------------------------------------
   BOLTZMANN-INVARIANT MOUNTAIN TEST
   ----------------------------------------------------------------
   Axiom: Mountains must be Boltzmann-invariant across all indices.

   A constraint passes the Boltzmann-invariant mountain test iff:
   1. χ(C, P, S) factorizes (cross_index_coupling ≤ threshold)
   2. Classification is scope-invariant (same type at all scopes)
   3. No coupling drift (coupling topology is static)
   4. No excess extraction above Boltzmann floor

   This is the mathematically crisp definition of "natural law."

   SHADOW MODE: Results logged, not enforced.
   ---------------------------------------------------------------- */

%% boltzmann_invariant_mountain(+Constraint, -Result)
%  Tests whether a constraint satisfies all four Boltzmann
%  invariance conditions for Mountain classification.
%
%  Result is one of:
%    invariant(Details)        — passes all four tests
%    variant(FailedTests)      — fails one or more tests
%    inconclusive(Reason)      — insufficient data

boltzmann_invariant_mountain(C, inconclusive(insufficient_data)) :-
    epistemic_access_check(C, false), !.

boltzmann_invariant_mountain(C, Result) :-
    % Test 1: Factorization (Boltzmann compliance)
    boltzmann_compliant(C, CompResult),
    (   CompResult = compliant(_)
    ->  T1 = pass(factorization)
    ;   T1 = fail(factorization, CompResult)
    ),

    % Test 2: Scope invariance
    scope_invariance_test(C, ScopeResult),
    (   ScopeResult = invariant
    ->  T2 = pass(scope_invariance)
    ;   T2 = fail(scope_invariance, ScopeResult)
    ),

    % Test 3: No excess extraction above Boltzmann floor
    (   excess_extraction(C, Excess)
    ->  (   Excess =< 0.01
        ->  T3 = pass(no_excess_extraction)
        ;   T3 = fail(excess_extraction, Excess)
        )
    ;   T3 = pass(no_extraction_data)  % Mountains often have ε ≈ 0
    ),

    % Test 4: Natural law signature (existing check)
    % Runtime qualification: avoid circular use_module with signature_detection.
    % Both modules are loaded by stack.pl before any Boltzmann tests run.
    signature_detection:get_constraint_profile(C, Profile),
    (   signature_detection:natural_law_signature(Profile)
    ->  T4 = pass(natural_law_signature)
    ;   T4 = fail(natural_law_signature)
    ),

    % Aggregate results
    Tests = [T1, T2, T3, T4],
    include(is_failure, Tests, Failures),
    (   Failures = []
    ->  Result = invariant(Tests)
    ;   Result = variant(Failures)
    ).

%% scope_invariance_test(+Constraint, -Result)
%  Tests whether classification is stable across all scope levels
%  while holding power fixed at analytical (the most sensitive).
scope_invariance_test(C, Result) :-
    coupling_test_scopes(Scopes),
    findall(
        Type,
        (   member(S, Scopes),
            coupling_test_context(analytical, S, Ctx),
            classify_at_context(C, Ctx, Type)
        ),
        Types
    ),
    sort(Types, UniqueTypes),
    % OQ-60 mech 1: an EMPTY type list means classification produced nothing
    % at any scope — no data, distinct from measured variance. variant([])
    % previously leaked here and scored SI = 1.0 - (0-1)*0.25 = 1.25 downstream.
    (   UniqueTypes == []
    ->  Result = no_data
    ;   length(UniqueTypes, 1)
    ->  Result = invariant
    ;   Result = variant(UniqueTypes)
    ).

%% is_failure(+TestResult)
is_failure(fail(_)).
is_failure(fail(_, _)).

/* ----------------------------------------------------------------
   IB-AWARE COMPLEXITY THRESHOLD REFINEMENT
   ----------------------------------------------------------------
   Refines complexity_adjusted_threshold/2 for constraints that
   have theater ratio data. High theater ratio (high NoiseRatio
   in IB terms) suggests the coupling is extractive rather than
   functional, even in high-complexity coordination types.

   This prevents global_infrastructure constraints from getting
   a free pass on coupling if their theater ratio reveals the
   coupling is performance rather than function.
   ---------------------------------------------------------------- */

%% ib_adjusted_threshold(+Constraint, -Threshold)
%  Like complexity_adjusted_threshold but reduces the offset
%  when theater ratio is high (IB signal loss).
ib_adjusted_threshold(C, Threshold) :-
    config:param(boltzmann_coupling_threshold, BaseThreshold),
    coordination_type_offset(C, RawOffset),
    % If theater ratio is available, scale the offset down
    % proportionally. High theater = coupling is likely extractive
    % not functional, so don't give it the full complexity benefit.
    (   config:param(theater_metric_name, TM),
        narrative_ontology:constraint_metric(C, TM, TR),
        TR > 0.0
    ->  % IB scaling: offset × (1 - TheaterRatio)
        % At TR=0: full offset. At TR=0.7: only 30% of offset.
        SignalRetention is max(0.0, 1.0 - TR),
        AdjustedOffset is RawOffset * SignalRetention
    ;   AdjustedOffset = RawOffset
    ),
    Threshold is BaseThreshold + AdjustedOffset.
