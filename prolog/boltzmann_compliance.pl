:- module(boltzmann_compliance, [
    boltzmann_compliant/2,
    boltzmann_shadow_audit/2,
    cross_index_coupling/2,
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
    (   GridSize < 2
    ->  CouplingScore = 0.0  % Not enough data points
    ;   count_coupling_violations(Grid, Powers, Scopes, Violations),
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
classify_at_context_impl(C, Context, Type) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    (   narrative_ontology:constraint_metric(C, ExtMetricName, BaseEps)
    ->  true
    ;   BaseEps = 0.5
    ),
    constraint_indexing:extractiveness_for_agent(C, Context, Chi),
    config:param(suppression_metric_name, SuppMetricName),
    (   narrative_ontology:constraint_metric(C, SuppMetricName, Supp)
    ->  true
    ;   Supp = 0
    ),
    drl_core:classify_from_metrics(C, BaseEps, Chi, Supp, Context, Type).

%% count_coupling_violations(+Grid, +Powers, +Scopes, -Violations)
%  Counts how many (Power, Scope) pairs show classification that
%  doesn't factorize. For each power level, checks if the type
%  is invariant across scopes. Each scope-level change in type
%  counts as a violation.
count_coupling_violations(Grid, Powers, Scopes, Violations) :-
    findall(1,
        (   member(P, Powers),
            member(S1, Scopes),
            member(S2, Scopes),
            S1 @< S2,
            member(classified(P, S1, T1), Grid),
            member(classified(P, S2, T2), Grid),
            T1 \= T2
        ),
        ViolationList
    ),
    length(ViolationList, ScopeViolations),
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
    length(PowerViolationList, PowerViolations),
    Violations is ScopeViolations + PowerViolations.

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
% Indexically opaque transitions: legitimate waypoint on rope → {tangled_rope, snare} path
% As d increases from institutional, extraction becomes visible before consent activates.
expected_power_divergence(_, _, rope, indexically_opaque) :- !.
expected_power_divergence(_, _, indexically_opaque, rope) :- !.
expected_power_divergence(_, _, indexically_opaque, tangled_rope) :- !.
expected_power_divergence(_, _, tangled_rope, indexically_opaque) :- !.
expected_power_divergence(_, _, indexically_opaque, snare) :- !.
expected_power_divergence(_, _, snare, indexically_opaque) :- !.

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

coordination_type_to_offset_param(information_standard,  complexity_offset_information_standard).
coordination_type_to_offset_param(resource_allocation,   complexity_offset_resource_allocation).
coordination_type_to_offset_param(enforcement_mechanism, complexity_offset_enforcement_mechanism).
coordination_type_to_offset_param(global_infrastructure, complexity_offset_global_infrastructure).

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
epistemic_access_check(C, true) :-
    config:param(boltzmann_min_classifications, MinN),
    findall(Ctx,
        constraint_indexing:constraint_classification(C, _, Ctx),
        Ctxs
    ),
    length(Ctxs, N),
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
    narrative_ontology:constraint_metric(C, ExtMetricName, Eps),
    boltzmann_floor_for(C, Floor),
    ExcessEps is max(0.0, Eps - Floor).

%% boltzmann_floor_for(+Constraint, -Floor)
%  Returns the Boltzmann floor for a constraint.
%  Priority: per-constraint override > coordination type > default
boltzmann_floor_for(C, Floor) :-
    narrative_ontology:boltzmann_floor_override(C, Floor), !.
boltzmann_floor_for(C, Floor) :-
    narrative_ontology:coordination_type(C, Type),
    coordination_type_to_floor_param(Type, ParamName),
    config:param(ParamName, Floor), !.
boltzmann_floor_for(_, Floor) :-
    config:param(boltzmann_floor_default, Floor).

coordination_type_to_floor_param(information_standard,  boltzmann_floor_information_standard).
coordination_type_to_floor_param(resource_allocation,   boltzmann_floor_resource_allocation).
coordination_type_to_floor_param(enforcement_mechanism, boltzmann_floor_enforcement_mechanism).
coordination_type_to_floor_param(global_infrastructure, boltzmann_floor_global_infrastructure).

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
    (   length(UniqueTypes, 1)
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
