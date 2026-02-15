% ============================================================================
% LOGICAL FINGERPRINT ENGINE
% ============================================================================
% Extracts structural "logical fingerprints" from constraints — the qualitative
% shape of a constraint's logic, abstracted away from domain-specific content.
%
% A logical fingerprint captures HOW a constraint works, not WHAT it constrains:
%   - How classification shifts across power perspectives (the "shift")
%   - What structural properties are present (the "skeleton")
%   - What SHOULD be present but ISN'T (the "negative space" / voids)
%   - The topology of who benefits and who is harmed (the "actors")
%   - The direction of temporal drift (the "trajectory")
%   - The metric zone — categorical, not numeric (the "regime")
%
% Two constraints with the same fingerprint are LOGICALLY ISOMORPHIC —
% they operate through the same mechanism regardless of domain.
%
% DESIGN PRINCIPLES:
%   - Prolog-native: fingerprints are structured terms, matchable via unification
%   - Composable: each dimension independently queryable
%   - Graceful degradation: missing data yields 'unknown', not failure
%   - Config-driven: zone boundaries from config.pl, not hardcoded
%
% FUTURE EXTENSIONS (designed for, not yet implemented):
%   1. Logical Succession — causal chains between constraints
%      (add requires_context/2 or pre_condition/2 predicates)
%   2. Isomorphism of Resistance — matching solutions across domains
%      (extend fingerprint with counter_move/2 from resolved constraints)
%   3. Regime Detection — meta-constraint ecosystems
%      (cluster fingerprint families into named regime types)
%   4. Semantic Distance Maximizer — Python layer scoring
%      (high logical similarity + low content similarity = discovery)
%
% Integration: Load after drl_core.pl, narrative_ontology.pl, config.pl
% ============================================================================

:- module(logical_fingerprint, [
    % Core fingerprint — full logical identity
    logical_fingerprint/2,          % logical_fingerprint(C, Fingerprint)

    % Individual dimensions (composable, independently queryable)
    fingerprint_shift/2,            % Perspectival classification shift
    fingerprint_properties/2,       % Structural properties present
    fingerprint_voids/2,            % Negative space — diagnostic absences
    fingerprint_actors/2,           % Actor topology (beneficiary/victim)
    fingerprint_drift/2,            % Temporal drift trajectory
    fingerprint_zone/2,             % Metric zone (categorical)
    fingerprint_coupling/2,         % Coupling topology (Boltzmann v5.0)

    % Discovery
    known_constraint/1,             % All discoverable constraint IDs (atoms only)

    % Grouping utilities
    shift_family/2,                 % All constraints sharing a shift pattern
    all_shift_patterns/1,           % All distinct shift patterns in corpus
    fingerprint_match/4             % Match two constraints on specific dimensions
]).

:- use_module(drl_core).
:- use_module(narrative_ontology).
:- use_module(constraint_indexing).
:- use_module(config).
:- use_module(domain_priors).
:- use_module(structural_signatures).

:- use_module(library(lists)).

% ============================================================================
% CORE FINGERPRINT
% ============================================================================

% Categorical: Presheaf invariant — qualitative shape of the constraint's logic, abstracting away domain content
%% logical_fingerprint(+Constraint, -Fingerprint)
%  Computes the full logical fingerprint — a structured term capturing
%  the qualitative shape of a constraint's logic.
%
%  Two constraints with unifiable fingerprints are logically isomorphic.
%
%  Fingerprint = fingerprint(Shift, Properties, Voids, Actors, Drift, Zone, Coupling)
%
%  v5.0: Added 7th dimension — Coupling Topology (Boltzmann compliance).
%  Maps which index dimensions are independent, weakly coupled,
%  strongly coupled, or nonsensically coupled.
%
%  v5.1: Extended coupling dimension with Purity Score — scalar [0,1]
%  combining all four Boltzmann structural tests into a single metric
%  for ranking, drift detection, and cross-domain comparison.

logical_fingerprint(C, fingerprint(Shift, Properties, Voids, Actors, Drift, Zone, Coupling)) :-
    fingerprint_shift(C, Shift),
    fingerprint_properties(C, Properties),
    fingerprint_voids(C, Voids),
    fingerprint_actors(C, Actors),
    fingerprint_drift(C, Drift),
    fingerprint_zone(C, Zone),
    fingerprint_coupling(C, Coupling).

% ============================================================================
% DIMENSION 1: PERSPECTIVAL SHIFT
% ============================================================================
% The most powerful discriminator. Captures how classification transforms
% as you change WHO is looking at the constraint.
%
% Two constraints with the same shift pattern are governed by the same
% underlying power dynamic, regardless of subject matter.

%% fingerprint_shift(+C, -Shift)
%  Computes classification at each standard power level.
%  Returns shift(PowerlessType, ModerateType, InstitutionalType, AnalyticalType)

fingerprint_shift(C, shift(Powerless, Moderate, Institutional, Analytical)) :-
    classify_at_power(C, powerless, Powerless),
    classify_at_power(C, moderate, Moderate),
    classify_at_power(C, institutional, Institutional),
    classify_at_power(C, analytical, Analytical).

%% classify_at_power(+C, +PowerLevel, -Type)
%  Classifies constraint from the standard context for a given power level.
classify_at_power(C, Power, Type) :-
    standard_context_for_power(Power, Context),
    (drl_core:dr_type(C, Context, T) -> Type = T ; Type = unknown).

% Standard contexts — one per power level, using canonical parameters
% from drl_core:standard_context/1 for consistency.
standard_context_for_power(powerless,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

standard_context_for_power(moderate,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

standard_context_for_power(institutional,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

standard_context_for_power(analytical,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% ============================================================================
% DIMENSION 2: STRUCTURAL PROPERTIES
% ============================================================================
% What boolean/relational predicates hold for this constraint.
% This is the "skeleton" — the logical structure that's present.

%% fingerprint_properties(+C, -Properties)
%  Returns a sorted list of structural property atoms that hold for C.

fingerprint_properties(C, Properties) :-
    findall(Prop, structural_property_holds(C, Prop), Raw),
    sort(Raw, Properties).

% Each clause detects one structural property.
% Using cuts after findall-internal checks to avoid duplicate detection.
structural_property_holds(C, enforcement) :-
    drl_core:requires_active_enforcement(C).

structural_property_holds(C, natural) :-
    drl_core:emerges_naturally(C).

structural_property_holds(C, sunset) :-
    narrative_ontology:has_sunset_clause(C).

structural_property_holds(C, coordination) :-
    narrative_ontology:has_coordination_function(C).

structural_property_holds(C, asymmetric) :-
    narrative_ontology:has_asymmetric_extraction(C).

structural_property_holds(C, has_beneficiaries) :-
    narrative_ontology:constraint_beneficiary(C, _), !.

structural_property_holds(C, has_victims) :-
    narrative_ontology:constraint_victim(C, _), !.

structural_property_holds(C, has_temporal_data) :-
    narrative_ontology:measurement(_, C, _, _, _), !.

structural_property_holds(C, has_theater) :-
    config:param(theater_metric_name, TM),
    narrative_ontology:constraint_metric(C, TM, TR),
    TR > 0.0, !.

% ============================================================================
% DIMENSION 3: NEGATIVE SPACE (Structural Voids)
% ============================================================================
% In logical systems, the ABSENCE of a property is often more diagnostic
% than its presence. A void is not just "missing data" — it's a property
% that SHOULD exist given the constraint's profile but DOESN'T.
%
% Voids reveal mechanism type: a high-extraction constraint without
% accountability structures is a fundamentally different beast than one with them.

%% fingerprint_voids(+C, -Voids)
%  Returns a sorted list of diagnostic structural voids.

fingerprint_voids(C, Voids) :-
    findall(Void, structural_void(C, Void), Raw),
    sort(Raw, Voids).

% --- High extraction without accountability ---
% Extraction > snare floor but no sunset, no alternatives path.
% These are "black holes" — designed to be permanent and unaccountable.
structural_void(C, unaccountable_extraction) :-
    drl_core:base_extractiveness(C, E),
    config:param(snare_epsilon_floor, Floor),
    E >= Floor,
    \+ narrative_ontology:has_sunset_clause(C).

% --- Enforcement without coordination ---
% Active enforcement but no coordination function. Pure coercion —
% suppression exists to suppress, not to coordinate.
structural_void(C, coercion_without_coordination) :-
    drl_core:requires_active_enforcement(C),
    \+ narrative_ontology:has_coordination_function(C).

% --- Suppression without enforcement mechanism ---
% High suppression but nothing maintaining it. Either naturalized (internalized
% piton) or ghost enforcement (the enforcer left but the fear remains).
structural_void(C, unenforced_suppression) :-
    drl_core:get_raw_suppression(C, S),
    config:param(snare_suppression_floor, Floor),
    S >= Floor,
    \+ drl_core:requires_active_enforcement(C),
    \+ drl_core:emerges_naturally(C).

% --- Victims without exit ---
% Documented victims but no alternatives path and trapped context.
% The constraint KNOWS it harms but provides no escape.
structural_void(C, no_exit_for_victims) :-
    narrative_ontology:constraint_victim(C, _),
    \+ narrative_ontology:has_sunset_clause(C),
    drl_core:base_extractiveness(C, E),
    config:param(tangled_rope_epsilon_floor, Floor),
    E >= Floor.

% --- Drift without sunset ---
% Temporal measurement data shows evolution, but no sunset clause.
% A constraint that changes but was never designed to end.
structural_void(C, drifting_without_limit) :-
    narrative_ontology:measurement(_, C, _, _, _),
    \+ narrative_ontology:has_sunset_clause(C),
    drl_core:base_extractiveness(C, E),
    config:param(rope_epsilon_ceiling, Ceil),
    E > Ceil.

% --- Coordination without beneficiaries ---
% Claims coordination function but no documented beneficiaries.
% Who is this coordinating for? Possible theater.
structural_void(C, undocumented_coordination) :-
    drl_core:requires_active_enforcement(C),
    \+ narrative_ontology:constraint_beneficiary(C, _),
    drl_core:get_raw_suppression(C, S),
    S > 0.30.

% --- Mountain claim with extraction ---
% Claimed as natural/immutable but has measurable extraction.
% Possible naturalized snare (false mountain).
structural_void(C, extractive_immutable) :-
    narrative_ontology:constraint_claim(C, mountain),
    drl_core:base_extractiveness(C, E),
    config:param(mountain_extractiveness_max, MaxX),
    E > MaxX.

% --- Asymmetric extraction without enforcement ---
% Documented victims and asymmetric extraction, but no active enforcement.
% The extraction is self-sustaining — possibly the most insidious form.
structural_void(C, self_sustaining_extraction) :-
    narrative_ontology:has_asymmetric_extraction(C),
    drl_core:base_extractiveness(C, E),
    config:param(tangled_rope_epsilon_floor, Floor),
    E >= Floor,
    \+ drl_core:requires_active_enforcement(C).

% ============================================================================
% DIMENSION 4: ACTOR TOPOLOGY
% ============================================================================
% Not WHO the actors are, but the STRUCTURE of actor relationships.
% Abstracted to categorical topology: concentrated vs distributed,
% one-sided vs balanced.

%% fingerprint_actors(+C, -Actors)
%  Returns actors(BeneficiaryTopology, VictimTopology)
%  where topology is one of: none, concentrated, distributed

fingerprint_actors(C, actors(BenefTopology, VictimTopology)) :-
    findall(B, narrative_ontology:constraint_beneficiary(C, B), Bs),
    sort(Bs, UniqueBs),
    length(UniqueBs, BCount),
    count_to_topology(BCount, BenefTopology),
    findall(V, narrative_ontology:constraint_victim(C, V), Vs),
    sort(Vs, UniqueVs),
    length(UniqueVs, VCount),
    count_to_topology(VCount, VictimTopology).

%% count_to_topology(+Count, -Topology)
count_to_topology(0, none).
count_to_topology(1, concentrated).
count_to_topology(N, distributed) :- N >= 2.

% ============================================================================
% DIMENSION 5: TEMPORAL DRIFT
% ============================================================================
% The direction the constraint is moving. Not where it IS but where it's GOING.
% Categorical: rising, stable, falling, unknown.

%% fingerprint_drift(+C, -Drift)
%  Returns drift(ExtractionTrend, SuppressionTrend, TheaterTrend)

fingerprint_drift(C, drift(ExtrTrend, SuppTrend, TheaterTrend)) :-
    metric_trend(C, base_extractiveness, ExtrTrend),
    metric_trend(C, suppression_requirement, SuppTrend),
    config:param(theater_metric_name, TM),
    metric_trend(C, TM, TheaterTrend).

%% metric_trend(+C, +MetricName, -Trend)
%  Determines categorical trend from temporal measurements.
metric_trend(C, Metric, Trend) :-
    findall(T-V, narrative_ontology:measurement(_, C, Metric, T, V), Pairs),
    (   Pairs = [_, _ | _]      % At least 2 measurements
    ->  sort(Pairs, Sorted),
        Sorted = [_-VFirst | _],
        last(Sorted, _-VLast),
        Delta is VLast - VFirst,
        (   Delta > 0.05  -> Trend = rising
        ;   Delta < -0.05 -> Trend = falling
        ;   Trend = stable
        )
    ;   Trend = unknown
    ).

% ============================================================================
% DIMENSION 6: METRIC ZONE
% ============================================================================
% Where the constraint sits in metric space — categorical, not numeric.
% Zone boundaries derived from config.pl thresholds for consistency
% with classify_from_metrics/6.

%% fingerprint_zone(+C, -Zone)
%  Returns zone(ExtractionZone, SuppressionZone)

fingerprint_zone(C, zone(ExtrZone, SuppZone)) :-
    (drl_core:base_extractiveness(C, E) -> extraction_zone(E, ExtrZone) ; ExtrZone = unknown),
    (drl_core:get_raw_suppression(C, S) -> suppression_zone(S, SuppZone) ; SuppZone = unknown).

%% extraction_zone(+Epsilon, -Zone)
%  Categorizes base extraction into named zones using config thresholds.
extraction_zone(E, negligible) :-
    config:param(mountain_extractiveness_max, Ceil),
    E =< Ceil, !.
extraction_zone(E, low) :-
    config:param(rope_epsilon_ceiling, Ceil),
    E =< Ceil, !.
extraction_zone(E, moderate) :-
    config:param(tangled_rope_epsilon_floor, Floor),
    E < Floor, !.
extraction_zone(E, high) :-
    config:param(snare_epsilon_floor, Floor),
    E < Floor, !.
extraction_zone(_, extreme).

%% suppression_zone(+Supp, -Zone)
%  Categorizes suppression into named zones using config thresholds.
suppression_zone(S, negligible) :-
    config:param(mountain_suppression_ceiling, Ceil),
    S =< Ceil, !.
suppression_zone(S, low) :-
    S =< 0.30, !.
suppression_zone(S, moderate) :-
    config:param(tangled_rope_suppression_floor, Floor),
    S < Floor, !.
suppression_zone(S, high) :-
    config:param(snare_suppression_floor, Floor),
    S < Floor, !.
suppression_zone(_, extreme).

% ============================================================================
% DIMENSION 7: COUPLING TOPOLOGY (Boltzmann v5.0)
% ============================================================================
% Maps the independence structure of a constraint across index dimensions.
% Based on Tamuz & Sandomirskiy (2025): the Boltzmann distribution is the
% ONLY law describing unrelated systems. A constraint whose classification
% factorizes across Power × Scope is "Boltzmann-compliant" — its dimensions
% are genuinely independent.
%
% Coupling categories:
%   independent          — classification factorizes (CouplingScore ≤ threshold)
%   weakly_coupled       — minor coupling (threshold < score ≤ strong_threshold)
%   strongly_coupled     — significant coupling (score > strong_threshold)
%   nonsensically_coupled — coupling with no functional justification
%   inconclusive         — insufficient data for reliable test
%
% This dimension enables:
%   - Comparing constraints by coupling structure (isomorphism on coupling)
%   - Detecting "coupling drift" as a lifecycle event
%   - Identifying false Mountains via Boltzmann invariance failure
%   - Distinguishing functional entanglement from extractive coupling

%% fingerprint_coupling(+C, -Coupling)
%  Returns coupling(Category, Score, CoupledPairs, BoltzmannCompliance, PurityScore)
%  where Category is the categorical coupling level, Score is the raw
%  coupling score, and PurityScore is the scalar purity in [0,1] (or -1.0
%  if insufficient data).

fingerprint_coupling(C, coupling(Category, Score, CoupledPairs, Compliance, Purity)) :-
    structural_signatures:epistemic_access_check(C, EpistemicOk),
    (   EpistemicOk == false
    ->  Category = inconclusive,
        Score = unknown,
        CoupledPairs = [],
        Compliance = inconclusive(insufficient_classifications),
        Purity = -1.0
    ;   structural_signatures:cross_index_coupling(C, Score),
        structural_signatures:complexity_adjusted_threshold(C, Threshold),
        config:param(boltzmann_coupling_strong_threshold, StrongThreshold),
        categorize_coupling(Score, Threshold, StrongThreshold, C, Category),
        (   structural_signatures:detect_nonsensical_coupling(C, CoupledPairs, _)
        ->  true
        ;   CoupledPairs = []
        ),
        structural_signatures:boltzmann_compliant(C, Compliance),
        structural_signatures:purity_score(C, Purity)
    ).

%% categorize_coupling(+Score, +Threshold, +StrongThreshold, +C, -Category)
%  Converts raw coupling score into categorical coupling level.
%  Checks for "nonsensical" coupling: strong coupling with no
%  functional justification (no coordination function declared).
categorize_coupling(Score, Threshold, _, _, independent) :-
    Score =< Threshold, !.
categorize_coupling(Score, _, StrongThreshold, C, nonsensically_coupled) :-
    Score > StrongThreshold,
    \+ narrative_ontology:has_coordination_function(C), !.
categorize_coupling(Score, _, StrongThreshold, _, strongly_coupled) :-
    Score > StrongThreshold, !.
categorize_coupling(_, _, _, _, weakly_coupled).

% ============================================================================
% CONSTRAINT DISCOVERY
% ============================================================================
% Discovers all constraint IDs in the corpus. Uses constraint_metric/3 as
% the universal source (every properly formatted file has these), with
% fallback to constraint_claim/2. Filters to atoms only — some legacy files
% wrap IDs in list brackets which would break downstream unification.

%% known_constraint(-C)
%  Enumerates all discoverable constraint IDs (atoms only, deduplicated).
%  Use findall + sort for the full list, or call directly for backtracking.
known_constraint(C) :-
    known_constraint_raw(C),
    atom(C).

known_constraint_raw(C) :-
    narrative_ontology:constraint_metric(C, _, _).
known_constraint_raw(C) :-
    narrative_ontology:constraint_claim(C, _).
known_constraint_raw(C) :-
    constraint_indexing:constraint_classification(C, _, _).

% ============================================================================
% GROUPING UTILITIES
% ============================================================================
% These enable the core use case: find constraints that share the same
% logical shape, then examine their subject matter for surprises.

%% shift_family(+Pattern, -Constraints)
%  Finds all constraints matching a specific shift pattern.
%  Use all_shift_patterns/1 to discover existing patterns first,
%  or use partial unification: shift_family(shift(snare, _, rope, _), Cs).
shift_family(Pattern, Constraints) :-
    findall(C,
            (known_constraint(C),
             fingerprint_shift(C, Pattern)),
            CList),
    sort(CList, Constraints),
    Constraints \= [].

%% all_shift_patterns(-Patterns)
%  Returns all distinct shift patterns present in the corpus.
%  Each pattern appears once. Use shift_family/2 to get members.
all_shift_patterns(Patterns) :-
    findall(P,
            (known_constraint(C),
             fingerprint_shift(C, P)),
            All),
    sort(All, Patterns).

%% fingerprint_match(+C1, +C2, +Dimensions, -Matches)
%  Checks which dimensions match between two constraints.
%  Dimensions is a list of atoms: [shift, properties, voids, actors, drift, zone]
%  Returns list of dimension-match pairs.
%
%  Example: fingerprint_match(copyleft, hoa_covenants, [shift, actors], Matches)
%  Matches = [shift-true, actors-false] (or similar)

fingerprint_match(C1, C2, Dimensions, Matches) :-
    C1 \= C2,
    findall(Dim-Result,
            (member(Dim, Dimensions),
             dimension_matches(C1, C2, Dim, Result)),
            Matches).

%% dimension_matches(+C1, +C2, +Dimension, -Result)
%  Tests whether a specific dimension matches between two constraints.
dimension_matches(C1, C2, shift, Result) :-
    fingerprint_shift(C1, S1),
    fingerprint_shift(C2, S2),
    (S1 = S2 -> Result = true ; Result = false).

dimension_matches(C1, C2, properties, Result) :-
    fingerprint_properties(C1, P1),
    fingerprint_properties(C2, P2),
    (P1 = P2 -> Result = true ; Result = false).

dimension_matches(C1, C2, voids, Result) :-
    fingerprint_voids(C1, V1),
    fingerprint_voids(C2, V2),
    (V1 = V2 -> Result = true ; Result = false).

dimension_matches(C1, C2, actors, Result) :-
    fingerprint_actors(C1, A1),
    fingerprint_actors(C2, A2),
    (A1 = A2 -> Result = true ; Result = false).

dimension_matches(C1, C2, drift, Result) :-
    fingerprint_drift(C1, D1),
    fingerprint_drift(C2, D2),
    (D1 = D2 -> Result = true ; Result = false).

dimension_matches(C1, C2, zone, Result) :-
    fingerprint_zone(C1, Z1),
    fingerprint_zone(C2, Z2),
    (Z1 = Z2 -> Result = true ; Result = false).

dimension_matches(C1, C2, coupling, Result) :-
    fingerprint_coupling(C1, coupling(Cat1, _, _, _, _)),
    fingerprint_coupling(C2, coupling(Cat2, _, _, _, _)),
    (Cat1 = Cat2 -> Result = true ; Result = false).

% ============================================================================
% CONVENIENCE PREDICATES
% ============================================================================

%% print_fingerprint(+C)
%  Pretty-prints the logical fingerprint for a constraint.
print_fingerprint(C) :-
    format('~n=== Logical Fingerprint: ~w ===~n', [C]),
    (fingerprint_shift(C, shift(Pw, Mod, Inst, An))
    -> format('  Shift (computed via dr_type/3):~n'),
       format('    powerless=~w  moderate=~w  institutional=~w  analytical=~w~n',
              [Pw, Mod, Inst, An])
    ;  format('  Shift:      (unable to compute)~n')),
    (fingerprint_properties(C, Props)
    -> format('  Properties: ~w~n', [Props])
    ;  format('  Properties: (none)~n')),
    (fingerprint_voids(C, Voids)
    -> format('  Voids:      ~w~n', [Voids])
    ;  format('  Voids:      (none)~n')),
    (fingerprint_actors(C, actors(BT, VT))
    -> format('  Actors:     beneficiaries=~w  victims=~w~n', [BT, VT])
    ;  format('  Actors:     (unknown)~n')),
    (fingerprint_drift(C, drift(ET, ST, TT))
    -> format('  Drift:      extraction=~w  suppression=~w  theater=~w~n', [ET, ST, TT])
    ;  format('  Drift:      (no temporal data)~n')),
    (fingerprint_zone(C, zone(EZ, SZ))
    -> format('  Zone:       extraction=~w  suppression=~w~n', [EZ, SZ])
    ;  format('  Zone:       (unknown)~n')),
    (fingerprint_coupling(C, coupling(Cat, Score, Pairs, Comp, Purity))
    -> (Score = unknown
       -> format('  Coupling:   ~w (insufficient data)~n', [Cat])
       ;  Purity =:= -1.0
       -> format('  Coupling:   ~w (score=~3f, pairs=~w, boltzmann=~w)~n',
                  [Cat, Score, Pairs, Comp]),
          format('  Purity:     inconclusive~n')
       ;  format('  Coupling:   ~w (score=~3f, pairs=~w, boltzmann=~w)~n',
                  [Cat, Score, Pairs, Comp]),
          purity_zone(Purity, PurityZone),
          format('  Purity:     ~3f (~w)~n', [Purity, PurityZone])
       )
    ;  format('  Coupling:   (unable to compute)~n')),
    format('~n').

%% purity_zone(+Score, -Zone)
%  Categorizes purity score into named zones for display.
purity_zone(S, pristine)     :- S >= 0.90, !.
purity_zone(S, sound)        :- S >= 0.70, !.
purity_zone(S, borderline)   :- S >= 0.50, !.
purity_zone(S, contaminated) :- S >= 0.30, !.
purity_zone(_, degraded).
