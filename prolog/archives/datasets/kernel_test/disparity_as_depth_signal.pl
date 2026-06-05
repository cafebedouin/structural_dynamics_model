% ============================================================================
% CONSTRAINT STORY: disparity_as_depth_signal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_disparity_as_depth_signal, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: disparity_as_depth_signal
 *   human_readable: Disparity as Depth Signal in Multi-Position Observation
 *   domain: epistemology/standpoint_theory/philosophy_of_knowledge
 *
 * SUMMARY:
 *   The principle that disparity between observation positions encodes
 *   structural information rather than error is a foundational constraint in
 *   epistemology, perception science, and measurement theory. Stereoscopic
 *   vision depends on the fact that the displacement between left-eye and
 *   right-eye images carries depth information. Parallax measurement in
 *   astronomy depends on the fact that a star's apparent position shifts
 *   systematically when observed from different points in Earth's orbit, and
 *   the magnitude of shift encodes distance. Multi-baseline interferometry
 *   depends on the fact that phase differences between spatially separated
 *   detectors encode source structure. Standpoint theory in social
 *   epistemology claims the same principle applies to knowledge production:
 *   systematic differences in perspective between observers occupying
 *   different social positions encode information about social structure
 *   rather than representing error to be eliminated through consensus. This
 *   constraint is presented as a mountain — a natural law that operates
 *   independently of observer intention or theoretical commitment. However,
 *   the presence of identifiable beneficiaries (standpoint theorists whose
 *   framework gains legitimacy, multi-position analysts whose methods gain
 *   justification) triggers the false summit detector. The critical empirical
 *   question is whether the constraint operates independently of the
 *   theoretical frameworks that describe it, or whether it is a constructed
 *   principle that serves the interests of those who advocate for
 *   multi-position analysis.
 *
 * KEY AGENTS:
 *   - Standpoint Theorists: Primary beneficiary (institutional/arbitrage) — framework gains epistemic legitimacy if disparity-as-signal is a natural law rather than a theoretical choice
 *   - Multi-Position Analysts: Secondary beneficiary (organized/constrained) — methodological pluralism justified if disparity carries structural information
 *   - Stereoscopic Observers: Physical instantiation (analytical/analytical) — binocular vision, parallax measurement, interferometry all depend on the same geometric principle
 *   - Naive Observer: Pre-theoretical agent (powerless/trapped) — experiences depth perception through binocular disparity without conceptual framework
 *   - Empirical Researcher: Methodological agent (moderate/mobile) — encounters disparity-as-signal as a practical constraint in multi-site or multi-method research
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(disparity_as_depth_signal, 0.08).
domain_priors:suppression_score(disparity_as_depth_signal, 0.03).
domain_priors:theater_ratio(disparity_as_depth_signal, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(disparity_as_depth_signal, extractiveness, 0.08).
narrative_ontology:constraint_metric(disparity_as_depth_signal, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(disparity_as_depth_signal, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(disparity_as_depth_signal, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(disparity_as_depth_signal, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(disparity_as_depth_signal, mountain).
narrative_ontology:human_readable(disparity_as_depth_signal, "Disparity as Depth Signal in Multi-Position Observation").
narrative_ontology:topic_domain(disparity_as_depth_signal, "epistemology/standpoint_theory/philosophy_of_knowledge").

domain_priors:emerges_naturally(disparity_as_depth_signal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(disparity_as_depth_signal, standpoint_theorists).
narrative_ontology:constraint_beneficiary(disparity_as_depth_signal, multi_position_analysts).
narrative_ontology:constraint_beneficiary(disparity_as_depth_signal, stereoscopic_observers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — The constraint that disparity between perspectives carries structural information rather than error is a mathematical and epistemic necessity. Stereoscopic vision, parallax measurement, and multi-baseline interferometry all depend on the same principle: displacement between observation positions encodes depth. This is not a contingent feature of human perception or social epistemology but a geometric fact about how spatial structure projects onto observation planes. The constraint emerges from the mathematics of projection and cannot be bypassed by any observer.
constraint_indexing:constraint_classification(disparity_as_depth_signal, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: STANDPOINT THEORIST (MOUNTAIN) — Even from the beneficiary position (standpoint theory gains epistemic legitimacy from this principle), the constraint is experienced as immutable. The theorist cannot choose whether disparity carries information — they can only choose whether to attend to it. The constraint operates independently of theoretical commitment: a researcher who believes all perspectives should converge will still observe structured disparity when measuring from genuinely different structural positions. The benefit is in recognizing what already exists, not in constructing it.
constraint_indexing:constraint_classification(disparity_as_depth_signal, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: EMPIRICAL RESEARCHER (MOUNTAIN) — A social scientist conducting multi-site ethnography or a physicist calibrating detector arrays encounters the same constraint: when observation positions differ structurally, the measurements differ systematically, and the pattern of difference encodes information about the observed system's structure. The researcher can choose to ignore disparity (treating it as noise) or attend to it (treating it as signal), but cannot choose whether the disparity exists or whether it carries information. The constraint is experienced as a methodological fact, not a theoretical choice.
constraint_indexing:constraint_classification(disparity_as_depth_signal, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: NAIVE OBSERVER (MOUNTAIN) — An observer with no theoretical framework for interpreting multi-position data still experiences the constraint. A child learning depth perception through binocular vision, a navigator using landmarks from different vantage points, or a community member noticing that different social positions yield different accounts of the same event — all encounter disparity as depth signal before they have language or theory to describe it. The constraint operates at the perceptual level, prior to conceptual elaboration. Immutability is experienced as 'this is just how seeing works' rather than as a theoretical claim.
constraint_indexing:constraint_classification(disparity_as_depth_signal, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: ORGANIZED METHODOLOGICAL PLURALIST (MOUNTAIN) — Research communities that have institutionalized multi-method triangulation (mixed-methods research, multi-modal sensing, cross-disciplinary collaboration) experience the constraint as a foundational principle that justifies their organizational structure. The pluralist framework exists because disparity-as-signal is real; the framework does not create the constraint. Even if the community disbanded, researchers returning to single-method approaches would still observe that adding a second method from a different position produces systematic disparity that encodes structural information about the phenomenon.
constraint_indexing:constraint_classification(disparity_as_depth_signal, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(disparity_as_depth_signal_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(disparity_as_depth_signal, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(disparity_as_depth_signal, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(disparity_as_depth_signal, ExtMetricName, E),
    domain_priors:suppression_score(disparity_as_depth_signal, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(disparity_as_depth_signal),
    narrative_ontology:constraint_metric(disparity_as_depth_signal, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(disparity_as_depth_signal, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(disparity_as_depth_signal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint operates as a geometric and epistemic necessity with minimal extraction. Standpoint theorists benefit from the constraint's existence (their framework describes a real phenomenon rather than constructing a theoretical artifact), but this benefit is not extractive — it is the reward for accurate description. The constraint does not require active enforcement, does not suppress alternatives, and does not concentrate benefits through coercion. The low extractiveness reflects that this is primarily a coordination mechanism (enabling depth perception, parallax measurement, multi-position analysis) with minimal overhead. Suppression (0.03): Minimal. Observers can choose to ignore disparity (treating it as noise to be averaged away), but the constraint does not prevent this choice. Single-position observation remains possible; the constraint only specifies that depth information is lost when disparity is eliminated. No active enforcement mechanism exists. Accessibility collapse (0.92): Very high. The constraint is accessible to any observer who compares measurements from different positions. No specialized training, equipment, or institutional access is required to observe that different vantage points yield different views and that the pattern of difference encodes spatial structure. A child learning depth perception, a navigator using landmarks, and a physicist calibrating detector arrays all encounter the same principle. Resistance (0.08): Very low. The constraint cannot be bypassed by any observer. Attempts to eliminate disparity (averaging across positions, enforcing consensus, treating difference as error) result in loss of depth information, confirming the constraint's operation. The constraint is not a social convention that could be renegotiated or a policy that could be repealed. Theater ratio (0.15): Low. The constraint operates with minimal performative content. Depth perception through binocular vision, parallax measurement in astronomy, and multi-baseline interferometry are functional mechanisms with little ritual or symbolic overhead. Standpoint theory adds some theoretical elaboration, but the core principle (disparity encodes structure) operates independently of theoretical framing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all five perspectives classify as mountain. The uniformity reflects that the constraint operates as a geometric necessity independently of observer power, time horizon, or exit options. The standpoint theorist benefits from the constraint's existence but cannot construct or modify it. The naive observer experiences the constraint pre-theoretically. The empirical researcher encounters it as a methodological fact. The organized pluralist institutionalizes methods that depend on it. The analytical observer recognizes it as a projection geometry principle. The absence of perspectival gap is itself diagnostic: genuine natural laws appear immutable from all positions. The false summit question is whether this uniformity reflects true immutability or successful naturalization of a constructed principle.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives classify as mountain because the constraint operates as a geometric and epistemic necessity independently of observer position. The standpoint theorist is a beneficiary (the framework gains legitimacy from describing a real phenomenon), but this does not change the classification — the constraint is not constructed to serve the theorist's interests; the theorist's interests are served by accurately describing a pre-existing constraint. The directionality values reflect structural position (beneficiaries have low d, naive observers have high d), but the effective extraction chi remains low across all positions because suppression is minimal and the constraint emerges naturally. The false summit detector will flag this constraint because beneficiaries are declared on a mountain, triggering investigation of whether the 'natural law' framing naturalizes a contingent theoretical construction. The critical test is whether the constraint operates in domains where standpoint theory has no influence (physical measurement systems, animal depth perception, pre-theoretical human perception).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that very low extraction (0.08) combined with natural emergence and minimal suppression produces mountain classification from all perspectives. The mandatrophy risk is not 'is this extraction or coordination?' but 'is this a genuine natural law or a naturalized construction?' The presence of beneficiaries (standpoint theorists, multi-position analysts) triggers the false summit detector, routing the constraint to empirical investigation: does the principle operate independently of the theoretical frameworks that describe it? If yes, the mountain classification stands and the beneficiaries are simply those who accurately described a pre-existing constraint. If no, reclassification to tangled_rope is warranted (a theoretical construction with genuine coordination function but also serving theorists' interests). The omega variables document this irreducible uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    false_summit_beneficiary_construction,
    'Does the presence of identifiable beneficiaries (standpoint theorists, multi-position analysts) indicate that this constraint is a constructed theoretical claim rather than a genuine natural law?',
    'Test whether the constraint operates independently of theoretical commitment: do researchers who reject standpoint theory still observe structured disparity when measuring from different positions? Do physical systems (stereoscopic cameras, parallax measurements, interferometry) exhibit the same principle without human interpretation?',
    'If the constraint operates only within standpoint-theory frameworks, reclassify as tangled_rope (theoretical construction with genuine coordination function but also serving theorists'' interests). If the constraint operates independently across physical, perceptual, and social domains, confirm mountain (natural law that standpoint theory describes rather than constructs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_beneficiary_construction, empirical, 'Whether beneficiary presence indicates constructed vs. natural constraint').

omega_variable(
    disparity_persistence_threshold,
    'What threshold of disparity persistence distinguishes signal (structured difference encoding depth) from noise (measurement error converging toward true value)?',
    'Statistical analysis of multi-position measurements: does disparity magnitude scale with baseline separation (signal) or decrease with sample size (noise)? Does the disparity pattern remain stable across repeated measurements from the same positions?',
    'If no clear threshold exists, the signal/noise distinction may be observer-dependent rather than structural. If threshold is identifiable, it provides an empirical test for when disparity carries information.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disparity_persistence_threshold, empirical, 'Threshold for distinguishing structured disparity from convergent noise').

omega_variable(
    projection_geometry_universality,
    'Is the disparity-as-depth principle universal across all projection geometries, or does it depend on specific assumptions about the observation space?',
    'Mathematical analysis: does the principle hold in non-Euclidean geometries, curved spacetimes, or abstract observation spaces? Empirical test: do domains with non-standard projection geometries (quantum measurement, social network topology) exhibit the same disparity-depth relationship?',
    'If geometry-dependent, the constraint may be a special case rather than a universal law. If universal, the mountain classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(projection_geometry_universality, conceptual, 'Whether disparity-depth principle is geometry-universal or domain-specific').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(disparity_as_depth_signal, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(disp_depth_tr_t0, disparity_as_depth_signal, theater_ratio, 0, 0.15).
narrative_ontology:measurement(disp_depth_tr_t25, disparity_as_depth_signal, theater_ratio, 25, 0.15).
narrative_ontology:measurement(disp_depth_tr_t50, disparity_as_depth_signal, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(disp_depth_be_t0, disparity_as_depth_signal, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(disp_depth_be_t25, disparity_as_depth_signal, base_extractiveness, 25, 0.08).
narrative_ontology:measurement(disp_depth_be_t50, disparity_as_depth_signal, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(disparity_as_depth_signal, information_standard).
narrative_ontology:affects_constraint(disparity_as_depth_signal, standpoint_epistemic_privilege).
narrative_ontology:affects_constraint(disparity_as_depth_signal, parallax_distance_measurement).
narrative_ontology:affects_constraint(disparity_as_depth_signal, stereoscopic_depth_perception).

% DUAL FORMULATION NOTE:
% This constraint is the foundational principle underlying multiple domain-specific instantiations. Stereoscopic depth perception, parallax measurement, and standpoint epistemology all depend on the same geometric fact: displacement between observation positions encodes structural information about the observed system. Each domain-specific constraint has its own extractiveness reflecting the institutional arrangements around that application, but all inherit the core principle's mountain status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
