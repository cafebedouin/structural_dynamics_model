% ============================================================================
% CONSTRAINT STORY: allometric_scaling_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_allometric_scaling_law, []).

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
 *   constraint_id: allometric_scaling_law
 *   human_readable: Allometric Scaling Law (Kleiber's Law and Related Power-Law Relationships)
 *   domain: biology/physics/mathematics
 *
 * SUMMARY:
 *   Allometric scaling laws—epitomized by Kleiber's law (basal metabolic rate
 *   scales as body mass^0.67)—describe how biological properties scale with
 *   body size across species. This constraint is a paradigmatic natural law:
 *   the exponent is universal, reproduced across millions of species, stable
 *   across evolutionary timescales, and derivable from first principles in
 *   physics and dimensional analysis. The constraint has zero degrees of
 *   freedom for any organism—a mouse cannot negotiate its metabolic rate, a
 *   whale cannot escape the scaling relationship, and no evolutionary
 *   strategy can circumvent the fundamental geometry of
 *   surface-area-to-volume relationships. The extractiveness metric is
 *   near-zero because there is no extraction mechanism—no agent benefits at
 *   another's expense. The suppression is minimal because there is nothing to
 *   resist. The theater ratio is low because the constraint is functionally
 *   transparent: the scaling follows directly from measurable physical
 *   quantities with no performative overlay. Allometric scaling exemplifies a
 *   constraint where all perspectives converge on the same type across all
 *   observation contexts.
 *
 * KEY AGENTS:
 *   - Biological organisms: Subjects of the constraint (powerless/trapped) — metabolic rate is determined, not chosen
 *   - Evolutionary populations: Constrained by scaling (powerful/mobile) — selection operates within the scaling relationship but cannot escape it
 *   - Measurement systems: Observers of the scaling (analytical/analytical) — reproducibly detect the exponent across methodologies
 *   - Research communities: Institutional interpreters (institutional/arbitrage) — generate explanatory theories but cannot displace the empirical regularity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(allometric_scaling_law, 0.12).
domain_priors:suppression_score(allometric_scaling_law, 0.03).
domain_priors:theater_ratio(allometric_scaling_law, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(allometric_scaling_law, extractiveness, 0.12).
narrative_ontology:constraint_metric(allometric_scaling_law, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(allometric_scaling_law, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(allometric_scaling_law, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(allometric_scaling_law, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(allometric_scaling_law, mountain).
narrative_ontology:human_readable(allometric_scaling_law, "Allometric Scaling Law (Kleiber's Law and Related Power-Law Relationships)").
narrative_ontology:topic_domain(allometric_scaling_law, "biology/physics/mathematics").

domain_priors:emerges_naturally(allometric_scaling_law).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORGANISM (MOUNTAIN) — No organism can escape metabolic scaling. An individual mouse's metabolic rate is constrained by its body mass raised to approximately the 2/3 power, regardless of species, environment, or evolutionary history. The constraint is not negotiable, not circumventable, and not dependent on institutional framing. Zero degrees of freedom.
constraint_indexing:constraint_classification(allometric_scaling_law, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From dimensional analysis and scaling theory, allometric relationships are inevitable consequences of physical law. The exponent follows from constraints on surface-area-to-volume relationships, fractal-like branching networks, and conservation laws. The scaling is not enforced by any agent—it emerges from the structure of geometry and physics itself. No alternative formulation exists within known physical law.
constraint_indexing:constraint_classification(allometric_scaling_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: EVOLUTIONARY POPULATION (MOUNTAIN) — Natural selection cannot circumvent allometric constraints. Populations subject to metabolic scaling show fitness-dependent survival across all body sizes—the constraint is universal and applies equally to competing lineages. Selection optimizes within the constraint but cannot escape it. The constraint is prior to strategy.
constraint_indexing:constraint_classification(allometric_scaling_law, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: RESEARCH COMMUNITY (MOUNTAIN) — Biologists, physicists, and physiologists attempting to predict metabolic rates encounter the scaling law as an invariant—reproducible across species, across geologic timescales, and across measurement methodologies. No amount of institutional effort or alternative framework displaces the empirical regularity. The constraint is observable-invariant.
constraint_indexing:constraint_classification(allometric_scaling_law, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(allometric_scaling_law_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(allometric_scaling_law, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(allometric_scaling_law, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(allometric_scaling_law, ExtMetricName, E),
    domain_priors:suppression_score(allometric_scaling_law, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(allometric_scaling_law),
    narrative_ontology:constraint_metric(allometric_scaling_law, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(allometric_scaling_law, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(allometric_scaling_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Near-zero. Allometric scaling is not extractive—no organism exploits another through the constraint, no coalition captures benefits at the expense of a vulnerable group. The low value reflects that this is a pure natural limit, not a distribution mechanism. Suppression (0.03): Near-zero. The constraint is not coercive; it is physical law. Organisms do not experience suppression because they have no choice to resist—the scaling is prior to agency. Theater ratio (0.15): Very low. The scaling is functionally transparent. The relationship between mass and metabolic rate is directly measurable and derivable from first principles. There is minimal performative content—the constraint is what it appears to be. Claimed type (Mountain): Accessibility collapse (0.92) indicates that the constraint is not accessible to modification—no organism can change its scaling exponent. Resistance (0.08) indicates minimal opposition to the constraint as fact—scientific consensus is strong. Emerges naturally (true): The constraint follows from geometry and physics, not institutional design. All three NL profile requirements are satisfied, certifying mountain status.
 *
 * PERSPECTIVAL GAP:
 *   Unlike most constraints that show perspectival divergence, allometric scaling classifies identically as mountain across ALL perspectives. The powerless organism, the evolutionary population, the measurement community, and the analytical observer all perceive the same universal, immutable relationship. This uniformity is diagnostic: allometric scaling is a rare case where the constraint is truly observation-invariant. The lack of perspectival gap confirms that the constraint is not contingent on institutional framing, measurement methodology (within reason), or observer position. Every perspective returns mountain because the constraint is prior to all observers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to this constraint because it is a pure natural law with no extraction mechanism. There is no beneficiary receiving disproportionate value, no victim bearing disproportionate cost, and no institutional asymmetry in how the constraint operates. The constraint applies identically to all organisms of a given body mass, regardless of species, environment, or power status. The d parameter (directionality) would be undefined or neutral (0.5) because there is no structural relationship flowing from one agent to another—the constraint is simply a property of physical reality.
 *
 * MANDATROPHY ANALYSIS:
 *   Allometric scaling resolves the mandatrophy trivially: it is purely coordination-free. There is no hybrid extraction-coordination function to disambiguate. The constraint is a pure natural limit with zero mandatrophy risk. The classification as mountain is confirmed by all metrics and all perspectives, eliminating the possibility that institutional framing is naturalizing what is actually a contingent arrangement. This constraint serves as a control case—the benchmark against which more complex constraints (ropes, snares, tangles) are measured. If a constraint shows perspectival divergence or if different measurement methodologies yield different classifications, it is not an allometric law; it is something more contingent and more prone to institutional capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanistic_explanation_gap,
    'Does the 2/3 scaling exponent reflect a single universal mechanism (e.g., fractal branching, surface-area constraint) or multiple mechanisms that converge on the same exponent across different phyla?',
    'Detailed mechanistic models of metabolic networks across taxa; comparison of actual branching geometries in real circulatory systems vs fractal predictions; identification of taxa that deviate from 2/3 and their alternative mechanisms',
    'If single mechanism: the law is a direct consequence of a single physical principle, confirming deep mountain status. If multiple mechanisms: the exponent is a convergent solution to multiple independent constraints, still a mountain but with epistemic uncertainty about the fundamental cause.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mechanistic_explanation_gap, empirical, 'Whether 2/3 exponent reflects single or multiple underlying mechanisms').

omega_variable(
    exponent_universality_bounds,
    'What is the true range of allometric exponents across all biological systems? Are deviations from 2/3 measurement error, taxon-specific biology, or evidence of multiple scaling laws operating in parallel?',
    'Meta-analysis of thousands of published allometric measurements; separation of measurement error from biological variation; phylogenetic comparative analysis controlling for shared evolutionary history',
    'If all exponents cluster tightly around 2/3 with variation < 0.05: single universal law, mountain classification confirmed. If exponents vary systematically by clade or metabolic strategy: multiple constraints operating, possibly requiring decomposition into family of related laws.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exponent_universality_bounds, empirical, 'Range and universality of allometric exponents across biology').

omega_variable(
    measurement_scale_artifacts,
    'Do apparent exponent values depend on measurement methodology (basal vs field metabolic rate, fasting vs feeding state, environmental temperature, body size range of sample)?',
    'Standardized measurement protocols applied to same organisms under varying conditions; analysis of how exponent estimates change with body size range sampled; comparison of laboratory vs field measurements',
    'If methodology-dependent: the observed exponent is a measurement artifact, and the true constraint may be weaker or operate differently. If methodology-invariant: the exponent is robust, confirming mountain status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_scale_artifacts, empirical, 'Whether allometric exponents depend on measurement methodology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(allometric_scaling_law, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(allom_tr_t0, allometric_scaling_law, theater_ratio, 0, 0.12).
narrative_ontology:measurement(allom_tr_t50, allometric_scaling_law, theater_ratio, 50, 0.14).
narrative_ontology:measurement(allom_tr_t100, allometric_scaling_law, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(allom_be_t0, allometric_scaling_law, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(allom_be_t50, allometric_scaling_law, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(allom_be_t100, allometric_scaling_law, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(allometric_scaling_law, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
