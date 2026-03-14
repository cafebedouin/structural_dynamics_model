% ============================================================================
% CONSTRAINT STORY: solid_state_phased_array_steering
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_solid_state_phased_array_steering, []).

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
 *   constraint_id: solid_state_phased_array_steering
 *   human_readable: Solid State Phased Array Steering
 *   domain: physics/electromagnetics
 *
 * SUMMARY:
 *   Solid state phased array steering is a fundamental constraint arising
 *   from the physics of electromagnetic wave interference. A phased array
 *   achieves directional steering by controlling the phase offset between
 *   elements in an antenna array. The main lobe (primary radiation pattern)
 *   can be steered by varying this phase offset, governed by the Grating
 *   Equation. However, this same equation produces grating lobes — unwanted
 *   secondary radiation patterns at fixed angles determined by wavelength and
 *   element spacing. This constraint is present in every phased array
 *   implementation and represents an irreducible aspect of how wave
 *   interference works in discrete linear structures. The constraint exhibits
 *   zero degrees of freedom across all indices: no amount of engineering
 *   ingenuity, computational resources, or design innovation can eliminate
 *   the fundamental interference pattern. The constraint remains stable
 *   across time (measured theater ratio = 0.15, minimal drift), reflecting
 *   that the underlying physics has not changed and no institutional
 *   narrative has emerged to reframe or replace it.
 *
 * KEY AGENTS:
 *   - Physicists and Electromagnetic Theorists: Analytical observers (analytical/analytical) — formalize and verify the grating equation as invariant
 *   - Hardware Engineers: Powerful to moderate agents (powerful/mobile to moderate/constrained) — must design systems that accommodate grating limits
 *   - System Designers: Moderate agents (moderate/constrained) — optimize within grating constraint boundaries
 *   - Military/Radar Applications: Institutional actors (institutional/arbitrage) — benefit from steering capability despite grating limitation
 *   - Measurement and Standards Bodies: Institutional actors (institutional/arbitrage) — establish performance baselines that incorporate grating effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(solid_state_phased_array_steering, 0.18).
domain_priors:suppression_score(solid_state_phased_array_steering, 0.03).
domain_priors:theater_ratio(solid_state_phased_array_steering, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(solid_state_phased_array_steering, extractiveness, 0.18).
narrative_ontology:constraint_metric(solid_state_phased_array_steering, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(solid_state_phased_array_steering, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(solid_state_phased_array_steering, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(solid_state_phased_array_steering, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(solid_state_phased_array_steering, mountain).
narrative_ontology:human_readable(solid_state_phased_array_steering, "Solid State Phased Array Steering").
narrative_ontology:topic_domain(solid_state_phased_array_steering, "physics/electromagnetics").

domain_priors:emerges_naturally(solid_state_phased_array_steering).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PHYSICAL LAW — Phase interference of electromagnetic waves in a linear array produces a grating pattern with fixed main lobe and grating lobe positions determined by the Grating Equation: sin(θ) = (λ/d)[m + (φ/2π)] where φ is the phase offset between elements. This relationship is invariant across all implementations and scales. No degrees of freedom exist to escape the grating structure itself.
constraint_indexing:constraint_classification(solid_state_phased_array_steering, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: WELL-RESOURCED ENGINEER — Even with unlimited resources and design freedom, the grating equation constraint persists. Design can optimize element spacing, frequency selection, and control algorithms, but cannot eliminate the fundamental interference pattern. The constraint appears immutable regardless of implementation approach.
constraint_indexing:constraint_classification(solid_state_phased_array_steering, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: HARDWARE DEVELOPER — Must work within the grating equation limits. Cannot design around the constraint through engineering choices alone. The physical reality of wave interference provides zero exit option — development resources and timelines must accommodate the fundamental limits.
constraint_indexing:constraint_classification(solid_state_phased_array_steering, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(solid_state_phased_array_steering_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(solid_state_phased_array_steering, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(solid_state_phased_array_steering, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(solid_state_phased_array_steering, ExtMetricName, E),
    domain_priors:suppression_score(solid_state_phased_array_steering, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(solid_state_phased_array_steering),
    narrative_ontology:constraint_metric(solid_state_phased_array_steering, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(solid_state_phased_array_steering, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(solid_state_phased_array_steering_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The grating equation produces a structural constraint but does not extract resources or impose asymmetric costs. The constraint is symmetrical — all agents (designers, users, researchers) experience the same immutable limits. There is no beneficiary capturing value from victims; the constraint is a pure natural limitation. Theater ratio (0.15): Very low. The constraint requires minimal performative activity — its existence is directly verifiable through physical measurement. No institutional narrative, regulatory framework, or organizational theater is needed to maintain the constraint. Suppression (0.03): Minimal. While the constraint limits design freedom, it does not suppress information flow, alternative approaches, or exit options beyond what the physics naturally requires. Accessibility collapse (0.92): Very high. The grating equation is accessible only through rigorous mathematical and experimental understanding; lay understanding of wave physics is minimal. However, the constraint's reality is universal and unambiguous — accessibility to the physics is difficult, but accessibility to the constraint's real effects is complete. Resistance (0.08): Very low. The constraint encounters no active resistance — no actor benefits from denying its existence or maintaining false beliefs about it. The scientific consensus on grating patterns is stable and high-fidelity.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify identically as mountain because the constraint is invariant across all observation positions. A well-resourced engineer cannot escape the grating equation through resource investment. A constrained hardware developer experiences the same grating limits as an unconstrained designer with infinite resources. The analytical observer sees the same immutable physics that shapes the engineer's actual design tradeoffs. This uniformity across perspectives is the defining characteristic of a true natural law constraint — no perspectival leverage exists.
 *
 * DIRECTIONALITY LOGIC:
 *   The grating equation constraint exhibits zero directionality differentiation. All agents — regardless of power, exit options, or scope — experience the same structural limit. The constraint is neither extractive (no beneficiary/victim relationship) nor coordinate (no agent benefits from others' compliance). It is a symmetrical physical reality. The absence of beneficiary and victim declarations reflects this structural neutrality: the constraint does not extract value from one agent to another.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy in the most straightforward manner — as a genuine natural law with no extractive or coordinative function. The false summit (mountain misclassified as a performance narrative) does not apply here. The actual narrative would be: 'The grating equation is an inconvenient physical reality, so we use design techniques to suppress or mitigate grating lobes.' This narrative does not naturalize a contingent institutional arrangement; it acknowledges the constraint and describes legitimate engineering responses. No mandatrophy exists to resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grating_mitigation_effectiveness,
    'Do advanced techniques (random element spacing, non-uniform arrays, frequency hopping) actually eliminate grating lobes or only suppress them?',
    'Rigorous electromagnetic simulation and measurement of suppressed vs relocated grating lobes across design parameter space',
    'If grating lobes are relocated but not eliminated: mountain classification confirmed. If techniques achieve true elimination: classify as rope (coordination with wave physics rather than immutable constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grating_mitigation_effectiveness, empirical, 'Whether mitigation techniques eliminate or only suppress grating lobes').

omega_variable(
    wavelength_dependency_universality,
    'Is the grating equation constraint truly wavelength-independent or does behavior differ significantly at extreme frequency ranges (THz, microwave)?',
    'Experimental verification of grating pattern structure across frequency spectrum from microwave to optical domains',
    'If universal: mountain classification strengthened. If domain-dependent: constraint may decompose into separate stories per frequency regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wavelength_dependency_universality, empirical, 'Whether grating equation holds universally across frequency ranges').

omega_variable(
    design_freedom_boundary,
    'What is the minimal set of design constraints required to produce a functional phased array, and can this set be further reduced?',
    'Historical review of phased array designs and parameter space exploration for minimum achievable constraint coupling',
    'If design freedom is truly zero: mountain classification robust. If residual degrees of freedom exist: classify as rope with embedded optimization problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_freedom_boundary, conceptual, 'Whether residual design freedom exists beyond grating equation constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(solid_state_phased_array_steering, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sspa_tr_t0, solid_state_phased_array_steering, theater_ratio, 0, 0.12).
narrative_ontology:measurement(sspa_tr_t20, solid_state_phased_array_steering, theater_ratio, 20, 0.14).
narrative_ontology:measurement(sspa_tr_t40, solid_state_phased_array_steering, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(sspa_be_t0, solid_state_phased_array_steering, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(sspa_be_t20, solid_state_phased_array_steering, base_extractiveness, 20, 0.17).
narrative_ontology:measurement(sspa_be_t40, solid_state_phased_array_steering, base_extractiveness, 40, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(solid_state_phased_array_steering, information_standard).
narrative_ontology:affects_constraint(solid_state_phased_array_steering, radar_detection_range_tradeoff).
narrative_ontology:affects_constraint(solid_state_phased_array_steering, antenna_sidelobe_suppression_cost).
narrative_ontology:affects_constraint(solid_state_phased_array_steering, frequency_allocation_spectrum_efficiency).

% DUAL FORMULATION NOTE:
% The grating equation constraint is upstream of several applied constraints in radar and communications systems. Those downstream constraints inherit the grating limits and must design around them. This constraint family reflects decomposition along application domains, not along structural ε values — all members are mountains with different engineering contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
