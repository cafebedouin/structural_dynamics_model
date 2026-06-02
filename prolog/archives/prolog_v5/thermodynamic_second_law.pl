% ============================================================================
% CONSTRAINT STORY: thermodynamic_second_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thermodynamic_second_law, []).

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
 *   constraint_id: thermodynamic_second_law
 *   human_readable: The Thermodynamic Second Law (Entropy Constraint)
 *   domain: physics/thermodynamics/fundamental_law
 *
 * SUMMARY:
 *   The thermodynamic second law constrains all material and energy
 *   transformation in the universe. It states that entropy in an isolated
 *   system never decreases — it increases monotonically (or remains constant
 *   in reversible processes). This is a fundamental physical law, not an
 *   institutional arrangement, policy constraint, or coordination mechanism.
 *   The second law is invariant across all observational frames, measurement
 *   methodologies, and temporal horizons. It emerges from statistical
 *   mechanics (Boltzmann's interpretation) and information theory (Gibbs's
 *   formulation) and is validated across all domains of physics and
 *   chemistry. Unlike institutional constraints that can be negotiated,
 *   circumvented, or re-framed through power shifts, the second law is
 *   absolutely immutable. No agent can escape it, no coalition can negotiate
 *   with it, and no alternative institutional design can overcome it.
 *
 * KEY AGENTS:
 *   - Any Material System: Powerless victim (trapped/universal) — subject to entropy increase with no exit option
 *   - Thermodynamic Agents (Heat Engines, Living Systems): Institutional actor (analytical/analytical) — must operate within Carnot efficiency limits and entropy production constraints
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as a law of nature, invariant and non-negotiable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thermodynamic_second_law, 0.08).
domain_priors:suppression_score(thermodynamic_second_law, 0.02).
domain_priors:theater_ratio(thermodynamic_second_law, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thermodynamic_second_law, extractiveness, 0.08).
narrative_ontology:constraint_metric(thermodynamic_second_law, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(thermodynamic_second_law, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(thermodynamic_second_law, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(thermodynamic_second_law, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thermodynamic_second_law, mountain).
narrative_ontology:human_readable(thermodynamic_second_law, "The Thermodynamic Second Law (Entropy Constraint)").
narrative_ontology:topic_domain(thermodynamic_second_law, "physics/thermodynamics/fundamental_law").

domain_priors:emerges_naturally(thermodynamic_second_law).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AGENT IN A CLOSED SYSTEM (MOUNTAIN) — Any material system cannot escape the fundamental constraint that entropy increases or remains constant in isolated processes. No exit option exists; the constraint is unchangeable from any temporal horizon. Entropy rise is not negotiable — it is a law, not a coordination mechanism or extractive institution.
constraint_indexing:constraint_classification(thermodynamic_second_law, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — The second law is invariant across all observational frameworks, measurement methodologies, and theoretical formulations (Boltzmann's H-theorem, Gibbs's ensemble formulation, information-theoretic entropic interpretations). The classification does not depend on perspective, observer position, or choice of measurement basis. This is a natural law in the strictest sense.
constraint_indexing:constraint_classification(thermodynamic_second_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ENGINEERING INSTITUTIONS (MOUNTAIN) — Industrial systems, power plants, heat engines, and thermodynamic machines all operate subject to the second law. No institutional arrangement can circumvent it. Efficiency limits (Carnot efficiency) are absolute constraints on all thermal machinery. Institutional actors attempting to violate these constraints simply fail — their machines do not work. The constraint is independent of belief, doctrine, or institutional design.
constraint_indexing:constraint_classification(thermodynamic_second_law, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thermodynamic_second_law_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(thermodynamic_second_law, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thermodynamic_second_law, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(thermodynamic_second_law, ExtMetricName, E),
    domain_priors:suppression_score(thermodynamic_second_law, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(thermodynamic_second_law),
    narrative_ontology:constraint_metric(thermodynamic_second_law, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(thermodynamic_second_law, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(thermodynamic_second_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The second law does not extract value from some agents and transfer it to others. It is a constraint on all material processes equally. The low extractiveness reflects that this is a natural law, not an extraction mechanism. Suppression (0.02): Minimal. The constraint does not suppress alternatives through coercion or institutional design — alternatives to increasing entropy are not suppressed, they are physically impossible. No escape exists because escape contradicts fundamental physics, not because institutional power prevents it. Theater ratio (0.05): Minimal. There is no performative or ritual element to entropy increase. The constraint operates mechanically, without need for theatrical legitimation. Accessibility collapse (0.92): Very high. The constraint is accessible only through mathematical formalism and experimental physics. A novice observer cannot understand the constraint from intuitive experience — thermodynamic intuitions are often wrong (people expect perpetual motion or perfect engines). The accessibility barrier is steep and unavoidable. Resistance (0.04): Very low. No meaningful resistance to the law is possible. Attempts to violate it fail empirically. The constraint is not resisted — it is obeyed by the structure of reality itself.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits NO perspectival gap — all observations converge on Mountain classification from all agent positions. The powerless agent experiencing the constraint sees it as unchangeable. The institutional actor trying to overcome it finds it immutable. The analytical observer sees invariance across all measurement methodologies and observational frames. This uniform classification is the defining characteristic of a natural law. When a constraint appears as Mountain from all perspectives with different power levels, exit options, and temporal horizons, the classification is validated.
 *
 * DIRECTIONALITY LOGIC:
 *   The second law has no directionality value (d) because it is not an extraction relationship. It does not distribute costs and benefits asymmetrically — it constrains all systems uniformly. The chi formula does not apply. The constraint is not a constraint ON agents; it is a constraint OF the universe's structure. Every agent, regardless of power or position, is equally bound by it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    time_direction_arrow,
    'Does the thermodynamic arrow of time arise from entropy increase (as derived from statistical mechanics) or is entropy increase a consequence of an independent time-directedness in the universe?',
    'Microscopic reversibility of fundamental laws vs. macroscopic irreversibility; analysis of whether time directionality is emergent from statistical mechanics or primitive. CPT symmetry considerations in particle physics.',
    'If entropy-driven: the constraint is a statistical consequence of boundary conditions + dynamics (remains Mountain). If time-primitive: entropy is an indicator of a more fundamental constraint (still Mountain, but with different underlying mechanism). Classification unchanged either way.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(time_direction_arrow, conceptual, 'Origin of time''s arrow: entropy vs. fundamental asymmetry').

omega_variable(
    local_entropy_decrease,
    'Can local entropy decrease indefinitely in an open system without thermodynamic cost, or is there a fundamental limit to ordered structure complexity at finite temperature?',
    'Analysis of dissipation required for entropy export; bounds on information storage density (Bekenstein bound); limits to computation and data processing from thermodynamic considerations.',
    'If no fundamental limit: ordered structures (life, computation, civilization) face only material/energetic constraints, not entropic ones (remains Mountain but less binding for local agents). If fundamental limit exists: even open systems face absolute constraints on sustained complexity (remains Mountain, applies more broadly).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_entropy_decrease, empirical, 'Limits to local entropy decrease in open systems').

omega_variable(
    quantum_measurement_entropy,
    'Does quantum measurement genuinely increase entropy or does measurement-induced decoherence preserve total information while redistributing it? Does the universe''s quantum state remain pure or does decoherence imply true information loss?',
    'Comparison of quantum von Neumann entropy vs. classical thermodynamic entropy; resolution of black hole information paradox; experimental tests of information preservation in quantum systems.',
    'If information is preserved: the second law is about information redistribution rather than true loss (conceptual reframing, classification unchanged). If information is genuinely lost: quantum mechanics violates unitarity and the second law operates at a deeper level than quantum formalism (conceptual reframing, classification unchanged).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quantum_measurement_entropy, empirical, 'Information loss vs. decoherence in quantum measurement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thermodynamic_second_law, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ther_tr_t0, thermodynamic_second_law, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ther_tr_t1000, thermodynamic_second_law, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(ther_tr_t2000, thermodynamic_second_law, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(ther_be_t0, thermodynamic_second_law, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ther_be_t1000, thermodynamic_second_law, base_extractiveness, 1000, 0.08).
narrative_ontology:measurement(ther_be_t2000, thermodynamic_second_law, base_extractiveness, 2000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thermodynamic_second_law, global_infrastructure).
narrative_ontology:affects_constraint(thermodynamic_second_law, biological_evolution_constraint).
narrative_ontology:affects_constraint(thermodynamic_second_law, information_processing_limits).
narrative_ontology:affects_constraint(thermodynamic_second_law, perpetual_motion_prohibition).

% DUAL FORMULATION NOTE:
% The thermodynamic second law is upstream of all constraints involving energy transformation, information processing, or biological organization. Systems constrained by information-theoretic entropy limits are downstream of this constraint. The network structure reflects causal dependence: many specific constraints are applications or consequences of the second law's fundamental principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
