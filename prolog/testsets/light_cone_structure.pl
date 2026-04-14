% ============================================================================
% CONSTRAINT STORY: light_cone_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_light_cone_structure, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: light_cone_structure
 *   human_readable: Light Cone Structure in Spacetime
 *   domain: physics/special_relativity
 *
 * SUMMARY:
 *   Light cone structure defines causal relationships in spacetime: events
 *   are causally connected only if they lie within each other's light cones,
 *   separated by timelike or lightlike intervals. No signal or object can
 *   travel faster than light. This is not a regulatory constraint imposed by
 *   institutions, nor a coordination mechanism solving a collective action
 *   problem. It is a fundamental structural property of spacetime geometry
 *   that emerges from the constancy of light speed and Lorentz invariance.
 *   The constraint exhibits zero degrees of freedom: no agent, no
 *   institution, no technology, and no future discovery can circumvent it
 *   without contradicting the entire framework of special relativity itself.
 *
 * KEY AGENTS:
 *   - Material Particles: Causally confined agents (powerless/trapped) — cannot exceed light speed; no exit options exist
 *   - Electromagnetic Signals: Limited to light cone boundaries (trapped/trapped) — cannot carry information faster than light
 *   - Analytical Framework: Logical-mathematical constraint (analytical/analytical) — emerges from Lorentz invariance, not imposed externally
 *   - Physics Institution: Empirical validator (institutional/analytical) — confirms through experiments, does not create the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(light_cone_structure, 0.12).
domain_priors:suppression_score(light_cone_structure, 0.02).
domain_priors:theater_ratio(light_cone_structure, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(light_cone_structure, extractiveness, 0.12).
narrative_ontology:constraint_metric(light_cone_structure, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(light_cone_structure, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(light_cone_structure, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(light_cone_structure, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(light_cone_structure, mountain).
narrative_ontology:human_readable(light_cone_structure, "Light Cone Structure in Spacetime").
narrative_ontology:topic_domain(light_cone_structure, "physics/special_relativity").

domain_priors:emerges_naturally(light_cone_structure).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATERIAL PARTICLE (MOUNTAIN) — Any massive object is causally confined to its past light cone. No physical process can accelerate an object to light speed or beyond. This agent cannot exit or perceive alternatives. The constraint is absolute and unchangeable from the particle's frame.
constraint_indexing:constraint_classification(light_cone_structure, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From the perspective of special relativity, light cone structure emerges from the invariance of the speed of light and the structure of spacetime itself. The constraint is a logical consequence of Lorentz symmetry. No measurement basis or observable selection changes this fundamental property. Universal application across all frames of reference.
constraint_indexing:constraint_classification(light_cone_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: PHYSICS INSTITUTION (MOUNTAIN) — Experimental verification across 150+ years of physics (electromagnetism, special relativity, high-energy experiments) confirms light cone causality without exception. The institution cannot propose alternatives that survive empirical testing. This is not institutional preference but empirical fact.
constraint_indexing:constraint_classification(light_cone_structure, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(light_cone_structure_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(light_cone_structure, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(light_cone_structure, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(light_cone_structure, ExtMetricName, E),
    domain_priors:suppression_score(light_cone_structure, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(light_cone_structure),
    narrative_ontology:constraint_metric(light_cone_structure, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(light_cone_structure, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(light_cone_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint extracts nothing from agents — it simply defines what causality is. The 0.12 value accounts for mathematical formalism overhead and the minor practical inconvenience of relativistic effects (time dilation, length contraction) that agents must accommodate when moving at high velocities. These are not extraction but natural consequences of the geometry. Suppression (0.02): Minimal. There are no alternatives being suppressed. Agents do not have options being taken away; they have never had options outside the light cone. This is foundational structure, not coercive suppression. Theater ratio (0.05): Minimal. No performative activity is required to maintain light cone structure. It is not maintained through ritual, enforcement, or institutional performance — it is simply how spacetime is structured.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All three perspectives converge on the same classification: Mountain. Material particles experience light cone structure as immutable (their frame). The analytical observer recognizes it as a logical consequence of Lorentz invariance (theory frame). The institution confirms it through 150+ years of experimental testing (empirical frame). From every structural position and observational basis, the classification is identical. This uniformity is the hallmark of a true natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN VALIDATION: Light cone structure passes all mandatrophy gates. (1) Emerges naturally: YES — follows from the constancy of light speed (c = const in all frames), not imposed externally. (2) Accessibility collapse ≥ 0.85: 0.92 — any attempt to conceive of causality outside the light cone violates the logical framework of relativity and becomes empirically falsifiable. The constraint is fully saturated across all possible observation sites. (3) Resistance ≤ 0.15: 0.08 — only minor technical exceptions exist (e.g., closed timelike curves in some GR solutions), and these are exotic, unstable, and almost certainly unphysical. General relativity retains light cone structure as fundamental. No agent or institution seriously proposes alternatives. (4) Extractiveness ≤ 0.25, Suppression ≤ 0.05: SATISFIED. This is a canonical mountain — no extraction, no suppression, no institutional maintenance required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(light_cone_structure, 1905, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(light_cone_structure, information_standard).
narrative_ontology:affects_constraint(light_cone_structure, thermodynamic_arrow_of_time).
narrative_ontology:affects_constraint(light_cone_structure, quantum_entanglement_nonlocality).
narrative_ontology:affects_constraint(light_cone_structure, general_relativity_geodesics).

% DUAL FORMULATION NOTE:
% Light cone structure is upstream to thermodynamic time asymmetry (entropy increases forward in time precisely because the future light cone is causally open). It constrains quantum entanglement — entangled particles cannot transmit faster-than-light signals within the light cone. It is generalized by general relativity, where geodesics replace straight lines and spacetime curvature modifies light cone tilt, but the fundamental causal structure persists.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
