% ============================================================================
% CONSTRAINT STORY: material_tensile_strength
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_material_tensile_strength, []).

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
 *   constraint_id: material_tensile_strength
 *   human_readable: Ultimate Tensile Strength (UTS)
 *   domain: technological/materials_science
 *
 * SUMMARY:
 *   Ultimate Tensile Strength (UTS) is the maximum tensile (pulling) stress
 *   that a material can sustain before necking and fracture occur. This is a
 *   fundamental property emerging from atomic-scale bonding, crystal
 *   structure, and defect characteristics. UTS is among the purest examples
 *   of a natural law constraint in the technological domain. It exhibits zero
 *   degrees of freedom across all observational contexts: whether measured in
 *   laboratory tensile tests, computed from first-principles quantum
 *   mechanics, or applied in structural engineering design, the UTS value
 *   remains constant for a given material composition and microstructure.
 *   There are no agents who benefit at the expense of others, no coercive
 *   enforcement mechanism, no theatrical performance, and no suppression of
 *   alternatives. The constraint is the material's physical reality.
 *
 * KEY AGENTS:
 *   - The Material: The substrate itself — tensile strength is its atomic property, not an external imposition
 *   - Materials Scientists: Analytical observers who measure and characterize UTS via standardized testing (ASTM E8/E8M protocols)
 *   - Structural Engineers: Design agents who must account for UTS as a hard boundary in load-bearing applications
 *   - Manufacturing Institutions: Organizations that guarantee UTS through process control and batch certification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(material_tensile_strength, 0.08).
domain_priors:suppression_score(material_tensile_strength, 0.02).
domain_priors:theater_ratio(material_tensile_strength, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(material_tensile_strength, extractiveness, 0.08).
narrative_ontology:constraint_metric(material_tensile_strength, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(material_tensile_strength, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(material_tensile_strength, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(material_tensile_strength, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(material_tensile_strength, mountain).
narrative_ontology:human_readable(material_tensile_strength, "Ultimate Tensile Strength (UTS)").
narrative_ontology:topic_domain(material_tensile_strength, "technological/materials_science").

domain_priors:emerges_naturally(material_tensile_strength).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MATERIAL ITSELF (MOUNTAIN) — UTS is an irreducible physical property. The material has no agency, no exit option, and no choice in its maximum tensile capacity. From the material's structural reference frame, tensile strength is not a constraint imposed externally but an inherent atomic-scale property. Zero degrees of freedom. The constraint is the material.
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE ANALYTICAL ENGINEER (MOUNTAIN) — Tensile strength is a discovered, measurable property of matter. It emerges from atomic bonding, crystal structure, and defect density. No engineer can override it; no design process can circumvent it. The constraint is universal across all material instances of the same composition and microstructure. It is not enforced; it simply is. Zero degrees of freedom in a well-characterized material.
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE MANUFACTURING INSTITUTION (MOUNTAIN) — Industrial materials science accepts UTS as an invariant constraint. No amount of process optimization, economic pressure, or regulatory mandate can alter the fundamental tensile capacity of a given material composition. Manufacturing institutions structure their entire design and quality systems around this immutable limit. It is not a negotiable tradeoff; it is the starting point for all tensile design.
constraint_indexing:constraint_classification(material_tensile_strength, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(material_tensile_strength_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(material_tensile_strength, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(material_tensile_strength, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(material_tensile_strength, ExtMetricName, E),
    domain_priors:suppression_score(material_tensile_strength, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(material_tensile_strength),
    narrative_ontology:constraint_metric(material_tensile_strength, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(material_tensile_strength, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(material_tensile_strength_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. UTS does not extract value from any agent because there is no asymmetric benefit. No one captures value at another's expense. The property is a direct description of material behavior, not a mechanism for redistribution. Suppression (0.02): Negligible. There is no suppression of alternatives because there are no alternatives to suppress. A given material composition either has a specific UTS or it does not. No state of affairs is hidden or prevented from observation. Theater ratio (0.15): Very low. UTS testing is highly objective — ASTM E8/E8M standardizes specimen geometry, testing apparatus, strain rate, and data collection. The performance is directly observable and reproducible. No performative ritual masks the underlying reality. Accessibility collapse (0.92): High. UTS is equally accessible to all observers with access to the same material sample and measurement apparatus. No privileged position allows better access. Resistance to observation (0.08): Low. UTS is straightforward to measure with standard tensile testing equipment. Crystal structure can be probed with X-ray diffraction. Atomic bonding can be calculated with quantum mechanics. The constraint yields its structure readily to investigation.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All three perspectives — material, engineer, institution — converge on identical classification: Mountain. UTS is invariant across observation contexts because it is a property of matter itself, not a social or institutional arrangement. The 'gap' would only appear if we attempted to treat UTS as a negotiable parameter or a social construct, but the structural data (zero extractiveness, zero suppression, zero theater) reveals such framing as false.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) does not apply to mountain constraints. The d-value formalism was designed to capture power asymmetries, exit options, and beneficiary/victim structures in constraints that distribute costs and benefits. UTS exhibits none of these features. It is not 'high extraction' at d=1.0 (maximum target status); it is not extraction at all. It is a boundary condition that applies uniformly to all agents. Engineers do not occupy different structural positions relative to UTS — they all face the same constraint. There is no directionality derivation because there is no directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(material_tensile_strength, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(material_tensile_strength, global_infrastructure).
narrative_ontology:affects_constraint(material_tensile_strength, material_fatigue_strength).
narrative_ontology:affects_constraint(material_tensile_strength, stress_concentration_factor).
narrative_ontology:affects_constraint(material_tensile_strength, fracture_toughness).

% DUAL FORMULATION NOTE:
% UTS is a component of a larger materials constraint family. It interacts with fatigue strength (cyclical loading), fracture toughness (resistance to crack propagation), and stress concentration (localized stress amplification). These are distinct constraints with their own extractiveness values. UTS is the upstream mountain; the others are often tangled_rope (coordination of material selection with design optimization) or snare (fatigue-induced failure in cost-optimized designs).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
