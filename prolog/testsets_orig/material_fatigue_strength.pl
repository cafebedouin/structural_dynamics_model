% ============================================================================
% CONSTRAINT STORY: material_fatigue_strength
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_material_fatigue_strength, []).

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
 *   constraint_id: material_fatigue_strength
 *   human_readable: Material Fatigue Strength Limit
 *   domain: materials_science/mechanical_engineering
 *
 * SUMMARY:
 *   Material fatigue strength is the maximum cyclic stress a material can
 *   endure indefinitely without failure. This constraint exemplifies a pure
 *   natural law constraint with zero degrees of freedom. The S-N
 *   (Stress-Number of cycles) curve is universal across engineering domains:
 *   as stress amplitude increases, the number of cycles to failure decreases
 *   according to a predictable curve that reaches a plateau (the endurance
 *   limit) below which failure probability drops to negligible levels.
 *   Fatigue failure arises from the irreversible accumulation of dislocations
 *   and microcracks under cyclic loading — a process driven by the
 *   thermodynamic irreversibility of plastic deformation. This constraint
 *   binds all material systems, all geometries, and all time horizons. No
 *   agent can negotiate, exit, or circumvent it; they can only design around
 *   it by selecting appropriate material properties, stress levels, and
 *   geometric safety factors. The constraint emerges naturally from the
 *   atomic structure of solids and the physics of deformation.
 *
 * KEY AGENTS:
 *   - Material substrate: Primary target (powerless/trapped) — cannot negotiate fatigue; damage accumulates irreversibly
 *   - Engineering design: Primary observer (powerful/mobile at biographical horizon) — experiences constraint as unchangeable boundary; can only optimize within it
 *   - Materials scientist: Secondary observer (powerful/mobile) — can create new materials with higher fatigue limits but cannot eliminate fatigue itself
 *   - Analytical observer: Universalist (analytical/analytical) — sees constraint as logically necessary consequence of thermodynamic irreversibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(material_fatigue_strength, 0.12).
domain_priors:suppression_score(material_fatigue_strength, 0.03).
domain_priors:theater_ratio(material_fatigue_strength, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(material_fatigue_strength, extractiveness, 0.12).
narrative_ontology:constraint_metric(material_fatigue_strength, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(material_fatigue_strength, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(material_fatigue_strength, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(material_fatigue_strength, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(material_fatigue_strength, mountain).
narrative_ontology:human_readable(material_fatigue_strength, "Material Fatigue Strength Limit").
narrative_ontology:topic_domain(material_fatigue_strength, "materials_science/mechanical_engineering").

domain_priors:emerges_naturally(material_fatigue_strength).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATERIAL SUBSTRATE (MOUNTAIN) — Fatigue strength is an immutable property of the material at universal scope and civilizational horizon. The material cannot exit, negotiate, or alter the constraint. Cyclic stress above the endurance limit causes progressive microcrystalline damage that accumulates irreversibly until failure occurs. This is a natural law, not a social or institutional arrangement.
constraint_indexing:constraint_classification(material_fatigue_strength, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, fatigue strength emerges from the atomic structure of matter and the irreversible nature of dislocation accumulation. The constraint is logically and physically necessary — it follows from the laws of thermodynamics and solid-state mechanics. No observable methodology changes the ε value; this is a structural invariant across all measurement contexts.
constraint_indexing:constraint_classification(material_fatigue_strength, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ENGINEERING DESIGN COMMUNITY (MOUNTAIN) — Engineers experience fatigue strength as an unchangeable boundary condition on design space. While material selection and geometric optimization can shift the effective threshold (shot peening, surface treatments, design for reduced stress concentration), the fundamental constraint is immutable at the generational horizon and global scope. No engineering technique can eliminate fatigue as a failure mode — only delay its onset or shift where failure occurs.
constraint_indexing:constraint_classification(material_fatigue_strength, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MATERIALS SCIENTIST (MOUNTAIN) — Even with maximum research agency and resources, the scientist at biographical timescale cannot alter the fatigue limit through willpower or funding. New materials can be engineered with higher fatigue thresholds, but the constraint itself — that cyclic stress causes cumulative damage up to failure — remains invariant. The scientist's power translates to shifting the location of the boundary, not removing it.
constraint_indexing:constraint_classification(material_fatigue_strength, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(material_fatigue_strength_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(material_fatigue_strength, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(material_fatigue_strength, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(material_fatigue_strength, ExtMetricName, E),
    domain_priors:suppression_score(material_fatigue_strength, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(material_fatigue_strength),
    narrative_ontology:constraint_metric(material_fatigue_strength, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(material_fatigue_strength, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(material_fatigue_strength_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The constraint itself imposes no asymmetric extraction — fatigue strength is a property of the material, not a mechanism for transferring value from one agent to another. The small non-zero value reflects the minimal performative content in fatigue testing (slight measurement error, statistical noise in large population testing). Suppression (0.03): Negligible. There is no suppression of alternatives because there are no alternatives — the constraint is immutable. The small value reflects measurement uncertainty at the boundary. Theater ratio (0.15): Low. Fatigue testing protocols (ASTM standards) are highly technical and functional with minimal theater. The ratio is non-zero only because S-N curve interpretation requires statistical judgment (setting the endurance limit involves probabilistic reasoning) rather than pure physical measurement. Accessibility collapse (0.92): High. Once fatigue strength is established through rigorous testing, the constraint is completely inaccessible — designers cannot negotiate around it. Resistance (0.08): Low. There is almost no institutional or social resistance to accepting fatigue as a natural law; the constraint is universally recognized as legitimate. The small value reflects rare edge cases of denial (ignoring fatigue in inexperienced design).
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows zero perspectival gap — all perspectives converge on Mountain. The material under stress, the engineer, the scientist, and the analytical observer all perceive the same immutable boundary. This uniformity is diagnostic: it confirms the constraint is a true natural law, not a social construction wearing the clothes of physics. If any perspective had produced a different type, the constraint would be revealed as contingent institutional framing (e.g., if the engineer saw Rope, fatigue would be a coordination problem about testing standards rather than a physical law).
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality analysis applies to pure mountain constraints. There is no extraction flow, no beneficiary, and no victim — only an immutable boundary that applies uniformly to all agents. The constraint is agent-independent.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fatigue_limit_definition_precision,
    'Is the fatigue limit (S-N curve endurance threshold) a sharp boundary or a probabilistic phenomenon with a tail distribution?',
    'Statistical analysis of large-scale fatigue test data across material batches and specimen geometries; examination of whether failures below classical endurance limit occur at measurable frequencies vs zero frequency.',
    'If sharp boundary: pure mathematical/physical constant (mountain confirmed). If probabilistic tail: constraint has a small extractive component (boundary depends on acceptable failure probability), shifting classification toward rope at appropriate timescale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fatigue_limit_definition_precision, empirical, 'Fatigue limit as sharp vs probabilistic boundary').

omega_variable(
    environmental_modifier_universality,
    'Do environmental factors (temperature, corrosion, radiation, surface oxidation) systematically modify the measured fatigue limit such that the underlying property is context-dependent rather than material-intrinsic?',
    'Comparative fatigue testing across environmental conditions; isolation of intrinsic material property from environmental modification; examination of whether ''fatigue strength'' refers to pristine material or to material-in-service.',
    'If purely intrinsic: mountain classification is robust. If environment-dependent: constraint may be tangled rope (coordination across environmental controls) with environmental modification as an extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_modifier_universality, empirical, 'Whether fatigue limit is intrinsic or environment-modified').

omega_variable(
    microstructural_reversibility,
    'Can microstructural damage from cyclic loading be reversed or healed through annealing, or is dislocation accumulation fundamentally irreversible?',
    'In-situ TEM observation of dislocation dynamics during fatigue cycling and subsequent annealing; thermodynamic analysis of whether reverse processes exist at feasible timescales.',
    'If irreversible: constraint is mountain (second law of thermodynamics). If reversible: constraint may be rope (coordinated healing process with cost) or even scaffold (temporary damage with reset capability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(microstructural_reversibility, empirical, 'Irreversibility of fatigue-induced microstructural damage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(material_fatigue_strength, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fatigue_tr_t0, material_fatigue_strength, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fatigue_tr_t50, material_fatigue_strength, theater_ratio, 50, 0.15).
narrative_ontology:measurement(fatigue_tr_t100, material_fatigue_strength, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(fatigue_be_t0, material_fatigue_strength, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(fatigue_be_t50, material_fatigue_strength, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(fatigue_be_t100, material_fatigue_strength, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(material_fatigue_strength, information_standard).
narrative_ontology:affects_constraint(material_fatigue_strength, mechanical_component_design_safety).
narrative_ontology:affects_constraint(material_fatigue_strength, bridge_infrastructure_lifetime).
narrative_ontology:affects_constraint(material_fatigue_strength, aircraft_structural_integrity).

% DUAL FORMULATION NOTE:
% Material fatigue strength is a foundational constraint upstream of all mechanical engineering design constraints. Any constraint that involves cyclic loading (turbines, bearings, joints, structures) is downstream of this natural law. The affects_constraints array lists instantiations of this constraint in specific domains, not decompositions of it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
