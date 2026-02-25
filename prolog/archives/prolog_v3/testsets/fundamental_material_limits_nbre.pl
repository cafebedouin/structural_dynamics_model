% ============================================================================
% CONSTRAINT STORY: fundamental_material_limits_nbre
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fundamental_material_limits_nbre, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fundamental_material_limits_nbre
 *   human_readable: Fundamental Material Limits of Niobium-Rhenium (NbRe)
 *   domain: physics/materials_science
 *
 * SUMMARY:
 *   The intrinsic physical properties of the superconducting alloy
 *   Niobium-Rhenium (NbRe), such as its critical temperature (Tc) of
 *   approximately 7-9K and its weak spin-triplet pair admixture, impose a
 *   hard, non-negotiable ceiling on its performance in advanced applications.
 *   This is particularly relevant in superconducting spintronics, where a
 *   higher Tc and stronger triplet component are desired for creating
 *   efficient spin-valves and other quantum devices. These limits are not
 *   matters of policy or engineering refinement but are dictated by the
 *   material's electronic band structure and phononic properties.
 *
 * KEY AGENTS:
 *   - Spintronics Engineers (moderate/constrained): Their research and development goals are directly thwarted by the material's limitations.
 *   - Materials Theorists (analytical/analytical): They model and understand why these limits exist, viewing them as fundamental consequences of physical law.
 *   - Device Fabricators (powerless/trapped): They must work within the measured parameters of the material, with no recourse to alter them.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_material_limits_nbre, 0.05).
domain_priors:suppression_score(fundamental_material_limits_nbre, 0.02).
domain_priors:theater_ratio(fundamental_material_limits_nbre, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, extractiveness, 0.05).
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fundamental_material_limits_nbre, mountain).
narrative_ontology:human_readable(fundamental_material_limits_nbre, "Fundamental Material Limits of Niobium-Rhenium (NbRe)").
narrative_ontology:topic_domain(fundamental_material_limits_nbre, "physics/materials_science").

domain_priors:emerges_naturally(fundamental_material_limits_nbre).

% --- Structural relationships ---
narrative_ontology:constraint_victim(fundamental_material_limits_nbre, spintronics_engineers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: The engineer is constrained by the material's intrinsic properties; they can switch materials but cannot change this one.
constraint_indexing:constraint_classification(fundamental_material_limits_nbre, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: The theorist understands the limit as a consequence of quantum mechanics and crystallography, an unchangeable feature of the universe.
constraint_indexing:constraint_classification(fundamental_material_limits_nbre, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: The fabricator working with a specific NbRe wafer is trapped by its measured properties for the current production run.
constraint_indexing:constraint_classification(fundamental_material_limits_nbre, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fundamental_material_limits_nbre_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fundamental_material_limits_nbre, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fundamental_material_limits_nbre, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fundamental_material_limits_nbre, ExtMetricName, E),
    domain_priors:suppression_score(fundamental_material_limits_nbre, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fundamental_material_limits_nbre),
    narrative_ontology:constraint_metric(fundamental_material_limits_nbre, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fundamental_material_limits_nbre, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fundamental_material_limits_nbre_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extremely low extractiveness (ε=0.05) and suppression (0.02) reflect that this is a constraint of physics, not a social or economic system. It does not extract value, it simply defines the boundaries of the possible. The Natural Law profile is met with high accessibility_collapse (0.98), as Tc is easily and repeatedly verifiable across labs, and low resistance (0.02), as no amount of effort can change the intrinsic Tc of this specific alloy.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All agents, regardless of their relationship to the material, perceive its limits as a Mountain. The engineer's frustration does not alter the classification, it merely highlights the 'victim' status in the sense of having one's goals obstructed by an immovable object.
 *
 * DIRECTIONALITY LOGIC:
 *   Spintronics engineers are designated as 'victims' not because a system is extracting from them, but because their objectives are fundamentally limited by this physical reality. The system calculates a high directionality (d) for this group. However, because the base extractiveness (ε) is negligible, the effective extraction (χ) remains close to zero, ensuring the classification remains Mountain. This correctly models a non-anthropocentric constraint that nevertheless has asymmetric impacts on human goals.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a canonical example of a natural law, posing no risk of mandatrophy. The metrics firmly place it in the Mountain category from all perspectives, preventing any misinterpretation of a physical limit as a correctable social or political failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_material_limits_nbre, 1955, 2055).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
