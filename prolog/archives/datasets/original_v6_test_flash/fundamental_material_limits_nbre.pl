% ============================================================================
% CONSTRAINT STORY: fundamental_material_limits_nbre
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
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
 *   The critical temperature dictates the operating temperature of
 *   superconducting devices using NbRe. The low critical temperature requires
 *   significant cooling overhead. The weak spin-triplet pair admixture places
 *   a limit on the use of NbRe in spintronics applications.
 *
 * KEY AGENTS:
 *   - Superconducting Qubit Designer (powerless/trapped)
 *   - Materials Science Community (institutional/analytical)
 *   - Analytical Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_material_limits_nbre, 0.1).
domain_priors:suppression_score(fundamental_material_limits_nbre, 0.02).
domain_priors:theater_ratio(fundamental_material_limits_nbre, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, extractiveness, 0.1).
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(fundamental_material_limits_nbre, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fundamental_material_limits_nbre, mountain).
narrative_ontology:human_readable(fundamental_material_limits_nbre, "Fundamental Material Limits of Niobium-Rhenium (NbRe)").
narrative_ontology:topic_domain(fundamental_material_limits_nbre, "physics/materials_science").

domain_priors:emerges_naturally(fundamental_material_limits_nbre).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Superconducting qubit designers are fundamentally limited by material properties of NbRe; they cannot change the critical temperature or the degree of spin-triplet pair admixture because those are properties of NbRe itself, and superconducting qubits require *some* material with these properties.
constraint_indexing:constraint_classification(fundamental_material_limits_nbre, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% Materials Science Community is limited by the periodic table and the properties of elements. Modifying NbRe requires fundamentally different materials, which may not have the same desirable properties of NbRe.
constraint_indexing:constraint_classification(fundamental_material_limits_nbre, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of an analytical observer, the material limits of NbRe are fixed by the laws of physics and chemistry.
constraint_indexing:constraint_classification(fundamental_material_limits_nbre, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   The extractiveness and suppression are very low because NbRe's material limits emerge naturally and are difficult to overcome with current technologies. The theater ratio is low because there is little performative activity associated with the material limits.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify as mountain, reflecting the fundamental and immutable nature of the material limits. There is no perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   N/A
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_material_limits_nbre, 0, 100).

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
