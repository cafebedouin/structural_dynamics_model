% ============================================================================
% CONSTRAINT STORY: factional_instability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_factional_instability, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: factional_instability
 *   human_readable: The Republican Remedy for Factional Violence
 *   domain: political
 *
 * SUMMARY:
 *   Based on Federalist Paper No. 10, the Republican Remedy for Factional
 *   Violence addresses the inherent risk of factions destabilizing
 *   government. By extending the sphere of governance and incorporating
 *   numerous diverse interests, the remedy aims to dilute the power of any
 *   single faction. This, in turn, necessitates negotiation and compromise,
 *   promoting stability and protecting minority rights.
 *
 * KEY AGENTS:
 *   - National Stability: Primary beneficiary (institutional/analytical) — benefits from reduced factional conflict.
 *   - Minority Rights: Secondary beneficiary (organized/mobile) — benefits from protections against majority tyranny.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(factional_instability, 0.35).
domain_priors:suppression_score(factional_instability, 0.25).
domain_priors:theater_ratio(factional_instability, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(factional_instability, extractiveness, 0.35).
narrative_ontology:constraint_metric(factional_instability, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(factional_instability, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(factional_instability, rope).
narrative_ontology:human_readable(factional_instability, "The Republican Remedy for Factional Violence").
narrative_ontology:topic_domain(factional_instability, "political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(factional_instability, national_stability).
narrative_ontology:constraint_beneficiary(factional_instability, minority_rights).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of maintaining national stability, the extended republic and its representative institutions serve as a mechanism for aggregating diverse interests, diluting the power of any single faction, and promoting compromise and consensus.
constraint_indexing:constraint_classification(factional_instability, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% From the perspective of minority rights advocates, the extended republic's diversity and emphasis on representation provide avenues for protecting vulnerable groups from the tyranny of the majority, fostering inclusivity and tolerance.
constraint_indexing:constraint_classification(factional_instability, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical observer sees the republican remedy as a rope, facilitating the management of factionalism through institutional design and encouraging cooperation between diverse groups.
constraint_indexing:constraint_classification(factional_instability, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(factional_instability_tests).
:- end_tests(factional_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. Some limitations on individual faction power, but primarily a coordination mechanism. Suppression (0.25): Low. Aims to manage factionalism, not suppress it entirely. Theater Ratio (0.15): Low. Primarily functional.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives see this as a rope because the republican remedy coordinates between factions. The analytical observer is the meta-perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is primarily beneficial, leading to rope classification. All relevant actors are intended to benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling because it highlights the coordination aspect over enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(factional_instability, 1788, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(factional_instability, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
