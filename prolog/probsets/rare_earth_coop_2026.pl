% ============================================================================
% CONSTRAINT STORY: rare_earth_coop_2026
% ============================================================================
% Version: 0.2 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_coop_2026, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rare_earth_coop_2026
 *   human_readable: Manufacturer-Owned Rare Earth Cooperative (MOREC)
 *   domain: economic/industrial
 *
 * SUMMARY:
 *   A horizontal coordination mechanism where manufacturers pool capital to
 *   bypass the profit-seeking mandates of Project Vault, securing rare earth
 *   elements for their production needs.
 *
 * KEY AGENTS:
 *   - Member Manufacturers: Beneficiaries (institutional/mobile) - benefit from secured supply and potentially lower costs.
 *   - Analytical Observer: (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_coop_2026, 0.2).
domain_priors:suppression_score(rare_earth_coop_2026, 0.15).
domain_priors:theater_ratio(rare_earth_coop_2026, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_coop_2026, extractiveness, 0.2).
narrative_ontology:constraint_metric(rare_earth_coop_2026, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(rare_earth_coop_2026, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_coop_2026, rope).
narrative_ontology:human_readable(rare_earth_coop_2026, "Manufacturer-Owned Rare Earth Cooperative (MOREC)").
narrative_ontology:topic_domain(rare_earth_coop_2026, "economic/industrial").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_coop_2026, member_manufacturers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Member manufacturers experience this as a coordination mechanism to secure rare earth elements outside of the Project Vault framework. They have the power to exit and procure from other sources, although this may be less desirable.
constraint_indexing:constraint_classification(rare_earth_coop_2026, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% From a broad analytical perspective, this cooperative represents a coordination mechanism to bypass a specific constraint (Project Vault). It facilitates resource allocation among members, enhancing overall supply chain resilience.
constraint_indexing:constraint_classification(rare_earth_coop_2026, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_coop_2026_tests).
:- end_tests(rare_earth_coop_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Low extractiveness and suppression reflect the voluntary nature of the cooperative and the availability of alternative sourcing options.
 *
 * PERSPECTIVAL GAP:
 *   Both member manufacturers and analytical observers perceive this as a beneficial coordination mechanism, hence the consistent 'rope' classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Member manufacturers are the primary beneficiaries and have the option to exit the cooperative, indicating a low 'd' value and thus a coordination-focused perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction because the cooperative is designed to address a specific need rather than extract value from its members. It's a response to a larger constraint, not a source of extraction itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_coop_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_coop_2026, resource_allocation).
narrative_ontology:affects_constraint(rare_earth_coop_2026, project_vault_dependency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
