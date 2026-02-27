% ============================================================================
% CONSTRAINT STORY: independent_criticism_patronage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_independent_criticism_patronage, []).

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
 *   constraint_id: independent_criticism_patronage
 *   human_readable: The Patronage Model for Independent Cultural Criticism
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint describes the system supporting niche, long-form cultural
 *   criticism through direct audience patronage, as exemplified by platforms
 *   like Patreon or Substack funding blogs like Colin Marshall's.
 *
 * KEY AGENTS:
 *   - Independent Critics: Creators who receive patronage.
 *   - Patrons: Individuals who provide financial support.
 *   - Patronage Platforms: Platforms facilitating the exchange (Patreon, Substack).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(independent_criticism_patronage, 0.2).
domain_priors:suppression_score(independent_criticism_patronage, 0.15).
domain_priors:theater_ratio(independent_criticism_patronage, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(independent_criticism_patronage, extractiveness, 0.2).
narrative_ontology:constraint_metric(independent_criticism_patronage, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(independent_criticism_patronage, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(independent_criticism_patronage, rope).
narrative_ontology:human_readable(independent_criticism_patronage, "The Patronage Model for Independent Cultural Criticism").
narrative_ontology:topic_domain(independent_criticism_patronage, "economic/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(independent_criticism_patronage, independent_critics).
narrative_ontology:constraint_beneficiary(independent_criticism_patronage, patronage_platforms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Patronage platforms like Patreon and Substack benefit from facilitating these transactions. They can arbitrage between different critics and patrons, and can exit relatively easily by focusing on other types of creators.
constraint_indexing:constraint_classification(independent_criticism_patronage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Independent critics benefit from direct funding, allowing them to pursue niche topics without the constraints of traditional media. They are relatively mobile, able to switch platforms if needed.
constraint_indexing:constraint_classification(independent_criticism_patronage, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% Patrons benefit from access to niche content and direct interaction with creators. They are generally mobile, able to adjust their patronage as desired.
constraint_indexing:constraint_classification(independent_criticism_patronage, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% From a long-term analytical perspective, this model represents a beneficial coordination mechanism for supporting independent cultural criticism in the digital age.
constraint_indexing:constraint_classification(independent_criticism_patronage, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(independent_criticism_patronage_tests).
:- end_tests(independent_criticism_patronage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This system is primarily a coordination mechanism (rope). Extractiveness is low because patrons voluntarily contribute, and critics provide content in return. Suppression is low as alternative funding models (advertising, institutional support) still exist.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify this as a rope because all actors benefit from the system. Critics gain funding, patrons gain access to content, and platforms facilitate the exchange.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents are net beneficiaries. Critics and platforms directly benefit, and patrons receive desired content in return for voluntary contributions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling coordination as pure extraction because all actors participate voluntarily and derive benefit. Patrons can freely choose to stop their patronage, and critics are free to create content without using the platform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(independent_criticism_patronage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(independent_criticism_patronage, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
