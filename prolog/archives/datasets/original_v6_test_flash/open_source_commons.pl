% ============================================================================
% CONSTRAINT STORY: open_source_commons
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_source_commons, []).

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
 *   constraint_id: open_source_commons
 *   human_readable: The Mutual Garden
 *   domain: social/technological
 *
 * SUMMARY:
 *   The open-source commons represents a high-trust coordination environment
 *   where value is shared among community members and downstream users. It
 *   operates on the principle of mutual contribution and benefit, fostering
 *   collaboration and innovation.
 *
 * KEY AGENTS:
 *   - Community Members: Contributors and beneficiaries of the shared resources.
 *   - Downstream Users: Entities that utilize the open-source resources in their projects.
 *   - Analytical Observer: Evaluates the overall effectiveness and impact of the commons.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_source_commons, 0.15).
domain_priors:suppression_score(open_source_commons, 0.1).
domain_priors:theater_ratio(open_source_commons, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_source_commons, extractiveness, 0.15).
narrative_ontology:constraint_metric(open_source_commons, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(open_source_commons, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_source_commons, rope).
narrative_ontology:human_readable(open_source_commons, "The Mutual Garden").
narrative_ontology:topic_domain(open_source_commons, "social/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_source_commons, community_members).
narrative_ontology:constraint_beneficiary(open_source_commons, downstream_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A community member benefits from the shared resources and contributions of others. They have the option to leave the community if it no longer serves their needs.
constraint_indexing:constraint_classification(open_source_commons, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% Downstream users (e.g., companies using open-source libraries) benefit from the freely available resources. They can choose alternative libraries or develop their own if needed.
constraint_indexing:constraint_classification(open_source_commons, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From an analytical perspective, the open-source commons represents a coordination mechanism that facilitates innovation and knowledge sharing.
constraint_indexing:constraint_classification(open_source_commons, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_source_commons_tests).
:- end_tests(open_source_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low because the system is designed for mutual benefit. Suppression is low because participation is voluntary and alternatives exist. The theater ratio is low because the activities directly contribute to the shared resource pool.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify the open-source commons as a rope because it primarily serves as a coordination mechanism with minimal extraction or suppression. The different perspectives highlight the various ways in which the commons benefits its participants.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the community members and downstream users. The 'd' value is derived from the beneficiary status and the exit options available to them. Since they benefit from the system and have the option to leave, the 'd' value is low, resulting in a low effective extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_source_commons, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_source_commons, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
