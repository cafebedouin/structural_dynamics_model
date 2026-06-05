% ============================================================================
% CONSTRAINT STORY: platform_cooperativism_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_cooperativism_governance, []).

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
 *   constraint_id: platform_cooperativism_governance
 *   human_readable: Democratic Worker Governance in Platform Cooperativism
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   Platform Cooperativism is a model where a digital platform is owned and
 *   governed by its workers. This structure is intended to promote fair labor
 *   practices, democratic decision-making, and equitable distribution of
 *   profits. The success of platform cooperativism depends on effective
 *   governance mechanisms that balance the interests of all stakeholders.
 *   While designed as a pure coordination mechanism, competitive pressures
 *   and free-rider problems can generate extraction.
 *
 * KEY AGENTS:
 *   - Platform Cooperative Workers: Primary beneficiaries (organized/mobile) - Benefit from democratic governance and fair labor practices.
 *   - Platform Cooperative Users: Secondary beneficiaries (moderate/mobile) - Benefit from a platform run in their best interests.
 *   - Analytical Observer: Observer (analytical/analytical) - Studies the effectiveness of the governance structure.
 *   - Competing Capital-backed Platforms: Incumbent platform (institutional/constrained) - Pressured by the more effective internal coordination of the cooperatives.
 *   - Uncoordinated Labor Market: (powerless/trapped) - Extracted by the superior coordination of the platform cooperative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_cooperativism_governance, 0.35).
domain_priors:suppression_score(platform_cooperativism_governance, 0.25).
domain_priors:theater_ratio(platform_cooperativism_governance, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_cooperativism_governance, extractiveness, 0.35).
narrative_ontology:constraint_metric(platform_cooperativism_governance, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(platform_cooperativism_governance, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_cooperativism_governance, tangled_rope).
narrative_ontology:human_readable(platform_cooperativism_governance, "Democratic Worker Governance in Platform Cooperativism").
narrative_ontology:topic_domain(platform_cooperativism_governance, "economic/social/technological").

domain_priors:requires_active_enforcement(platform_cooperativism_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_cooperativism_governance, platform_cooperative_workers).
narrative_ontology:constraint_beneficiary(platform_cooperativism_governance, platform_cooperative_users).
narrative_ontology:constraint_victim(platform_cooperativism_governance, competing_capital_backed_platforms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of the workers who own and govern the platform. They experience the governance structure as a coordination mechanism, enabling them to collectively make decisions and benefit from their labor.
constraint_indexing:constraint_classification(platform_cooperativism_governance, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% Perspective of the users of the platform cooperative. They benefit from the democratic governance structure, as it ensures that the platform is run in their best interests. They can exit if the platform doesn't meet their needs.
constraint_indexing:constraint_classification(platform_cooperativism_governance, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% Perspective of an analytical observer studying the platform cooperative model. They see the governance structure as a coordination mechanism that promotes worker empowerment and democratic decision-making.
constraint_indexing:constraint_classification(platform_cooperativism_governance, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Perspective of incumbent platforms with traditional ownership and governance. They experience pressure from the more coordinated cooperative, but have difficulty coordinating a response. They benefit from their existing scale but are constrained in responding to the values-driven coordination.
constraint_indexing:constraint_classification(platform_cooperativism_governance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective of the uncoordinated labor market, which is trapped by the superior coordination of the platform cooperative, extracting labor value.
constraint_indexing:constraint_classification(platform_cooperativism_governance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_cooperativism_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_cooperativism_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_cooperativism_governance, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(platform_cooperativism_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The governance structure facilitates coordination among workers, ensuring that the platform operates in their best interests (low extraction). The low theater ratio reflects the genuine attempt to implement democratic decision-making processes.
 *
 * PERSPECTIVAL GAP:
 *   The workers experience the governance structure as a coordination mechanism, empowering them to make decisions collectively. Users benefit from the platform being run in their interests. A competing capital-backed platform views this structure as creating competitive pressure to restructure, extracting some profit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_cooperativism_governance, 2014, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_cooperativism_governance, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
