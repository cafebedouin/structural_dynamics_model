% ============================================================================
% CONSTRAINT STORY: portuguese_presidential_term_limits
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_portuguese_presidential_term_limits, []).

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
 *   constraint_id: portuguese_presidential_term_limits
 *   human_readable: Portuguese Constitutional Term Limits (Article 123)
 *   domain: political/legal
 *
 * SUMMARY:
 *   Under the Portuguese Constitution, Article 123 establishes presidential
 *   term limits, preventing any individual from serving more than two
 *   consecutive terms. This provision aims to prevent the concentration of
 *   power and safeguard democratic principles, ensuring a regular turnover of
 *   leadership and fostering a more inclusive political landscape.
 *
 * KEY AGENTS:
 *   - Portuguese Democracy: Beneficiary (powerless/trapped) — Benefits from the prevention of long-term concentration of power.
 *   - Portuguese Constitutional Court: Beneficiary (institutional/analytical) — Benefits from upholding the rule of law and preventing constitutional overreach.
 *   - Aspiring Presidential Candidates: Beneficiary (powerful/mobile) - benefits from the structural openings for leadership
 *   - Incumbent President: Limited Power (moderate/constrained) — the constraint limits the power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(portuguese_presidential_term_limits, 0.15).
domain_priors:suppression_score(portuguese_presidential_term_limits, 0.1).
domain_priors:theater_ratio(portuguese_presidential_term_limits, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, extractiveness, 0.15).
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(portuguese_presidential_term_limits, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(portuguese_presidential_term_limits, rope).
narrative_ontology:human_readable(portuguese_presidential_term_limits, "Portuguese Constitutional Term Limits (Article 123)").
narrative_ontology:topic_domain(portuguese_presidential_term_limits, "political/legal").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(portuguese_presidential_term_limits, portuguese_democracy).
narrative_ontology:constraint_beneficiary(portuguese_presidential_term_limits, aspiring_presidential_candidates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The average citizen benefits from the term limits as they prevent the potential for authoritarianism, even though they have no direct influence or exit.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The court benefits from upholding the constitution, ensuring stability. The court has the analytical power to judge the laws.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% Aspiring candidates may be constrained, but they also benefit from the knowledge that no one can occupy the presidency indefinitely.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% Analytical perspective to study impact in governance.
constraint_indexing:constraint_classification(portuguese_presidential_term_limits, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(portuguese_presidential_term_limits_tests).
:- end_tests(portuguese_presidential_term_limits_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The term limits are primarily a coordination mechanism (Rope) because they promote democratic values and prevent power concentration. The extractiveness is low because they don't severely restrict anyone's freedom, they only limit the duration of the presidential term. The suppression is also low, as it doesn't heavily suppress any alternative political arrangement.
 *
 * PERSPECTIVAL GAP:
 *   While all perspectives generally agree that the term limits are beneficial, they differ in the degree of impact. The average citizen benefits indirectly, while the court's and aspiring candidates' benefits are more direct and tangible. All see as rope, due to low extraction
 *
 * DIRECTIONALITY LOGIC:
 *   The average citizen's directionality is more passive as they don't have direct control or influence over the enforcement of term limits, yet they benefit from the broader democratic safeguards. The Constitutional Court's directionality is highly beneficial because upholding the term limits reinforces their role and authority within the political system. The aspiring candidates benefit as they get opportunities.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is designed to avoid mandatrophy by being a clear limitation for a political position without creating an avenue for mandatrophic extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(portuguese_presidential_term_limits, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(portuguese_presidential_term_limits, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
