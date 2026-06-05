% ============================================================================
% CONSTRAINT STORY: s1_visa_judgment_sharing_agreement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_s1_visa_judgment_sharing_agreement, []).

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
 *   constraint_id: s1_visa_judgment_sharing_agreement
 *   human_readable: Visa Judgment Sharing Agreement (AMEX Antitrust Case)
 *   domain: legal/economic
 *
 * SUMMARY:
 *   The Visa Judgment Sharing Agreement was a contractual coordination
 *   mechanism between Visa U.S.A. and its member banks. It facilitated the
 *   sharing of information and legal judgments related to antitrust
 *   litigation. The goal was to reduce legal costs and mitigate risks
 *   associated with potential antitrust violations. American Express
 *   challenged the agreement as part of a broader antitrust case, arguing
 *   that it suppressed competition.
 *
 * KEY AGENTS:
 *   - Visa U.S.A.: Primary beneficiary (institutional/arbitrage) — benefits from reduced legal costs and improved risk management.
 *   - Visa Member Banks: Secondary beneficiary (institutional/constrained) — benefit from shared legal information but are constrained by the agreement terms.
 *   - American Express: Challenger (institutional/mobile) - Views the agreement as anti-competitive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(s1_visa_judgment_sharing_agreement, 0.35).
domain_priors:suppression_score(s1_visa_judgment_sharing_agreement, 0.25).
domain_priors:theater_ratio(s1_visa_judgment_sharing_agreement, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(s1_visa_judgment_sharing_agreement, extractiveness, 0.35).
narrative_ontology:constraint_metric(s1_visa_judgment_sharing_agreement, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(s1_visa_judgment_sharing_agreement, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(s1_visa_judgment_sharing_agreement, rope).
narrative_ontology:human_readable(s1_visa_judgment_sharing_agreement, "Visa Judgment Sharing Agreement (AMEX Antitrust Case)").
narrative_ontology:topic_domain(s1_visa_judgment_sharing_agreement, "legal/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(s1_visa_judgment_sharing_agreement, visa_member_banks).
narrative_ontology:constraint_beneficiary(s1_visa_judgment_sharing_agreement, visa_usa).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Visa sees this as a beneficial coordination mechanism for risk management, sharing information, and reducing legal costs.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Member banks are constrained by their agreement with Visa but benefit from shared legal information and risk mitigation.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a broad perspective, the agreement facilitates information sharing and reduces redundant litigation, fostering efficiency within the Visa network.
constraint_indexing:constraint_classification(s1_visa_judgment_sharing_agreement, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(s1_visa_judgment_sharing_agreement_tests).
:- end_tests(s1_visa_judgment_sharing_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Low-Moderate. Some extraction exists as banks are required to participate in the agreement. Suppression (0.25): Low. Member banks are constrained by the agreement but have some exit options. Theater Ratio (0.15): Low. The agreement primarily serves a functional purpose of information sharing.
 *
 * PERSPECTIVAL GAP:
 *   Visa and its member banks view the agreement as a beneficial coordination mechanism (Rope), while a challenger like American Express might perceive it as anti-competitive. The analytical observer recognizes the coordination benefits but also acknowledges the potential for anti-competitive effects.
 *
 * DIRECTIONALITY LOGIC:
 *   Visa U.S.A. and member banks are primary beneficiaries (d close to 0). American Express, as a competitor, may view it with a directionality closer to 1, but is not a party to the agreement itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This agreement is primarily a coordination mechanism for managing legal risk. It is not intended to be an extractive snare but to provide mutual benefit to Visa and its member banks by sharing legal information and reducing redundant litigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(s1_visa_judgment_sharing_agreement, 1990, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(s1_visa_judgment_sharing_agreement, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
