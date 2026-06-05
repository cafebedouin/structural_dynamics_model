% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u4_exp_r5
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u4_exp_r5, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: academic_peer_review_gatekeeping_u4_exp_r5
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system functions as a mechanism where the primary
 *   producers of value (researchers) provide their labor for free—as authors,
 *   peer reviewers, and editors—to for-profit publishing corporations. These
 *   corporations then package the resulting product (journals) and sell it
 *   back at extremely high prices to the researchers' own institutions (via
 *   university libraries), while also restricting access for the general
 *   public whose taxes often funded the initial research. The system is
 *   maintained by the 'publish or perish' culture of academia, where career
 *   advancement is tied to publication in prestigious, high-cost journals.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - Must participate to gain tenure.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - Capture immense value from free labor and institutional budgets.
 *   - University Libraries/Consortia: Institutional victims (institutional/constrained) - Forced to pay escalating subscription fees.
 *   - Tenured Senior Academics: Secondary beneficiaries/enforcers (powerful/mobile) - Uphold the prestige system from which they benefit and act as gatekeepers.
 *   - The General Public: Tertiary victims (powerless/trapped) - Denied access to publicly-funded knowledge.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u4_exp_r5, 0.78).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u4_exp_r5, 0.85).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u4_exp_r5, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r5, extractiveness, 0.78).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r5, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r5, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u4_exp_r5, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u4_exp_r5, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u4_exp_r5, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u4_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u4_exp_r5, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u4_exp_r5, tenured_senior_academics).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r5, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r5, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r5, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: The untenured academic who must 'publish or perish' in high-prestige journals, providing free labor (writing, reviewing) to a system that extracts from their own institution.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: The publisher who views the system as a highly efficient coordination mechanism for vetting and distributing research, generating significant profit with minimal input cost.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r5, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: The library consortium that must negotiate exorbitant 'big deal' subscription packages, aware of the extraction but unable to exit without crippling their institution's research capacity.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r5, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: The analytical observer who sees both the genuine coordination function (quality control, signaling) and the captured, highly extractive financial model built upon it.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u4_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r5, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u4_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u4_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.78) is extremely high, reflecting the publishers' profit margins built on unpaid labor and captive institutional customers. Suppression (0.85) is also very high due to the lack of viable alternatives for career progression; tenure and grant committees heavily weigh publications in these specific journals, creating a powerful lock-in effect.
 *
 * PERSPECTIVAL GAP:
 *   A vast gap exists between the Junior Professor, who experiences the system as a coercive Snare demanding labor without direct compensation for a chance at career survival, and the Publisher, who operates it as a highly profitable Rope for coordinating and monetizing scholarly communication. The University Library sees the Tangled Rope, acknowledging the need for journals but buckling under the extractive costs. The analytical view confirms the Tangled Rope, recognizing that a genuine coordination function (peer review) has been almost entirely captured by an extractive business model.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is clear: value flows from researchers (labor) and their institutions (cash) to the publishers. Junior professors and university libraries are the primary victims. Publishers are the primary beneficiaries. Senior tenured academics act as both beneficiaries (reaping prestige) and enforcers of the system, creating a self-perpetuating cycle.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope resolves the mandatrophy of calling it a pure Snare. While highly extractive, it cannot be a Snare because it performs a genuine, if distorted, coordination function: quality control and prestige signaling. A pure Snare has no such function. Conversely, calling it a Rope would ignore the massive, asymmetric extraction. The Tangled Rope classification correctly identifies the hybrid nature, where a coordination mechanism has become a vehicle for rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_vs_rent_seeking,
    'Is the high cost and restrictive access a necessary byproduct of ensuring high-quality peer review and editorial standards, or is it primarily rent-seeking by publishers exploiting a captured market?',
    'Comparative analysis of research quality, retraction rates, and citation impact between high-cost subscription journals and non-profit, open-access alternatives over a multi-decade period.',
    'If primarily for quality, the constraint leans towards a costly Rope. If primarily rent-seeking, it is a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_vs_rent_seeking, empirical, 'Distinguishing the necessary cost of quality control from publisher rent-seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u4_exp_r5, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1970, academic_peer_review_gatekeeping_u4_exp_r5, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(acad_tr_t1995, academic_peer_review_gatekeeping_u4_exp_r5, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(acad_tr_t2025, academic_peer_review_gatekeeping_u4_exp_r5, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(acad_be_t1970, academic_peer_review_gatekeeping_u4_exp_r5, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(acad_be_t1995, academic_peer_review_gatekeeping_u4_exp_r5, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(acad_be_t2025, academic_peer_review_gatekeeping_u4_exp_r5, base_extractiveness, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u4_exp_r5, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u4_exp_r5, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u4_exp_r5, scientific_reproducibility_crisis).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u4_exp_r5, tenure_and_promotion_systems).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
