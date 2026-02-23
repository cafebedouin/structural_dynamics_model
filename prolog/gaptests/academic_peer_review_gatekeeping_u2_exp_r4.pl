% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_exp_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_exp_r4, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_exp_r4
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model functions as a constraint where the primary
 *   producers of value—researchers—provide their labor (writing, peer review,
 *   editing) for free to for-profit publishers. These publishers then erect
 *   paywalls and sell access to the finished product, often funded by public
 *   money, back to the researchers' own universities at exorbitant prices.
 *   This creates a circular flow of value from the public and academia to a
 *   small number of powerful publishing houses.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - Must participate to gain tenure.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - Capture value from free labor and subscription fees.
 *   - University Libraries/Consortia: Analytical victims (analytical/constrained) - Forced to pay escalating costs for research their institutions produced.
 *   - The General Public: Secondary victims (powerless/trapped) - Blocked from accessing publicly-funded research.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r4, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_exp_r4, 0.75).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_exp_r4, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r4, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r4, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r4, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_exp_r4, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_exp_r4, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_exp_r4, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_exp_r4, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_exp_r4, tenured_senior_academics).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r4, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r4, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r4, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Must publish in high-impact, costly journals to secure tenure, providing free labor (writing, reviewing) into a system that their own institution pays dearly to access. The 'publish or perish' mandate makes exit nearly impossible without abandoning their career.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Views the system as a valuable coordination service: organizing peer review, curating quality, and providing a stable platform for scientific discourse. The high profits are seen as a fair return for managing this complex process and upholding academic standards.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r4, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Recognizes the coordination function of peer review but sees the immense and asymmetric extraction. They are trapped in negotiations for 'big deal' subscription packages, fully aware that they are paying for access to research their own institutions produced.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% A tenured academic who acts as a gatekeeper. They benefit from the prestige and influence but are also constrained by the system, providing significant uncompensated labor. They see both the coordination benefits (upholding standards) and the extractive costs.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r4, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r4, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the publishers' profit margins, built on the uncompensated labor of academics. The high suppression (0.75) comes from the 'publish or perish' culture and the academic prestige system, which locks researchers into participating and makes alternative publishing models difficult to establish as legitimate career pathways.
 *
 * PERSPECTIVAL GAP:
 *   A vast gap exists between the Junior Professor, who experiences the system as a coercive Snare demanding labor without direct compensation, and the Publisher, who frames it as a necessary Rope for coordinating scientific quality control. The Library Consortium, seeing both sides, correctly identifies it as a Tangled Rope—a system with a legitimate coordination function that has been captured for highly extractive purposes.
 *
 * DIRECTIONALITY LOGIC:
 *   Value flows from researchers (labor) and universities (subscriptions) to publishers (profits). Junior professors are the primary targets of extraction, as their career progression is directly tied to their participation. Publishers are the direct beneficiaries. Senior academics are secondary beneficiaries, gaining prestige and gatekeeping power, even as they also provide free labor.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this system as a Tangled Rope is crucial. A simple Snare classification would miss the genuine (though perhaps inefficient) coordination function of peer review that provides the system its legitimacy and resilience. A Rope classification would ignore the massive, asymmetric extraction of value. The Tangled Rope model correctly captures this duality, showing how a coordination mechanism can be used to enable and obscure a highly extractive process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_control_necessity,
    'Is the publisher-centric model with its high costs essential for maintaining rigorous peer review, or is it primarily rent-seeking that could be replaced by more efficient, researcher-led systems?',
    'Large-scale comparative studies of article quality, retraction rates, and scientific impact between top-tier subscription journals and high-quality, low-cost open-access alternatives.',
    'If the model is necessary for quality, it remains a Tangled Rope. If alternatives are equally or more effective, the current system is a Snare legitimized by a theatrical claim of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_control_necessity, empirical, 'Whether the system's extraction is a necessary cost for quality control or is pure rent-seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_exp_r4, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u2_exp_r4, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u2_exp_r4, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(acad_tr_t2024, academic_peer_review_gatekeeping_u2_exp_r4, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u2_exp_r4, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u2_exp_r4, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(acad_be_t2024, academic_peer_review_gatekeeping_u2_exp_r4, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_exp_r4, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r4, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r4, scientific_research_velocity).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r4, academic_tenure_process).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
