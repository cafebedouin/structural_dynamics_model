% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u1_exp_r2
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u1_exp_r2, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u1_exp_r2
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model functions as a constraint where
 *   researchers, primarily at publicly-funded institutions, provide free
 *   labor (research, writing, peer review) to for-profit publishers. These
 *   publishers then erect paywalls and sell access to this research back to
 *   the same institutions at exorbitant prices. The system is maintained by a
 *   prestige economy, where career advancement (tenure, grants) is tied to
 *   publishing in high-impact, publisher-owned journals.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - provide free labor under career duress.
 *   - Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture and monetize the value of the free labor.
 *   - University Libraries: Institutional victims (organized/constrained) - forced to pay high subscription fees for research their own faculty produced.
 *   - Senior Faculty/Editors: Secondary beneficiaries/enforcers (organized/mobile) - act as gatekeepers, upholding the system that grants them prestige and influence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u1_exp_r2, 0.78).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u1_exp_r2, 0.82).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u1_exp_r2, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r2, extractiveness, 0.78).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r2, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r2, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u1_exp_r2, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u1_exp_r2, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u1_exp_r2, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u1_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u1_exp_r2, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u1_exp_r2, tenured_senior_faculty_editors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r2, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r2, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r2, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Must publish in high-impact journals to secure tenure, providing free labor (research, writing, reviewing) to a system that holds their career prospects captive.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Experiences an efficient coordination mechanism for acquiring high-value content at zero cost, packaging it, and selling it back to the source institutions at a high margin.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Sees the full structure: a genuine coordination function (quality signaling, credentialing) inextricably linked with a highly extractive, rent-seeking business model.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Participates as a gatekeeper, benefiting from the prestige and influence the system provides, while also recognizing its extractive nature and contributing labor to it.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r2, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u1_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u1_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u1_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the publishers' profit margins, built on unpaid labor and public funding. The high suppression (0.82) reflects the 'publish or perish' culture and the lock-in of journal impact factors, which makes alternative publishing venues (like pre-print servers or institutional repositories) appear less viable for career progression.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the publisher, who sees an elegant Rope for coordinating and monetizing intellectual property, and the junior academic, who experiences a Snare that extracts their labor for the benefit of others. The analytical view recognizes both functions are present, hence the Tangled Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is clear: value flows from researchers and their institutions (the victims) to the publishers (the beneficiaries). Senior faculty act as both victims (providing labor) and beneficiaries (gaining prestige), but the net financial flow is overwhelmingly extractive in favor of the publishers.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope is crucial. A pure Snare classification would miss the genuine (though perhaps inefficient) coordination function of quality control and signaling that the journal system provides. A Rope classification would ignore the massive, asymmetric extraction. The Tangled Rope correctly identifies that a coordination mechanism has been captured for rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_control_vs_extraction,
    'Is the peer review system a necessary coordination mechanism for scientific quality, or primarily a theatrical justification for rent-seeking by publishers?',
    'Comparative analysis of outcomes, retraction rates, and scientific impact between high-prestige toll-access journals and well-run open access platforms with transparent review.',
    'If the quality function is proven to be largely theatrical or replicable by non-extractive means, the constraint is a pure Snare. If it is a necessary, irreplaceable coordination function, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_control_vs_extraction, empirical, 'Whether the system's quality control function is genuine or a pretext for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u1_exp_r2, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u1_exp_r2, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u1_exp_r2, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(acad_tr_t2020, academic_peer_review_gatekeeping_u1_exp_r2, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u1_exp_r2, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u1_exp_r2, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(acad_be_t2020, academic_peer_review_gatekeeping_u1_exp_r2, base_extractiveness, 2020, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u1_exp_r2, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u1_exp_r2, university_tenure_system).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u1_exp_r2, public_research_funding_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
