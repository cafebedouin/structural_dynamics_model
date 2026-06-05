% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_exp_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_exp_r5, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_exp_r5
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system relies on researchers providing free labor
 *   (writing, peer review, editing) to for-profit publishers. These
 *   publishers then sell access to the resulting research, often funded by
 *   public money, back to the researchers' own institutions at extremely high
 *   profit margins. The system is maintained by the 'publish or perish'
 *   culture of academia, where career advancement is tied to publication in
 *   high-prestige, high-cost journals.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - Must publish to secure tenure, providing free labor.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - Capture immense value from free labor and subscription fees.
 *   - Research Institutions / Library Consortia: Secondary victims and enforcers (institutional/constrained) - Pay the high costs while also using the system's prestige signals for internal evaluation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r5, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_exp_r5, 0.75).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_exp_r5, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r5, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r5, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r5, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_exp_r5, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_exp_r5, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_exp_r5, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_exp_r5, for_profit_journal_publishers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r5, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r5, research_institutions).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r5, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior academic, publishing in prestigious journals is non-negotiable for career survival ('publish or perish'). They provide free labor (writing, reviewing, editing) to a system that holds their future captive.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the publisher's view, they are providing an essential coordination service: managing peer review, curating content, and bestowing a valuable signal of quality and prestige. The high margins are the price for this service.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r5, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Librarians and consortia see both sides: the system does coordinate scholarly communication, but at an extractive cost that is unsustainable. They are tasked with analyzing this trade-off and negotiating within its constraints.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Universities are both victims (paying exorbitant subscription fees) and enforcers (using journal prestige in tenure decisions). They cannot easily exit without damaging their own standing and their faculty's careers.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r5, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r5, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high due to the business model of monetizing free academic labor and publicly funded research. Suppression (0.75) is very high because the prestige of top-tier journals creates a powerful lock-in effect, making alternative publishing venues appear less viable for career-conscious academics.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the publisher, it's a Rope that coordinates and validates knowledge. For the junior academic, it's a Snare that coerces free labor under threat of career failure. For the university, it's a Tangled Rope—a necessary, functional system for evaluation that is simultaneously draining its own budget.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction is unambiguously from the academic community (both individuals and institutions) to the shareholders of large publishing houses. Publishers are the beneficiaries of the free labor and the subscription revenue. Researchers and their institutions are the victims who provide the labor and pay the fees.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would ignore the genuine (though arguably overpriced) coordination function of managing peer review and signaling quality. Classifying it as a Rope would ignore the massive, non-consensual extraction. Tangled Rope correctly identifies the dual nature: a system with a coordination function that has been captured and leveraged for asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_vs_extraction,
    'Is the high cost imposed by publishers a necessary price for the prestige and quality-signaling function they provide, or is it pure rent-seeking on a captured market?',
    'Comparative analysis of article quality and impact between high-cost journals and high-quality, low-cost open-access alternatives over a multi-decade period.',
    'If the prestige function is proven to be inseparable from the cost structure, it remains a Tangled Rope. If it's primarily rent-seeking, the system is a Snare at the institutional level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_vs_extraction, empirical, 'Whether publisher costs are for quality-signaling or rent-seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_exp_r5, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u2_exp_r5, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u2_exp_r5, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(acad_tr_t2020, academic_peer_review_gatekeeping_u2_exp_r5, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u2_exp_r5, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u2_exp_r5, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(acad_be_t2020, academic_peer_review_gatekeeping_u2_exp_r5, base_extractiveness, 2020, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_exp_r5, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r5, university_tenure_system).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r5, public_access_to_research).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
