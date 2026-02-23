% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_exp_r3, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_exp_r3
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model is a system where the producers and
 *   consumers of research are largely the same group: academics and their
 *   institutions. Researchers provide labor (writing, peer review, editing)
 *   for free to for-profit publishers. These publishers then package the
 *   research and sell access back to university libraries at extremely high
 *   subscription rates, effectively monetizing publicly-funded work and free
 *   academic labor.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - Must publish in prestigious journals to secure tenure.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - Capture immense value from free labor and institutional budgets.
 *   - University Libraries/Consortia: Institutional victims and analytical observers (organized/constrained) - Forced to pay escalating costs, possess data to see the full structure.
 *   - Tenured Professors: System enforcers and secondary victims (organized/constrained) - Uphold the system through tenure committees while also providing free labor.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r3, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_exp_r3, 0.75).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_exp_r3, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r3, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r3, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_exp_r3, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_exp_r3, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_exp_r3, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_exp_r3, for_profit_journal_publishers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r3, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r3, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r3, taxpayers_funding_research).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior academic on a tenure track, publishing in high-prestige, paywalled journals is non-negotiable. The system extracts their labor (research, writing, reviewing) for free and their institution's money (subscriptions) under the threat of career failure. It is a classic Snare.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the publisher's perspective, they are providing an essential coordination service: managing peer review, ensuring quality, handling typesetting and distribution, and maintaining a prestigious brand. The costs are framed as necessary for this service. The negative effective extraction (χ) from this viewpoint classifies it as a Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r3, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% An analytical observer, such as a library consortium analyzing subscription costs against value, sees both the coordination function and the immense, asymmetric extraction. They recognize the system's utility but also its predatory pricing and reliance on captured labor and markets. This is the canonical Tangled Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Senior academics are both beneficiaries (of the prestige system they mastered) and victims (providing free labor as reviewers/editors). They have more power and constrained exit options, but still operate within the system they enforce on junior colleagues. They experience it as a Tangled Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r3, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.68) is high, reflecting the publishers' profit margins built on free labor and publicly funded research. Suppression (0.75) is also high due to the 'publish or perish' culture, the lock-in of journal prestige for career advancement, and the lack of viable, prestigious alternatives for tenure-track faculty.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists. Junior faculty experience a Snare due to the coercive nature of tenure requirements. Publishers frame their role as a Rope, providing essential coordination. Analytical observers like library consortia see the full picture: a system with a real coordination function that has been captured for extractive purposes, making it a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of value flow is unambiguous. Labor, research content, and public funds flow from academics, universities, and taxpayers towards the publishers. The publishers are the clear beneficiaries, while researchers and their institutions are the victims who bear the costs and provide the uncompensated value.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this system as a pure Snare would be inaccurate, as it ignores the genuine and necessary coordination function of vetting and disseminating research. Conversely, classifying it as a Rope ignores the massive, non-consensual extraction. The Tangled Rope classification correctly identifies that a coordination mechanism has become a vehicle for rent-seeking, preventing the mislabeling that would occur from either extreme perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction,
    'Is the high cost of journal subscriptions a necessary price for the coordination and quality control provided, or is it pure rent-seeking on a captured market?',
    'Comparative analysis of publication costs and quality between for-profit journals and non-profit/open-access alternatives (e.g., Sci-Hub usage data, arXiv overlay journals).',
    'If costs are shown to be primarily for coordination, the classification leans towards Tangled Rope. If they are overwhelmingly rent-seeking, it approaches a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction, empirical, 'Distinguishing necessary coordination costs from pure rent-seeking by publishers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_exp_r3, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u2_exp_r3, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u2_exp_r3, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(acad_tr_t2020, academic_peer_review_gatekeeping_u2_exp_r3, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u2_exp_r3, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u2_exp_r3, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(acad_be_t2020, academic_peer_review_gatekeeping_u2_exp_r3, base_extractiveness, 2020, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_exp_r3, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r3, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r3, scientific_reproducibility_crisis).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r3, tenure_and_promotion_systems).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
