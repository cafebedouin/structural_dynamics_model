% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u4_exp_r4
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u4_exp_r4, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u4_exp_r4
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model is a system where researchers, typically
 *   funded by public money, provide free labor (writing, peer review,
 *   editing) to for-profit publishers. These publishers then erect paywalls
 *   around the finished research and sell access, often in expensive
 *   subscription bundles, back to the same universities and institutions that
 *   produced it. This creates a circular flow where public funds are used to
 *   generate private profit, while access to knowledge is restricted.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - Must publish in prestigious journals to secure tenure and funding.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - Capture immense value from free labor and content.
 *   - Research Institutions/Libraries: Secondary victims (institutional/constrained) - Forced to pay high subscription fees for research their own faculty produced.
 *   - Senior Faculty: Secondary beneficiaries/enforcers (powerful/mobile) - Uphold the system from which they benefited and now act as gatekeepers (editors, reviewers).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u4_exp_r4, 0.78).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u4_exp_r4, 0.82).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u4_exp_r4, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r4, extractiveness, 0.78).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r4, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r4, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u4_exp_r4, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u4_exp_r4, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u4_exp_r4, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u4_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u4_exp_r4, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u4_exp_r4, senior_faculty_gatekeepers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r4, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r4, research_institutions).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r4, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the junior academic, publishing in high-impact, paywalled journals is a non-negotiable requirement for career survival ('publish or perish'). The labor is uncompensated and the system is coercive.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the publisher's perspective, the system is an extremely efficient coordination mechanism for sourcing, vetting, and monetizing intellectual property at minimal cost, creating a highly profitable and defensible business model.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r4, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Libraries are trapped in 'big deal' subscription bundles. While they recognize the extractive nature, they are constrained by faculty demand and the lack of viable, prestigious alternatives, forcing them to participate in the system.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r4, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The system possesses a genuine coordination function (quality control, credentialing) but has been captured by an extractive model that privatizes publicly funded research. Both functions are real and inseparable.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u4_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r4, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u4_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u4_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is extremely high due to the business model of monetizing free labor and publicly funded work. Suppression (0.82) is also high, enforced by the 'publish or perish' culture, the tenure system's reliance on journal impact factors, and the difficulty of building prestigious alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. Publishers see an efficient Rope for coordinating and credentialing research. Junior researchers experience a coercive Snare essential for their careers. Analytical observers and institutions see a Tangled Rope, acknowledging the system's valid coordination function (quality filtering) has been co-opted by a highly extractive business model.
 *
 * DIRECTIONALITY LOGIC:
 *   Value is systematically extracted from researchers (labor), their institutions (subscription fees), and the public (research funding) and directed towards the shareholders of a small number of large publishing houses. The directionality is unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would ignore the genuine, if often inefficient, coordination function that peer review provides. Classifying it as a Rope would ignore the massive, non-consensual extraction. The Tangled Rope classification is essential to capture this duality, preventing mischaracterization and highlighting that the coordination mechanism itself is the vehicle for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_as_coordination,
    'Is the prestige of top-tier journals an irreducible coordination good, or a form of manufactured scarcity that enables rent-seeking?',
    'Longitudinal analysis comparing the scientific impact (e.g., citation rates, real-world application) of research published in top-tier paywalled journals versus high-quality open-access platforms.',
    'If prestige is a manufactured artifact, the system is a pure Snare. If it provides an essential, irreplaceable coordination signal, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_as_coordination, empirical, 'Whether journal prestige is a real coordination good or manufactured scarcity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u4_exp_r4, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1970, academic_peer_review_gatekeeping_u4_exp_r4, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(acad_tr_t1995, academic_peer_review_gatekeeping_u4_exp_r4, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(acad_tr_t2024, academic_peer_review_gatekeeping_u4_exp_r4, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(acad_be_t1970, academic_peer_review_gatekeeping_u4_exp_r4, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(acad_be_t1995, academic_peer_review_gatekeeping_u4_exp_r4, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(acad_be_t2024, academic_peer_review_gatekeeping_u4_exp_r4, base_extractiveness, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u4_exp_r4, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u4_exp_r4, academic_tenure_process).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u4_exp_r4, public_research_funding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
