% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_exp_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_exp_r1, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_exp_r1
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model functions as a system where the primary
 *   producers and consumers of research (academics and their institutions)
 *   provide free labor (writing, peer review, editing) to for-profit third
 *   parties (publishers). These publishers then erect paywalls and sell
 *   access to the finished product back to the same institutions at high
 *   markups, capturing the value created by the academic community.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary target/labor (Powerless/Trapped) - Must participate to gain tenure.
 *   - For-Profit Journal Publishers: Primary beneficiary (Institutional/Arbitrage) - Extract value from the system.
 *   - University Libraries/Consortia: Victim/Auditor (Institutional/Analytical) - Bear the direct financial costs and analyze the value proposition.
 *   - University Tenure Committees: Enforcer (Institutional/Constrained) - Uphold the system by using journal prestige as a key metric for advancement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r1, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_exp_r1, 0.75).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_exp_r1, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r1, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_exp_r1, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_exp_r1, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_exp_r1, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_exp_r1, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_exp_r1, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_exp_r1, tenured_senior_academics).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r1, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r1, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_exp_r1, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Must publish in high-impact, often paywalled, journals to secure tenure ('publish or perish'). Provides free labor (writing, reviewing) and their institution pays to access the final product. Exit is prohibitively costly to their career.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Views the system as a valuable coordination mechanism for validating and disseminating research. The business model leverages network effects and prestige, allowing for arbitrage across different fields and pricing models. Extraction is seen as a fee for service (coordination, platform maintenance, branding).
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Simultaneously a victim (paying exorbitant subscription fees) and an enforcer (using journal prestige as a proxy for quality in tenure decisions). Exit is constrained by the need to compete for talent and prestige, which is tied to the existing journal ecosystem.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Analyzes the system's costs and benefits. Recognizes the genuine coordination function of peer review but also quantifies the immense value extraction by publishers. Sees the full hybrid nature of the constraint.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_exp_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high due to the direct monetization of free academic labor and publicly funded research. Suppression (0.75) is severe because the 'publish or perish' mandate and the prestige economy of top-tier journals create powerful lock-in, making alternative publishing models risky for career-conscious academics.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the junior professor on a tenure clock, the system is a Snare they cannot escape. For the publisher, it is a Rope that efficiently coordinates the validation and distribution of science. For the library consortium or university, it is a Tangled Rope: a necessary coordination tool that has become captured by extractive interests.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is clear: value flows from academics (labor) and universities/public funders (money) to the shareholders of large publishing houses. Publishers are the unambiguous beneficiaries. Junior academics, university libraries, and the general public who cannot access research they funded are the victims.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure Snare would ignore the genuine, and historically crucial, coordination function that peer review and journal curation provide. Classifying it as a Rope would ignore the massive, asymmetric extraction. The Tangled Rope classification is essential as it correctly identifies the system as a hybrid: a coordination mechanism that has been co-opted for rent-seeking, where the enforcement by tenure committees maintains the structure long after its economic logic has become pathological.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_as_quality_proxy,
    'Is the journal prestige system a necessary, if expensive, proxy for research quality, or is it a purely inertial system of rent-seeking that actively hinders superior evaluation methods?',
    'Longitudinal study comparing career outcomes and research impact for academics who publish in top-tier paywalled journals versus those who use high-quality open-access platforms, controlling for institutional affiliation.',
    'If it's a necessary proxy, the system remains a Tangled Rope. If it's purely inertial rent-seeking, it degrades into a Snare from all but the beneficiary's perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_as_quality_proxy, empirical, 'Whether journal prestige is a functional quality signal or an inertial rent-seeking mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_exp_r1, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u2_exp_r1, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(acad_tr_t2002, academic_peer_review_gatekeeping_u2_exp_r1, theater_ratio, 2002, 0.25).
narrative_ontology:measurement(acad_tr_t2024, academic_peer_review_gatekeeping_u2_exp_r1, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u2_exp_r1, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(acad_be_t2002, academic_peer_review_gatekeeping_u2_exp_r1, base_extractiveness, 2002, 0.55).
narrative_ontology:measurement(acad_be_t2024, academic_peer_review_gatekeeping_u2_exp_r1, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_exp_r1, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r1, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r1, public_access_to_research).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_exp_r1, academic_tenure_process).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
