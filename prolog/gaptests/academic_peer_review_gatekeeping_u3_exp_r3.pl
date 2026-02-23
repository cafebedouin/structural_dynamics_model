% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u3_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u3_exp_r3, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u3_exp_r3
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model is a system where the primary producers of
 *   value (researchers, peer reviewers) provide their labor for free to
 *   for-profit publishers. These publishers then package the research and
 *   sell it back at extremely high margins to the researchers' own
 *   institutions, which are funded by public and private money. The system is
 *   maintained by the 'publish or perish' culture of academia, where career
 *   advancement is tied to publishing in high-prestige, publisher-owned
 *   journals.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - must participate to gain tenure.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture the value of free labor and institutional prestige.
 *   - Research Institutions (Universities): Secondary victims and enforcers (institutional/constrained) - pay exorbitant subscription fees while also using publisher prestige as a metric for hiring and promotion.
 *   - The General Public: Tertiary victims (powerless/trapped) - denied access to publicly-funded research.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u3_exp_r3, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u3_exp_r3, 0.75).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u3_exp_r3, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_exp_r3, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_exp_r3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_exp_r3, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u3_exp_r3, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u3_exp_r3, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u3_exp_r3, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u3_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u3_exp_r3, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u3_exp_r3, tenured_senior_faculty).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_exp_r3, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_exp_r3, research_institutions).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_exp_r3, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the junior academic facing 'publish or perish', the system is a coercive snare. Career progression is contingent on participation, with no viable alternatives for achieving the required prestige.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the publisher's perspective, the system is a highly efficient coordination mechanism (Rope) for vetting and disseminating research, creating a valuable, prestige-ranked information good.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r3, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the genuine coordination function (quality control, dissemination) and the severe asymmetric extraction, classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Universities are both victims (paying high subscription fees) and enforcers (via tenure committees). Their exit is constrained by the need for their researchers to access and publish in these same journals, making it a Tangled Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r3, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u3_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u3_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u3_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the immense profit margins built on unpaid labor. The high suppression (0.75) reflects the lack of viable, prestige-equivalent alternatives for career advancement, effectively locking academics into the system. The system requires active enforcement through tenure committees and grant-awarding bodies that use journal prestige as a primary heuristic for quality.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the publisher, who views the system as a value-add Rope for coordinating scientific discourse, and the junior academic, who experiences it as a coercive Snare demanding participation for career survival. The institution is caught in the middle, seeing a Tangled Rope where it is both a victim of extraction and an enforcer of the rules.
 *
 * DIRECTIONALITY LOGIC:
 *   Value flows directionally from researchers (labor) and their institutions (subscriptions) to the publishers (profits). Senior tenured faculty who have already navigated the system become secondary beneficiaries, as their status is reinforced by the high barriers to entry they overcame. Junior faculty and the public are the primary cost-bearers.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is critical here. A simple Snare classification would ignore the genuine (though arguably inefficient) coordination function that peer review provides. A Rope classification would ignore the massive, asymmetric extraction of value. The Tangled Rope correctly identifies that a system can perform a coordination function while simultaneously being highly extractive and coercive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_quality_correlation,
    'Is the journal prestige, which enables extraction, a reliable proxy for research quality and replicability, or is it a self-perpetuating social construct?',
    'Large-scale analysis of retraction rates, replication success, and long-term citation impact versus journal impact factor and publisher profit margins.',
    'If correlation is weak, the system's coordination claim collapses, and it becomes a pure Snare. If correlation is strong, it remains a classic Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_quality_correlation, empirical, 'Whether journal prestige is a valid proxy for research quality.').

omega_variable(
    open_access_viability,
    'Can alternative models (e.g., university-led open access platforms) achieve sufficient prestige to become a viable exit option for academics?',
    'Tracking the career outcomes of researchers who exclusively use alternative platforms over a 10-20 year period.',
    'If alternatives become viable, the suppression score would decrease, potentially shifting the classification from Tangled Rope to a less coercive form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_access_viability, empirical, 'The long-term viability of non-traditional publishing models as an exit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u3_exp_r3, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1960, academic_peer_review_gatekeeping_u3_exp_r3, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(acad_tr_t1990, academic_peer_review_gatekeeping_u3_exp_r3, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(acad_tr_t2024, academic_peer_review_gatekeeping_u3_exp_r3, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(acad_be_t1960, academic_peer_review_gatekeeping_u3_exp_r3, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(acad_be_t1990, academic_peer_review_gatekeeping_u3_exp_r3, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(acad_be_t2024, academic_peer_review_gatekeeping_u3_exp_r3, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u3_exp_r3, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_exp_r3, university_tenure_process).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_exp_r3, public_access_to_research).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
