% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u3_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u3_exp_r2, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u3_exp_r2
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model is a system where the primary producers and
 *   quality-control laborers (researchers) provide their work for free to
 *   for-profit publishers. These publishers then construct paywalls and sell
 *   access to the aggregated research back to the researchers' own
 *   institutions at extremely high prices. The system is maintained by the
 *   'publish or perish' culture of academia, where career advancement is tied
 *   to publication in high-prestige, publisher-controlled journals.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - must provide free labor to publish for tenure.
 *   - Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture and monetize the value of academic labor.
 *   - University Libraries/Consortia: Victims and auditors (institutional/analytical) - bear the financial costs while analyzing the system's inefficiencies.
 *   - University Administrations: Enforcers and victims (institutional/constrained) - perpetuate the system via tenure criteria while paying its costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u3_exp_r2, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u3_exp_r2, 0.75).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u3_exp_r2, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_exp_r2, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_exp_r2, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_exp_r2, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u3_exp_r2, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u3_exp_r2, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u3_exp_r2, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u3_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u3_exp_r2, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u3_exp_r2, tenured_senior_faculty).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_exp_r2, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_exp_r2, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_exp_r2, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the junior academic, the system is a coercive snare. Publication in prestigious, publisher-owned journals is a non-negotiable requirement for career survival (tenure), forcing them to provide free labor (writing, reviewing) to the very entities that extract value from their work.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the publisher's perspective, this is an elegant coordination mechanism (Rope). It uses academic ambition and university requirements to efficiently source, vet, and package research content at minimal cost, creating a highly profitable and defensible business model.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% University administrators are caught in the middle. They enforce the system by using journal prestige as a proxy for quality in tenure decisions, but are also victims of it, paying exorbitant subscription fees to publishers. Their exit is constrained by the need to maintain institutional rankings.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view sees the full structure: a genuine coordination function (quality signaling, however flawed) tangled with a highly extractive rent-seeking apparatus. This perspective understands both the system's utility and its immense cost.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u3_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u3_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u3_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the immense surplus value captured by publishers from uncompensated academic labor. The high suppression (0.75) represents the 'publish or perish' mandate and the prestige economy, which make alternative publishing routes career-limiting for junior academics, effectively suppressing competition.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the publisher, who views the system as an efficient Rope for coordinating quality control and distribution, and the junior researcher, who experiences it as a coercive Snare demanding labor without compensation for career survival. The analytical perspective of library consortia correctly identifies it as a Tangled Rope, acknowledging the coordination function (prestige signaling) is inextricably linked to the extractive business model.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is clear: value flows from researchers (as authors and reviewers) and their institutions (via library budgets) to the shareholders of large publishing houses. Publishers are the unambiguous beneficiaries. Junior faculty are the primary victims, providing the labor under duress. The general public is a secondary victim, denied access to publicly-funded research.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope is crucial for avoiding mandatrophy. A simple Snare classification would ignore the very real coordination function (however imperfect) that peer review and journal prestige provide, which is the source of the system's legitimacy and resilience. A Rope classification would ignore the staggering and asymmetric extraction. The Tangled Rope classification correctly identifies that a coordination mechanism has been captured and weaponized for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_control_vs_rent_seeking,
    'Is the publisher-led peer review system a necessary mechanism for ensuring scientific quality, or is it a historical artifact captured for rent-seeking that could be replaced by less extractive alternatives?',
    'A large-scale comparative study of research quality, retraction rates, and scientific impact between top-tier publisher journals and community-run, open-access platforms over a decade.',
    'If found to be a necessary quality filter, the constraint leans towards a high-cost Rope. If found to be replaceable with no loss of quality, it is confirmed as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_control_vs_rent_seeking, empirical, 'Distinguishes the coordination (quality control) from the extraction (profit) function of publisher gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u3_exp_r2, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1980, academic_peer_review_gatekeeping_u3_exp_r2, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u3_exp_r2, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(acad_tr_t2020, academic_peer_review_gatekeeping_u3_exp_r2, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(acad_be_t1980, academic_peer_review_gatekeeping_u3_exp_r2, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u3_exp_r2, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(acad_be_t2020, academic_peer_review_gatekeeping_u3_exp_r2, base_extractiveness, 2020, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u3_exp_r2, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_exp_r2, university_tenure_process).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_exp_r2, public_access_to_research).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
