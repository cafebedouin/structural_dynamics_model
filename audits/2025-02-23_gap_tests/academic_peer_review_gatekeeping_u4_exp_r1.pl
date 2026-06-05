% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u4_exp_r1
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u4_exp_r1, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u4_exp_r1
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model is a system where the primary producers and
 *   quality-control laborers (academics) provide their work for free to
 *   for-profit publishers. These publishers then erect paywalls and sell
 *   access to the aggregated research back to the institutions that employ
 *   the researchers, often at immense profit margins. The system is
 *   maintained by a prestige economy, where career advancement (e.g., tenure)
 *   is tied to publication in high-status, paywalled journals.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - provide free labor (research, review) under career pressure.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture and monetize the value of academic labor.
 *   - University Libraries/Consortia: Institutional victims (institutional/constrained) - forced to pay escalating subscription fees.
 *   - The General Public: Secondary victims (powerless/trapped) - denied access to publicly-funded research.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u4_exp_r1, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u4_exp_r1, 0.72).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u4_exp_r1, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r1, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r1, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u4_exp_r1, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u4_exp_r1, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u4_exp_r1, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u4_exp_r1, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u4_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u4_exp_r1, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u4_exp_r1, tenured_senior_academics).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r1, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r1, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u4_exp_r1, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: The junior academic who must 'publish or perish' experiences the system as a pure extraction mechanism, providing free labor (research, writing, reviewing) in exchange for a chance at career survival, with no alternative prestigious venues.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: The publisher views the system as a valuable coordination mechanism for ensuring research quality and prestige, from which they derive a legitimate profit. Their arbitrage power allows them to optimize operations globally.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: The analytical observer sees both the genuine (if inefficient) coordination function of quality filtering and the massive, asymmetric extraction of value from publicly-funded research for private profit.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: Library consortia are institutional actors trapped within the system. They must pay exorbitant fees to access research produced by their own faculty, making them both victims of extraction and enforcers of the system's legitimacy.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u4_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u4_exp_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u4_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u4_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.68) reflects the enormous profit margins of major publishers built on free labor and content. The high suppression (0.72) reflects the 'publish or perish' culture and the tenure system, which locks academics into participating and marginalizes alternative models like open-access journals or pre-print servers as less prestigious.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Junior Professor (Snare), who is trapped in a system of uncompensated labor for career survival, and the Publisher (Rope), who frames the system as a necessary service for coordinating and validating scientific knowledge. The analytical view (Tangled Rope) acknowledges both the claimed coordination function and the undeniable, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is clear: value flows from researchers and their institutions (and by extension, public funding bodies) to the shareholders of publishing houses. Junior professors and university libraries are the primary cost-bearers. Publishers and senior academics who serve as editors (gaining prestige) are the primary beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification is crucial. Labeling the system a pure Snare would ignore the (real or perceived) coordination function of quality control that provides its ideological justification. Labeling it a Rope would ignore the massive extraction of wealth. The Tangled Rope correctly identifies that a system can perform a coordination function while simultaneously being highly extractive and coercive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_control_vs_extraction,
    'Is the peer review system a genuine coordination mechanism for quality control, or a theatrical justification for rent-seeking by publishers?',
    'Comparative analysis of research quality, retraction rates, and impact between high-prestige journals and well-moderated open-access platforms or pre-print archives.',
    'If the quality control function is proven to be minimal or illusory, the constraint would collapse from a Tangled Rope to a pure Snare from the analytical perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_control_vs_extraction, empirical, 'Ambiguity between the system's claimed coordination function (quality control) and its observed extractive outcomes (rent-seeking).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u4_exp_r1, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1975, academic_peer_review_gatekeeping_u4_exp_r1, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(acad_tr_t1998, academic_peer_review_gatekeeping_u4_exp_r1, theater_ratio, 1998, 0.3).
narrative_ontology:measurement(acad_tr_t2024, academic_peer_review_gatekeeping_u4_exp_r1, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(acad_be_t1975, academic_peer_review_gatekeeping_u4_exp_r1, base_extractiveness, 1975, 0.22).
narrative_ontology:measurement(acad_be_t1998, academic_peer_review_gatekeeping_u4_exp_r1, base_extractiveness, 1998, 0.48).
narrative_ontology:measurement(acad_be_t2024, academic_peer_review_gatekeeping_u4_exp_r1, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u4_exp_r1, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u4_exp_r1, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u4_exp_r1, scientific_reproducibility_crisis).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u4_exp_r1, tenure_and_promotion_systems).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
