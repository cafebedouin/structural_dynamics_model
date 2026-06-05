% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u3_exp_r1
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u3_exp_r1, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u3_exp_r1
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system relies on researchers and academics
 *   providing free labor (writing, reviewing, editing) to for-profit
 *   publishing houses. These publishers then erect paywalls and sell access
 *   to the finished research—often funded by public grants—back to the
 *   researchers' own universities at exorbitant subscription rates. The
 *   system is maintained by the 'publish or perish' culture of academia,
 *   where publication in high-prestige journals is essential for career
 *   advancement and tenure.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - must provide free labor and publish to secure a career.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture immense value from free labor and subscription fees.
 *   - University Libraries/Consortia: Institutional victims (institutional/constrained) - forced to pay escalating fees, often at the expense of other resources.
 *   - The General Public: Secondary victims (powerless/trapped) - denied access to publicly-funded research.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u3_exp_r1, 0.78).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u3_exp_r1, 0.82).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u3_exp_r1, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_exp_r1, extractiveness, 0.78).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_exp_r1, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u3_exp_r1, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u3_exp_r1, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u3_exp_r1, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u3_exp_r1, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u3_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u3_exp_r1, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u3_exp_r1, tenured_senior_faculty).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_exp_r1, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_exp_r1, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u3_exp_r1, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: The 'publish or perish' system makes participation non-optional for career survival, creating a coercive labor extraction dynamic.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: From the publisher's view, this is a coordination mechanism for quality control and dissemination, justifying its business model.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: Universities are trapped paying high fees for access to research their own faculty produced, recognizing both the coordination function and the severe extraction.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: The analytical view sees a system with a genuine coordination function (peer review) that has been captured for asymmetric value extraction.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u3_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u3_exp_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u3_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u3_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is extremely high, reflecting the direct conversion of free academic labor into corporate profit. Suppression (0.82) is also very high due to the institutional lock-in of the tenure system and journal impact factors, which makes alternative publishing models (like pre-print servers or institutional repositories) appear less viable for career progression.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists. The publisher frames the system as a Rope, a necessary service for coordinating peer review and ensuring research quality. The junior academic, trapped by career incentives, experiences it as a Snare of coercive, uncompensated labor. The university, paying the bills, sees a Tangled Rope: a necessary evil that provides a service (prestige sorting) but at an extractive cost.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. Value flows from academics (as authors and reviewers) and their institutions (via subscription fees) to the shareholders of publishing companies. Publishers are the clear beneficiaries. Academics, universities, and the public are the victims who bear the costs of labor and restricted access.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case for the Tangled Rope classification, resolving a potential mandatrophy. To classify it as a pure Snare would ignore the very real, albeit overpriced, coordination function it serves in organizing peer review and signaling prestige. To classify it as a Rope would be to ignore the gargantuan and asymmetric extraction of value. The Tangled Rope classification correctly identifies that a legitimate coordination mechanism has been captured and financialized to a predatory degree.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_as_coordination,
    'Is the prestige signaling of top-tier journals an irreplaceable coordination good, or a theatrical justification for rent-seeking?',
    'Longitudinal analysis of citation and impact metrics for prestige open-access journals versus traditional paywalled journals, controlling for field and funding.',
    'If prestige is a robust and irreplaceable coordination signal, the system remains a Tangled Rope. If it is primarily theatrical, the system degrades towards a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_as_coordination, empirical, 'Whether journal prestige is an essential coordination good or theatrical rent-seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u3_exp_r1, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1975, academic_peer_review_gatekeeping_u3_exp_r1, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u3_exp_r1, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(acad_tr_t2025, academic_peer_review_gatekeeping_u3_exp_r1, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(acad_be_t1975, academic_peer_review_gatekeeping_u3_exp_r1, base_extractiveness, 1975, 0.3).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u3_exp_r1, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(acad_be_t2025, academic_peer_review_gatekeeping_u3_exp_r1, base_extractiveness, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u3_exp_r1, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_exp_r1, academic_hiring_and_tenure).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_exp_r1, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u3_exp_r1, public_access_to_research).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
