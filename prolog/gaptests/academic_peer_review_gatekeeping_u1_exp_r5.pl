% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u1_exp_r5
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u1_exp_r5, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u1_exp_r5
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system functions as a coordination mechanism for
 *   scientific discourse (peer review) but has been captured by for-profit
 *   publishers. These entities leverage the free labor of academics (as
 *   authors, reviewers, and editors) to create a product (journals) which
 *   they then sell at extremely high prices back to the very institutions
 *   that employ the academics. The system is maintained by the 'publish or
 *   perish' culture of academia, where career advancement is tied to
 *   publication in high-prestige, publisher-owned journals.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - must provide free labor and publish in high-cost journals to secure tenure.
 *   - Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture the value of academic labor and sell it back to the community.
 *   - University Libraries: Secondary victims (institutional/constrained) - forced to pay exorbitant subscription fees, draining university resources.
 *   - Tenured Faculty/Tenure Committees: Enforcers (institutional/constrained) - uphold the system by evaluating candidates based on prestige metrics tied to publisher-owned journals.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u1_exp_r5, 0.78).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u1_exp_r5, 0.8).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u1_exp_r5, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r5, extractiveness, 0.78).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r5, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r5, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u1_exp_r5, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u1_exp_r5, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u1_exp_r5, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u1_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u1_exp_r5, journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u1_exp_r5, tenured_faculty_gatekeepers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r5, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r5, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r5, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: The untenured academic who must 'publish or perish', providing free labor into a system that extracts from their own institution. Exit is career-ending.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: The for-profit publisher who sees the system as a valuable coordination mechanism for vetting and disseminating scientific knowledge, from which they derive a legitimate profit.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r5, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: The analytical observer who sees both the genuine coordination function of peer review and the severe, asymmetric extraction imposed by the publisher business model.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: The university, which is both a victim (via library budgets) and an enforcer (via tenure committees). It is constrained by the need for its faculty to participate in the prestige economy.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r5, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u1_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r5, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u1_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u1_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.78) is extremely high, reflecting the publishers' profit margins built on unpaid labor and public funding. Suppression (0.80) is also high; while alternatives like pre-print servers and open-access journals exist, the prestige economy and tenure requirements strongly disincentivize abandoning the established system.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the junior professor, it is a Snare; their career is trapped by the demand to feed a system that extracts from them. For the publisher, it is a Rope; a well-organized system for coordinating and monetizing scientific validation. The analytical view of a library consortium or the system as a whole reveals the Tangled Rope: a system with a genuine coordination function that has been co-opted for massive, asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of value flow is unambiguous. Labor and content flow from academics to publishers for free. Money flows from university libraries (often publicly funded) to publishers. The primary beneficiaries are the publishers' shareholders. The primary victims are the junior academics (who provide the labor under duress) and the public/students (whose tuition and taxes fund the buy-back of their own research).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case of mandatrophy. A system for scholarly communication (a Rope) has degraded into a highly extractive Tangled Rope. Classifying it as a pure Snare would ignore the real, albeit exploited, coordination function it still provides. Classifying it as a Rope would ignore the crippling extraction. The Tangled Rope classification correctly identifies that both are present, which is essential for designing interventions that might reform the system without destroying the coordination function entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_vs_quality_control,
    'Is the high cost and restrictive access a necessary byproduct of ensuring high-quality research, or is it a rent-seeking mechanism that primarily leverages prestige?',
    'Comparative analysis of research quality, retraction rates, and citation impact between high-cost subscription journals and high-quality, low-cost open-access alternatives.',
    'If primarily for quality control, the constraint might be a less extractive Tangled Rope. If primarily rent-seeking, it is a pure Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_vs_quality_control, empirical, 'Whether the system's extraction is a necessary cost for quality control or pure rent-seeking based on prestige.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u1_exp_r5, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1970, academic_peer_review_gatekeeping_u1_exp_r5, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(acad_tr_t1995, academic_peer_review_gatekeeping_u1_exp_r5, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(acad_tr_t2024, academic_peer_review_gatekeeping_u1_exp_r5, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(acad_be_t1970, academic_peer_review_gatekeeping_u1_exp_r5, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(acad_be_t1995, academic_peer_review_gatekeeping_u1_exp_r5, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(acad_be_t2024, academic_peer_review_gatekeeping_u1_exp_r5, base_extractiveness, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u1_exp_r5, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u1_exp_r5, university_tenure_system).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u1_exp_r5, public_access_to_research).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
