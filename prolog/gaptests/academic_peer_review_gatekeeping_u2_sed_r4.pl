% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_sed_r4
% ============================================================================
% Version: 2.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_sed_r4, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_sed_r4
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing system functions as a mechanism where the primary
 *   producers of value (researchers, reviewers) provide free labor to
 *   for-profit publishers. These publishers then bundle the research and sell
 *   access back to the researchers' own institutions at exorbitant prices.
 *   The system is maintained by a prestige economy tied to career advancement
 *   (tenure), which suppresses alternatives.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary target (Powerless/Trapped) - Must publish in prestigious journals to secure tenure, providing free labor as authors and reviewers.
 *   - Journal Publishers: Primary beneficiary (Institutional/Arbitrage) - Capture immense value from free labor and public funding, with high profit margins.
 *   - University Libraries/Consortia: Victim/Auditor (Institutional/Constrained) - Forced to pay escalating subscription fees, draining university budgets.
 *   - The General Public: Victim (Powerless/Trapped) - Denied access to the results of publicly-funded research.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_sed_r4, 0.78).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_sed_r4, 0.8).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_sed_r4, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r4, extractiveness, 0.78).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r4, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r4, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_sed_r4, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_sed_r4, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_sed_r4, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_sed_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_sed_r4, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_sed_r4, tenured_gatekeepers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r4, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r4, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r4, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE JUNIOR PROFESSOR (SNARE). Career progression (tenure) is contingent on publishing in high-prestige journals, creating a coercive 'publish or perish' environment with no viable exit.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE JOURNAL PUBLISHER (ROPE). The system is an efficient coordination mechanism to acquire high-value content for free, add a veneer of prestige, and sell it at a massive markup.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r4, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE LIBRARY CONSORTIUM (TANGLED ROPE). Sees both the claimed coordination function (quality control) and the severe, asymmetric extraction that drains institutional budgets.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE OPEN ACCESS ADVOCATE (TANGLED ROPE). Acknowledges the need for peer review but views the current implementation as a captured system to be replaced or reformed.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r4, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_sed_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r4, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_sed_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_sed_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.78) is extremely high, reflecting the direct conversion of free labor and publicly funded research into private profit. Suppression (0.80) is also high due to the 'publish or perish' culture and the lock-in of journal prestige metrics with academic hiring and promotion, making exit nearly impossible for career-focused academics.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: for the junior professor, it is a Snare they must navigate to survive professionally. For the publisher, it is an elegant Rope, a highly effective and low-cost system for coordinating content acquisition and monetization. The analytical observer sees the Tangled Rope: a system with a legitimate coordination goal (vetting research quality) that has been almost entirely subsumed by an extractive, rent-seeking business model.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction is unambiguous. Value flows from researchers (labor), universities (salaries, library budgets), and public funding agencies towards the shareholders of a small number of large publishing houses. Senior tenured academics can become secondary beneficiaries by acting as gatekeeping editors, reinforcing the system that grants them prestige.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case of mandatrophy. The original mandate (coordination to ensure research quality) is still claimed, but the primary function has become extraction. Classifying it as a Tangled Rope correctly captures this duality, preventing it from being mislabeled as a pure Snare (which would ignore the coordination function that provides its legitimacy) or a Rope (which would ignore the crippling extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_vs_rent_seeking,
    'Is the high cost and restricted access a necessary byproduct of ensuring high-quality research, or is it primarily rent-seeking by publishers?',
    'Comparative analysis of article quality and impact between high-cost subscription journals and high-quality, low-cost open-access platforms.',
    'If primarily rent-seeking, the system is a pure Snare. If a necessary cost for quality, it remains a highly extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_vs_rent_seeking, empirical, 'Distinguishing the necessary cost of quality control from publisher rent-seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_sed_r4, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1970, academic_peer_review_gatekeeping_u2_sed_r4, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(acad_tr_t1995, academic_peer_review_gatekeeping_u2_sed_r4, theater_ratio, 1995, 0.4).
narrative_ontology:measurement(acad_tr_t2025, academic_peer_review_gatekeeping_u2_sed_r4, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(acad_be_t1970, academic_peer_review_gatekeeping_u2_sed_r4, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(acad_be_t1995, academic_peer_review_gatekeeping_u2_sed_r4, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(acad_be_t2025, academic_peer_review_gatekeeping_u2_sed_r4, base_extractiveness, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_sed_r4, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_sed_r4, university_hiring_and_tenure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
