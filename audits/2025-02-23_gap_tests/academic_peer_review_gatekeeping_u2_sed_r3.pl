% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u2_sed_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u2_sed_r3, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u2_sed_r3
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model functions as a constraint where the primary
 *   producers of value (researchers) provide their labor for free to
 *   for-profit publishers. These publishers then erect paywalls and sell
 *   access to the aggregated research back to the researchers' own
 *   institutions at extremely high profit margins. The system is maintained
 *   by the institutional requirement for academics to publish in prestigious
 *   journals to secure tenure and career advancement.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary target (powerless/trapped) - must publish to secure tenure, providing free labor as authors and reviewers.
 *   - For-Profit Journal Publishers: Primary beneficiary (institutional/arbitrage) - capture the value of free academic labor and sell it back to the system.
 *   - University Libraries/Consortia: Victim/Auditor (institutional/constrained) - must pay escalating subscription fees, often with limited power to negotiate.
 *   - The General Public: Victim (powerless/trapped) - largely excluded from accessing publicly-funded research.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_sed_r3, 0.78).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u2_sed_r3, 0.8).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u2_sed_r3, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r3, extractiveness, 0.78).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r3, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u2_sed_r3, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u2_sed_r3, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u2_sed_r3, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u2_sed_r3, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u2_sed_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_sed_r3, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u2_sed_r3, tenured_senior_academics).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r3, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r3, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u2_sed_r3, the_general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior academic, the 'publish or perish' mandate makes participation non-optional. They provide free labor (writing, reviewing) and their institution pays to access the result. This is a classic extraction trap.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the publisher's perspective, they are coordinating the complex process of peer review, production, and dissemination to ensure quality control. The system is a highly efficient value-creation mechanism.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r3, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the genuine coordination function of peer review and the immense, asymmetric extraction built upon it. The coordination is real, but so is the rent-seeking.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A tenured professor has more agency but is still embedded in the system. They benefit from its prestige while also contributing free labor. They see the flaws but also the coordination benefits, classifying it as a tangled rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r3, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u2_sed_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u2_sed_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u2_sed_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u2_sed_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.78) is extremely high, reflecting the publishers' ~40% profit margins built on free labor and content. Suppression (0.80) is also high; while alternatives like pre-print servers and open access journals exist, the prestige and career-incentive systems are overwhelmingly tied to the legacy publishers, making exit costly for academics.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the publisher, it's a Rope that coordinates quality control. For the junior academic, it's a Snare they cannot escape without sacrificing their career. The analytical view sees both sides, identifying the genuine (if inefficient) coordination function entangled with a highly extractive business model.
 *
 * DIRECTIONALITY LOGIC:
 *   The flow of value is unidirectional. Labor and content flow from academics to publishers for free. Money flows from university libraries (funded by tuition and public grants) to publishers. The publishers and the senior academics who act as gatekeepers are the primary beneficiaries. Junior academics, universities, and the public are the victims.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this system as a pure Snare would ignore the real, if often exaggerated, coordination function of peer review in filtering and signaling research quality. The Tangled Rope classification correctly identifies that participants are not merely trapped; they are complying with a system that has a dual function of both coordination and extraction. This prevents mislabeling the problem as pure coercion and focuses on the entanglement of function and rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_as_coordination_good,
    'Is the 'prestige' conferred by high-impact journals a genuine coordination good (signaling quality) or a manufactured scarcity that primarily serves the publisher's business model?',
    'Comparative analysis of citation/impact metrics for articles on pre-print servers versus their final journal versions, controlling for self-selection bias. Correlation of journal impact factor with retraction rates and reproducibility.',
    'If prestige is a genuine and efficient quality signal, the system is a Tangled Rope. If it is primarily manufactured scarcity with little correlation to quality, the system is a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_as_coordination_good, empirical, 'Whether journal prestige is a real quality signal or manufactured scarcity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u2_sed_r3, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t0, academic_peer_review_gatekeeping_u2_sed_r3, theater_ratio, 0, 0.2).
narrative_ontology:measurement(acad_tr_t15, academic_peer_review_gatekeeping_u2_sed_r3, theater_ratio, 15, 0.45).
narrative_ontology:measurement(acad_tr_t30, academic_peer_review_gatekeeping_u2_sed_r3, theater_ratio, 30, 0.6).

% Extraction over time
narrative_ontology:measurement(acad_be_t0, academic_peer_review_gatekeeping_u2_sed_r3, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(acad_be_t15, academic_peer_review_gatekeeping_u2_sed_r3, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(acad_be_t30, academic_peer_review_gatekeeping_u2_sed_r3, base_extractiveness, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u2_sed_r3, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_sed_r3, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_sed_r3, scientific_progress_velocity).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u2_sed_r3, tenure_and_promotion_systems).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
