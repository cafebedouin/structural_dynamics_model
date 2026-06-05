% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping_u1_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping_u1_exp_r3, []).

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
 *   constraint_id: academic_peer_review_gatekeeping_u1_exp_r3
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The academic publishing model functions as a constraint where
 *   researchers, primarily at public institutions, provide free labor
 *   (writing, peer review, editing) to for-profit publishers. These
 *   publishers then erect paywalls and sell access to the finished research
 *   back to the same institutions at extremely high costs. The system is
 *   maintained by the 'publish or perish' culture of academia, where career
 *   advancement (i.e., tenure) is tied to publishing in high-prestige,
 *   high-cost journals.
 *
 * KEY AGENTS:
 *   - Junior Professors: Primary victims (powerless/trapped) - must provide free labor and publish in prestige journals to secure a career.
 *   - For-Profit Journal Publishers: Primary beneficiaries (institutional/arbitrage) - capture immense value from free labor and institutional subscriptions.
 *   - University Libraries/Consortia: Victims and auditors (institutional/constrained) - bear the direct financial costs while attempting to negotiate better terms.
 *   - Tenured Senior Faculty: Secondary beneficiaries and enforcers (powerful/mobile) - uphold the system that grants them prestige and gatekeeping power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u1_exp_r3, 0.68).
domain_priors:suppression_score(academic_peer_review_gatekeeping_u1_exp_r3, 0.75).
domain_priors:theater_ratio(academic_peer_review_gatekeeping_u1_exp_r3, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r3, extractiveness, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping_u1_exp_r3, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping_u1_exp_r3, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping_u1_exp_r3, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping_u1_exp_r3, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping_u1_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u1_exp_r3, for_profit_journal_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping_u1_exp_r3, tenured_senior_faculty).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r3, junior_professors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r3, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping_u1_exp_r3, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the junior academic, the 'publish or perish' mandate makes participation non-optional. They provide free labor (writing, reviewing) to a system that extracts from their institution and determines their career trajectory, making it a classic Snare.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the publisher's perspective, the system is a highly efficient Rope. It coordinates the validation and dissemination of research using volunteer labor, creating a valuable, prestige-gated asset with minimal input costs.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r3, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the genuine coordination function (quality control via peer review) and the severe, asymmetric extraction. This hybrid nature is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Universities are both enforcers (via tenure committees) and victims (via high subscription costs). Their exit options are constrained by the need to maintain prestige, locking them into a Tangled Rope where they enforce the system that extracts from them.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r3, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_u1_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping_u1_exp_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping_u1_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(academic_peer_review_gatekeeping_u1_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) is high due to the direct conversion of free, publicly-funded labor into private profit. Suppression (0.75) is severe because tenure and funding systems create a near-total lack of viable alternatives for career-focused academics. The system actively punishes those who opt out.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Junior Professor (Snare) who is coerced into participation with no upside, and the Publisher (Rope) who sees an elegant system for coordinating and monetizing intellectual production. The analytical view (Tangled Rope) reconciles these by acknowledging the system's dual nature as both a coordination mechanism and an extractive enterprise.
 *
 * DIRECTIONALITY LOGIC:
 *   The flow of value is unidirectional. Labor and content flow from academics to publishers for free. Money flows from university libraries (often publicly funded) to publishers. The publishers are the clear beneficiaries. Junior faculty, libraries, and the public (denied access) are the victims.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope is critical. A simple Snare classification would miss the genuine, albeit co-opted, coordination function of peer review in validating scientific claims. This coordination function is the 'rope' that gives the system its legitimacy and persistence, even as the extractive 'snare' component has become dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_vs_quality_control,
    'Is the gatekeeping and high cost a necessary byproduct of ensuring high-quality research validation, or is it pure rent-seeking on an artificial prestige economy?',
    'Comparative analysis of research quality and impact between high-cost journals and high-quality, low-cost open-access platforms over a decade.',
    'If primarily for quality control, the system is a less extractive Tangled Rope. If primarily rent-seeking, it is a Snare from nearly all non-beneficiary perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_vs_quality_control, empirical, 'Whether the system's extraction is a necessary cost for quality control or pure rent-seeking on prestige.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping_u1_exp_r3, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t1975, academic_peer_review_gatekeeping_u1_exp_r3, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(acad_tr_t2000, academic_peer_review_gatekeeping_u1_exp_r3, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(acad_tr_t2025, academic_peer_review_gatekeeping_u1_exp_r3, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(acad_be_t1975, academic_peer_review_gatekeeping_u1_exp_r3, base_extractiveness, 1975, 0.25).
narrative_ontology:measurement(acad_be_t2000, academic_peer_review_gatekeeping_u1_exp_r3, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(acad_be_t2025, academic_peer_review_gatekeeping_u1_exp_r3, base_extractiveness, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping_u1_exp_r3, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u1_exp_r3, university_funding_models).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u1_exp_r3, scientific_reproducibility_crisis).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping_u1_exp_r3, intellectual_property_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
