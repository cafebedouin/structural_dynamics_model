% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__conceptualization_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Digital Money Emergence Boundary: Conceptualization Reading
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'conceptualization reading' of the digital
 *   money emergence boundary. It posits that digital money emerged when it
 *   became theoretically conceivable and formally described, rooted in
 *   advances in telecommunications and cryptography (e.g., David Chaum's work
 *   in 1985). This reading emphasizes intellectual history and foundational
 *   research, placing the emergence boundary earlier than readings focused on
 *   infrastructure or consumer adoption. The constraint is claimed as a
 *   Mountain because the theoretical possibility, once established, is an
 *   unchangeable fact of intellectual history, with negligible extraction or
 *   suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.05).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.02).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, mountain).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Emergence Boundary: Conceptualization Reading").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:emerges_naturally(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, 'f4a0fe11-a373-44b8-966c-18673c2297ea').
narrative_ontology:cs_kernel_codification('f4a0fe11-a373-44b8-966c-18673c2297ea', distributed).
narrative_ontology:cs_authority_grounding('f4a0fe11-a373-44b8-966c-18673c2297ea', expertise).
narrative_ontology:cs_reading_relation('f4a0fe11-a373-44b8-966c-18673c2297ea', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4a0fe11-a373-44b8-966c-18673c2297ea', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('f4a0fe11-a373-44b8-966c-18673c2297ea', foundational, theoretical_possibility_defines_emergence).
narrative_ontology:cs_axiom_status(theoretical_possibility_defines_emergence, holdable).
narrative_ontology:cs_axiom_grounding('f4a0fe11-a373-44b8-966c-18673c2297ea', theoretical_possibility_defines_emergence, conventional).
narrative_ontology:cs_reference_frame('f4a0fe11-a373-44b8-966c-18673c2297ea', chaum_formalization_era).
narrative_ontology:cs_drift_state('f4a0fe11-a373-44b8-966c-18673c2297ea', contemporary_multi_criteria_debate, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f4a0fe11-a373-44b8-966c-18673c2297ea', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_researchers).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, theoretical_computer_scientists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the establishment of intellectual priority and the recognition of foundational contributions to the field of digital money. Their careers and reputations are built on these conceptual milestones.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, academic_researchers, beneficiary,
    organized, generational, mobile, global).

% Their work on cryptographic protocols and distributed systems laid the theoretical groundwork for digital money. This reading validates their foundational contributions as the true 'emergence' point.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, theoretical_computer_scientists, beneficiary,
    organized, generational, mobile, global).

% Analyze and interpret the historical development of money and technology. They are interested in accurately dating the 'emergence' of digital money based on various criteria, including conceptual breakthroughs.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, financial_historians, observer,
    analytical, generational, analytical, global).

% While not directly involved in the conceptualization, their later policy decisions are influenced by how digital money's origins are understood. They might prefer a later 'emergence' date tied to practical implementation for regulatory purposes.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, central_bankers, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared intellectual framework and timeline for understanding the origins of digital money, allowing researchers to build upon recognized foundational concepts.
% TRANSFER_FUNCTION: Transfers intellectual priority and recognition to the academic and research communities responsible for the theoretical breakthroughs, influencing funding and career trajectories.
% ABSENT_VOICES: Policymakers and regulators, who often focus on the practical and legal emergence of digital money, might argue that theoretical possibility is not 'emergence' in a policy-relevant sense. Their perspective is often secondary in academic historical accounts.
% DISAPPEARANCE_RATIONALE: The historical facts of theoretical development (telecommunications advances, Chaum's work) are immutable. If this conceptualization boundary vanished, the underlying historical events would remain, though their interpretation as 'emergence' might shift to other criteria.
% FOUNDING_PROBLEM: To define the earliest point at which digital money became a coherent, theoretically understood concept, distinct from mere electronic record-keeping.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science and technology, as well as the academic community itself, corroborate the importance of theoretical milestones in defining the emergence of new technologies. This is attested by peer-reviewed literature and academic curricula.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, world_unchanged).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, ExtMetricName, E),
    domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(digital_money_emergence_boundary__conceptualization_reading),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects that while academic priority is a form of 'gain,' it's not a direct economic extraction from a broad base. Suppression (0.02) is minimal, as theoretical ideas are not coercively enforced. Accessibility collapse (0.95) is high because once the theoretical possibility is understood, there are few 'alternatives' to that historical fact. Resistance (0.01) is low, as the historical timeline of theoretical breakthroughs is largely uncontested, though its interpretation as 'emergence' is. The presence of beneficiaries (academic researchers) on a Mountain triggers the False Summit Mountain (FSM) detection, which is appropriate here as the 'naturalness' of this boundary is conceptually contested by other readings.
 *
 * PERSPECTIVAL GAP:
 *   While the conceptualization of digital money is a historical fact, its designation as the 'emergence boundary' is a matter of interpretation. Other readings (infrastructure, consumer holdings) would place the boundary later, leading to different classifications and beneficiary/victim sets. This story focuses solely on the conceptualization perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic researchers and theoretical computer scientists are beneficiaries (d near 0.0) as this reading validates their intellectual contributions. Financial historians are observers (d near 0.5), analyzing the phenomenon without direct benefit or cost. Central bankers are excluded (d near 1.0) as their policy-oriented perspective is not central to this conceptualization, and they might implicitly bear costs if this early definition complicates later regulatory efforts.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_practical_emergence,
    'Is ''emergence'' best defined by theoretical possibility, practical infrastructure, or consumer adoption?',
    'A consensus among historians and economists on a primary criterion for ''emergence'' in the context of new technologies, or a clear policy mandate for a specific definition.',
    'If practical or consumer-focused definitions gain primacy, this conceptualization reading would be reclassified from a Mountain to a less foundational type, or its significance as an ''emergence'' boundary would diminish.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_vs_practical_emergence, conceptual, 'Ambiguity in the definition of ''emergence'' for digital money.').

omega_variable(
    intellectual_priority_as_extraction,
    'To what extent does the establishment of intellectual priority (beneficiary of this reading) constitute a form of extraction, even if non-monetary?',
    'Sociological study of academic reward systems and their impact on resource allocation (e.g., grants, positions) within the field of digital currency research.',
    'If intellectual priority is shown to have significant, concentrated downstream effects on resource allocation, the extractiveness metric for this ''Mountain'' could be re-evaluated upwards, potentially triggering an FSM reclassification to a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intellectual_priority_as_extraction, empirical, 'The subtle extractive nature of intellectual priority claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 1960, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1960, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(digi_tr_t1970, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1970, 0.01).
narrative_ontology:measurement(digi_tr_t1985, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1985, 0.01).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1960, 0.01).
narrative_ontology:measurement(digi_be_t1970, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1970, 0.03).
narrative_ontology:measurement(digi_be_t1985, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1985, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1960, 0.01).
narrative_ontology:measurement(digi_su_t1970, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1970, 0.02).
narrative_ontology:measurement(digi_su_t1985, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1985, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
