% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money Origin: Concept Became Thinkable
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story defines the origin of digital money as the point
 *   when its concept became technically and institutionally conceivable,
 *   preceding widespread implementation. It emphasizes the intellectual and
 *   design breakthroughs that made digital money 'thinkable.' This is one
 *   reading of the 'digital_money_origin' kernel, focusing on the conceptual
 *   and theoretical emergence rather than practical use or regulatory
 *   recognition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.15).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.05).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, mountain).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money Origin: Concept Became Thinkable").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:emerges_naturally(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, '6d42d626-b27b-4ab8-a07e-8ec8c4079b65').
narrative_ontology:cs_kernel_codification('6d42d626-b27b-4ab8-a07e-8ec8c4079b65', implicit).
narrative_ontology:cs_authority_grounding('6d42d626-b27b-4ab8-a07e-8ec8c4079b65', expertise).
narrative_ontology:cs_interpretation_layer_present('6d42d626-b27b-4ab8-a07e-8ec8c4079b65').
narrative_ontology:cs_reading_relation('6d42d626-b27b-4ab8-a07e-8ec8c4079b65', digital_money_origin__first_held_reading, influences).
narrative_ontology:cs_reading_relation('6d42d626-b27b-4ab8-a07e-8ec8c4079b65', digital_money_origin__regulatory_recognition_reading, influences).
narrative_ontology:cs_axiom('6d42d626-b27b-4ab8-a07e-8ec8c4079b65', foundational, conceptual_precedence_over_implementation).
narrative_ontology:cs_axiom_status(conceptual_precedence_over_implementation, holdable).
narrative_ontology:cs_axiom_grounding('6d42d626-b27b-4ab8-a07e-8ec8c4079b65', conceptual_precedence_over_implementation, conventional).
narrative_ontology:cs_reference_frame('6d42d626-b27b-4ab8-a07e-8ec8c4079b65', conceptual_breakthrough_paradigm).
narrative_ontology:cs_drift_state('6d42d626-b27b-4ab8-a07e-8ec8c4079b65', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6d42d626-b27b-4ab8-a07e-8ec8c4079b65', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, academic_theorists).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, technological_determinism_thesis).
narrative_ontology:constraint_vindicates(digital_money_origin__became_thinkable_reading, conceptual_precedence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the individuals and groups who first conceived of and designed digital money systems, often within academic, governmental, or corporate research settings. Their work laid the conceptual groundwork, and this reading validates their foundational contributions.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_institutional_architects, beneficiary,
    institutional, generational, analytical, global).

% Scholars who study the history and evolution of money, particularly those who emphasize the role of ideas, technology, and institutional design in monetary innovation. This reading aligns with their theoretical frameworks.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, academic_theorists, beneficiary,
    analytical, generational, analytical, global).

% The broader population who would later adopt or be affected by digital money, but who were not directly involved in its initial conceptualization. They observe the emergence from a distance, without direct agency in its origin.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, general_public, observer,
    powerless, biographical, constrained, global).

% Central banks and financial regulators whose frameworks were initially challenged or bypassed by the conceptual emergence of digital money. They were not part of the 'thinkable' phase but later had to react to its implications.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, traditional_monetary_authorities, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared conceptual framework for what constitutes 'money' in a digital context, allowing diverse technical and institutional efforts to converge on a common understanding.
% TRANSFER_FUNCTION: Transfers intellectual credit and historical precedence to those who first conceptualized digital money, rather than those who first implemented or regulated it.
% ABSENT_VOICES: Those who emphasize the practical, lived experience of money (e.g., early users of digital payment systems) or the formal recognition by states would argue that 'thinkable' is too early a point of origin, but their perspectives are not central to this conceptual framing.
% DISAPPEARANCE_RATIONALE: The historical fact of when digital money became conceptually possible is fixed. If this reading vanished, the underlying historical events would remain, but the interpretation emphasizing conceptual emergence would be lost, shifting the narrative focus to later stages of implementation or regulation.
% FOUNDING_PROBLEM: The problem of understanding how money could exist in a purely digital form, given the historical reliance on physical or tangible representations of value.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and economics corroborate that the conceptual and technical hurdles for digital money were significant. However, the problem of 'how money could exist digitally' is now largely solved, making the founding problem 'dead' in a practical sense, even if its historical significance remains.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_unchanged).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, ExtMetricName, E),
    domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(digital_money_origin__became_thinkable_reading),
    narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because the 'thinkability' of a concept, once achieved, is an irreversible historical fact. Its extractiveness is low (0.15) as it primarily confers intellectual precedence rather than direct material gains. Suppression is minimal (0.05) because it's about a conceptual shift, not active coercion. Accessibility collapse is high (0.9) because, once the concept is understood, the 'unthinkable' alternative collapses. Resistance is low (0.01) as it's a historical interpretation, not an active policy. The beneficiaries are those who contributed to this conceptual shift, gaining academic and historical recognition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of early institutional architects and academic theorists, this reading is a straightforward account of historical progress. For those focused on practical implementation or regulatory control, this conceptual origin might seem too abstract or premature, leading to different classifications for the 'first_held_reading' or 'regulatory_recognition_reading' siblings.
 *
 * DIRECTIONALITY LOGIC:
 *   Early institutional architects and academic theorists are beneficiaries (d near 0.0) as this reading validates their foundational work. The general public and traditional monetary authorities are observers or excluded, with d values reflecting their lack of direct agency in this conceptual phase.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a fundamental conceptual shift as a purely extractive mechanism. While there are beneficiaries of this historical framing, the 'thinkability' itself is a structural feature of intellectual history, not a human-constructed constraint designed for extraction. The low extractiveness and high accessibility collapse support its Mountain classification, indicating that the concept's emergence is a fixed point, not a decaying mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint truly a ''Mountain'' of conceptual emergence, or does its beneficiary structure (early institutional architects) suggest a ''False Summit Mountain'' where intellectual credit masks a constructed narrative?',
    'Analysis of the historical record for active suppression of alternative conceptualizations or deliberate framing to exclude other origin narratives. If such suppression is found, reclassify as a ''Tangled Rope'' or ''Snare'' of intellectual history.',
    'If reclassified as a False Summit, the constraint''s effective extractiveness would be higher, reflecting the active maintenance of a beneficial narrative rather than a natural emergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between genuine conceptual emergence and a constructed narrative benefiting specific actors.').

omega_variable(
    origin_point_ambiguity,
    'Is the ''conceptually thinkable'' origin point the most appropriate for understanding digital money, or do later points (first use, regulatory recognition) offer more salient structural insights?',
    'Comparative analysis of the structural implications of each origin point for contemporary monetary systems. If later points reveal significantly different and more impactful constraint structures, this reading''s salience might diminish.',
    'A shift in salience could lead to prioritizing other readings of the ''digital_money_origin'' kernel, potentially reclassifying the overall ''origin'' as a ''Contested'' rather than a ''Mountain'' phenomenon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(origin_point_ambiguity, conceptual, 'The choice of origin point for digital money''s emergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 1970, 1990).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1970, digital_money_origin__became_thinkable_reading, theater_ratio, 1970, 0.01).
narrative_ontology:measurement(digi_tr_t1975, digital_money_origin__became_thinkable_reading, theater_ratio, 1975, 0.01).
narrative_ontology:measurement(digi_tr_t1980, digital_money_origin__became_thinkable_reading, theater_ratio, 1980, 0.02).
narrative_ontology:measurement(digi_tr_t1985, digital_money_origin__became_thinkable_reading, theater_ratio, 1985, 0.02).
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__became_thinkable_reading, theater_ratio, 1990, 0.02).

% Extraction over time
narrative_ontology:measurement(digi_be_t1970, digital_money_origin__became_thinkable_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(digi_be_t1975, digital_money_origin__became_thinkable_reading, base_extractiveness, 1975, 0.12).
narrative_ontology:measurement(digi_be_t1980, digital_money_origin__became_thinkable_reading, base_extractiveness, 1980, 0.13).
narrative_ontology:measurement(digi_be_t1985, digital_money_origin__became_thinkable_reading, base_extractiveness, 1985, 0.14).
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__became_thinkable_reading, base_extractiveness, 1990, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1970, digital_money_origin__became_thinkable_reading, suppression_requirement, 1970, 0.03).
narrative_ontology:measurement(digi_su_t1975, digital_money_origin__became_thinkable_reading, suppression_requirement, 1975, 0.04).
narrative_ontology:measurement(digi_su_t1980, digital_money_origin__became_thinkable_reading, suppression_requirement, 1980, 0.04).
narrative_ontology:measurement(digi_su_t1985, digital_money_origin__became_thinkable_reading, suppression_requirement, 1985, 0.05).
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__became_thinkable_reading, suppression_requirement, 1990, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__became_thinkable_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__first_held_reading).
narrative_ontology:affects_constraint(digital_money_origin__became_thinkable_reading, digital_money_origin__regulatory_recognition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'digital_money_origin' kernel, focusing on conceptual emergence. It influences subsequent readings by establishing an early historical baseline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
