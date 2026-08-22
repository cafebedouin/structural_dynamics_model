% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_performance, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_obligation__study_as_performance
 *   human_readable: Kodashim Obligation: Study as Performance
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint represents the reading of Kodashim (sacrificial law) that
 *   posits the study of these laws as a direct spiritual enactment of
 *   sacrifice itself, rendering the physical absence of the Temple irrelevant
 *   to the law's spiritual efficacy. It is a foundational theological claim
 *   within certain streams of Jewish thought, particularly after the
 *   destruction of the Second Temple. The constraint is claimed as a Mountain
 *   due to its perceived naturalness within this theological framework, with
 *   zero extraction, suppression, or theater, as the act of study is seen as
 *   inherently fulfilling a divine command and benefiting cosmic order.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_performance, 0.0).
domain_priors:suppression_score(kodashim_obligation__study_as_performance, 0.0).
domain_priors:theater_ratio(kodashim_obligation__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, extractiveness, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_obligation__study_as_performance, "Kodashim Obligation: Study as Performance").
narrative_ontology:topic_domain(kodashim_obligation__study_as_performance, "religious_studies/jewish_law/textual_preservation").

domain_priors:emerges_naturally(kodashim_obligation__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_performance, 'b4d2a848-7883-4f1f-9ed9-f35a01e05785').
narrative_ontology:cs_kernel_codification('b4d2a848-7883-4f1f-9ed9-f35a01e05785', fixed_text).
narrative_ontology:cs_authority_grounding('b4d2a848-7883-4f1f-9ed9-f35a01e05785', lineage).
narrative_ontology:cs_interpretation_layer_present('b4d2a848-7883-4f1f-9ed9-f35a01e05785').
narrative_ontology:cs_reading_relation('b4d2a848-7883-4f1f-9ed9-f35a01e05785', kodashim_obligation__study_as_preparation, coexists_with).
narrative_ontology:cs_reading_relation('b4d2a848-7883-4f1f-9ed9-f35a01e05785', kodashim_obligation__study_as_archive, coexists_with).
narrative_ontology:cs_axiom('b4d2a848-7883-4f1f-9ed9-f35a01e05785', foundational, study_is_performance).
narrative_ontology:cs_axiom_status(study_is_performance, holdable).
narrative_ontology:cs_axiom_grounding('b4d2a848-7883-4f1f-9ed9-f35a01e05785', study_is_performance, deontological).
narrative_ontology:cs_axiom('b4d2a848-7883-4f1f-9ed9-f35a01e05785', foundational, temple_absence_irrelevant_to_spiritual_efficacy).
narrative_ontology:cs_axiom_status(temple_absence_irrelevant_to_spiritual_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('b4d2a848-7883-4f1f-9ed9-f35a01e05785', temple_absence_irrelevant_to_spiritual_efficacy, theological).
narrative_ontology:cs_reference_frame('b4d2a848-7883-4f1f-9ed9-f35a01e05785', post_temple_destruction_rabbinic_response).
narrative_ontology:cs_drift_state('b4d2a848-7883-4f1f-9ed9-f35a01e05785', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b4d2a848-7883-4f1f-9ed9-f35a01e05785', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_performance, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, cosmic_order).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_performance, israelite_community).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, torah_immanence_doctrine).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_performance, spiritual_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate recipient of the spiritual efficacy generated by the study of Kodashim, maintaining the balance and divine presence in the world. It is not an agent but a concept that benefits from the constraint's operation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, cosmic_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_obligation__study_as_performance, cosmic_order).

% Benefits from the spiritual continuity and atonement achieved through the study, even in the absence of the Temple. This study reinforces their identity and connection to divine law.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, israelite_community, beneficiary,
    organized, generational, identity_locked, global).

% Are the primary interpreters and transmitters of Kodashim, actively engaging in its study and teaching. They uphold the doctrine that study itself is a form of sacrificial performance, ensuring its perpetuation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Believe that the physical Temple must be rebuilt for sacrificial law to be fully effective. They are excluded from the 'study as performance' framing, as it diminishes the urgency of physical restoration, but their views coexist within the broader community.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_performance, messianic_restorationists, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a continuous, accessible means for the Israelite community to engage with divine sacrificial law and maintain spiritual connection, independent of physical Temple presence.
% TRANSFER_FUNCTION: Transfers spiritual merit and cosmic balance from the act of study to the cosmic order and the Israelite community, in lieu of physical animal sacrifice.
% ABSENT_VOICES: Messianic restorationists would argue that while study is valuable, it cannot fully substitute for the physical performance of sacrifices in a rebuilt Temple, thus diminishing the spiritual efficacy claimed by this reading.
% DISAPPEARANCE_RATIONALE: If the belief that study enacts sacrifice vanished, a core mechanism for spiritual continuity and atonement in the absence of the Temple would be lost. The community's relationship to divine law and its self-understanding would fundamentally reorganize, potentially leading to spiritual crisis or the emergence of entirely new practices.
% FOUNDING_PROBLEM: The destruction of the Second Temple left the Israelite community without the central mechanism for atonement and divine service prescribed by the Torah, creating a profound spiritual and legal void.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing absence of the Temple and the continued need for spiritual connection and atonement within the Israelite community, attested by centuries of rabbinic literature and communal practice, corroborate the problem's live status. This is further supported by the persistence of diverse theological responses to the Temple's destruction.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_obligation__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_performance, 0.0, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_obligation__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_obligation__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_obligation__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_obligation__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the theological claim: extractiveness is zero because study is a pure act of devotion, not a means of extracting resources from participants. Suppression is zero because participation is voluntary and spiritually rewarding, with no coercion. Theater ratio is zero as the act is considered genuinely efficacious, not performative. Accessibility collapse is high (0.95) because, within this framework, there are no viable alternatives to study for achieving the spiritual function of sacrifice. Resistance is zero as this reading is widely accepted within its theological context.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the constraint is a natural, divinely ordained mechanism for spiritual continuity. From the perspective of messianic restorationists, it might be seen as a temporary, incomplete substitute, or even a distraction from the ultimate goal of physical restoration. The engine will compute this divergence based on the structural data of different readings.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'cosmic_order' is a non-agent beneficiary, receiving spiritual sustenance. The 'israelite_community' is a direct beneficiary, gaining spiritual continuity and atonement. 'Rabbinic_scholars' are agenda-setters, actively promoting and embodying this reading. 'Messianic_restorationists' are excluded, as their emphasis on physical Temple restoration challenges the completeness of 'study as performance'.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_theological_construct,
    'Is the spiritual efficacy of study a genuine natural law of the cosmos, or a theological construct developed to cope with historical circumstances?',
    'Theological and philosophical analysis of divine command theory and the nature of spiritual causality, potentially informed by comparative religious studies.',
    'If a construct, its ''mountain'' classification might be re-evaluated as a ''rope'' or ''tangled_rope'' if identifiable beneficiaries actively maintain the belief for institutional gain, even if extraction is currently zero. If a natural law, the mountain classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_theological_construct, conceptual, 'Ambiguity between inherent cosmic principle and human theological interpretation.').

omega_variable(
    spiritual_efficacy_measurement,
    'How can the ''spiritual efficacy'' of study be empirically or experientially verified, beyond theological assertion?',
    'Qualitative sociological studies of community experience, phenomenological analysis of religious practice, or the development of new epistemic frameworks for non-empirical verification.',
    'Lack of verifiable efficacy could lead to a re-evaluation of the constraint''s ''theater_ratio'' if the practice is maintained primarily for social cohesion rather than its claimed spiritual function, potentially shifting it towards a ''piton'' or ''tangled_rope'' if there are unacknowledged costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spiritual_efficacy_measurement, empirical, 'Verifiability of the core claim of spiritual efficacy.').

omega_variable(
    relationship_to_sibling_readings,
    'What is the precise structural relationship between ''study as performance'' and the ''study as preparation'' and ''study as archive'' readings?',
    'Detailed textual analysis of rabbinic sources, historical theological debates, and contemporary communal practices to map the logical and practical dependencies or contradictions between these readings.',
    'A clearer understanding of the relationships could reveal subtle forms of suppression or extraction if one reading implicitly or explicitly marginalizes others, potentially altering the classification of this or sibling constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relationship_to_sibling_readings, conceptual, 'Clarifying the inter-reading dynamics within the Kodashim kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_performance, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_obligation__study_as_performance, theater_ratio, 70, 0.0).
narrative_ontology:measurement(koda_tr_t500, kodashim_obligation__study_as_performance, theater_ratio, 500, 0.0).
narrative_ontology:measurement(koda_tr_t1000, kodashim_obligation__study_as_performance, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_performance, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(koda_tr_t2024, kodashim_obligation__study_as_performance, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_obligation__study_as_performance, base_extractiveness, 70, 0.0).
narrative_ontology:measurement(koda_be_t500, kodashim_obligation__study_as_performance, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(koda_be_t1000, kodashim_obligation__study_as_performance, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_performance, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(koda_be_t2024, kodashim_obligation__study_as_performance, base_extractiveness, 2024, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_obligation__study_as_performance, suppression_requirement, 70, 0.0).
narrative_ontology:measurement(koda_su_t500, kodashim_obligation__study_as_performance, suppression_requirement, 500, 0.0).
narrative_ontology:measurement(koda_su_t1000, kodashim_obligation__study_as_performance, suppression_requirement, 1000, 0.0).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_performance, suppression_requirement, 1500, 0.0).
narrative_ontology:measurement(koda_su_t2024, kodashim_obligation__study_as_performance, suppression_requirement, 2024, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_preparation).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_performance, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'kodashim_obligation' kernel. Each reading offers a different structural interpretation of the same body of law in the absence of the Temple, leading to different classifications and stakeholder dynamics. This reading emphasizes the immanent spiritual efficacy of study.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
