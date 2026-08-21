% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__study_as_exercise, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Kodashim Corpus: Study as Mitzvah Performance
 *   domain: religious_studies/rabbinic_judaism
 *
 * SUMMARY:
 *   This constraint describes the rabbinic Jewish understanding that the
 *   study of the laws of sacrifice (Kodashim) is itself a form of spiritual
 *   performance, fulfilling the mitzvah (divine commandment) in the absence
 *   of the Temple. The kernel, the divine command regarding sacrifice, is
 *   'occupied' through continuous intellectual and spiritual engagement. This
 *   reading posits zero extraction, as study is considered a complete
 *   fulfillment, with no one deprived. It functions as a Rope, coordinating
 *   shared interpretive practice and maintaining communal spiritual
 *   continuity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.05).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.1).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.05).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Kodashim Corpus: Study as Mitzvah Performance").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious_studies/rabbinic_judaism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, 'e6de2c4e-35b2-4477-a861-0f9ac96d3f8b').
narrative_ontology:cs_kernel_codification('e6de2c4e-35b2-4477-a861-0f9ac96d3f8b', fixed_text).
narrative_ontology:cs_authority_grounding('e6de2c4e-35b2-4477-a861-0f9ac96d3f8b', lineage).
narrative_ontology:cs_interpretation_layer_present('e6de2c4e-35b2-4477-a861-0f9ac96d3f8b').
narrative_ontology:cs_reading_relation('e6de2c4e-35b2-4477-a861-0f9ac96d3f8b', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('e6de2c4e-35b2-4477-a861-0f9ac96d3f8b', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('e6de2c4e-35b2-4477-a861-0f9ac96d3f8b', foundational, study_is_equivalent_to_performance).
narrative_ontology:cs_axiom_status(study_is_equivalent_to_performance, holdable).
narrative_ontology:cs_axiom_grounding('e6de2c4e-35b2-4477-a861-0f9ac96d3f8b', study_is_equivalent_to_performance, theological).
narrative_ontology:cs_axiom('e6de2c4e-35b2-4477-a861-0f9ac96d3f8b', secondary, intellectual_engagement_maintains_cosmic_order).
narrative_ontology:cs_axiom_status(intellectual_engagement_maintains_cosmic_order, holdable).
narrative_ontology:cs_axiom_grounding('e6de2c4e-35b2-4477-a861-0f9ac96d3f8b', intellectual_engagement_maintains_cosmic_order, theological).
narrative_ontology:cs_reference_frame('e6de2c4e-35b2-4477-a861-0f9ac96d3f8b', rabbinic_interpretive_tradition_post_temple_destruction).
narrative_ontology:cs_drift_state('e6de2c4e-35b2-4477-a861-0f9ac96d3f8b', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e6de2c4e-35b2-4477-a861-0f9ac96d3f8b', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary practitioners and interpreters of Kodashim. Their continuous intellectual-spiritual engagement with the texts is considered the fulfillment of the mitzvah, maintaining cosmic order. Their professional and spiritual identity is deeply fused with this practice.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefits from the spiritual and communal continuity provided by the ongoing study of Kodashim. The community's sense of connection to tradition and divine will is sustained by this interpretive practice, even in the absence of physical sacrifice.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, jewish_community, beneficiary,
    organized, generational, identity_locked, global).

% Hold that only physical performance of sacrifices is true fulfillment. While they may engage in study, they view it as preparatory or secondary, not as the mitzvah itself. Their voice is marginalized within the dominant rabbinic discourse that emphasizes study.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, messianic_restorationists, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the spiritual and intellectual engagement of the Jewish community with the divine commandments concerning sacrifice, providing a continuous path for religious observance in the absence of the Temple.
% TRANSFER_FUNCTION: Transfers spiritual merit and communal continuity from the act of study by rabbinic scholars to the broader Jewish community, maintaining a connection to the divine will.
% ABSENT_VOICES: Those who believe only physical performance constitutes the mitzvah are largely excluded from the interpretive framework that elevates study to the status of performance. They would argue that this reading diminishes the urgency of Temple restoration.
% DISAPPEARANCE_RATIONALE: If the understanding that study is performance vanished, a core pillar of post-Temple Judaism would collapse. The spiritual engagement with a significant portion of Torah would be lost, leading to a profound crisis of religious meaning and practice for the Jewish community.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the cessation of physical sacrifices left a void in Jewish religious practice, particularly concerning the divine commandments related to Kodashim.
% FOUNDING_PROBLEM_CORROBORATION: The problem of how to observe the mitzvot of sacrifice in the absence of the Temple remains live for the entire Jewish community. Rabbinic literature across centuries, independent of any single benefiting party, attests to the ongoing theological and practical challenge this problem poses.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__study_as_exercise_tests).
:- end_tests(kodashim_corpus__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the act of study is considered a complete and fulfilling spiritual act, not a substitute or a lesser form. There is no 'cost' or 'deprivation' in this reading. Suppression is low (0.1) as this is a widely accepted and internalized interpretive tradition, requiring minimal active enforcement against dissenters, though alternative readings exist. Theater ratio is zero, as the study is the genuine function. Accessibility collapse is high (0.9) because for adherents of this reading, there are no other viable alternatives for fulfilling the mitzvah of Kodashim in the present era. Resistance is low (0.05) because this reading is foundational to post-Temple rabbinic Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars and the Jewish community, this constraint is a pure Rope, enabling spiritual continuity. From the perspective of messianic restorationists, it might be seen as a form of 'constrained' engagement, as it defers the physical performance they believe is ultimately required. However, within this specific reading, the study is complete in itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars are the agenda-setters and primary beneficiaries, as their intellectual engagement is the core of the practice. The broader Jewish community is also a beneficiary, deriving spiritual continuity and meaning. There are no direct victims in this reading, as the study is a complete fulfillment. Messianic restorationists are 'excluded' as their view of physical performance is not accommodated by this reading's core premise.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a vital spiritual practice as a degraded or extractive constraint. The 'mandate' (observance of sacrifice) is not atrophied but transformed and fulfilled through study, ensuring the constraint remains a genuine coordination mechanism for spiritual life, rather than an inertial performance or a cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a distinct reading of the Kodashim corpus kernel, or merely a variant of another reading?',
    'Analysis of core theological texts and rabbinic responsa to identify unique foundational axioms and their implications for practice and communal identity.',
    'If it is a distinct reading, its classification as a Rope stands. If it is a variant, its metrics and classification might be subsumed under a broader reading, potentially altering its perceived extractiveness or coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Clarifies the distinctness of the ''study as exercise'' reading within the Kodashim kernel.').

omega_variable(
    fulfillment_vs_substitution,
    'Is the study of Kodashim truly a ''fulfillment'' of the mitzvah, or a ''substitution'' for a lost practice?',
    'Theological and philosophical analysis of rabbinic texts concerning intentionality (kavanah) and the nature of divine command. This is a matter of internal theological coherence.',
    'If it is a mere substitution, the ''extractiveness'' might be re-evaluated as a form of spiritual compromise, potentially shifting the classification towards a more ''tangled'' form of coordination. If it is fulfillment, the Rope classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fulfillment_vs_substitution, conceptual, 'Examines the theological status of study in relation to physical sacrifice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(koda_be_t500, kodashim_corpus__study_as_exercise, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__study_as_exercise, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__study_as_exercise, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(koda_be_t2000, kodashim_corpus__study_as_exercise, base_extractiveness, 2000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__study_as_exercise, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(koda_su_t500, kodashim_corpus__study_as_exercise, suppression_requirement, 500, 0.1).
narrative_ontology:measurement(koda_su_t1000, kodashim_corpus__study_as_exercise, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__study_as_exercise, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement(koda_su_t2000, kodashim_corpus__study_as_exercise, suppression_requirement, 2000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
