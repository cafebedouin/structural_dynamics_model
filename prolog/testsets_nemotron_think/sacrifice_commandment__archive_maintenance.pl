% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrificial Law Study as Archive Maintenance for Future Temple Restoration
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The archive_maintenance reading of the sacrifice commandment holds that
 *   studying the laws of Temple sacrifice (Seder Kodashim) is a distinct
 *   obligation whose purpose is preserving technical knowledge for a future
 *   Third Temple. Unlike the study_as_performance reading (study IS worship)
 *   or performance_only reading (commandment suspended without Temple), this
 *   reading explicitly denies present worship value: the study is archival,
 *   not devotional. The constraint extracts moderate effort from current
 *   scholars and students (who master a complex, practically inapplicable
 *   corpus) for the benefit of a future generation that may never exist.
 *   Communal enforcement (social pressure, curricular mandates, identity
 *   formation) sustains the practice. The extraction has increased over 1950
 *   years as the corpus expanded through commentary layers while the
 *   restoration horizon receded.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.45).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.3).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.45).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrificial Law Study as Archive Maintenance for Future Temple Restoration").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, 'a7672dd5-7343-42ff-983e-12aa51f99497').
narrative_ontology:cs_kernel_codification('a7672dd5-7343-42ff-983e-12aa51f99497', fixed_text).
narrative_ontology:cs_authority_grounding('a7672dd5-7343-42ff-983e-12aa51f99497', lineage).
narrative_ontology:cs_interpretation_layer_present('a7672dd5-7343-42ff-983e-12aa51f99497').
narrative_ontology:cs_reading_relation('a7672dd5-7343-42ff-983e-12aa51f99497', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('a7672dd5-7343-42ff-983e-12aa51f99497', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_axiom('a7672dd5-7343-42ff-983e-12aa51f99497', foundational, technical_knowledge_preservation_obligates_future_restoration).
narrative_ontology:cs_axiom_status(technical_knowledge_preservation_obligates_future_restoration, holdable).
narrative_ontology:cs_axiom_grounding('a7672dd5-7343-42ff-983e-12aa51f99497', technical_knowledge_preservation_obligates_future_restoration, deontological).
narrative_ontology:cs_axiom('a7672dd5-7343-42ff-983e-12aa51f99497', foundational, messianic_preparation_distinct_from_present_worship).
narrative_ontology:cs_axiom_status(messianic_preparation_distinct_from_present_worship, holdable).
narrative_ontology:cs_axiom_grounding('a7672dd5-7343-42ff-983e-12aa51f99497', messianic_preparation_distinct_from_present_worship, deontological).
narrative_ontology:cs_reference_frame('a7672dd5-7343-42ff-983e-12aa51f99497', exilic_preservation_framework).
narrative_ontology:cs_drift_state('a7672dd5-7343-42ff-983e-12aa51f99497', contemporary_statehood_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a7672dd5-7343-42ff-983e-12aa51f99497', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_temple_restorers).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, messianic_generation).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, current_kollel_students).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, current_halakhic_scholars).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, technical_knowledge_preservation_obligation).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, messianic_restoration_certainty).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, exilic_continuity_through_study).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Devote years to mastering complex sacrificial laws (Kodashim tractates) with no prospect of practical application in their lifetime. Their professional identity and communal standing depend on this study. Exit would mean abandoning their vocational identity and community.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, current_kollel_students, payer,
    moderate, biographical, identity_locked, global).

% Produce commentaries, curricula, and halakhic decisions that maintain the study framework. They bear the intellectual labor of keeping the system coherent. Their authority derives from mastery of this corpus; exit would undermine their epistemic position.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, current_halakhic_scholars, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__archive_maintenance, current_halakhic_scholars, agenda_setter).

% The hypothetical future generation that will receive the preserved technical knowledge when the Temple is rebuilt. They cannot consent, object, or exit — they are the structural beneficiaries of present extraction.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_temple_restorers, beneficiary,
    powerless, civilizational, trapped, universal).

% The eschatological community for whom the restored Temple service will be the culmination of history. The archive maintenance reading explicitly names them as the beneficiary; their existence is a theological postulate, not a demographic fact.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, messianic_generation, beneficiary,
    powerless, civilizational, trapped, universal).

% Define the canonical curriculum, authorize commentaries, and enforce communal norms around sacrificial study. They administer the constraint's reproduction. Their legitimacy depends on the kernel's authority; they cannot exit without dissolving their office.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, rabbinic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Study the phenomenon as a cultural transmission mechanism. They neither pay nor collect; they analyze the constraint's operation from outside the commitment framework.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, secular_academic_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the technical knowledge of Temple sacrificial service across generations of exile so that restoration capability exists when the messianic condition obtains.
% TRANSFER_FUNCTION: Moves study time, intellectual labor, and communal resources from the current generation of scholars and students to the future restoration capacity of a hypothetical messianic generation.
% ABSENT_VOICES: Those who would question whether Temple restoration is certain enough to justify present extraction — including Reform and Conservative movements that reject sacrificial restoration theology, secular Jews who bear communal costs without sharing the eschatological horizon, and potential future restorers who might prefer different knowledge priorities. They are structurally excluded by the kernel's authority structure.
% DISAPPEARANCE_RATIONALE: If the study obligation vanished overnight, the technical chain of transmission for Temple service would sever within a generation. The Kodashim corpus would become dead text rather than living practice. Restoration would become technically impossible, not merely politically deferred.
% FOUNDING_PROBLEM: How to maintain sacrificial competence during prolonged exile without a functioning Temple, given the biblical imperative that the sacrificial system is perpetual.
% FOUNDING_PROBLEM_CORROBORATION: Traditional sources (Rambam Hilkhot Beit HaBechirah, Rav Kook's Orot) attest the founding problem as live. Modern academic historians (Haym Soloveitchik, Yaakov Elman) corroborate from outside the beneficiary set that the study regime emerged as a specific historical response to the 70 CE destruction, not as an eternal decree. The 'contested' status reflects that the beneficiary community disputes whether exile continues.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).
:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects substantial present cost (years of study, opportunity cost) for uncertain future benefit. Suppression (0.3) is moderate: communal norms and identity-locking create pressure, but no physical coercion. Theater ratio (0.2) is low: the study is intellectually rigorous and genuinely preserves knowledge, though performative elements (recitation without comprehension) exist at margins. Accessibility collapse (0.5): alternatives (secular professions, other Torah study) exist but carry high identity cost. Resistance (0.2) is low: the constraint is widely accepted within its commitment community. The rising extractiveness over time tracks commentary proliferation without corresponding restoration proximity.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (rabbinic authorities), the constraint is a rope: genuine coordination preserving a civilization's ritual memory. From the payer seats (students/scholars), it is a tangled_rope: coordination function real but extraction asymmetric and identity-locked. From the analytical observer seat, the future beneficiary is a theological postulate — the extraction has no verified recipient. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Current students and scholars are payers (d ≈ 0.7-0.8): they bear the study burden with identity-locked exit. Future restorers are beneficiaries (d ≈ 0.0): they receive the preserved corpus without contributing. Rabbinic authorities are agenda_setters (d ≈ 0.3): they administer and benefit from the system's reproduction but also bear interpretive labor. The directionality derives from beneficiary/victim declarations plus identity_locked exit for payers and trapped exit for future beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining sacrificial competence during exile) was live for centuries. Whether it remains live depends on whether exile continues — a contested theological claim. If exile has ended (State of Israel, Temple Mount access), the mandate may have atrophied into piton. If exile continues, the mandate remains live but the extraction/benefit ratio worsens as time extends. The constraint prevents mislabeling: it is not pure extraction (snare) because the coordination function (knowledge preservation) is real and verified by textual continuity; it is not pure coordination (rope) because the beneficiary cannot consent and the payer cannot exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'How does the archive_maintenance reading structurally relate to its sibling readings of the sacrifice_commandment kernel?',
    'Comparative analysis of the three readings'' beneficiary/victim structures, extractiveness profiles, and authority grounding. The omega records the committer-frame decomposition for the corpus.',
    'If sibling readings are foreclosed rather than coexisting, the kernel has a single dominant reading. If they coexist, the kernel exhibits persistent interpretive pluralism. This affects whether the archive_maintenance constraint is a stable branch or a contested fragment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relationship of this reading to sibling readings in the sacrifice_commandment kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression maintaining sacrificial study structural (communal sanctions, funding structures) or internalized (identity fusion where study IS the self)?',
    'Post-exit trajectory analysis: track individuals who leave kollel frameworks — does the sense of obligation persist? Comparative study of baalei teshuva vs. lifelong participants.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint travels with the agent after formal exit. This would raise the constraint''s classification toward snare for identity-locked payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in religious identity-locked contexts').

omega_variable(
    restoration_certainty_epistemic_status,
    'Is the future beneficiary (messianic generation) a theological certainty, a probabilistic expectation, or a symbolic construct?',
    'Theological analysis of sources: Rambam''s 13 principles (certainty), Rav Kook''s dialectical messianism (process), modern non-literalist readings (symbolic). Survey of believing community''s actual epistemic stance.',
    'If certainty: extraction is justified coordination (lower χ). If probabilistic: extraction is speculative (higher χ). If symbolic: beneficiary is fictive — constraint is snare with no real beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_certainty_epistemic_status, preference, 'Epistemic status of the future beneficiary in the archive_maintenance reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_tr_t500, sacrifice_commandment__archive_maintenance, theater_ratio, 500, 0.12).
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_tr_t1000, sacrifice_commandment__archive_maintenance, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_tr_t1500, sacrifice_commandment__archive_maintenance, theater_ratio, 1500, 0.18).
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_tr_t1950, sacrifice_commandment__archive_maintenance, theater_ratio, 1950, 0.2).

% Extraction over time
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_be_t500, sacrifice_commandment__archive_maintenance, base_extractiveness, 500, 0.3).
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_be_t1000, sacrifice_commandment__archive_maintenance, base_extractiveness, 1000, 0.38).
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_be_t1500, sacrifice_commandment__archive_maintenance, base_extractiveness, 1500, 0.42).
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_be_t1950, sacrifice_commandment__archive_maintenance, base_extractiveness, 1950, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_su_t500, sacrifice_commandment__archive_maintenance, suppression_requirement, 500, 0.22).
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_su_t1000, sacrifice_commandment__archive_maintenance, suppression_requirement, 1000, 0.25).
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_su_t1500, sacrifice_commandment__archive_maintenance, suppression_requirement, 1500, 0.28).
narrative_ontology:measurement(sacrifice_commandment__archive_maintenance_su_t1950, sacrifice_commandment__archive_maintenance, suppression_requirement, 1950, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__archive_maintenance, 0.08).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).

% DUAL FORMULATION NOTE:
% This constraint and its siblings form the sacrifice_commandment kernel family. The archive_maintenance reading posits a future beneficiary and moderate extractiveness; study_as_performance posits present devotional benefit; performance_only posits suspended obligation. They share the kernel (biblical sacrificial commandment) but instantiate different constraints with different ε, beneficiaries, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__archive_maintenance, moderate, 0.75).
constraint_indexing:directionality_override(sacrifice_commandment__archive_maintenance, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
