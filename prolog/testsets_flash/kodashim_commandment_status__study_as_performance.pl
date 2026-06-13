% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Kodashim Commandment Status: Study as Performance
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the halakhic reading that studying the laws of
 *   sacrifices (Kodashim) is considered a fulfillment of the commandment
 *   itself, particularly in the absence of the Temple. This reading maintains
 *   the spiritual and intellectual engagement with a central aspect of Jewish
 *   law, preventing the commandment from becoming obsolete or a source of
 *   unfulfilled obligation. It is a 'Rope' because it genuinely coordinates
 *   religious practice and provides a path to observance for its
 *   beneficiaries, with minimal extraction or suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.05).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.1).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.05).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Kodashim Commandment Status: Study as Performance").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious_studies/halakhic_theory/commitment_system_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, 'a40a25a4-9421-4744-81ca-cc9365f32270').
narrative_ontology:cs_kernel_codification('a40a25a4-9421-4744-81ca-cc9365f32270', fixed_text).
narrative_ontology:cs_authority_grounding('a40a25a4-9421-4744-81ca-cc9365f32270', lineage).
narrative_ontology:cs_interpretation_layer_present('a40a25a4-9421-4744-81ca-cc9365f32270').
narrative_ontology:cs_reading_relation('a40a25a4-9421-4744-81ca-cc9365f32270', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('a40a25a4-9421-4744-81ca-cc9365f32270', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('a40a25a4-9421-4744-81ca-cc9365f32270', foundational, study_is_equivalent_to_performance).
narrative_ontology:cs_axiom_status(study_is_equivalent_to_performance, holdable).
narrative_ontology:cs_axiom_grounding('a40a25a4-9421-4744-81ca-cc9365f32270', study_is_equivalent_to_performance, theological).
narrative_ontology:cs_axiom('a40a25a4-9421-4744-81ca-cc9365f32270', secondary, divine_commandment_is_always_fulfillable).
narrative_ontology:cs_axiom_status(divine_commandment_is_always_fulfillable, holdable).
narrative_ontology:cs_axiom_grounding('a40a25a4-9421-4744-81ca-cc9365f32270', divine_commandment_is_always_fulfillable, deontological).
narrative_ontology:cs_reference_frame('a40a25a4-9421-4744-81ca-cc9365f32270', rabbinic_halakhic_continuity).
narrative_ontology:cs_drift_state('a40a25a4-9421-4744-81ca-cc9365f32270', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a40a25a4-9421-4744-81ca-cc9365f32270', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, halakhic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, observant_jews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their intellectual engagement with Kodashim (laws of sacrifices) is elevated to the status of fulfilling the commandment itself, providing a continuous, meaningful role for their scholarship in the absence of the Temple. This reading validates their life's work and maintains the relevance of their field.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_scholars, beneficiary,
    institutional, generational, identity_locked, global).

% For them, studying the laws of sacrifices provides a direct means to fulfill a divine commandment that would otherwise be impossible in the absence of the Temple. This offers spiritual comfort and a path to religious observance, preventing a sense of loss or incompleteness in their religious practice.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, observant_jews, beneficiary,
    organized, biographical, identity_locked, global).

% While not directly harmed, this reading diminishes the urgency of their efforts to rebuild the Temple and restore sacrificial worship. They would argue that study is a substitute, not a full fulfillment, and that actual performance is required.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, messianic_activists, excluded,
    moderate, generational, constrained, global).

% Analyze the evolution of halakhic interpretations and the social functions of religious law in adapting to changing historical circumstances. They observe how this reading maintains the coherence and continuity of religious practice.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, secular_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious practice of observant Jews by providing a viable, accessible means to fulfill the commandment of sacrifices in the absence of the Temple, maintaining continuity of religious obligation and intellectual engagement with sacred texts.
% TRANSFER_FUNCTION: Transfers the spiritual merit and fulfillment associated with performing sacrifices to the act of studying their laws, from the divine source to the individual practitioner and scholar.
% ABSENT_VOICES: Messianic activists would object, arguing that study is a deferral, not a fulfillment, and that the true commandment requires physical performance in a rebuilt Temple. Their voices are present in the broader discourse but are structurally excluded from this reading's internal logic of fulfillment.
% DISAPPEARANCE_RATIONALE: If this reading vanished, observant Jews would lose a primary means of fulfilling a central commandment, leading to a significant spiritual void and potential crisis of religious practice. Halakhic scholarship on Kodashim would lose much of its immediate religious relevance, and the continuity of Jewish law would be challenged.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the performance of sacrificial commandments impossible, creating a profound theological and practical challenge for Jewish religious life.
% FOUNDING_PROBLEM_CORROBORATION: The problem of Temple absence remains live and is universally acknowledged across all branches of Judaism. The solution of study as performance is attested by centuries of rabbinic tradition and is a cornerstone of contemporary Orthodox Jewish practice, corroborated by countless religious texts and communal adherence.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).
:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) as this reading primarily offers a path to fulfillment rather than imposing costs. Suppression is low (0.1) because it is a voluntary interpretive framework, not coercively enforced. Theater ratio is zero (0.0) as the act of study is genuinely considered a valid form of performance within this framework, not a mere substitute. Accessibility collapse is high (0.9) because, for those who accept this reading, the alternative of not fulfilling the commandment is largely removed. Resistance is low (0.05) as this reading is widely accepted within Orthodox Judaism.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap among those who accept this reading, as it offers a clear and beneficial path to religious observance. The primary 'gap' is with those who adhere to sibling readings, who would view this as a deferral or an incomplete fulfillment, rather than a full one.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars and observant Jews are direct beneficiaries (d=0.0-0.1) as this reading provides them with a means of religious fulfillment and validates their intellectual and spiritual pursuits. There are no direct victims; those who disagree (e.g., messianic activists) are 'excluded' from this reading's internal logic of fulfillment, but not directly harmed by its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents mandatrophy by re-interpreting the 'mandate' of sacrifice laws to include study, thus keeping the commandment 'live' even when its original mode of performance is impossible. It avoids the piton trap by maintaining genuine function and benefit, rather than theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fulfillment_completeness_ambiguity,
    'Is ''study as performance'' considered a complete fulfillment of the commandment, or a partial/substitute fulfillment?',
    'Analysis of authoritative halakhic texts and rabbinic responsa regarding the qualitative equivalence of study versus physical performance.',
    'If partial, the effective ''extraction'' (in terms of unfulfilled spiritual obligation) might be higher for some adherents, potentially shifting the classification towards a ''Tangled Rope'' for those who feel a residual lack. If complete, the ''Rope'' classification holds strongly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fulfillment_completeness_ambiguity, conceptual, 'Ambiguity regarding the qualitative completeness of fulfillment through study.').

omega_variable(
    natural_law_vs_interpretation,
    'Is the equivalence of study and performance an inherent, ''natural'' aspect of divine law, or a rabbinic interpretation developed to address historical circumstances?',
    'Theological and philosophical analysis of the nature of divine commandments and the scope of rabbinic authority in re-interpreting their modes of fulfillment.',
    'If inherent, the constraint leans more towards a ''Mountain'' (a discovered truth). If interpretive, it remains a ''Rope'' (a constructed coordination mechanism), with the potential for alternative interpretations to emerge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_interpretation, conceptual, 'Whether the ''study as performance'' equivalence is inherent or interpretive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.0).
narrative_ontology:measurement(koda_tr_t650, kodashim_commandment_status__study_as_performance, theater_ratio, 650, 0.0).
narrative_ontology:measurement(koda_tr_t1300, kodashim_commandment_status__study_as_performance, theater_ratio, 1300, 0.0).
narrative_ontology:measurement(koda_tr_t1950, kodashim_commandment_status__study_as_performance, theater_ratio, 1950, 0.0).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(koda_be_t650, kodashim_commandment_status__study_as_performance, base_extractiveness, 650, 0.05).
narrative_ontology:measurement(koda_be_t1300, kodashim_commandment_status__study_as_performance, base_extractiveness, 1300, 0.05).
narrative_ontology:measurement(koda_be_t1950, kodashim_commandment_status__study_as_performance, base_extractiveness, 1950, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__study_as_performance, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(koda_su_t650, kodashim_commandment_status__study_as_performance, suppression_requirement, 650, 0.1).
narrative_ontology:measurement(koda_su_t1300, kodashim_commandment_status__study_as_performance, suppression_requirement, 1300, 0.1).
narrative_ontology:measurement(koda_su_t1950, kodashim_commandment_status__study_as_performance, suppression_requirement, 1950, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
