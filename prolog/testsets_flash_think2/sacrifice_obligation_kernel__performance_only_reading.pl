% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__performance_only_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrifice Obligation: Performance-Only Reading
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint represents the 'performance-only' reading of the
 *   sacrifice obligation kernel within Jewish religious law. It asserts that
 *   the divine command for sacrifices requires physical performance in a
 *   rebuilt Temple, and that study of the laws of sacrifice, while
 *   meritorious, does not fulfill the mitzvah itself. The destruction of the
 *   Second Temple in 70 CE rendered this obligation physically impossible to
 *   fulfill, creating a state of perpetual unfulfilled command for the Jewish
 *   people. The constraint is classified as a Snare due to the high,
 *   persistent burden (extraction) on the Jewish people, maintained by the
 *   active enforcement of this specific halakhic interpretation, which
 *   suppresses alternative, less burdensome readings.
 *
 * KEY AGENTS:
 *   - halakhic_authorities: Agenda setter (institutional/identity_locked) — maintain and enforce the interpretation
 *   - entire_jewish_people: Payer (powerless/identity_locked) — bear the burden of unfulfilled obligation
 *   - proponents_of_alternative_readings: Excluded (moderate/constrained) — advocate for less literal interpretations
 *   - analytical_observers: Observer (analytical/analytical) — analyze the structural implications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.95).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.9).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, snare).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation: Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, '293f712f-dfac-4141-ba58-59356df17001').
narrative_ontology:cs_kernel_codification('293f712f-dfac-4141-ba58-59356df17001', fixed_text).
narrative_ontology:cs_authority_grounding('293f712f-dfac-4141-ba58-59356df17001', lineage).
narrative_ontology:cs_interpretation_layer_present('293f712f-dfac-4141-ba58-59356df17001').
narrative_ontology:cs_reading_relation('293f712f-dfac-4141-ba58-59356df17001', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('293f712f-dfac-4141-ba58-59356df17001', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('293f712f-dfac-4141-ba58-59356df17001', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('293f712f-dfac-4141-ba58-59356df17001', foundational, physical_performance_is_sine_qua_non).
narrative_ontology:cs_axiom_status(physical_performance_is_sine_qua_non, holdable).
narrative_ontology:cs_axiom_grounding('293f712f-dfac-4141-ba58-59356df17001', physical_performance_is_sine_qua_non, deontological).
narrative_ontology:cs_axiom('293f712f-dfac-4141-ba58-59356df17001', foundational, study_is_preparatory_not_substitutive).
narrative_ontology:cs_axiom_status(study_is_preparatory_not_substitutive, holdable).
narrative_ontology:cs_axiom_grounding('293f712f-dfac-4141-ba58-59356df17001', study_is_preparatory_not_substitutive, conventional).
narrative_ontology:cs_reference_frame('293f712f-dfac-4141-ba58-59356df17001', temple_era_halakha).
narrative_ontology:cs_drift_state('293f712f-dfac-4141-ba58-59356df17001', post_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('293f712f-dfac-4141-ba58-59356df17001', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__performance_only_reading, halakhic_authorities).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, entire_jewish_people).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, divine_command_immutability).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__performance_only_reading, halakhic_literalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret, transmit, and enforce the halakhic tradition, including the understanding that sacrifice obligations require physical performance. They do not directly 'collect' material gains, but their authority and the continuity of the tradition are maintained by this interpretation, which is a form of institutional benefit.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Bear the spiritual and psychological burden of an unfulfilled divine command for nearly two millennia. Their identity is deeply intertwined with the obligation, making exit from the framework unthinkable, despite the impossibility of performance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, entire_jewish_people, payer,
    powerless, generational, identity_locked, global).

% Advocate for interpretations where study or symbolic acts fulfill the mitzvah, or where the obligation is suspended. Their views are largely suppressed or marginalized within the dominant halakhic discourse that insists on physical performance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, proponents_of_alternative_readings, excluded,
    moderate, biographical, constrained, global).

% Analyze the structural implications of this halakhic interpretation, observing its persistence and impact on the community without being bound by its internal logic.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, analytical_observers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__performance_only_reading, halakhic_authorities).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__performance_only_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. In this reading, the constraint primarily defines an unfulfilled obligation rather than coordinating action.
% TRANSFER_FUNCTION: Transfers the spiritual and psychological burden of an unfulfilled divine command onto the Jewish people, while reinforcing the authority of the halakhic tradition that maintains this interpretation.
% ABSENT_VOICES: Proponents of alternative readings (e.g., 'study as exercise,' 'messianic suspension,' 'symbolic archive') are structurally excluded from the authoritative discourse of this reading; they would argue for a less burdensome or more adaptable understanding of the obligation.
% DISAPPEARANCE_RATIONALE: If this interpretation (that sacrifice requires physical performance and study is not a substitute) vanished, the religious framework of the Jewish people would fundamentally shift. The nature of prayer, messianic anticipation, and the understanding of divine command would be profoundly altered, alleviating a nearly 2000-year-old spiritual burden.
% FOUNDING_PROBLEM: To establish a direct, physical means of atonement and communion with God for the Jewish people, as commanded in the Torah, and to ensure its proper performance according to divine law.
% FOUNDING_PROBLEM_CORROBORATION: Traditional Jewish texts (Talmud, Midrash, codes of law) and the continuous practice of prayer for the Temple's restoration corroborate the founding problem and its ongoing status. This is attested by the vast majority of religious authorities and communities, not just those who benefit from the current interpretation.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The 'extractiveness' is high (0.95) because the constraint imposes a profound spiritual and psychological burden of unfulfilled divine command on the Jewish people, persisting for nearly two millennia. 'Suppression' is high (0.90) because this reading actively suppresses alternative interpretations that might offer a path to fulfillment through study or symbolic acts, thereby maintaining the burden. 'Theater ratio' is low (0.10) as there is no actual performance to be theatrical about; the constraint's force lies in the unfulfilled command. 'Accessibility collapse' is high (0.90) as the physical means of fulfillment are entirely absent, and interpretive alternatives are largely foreclosed by this reading. 'Resistance' is low (0.15) because, despite the burden, the community largely accepts the authority of this interpretation, channeling longing into prayer and messianic anticipation rather than active resistance to the interpretive framework itself. The metrics are stable over time, reflecting the enduring nature of the historical impossibility and the interpretive tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'entire_jewish_people,' the constraint is a profound, unalleviated burden. From the 'halakhic_authorities' perspective, it is the faithful transmission of divine will, maintaining the integrity of the tradition. The engine's classification as a Snare highlights how the maintenance of this interpretation, even without direct material gain, functions to extract a spiritual cost from the community by foreclosing alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'entire_jewish_people' are the primary victims (payers) as they bear the burden of the unfulfilled command. The 'halakhic_authorities' are identified as beneficiaries (agenda_setter) not because they collect material gains, but because their institutional authority and the continuity of the halakhic tradition are maintained and reinforced by this specific, literalist interpretation. 'Proponents_of_alternative_readings' are excluded, as their interpretations are suppressed by the dominant reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_without_material_beneficiary,
    'Can ''extraction'' be meaningfully high when there is no direct material beneficiary, but rather a structural burden maintained by an interpretive tradition?',
    'Refinement of the ''extraction'' metric to explicitly account for spiritual/psychological burdens and institutional authority maintenance as forms of ''gain'' for agenda-setters in commitment systems.',
    'If such burdens/gains are fully integrated, the Snare classification is robust. If ''extraction'' strictly requires material transfer, this constraint might be reclassified to a type that better captures structural impossibility without a direct extractor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_without_material_beneficiary, conceptual, 'Ambiguity of extraction in non-material contexts.').

omega_variable(
    suppression_of_interpretive_alternatives,
    'To what extent is the high ''suppression'' a result of active enforcement by halakhic authorities versus internalized acceptance by the community?',
    'Sociological and theological analysis of the mechanisms by which alternative interpretations are marginalized, including formal rulings, educational curricula, and communal social pressure. If internalized acceptance is dominant, the effective suppression is higher than structural measures suggest.',
    'If suppression is primarily internalized, the constraint''s persistence is more deeply embedded in identity, making it harder to dislodge even if formal enforcement weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_interpretive_alternatives, empirical, 'Structural vs. internalized suppression of alternative readings.').

omega_variable(
    live_vs_dead_obligation,
    'Is the founding problem of atonement and communion truly ''live'' if the divinely commanded means of addressing it are physically impossible?',
    'Theological and philosophical analysis of the nature of divine command and human capacity. If the command is deemed ''dead'' in its original form, the constraint''s persistence becomes a pure act of institutional inertia.',
    'If the founding problem is reclassified as ''dead,'' the constraint''s Snare classification would lean more heavily towards a Piton, as its function would be entirely atrophied, maintained only by inertia and theatrical adherence to a past mandate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(live_vs_dead_obligation, conceptual, 'Status of the founding problem given physical impossibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 70, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 500, 0.1).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sacr_tr_t2024, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 70, 0.95).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 500, 0.95).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1000, 0.95).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1500, 0.95).
narrative_ontology:measurement(sacr_be_t2024, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 70, 0.9).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 500, 0.9).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1000, 0.9).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1500, 0.9).
narrative_ontology:measurement(sacr_su_t2024, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_obligation_kernel,' which concerns the divine command for sacrifices. This reading emphasizes physical performance and rejects substitutes, leading to high extraction due to unfulfilled obligation. Other readings offer alternative interpretations of the obligation's status or fulfillment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
