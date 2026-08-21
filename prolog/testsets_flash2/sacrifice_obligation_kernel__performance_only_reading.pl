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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrifice Obligation: Performance Only Reading
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This constraint represents the 'performance only' reading of the
 *   sacrifice obligation kernel within Jewish law. It asserts that the divine
 *   command for sacrifices requires physical performance, and that study of
 *   the laws, while meritorious, does not fulfill the mitzvah itself. This
 *   creates a structural impossibility for the Jewish people since the
 *   destruction of the Temple, leading to a persistent state of unfulfilled
 *   divine command. The constraint is classified as a Mountain due to its
 *   perceived immutability as divine law, despite the high 'extraction' of
 *   capacity and fulfillment from the Jewish people.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.95).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 1.0).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation: Performance Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious_law/halakhic_authority").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, '52ac0226-d916-4fc9-aeb0-7d9ccc43afd8').
narrative_ontology:cs_kernel_codification('52ac0226-d916-4fc9-aeb0-7d9ccc43afd8', fixed_text).
narrative_ontology:cs_authority_grounding('52ac0226-d916-4fc9-aeb0-7d9ccc43afd8', lineage).
narrative_ontology:cs_interpretation_layer_present('52ac0226-d916-4fc9-aeb0-7d9ccc43afd8').
narrative_ontology:cs_reading_relation('52ac0226-d916-4fc9-aeb0-7d9ccc43afd8', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('52ac0226-d916-4fc9-aeb0-7d9ccc43afd8', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('52ac0226-d916-4fc9-aeb0-7d9ccc43afd8', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('52ac0226-d916-4fc9-aeb0-7d9ccc43afd8', foundational, mitzvah_requires_physical_performance).
narrative_ontology:cs_axiom_status(mitzvah_requires_physical_performance, holdable).
narrative_ontology:cs_axiom_grounding('52ac0226-d916-4fc9-aeb0-7d9ccc43afd8', mitzvah_requires_physical_performance, deontological).
narrative_ontology:cs_axiom('52ac0226-d916-4fc9-aeb0-7d9ccc43afd8', foundational, study_is_preparatory_not_substitutive).
narrative_ontology:cs_axiom_status(study_is_preparatory_not_substitutive, holdable).
narrative_ontology:cs_axiom_grounding('52ac0226-d916-4fc9-aeb0-7d9ccc43afd8', study_is_preparatory_not_substitutive, deontological).
narrative_ontology:cs_reference_frame('52ac0226-d916-4fc9-aeb0-7d9ccc43afd8', torah_literal_command).
narrative_ontology:cs_drift_state('52ac0226-d916-4fc9-aeb0-7d9ccc43afd8', post_temple_destruction_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('52ac0226-d916-4fc9-aeb0-7d9ccc43afd8', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, jewish_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commanded by divine law to perform sacrifices, but physically unable to do so due to the destruction of the Temple. Bears the burden of unfulfilled divine command, with no direct agent extracting, but the structural impossibility creates a profound, persistent 'extraction' of capacity and fulfillment. Identity-locked by religious commitment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, jewish_people, payer,
    powerless, generational, identity_locked, global).

% Interpret and transmit the divine command, affirming the necessity of physical performance. They maintain the legal framework and the expectation of future fulfillment, but cannot alter the core requirement. Their authority is grounded in lineage and tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Analyze the structural implications of a divine command that is currently impossible to fulfill. They observe the persistence of the obligation and its impact on the commanded community, without being subject to the command themselves.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the understanding of divine command, ensuring that the community maintains the literal interpretation of sacrifice as a physical act, preserving the integrity of the mitzvah's original form.
% TRANSFER_FUNCTION: Transfers the burden of unfulfilled divine obligation onto the Jewish people, creating a persistent state of spiritual and ritual deficit due to the physical impossibility of performance. No direct agent receives this 'transfer' in an extractive sense; it is a structural gap.
% ABSENT_VOICES: No voices are 'absent' in the sense of being excluded from a conversation, as this is a theological interpretation. However, alternative readings (e.g., study as fulfillment) represent different approaches to the same core problem, which this reading implicitly rejects as insufficient.
% DISAPPEARANCE_RATIONALE: If the obligation for physical sacrifice performance vanished, it would fundamentally alter a core aspect of Jewish religious law and identity, removing a central, unfulfilled divine command that has shaped religious life and aspiration for nearly two millennia. The entire halakhic system and messianic expectation would rearrange.
% FOUNDING_PROBLEM: The divine command for animal sacrifices as a central act of worship and atonement, as detailed in the Torah.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by the continuous study of sacrificial laws and prayers for the restoration of the Temple across nearly all traditional Jewish communities, corroborating the ongoing nature of the unfulfilled command from outside any single benefiting party (as there are no direct beneficiaries of this structural impossibility).
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_kernel__performance_only_reading),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is extremely high (0.95) because the entire Jewish people are commanded to perform an act that has been physically impossible for nearly 2,000 years, creating a profound and persistent spiritual deficit. Suppression is total (1.0) because the physical destruction of the Temple is an absolute barrier to performance, and the divine nature of the command leaves no alternative means of fulfillment within this reading. Theater ratio is low (0.05) as there is no performative maintenance of the sacrifice itself, only the study and prayer for its restoration, which are distinct activities. Accessibility collapse is near total (0.99) as no alternative means of fulfilling the mitzvah are recognized. Resistance is low (0.05) because the impossibility is accepted as divine decree, not a human-imposed burden to be resisted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish people, this constraint represents a profound and enduring challenge, a core religious obligation that cannot be met. From the perspective of Halakhic authorities, it is a fixed divine truth to be upheld and transmitted, maintaining the integrity of the law. The analytical observer sees the structural tension and its historical persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people are the primary targets/victims, bearing the burden of an unfulfilled divine command. Halakhic authorities are agenda-setters, interpreting and transmitting the command, but not directly benefiting from the 'extraction' in a material sense. There are no direct beneficiaries in this reading, as the 'extraction' is a structural consequence of divine command and historical circumstance, not an agent-driven process.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Mountain because its persistence is rooted in divine command and historical fact (Temple destruction), not in active enforcement or benefit to an agent. The 'extraction' is a structural impossibility, not a rent collected by a party. Mandatrophy is not applicable in the usual sense, as the 'mandate' (divine command) is considered eternal, and its 'function' (physical sacrifice) is suspended by external, unchangeable circumstances, not by internal decay or obsolescence. The classification prevents mislabeling a theological/historical structural impossibility as a human-imposed snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_impossibility,
    'Is the impossibility of sacrifice performance a ''natural law'' (an irreducible physical/historical limit) or a ''constructed impossibility'' (a theological interpretation that could, in principle, be re-interpreted)?',
    'Theological re-evaluation of the nature of divine command and the possibility of non-physical fulfillment, or a shift in the accepted halakhic interpretive tradition.',
    'If re-interpreted as a constructed impossibility, the ''mountain'' classification would shift towards a ''tangled_rope'' or ''snare'' if an agent were found to benefit from maintaining the impossibility, or a ''rope'' if a new coordination mechanism for fulfillment were established.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_impossibility, conceptual, 'Ambiguity regarding the fundamental nature of the constraint''s persistence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (physical destruction of the Temple) or internalized (theological acceptance of the impossibility)?',
    'Analysis of historical and contemporary responses to the Temple''s destruction: if significant movements arose to find alternative physical means of sacrifice, it would suggest less internalized suppression. If the impossibility is universally accepted as divine will, it points to internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the community carries the suppression with them. However, in this case, both structural and internalized aspects reinforce each other, leading to maximal suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the impossibility of sacrifice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 0, 1954).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t390, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 390, 0.05).
narrative_ontology:measurement(sacr_tr_t780, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 780, 0.05).
narrative_ontology:measurement(sacr_tr_t1170, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1170, 0.05).
narrative_ontology:measurement(sacr_tr_t1560, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1560, 0.05).
narrative_ontology:measurement(sacr_tr_t1954, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1954, 0.05).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(sacr_be_t390, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 390, 0.95).
narrative_ontology:measurement(sacr_be_t780, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 780, 0.95).
narrative_ontology:measurement(sacr_be_t1170, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1170, 0.95).
narrative_ontology:measurement(sacr_be_t1560, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1560, 0.95).
narrative_ontology:measurement(sacr_be_t1954, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1954, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0, 1.0).
narrative_ontology:measurement(sacr_su_t390, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 390, 1.0).
narrative_ontology:measurement(sacr_su_t780, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 780, 1.0).
narrative_ontology:measurement(sacr_su_t1170, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1170, 1.0).
narrative_ontology:measurement(sacr_su_t1560, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1560, 1.0).
narrative_ontology:measurement(sacr_su_t1954, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1954, 1.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__performance_only_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'sacrifice_obligation_kernel'. Each reading offers a distinct interpretation of the divine command for sacrifices, leading to different structural implications and classifications. This 'performance_only_reading' emphasizes the literal, physical requirement, leading to a high-extraction Mountain due to the historical impossibility of fulfillment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
