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
 *   human_readable: Sacrifice Obligation: Performance-Only Reading
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint, the 'performance_only_reading' of the
 *   'sacrifice_obligation_kernel', asserts that the divine command for
 *   sacrifices requires physical performance, and that study, while
 *   preparatory, does not fulfill the mitzvah. The destruction of the Second
 *   Temple rendered this obligation physically impossible to fulfill,
 *   creating a state of perpetual unfulfillment for the Jewish people. The
 *   constraint is claimed as a Mountain due to its fixed, unchangeable nature
 *   as a divine command, and the absence of any party actively collecting
 *   from its operation. However, the metrics reflect a high degree of
 *   'extraction' in the form of an unfulfillable burden, and high
 *   'suppression' due to the physical impossibility, which is a key
 *   divergence the engine will analyze.
 *
 * KEY AGENTS:
 *   - the_jewish_people: Primary target (powerless/trapped) — bears the burden of unfulfilled obligation
 *   - halakhic_authorities: Agenda setter (institutional/constrained) — interprets and transmits the obligation
 *   - analytical_observers: Analytical observer (analytical/analytical) — examines structural implications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.9).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.9).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation: Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, '70e0fbb4-470c-44b6-b677-5926b6619b27').
narrative_ontology:cs_kernel_codification('70e0fbb4-470c-44b6-b677-5926b6619b27', fixed_text).
narrative_ontology:cs_authority_grounding('70e0fbb4-470c-44b6-b677-5926b6619b27', lineage).
narrative_ontology:cs_interpretation_layer_present('70e0fbb4-470c-44b6-b677-5926b6619b27').
narrative_ontology:cs_reading_relation('70e0fbb4-470c-44b6-b677-5926b6619b27', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('70e0fbb4-470c-44b6-b677-5926b6619b27', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('70e0fbb4-470c-44b6-b677-5926b6619b27', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('70e0fbb4-470c-44b6-b677-5926b6619b27', foundational, physical_performance_is_sine_qua_non).
narrative_ontology:cs_axiom_status(physical_performance_is_sine_qua_non, holdable).
narrative_ontology:cs_axiom_grounding('70e0fbb4-470c-44b6-b677-5926b6619b27', physical_performance_is_sine_qua_non, deontological).
narrative_ontology:cs_axiom('70e0fbb4-470c-44b6-b677-5926b6619b27', secondary, study_is_preparatory_not_substitutive).
narrative_ontology:cs_axiom_status(study_is_preparatory_not_substitutive, holdable).
narrative_ontology:cs_axiom_grounding('70e0fbb4-470c-44b6-b677-5926b6619b27', study_is_preparatory_not_substitutive, conventional).
narrative_ontology:cs_reference_frame('70e0fbb4-470c-44b6-b677-5926b6619b27', temple_era_halakha).
narrative_ontology:cs_drift_state('70e0fbb4-470c-44b6-b677-5926b6619b27', post_second_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('70e0fbb4-470c-44b6-b677-5926b6619b27', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, the_jewish_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commanded by divine law to perform sacrifices, but rendered unable to do so by the destruction of the Temple. They bear the burden of unfulfilled obligation and the longing for its restoration, without any means to fulfill it in the present.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, the_jewish_people, payer,
    powerless, generational, trapped, global).

% Responsible for interpreting, transmitting, and maintaining the understanding of the sacrifice obligation. They affirm the requirement for physical performance and the current impossibility, guiding the community in its absence but not benefiting from the unfulfillment itself.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Examine the structural implications of a divine command that is unfulfillable for millennia, analyzing its impact on religious identity, practice, and theological development from an external, non-participatory perspective.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the continuity of a core divine command and the identity of the Jewish people as bound by it, even in its unfulfillable state, by affirming the necessity of physical performance.
% TRANSFER_FUNCTION: Transfers the burden of unfulfilled divine command and the associated spiritual longing onto the Jewish people, creating a perpetual state of anticipation for messianic restoration.
% ABSENT_VOICES: Those who might argue for a complete abrogation of the mitzvah or a reinterpretation as purely symbolic, but whose views are outside the traditional halakhic discourse that insists on physical performance.
% DISAPPEARANCE_RATIONALE: If the obligation for physical sacrifice vanished overnight, the entire religious framework surrounding the Temple, atonement, and messianic anticipation would be fundamentally altered, requiring a complete re-evaluation of divine command and human obligation, profoundly reshaping Jewish identity and practice.
% FOUNDING_PROBLEM: To establish a means of atonement, communion with God, and maintenance of the covenant through prescribed ritual performance, ensuring the spiritual well-being of the community.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts (Torah, Talmud), continuous religious practice, and theological discourse across millennia attest to the founding problem and its ongoing relevance, even in the absence of the Temple. This is corroborated by the persistent longing for the Temple's rebuilding and the restoration of sacrifices, as expressed in daily prayers and liturgy.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.9) reflects the profound and enduring burden of an unfulfillable divine command, which has persisted for nearly two millennia. This is not extraction by an agent, but a structural cost of unfulfillment. Suppression (0.9) is high due to the physical impossibility of performing sacrifices without the Temple, effectively collapsing all alternatives for direct fulfillment. Theater ratio (0.1) is low because the obligation is taken seriously and is not maintained for performative reasons; the longing for its fulfillment is genuine. Resistance (0.1) is low because the obligation is generally accepted as divine will, despite its current impossibility. Accessibility collapse (0.95) is near total for the act of performance itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish people, the constraint represents a profound and enduring burden of unfulfilled divine will. From the perspective of halakhic authorities, it is a fixed divine truth to be upheld and transmitted. An analytical observer might classify it as a Mountain due to its fixed nature and lack of direct beneficiaries, but note the high 'extractiveness' as a structural cost rather than a transfer.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people are the primary targets, bearing the cost of unfulfilled obligation. Halakhic authorities act as agenda setters, interpreting and transmitting the command, but do not benefit from the unfulfillment. There are no direct beneficiaries of this 'extraction' in the sense of a party collecting rents; the 'extraction' is the burden itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to perform sacrifices) has not atrophied; it remains a live divine command. However, the means of fulfilling that mandate have been removed. The classification as a Mountain with high extractiveness prevents mislabeling it as a Snare (which would imply an active extractor) or a Piton (which would imply atrophy or theatricality). The 'extraction' is the cost of an unfulfillable, yet persistent, divine command.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nature_of_extraction_without_beneficiary,
    'Can ''extraction'' be meaningfully applied to a burden that has no direct beneficiary or recipient agent?',
    'Conceptual clarification of ''extraction'' within the framework: if extraction is defined as any cost imposed by a constraint, regardless of recipient, then the current application is valid. If it requires a recipient, then a new metric for ''unfulfillable burden'' may be needed.',
    'If extraction requires a recipient, the ''extractiveness'' metric for this constraint would need re-evaluation, potentially leading to a different classification (e.g., a Mountain with a unique ''burden'' metric).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nature_of_extraction_without_beneficiary, conceptual, 'Clarifying the definition of ''extraction'' in the absence of a direct beneficiary.').

omega_variable(
    divine_command_as_natural_law,
    'Is a divine command, within a religious framework, equivalent to ''emerges_naturally'' for the purpose of Mountain classification?',
    'Analysis of the epistemic grounding of religious law within the framework: if divine law is treated as an irreducible, foundational truth within its domain, then ''emerges_naturally'' is appropriate. If it''s seen as a human construct, then it is not.',
    'If not considered ''natural'', the Mountain classification would be challenged, potentially shifting to a constructed type (e.g., a Snare or Tangled Rope, if an active enforcer/beneficiary could be identified, or a Piton if atrophied).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_as_natural_law, conceptual, 'Assessing the ''naturalness'' of divine law for classification.').

omega_variable(
    future_fulfillment_impact,
    'How would the classification of this constraint change if the Temple were rebuilt and sacrifices restored (messianic era)?',
    'Counterfactual analysis: if the constraint became fulfillable, its ''extractiveness'' (as unfulfilled burden) would drop to near zero, and ''suppression'' would decrease, likely reclassifying it as a Rope or Mountain of active practice.',
    'A shift from unfulfillable burden to active practice would fundamentally alter the constraint''s metrics and classification, demonstrating its context-dependency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(future_fulfillment_impact, empirical, 'Impact of future fulfillment on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 70, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 70, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 500, 0.1).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sacr_tr_t2020, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 70, 0.9).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 500, 0.9).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1000, 0.9).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1500, 0.9).
narrative_ontology:measurement(sacr_be_t2020, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 2020, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 70, 0.9).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 500, 0.9).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1000, 0.9).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1500, 0.9).
narrative_ontology:measurement(sacr_su_t2020, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_obligation_kernel', focusing on the requirement for physical performance. It is structurally distinct from other readings that interpret the obligation differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
