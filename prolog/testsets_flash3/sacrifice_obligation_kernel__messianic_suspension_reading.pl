% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Sacrifice Obligation: Messianic Suspension Reading
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the divine obligation
 *   for sacrifices within Jewish law, asserting that the obligation is
 *   divinely suspended (not transformed or fulfilled by other means) until
 *   the messianic era and the rebuilding of the Temple. During this
 *   suspension, the study of sacrificial laws is understood as maintaining
 *   'operational readiness' for future performance, rather than as a
 *   substitute for the actual mitzvah. This reading emphasizes the
 *   unchangeable nature of the divine command and the temporary, divinely
 *   ordained abeyance of its performance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.05).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.02).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Sacrifice Obligation: Messianic Suspension Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, 'f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c').
narrative_ontology:cs_kernel_codification('f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c', fixed_text).
narrative_ontology:cs_authority_grounding('f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c', lineage).
narrative_ontology:cs_interpretation_layer_present('f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c').
narrative_ontology:cs_reading_relation('f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c', foundational, divine_command_immutable).
narrative_ontology:cs_axiom_status(divine_command_immutable, holdable).
narrative_ontology:cs_axiom_grounding('f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c', divine_command_immutable, deontological).
narrative_ontology:cs_axiom('f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c', foundational, suspension_is_abeyance_not_transformation).
narrative_ontology:cs_axiom_status(suspension_is_abeyance_not_transformation, holdable).
narrative_ontology:cs_axiom_grounding('f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c', suspension_is_abeyance_not_transformation, theological).
narrative_ontology:cs_reference_frame('f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c', divine_command_and_messianic_promise).
narrative_ontology:cs_drift_state('f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c', contemporary_diaspora_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f0c51c6c-6e9a-49ac-a7dc-e01f21e5135c', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_of_israel).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, kohanim_priesthood).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, individual_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives the preserved knowledge and operational readiness for the eventual restoration of the Temple and sacrifices. Benefits from the continuity of tradition and the potential for future fulfillment of divine commandments.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_of_israel, beneficiary,
    powerless, generational, identity_locked, global).

% Maintains their ancestral role and specialized knowledge, ensuring they are prepared to resume their duties when the Temple is rebuilt. Their identity is tied to this future function.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, kohanim_priesthood, beneficiary,
    moderate, generational, identity_locked, global).

% Interpret and transmit the divine law, affirming the suspension and the instrumental role of study. They guide the community in maintaining readiness without violating the terms of suspension.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Bear the 'cost' of maintaining readiness through study and adherence to related practices, without the immediate spiritual benefit of actual sacrifice. Their commitment is to a future, not present, fulfillment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, individual_adherents, payer,
    powerless, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective understanding and practice of a divinely ordained obligation during a period of suspension, ensuring continuity and readiness for its eventual restoration.
% TRANSFER_FUNCTION: Transfers the responsibility for active performance into a state of readiness and study, from the present generation to future generations, preserving knowledge and identity.
% ABSENT_VOICES: Those who believe study *does* fulfill the obligation (study_as_exercise_reading) or that only physical performance counts (performance_only_reading) are present in the broader discourse but are not the primary voice in this specific reading's framework, which emphasizes suspension and future readiness.
% DISAPPEARANCE_RATIONALE: If this understanding vanished, the community's relationship to a core divine commandment would be fundamentally altered. It would either lead to attempts at premature performance, a sense of unfulfilled obligation, or a complete abandonment of the tradition, profoundly reorganizing religious life and identity.
% FOUNDING_PROBLEM: The destruction of the Temple and the inability to perform divinely commanded sacrifices created a crisis of religious practice and continuity.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live as long as the Temple is not rebuilt and sacrifices cannot be performed. This is attested by the ongoing prayers for restoration and the continued study of sacrificial laws across diverse Jewish communities, not just those who directly benefit from this specific interpretation.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_kernel__messianic_suspension_reading),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low because the obligation is understood as suspended, not violated or actively extracting from adherents. There are no 'victims' in the sense of being harmed by the constraint; rather, adherents are guided to a state of patient readiness. Suppression is minimal, as this is a theological interpretation, not a coercive enforcement. Theater ratio is low because the study is genuinely instrumental to future performance, not merely performative. Accessibility collapse is high because the physical performance of sacrifices is genuinely impossible under current conditions. Resistance is negligible because this reading is widely accepted as a pragmatic and faithful response to the historical reality of the Temple's destruction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individual adherents, the constraint is a 'mountain' of divine decree, unchangeable and requiring patient adherence. From the perspective of future generations, it is a 'rope' that preserves vital knowledge and continuity. Halakhic authorities, as agenda-setters, also view it as a 'mountain' of divine law, which they interpret and transmit.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and the Kohanim priesthood are beneficiaries, as this reading ensures the preservation of their heritage and future role. Individual adherents are 'payers' in the sense that they bear the responsibility of study and readiness without immediate fulfillment, but this is a chosen commitment, not an extraction. Halakhic authorities are agenda-setters, interpreting and guiding the community. The directionality for all is low, reflecting the non-extractive nature of a suspended divine command.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nature_of_suspension,
    'Is the divine suspension of sacrifice obligation a temporary abeyance or a fundamental redefinition of the mitzvah''s performance conditions?',
    'Theological consensus or a new divine revelation clarifying the nature of the suspension.',
    'If redefinition, other readings (e.g., study_as_exercise_reading) might gain halakhic validity, potentially shifting this constraint from a Mountain to a Rope or even a Snare if the redefinition is seen as extractive. If temporary abeyance, this reading''s Mountain classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nature_of_suspension, conceptual, 'Ambiguity regarding the theological nature of the suspension of sacrifice.').

omega_variable(
    study_as_substitute_or_readiness,
    'Does the study of sacrificial laws truly maintain ''operational readiness'' or does it function as a de facto substitute for the mitzvah, even if not explicitly acknowledged?',
    'Empirical observation of community practice and theological discourse: if study increasingly replaces the *desire* for physical performance, it functions as a substitute. If the desire for physical performance remains strong, it is readiness.',
    'If study is a de facto substitute, the ''study_as_exercise_reading'' gains strength, potentially reclassifying this constraint as a Rope (coordination around study) or even a Tangled Rope if some benefit disproportionately from the ''substitute'' status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_substitute_or_readiness, empirical, 'The functional role of study during the suspension period.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 500, 0.1).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2000, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__messianic_suspension_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, symbolic_archive_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
