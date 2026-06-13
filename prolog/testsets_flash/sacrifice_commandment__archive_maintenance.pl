% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrifice Commandment: Archive Maintenance Reading
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'archive maintenance' reading of the
 *   sacrifice commandment, where the study of Temple laws is understood as
 *   preserving technical knowledge for a future messianic restoration, rather
 *   than fulfilling the commandment in the present. This reading emphasizes
 *   the practical, future-oriented utility of study, acknowledging that the
 *   full performance of sacrifices is currently impossible. The constraint's
 *   extractiveness is moderate, reflecting the investment in study for a
 *   deferred and somewhat uncertain future benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.45).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.2).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.45).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, rope).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrifice Commandment: Archive Maintenance Reading").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious_studies/halakhic_theory/commitment_system_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, '85fa9f3e-f7ac-420d-96f5-65e480526c88').
narrative_ontology:cs_kernel_codification('85fa9f3e-f7ac-420d-96f5-65e480526c88', fixed_text).
narrative_ontology:cs_authority_grounding('85fa9f3e-f7ac-420d-96f5-65e480526c88', lineage).
narrative_ontology:cs_interpretation_layer_present('85fa9f3e-f7ac-420d-96f5-65e480526c88').
narrative_ontology:cs_reading_relation('85fa9f3e-f7ac-420d-96f5-65e480526c88', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('85fa9f3e-f7ac-420d-96f5-65e480526c88', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_axiom('85fa9f3e-f7ac-420d-96f5-65e480526c88', foundational, study_as_preparation_not_fulfillment).
narrative_ontology:cs_axiom_status(study_as_preparation_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('85fa9f3e-f7ac-420d-96f5-65e480526c88', study_as_preparation_not_fulfillment, conventional).
narrative_ontology:cs_axiom('85fa9f3e-f7ac-420d-96f5-65e480526c88', foundational, future_restoration_is_halakhically_relevant).
narrative_ontology:cs_axiom_status(future_restoration_is_halakhically_relevant, holdable).
narrative_ontology:cs_axiom_grounding('85fa9f3e-f7ac-420d-96f5-65e480526c88', future_restoration_is_halakhically_relevant, theological).
narrative_ontology:cs_reference_frame('85fa9f3e-f7ac-420d-96f5-65e480526c88', post_temple_exile_halakha).
narrative_ontology:cs_drift_state('85fa9f3e-f7ac-420d-96f5-65e480526c88', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('85fa9f3e-f7ac-420d-96f5-65e480526c88', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_generations).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, halakhic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, lay_adherents).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, continuity_of_halakha).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, importance_of_oral_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dedicate their lives to the rigorous study and transmission of sacrifice laws, ensuring the continuity of the tradition. They are deeply invested in the intellectual and spiritual framework that justifies this study, even with deferred practical application.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, halakhic_scholars, agenda_setter,
    institutional, biographical, identity_locked, global).

% Will inherit the preserved knowledge and technical understanding necessary for the potential restoration of Temple service in a messianic era. They are the primary, albeit indirect, beneficiaries of the current generation's efforts.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_generations, beneficiary,
    powerless, generational, analytical, global).

% Support the institutions and scholars engaged in this study through donations and communal resources. They may not directly engage in the rigorous study but value its role in maintaining the tradition and preparing for the future.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, lay_adherents, payer,
    moderate, biographical, constrained, local).

% Advocate for immediate, practical steps towards Temple restoration, potentially viewing extensive theoretical study without direct action as insufficient or even delaying the messianic process. Their more immediate-action perspective is not fully accommodated by this reading.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, messianic_activists, excluded,
    organized, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intellectual and spiritual efforts of a dispersed community towards a shared, long-term goal of preserving knowledge for a future messianic restoration, ensuring continuity of a complex religious tradition.
% TRANSFER_FUNCTION: Transfers intellectual and spiritual labor from current halakhic scholars and financial support from lay adherents to the future generations, in the form of preserved, ready-to-implement knowledge of sacrifice laws.
% ABSENT_VOICES: Messianic activists and those who believe the commandment is entirely suspended would object, arguing that this reading either defers action too much or misinterprets the nature of the commandment. They are often marginalized in the discourse dominated by established halakhic authorities.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the dedicated study of sacrifice laws would likely cease or diminish significantly. This would lead to a loss of critical technical knowledge, making future Temple restoration (if it were to occur) far more difficult or impossible, fundamentally altering the long-term trajectory of the tradition.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the physical performance of sacrifices impossible, creating a crisis of how to fulfill a central divine commandment and preserve its intricate details for a future restoration.
% FOUNDING_PROBLEM_CORROBORATION: The problem of Temple destruction and the inability to perform sacrifices remains live, attested by nearly all Jewish religious authorities and historical texts. The need to preserve knowledge for a future restoration is a widely accepted theological premise, corroborated by centuries of rabbinic literature and communal practice, not just by the scholars directly benefiting from the study.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).

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
 *   The extractiveness (0.45) is moderate because the benefit of study is primarily for a future generation, making the present investment a form of deferred gratification with uncertain direct returns for the current generation of scholars. Suppression (0.2) is low as participation in this form of study is largely voluntary and driven by religious commitment rather than coercion. Theater ratio (0.1) is low because the study is genuinely aimed at preserving knowledge, not merely performing a ritual without substance. Accessibility collapse (0.15) is low as there are many alternative forms of religious observance and study. Resistance (0.05) is low as this reading is a widely accepted, if not universally dominant, approach to the commandment.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of current halakhic scholars, the constraint involves a significant investment of time and intellectual effort for a benefit that is largely deferred to a future messianic era. From the perspective of future generations, this constraint is a pure benefit, as it ensures the continuity of essential knowledge. The engine's classification will reflect this deferred benefit structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars are beneficiaries in that they gain intellectual and spiritual engagement, and contribute to a sacred tradition, but they also bear the 'cost' of dedicating time and effort to study whose primary utility is deferred. Future generations are clear beneficiaries, as they will inherit the preserved knowledge. There are no direct 'victims' in this reading, as the 'extraction' is primarily the deferred nature of the benefit rather than a coercive transfer.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the study of sacrifice law primarily an act of archive maintenance for future restoration, or does it carry present-day spiritual efficacy?',
    'Theological consensus shift or a definitive halakhic ruling on the nature of study in the absence of the Temple.',
    'If it carries present-day spiritual efficacy (as in the ''study_as_performance'' reading), the extractiveness of this constraint would be lower, as the ''cost'' of study yields immediate ''benefit''. If it is purely archive maintenance, the current moderate extractiveness is appropriate, reflecting the deferred and uncertain benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between archive maintenance and present spiritual performance readings of the sacrifice commandment.').

omega_variable(
    future_utility_certainty,
    'How certain is the future utility of currently preserved knowledge for Temple restoration?',
    'Theological developments regarding the nature of the messianic era and the Third Temple, or a re-evaluation of the practical applicability of ancient halakhic details.',
    'Higher certainty of future utility would lower the perceived extractiveness, as the investment in study is more reliably recouped. Lower certainty would increase extractiveness, as the present cost is for a more speculative future benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_utility_certainty, empirical, 'Uncertainty regarding the practical relevance of preserved knowledge for a future Temple.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t10, sacrifice_commandment__archive_maintenance, theater_ratio, 10, 0.1).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_commandment__archive_maintenance, theater_ratio, 20, 0.1).
narrative_ontology:measurement(sacr_tr_t30, sacrifice_commandment__archive_maintenance, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sacr_be_t10, sacrifice_commandment__archive_maintenance, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__archive_maintenance, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(sacr_be_t30, sacrifice_commandment__archive_maintenance, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sacr_su_t10, sacrifice_commandment__archive_maintenance, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(sacr_su_t20, sacrifice_commandment__archive_maintenance, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(sacr_su_t30, sacrifice_commandment__archive_maintenance, suppression_requirement, 30, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_commandment' kernel, focusing on the preservation of knowledge for future Temple restoration. It is linked to other readings that emphasize present performance or suspension of the commandment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
