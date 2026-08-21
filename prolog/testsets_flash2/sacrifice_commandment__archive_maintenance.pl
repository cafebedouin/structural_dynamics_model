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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrifice Commandment: Archive Maintenance Reading
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'archive maintenance' reading of the
 *   sacrifice commandment, where the primary obligation in the absence of the
 *   Temple is to study and preserve the technical knowledge of sacrificial
 *   laws for future restoration. It is a scaffold because its justification
 *   is transitional (until the Temple is rebuilt) and it carries a 'sunset
 *   clause' in its very nature (it ceases to be 'archive maintenance' once
 *   the Temple is restored and sacrifices resume). The extractiveness is
 *   moderate, as resources and intellectual effort are diverted from
 *   immediate needs to a deferred utility, but there are clear beneficiaries
 *   in future generations and the scholars themselves. Suppression is low, as
 *   participation is largely voluntary and driven by religious commitment
 *   rather than coercion.
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
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, scaffold).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrifice Commandment: Archive Maintenance Reading").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious_studies/halakhic_theory/commitment_system_analysis").

narrative_ontology:has_sunset_clause(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, 'cf4632c2-44e4-4d1b-9cb6-70040038b6b6').
narrative_ontology:cs_kernel_codification('cf4632c2-44e4-4d1b-9cb6-70040038b6b6', fixed_text).
narrative_ontology:cs_authority_grounding('cf4632c2-44e4-4d1b-9cb6-70040038b6b6', lineage).
narrative_ontology:cs_interpretation_layer_present('cf4632c2-44e4-4d1b-9cb6-70040038b6b6').
narrative_ontology:cs_reading_relation('cf4632c2-44e4-4d1b-9cb6-70040038b6b6', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('cf4632c2-44e4-4d1b-9cb6-70040038b6b6', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_axiom('cf4632c2-44e4-4d1b-9cb6-70040038b6b6', foundational, knowledge_preservation_is_deferred_fulfillment).
narrative_ontology:cs_axiom_status(knowledge_preservation_is_deferred_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('cf4632c2-44e4-4d1b-9cb6-70040038b6b6', knowledge_preservation_is_deferred_fulfillment, deontological).
narrative_ontology:cs_axiom('cf4632c2-44e4-4d1b-9cb6-70040038b6b6', foundational, messianic_era_is_future_not_present).
narrative_ontology:cs_axiom_status(messianic_era_is_future_not_present, holdable).
narrative_ontology:cs_axiom_grounding('cf4632c2-44e4-4d1b-9cb6-70040038b6b6', messianic_era_is_future_not_present, theological).
narrative_ontology:cs_reference_frame('cf4632c2-44e4-4d1b-9cb6-70040038b6b6', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('cf4632c2-44e4-4d1b-9cb6-70040038b6b6', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cf4632c2-44e4-4d1b-9cb6-70040038b6b6', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_generations).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, halakhic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, lay_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary agents who engage in the study and preservation of sacrificial laws. Their professional identity and religious commitment are tied to maintaining this knowledge, even if its direct application is deferred. They benefit from the intellectual pursuit and the status of being custodians of tradition.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% The ultimate beneficiaries of this constraint, as the preserved knowledge is intended for their use in a restored Temple. They bear no present cost but rely on the current generation's commitment to this long-term project.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_generations, beneficiary,
    powerless, civilizational, analytical, universal).

% Support the institutions and scholars dedicated to this study, often through donations or communal resources. They may not directly engage with the complex halakhic details but contribute to the infrastructure that maintains the archive. Their 'payment' is primarily in deferred gratification and resource allocation away from immediate needs.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, lay_adherents, payer,
    moderate, biographical, constrained, local).

% Advocate for immediate, active preparation for Temple restoration, potentially including physical reconstruction or ritual practice. This reading's emphasis on 'archive maintenance' rather than 'present worship' sidelines their more urgent, action-oriented approach, making them excluded from the dominant discourse of this constraint.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, messianic_activists, excluded,
    organized, immediate, identity_locked, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the long-term preservation of complex, technical religious knowledge across generations, ensuring that the practical details of Temple sacrifice are not lost, for a future time when they can be implemented.
% TRANSFER_FUNCTION: Transfers intellectual effort and communal resources from the present generation of scholars and adherents to a future generation, in the form of preserved halakhic knowledge.
% ABSENT_VOICES: Messianic activists and those who believe the commandment is entirely suspended without the Temple would object. Messianic activists would argue for immediate, tangible action rather than deferred study, while those who believe in suspension would question the present utility and resource allocation for a non-operative commandment.
% DISAPPEARANCE_RATIONALE: If the commitment to archive maintenance vanished, the highly specialized knowledge of Temple sacrifice would likely degrade and be lost over generations, making future restoration significantly more difficult or impossible. The intellectual and communal structures built around this study would dissolve.
% FOUNDING_PROBLEM: The destruction of the Temple and the cessation of sacrifices created a crisis of continuity for a central divine commandment, raising the problem of how to fulfill or prepare for its eventual re-implementation without the physical means.
% FOUNDING_PROBLEM_CORROBORATION: Halakhic authorities across various traditions corroborate the ongoing nature of the problem, emphasizing the importance of study for future application. Historical texts and rabbinic responsa from outside the immediate scholarly beneficiaries consistently affirm the need to preserve this knowledge for a future messianic era.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.45, 'gemini-2.5-flash', 'none', direct).

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
 *   The claimed type is 'scaffold' due to its transitional nature and implicit sunset clause (restoration of the Temple). Extractiveness is 0.45 because while the study is a form of 'payment' in time and resources, it's for a future, uncertain benefit, and it diverts from other potential religious or communal activities. Suppression is low (0.20) as this is primarily a voluntary religious commitment, not enforced by external coercion. Theater ratio is low (0.10) because the study is genuinely functional for its stated purpose of knowledge preservation, not merely performative. Accessibility collapse is moderate (0.30) as alternative forms of religious observance exist, but for those committed to this specific commandment, the 'archive maintenance' path is the primary one. Resistance is low (0.15) because the practice is widely accepted within its community, though some may advocate for different approaches.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of halakhic scholars, this is a vital, functional scaffold ensuring continuity of tradition. From the perspective of messianic activists, it might be seen as a form of 'delay' or even 'snare' if it diverts resources from more immediate, active messianic preparation. The engine's classification will reflect the structural reality of deferred benefit and resource allocation.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars act as agenda-setters and beneficiaries, gaining intellectual and spiritual fulfillment, as well as status, from their role in preserving this knowledge. Future generations are the ultimate beneficiaries, receiving the preserved knowledge. Lay adherents are payers, contributing resources and effort to support the scholarly infrastructure. Messianic activists are excluded, as their focus on immediate action is not aligned with this reading's deferred utility.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_utility_certainty,
    'What is the actual probability and timeline of Temple restoration, and thus the utility of this preserved knowledge?',
    'Empirical observation of geopolitical and religious developments, or a shift in theological consensus regarding messianic timing.',
    'If the probability is very low or the timeline extremely distant, the ''scaffold'' justification weakens, and the constraint might reclassify towards ''piton'' (inertial maintenance of a non-functional archive) or ''snare'' (if resources are extracted for a perpetually deferred goal).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_utility_certainty, empirical, 'Uncertainty regarding the future utility of the preserved knowledge.').

omega_variable(
    resource_diversion_impact,
    'To what extent does the allocation of intellectual and communal resources to archive maintenance divert from other pressing religious or social needs?',
    'Sociological and economic analysis of resource allocation within the community, comparing investment in this study versus other communal projects (e.g., poverty relief, education for immediate needs).',
    'If diversion is substantial and demonstrably detrimental to other needs, the extractiveness of this constraint would be re-evaluated upward, potentially shifting it towards a ''tangled_rope'' or ''snare'' if the ''beneficiaries'' (future generations) are too abstract to justify present costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_diversion_impact, empirical, 'The opportunity cost of resource allocation to deferred utility.').

omega_variable(
    reading_framing_legitimacy,
    'Is the ''archive maintenance'' framing a legitimate interpretation of the sacrifice commandment, or a rationalization for inaction/deferred fulfillment?',
    'Theological debate and consensus within halakhic discourse, or a shift in the perceived urgency of messianic redemption.',
    'If deemed a rationalization, the constraint''s legitimacy would erode, and its ''scaffold'' nature would be questioned, potentially reclassifying it as a ''piton'' (if maintained by inertia) or ''snare'' (if it serves to suppress more active forms of religious observance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_legitimacy, conceptual, 'Conceptual legitimacy of the ''archive maintenance'' reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_commandment__archive_maintenance, theater_ratio, 20, 0.09).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_commandment__archive_maintenance, theater_ratio, 40, 0.1).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_commandment__archive_maintenance, theater_ratio, 60, 0.1).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_commandment__archive_maintenance, theater_ratio, 80, 0.1).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_commandment__archive_maintenance, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__archive_maintenance, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(sacr_be_t40, sacrifice_commandment__archive_maintenance, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(sacr_be_t60, sacrifice_commandment__archive_maintenance, base_extractiveness, 60, 0.44).
narrative_ontology:measurement(sacr_be_t80, sacrifice_commandment__archive_maintenance, base_extractiveness, 80, 0.45).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__archive_maintenance, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sacr_su_t20, sacrifice_commandment__archive_maintenance, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(sacr_su_t40, sacrifice_commandment__archive_maintenance, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(sacr_su_t60, sacrifice_commandment__archive_maintenance, suppression_requirement, 60, 0.2).
narrative_ontology:measurement(sacr_su_t80, sacrifice_commandment__archive_maintenance, suppression_requirement, 80, 0.2).
narrative_ontology:measurement(sacr_su_t100, sacrifice_commandment__archive_maintenance, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
