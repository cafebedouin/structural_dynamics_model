% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Obligation: Study as Preparation for Messianic Restoration
 *   domain: religious_studies/jewish_law/textual_preservation
 *
 * SUMMARY:
 *   This constraint describes the traditional Jewish legal obligation to
 *   study the laws of Kodashim (sacrifices) even while the Temple is
 *   destroyed and the laws cannot be performed. The study is understood not
 *   as a mere academic exercise, but as an active, binding preparation for
 *   the messianic era when the Temple will be rebuilt and the sacrificial
 *   service restored. It ensures the technical knowledge remains intact and
 *   accessible across generations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.25).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.15).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.25).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Obligation: Study as Preparation for Messianic Restoration").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious_studies/jewish_law/textual_preservation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, '6d784f3c-6c84-4580-adb1-d281cf7e53ab').
narrative_ontology:cs_kernel_codification('6d784f3c-6c84-4580-adb1-d281cf7e53ab', fixed_text).
narrative_ontology:cs_authority_grounding('6d784f3c-6c84-4580-adb1-d281cf7e53ab', lineage).
narrative_ontology:cs_interpretation_layer_present('6d784f3c-6c84-4580-adb1-d281cf7e53ab').
narrative_ontology:cs_reading_relation('6d784f3c-6c84-4580-adb1-d281cf7e53ab', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('6d784f3c-6c84-4580-adb1-d281cf7e53ab', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_axiom('6d784f3c-6c84-4580-adb1-d281cf7e53ab', foundational, halakha_is_eternal_and_binding).
narrative_ontology:cs_axiom_status(halakha_is_eternal_and_binding, holdable).
narrative_ontology:cs_axiom_grounding('6d784f3c-6c84-4580-adb1-d281cf7e53ab', halakha_is_eternal_and_binding, deontological).
narrative_ontology:cs_axiom('6d784f3c-6c84-4580-adb1-d281cf7e53ab', foundational, temple_service_is_future_obligation).
narrative_ontology:cs_axiom_status(temple_service_is_future_obligation, holdable).
narrative_ontology:cs_axiom_grounding('6d784f3c-6c84-4580-adb1-d281cf7e53ab', temple_service_is_future_obligation, theological).
narrative_ontology:cs_reference_frame('6d784f3c-6c84-4580-adb1-d281cf7e53ab', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('6d784f3c-6c84-4580-adb1-d281cf7e53ab', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6d784f3c-6c84-4580-adb1-d281cf7e53ab', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_future_generation).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, rabbinic_scholars).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_generation_of_jews).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, divine_covenant_continuity).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, oral_torah_authority).
narrative_ontology:constraint_vindicates(kodashim_obligation__study_as_preparation, redemption_through_action).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They are the primary interpreters and transmitters of Kodashim law. They define the obligation of study as preparation, gain status and purpose from this role, and ensure the continuity of the tradition. Their identity is deeply fused with this commitment.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% They bear the burden of studying complex, unperformable laws, investing significant time and intellectual effort without immediate ritual fulfillment. They defer the cosmic repair to a future generation, accepting this as a necessary part of their covenantal obligation.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_generation_of_jews, payer,
    moderate, biographical, constrained, global).

% This future generation will inherit the meticulously preserved technical knowledge of sacrificial law, enabling them to perform the Temple service immediately upon its restoration. They are the ultimate beneficiaries of the current generation's preparatory study.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_future_generation, beneficiary,
    analytical, civilizational, analytical, universal).

% They study Kodashim as a historical and cultural artifact, analyzing its evolution and impact on Jewish thought and society, but do not participate in its religious observance or preparatory function.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, secular_historians, observer,
    analytical, generational, analytical, global).

% These adherents believe that studying Kodashim *itself* enacts the spiritual function of sacrifice, making physical restoration less central. While part of the broader tradition, their interpretation of the law's efficacy is distinct from the 'preparation' view.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, study_as_performance_adherents, excluded,
    organized, biographical, constrained, local).

% These adherents view Kodashim primarily as historical documentation of a defunct system, valuable for cultural identity but not as a binding legal obligation for future performance. Their focus is on preservation, not preparation for ritual enactment.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, study_as_archive_adherents, excluded,
    organized, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To preserve the complex, highly technical knowledge of sacrificial law across generations, ensuring its accurate transmission and readiness for performance upon the rebuilding of the Temple in the messianic era.
% TRANSFER_FUNCTION: Transfers spiritual merit, communal identity, and practical ritual knowledge from the current generation (through study and teaching) to the messianic future generation, who will ultimately perform the sacrifices.
% ABSENT_VOICES: Adherents of 'study_as_performance' would argue that the spiritual efficacy of sacrifice is already achieved through study, diminishing the 'preparation' aspect. Adherents of 'study_as_archive' would question the binding legal obligation and future performability, viewing it as purely historical. Both are structurally excluded from the 'preparation' framing's primary discourse.
% DISAPPEARANCE_RATIONALE: If the obligation to study Kodashim as preparation vanished, the intricate technical knowledge required for Temple service would likely be lost or corrupted over generations. This would fundamentally alter the possibility and nature of messianic restoration, requiring a complete re-establishment of ritual practice from scratch, thus profoundly rearranging the religious future.
% FOUNDING_PROBLEM: How to maintain the binding nature and practical knowledge of sacrificial law during the Temple's destruction and prolonged exile, ensuring its readiness for future rebuilding and performance in the messianic era.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream Orthodox Jewish legal authorities and a vast body of rabbinic literature spanning centuries consistently corroborate the 'live' status of this problem. Non-Orthodox Jewish movements might contest the 'live' status of the problem, but within the framework of traditional Jewish law, it remains a central concern.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the 'cost' of study is largely voluntary and framed as a spiritual investment rather than a coercive burden, with clear future benefits. Suppression is low (0.15) as adherence is primarily driven by religious commitment and communal norms, not active enforcement. Theater ratio is very low (0.05) because the study is considered genuinely functional for its stated purpose of preservation and preparation. Accessibility collapse is moderate (0.6) as alternatives to this specific form of study exist (e.g., secular historical study, or other forms of religious observance), but this is the prescribed path for maintaining the specific legal knowledge.
 *
 * PERSPECTIVAL GAP:
 *   While rabbinic scholars and the messianic future generation perceive this as a clear, beneficial coordination for future redemption, the current generation might experience the burden of studying unperformable laws as a form of extraction, albeit one accepted within a broader covenantal framework. The engine's computation will highlight this difference in perceived benefit/cost.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'current_generation_of_jews' are the primary payers, investing time and effort in study without immediate ritual fulfillment, bearing the cost of deferred cosmic repair. The 'messianic_future_generation' are the ultimate beneficiaries, inheriting the preserved knowledge. 'Rabbinic_scholars' are both agenda-setters and beneficiaries, gaining status and purpose from their role in maintaining and transmitting this tradition. The constraint subsidizes the future generation by ensuring the continuity of essential knowledge.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_unperformable_ambiguity,
    'How is the concept of a ''binding but unperformable'' law sustained without leading to either abandonment or symbolic reinterpretation that negates its future literal performance?',
    'Analysis of halakhic responsa and communal practice over time: if the legal discourse consistently maintains the literal future obligation without significant erosion or purely symbolic redefinition, the concept is robustly sustained.',
    'If the concept proves fragile, the constraint''s perceived legitimacy and the ''preparation'' function would weaken, potentially shifting its classification towards a more theatrical or archival type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_unperformable_ambiguity, conceptual, 'Conceptual robustness of ''binding but unperformable'' legal status.').

omega_variable(
    knowledge_preservation_efficacy,
    'To what extent does textual study alone, without practical performance, effectively preserve the nuanced technical knowledge required for complex ritual acts like animal sacrifice?',
    'Comparative historical analysis of other lost ritual traditions, or hypothetical simulation/reconstruction efforts: if significant gaps or ambiguities emerge that only performance could resolve, the efficacy of study alone is limited.',
    'If study is found to be insufficient for full preservation, the ''preparation'' function is partly theatrical, increasing the theater_ratio and potentially shifting the classification towards Piton or Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(knowledge_preservation_efficacy, empirical, 'Empirical efficacy of study for ritual knowledge preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t70, kodashim_obligation__study_as_preparation, theater_ratio, 70, 0.05).
narrative_ontology:measurement(koda_tr_t500, kodashim_obligation__study_as_preparation, theater_ratio, 500, 0.05).
narrative_ontology:measurement(koda_tr_t1000, kodashim_obligation__study_as_preparation, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(koda_tr_t1500, kodashim_obligation__study_as_preparation, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(koda_tr_t2024, kodashim_obligation__study_as_preparation, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(koda_be_t70, kodashim_obligation__study_as_preparation, base_extractiveness, 70, 0.2).
narrative_ontology:measurement(koda_be_t500, kodashim_obligation__study_as_preparation, base_extractiveness, 500, 0.22).
narrative_ontology:measurement(koda_be_t1000, kodashim_obligation__study_as_preparation, base_extractiveness, 1000, 0.23).
narrative_ontology:measurement(koda_be_t1500, kodashim_obligation__study_as_preparation, base_extractiveness, 1500, 0.24).
narrative_ontology:measurement(koda_be_t2024, kodashim_obligation__study_as_preparation, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t70, kodashim_obligation__study_as_preparation, suppression_requirement, 70, 0.15).
narrative_ontology:measurement(koda_su_t500, kodashim_obligation__study_as_preparation, suppression_requirement, 500, 0.15).
narrative_ontology:measurement(koda_su_t1000, kodashim_obligation__study_as_preparation, suppression_requirement, 1000, 0.15).
narrative_ontology:measurement(koda_su_t1500, kodashim_obligation__study_as_preparation, suppression_requirement, 1500, 0.15).
narrative_ontology:measurement(koda_su_t2024, kodashim_obligation__study_as_preparation, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, identity_coordination).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, jewish_diaspora_identity).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, rabbinic_authority).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'kodashim_obligation' kernel, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
