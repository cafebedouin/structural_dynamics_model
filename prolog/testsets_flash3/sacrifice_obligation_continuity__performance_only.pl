% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation: Performance Only Reading
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint represents a reading of the
 *   'sacrifice_obligation_continuity' kernel, specifically the
 *   'performance_only' reading. It asserts that the obligation to perform
 *   sacrifices remains strictly tied to physical ritual, which is currently
 *   impossible due to the absence of the Temple. Study of the laws is seen as
 *   preparation for a future restoration, not as a substitute for actual
 *   performance. This places the current generation of adherents in a
 *   position of unfulfillable obligation, leading to high extractiveness
 *   (spiritual deficit, guilt) and high suppression (no viable alternative
 *   for fulfillment).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.85).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.9).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, snare).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation: Performance Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '3c73e34f-bff4-431c-ad84-3d9cbdd2c27e').
narrative_ontology:cs_kernel_codification('3c73e34f-bff4-431c-ad84-3d9cbdd2c27e', fixed_text).
narrative_ontology:cs_authority_grounding('3c73e34f-bff4-431c-ad84-3d9cbdd2c27e', lineage).
narrative_ontology:cs_interpretation_layer_present('3c73e34f-bff4-431c-ad84-3d9cbdd2c27e').
narrative_ontology:cs_reading_relation('3c73e34f-bff4-431c-ad84-3d9cbdd2c27e', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('3c73e34f-bff4-431c-ad84-3d9cbdd2c27e', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('3c73e34f-bff4-431c-ad84-3d9cbdd2c27e', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('3c73e34f-bff4-431c-ad84-3d9cbdd2c27e', foundational, ritual_requires_physical_performance).
narrative_ontology:cs_axiom_status(ritual_requires_physical_performance, holdable).
narrative_ontology:cs_axiom_grounding('3c73e34f-bff4-431c-ad84-3d9cbdd2c27e', ritual_requires_physical_performance, deontological).
narrative_ontology:cs_axiom('3c73e34f-bff4-431c-ad84-3d9cbdd2c27e', foundational, study_is_preparation_not_fulfillment).
narrative_ontology:cs_axiom_status(study_is_preparation_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('3c73e34f-bff4-431c-ad84-3d9cbdd2c27e', study_is_preparation_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('3c73e34f-bff4-431c-ad84-3d9cbdd2c27e', pre_destruction_temple_ritual).
narrative_ontology:cs_drift_state('3c73e34f-bff4-431c-ad84-3d9cbdd2c27e', post_destruction_diaspora, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('3c73e34f-bff4-431c-ad84-3d9cbdd2c27e', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, current_generation_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adherents are bound by an unfulfillable obligation, leading to a state of perpetual guilt or spiritual deficit. Their identity is deeply tied to the tradition, making exit unthinkable, but the core ritual is impossible to perform.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, current_generation_adherents, payer,
    powerless, biographical, identity_locked, global).

% Interpret and transmit the tradition, emphasizing the literal requirement for physical performance. They maintain the textual tradition and prepare for a future restoration, but cannot alter the core requirement. Their authority is grounded in this interpretive lineage.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, rabbinic_scholars, agenda_setter,
    institutional, generational, constrained, global).

% The conceptual beneficiary of this reading, as all current efforts are directed towards its eventual realization. It represents the state where the obligation can finally be fulfilled, but it is not an active agent.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, future_messianic_era, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_continuity__performance_only, future_messianic_era).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the community's focus on the future restoration of the Temple and its rituals, ensuring the continuity of the textual tradition and the readiness for performance.
% TRANSFER_FUNCTION: Transfers a sense of unfulfilled obligation and spiritual longing from the current generation of adherents to the conceptual future messianic era, where it is anticipated to be resolved.
% ABSENT_VOICES: Those who might argue for a more lenient or symbolic interpretation of the obligation are implicitly excluded by the strict adherence to physical performance. Their voices are present in other readings of the kernel, but not in this one.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the spiritual landscape for adherents would fundamentally shift. The burden of unfulfillable obligation would lift, potentially leading to new forms of religious practice or a re-evaluation of the tradition's core tenets. The focus on future restoration would diminish, and the role of study as mere preparation would be challenged.
% FOUNDING_PROBLEM: The destruction of the Temple and the cessation of physical sacrifices, creating a dilemma for adherents regarding the continuity of divine commandments.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by rabbinic scholars and a significant portion of the adherent community, who continue to mourn the Temple's destruction and pray for its restoration. This corroboration comes from within the tradition but is widely held.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because adherents are perpetually unable to fulfill a core religious duty, leading to spiritual and psychological costs without remedy. Suppression is high because the 'identity_locked' exit option for adherents means they cannot simply abandon the obligation, and no alternative means of fulfillment (like study) is recognized. Theater ratio is low because the constraint is genuinely about an unfulfilled obligation, not a performance masking atrophy. Accessibility collapse is high as there are no recognized alternatives to physical performance. Resistance is low because the impossibility is widely accepted within this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of current_generation_adherents, this constraint is a snare, trapping them in an unfulfillable obligation. From the perspective of rabbinic_scholars, it is a necessary truth of the tradition, a mountain-like reality that must be acknowledged and prepared for, even if it causes hardship.
 *
 * DIRECTIONALITY LOGIC:
 *   Current generation adherents are full targets (payer role, identity_locked exit, unfulfillable obligation). Rabbinic scholars, while also bound by the tradition, act as agenda-setters and interpreters, maintaining the framework that defines the obligation, thus having a more moderate directionality. The future messianic era is a conceptual beneficiary, as the entire framework is oriented towards its eventual realization.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a Piton or a Rope. It is not a Piton because the obligation is still very much 'live' and causes significant spiritual extraction, not mere inertial maintenance. It is not a Rope because it fails to coordinate a collective good for the current generation; instead, it imposes an unfulfillable burden. The high extractiveness and suppression, coupled with the unfulfillable nature of the obligation, firmly place it as a Snare from the perspective of the adherents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''performance_only'' reading of the ''sacrifice_obligation_continuity'' kernel?',
    'Analysis of primary textual sources and interpretive traditions to confirm the specific tenets of this reading and its distinction from sibling readings.',
    'If misidentified, the classification of extractiveness and suppression would be inaccurate, potentially leading to a different constraint type or a different understanding of the underlying spiritual burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific interpretation of the sacrifice obligation.').

omega_variable(
    spiritual_extraction_quantification,
    'How can the ''spiritual deficit'' or ''guilt'' experienced by adherents be more precisely quantified as extractiveness?',
    'Sociological studies, theological surveys, and qualitative interviews with adherents to measure the psychological and communal impact of the unfulfillable obligation.',
    'A more precise quantification could refine the extractiveness score, potentially shifting the severity of the Snare classification, though the core type is unlikely to change given the structural impossibility of fulfillment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_extraction_quantification, empirical, 'Quantifies the non-material costs of the unfulfillable obligation.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the high suppression primarily structural (absence of Temple) or internalized (identity-locked adherence to the tradition)?',
    'Post-exit suppression trajectory: if adherents were to leave the tradition, would the sense of unfulfilled obligation persist? If so, it indicates a strong internalized component.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after any theoretical ''exit'' from the formal constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural vs. internalized suppression mechanism for spiritual obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__performance_only, theater_ratio, 20, 0.1).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_continuity__performance_only, theater_ratio, 40, 0.1).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_continuity__performance_only, theater_ratio, 60, 0.1).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_continuity__performance_only, theater_ratio, 80, 0.1).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_continuity__performance_only, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__performance_only, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_continuity__performance_only, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_continuity__performance_only, base_extractiveness, 60, 0.84).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_continuity__performance_only, base_extractiveness, 80, 0.85).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_continuity__performance_only, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(sacr_su_t20, sacrifice_obligation_continuity__performance_only, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(sacr_su_t40, sacrifice_obligation_continuity__performance_only, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(sacr_su_t60, sacrifice_obligation_continuity__performance_only, suppression_requirement, 60, 0.89).
narrative_ontology:measurement(sacr_su_t80, sacrifice_obligation_continuity__performance_only, suppression_requirement, 80, 0.9).
narrative_ontology:measurement(sacr_su_t100, sacrifice_obligation_continuity__performance_only, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
