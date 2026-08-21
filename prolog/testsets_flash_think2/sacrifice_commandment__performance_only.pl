% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__performance_only, []).

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
 *   constraint_id: sacrifice_commandment__performance_only
 *   human_readable: Sacrifice Commandment: Performance-Only Reading
 *   domain: religious/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint represents the 'performance-only' reading of the
 *   sacrifice commandment within Halakha, which asserts that physical
 *   execution of sacrifices is an indispensable requirement, and therefore,
 *   in the absence of the Temple, the commandment is suspended, not fulfilled
 *   by alternative means. This reading directs significant scholarly and
 *   devotional energy towards the theoretical study of an unperformable act,
 *   leading to high extraction from scholarly attention and devout adherents.
 *   The constraint is claimed as a Snare due to its high extractiveness and
 *   the identifiable victims whose religious energy is diverted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__performance_only, 0.85).
domain_priors:suppression_score(sacrifice_commandment__performance_only, 0.78).
domain_priors:theater_ratio(sacrifice_commandment__performance_only, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, extractiveness, 0.85).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_commandment__performance_only, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__performance_only, snare).
narrative_ontology:human_readable(sacrifice_commandment__performance_only, "Sacrifice Commandment: Performance-Only Reading").
narrative_ontology:topic_domain(sacrifice_commandment__performance_only, "religious/halakhic_theory/commitment_system_analysis").

domain_priors:requires_active_enforcement(sacrifice_commandment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__performance_only, 'ded61745-e1aa-4e83-9203-ecf5b6bbee55').
narrative_ontology:cs_kernel_codification('ded61745-e1aa-4e83-9203-ecf5b6bbee55', fixed_text).
narrative_ontology:cs_authority_grounding('ded61745-e1aa-4e83-9203-ecf5b6bbee55', lineage).
narrative_ontology:cs_interpretation_layer_present('ded61745-e1aa-4e83-9203-ecf5b6bbee55').
narrative_ontology:cs_reading_relation('ded61745-e1aa-4e83-9203-ecf5b6bbee55', sacrifice_commandment__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('ded61745-e1aa-4e83-9203-ecf5b6bbee55', sacrifice_commandment__archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('ded61745-e1aa-4e83-9203-ecf5b6bbee55', foundational, physical_execution_is_sine_qua_non).
narrative_ontology:cs_axiom_status(physical_execution_is_sine_qua_non, holdable).
narrative_ontology:cs_axiom_grounding('ded61745-e1aa-4e83-9203-ecf5b6bbee55', physical_execution_is_sine_qua_non, deontological).
narrative_ontology:cs_axiom('ded61745-e1aa-4e83-9203-ecf5b6bbee55', secondary, absence_of_temple_suspends_commandment).
narrative_ontology:cs_axiom_status(absence_of_temple_suspends_commandment, holdable).
narrative_ontology:cs_axiom_grounding('ded61745-e1aa-4e83-9203-ecf5b6bbee55', absence_of_temple_suspends_commandment, conventional).
narrative_ontology:cs_reference_frame('ded61745-e1aa-4e83-9203-ecf5b6bbee55', original_temple_era_physical_commandment).
narrative_ontology:cs_drift_state('ded61745-e1aa-4e83-9203-ecf5b6bbee55', post_second_temple_destruction, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('ded61745-e1aa-4e83-9203-ecf5b6bbee55', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__performance_only, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, halakhic_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__performance_only, religious_authorities).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, devout_adherents).
narrative_ontology:constraint_victim(sacrifice_commandment__performance_only, scholarly_attention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the divine law, maintaining the strict requirement for physical performance of sacrifices. Their intellectual labor is directed at understanding a suspended commandment, reinforcing their role as custodians of complex, unperformable traditions.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Their authority is maintained and reinforced by the strict interpretation that the sacrifice commandment is suspended due to the absence of the Temple. This emphasizes the need for their guidance in navigating complex, unfulfilled divine obligations.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, religious_authorities, beneficiary,
    institutional, generational, identity_locked, global).

% Are devoutly committed to fulfilling divine commandments but are structurally prevented from performing sacrifices due to the Temple's destruction. Their religious energy is redirected towards study of the laws or other forms of worship, often under the guidance of religious authorities.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, devout_adherents, payer,
    powerless, biographical, identity_locked, global).

% Represents the collective intellectual and spiritual resources of the community, which are largely directed towards theoretical understanding and anticipation of a suspended practice, rather than immediate, actionable religious life or other areas of Halakha.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, scholarly_attention, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(sacrifice_commandment__performance_only, scholarly_attention).

% Advocate for an alternative interpretation where the study of sacrifice law itself constitutes fulfillment of the commandment. This reading explicitly rejects their view, structurally excluding their interpretation from the dominant halakhic discourse.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, proponents_of_study_as_performance, excluded,
    organized, biographical, constrained, global).

% Focus on preserving the technical knowledge of sacrifice for a future restoration of the Temple, viewing this as messianic preparation rather than present worship. This reading does not directly contradict their goal but maintains a distinct position regarding the commandment's current status.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__performance_only, proponents_of_archive_maintenance, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intellectual and spiritual life of the community around a central, yet currently unperformable, divine command, maintaining a shared understanding of its suspension and the conditions for its future reinstatement.
% TRANSFER_FUNCTION: Transfers significant scholarly and devotional energy from immediate, performable religious acts to the theoretical study and anticipation of a suspended commandment, reinforcing the authority of those who interpret this suspension.
% ABSENT_VOICES: Proponents of 'study as performance' are structurally excluded from the dominant discourse of this reading, as their interpretation of fulfillment is deemed incorrect. They would argue for a more accessible and immediate mode of divine service.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the entire theological and halakhic framework governing the sacrifice commandment's suspension would collapse. This would necessitate a radical re-evaluation of divine obligation, the role of study, and the authority of religious leadership, fundamentally reorganizing Jewish religious life.
% FOUNDING_PROBLEM: To maintain the integrity and authority of the divine commandment of sacrifice after the destruction of the Second Temple, ensuring its meaning is not diluted or reinterpreted in ways that contradict its original physical requirement, while providing a coherent halakhic response to its unperformability.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing physical absence of the Temple and the continuous rabbinic discourse over nearly two millennia attest to the persistence of this problem. Historical records and archaeological evidence corroborate the Temple's destruction, and the vast body of Halakha on this topic demonstrates the enduring need for a coherent response, from outside the immediate beneficiaries of this specific interpretation.
narrative_ontology:disappearance_verdict(sacrifice_commandment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_commandment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__performance_only, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because 1,900 years of intellectual and spiritual labor are directed towards a commandment that cannot be performed, effectively 'extracting' this energy from other areas of religious life. Suppression is high (0.78) because the theological framework actively suppresses alternative interpretations that would allow for symbolic or intellectual fulfillment. The accessibility collapse is near total (0.95) as the physical requirement makes any alternative impossible. Resistance is low (0.15) because this interpretation is deeply ingrained within the dominant halakhic tradition. The theater ratio is low (0.1) as the focus is on the *absence* of performance, not a theatrical substitute.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of halakhic scholars and religious authorities, this reading maintains the integrity of divine law and their role as its custodians. From the perspective of devout adherents and scholarly attention, it represents a structural barrier to direct fulfillment and a diversion of spiritual energy. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars and religious authorities are beneficiaries (d near 0.0) as their institutional authority and interpretive roles are reinforced by maintaining this strict, complex, and suspended commandment. Devout adherents and scholarly attention are targets (d near 1.0) as their desire for fulfillment and intellectual resources are directed towards an unperformable act. Proponents of 'study as performance' are excluded, as their alternative interpretation is rejected by this reading's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_alternative_fulfillment,
    'Is the sacrifice commandment truly suspended, or are there alternative, non-physical modes of fulfillment that this reading forecloses?',
    'A shift in halakhic consensus, potentially driven by new theological insights or a re-evaluation of scriptural interpretation, that formally recognizes a non-physical mode of fulfillment.',
    'If alternative fulfillment is recognized, the extractiveness and suppression of this constraint would significantly decrease, reclassifying it from a Snare to a Rope or even a Piton (if the original problem is deemed resolved).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_alternative_fulfillment, conceptual, 'Ambiguity regarding the possibility of non-physical fulfillment of the sacrifice commandment.').

omega_variable(
    impact_on_living_halakha,
    'To what extent does the diversion of scholarly attention to suspended commandments detract from the development and application of Halakha relevant to contemporary living?',
    'Sociological and theological studies analyzing the allocation of scholarly resources and the perceived vitality of different areas of Halakha over time.',
    'If a significant negative impact is demonstrated, it would further support the high extractiveness of this constraint, highlighting the opportunity cost of this interpretive framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_living_halakha, empirical, 'The opportunity cost of scholarly focus on suspended commandments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__performance_only, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__performance_only, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t380, sacrifice_commandment__performance_only, theater_ratio, 380, 0.1).
narrative_ontology:measurement(sacr_tr_t760, sacrifice_commandment__performance_only, theater_ratio, 760, 0.1).
narrative_ontology:measurement(sacr_tr_t1140, sacrifice_commandment__performance_only, theater_ratio, 1140, 0.1).
narrative_ontology:measurement(sacr_tr_t1520, sacrifice_commandment__performance_only, theater_ratio, 1520, 0.1).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_commandment__performance_only, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__performance_only, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(sacr_be_t380, sacrifice_commandment__performance_only, base_extractiveness, 380, 0.82).
narrative_ontology:measurement(sacr_be_t760, sacrifice_commandment__performance_only, base_extractiveness, 760, 0.83).
narrative_ontology:measurement(sacr_be_t1140, sacrifice_commandment__performance_only, base_extractiveness, 1140, 0.84).
narrative_ontology:measurement(sacr_be_t1520, sacrifice_commandment__performance_only, base_extractiveness, 1520, 0.84).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_commandment__performance_only, base_extractiveness, 1900, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__performance_only, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(sacr_su_t380, sacrifice_commandment__performance_only, suppression_requirement, 380, 0.76).
narrative_ontology:measurement(sacr_su_t760, sacrifice_commandment__performance_only, suppression_requirement, 760, 0.77).
narrative_ontology:measurement(sacr_su_t1140, sacrifice_commandment__performance_only, suppression_requirement, 1140, 0.77).
narrative_ontology:measurement(sacr_su_t1520, sacrifice_commandment__performance_only, suppression_requirement, 1520, 0.78).
narrative_ontology:measurement(sacr_su_t1900, sacrifice_commandment__performance_only, suppression_requirement, 1900, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__performance_only, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_commandment' kernel, focusing on the requirement for physical performance and the resulting suspension of the commandment without the Temple. It is structurally distinct from the 'study_as_performance' and 'archive_maintenance' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
