% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__domain_partition_reading, []).

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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Simultaneous Veneration: Domain Partition Reading
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint represents the 'domain partition' reading of simultaneous
 *   veneration in pre-Meiji Japan, where kami and buddhas were understood as
 *   functionally distinct entities governing separate domains (this-worldly
 *   prosperity vs. afterlife salvation). Simultaneous veneration was thus
 *   seen as a coherent, domain-appropriate specialization rather than a
 *   contradiction. This reading emphasizes the coordination function of this
 *   conceptual framework for religious practitioners and institutions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.05).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.02).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Simultaneous Veneration: Domain Partition Reading").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious_studies/comparative_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, '42c32496-544c-4b18-820a-b32b67ff9a33').
narrative_ontology:cs_kernel_codification('42c32496-544c-4b18-820a-b32b67ff9a33', implicit).
narrative_ontology:cs_authority_grounding('42c32496-544c-4b18-820a-b32b67ff9a33', practice).
narrative_ontology:cs_interpretation_layer_present('42c32496-544c-4b18-820a-b32b67ff9a33').
narrative_ontology:cs_reading_relation('42c32496-544c-4b18-820a-b32b67ff9a33', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('42c32496-544c-4b18-820a-b32b67ff9a33', simultaneous_veneration__pragmatic_incoherence_reading, forecloses).
narrative_ontology:cs_axiom('42c32496-544c-4b18-820a-b32b67ff9a33', foundational, kami_buddha_functional_specialization).
narrative_ontology:cs_axiom_status(kami_buddha_functional_specialization, holdable).
narrative_ontology:cs_axiom_grounding('42c32496-544c-4b18-820a-b32b67ff9a33', kami_buddha_functional_specialization, conventional).
narrative_ontology:cs_axiom('42c32496-544c-4b18-820a-b32b67ff9a33', foundational, religious_coherence_through_domain_partition).
narrative_ontology:cs_axiom_status(religious_coherence_through_domain_partition, holdable).
narrative_ontology:cs_axiom_grounding('42c32496-544c-4b18-820a-b32b67ff9a33', religious_coherence_through_domain_partition, instrumental).
narrative_ontology:cs_reference_frame('42c32496-544c-4b18-820a-b32b67ff9a33', pre_meiji_syncretic_practice).
narrative_ontology:cs_drift_state('42c32496-544c-4b18-820a-b32b67ff9a33', meiji_separation_edict, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('42c32496-544c-4b18-820a-b32b67ff9a33', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, japanese_religious_practitioners).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, religious_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a clear, non-contradictory framework for engaging with both kami (for this-worldly concerns) and buddhas (for afterlife concerns), allowing for comprehensive spiritual practice without conflict.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, japanese_religious_practitioners, beneficiary,
    moderate, biographical, mobile, local).

% Benefits from a stable theological framework that allows for the coexistence and specialization of Shinto shrines and Buddhist temples, reducing inter-institutional conflict and facilitating distinct revenue streams and ritual services.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, religious_institutions, agenda_setter,
    organized, generational, constrained, national).

% Analyzes the historical and theological coherence of the domain partition reading, evaluating its explanatory power against other interpretations of Japanese religious history.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, theological_scholars, observer,
    analytical, generational, analytical, global).

% Historically attempted to enforce a strict separation of kami and buddhas, viewing simultaneous veneration as incoherent or impure. This reading directly contradicts their historical agenda by asserting the coherence of the pre-Meiji system.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, meiji_state_shinto_advocates, excluded,
    institutional, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates religious practice by assigning distinct, non-overlapping functional domains to kami (this-worldly prosperity, local protection) and buddhas (afterlife salvation, spiritual enlightenment), allowing practitioners to address a full spectrum of spiritual needs through specialized entities.
% TRANSFER_FUNCTION: Facilitates the flow of ritual offerings, prayers, and spiritual devotion to the appropriate divine entities based on the practitioner's specific needs (e.g., prayers for good harvest to kami, prayers for ancestors to buddhas).
% ABSENT_VOICES: The Meiji-era State Shinto advocates, who enforced a strict separation of kami and buddhas, would object to this reading's assertion of pre-Meiji coherence. They are absent from this reading's internal logic, which validates the pre-separation system.
% DISAPPEARANCE_RATIONALE: If this understanding of domain partition vanished, Japanese religious practice would lose a foundational framework for its historical syncretism. Practitioners would face conceptual confusion regarding the roles of kami and buddhas, potentially leading to a fragmentation of religious identity and institutional conflict.
% FOUNDING_PROBLEM: The need to reconcile indigenous animistic beliefs (kami worship) with imported Buddhist doctrines, providing a coherent framework for practitioners to engage with both without perceived contradiction.
% FOUNDING_PROBLEM_CORROBORATION: Scholarly consensus in comparative religion and Japanese history corroborates the historical existence and functional coherence of this domain partition, as evidenced by pre-Meiji religious texts, ritual practices, and institutional structures. This corroboration comes from outside the direct beneficiaries (practitioners and institutions) and is supported by analytical observers (theological scholars).
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__domain_partition_reading_tests).
:- end_tests(simultaneous_veneration__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because it provides a clear coordination function (domain specialization) with minimal extraction (0.05) and suppression (0.02). It allows for a comprehensive religious life without requiring active enforcement to maintain its coherence; its persistence is due to its utility. The low theater ratio (0.01) reflects that the framework was genuinely functional, not performative. Accessibility collapse is high (0.9) because, within this framework, alternatives to this specialized veneration are conceptually difficult to form without introducing contradiction. Resistance is low (0.01) because the framework was widely accepted and beneficial to practitioners.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of practitioners and institutions, this framework was a functional Rope, enabling coherent religious life. From the perspective of later Meiji-era reformers, this system was seen as incoherent and requiring forceful separation. This constraint models the internal coherence of the pre-Meiji system, not the later critique.
 *
 * DIRECTIONALITY LOGIC:
 *   Japanese religious practitioners are beneficiaries as they gain a coherent framework for their spiritual needs. Religious institutions are agenda-setters, as they administer and benefit from this stable theological division of labor. Theological scholars are observers, analyzing its historical and conceptual validity. Meiji State Shinto advocates are excluded, as their opposing view is outside the scope of this reading's internal coherence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy within its historical context, as the founding problem (reconciling indigenous and imported beliefs) remained live throughout the interval. The 'contested' status of the founding problem in the six questions refers to later historical interpretations, not a failure of the constraint's original mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_coherence_vs_later_critique,
    'Does this reading accurately reflect the lived experience and theological understanding of pre-Meiji practitioners, or is it a post-hoc rationalization influenced by later historical critiques?',
    'Further archaeological and textual evidence from the period, particularly non-elite sources, to corroborate the widespread understanding of domain partition.',
    'If it''s a post-hoc rationalization, the constraint''s ''Rope'' classification might be an overstatement of its internal coherence, potentially shifting towards a ''Piton'' or ''Tangled Rope'' if underlying contradictions were actively suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_coherence_vs_later_critique, empirical, 'Assessing the historical accuracy of the domain partition reading.').

omega_variable(
    functional_distinction_vs_ontological_identity,
    'Is the functional distinction between kami and buddhas truly independent of any underlying ontological relationship, or does this reading implicitly rely on a subtle form of ontological fusion?',
    'Detailed philosophical analysis of pre-Meiji theological texts to identify explicit or implicit statements on the metaphysical nature of kami and buddhas in relation to their functional roles.',
    'If an implicit ontological fusion is found, this reading''s distinctness from the ''ontological_fusion_reading'' would diminish, potentially leading to a re-evaluation of its ''Rope'' classification if the fusion introduced unacknowledged extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_distinction_vs_ontological_identity, conceptual, 'Distinguishing functional specialization from ontological identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t700, simultaneous_veneration__domain_partition_reading, theater_ratio, 700, 0.01).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__domain_partition_reading, theater_ratio, 1000, 0.01).
narrative_ontology:measurement(simu_tr_t1300, simultaneous_veneration__domain_partition_reading, theater_ratio, 1300, 0.01).
narrative_ontology:measurement(simu_tr_t1600, simultaneous_veneration__domain_partition_reading, theater_ratio, 1600, 0.01).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__domain_partition_reading, theater_ratio, 1868, 0.01).

% Extraction over time
narrative_ontology:measurement(simu_be_t700, simultaneous_veneration__domain_partition_reading, base_extractiveness, 700, 0.05).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(simu_be_t1300, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1300, 0.05).
narrative_ontology:measurement(simu_be_t1600, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1600, 0.05).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1868, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t700, simultaneous_veneration__domain_partition_reading, suppression_requirement, 700, 0.02).
narrative_ontology:measurement(simu_su_t1000, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1000, 0.02).
narrative_ontology:measurement(simu_su_t1300, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1300, 0.02).
narrative_ontology:measurement(simu_su_t1600, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1600, 0.02).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1868, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'simultaneous_veneration' kernel, focusing on the domain partition. It is linked to sibling readings that offer alternative interpretations of the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
