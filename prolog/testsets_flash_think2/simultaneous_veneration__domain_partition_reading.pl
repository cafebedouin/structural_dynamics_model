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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Functional Domain Partition for Kami and Buddhas
 *   domain: religious/cultural
 *
 * SUMMARY:
 *   This constraint describes the interpretive framework in Japanese
 *   religious history where Kami and Buddhas were understood as functionally
 *   distinct entities governing separate domains (this-worldly prosperity vs.
 *   afterlife salvation). Simultaneous veneration was seen as
 *   domain-appropriate specialization, a coherent and functional approach to
 *   religious practice. This is one reading of the 'simultaneous_veneration'
 *   kernel, focusing on the functional partition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.15).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.2).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Functional Domain Partition for Kami and Buddhas").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious/cultural").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, '3c09e9fa-e264-42a0-9463-ba88d5ecf10e').
narrative_ontology:cs_kernel_codification('3c09e9fa-e264-42a0-9463-ba88d5ecf10e', implicit).
narrative_ontology:cs_authority_grounding('3c09e9fa-e264-42a0-9463-ba88d5ecf10e', practice).
narrative_ontology:cs_interpretation_layer_present('3c09e9fa-e264-42a0-9463-ba88d5ecf10e').
narrative_ontology:cs_reading_relation('3c09e9fa-e264-42a0-9463-ba88d5ecf10e', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c09e9fa-e264-42a0-9463-ba88d5ecf10e', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('3c09e9fa-e264-42a0-9463-ba88d5ecf10e', foundational, functional_distinction_of_deities).
narrative_ontology:cs_axiom_status(functional_distinction_of_deities, holdable).
narrative_ontology:cs_axiom_grounding('3c09e9fa-e264-42a0-9463-ba88d5ecf10e', functional_distinction_of_deities, conventional).
narrative_ontology:cs_axiom('3c09e9fa-e264-42a0-9463-ba88d5ecf10e', foundational, domain_appropriate_veneration).
narrative_ontology:cs_axiom_status(domain_appropriate_veneration, holdable).
narrative_ontology:cs_axiom_grounding('3c09e9fa-e264-42a0-9463-ba88d5ecf10e', domain_appropriate_veneration, conventional).
narrative_ontology:cs_reference_frame('3c09e9fa-e264-42a0-9463-ba88d5ecf10e', heian_period_syncretic_coexistence).
narrative_ontology:cs_drift_state('3c09e9fa-e264-42a0-9463-ba88d5ecf10e', meiji_restoration_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3c09e9fa-e264-42a0-9463-ba88d5ecf10e', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, religious_practitioners).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, religious_institutions).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, theological_pluralism_doctrine).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, functional_differentiation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adhere to the understanding that Kami and Buddhas govern distinct domains, allowing for coherent simultaneous veneration. They benefit from reduced cognitive dissonance and clear guidance on appropriate ritual practice.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, religious_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Maintain and transmit this theological framework through teachings and ritual practices. They benefit from the stability and clarity this interpretation provides for their institutional roles and the guidance of their adherents.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, religious_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__domain_partition_reading, religious_institutions, beneficiary).

% Analyze the historical and theological development of this interpretive framework, documenting its role in Japanese religious history and its relationship to other readings of simultaneous veneration.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, universal).

% Propose that Kami and Buddhas are ontologically identical, a view not foregrounded by this domain-partition reading. Their alternative interpretation is not the primary framework for understanding veneration within this constraint.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, ontological_fusion_advocates, excluded,
    moderate, biographical, constrained, global).

% Argue that simultaneous veneration was never truly coherent, and practitioners held contradictory beliefs. This perspective is not acknowledged as the primary explanation for the persistence of dual veneration within this constraint's framework.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, pragmatic_incoherence_advocates, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent framework for the simultaneous veneration of Kami and Buddhas by assigning them distinct, non-overlapping functional domains (this-worldly prosperity vs. afterlife salvation), thereby reducing theological contradiction for practitioners.
% TRANSFER_FUNCTION: Transfers cognitive clarity and reduced theological conflict to religious practitioners and institutions, in exchange for adherence to the domain-partitioning norm in their understanding and practice.
% ABSENT_VOICES: Advocates of ontological fusion or pragmatic incoherence are structurally excluded from the discourse that establishes this domain partition as the primary interpretive framework. They would argue for alternative explanations of Kami-Buddha relations.
% DISAPPEARANCE_RATIONALE: If this interpretive framework vanished overnight, religious practitioners would lack clear guidance on how to reconcile Kami and Buddha veneration, potentially leading to theological confusion, increased conflict between traditions, or a shift towards more exclusive religious identities, reorganizing the landscape of religious practice.
% FOUNDING_PROBLEM: The need to reconcile the indigenous Kami tradition with imported Buddhism, allowing for simultaneous veneration without theological contradiction or syncretic confusion, thereby maintaining social and religious harmony.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts from religious scholars, institutional records from temples and shrines, and ethnographic studies of contemporary practice attest to the development and persistence of this interpretive framework as a functional solution to the problem of dual veneration. This corroboration comes from both internal religious authorities and external academic observers.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a 'rope' due to its primary function of coordinating religious practice and belief, providing a coherent framework for dual veneration with minimal extraction. Extractiveness (0.15) is low, representing the inherent cognitive cost of maintaining a complex theological distinction, not active rent-seeking. Suppression (0.20) is also low, reflecting the cultural and social pressure to conform to this widely accepted interpretation, rather than coercive enforcement. Theater ratio (0.10) is negligible, as the framework was genuinely functional. The metrics show slight increases over time as the interpretation became more institutionalized, but remained within the 'rope' range.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious practitioners and institutions, this framework provided a clear and beneficial coordination mechanism. However, alternative readings, such as ontological fusion or pragmatic incoherence, offer different explanations for the same phenomena, highlighting the interpretive contestation inherent in the kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious practitioners and institutions are beneficiaries (low directionality) as they gain cognitive clarity and a stable framework for their practices. There are no identifiable victims, as the constraint primarily serves a coordination function without imposing asymmetric costs. Scholars act as observers, analyzing the framework's historical and theological significance.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of this constraint — to provide a coherent framework for simultaneous Kami and Buddha veneration — remained live throughout the specified interval. Its persistence was due to its ongoing functional utility in coordinating religious life, not due to atrophied function or theatrical maintenance. The founding problem of reconciling indigenous and imported traditions remained relevant until external forces (Meiji Restoration) introduced new constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately identified as the ''domain_partition_reading'' of the ''simultaneous_veneration'' kernel?',
    'Further historical and theological analysis of primary sources to confirm the distinctness and prevalence of this interpretive framework compared to sibling readings.',
    'If this reading is not sufficiently distinct or prevalent, it might need to be subsumed into a broader ''coexists_with'' relationship with other interpretations, or its classification as a ''rope'' might be challenged if its coordination function was less dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific identity of this constraint as one reading of a contested kernel.').

omega_variable(
    sibling_ontological_fusion_impact,
    'How would the classification of simultaneous veneration change if the ''ontological_fusion_reading'' (Kami and Buddhas are ontologically identical) were adopted as the primary framework?',
    'Analyze the structural implications of the ontological fusion reading: would it imply a different beneficiary/victim structure, or a higher/lower extraction, if one entity''s identity subsumed the other?',
    'If ontological fusion implied a hierarchy or a dominant entity, it might lead to a higher extractiveness or even a ''tangled_rope'' classification, as one aspect of the divine might extract from the other''s domain. If it implied perfect equivalence, it might remain a ''rope'' but with a different coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_ontological_fusion_impact, conceptual, 'Impact of an alternative ontological fusion reading on constraint classification.').

omega_variable(
    sibling_pragmatic_incoherence_impact,
    'How would the classification of simultaneous veneration change if the ''pragmatic_incoherence_reading'' (practitioners held contradictory beliefs) were adopted as the primary framework?',
    'Examine evidence for widespread cognitive dissonance or unacknowledged conflict among practitioners. If incoherence was the norm, was there an unmeasured ''extraction'' of cognitive burden?',
    'If pragmatic incoherence was dominant, the ''rope'' classification might be challenged. It could imply a lower extractiveness if no real constraint existed, or a higher, unacknowledged extraction (e.g., cognitive burden) if practitioners were forced to hold contradictory beliefs without a coherent framework, potentially shifting towards a ''snare'' of cognitive extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_pragmatic_incoherence_impact, empirical, 'Impact of an alternative pragmatic incoherence reading on constraint classification.').

omega_variable(
    historical_dominance_ambiguity,
    'Was the ''domain_partition_reading'' truly the dominant interpretive framework throughout the interval, or merely one among several coexisting interpretations?',
    'Quantitative analysis of historical texts, ritual manuals, and popular religious narratives to assess the prevalence and influence of this reading compared to others.',
    'If this reading was less dominant than assumed, its ''rope'' classification might be less representative of the overall religious landscape, and the ''coexists_with'' relationships with sibling readings would gain more prominence, potentially leading to a more ''distributed'' authority grounding for the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_dominance_ambiguity, empirical, 'Assesses the historical dominance of this specific interpretive reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 800, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__domain_partition_reading, theater_ratio, 800, 0.08).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__domain_partition_reading, theater_ratio, 1000, 0.09).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__domain_partition_reading, theater_ratio, 1200, 0.09).
narrative_ontology:measurement(simu_tr_t1400, simultaneous_veneration__domain_partition_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement(simu_tr_t1600, simultaneous_veneration__domain_partition_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__domain_partition_reading, theater_ratio, 1868, 0.1).

% Extraction over time
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__domain_partition_reading, base_extractiveness, 800, 0.1).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1200, 0.13).
narrative_ontology:measurement(simu_be_t1400, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1400, 0.14).
narrative_ontology:measurement(simu_be_t1600, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1868, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t800, simultaneous_veneration__domain_partition_reading, suppression_requirement, 800, 0.15).
narrative_ontology:measurement(simu_su_t1000, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1000, 0.17).
narrative_ontology:measurement(simu_su_t1200, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1200, 0.18).
narrative_ontology:measurement(simu_su_t1400, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1400, 0.19).
narrative_ontology:measurement(simu_su_t1600, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1600, 0.2).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1868, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'simultaneous_veneration' kernel, focusing on functional domain partition. It is linked to sibling readings 'ontological_fusion_reading' and 'pragmatic_incoherence_reading' which offer alternative interpretations of Kami-Buddha relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
