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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition for Veneration
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint describes the 'domain partition' reading of simultaneous
 *   Kami-Buddha veneration in pre-Meiji Japan, where Kami were associated
 *   with this-worldly prosperity and Buddhas with afterlife salvation. This
 *   reading posits a functional specialization that allowed both religious
 *   systems to coexist and be venerated without inherent contradiction. It is
 *   presented as a coordination mechanism (a Rope) that minimized theological
 *   friction and maximized utility for practitioners. This story is one
 *   reading of the 'simultaneous_veneration' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.05).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.1).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Kami-Buddha Domain Partition for Veneration").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious_studies/comparative_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, 'dcc7e5e6-b37a-415c-9c36-bed55a13e770').
narrative_ontology:cs_kernel_codification('dcc7e5e6-b37a-415c-9c36-bed55a13e770', implicit).
narrative_ontology:cs_authority_grounding('dcc7e5e6-b37a-415c-9c36-bed55a13e770', practice).
narrative_ontology:cs_interpretation_layer_present('dcc7e5e6-b37a-415c-9c36-bed55a13e770').
narrative_ontology:cs_reading_relation('dcc7e5e6-b37a-415c-9c36-bed55a13e770', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('dcc7e5e6-b37a-415c-9c36-bed55a13e770', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('dcc7e5e6-b37a-415c-9c36-bed55a13e770', foundational, functional_differentiation_of_divine_entities).
narrative_ontology:cs_axiom_status(functional_differentiation_of_divine_entities, holdable).
narrative_ontology:cs_axiom_grounding('dcc7e5e6-b37a-415c-9c36-bed55a13e770', functional_differentiation_of_divine_entities, conventional).
narrative_ontology:cs_axiom('dcc7e5e6-b37a-415c-9c36-bed55a13e770', foundational, domain_specific_efficacy_of_veneration).
narrative_ontology:cs_axiom_status(domain_specific_efficacy_of_veneration, holdable).
narrative_ontology:cs_axiom_grounding('dcc7e5e6-b37a-415c-9c36-bed55a13e770', domain_specific_efficacy_of_veneration, empirically_contingent).
narrative_ontology:cs_reference_frame('dcc7e5e6-b37a-415c-9c36-bed55a13e770', pre_meiji_religious_pluralism).
narrative_ontology:cs_drift_state('dcc7e5e6-b37a-415c-9c36-bed55a13e770', contemporary_academic_discourse, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('dcc7e5e6-b37a-415c-9c36-bed55a13e770', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, practitioners_seeking_specific_benefits).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, religious_institutions).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, functional_differentiation_of_deities).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, religious_pluralism_as_specialization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a clear framework for addressing specific needs (e.g., kami for worldly success, buddhas for ancestral rites). This partition provides a coherent, low-friction path for ritual action without perceived contradiction.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, practitioners_seeking_specific_benefits, beneficiary,
    moderate, biographical, mobile, local).

% Benefits from a stable, non-contradictory theological framework that allows both Shinto shrines and Buddhist temples to operate without direct conflict over their core functions. This reduces inter-institutional friction and clarifies their respective roles.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, religious_institutions, beneficiary,
    organized, generational, constrained, national).

% Analyzes the historical and theological coherence of the domain partition reading. This seat seeks to understand the structural logic and functional implications of the constraint.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, scholarly_interpreters, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates religious practice by assigning distinct functional domains to Kami and Buddhas, allowing practitioners to seek specific benefits (e.g., worldly prosperity from Kami, salvation from Buddhas) without theological conflict or confusion.
% TRANSFER_FUNCTION: Facilitates the flow of ritual offerings and devotion to the appropriate divine entities based on the desired outcome, from practitioners to Kami or Buddhas via their respective institutions.
% ABSENT_VOICES: Those who might argue for a purely monotheistic or monistic understanding of divinity, or those who see the distinction as a later, artificial imposition, are not central to the operational logic of this domain-partitioned veneration.
% DISAPPEARANCE_RATIONALE: If the functional distinction between Kami and Buddhas vanished, practitioners would lose a clear guide for ritual action, leading to confusion and potential conflict between religious institutions. The established patterns of veneration and institutional roles would need to be re-evaluated.
% FOUNDING_PROBLEM: The need to integrate indigenous Japanese religious practices (Kami veneration) with imported Buddhism without creating theological contradiction or competition for adherents and resources.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts and contemporary religious leaders within both Shinto and Buddhist traditions attest to the ongoing utility of this functional partition for maintaining religious harmony and guiding practice. Anthropological studies of Japanese folk religion also corroborate its persistence among practitioners.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is very low (0.05) because this reading emphasizes a functional, non-extractive coordination. Suppression is low (0.1) as the system was largely self-regulating through cultural understanding rather than active coercion. Theater ratio is also low (0.05) as the functional distinction was genuinely operative for practitioners. Accessibility collapse is high (0.8) because once this functional partition is understood, alternative, contradictory frameworks for veneration become less accessible or desirable for practitioners seeking clear guidance. Resistance is low (0.02) because this reading provided a stable and useful framework for most practitioners and institutions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of practitioners and religious institutions, this domain partition was a coherent and beneficial arrangement, leading to a 'Rope' classification. An analytical observer might note the underlying tensions or alternative interpretations, but within this reading's framework, the system functioned as a coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Practitioners and religious institutions are beneficiaries, as the constraint provides a clear, low-friction framework for religious life. There are no identifiable victims in this reading, as the functional partition is seen as mutually beneficial. The directionality for beneficiaries would be low, reflecting the subsidy provided by this coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a functional coordination as pure extraction. The 'domain partition' reading highlights a genuine problem-solving function (integrating two religious traditions) that remained live throughout the pre-Meiji period, thus avoiding mandatrophy. The low extractiveness and suppression metrics reflect this functional utility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_accuracy_of_partition,
    'To what extent did practitioners consistently adhere to this strict domain partition, or did their beliefs and practices often blur these lines?',
    'Detailed analysis of primary historical sources (e.g., diaries, ritual manuals, folk tales) to identify instances of overlapping or contradictory veneration patterns.',
    'If adherence was inconsistent, the ''domain_partition_reading'' might be reclassified as having higher theater or lower accessibility collapse, as the stated coordination function was not always fully realized in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_accuracy_of_partition, empirical, 'Empirical question about the consistency of the domain partition in historical practice.').

omega_variable(
    conceptual_coherence_vs_fusion,
    'Is the functional domain partition a genuinely coherent theological framework, or does it merely paper over an underlying ontological tension that the ''ontological_fusion_reading'' attempts to resolve?',
    'Philosophical and theological analysis comparing the internal consistency of the domain partition with the metaphysical claims of honji-suijaku theory.',
    'If the partition is found to be conceptually fragile, the ''domain_partition_reading'' might be seen as a more ''tangled_rope'' or ''snare'' for intellectual coherence, requiring more ''suppression'' of alternative interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conceptual_coherence_vs_fusion, conceptual, 'Conceptual ambiguity regarding the fundamental coherence of the domain partition versus ontological fusion.').

omega_variable(
    meiji_restoration_impact,
    'How did the Meiji government''s Shinbutsu-bunri (separation of Kami and Buddhas) policies affect the perceived ''naturalness'' or ''coordination function'' of this domain partition?',
    'Historical analysis of post-Meiji religious discourse and practice, comparing it to the pre-Meiji period to identify shifts in how the Kami-Buddha relationship was understood and enacted.',
    'If the separation policies were seen as a violent rupture of a ''natural'' coordination, it would strengthen the ''rope'' classification for the pre-Meiji period. If the separation merely formalized an already existing, albeit unacknowledged, tension, it would weaken the ''rope'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_restoration_impact, empirical, 'Impact of external political intervention on the perceived functional distinction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t700, simultaneous_veneration__domain_partition_reading, theater_ratio, 700, 0.05).
narrative_ontology:measurement(simu_tr_t900, simultaneous_veneration__domain_partition_reading, theater_ratio, 900, 0.05).
narrative_ontology:measurement(simu_tr_t1100, simultaneous_veneration__domain_partition_reading, theater_ratio, 1100, 0.05).
narrative_ontology:measurement(simu_tr_t1300, simultaneous_veneration__domain_partition_reading, theater_ratio, 1300, 0.05).
narrative_ontology:measurement(simu_tr_t1500, simultaneous_veneration__domain_partition_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__domain_partition_reading, theater_ratio, 1868, 0.05).

% Extraction over time
narrative_ontology:measurement(simu_be_t700, simultaneous_veneration__domain_partition_reading, base_extractiveness, 700, 0.05).
narrative_ontology:measurement(simu_be_t900, simultaneous_veneration__domain_partition_reading, base_extractiveness, 900, 0.05).
narrative_ontology:measurement(simu_be_t1100, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1100, 0.05).
narrative_ontology:measurement(simu_be_t1300, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1300, 0.05).
narrative_ontology:measurement(simu_be_t1500, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1868, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t700, simultaneous_veneration__domain_partition_reading, suppression_requirement, 700, 0.1).
narrative_ontology:measurement(simu_su_t900, simultaneous_veneration__domain_partition_reading, suppression_requirement, 900, 0.1).
narrative_ontology:measurement(simu_su_t1100, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1100, 0.1).
narrative_ontology:measurement(simu_su_t1300, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1300, 0.1).
narrative_ontology:measurement(simu_su_t1500, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1868, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'simultaneous_veneration' kernel. This 'domain_partition_reading' emphasizes functional specialization, while the 'ontological_fusion_reading' posits metaphysical identity, and the 'pragmatic_incoherence_reading' views the practice as inherently contradictory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
