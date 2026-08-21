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
 *   human_readable: Functional Domain Partition of Kami and Buddhas
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint describes the conceptual framework within Japanese
 *   religious history where Kami (Shinto deities) and Buddhas are understood
 *   as functionally distinct entities, each governing separate domains
 *   (this-worldly prosperity vs. afterlife salvation). This reading posits
 *   that simultaneous veneration is not contradictory but rather a form of
 *   domain-appropriate specialization, allowing for a coherent syncretic
 *   practice. The constraint is claimed as a Rope because it primarily serves
 *   a coordination function by providing a stable interpretive framework for
 *   religious practice and scholarship.
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
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Functional Domain Partition of Kami and Buddhas").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious_studies/comparative_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, '67f5c9c9-7af0-4342-b04b-6330760f040e').
narrative_ontology:cs_kernel_codification('67f5c9c9-7af0-4342-b04b-6330760f040e', implicit).
narrative_ontology:cs_authority_grounding('67f5c9c9-7af0-4342-b04b-6330760f040e', practice).
narrative_ontology:cs_interpretation_layer_present('67f5c9c9-7af0-4342-b04b-6330760f040e').
narrative_ontology:cs_reading_relation('67f5c9c9-7af0-4342-b04b-6330760f040e', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('67f5c9c9-7af0-4342-b04b-6330760f040e', simultaneous_veneration__pragmatic_incoherence_reading, forecloses).
narrative_ontology:cs_axiom('67f5c9c9-7af0-4342-b04b-6330760f040e', foundational, functional_specialization_is_real).
narrative_ontology:cs_axiom_status(functional_specialization_is_real, holdable).
narrative_ontology:cs_axiom_grounding('67f5c9c9-7af0-4342-b04b-6330760f040e', functional_specialization_is_real, conventional).
narrative_ontology:cs_axiom('67f5c9c9-7af0-4342-b04b-6330760f040e', foundational, theological_coherence_is_achievable).
narrative_ontology:cs_axiom_status(theological_coherence_is_achievable, holdable).
narrative_ontology:cs_axiom_grounding('67f5c9c9-7af0-4342-b04b-6330760f040e', theological_coherence_is_achievable, conventional).
narrative_ontology:cs_reference_frame('67f5c9c9-7af0-4342-b04b-6330760f040e', heian_syncretic_framework).
narrative_ontology:cs_drift_state('67f5c9c9-7af0-4342-b04b-6330760f040e', contemporary_comparative_religion, gap(stable, minor, true)).
narrative_ontology:cs_created_at('67f5c9c9-7af0-4342-b04b-6330760f040e', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, practitioners_of_shinto_buddhism).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, religious_scholars).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, religious_syncretism_theory).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, functional_differentiation_in_religion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a coherent framework that allows them to engage with both Shinto and Buddhist practices without perceived contradiction, by understanding Kami as governing this-worldly prosperity and Buddhas as governing afterlife salvation.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, practitioners_of_shinto_buddhism, beneficiary,
    moderate, biographical, constrained, national).

% Benefit from a clear and widely accepted interpretive framework that explains the historical coexistence and integration of Shinto and Buddhism in Japan, providing a basis for academic analysis and teaching.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, religious_scholars, beneficiary,
    analytical, generational, analytical, global).

% Propagate and reinforce this understanding of functional distinction, which helps maintain the stability and legitimacy of their dual religious practices and institutions. They are the primary custodians of this interpretive tradition.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, religious_institutions_japan, agenda_setter,
    institutional, civilizational, constrained, national).

% Analyze the historical development and impact of this conceptual framework, often corroborating its role in shaping Japanese religious life, even if they do not personally adhere to its tenets.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, secular_historians, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent framework for practitioners to engage with both Kami and Buddhas without perceived contradiction, by assigning them distinct functional domains (this-worldly prosperity vs. other-worldly salvation), thereby guiding ritual practice and theological understanding.
% TRANSFER_FUNCTION: Transfers conceptual clarity and ritual appropriateness to practitioners, reducing cognitive dissonance and guiding their engagement with diverse religious entities. It also transfers interpretive authority to religious institutions and scholars who maintain this framework.
% ABSENT_VOICES: Practitioners who historically held more ontologically fused views (e.g., through honji-suijaku theory) or those who found the simultaneous veneration pragmatically incoherent. Their perspectives are often marginalized by the dominant interpretive frameworks that emphasize functional distinction or coherence.
% DISAPPEARANCE_RATIONALE: If this conceptual partition vanished overnight, the simultaneous veneration of Kami and Buddhas would lack a coherent theological justification, leading to significant confusion in practice and scholarship. The understanding of Japanese religious history would need to be fundamentally re-evaluated, as a key interpretive tool would be lost.
% FOUNDING_PROBLEM: The core problem was how to reconcile the indigenous Shinto tradition with the imported Buddhist tradition, allowing for their simultaneous veneration and integration into daily life without theological conflict or perceived contradiction among practitioners.
% FOUNDING_PROBLEM_CORROBORATION: Religious texts and commentaries from the Heian period onward, as well as modern comparative religion scholars, corroborate the historical need for such a reconciliation and the development of this domain-partitioning solution. The challenge of integrating diverse religious practices remains a live issue in many traditions.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The extractiveness, suppression, and theater ratio are all low because this constraint is a conceptual framework for understanding, rather than a mechanism for material extraction or coercive enforcement. It coordinates understanding and practice, benefiting those who adopt it with clarity. The historical interval (800-1868) reflects the period of its establishment and widespread acceptance before the Meiji Restoration's official separation of Shinto and Buddhism, which challenged the institutional practice but not necessarily the underlying conceptual framework for historical interpretation.
 *
 * PERSPECTIVAL GAP:
 *   While this reading provides a coherent framework, other perspectives (e.g., ontological fusion or pragmatic incoherence) offer alternative interpretations of the same historical phenomenon. The 'domain partition' reading emphasizes functional clarity, while others might focus on deeper metaphysical unity or the absence of a fully rationalized system. The engine's classification will reflect the low extraction and high coordination inherent in this specific interpretive frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Practitioners and scholars are beneficiaries, gaining conceptual clarity and a coherent framework for religious engagement and study. Religious institutions act as agenda-setters, propagating and maintaining this understanding. There are no direct victims, as the framework aims to resolve contradiction rather than impose costs. Secular historians observe and analyze its impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_simultaneous_veneration,
    'Is this constraint a genuine functional distinction, or is it an interpretive construct designed to rationalize an underlying ontological fusion or pragmatic incoherence?',
    'Analysis of primary religious texts and ritual practices to determine if the functional distinction was consistently articulated and applied, or if it served as a post-hoc rationalization for existing practices.',
    'If primarily a rationalization, the constraint''s ''rope'' classification might be re-evaluated towards a ''tangled_rope'' (if it masked extraction) or ''piton'' (if it became an inert, theatrical explanation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_simultaneous_veneration, conceptual, 'This constraint is the `domain_partition_reading` of the `simultaneous_veneration` kernel.').

omega_variable(
    ontological_fusion_alternative,
    'How would the classification of simultaneous veneration change if the `ontological_fusion_reading` (Kami and Buddhas are ontologically identical) were adopted?',
    'Re-evaluate the constraint''s metrics and stakeholder roles under the assumption of ontological identity, focusing on how such a view would impact coordination, extraction, and suppression.',
    'The `ontological_fusion_reading` would likely result in a different constraint type, potentially a ''rope'' or ''mountain'' if the fusion is seen as a natural metaphysical truth, or a ''tangled_rope'' if it enabled specific institutional power structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_fusion_alternative, conceptual, 'Impact of adopting the `ontological_fusion_reading`.').

omega_variable(
    pragmatic_incoherence_alternative,
    'How would the classification of simultaneous veneration change if the `pragmatic_incoherence_reading` (simultaneous veneration was never coherent) were adopted?',
    'Re-evaluate the constraint''s metrics and stakeholder roles under the assumption of fundamental incoherence, focusing on how practitioners managed contradictory beliefs and the role of enforcement in maintaining the practice.',
    'The `pragmatic_incoherence_reading` would likely result in a ''piton'' (if maintained by inertia and theatricality) or ''snare'' (if the incoherence was exploited for extraction by an agenda-setter), as the coordination function would be undermined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pragmatic_incoherence_alternative, conceptual, 'Impact of adopting the `pragmatic_incoherence_reading`.').

omega_variable(
    disagreement_locus,
    'Is the core disagreement about simultaneous veneration located in the functional distinction, ontological identity, or the coherence of practice?',
    'Detailed textual analysis of historical debates and contemporary scholarly arguments to pinpoint the precise point of contention among different interpretive schools.',
    'Identifying the locus of disagreement clarifies which structural elements of the constraint are most contested and informs the choice of appropriate analytical tools for resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_locus, conceptual, 'Locus of disagreement regarding simultaneous veneration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 800, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__domain_partition_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__domain_partition_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__domain_partition_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(simu_tr_t1400, simultaneous_veneration__domain_partition_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement(simu_tr_t1600, simultaneous_veneration__domain_partition_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__domain_partition_reading, theater_ratio, 1868, 0.1).

% Extraction over time
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__domain_partition_reading, base_extractiveness, 800, 0.15).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1000, 0.15).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1200, 0.15).
narrative_ontology:measurement(simu_be_t1400, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1400, 0.15).
narrative_ontology:measurement(simu_be_t1600, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1868, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t800, simultaneous_veneration__domain_partition_reading, suppression_requirement, 800, 0.2).
narrative_ontology:measurement(simu_su_t1000, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1000, 0.2).
narrative_ontology:measurement(simu_su_t1200, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1200, 0.2).
narrative_ontology:measurement(simu_su_t1400, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1400, 0.2).
narrative_ontology:measurement(simu_su_t1600, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1600, 0.2).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1868, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
