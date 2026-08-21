% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinto-Buddhism Ontological Partition (Partition Reading)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This constraint is the 'partition_reading' of the
 *   'shinbutsu_ontological_commitment' kernel. It describes the understanding
 *   that Shinto and Buddhism in Japan occupy separate, complementary domains
 *   (e.g., Shinto for life-cycle events and local kami worship; Buddhism for
 *   death, ancestors, and universalistic teachings) without requiring deep
 *   ontological integration. This reading emphasizes functional coexistence
 *   and low doctrinal conflict, allowing both traditions to maintain distinct
 *   identities and practices. Sibling readings include 'syncretic_reading'
 *   (which posits a unified cosmological order) and 'incoherence_reading'
 *   (which views their coexistence as institutionally tolerated incoherence).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.15).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.1).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinto-Buddhism Ontological Partition (Partition Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, '7c1f6730-3497-422f-8a8f-8e4b4ad622fa').
narrative_ontology:cs_kernel_codification('7c1f6730-3497-422f-8a8f-8e4b4ad622fa', implicit).
narrative_ontology:cs_authority_grounding('7c1f6730-3497-422f-8a8f-8e4b4ad622fa', practice).
narrative_ontology:cs_interpretation_layer_present('7c1f6730-3497-422f-8a8f-8e4b4ad622fa').
narrative_ontology:cs_reading_relation('7c1f6730-3497-422f-8a8f-8e4b4ad622fa', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c1f6730-3497-422f-8a8f-8e4b4ad622fa', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('7c1f6730-3497-422f-8a8f-8e4b4ad622fa', foundational, shinto_buddhism_functional_differentiation).
narrative_ontology:cs_axiom_status(shinto_buddhism_functional_differentiation, holdable).
narrative_ontology:cs_axiom_grounding('7c1f6730-3497-422f-8a8f-8e4b4ad622fa', shinto_buddhism_functional_differentiation, conventional).
narrative_ontology:cs_axiom('7c1f6730-3497-422f-8a8f-8e4b4ad622fa', foundational, ontological_autonomy_of_traditions).
narrative_ontology:cs_axiom_status(ontological_autonomy_of_traditions, holdable).
narrative_ontology:cs_axiom_grounding('7c1f6730-3497-422f-8a8f-8e4b4ad622fa', ontological_autonomy_of_traditions, deontological).
narrative_ontology:cs_reference_frame('7c1f6730-3497-422f-8a8f-8e4b4ad622fa', functional_complementarity_framework).
narrative_ontology:cs_drift_state('7c1f6730-3497-422f-8a8f-8e4b4ad622fa', contemporary_religious_studies_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7c1f6730-3497-422f-8a8f-8e4b4ad622fa', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, japanese_society).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, functional_differentiation_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__partition_reading, religious_pluralism_in_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear domain of practice (life-cycle rituals, local kami worship) that minimizes direct competition with Buddhism, allowing for stable institutional development and cultural relevance.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_institutions, beneficiary,
    organized, generational, constrained, national).

% Benefit from a clear domain of practice (death rituals, afterlife concerns, universalistic teachings) that minimizes direct competition with Shinto, allowing for stable institutional development and spiritual guidance.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_institutions, beneficiary,
    organized, generational, constrained, national).

% Benefits from the functional harmony and reduced inter-religious conflict, allowing individuals to participate in both traditions according to their life-stage needs without perceived contradiction or social friction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, japanese_society, beneficiary,
    moderate, generational, constrained, national).

% Analyze the historical and contemporary relationship between Shinto and Buddhism, interpreting the nature of their coexistence and the underlying ontological commitments. Their analysis helps to articulate this 'partition reading'.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, scholars_of_religion, observer,
    analytical, civilizational, analytical, universal).

% Historically and contemporaneously observes and sometimes influences the relationship between Shinto and Buddhism. While the state has at times enforced separation, this reading focuses on the cultural/ontological partition, which the state largely accommodates or reflects in its policies.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, japanese_state, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables two distinct religious traditions, Shinto and Buddhism, to coexist and thrive within Japanese society by functionally partitioning their domains of practice (e.g., Shinto for life-cycle events, Buddhism for death and afterlife) without requiring deep ontological integration or causing inter-religious conflict.
% TRANSFER_FUNCTION: Transfers clarity of purpose, reduced inter-religious friction, and stable institutional roles to religious organizations and practitioners, allowing for a comprehensive spiritual and ritual framework for individuals across their lifespan.
% ABSENT_VOICES: None, as this reading describes a largely functional and accepted arrangement. Any voices that would object to this partition would likely be advocating for a different reading (e.g., full syncretism or strict separation), which are captured in sibling constraints.
% DISAPPEARANCE_RATIONALE: If the understanding of this functional and ontological partition vanished overnight, the distinct roles of Shinto and Buddhist institutions would blur, leading to potential competition, doctrinal confusion, or forced mergers. This would disrupt established religious practices, social rituals, and individual spiritual paths, requiring a significant reorganization of religious life in Japan.
% FOUNDING_PROBLEM: To manage the integration of an imported, universalistic religion (Buddhism) with an indigenous, localized tradition (Shinto) in a way that allowed both to flourish and serve the spiritual needs of the population without constant conflict or the subsumption of one by the other.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from the Nara and Heian periods detailing the initial interactions and accommodations between the two traditions, ethnographic studies of contemporary Japanese religious practices, and analyses by independent scholars of Japanese religion (outside of specific Shinto or Buddhist institutional advocacy) corroborate the ongoing relevance of this problem and its resolution through partition.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.10) reflect this reading's emphasis on a stable, mutually beneficial arrangement that minimizes conflict and coercion. The partition is understood as a cultural accommodation rather than a forced imposition. Theater ratio is low (0.05) because the functional differentiation is genuinely practiced and understood by adherents. Accessibility collapse is moderate (0.40) as it defines distinct paths but doesn't eliminate alternatives within each tradition.
 *
 * PERSPECTIVAL GAP:
 *   This 'partition reading' offers a coherent framework for understanding the long-term coexistence of Shinto and Buddhism. However, other readings (e.g., 'syncretic_reading' or 'incoherence_reading') offer alternative interpretations of the same historical and cultural data, highlighting different aspects of the relationship. The engine's classification of this reading as a 'rope' reflects its functional coordination, while other readings might compute differently based on their emphasis on integration or conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto and Buddhist institutions are beneficiaries, as the partition allows them to maintain their distinct identities and spheres of influence without direct competition. Japanese society as a whole also benefits from the resulting religious harmony and comprehensive ritual framework. There are no identifiable victims under this reading, as it describes a functional, non-extractive coordination.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_functional_partition,
    'Is the observed partition between Shinto and Buddhism a genuine ontological distinction, or primarily a functional and political accommodation that avoids conflict?',
    'Deep philosophical and theological analysis of core doctrines, combined with historical and ethnographic studies of practitioner beliefs and institutional strategies. If the partition is primarily functional, it might be more susceptible to shifts in social or political power.',
    'If primarily functional, the constraint''s stability might be less ''natural'' and more dependent on ongoing social negotiation, potentially shifting its classification towards a more constructed type (e.g., Tangled Rope if extraction is present, or Scaffold if temporary). If genuinely ontological, its ''rope'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_functional_partition, conceptual, 'Distinguishing between an inherent ontological separation and a pragmatic functional division.').

omega_variable(
    historical_syncretism_reconciliation,
    'How does this ''partition reading'' reconcile with historical periods of explicit Shinbutsu-shūgō (syncretism) or forced separation (Haibutsu kishaku)?',
    'Detailed historical analysis of how the ''partition'' understanding evolved, persisted, or was challenged during periods of syncretism or state-enforced separation. This would clarify whether the partition is a continuous underlying structure or a dominant interpretation at specific times.',
    'If the partition reading is shown to be a later interpretive construct that downplays earlier syncretism, its ''rope'' classification might be challenged by a ''tangled_rope'' or ''snare'' reading that emphasizes historical power dynamics. If it represents a resilient underlying structure, its classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_syncretism_reconciliation, empirical, 'Reconciling the partition reading with historical evidence of syncretism and separation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(shin_tr_t25, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 25, 0.05).
narrative_ontology:measurement(shin_tr_t50, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement(shin_tr_t75, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 75, 0.05).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(shin_be_t25, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement(shin_be_t50, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(shin_be_t75, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 75, 0.15).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(shin_su_t25, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 25, 0.1).
narrative_ontology:measurement(shin_su_t50, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(shin_su_t75, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 75, 0.1).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_ontological_commitment' kernel, alongside 'syncretic_reading' and 'incoherence_reading'. Each reading offers a distinct structural interpretation of the relationship between Shinto and Buddhism in Japan.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
