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
    narrative_ontology:epsilon_provenance/5,
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
 *   veneration in Japanese religion, where kami and buddhas are understood as
 *   functionally distinct entities governing separate domains (this-worldly
 *   prosperity vs. afterlife salvation). Simultaneous veneration is thus seen
 *   as domain-appropriate specialization, not syncretism or confusion. This
 *   reading posits two parallel, non-extractive coordination mechanisms (one
 *   for kami, one for buddhas) that operate in a complementary fashion. The
 *   metrics reflect a low-extraction, low-suppression coordination mechanism,
 *   consistent with a Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.05).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.1).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Simultaneous Veneration: Domain Partition Reading").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious_studies/comparative_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, '44ecf216-4d42-4798-97fb-447759e6ed98').
narrative_ontology:cs_kernel_codification('44ecf216-4d42-4798-97fb-447759e6ed98', implicit).
narrative_ontology:cs_authority_grounding('44ecf216-4d42-4798-97fb-447759e6ed98', practice).
narrative_ontology:cs_interpretation_layer_present('44ecf216-4d42-4798-97fb-447759e6ed98').
narrative_ontology:cs_reading_relation('44ecf216-4d42-4798-97fb-447759e6ed98', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('44ecf216-4d42-4798-97fb-447759e6ed98', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('44ecf216-4d42-4798-97fb-447759e6ed98', foundational, kami_buddha_functional_specialization).
narrative_ontology:cs_axiom_status(kami_buddha_functional_specialization, holdable).
narrative_ontology:cs_axiom_grounding('44ecf216-4d42-4798-97fb-447759e6ed98', kami_buddha_functional_specialization, conventional).
narrative_ontology:cs_axiom('44ecf216-4d42-4798-97fb-447759e6ed98', foundational, this_worldly_afterlife_domain_separation).
narrative_ontology:cs_axiom_status(this_worldly_afterlife_domain_separation, holdable).
narrative_ontology:cs_axiom_grounding('44ecf216-4d42-4798-97fb-447759e6ed98', this_worldly_afterlife_domain_separation, conventional).
narrative_ontology:cs_reference_frame('44ecf216-4d42-4798-97fb-447759e6ed98', pre_meiji_syncretic_practice).
narrative_ontology:cs_drift_state('44ecf216-4d42-4798-97fb-447759e6ed98', contemporary_academic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('44ecf216-4d42-4798-97fb-447759e6ed98', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, practitioners_seeking_holistic_wellbeing).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, religious_institutions_offering_diverse_services).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who seek this-worldly benefits (health, prosperity) from kami and afterlife salvation from buddhas, finding a coherent, specialized system for their spiritual needs.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, practitioners_seeking_holistic_wellbeing, beneficiary,
    moderate, biographical, mobile, local).

% Shinto shrines and Buddhist temples that historically offered complementary services, benefiting from a clear division of spiritual labor and attracting diverse patronage without direct competition.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, religious_institutions_offering_diverse_services, beneficiary,
    organized, generational, constrained, regional).

% Academics who analyze the historical and theological coherence of Japanese religious practices, often seeking to understand the internal logic of syncretic traditions.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, scholars_of_japanese_religion, observer,
    analytical, generational, analytical, global).

% Historical actors who, during the Meiji Restoration, sought to purify Shinto from Buddhist influences, viewing simultaneous veneration as an illegitimate fusion rather than a functional partition. Their perspective is excluded from this reading's internal coherence.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, meiji_era_separatists, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the spiritual practices of individuals and the service offerings of religious institutions by assigning distinct, non-overlapping domains of efficacy to kami (this-worldly) and buddhas (afterlife), preventing conflict and ensuring comprehensive spiritual coverage.
% TRANSFER_FUNCTION: Facilitates the flow of spiritual benefits (prosperity, health, salvation) to practitioners by directing their veneration to the appropriate entity for their specific need, and directs patronage to institutions specializing in those domains.
% ABSENT_VOICES: Meiji-era separatists and modern purists who view any form of simultaneous veneration as an ontological confusion or a historical corruption would object, arguing for strict separation. Their voices are excluded from the framework of domain-appropriate specialization.
% DISAPPEARANCE_RATIONALE: If the understanding of kami and buddhas as functionally distinct entities governing separate domains vanished, practitioners would lose a coherent framework for their spiritual needs, and religious institutions would face direct competition or confusion over their roles, leading to a significant reorganization of religious practice and institutional structure.
% FOUNDING_PROBLEM: How to integrate indigenous Japanese beliefs (kami worship) with imported Buddhism without creating theological conflict or redundancy, while providing comprehensive spiritual services for all aspects of life and death.
% FOUNDING_PROBLEM_CORROBORATION: Scholars of Japanese religion and contemporary practitioners attest that this functional division provided a stable framework for centuries, and its underlying logic continues to inform syncretic practices, even after formal separation. Historical texts and ethnographic studies corroborate the problem's historical and ongoing relevance.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.05) because the system primarily serves to coordinate spiritual needs and institutional offerings without imposing significant costs or suppressing alternatives within its own framework. Suppression is low (0.1) as adherence is largely voluntary, driven by perceived efficacy rather than coercion. Theater ratio is zero, as the functional distinction is genuinely believed and practiced. Accessibility collapse is high (0.8) because once this domain partition is understood, the 'problem' of simultaneous veneration largely dissolves, making alternatives (like strict separation) less appealing for those seeking comprehensive spiritual coverage. Resistance is low (0.05) because this reading provides a coherent and beneficial framework for many practitioners.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of practitioners and institutions operating within this framework, the constraint is a beneficial coordination mechanism. From the perspective of those who reject the domain partition (e.g., Meiji separatists), the entire practice is incoherent or illegitimate, but this reading does not account for that external critique.
 *
 * DIRECTIONALITY LOGIC:
 *   Practitioners seeking holistic wellbeing and religious institutions offering diverse services are beneficiaries, as the constraint provides a clear, functional framework for their spiritual needs and offerings. There are no direct victims within this reading, as the system is seen as mutually beneficial. Excluded voices (like Meiji-era separatists) are those who reject the premise of functional distinction itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_evidence_for_domain_partition,
    'To what extent do historical and ethnographic records consistently support the explicit articulation and widespread understanding of this domain partition among all practitioners, or was it an elite theological construct?',
    'Comprehensive analysis of primary religious texts, popular devotional literature, and archaeological evidence across different social strata and time periods to gauge the pervasiveness and explicitness of the domain partition understanding.',
    'If the partition was primarily an elite construct, the ''rope'' classification might only apply to a subset of practitioners, and for others, the practice might have been sustained by pragmatic incoherence or ontological fusion, shifting the classification for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_evidence_for_domain_partition, empirical, 'Assesses the empirical grounding of the domain partition understanding across different social groups.').

omega_variable(
    coherence_with_ontological_fusion,
    'Does this functional domain partition logically preclude or merely coexist with the ''ontological fusion'' reading (honji-suijaku theory)?',
    'Detailed philosophical and theological analysis of how proponents of each reading reconciled or distinguished their views, examining whether a single coherent system could logically hold both functional distinction and ontological identity.',
    'If the readings are logically incompatible, the ''domain partition'' reading would ''foreclose'' the ''ontological fusion'' reading within a single coherent framework. If they can coexist, the relationship is one of ''coexistence'' or ''influence'', affecting the network structure of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coherence_with_ontological_fusion, conceptual, 'Examines the logical compatibility between functional domain partition and ontological fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__domain_partition_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(simu_tr_t500, simultaneous_veneration__domain_partition_reading, theater_ratio, 500, 0.0).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__domain_partition_reading, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(simu_tr_t1500, simultaneous_veneration__domain_partition_reading, theater_ratio, 1500, 0.0).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__domain_partition_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(simu_be_t500, simultaneous_veneration__domain_partition_reading, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(simu_be_t1500, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1500, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__domain_partition_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(simu_su_t500, simultaneous_veneration__domain_partition_reading, suppression_requirement, 500, 0.1).
narrative_ontology:measurement(simu_su_t1000, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(simu_su_t1500, simultaneous_veneration__domain_partition_reading, suppression_requirement, 1500, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'simultaneous_veneration' kernel. It posits a functional domain partition between kami and buddhas. Sibling readings offer alternative interpretations of the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
