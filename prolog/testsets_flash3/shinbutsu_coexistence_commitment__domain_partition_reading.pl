% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Shinbutsu Coexistence: Domain Partition Reading
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint describes the historical understanding of Shinbutsu-shugo
 *   (the syncretism of Shinto and Buddhism in Japan) as a functional division
 *   of labor, where Kami govern life and purity, and Buddhas govern death and
 *   the afterlife. This reading emphasizes parallel domains rather than deep
 *   ontological fusion. It allowed both religious systems to coexist and
 *   thrive, providing comprehensive spiritual services to the Japanese
 *   populace without requiring strict theological reconciliation. The
 *   constraint is claimed as a Rope due to its genuine coordination function
 *   and low extraction, reflecting a stable, mutually beneficial arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.2).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.1).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Shinbutsu Coexistence: Domain Partition Reading").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious_studies/philosophy_of_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, 'bb04089b-7037-427d-8079-471446761471').
narrative_ontology:cs_kernel_codification('bb04089b-7037-427d-8079-471446761471', implicit).
narrative_ontology:cs_authority_grounding('bb04089b-7037-427d-8079-471446761471', practice).
narrative_ontology:cs_interpretation_layer_present('bb04089b-7037-427d-8079-471446761471').
narrative_ontology:cs_reading_relation('bb04089b-7037-427d-8079-471446761471', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb04089b-7037-427d-8079-471446761471', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('bb04089b-7037-427d-8079-471446761471', foundational, functional_complementarity_is_sufficient).
narrative_ontology:cs_axiom_status(functional_complementarity_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('bb04089b-7037-427d-8079-471446761471', functional_complementarity_is_sufficient, conventional).
narrative_ontology:cs_axiom('bb04089b-7037-427d-8079-471446761471', foundational, ontological_unification_is_not_required).
narrative_ontology:cs_axiom_status(ontological_unification_is_not_required, holdable).
narrative_ontology:cs_axiom_grounding('bb04089b-7037-427d-8079-471446761471', ontological_unification_is_not_required, deontological).
narrative_ontology:cs_reference_frame('bb04089b-7037-427d-8079-471446761471', popular_religious_practice_as_normative).
narrative_ontology:cs_drift_state('bb04089b-7037-427d-8079-471446761471', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('bb04089b-7037-427d-8079-471446761471', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, local_kami_shrines).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temples).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, japanese_populace).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, functional_pluralism_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, cultural_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain their distinct ritual practices and receive patronage for life-cycle events, agricultural rites, and local protection, without needing to reconcile their theology with Buddhist doctrines. They benefit from a clear division of labor.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, local_kami_shrines, beneficiary,
    organized, generational, constrained, local).

% Focus on death rites, ancestral veneration, and salvation, receiving patronage for these services. They benefit from not having to compete with Kami worship in areas of life and purity, and from a stable, complementary religious landscape.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, buddhist_temples, beneficiary,
    organized, generational, constrained, local).

% Can engage with both traditions as needed for different life events (e.g., Shinto wedding, Buddhist funeral) without perceiving a contradiction or requiring theological consistency. They benefit from a flexible, comprehensive spiritual framework.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, japanese_populace, beneficiary,
    moderate, biographical, mobile, national).

% Analyze and interpret the historical and doctrinal relationship between Shinto and Buddhism. From this seat, the domain partition is an observed structural feature of the religious landscape, allowing for functional coexistence without deep ontological fusion.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, theologians_and_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious landscape by assigning distinct, complementary domains of influence to Kami and Buddhist deities, preventing direct competition and allowing both traditions to flourish side-by-side.
% TRANSFER_FUNCTION: Transfers spiritual services and ritual authority for specific life domains (e.g., birth/life to Shinto, death/afterlife to Buddhism) to the respective institutions, ensuring comprehensive coverage for the populace.
% ABSENT_VOICES: Strict monotheists or philosophical purists might object to the lack of ontological unification, arguing for a single, coherent theological system. However, their voices are largely absent from the historical Japanese religious discourse that shaped this coexistence.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished, the distinct roles of shrines and temples would blur, leading to competition, confusion in ritual practice, and a loss of the complementary spiritual services that the populace currently relies upon. The religious landscape would need to fundamentally reorganize.
% FOUNDING_PROBLEM: How to integrate or coexist with an imported religion (Buddhism) without displacing indigenous beliefs (Shinto), ensuring social harmony and comprehensive spiritual coverage for all aspects of life and death.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Japanese religion and cultural anthropologists attest that the functional coexistence and domain partitioning were effective solutions to the historical challenge of religious pluralism, and that the underlying need for comprehensive spiritual services remains.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.2) because the arrangement primarily facilitates coexistence and provides complementary services, rather than extracting resources from one party to benefit another. Suppression is also low (0.1) as the system was largely maintained by popular practice and mutual benefit, not active coercion. Theater ratio is negligible (0.05) as the functional division was genuinely operative. Accessibility collapse is moderate (0.7) because while alternatives to this dual system existed (e.g., exclusive adherence to one tradition), the cultural norm strongly favored the partitioned approach. Resistance is very low (0.05) as the system was widely accepted and beneficial.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the local shrines and temples, the domain partition was a practical and beneficial arrangement, allowing them to maintain their distinct identities and patronage. For the populace, it offered a flexible spiritual framework. Theologians might perceive a lack of philosophical rigor, but this reading emphasizes the functional, rather than the doctrinal, aspect of coexistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Local Kami shrines and Buddhist temples are beneficiaries, as the constraint clearly delineates their spheres of influence and ensures their continued relevance and patronage. The Japanese populace is also a beneficiary, gaining access to a comprehensive and flexible spiritual system. There are no clear victims, as the arrangement was largely consensual and mutually advantageous. Theologians and scholars act as observers, analyzing the structure without being directly subject to its benefits or costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Rope prevents mislabeling a genuinely coordinative and low-extraction arrangement as a Snare or Tangled Rope. The constraint's mandate (to enable coexistence and comprehensive spiritual services) remained live throughout the interval, and its functional benefits were clear to its participants. There is no evidence of mandatrophy or significant extraction accumulation prior to the Meiji Restoration's political intervention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_partition_vs_fusion_degree,
    'To what extent was the ''domain partition'' a genuine structural reality, versus a simplified interpretation of a more deeply fused or ambiguous syncretism?',
    'Detailed analysis of local ritual practices and popular beliefs across different regions and time periods, examining the degree of intermingling versus clear separation in practice.',
    'If practices show more fusion than partition, the extractiveness and suppression might be higher (as the ''partition'' would be a rhetorical cover for a more complex, potentially extractive, underlying structure). If the partition is robust, the Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_vs_fusion_degree, empirical, 'Ambiguity in the degree of actual domain separation versus fusion in Shinbutsu-shugo.').

omega_variable(
    popular_practice_as_authority_legitimacy,
    'Is ''popular practice'' a legitimate grounding for this constraint''s authority, or does it mask a lack of formal theological coherence that would be considered a weakness by other religious traditions?',
    'Comparative religious studies analysis of different models of religious authority (e.g., scriptural, hierarchical, experiential, popular) and their implications for internal consistency and external critique.',
    'If popular practice is deemed insufficient, the constraint''s stability might be re-evaluated as more fragile or ''incoherent'' from a doctrinal perspective, potentially shifting its classification towards a Piton or even an Incoherent Bundle reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_practice_as_authority_legitimacy, conceptual, 'The conceptual legitimacy of popular practice as the primary authority for the domain partition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t700, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 700, 0.05).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 900, 0.05).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1200, 0.05).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1868, 0.05).

% Extraction over time
narrative_ontology:measurement(shin_be_t700, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 700, 0.18).
narrative_ontology:measurement(shin_be_t900, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 900, 0.2).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1200, 0.22).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1500, 0.21).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1868, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t700, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 700, 0.08).
narrative_ontology:measurement(shin_su_t900, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 900, 0.1).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1200, 0.1).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1500, 0.09).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 1868, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_coexistence_commitment' kernel. This 'domain_partition_reading' emphasizes functional separation, while the 'syncretic_fusion_reading' posits ontological unity (honji suijaku) and the 'incoherent_bundle_reading' argues for a lack of underlying coherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
