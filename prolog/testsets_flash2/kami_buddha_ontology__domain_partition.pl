% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__domain_partition, []).

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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Kami and Buddha Ontological Domain Partition
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint represents the 'domain partition' reading of the
 *   kami-buddha ontology, where kami and buddhas are understood as
 *   ontologically distinct entities governing separate functional domains:
 *   Shinto for life, purity, and the living; Buddhism for death, impurity,
 *   and the deceased. This reading emphasizes complementarity without fusion,
 *   providing a practical framework for religious life in Japan, particularly
 *   after the Meiji separation of Shinto and Buddhism. It is a specific
 *   interpretation that contrasts with monistic or incoherent views of
 *   Shinbutsu-shugo.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.15).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.2).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.15).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami and Buddha Ontological Domain Partition").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious_studies/philosophy_of_religion/japanese_cultural_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, '7c2fdd2b-8fd4-4176-973f-dceef0c70538').
narrative_ontology:cs_kernel_codification('7c2fdd2b-8fd4-4176-973f-dceef0c70538', formalized).
narrative_ontology:cs_authority_grounding('7c2fdd2b-8fd4-4176-973f-dceef0c70538', lineage).
narrative_ontology:cs_interpretation_layer_present('7c2fdd2b-8fd4-4176-973f-dceef0c70538').
narrative_ontology:cs_reading_relation('7c2fdd2b-8fd4-4176-973f-dceef0c70538', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('7c2fdd2b-8fd4-4176-973f-dceef0c70538', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('7c2fdd2b-8fd4-4176-973f-dceef0c70538', foundational, kami_buddha_ontological_distinction).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_distinction, holdable).
narrative_ontology:cs_axiom_grounding('7c2fdd2b-8fd4-4176-973f-dceef0c70538', kami_buddha_ontological_distinction, deontological).
narrative_ontology:cs_axiom('7c2fdd2b-8fd4-4176-973f-dceef0c70538', foundational, functional_complementarity_without_fusion).
narrative_ontology:cs_axiom_status(functional_complementarity_without_fusion, holdable).
narrative_ontology:cs_axiom_grounding('7c2fdd2b-8fd4-4176-973f-dceef0c70538', functional_complementarity_without_fusion, conventional).
narrative_ontology:cs_reference_frame('7c2fdd2b-8fd4-4176-973f-dceef0c70538', post_meiji_separation_framework).
narrative_ontology:cs_drift_state('7c2fdd2b-8fd4-4176-973f-dceef0c70538', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7c2fdd2b-8fd4-4176-973f-dceef0c70538', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_priesthood).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_clergy).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, japanese_households).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, shinto_purity_doctrine).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, buddhist_impermanence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Shinto rituals, shrines, and doctrines, focusing on life, purity, and the living. Benefits from a clear, distinct domain of authority and practice, which this reading provides. Their role is to maintain the integrity of the kami domain.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_priesthood, agenda_setter,
    institutional, generational, constrained, national).

% Administers Buddhist temples, rites, and teachings, focusing on death, impurity, and the deceased. Benefits from a clear, distinct domain of authority and practice, which this reading provides. Their role is to maintain the integrity of the buddha domain.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_clergy, agenda_setter,
    institutional, generational, constrained, national).

% Navigate religious life by engaging Shinto for life events (births, weddings, festivals) and Buddhism for death-related events (funerals, ancestral rites). This reading provides a clear, practical framework for their religious practice, reducing cognitive dissonance and ritual confusion.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, japanese_households, beneficiary,
    moderate, biographical, constrained, local).

% Analyze the historical and philosophical development of Japanese religious thought. This reading offers a specific interpretive lens for understanding the relationship between Shinto and Buddhism, which they can evaluate against historical evidence and other theoretical models.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, religious_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, non-overlapping division of labor and ontological understanding between Shinto and Buddhism, allowing both traditions to flourish without direct conflict over ultimate authority or domain, and offering a coherent framework for lay practitioners.
% TRANSFER_FUNCTION: Transfers ritual authority and spiritual guidance for life-affirming events to Shinto, and for death-related events to Buddhism, from a unified, syncretic understanding to distinct, complementary systems.
% ABSENT_VOICES: Proponents of a unified, monistic understanding (e.g., Honji Suijaku theorists) or those who view the historical syncretism as fundamentally incoherent would object, arguing that this reading oversimplifies or misrepresents the complex historical reality of Shinbutsu-shugo.
% DISAPPEARANCE_RATIONALE: If this clear domain partition vanished, Japanese religious practice would face significant confusion regarding ritual appropriateness and ontological understanding. Households would lack a clear guide for engaging with kami and buddhas, and the distinct institutional roles of Shinto and Buddhist clergy would blur, leading to potential conflict or a return to more syncretic, less defined practices.
% FOUNDING_PROBLEM: The historical tension and potential for conflict or absorption between indigenous kami worship and imported Buddhism, requiring a framework for their coexistence and functional differentiation.
% FOUNDING_PROBLEM_CORROBORATION: Both Shinto and Buddhist institutions, as well as many lay practitioners, continue to operate largely within this functional division, corroborating its ongoing relevance. Anthropological studies of contemporary Japanese religious practice also attest to this lived reality.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).
:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness is low (0.15) because it primarily serves a coordination function, clarifying roles rather than imposing heavy costs. Suppression is also low (0.2) as adherence is largely voluntary and culturally embedded, rather than coercively enforced. Theater ratio is low (0.1) as the functional distinction is genuinely maintained in practice. Accessibility collapse is high (0.8) because once this framework is adopted, alternative ways of understanding the kami-buddha relationship become less accessible or relevant for practical religious life. Resistance is low (0.05) as this reading is widely accepted by practitioners for its practical utility.
 *
 * PERSPECTIVAL GAP:
 *   While this reading provides clarity and reduces conflict, other perspectives (e.g., those emphasizing historical syncretism or philosophical monism) would view this partition as an artificial imposition or an oversimplification of a more complex reality. The engine's classification of 'rope' reflects its functional utility from the perspective of those who adopt it, but omegas capture the contestability of this framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Both Shinto and Buddhist clergy are beneficiaries and agenda-setters, as this reading grants them clear, distinct domains of authority. Japanese households are beneficiaries, gaining a coherent framework for religious practice. Religious scholars are observers, analyzing the framework without direct benefit or cost from its operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_accuracy_of_partition,
    'To what extent does this ''domain partition'' reading accurately reflect the historical reality of Shinbutsu-shugo, as opposed to being a post-Meiji interpretive construct?',
    'Detailed historical and archaeological research into pre-Meiji religious practices and textual analysis of medieval Shinbutsu-shugo theories to identify the prevalence and nature of domain separation versus fusion.',
    'If largely a post-Meiji construct, the ''naturalness'' of this partition is reduced, potentially reclassifying it as a more actively enforced ''tangled_rope'' or ''snare'' that served political ends. If historically robust, its ''rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_accuracy_of_partition, empirical, 'Assesses whether the domain partition is an accurate historical description or a later interpretive imposition.').

omega_variable(
    ontological_vs_functional_distinction,
    'Is the distinction between kami and buddhas in this reading primarily ontological (they are fundamentally different kinds of beings) or primarily functional (they serve different roles, but their ultimate nature might be unified)?',
    'Philosophical analysis of key Shinto and Buddhist theological texts, examining explicit statements on the nature of kami and buddhas, and the implications of their interaction.',
    'If purely functional, the ''domain partition'' might be a ''scaffold'' for practical coordination, rather than a fundamental ''rope'' reflecting an underlying reality. If ontological, the ''rope'' classification is more robust, reflecting a deeper structural truth within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_vs_functional_distinction, conceptual, 'Clarifies whether the partition is a deep ontological claim or a pragmatic functional arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t1868, kami_buddha_ontology__domain_partition, theater_ratio, 1868, 0.05).
narrative_ontology:measurement(kami_tr_t1900, kami_buddha_ontology__domain_partition, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(kami_tr_t1945, kami_buddha_ontology__domain_partition, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(kami_tr_t1980, kami_buddha_ontology__domain_partition, theater_ratio, 1980, 0.09).
narrative_ontology:measurement(kami_tr_t2024, kami_buddha_ontology__domain_partition, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(kami_be_t1868, kami_buddha_ontology__domain_partition, base_extractiveness, 1868, 0.1).
narrative_ontology:measurement(kami_be_t1900, kami_buddha_ontology__domain_partition, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement(kami_be_t1945, kami_buddha_ontology__domain_partition, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(kami_be_t1980, kami_buddha_ontology__domain_partition, base_extractiveness, 1980, 0.14).
narrative_ontology:measurement(kami_be_t2024, kami_buddha_ontology__domain_partition, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t1868, kami_buddha_ontology__domain_partition, suppression_requirement, 1868, 0.3).
narrative_ontology:measurement(kami_su_t1900, kami_buddha_ontology__domain_partition, suppression_requirement, 1900, 0.25).
narrative_ontology:measurement(kami_su_t1945, kami_buddha_ontology__domain_partition, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(kami_su_t1980, kami_buddha_ontology__domain_partition, suppression_requirement, 1980, 0.18).
narrative_ontology:measurement(kami_su_t2024, kami_buddha_ontology__domain_partition, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, incoherent_bundle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
