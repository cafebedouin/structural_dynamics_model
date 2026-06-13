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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Kami and Buddha Ontological Domain Partition
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint describes a specific reading within Japanese religious
 *   history where Kami (Shinto deities) and Buddhas are understood as
 *   ontologically distinct entities, each governing separate functional
 *   domains: Shinto for matters of life, purity, and the living world, and
 *   Buddhism for death, impurity, and the deceased. This 'domain partition'
 *   reading emphasizes complementarity without fusion, allowing for peaceful
 *   coexistence and functional specialization of religious institutions
 *   without requiring a unified theological framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.15).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.25).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.15).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, mountain).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami and Buddha Ontological Domain Partition").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:emerges_naturally(kami_buddha_ontology__domain_partition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, '29a5d76e-d40e-4757-9d72-807070b74277').
narrative_ontology:cs_kernel_codification('29a5d76e-d40e-4757-9d72-807070b74277', implicit).
narrative_ontology:cs_authority_grounding('29a5d76e-d40e-4757-9d72-807070b74277', practice).
narrative_ontology:cs_interpretation_layer_present('29a5d76e-d40e-4757-9d72-807070b74277').
narrative_ontology:cs_reading_relation('29a5d76e-d40e-4757-9d72-807070b74277', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('29a5d76e-d40e-4757-9d72-807070b74277', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('29a5d76e-d40e-4757-9d72-807070b74277', foundational, kami_buddha_ontological_distinction).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_distinction, holdable).
narrative_ontology:cs_axiom_grounding('29a5d76e-d40e-4757-9d72-807070b74277', kami_buddha_ontological_distinction, deontological).
narrative_ontology:cs_axiom('29a5d76e-d40e-4757-9d72-807070b74277', foundational, functional_domain_complementarity).
narrative_ontology:cs_axiom_status(functional_domain_complementarity, holdable).
narrative_ontology:cs_axiom_grounding('29a5d76e-d40e-4757-9d72-807070b74277', functional_domain_complementarity, conventional).
narrative_ontology:cs_reference_frame('29a5d76e-d40e-4757-9d72-807070b74277', harmonious_functional_differentiation).
narrative_ontology:cs_drift_state('29a5d76e-d40e-4757-9d72-807070b74277', contemporary_academic_discourse, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('29a5d76e-d40e-4757-9d72-807070b74277', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_priesthood).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_clergy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, general_populace).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, functional_complementarity_doctrine).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__domain_partition, ontological_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a clear domain of authority over life, purity, and local kami worship, distinct from Buddhist practices. Their professional identity is deeply tied to maintaining this distinction.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_priesthood, beneficiary,
    institutional, generational, identity_locked, national).

% Benefits from a clear domain of authority over death, impurity, and ancestral rites, distinct from Shinto practices. Their institutional role is defined by this functional specialization.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_clergy, beneficiary,
    institutional, generational, identity_locked, national).

% Benefits from a coherent and complementary system for religious practice, providing clear guidance for rituals related to life events (Shinto) and death (Buddhism). Adherence is largely cultural and habitual.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, general_populace, beneficiary,
    moderate, biographical, constrained, local).

% Study the historical and philosophical development of these distinctions, questioning their 'naturalness' and analyzing their social and institutional functions. They are not bound by the ontological claims.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, analytical_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates religious practice by assigning distinct, complementary functional domains to Shinto (life, purity) and Buddhism (death, impurity), preventing overlap and conflict between religious institutions.
% TRANSFER_FUNCTION: Transfers clarity of ritual responsibility and institutional authority to Shinto priests for life-related events and to Buddhist clergy for death-related events, from the general populace seeking religious services.
% ABSENT_VOICES: Proponents of a unified, monistic kami-buddha ontology (e.g., historical 'honji suijaku' theorists) or those who view the entire system as an 'incoherent bundle' of contradictory practices would object, arguing for a different theological or analytical framework. Their voices are often marginalized in contexts where this domain partition is dominant.
% DISAPPEARANCE_RATIONALE: If this clear domain partition vanished, there would be significant confusion and potential conflict over ritual responsibilities, institutional authority, and theological coherence. Religious practices would need to be re-negotiated, and the distinct identities of Shinto and Buddhist institutions would be challenged, leading to a substantial reorganization of Japanese religious life.
% FOUNDING_PROBLEM: The need to integrate or differentiate indigenous Japanese kami worship with the newly introduced Buddhist traditions, avoiding conflict and establishing clear roles for each within society.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing existence of distinct Shinto shrines and Buddhist temples, each serving specific ritual functions, corroborates the continued relevance of managing the relationship between these traditions. While the specific 'domain partition' reading is contested by some scholars, the underlying need for functional coordination remains live, attested by the continued institutional separation and complementary ritual services observed by anthropologists and historians.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, ExtMetricName, E),
    domain_priors:suppression_score(kami_buddha_ontology__domain_partition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kami_buddha_ontology__domain_partition),
    narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because, within this reading, the distinction is treated as an inherent feature of reality, not a human construct. Extractiveness is low (0.15) as the primary function is coordination of religious practice, not rent-seeking. Suppression is low (0.25) because adherence is largely cultural and self-enforcing, with minimal active coercion. Theater ratio is low (0.1) as the distinction genuinely guides practice. Accessibility collapse is high (0.8) because, within this framework, alternatives to this partitioned understanding are largely unthinkable or considered incoherent. Resistance is low (0.05) as this reading is widely accepted in certain historical and contemporary contexts.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Shinto priests and Buddhist clergy, this partition is a natural and beneficial arrangement that clarifies their respective roles and reduces inter-institutional conflict. For an analytical observer, the 'naturalness' of this partition is contestable, potentially revealing it as a culturally constructed framework that, while functional, also serves to maintain institutional boundaries and authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto priests and Buddhist clergy are beneficiaries (d near 0.0) as this reading provides a clear mandate for their respective practices and institutional domains, reducing competition. The general populace (practitioners) are also beneficiaries, gaining a coherent framework for navigating life and death rituals. There are no direct 'victims' in this reading, as the partition is presented as a harmonious, functional division of labor.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain, even with beneficiaries, prevents mislabeling a functional, culturally embedded distinction as pure extraction. The low extractiveness and suppression metrics, combined with the 'emerges_naturally' flag, indicate that its persistence is not primarily due to coercive enforcement or rent-seeking, but rather its perceived ontological truth and functional utility within this specific reading. The FSM trigger (Mountain + beneficiaries) correctly flags the need for an omega variable to probe the 'naturalness' claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_cultural_construct,
    'Is the domain partition between kami and buddhas a genuine ontological distinction (natural law) or a culturally constructed and maintained framework that benefits specific religious institutions?',
    'Comparative analysis with other polytheistic/syncretic traditions lacking such a clear partition; historical analysis of the partition''s emergence and enforcement mechanisms.',
    'If a cultural construct, the constraint''s ''emerges_naturally'' claim is false, reclassifying it from Mountain to a form of Rope or Tangled Rope, with higher effective extraction for the benefiting clergy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_cultural_construct, conceptual, 'Ambiguity between natural ontological distinction and cultural construction.').

omega_variable(
    kami_buddha_ontology_kernel_reading,
    'This constraint is one reading of the ''kami_buddha_ontology'' kernel. What would change if a sibling reading were adopted?',
    'Analysis of historical shifts in dominant interpretations and their institutional consequences.',
    'If ''honji_suijaku_monism'' were adopted, the ontological distinction would collapse into a hierarchical monism, fundamentally altering the relationship between Shinto and Buddhist institutions. If ''incoherent_bundle'' were adopted, the coherence of the entire framework would be challenged, leading to potential reclassification as a Piton or Snare due to lack of genuine coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kami_buddha_ontology_kernel_reading, conceptual, 'Impact of alternative readings of the kami-buddha ontology kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__domain_partition, theater_ratio, 0, 0.05).
narrative_ontology:measurement(kami_tr_t100, kami_buddha_ontology__domain_partition, theater_ratio, 100, 0.08).
narrative_ontology:measurement(kami_tr_t200, kami_buddha_ontology__domain_partition, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__domain_partition, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(kami_be_t100, kami_buddha_ontology__domain_partition, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(kami_be_t200, kami_buddha_ontology__domain_partition, base_extractiveness, 200, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__domain_partition, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(kami_su_t100, kami_buddha_ontology__domain_partition, suppression_requirement, 100, 0.22).
narrative_ontology:measurement(kami_su_t200, kami_buddha_ontology__domain_partition, suppression_requirement, 200, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, shinto_ritual_purity_norms).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, buddhist_funeral_practices).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
