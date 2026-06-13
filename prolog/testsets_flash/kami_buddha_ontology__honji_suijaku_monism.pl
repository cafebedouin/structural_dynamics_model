% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji Suijaku Monism: Kami as Traces of Buddhas
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint describes the 'honji suijaku' (original ground and
 *   manifest traces) theory, a dominant theological framework in pre-modern
 *   Japan that posited kami (indigenous deities) as phenomenal manifestations
 *   (suijaku) of universal Buddhist buddhas/bodhisattvas (honji). It
 *   established a hierarchical monism, integrating Shinto into a
 *   Buddhist-centric cosmology. This is one reading of the
 *   kami_buddha_ontology kernel, emphasizing ontological identity and
 *   Buddhist priority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.2).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.1).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.2).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, mountain).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji Suijaku Monism: Kami as Traces of Buddhas").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:emerges_naturally(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, 'f0c71793-26bd-4fe8-be08-6b62da5b4f63').
narrative_ontology:cs_kernel_codification('f0c71793-26bd-4fe8-be08-6b62da5b4f63', formalized).
narrative_ontology:cs_authority_grounding('f0c71793-26bd-4fe8-be08-6b62da5b4f63', lineage).
narrative_ontology:cs_interpretation_layer_present('f0c71793-26bd-4fe8-be08-6b62da5b4f63').
narrative_ontology:cs_reading_relation('f0c71793-26bd-4fe8-be08-6b62da5b4f63', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('f0c71793-26bd-4fe8-be08-6b62da5b4f63', kami_buddha_ontology__incoherent_bundle, forecloses).
narrative_ontology:cs_axiom('f0c71793-26bd-4fe8-be08-6b62da5b4f63', foundational, buddhist_universal_ground).
narrative_ontology:cs_axiom_status(buddhist_universal_ground, holdable).
narrative_ontology:cs_axiom_grounding('f0c71793-26bd-4fe8-be08-6b62da5b4f63', buddhist_universal_ground, deontological).
narrative_ontology:cs_axiom('f0c71793-26bd-4fe8-be08-6b62da5b4f63', foundational, kami_phenomenal_manifestation).
narrative_ontology:cs_axiom_status(kami_phenomenal_manifestation, holdable).
narrative_ontology:cs_axiom_grounding('f0c71793-26bd-4fe8-be08-6b62da5b4f63', kami_phenomenal_manifestation, conventional).
narrative_ontology:cs_reference_frame('f0c71793-26bd-4fe8-be08-6b62da5b4f63', buddhist_cosmological_supremacy).
narrative_ontology:cs_drift_state('f0c71793-26bd-4fe8-be08-6b62da5b4f63', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('f0c71793-26bd-4fe8-be08-6b62da5b4f63', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_scholars).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, shinto_priests).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, local_kami_worshippers).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, buddhist_universalism).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, ontological_unity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a coherent theological framework that integrates indigenous deities into a universal Buddhist cosmology, providing a basis for intellectual systematization and doctrinal authority.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_scholars, beneficiary,
    institutional, generational, mobile, national).

% Benefit from the legitimization of Buddhist superiority and the integration of local cults, expanding their influence and justifying their role in mediating between the phenomenal and ultimate realities.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_institutions, beneficiary,
    institutional, generational, mobile, national).

% While integrated into the syncretic system, their deities are ontologically subordinated, potentially diminishing the independent authority and unique identity of Shinto practices and institutions. Their identity is tied to the kami they serve.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shinto_priests, payer,
    moderate, generational, identity_locked, local).

% Their traditional objects of worship are reinterpreted as manifestations of Buddhist deities, potentially altering their understanding of local sacredness and requiring adherence to Buddhist rituals for full spiritual efficacy. Their identity is fused with their local traditions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, local_kami_worshippers, payer,
    powerless, biographical, identity_locked, local).

% Would strongly object to the ontological subordination of kami, advocating for the absolute independence and indigenous purity of Shinto. Their voices were historically suppressed during periods of Buddhist dominance and later by state Shinto's own agenda.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, modern_shinto_revivalists, excluded,
    organized, generational, constrained, national).

% Analyze the philosophical coherence and historical development of the honji suijaku theory, evaluating its implications for comparative theology and the nature of religious truth claims.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, philosophers_of_religion, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified theological framework for understanding the relationship between indigenous Japanese deities (kami) and imported Buddhist deities, allowing for the peaceful coexistence and integration of two distinct religious traditions within a single cultural sphere.
% TRANSFER_FUNCTION: Transfers ontological priority and ultimate salvific power from indigenous kami to Buddhist buddhas/bodhisattvas, while integrating kami into a universal cosmology. This legitimizes Buddhist institutions and doctrines as the 'true' ground of reality.
% ABSENT_VOICES: Early Shinto practitioners who might have resisted the ontological subordination of their deities, and later, modern Shinto revivalists who explicitly reject this monistic interpretation in favor of Shinto's absolute independence. Their perspectives were marginalized by the dominant syncretic discourse.
% DISAPPEARANCE_RATIONALE: If the honji suijaku framework vanished, the historical and theological understanding of Japanese religion would be fundamentally altered. The integration of Shinto and Buddhism would collapse, requiring a complete re-evaluation of their relationship, potentially leading to renewed doctrinal conflicts or a radical re-partitioning of religious domains.
% FOUNDING_PROBLEM: The need to reconcile indigenous Japanese religious beliefs (Shinto) with the rapidly spreading and philosophically sophisticated foreign religion (Buddhism) without outright rejecting either, thereby preventing religious conflict and facilitating cultural assimilation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Japanese religion and cultural anthropologists corroborate the historical necessity of such a framework for religious and social cohesion. While its specific interpretation is contested, the underlying problem of religious pluralism and integration remains relevant, even if the power dynamics have shifted.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, ExtMetricName, E),
    domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kami_buddha_ontology__honji_suijaku_monism),
    narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because it represents a deeply embedded, systematized theological truth claim that shaped Japanese religious thought for centuries, presenting itself as an irreducible ontological fact. Its extractiveness (0.2) is low but present, reflecting the subtle ontological subordination of kami and the intellectual labor required to maintain the system. Suppression (0.1) is low, as it was largely accepted, but resistance from alternative views was present. Theater ratio (0.05) is minimal, as the system was genuinely functional in integrating religious practices. Accessibility collapse (0.9) is high because, within this framework, alternative ontological understandings of kami are largely foreclosed. Resistance (0.05) is low because the framework was widely adopted and intellectually robust for its time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Buddhist scholars, this framework is a profound theological insight that resolves religious tensions and reveals a deeper truth (Mountain). From the perspective of Shinto priests, it represents a subtle but real loss of independent ontological status for their deities, even if it enabled coexistence (Tangled Rope/Snare-flavored). The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist scholars and institutions are beneficiaries, as the framework provides a coherent, universalizing theology that elevates Buddhism. Shinto priests and local kami worshippers are payers, as their deities and practices are ontologically subordinated, even while being integrated. Modern Shinto revivalists are excluded, representing a later, explicit rejection of this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_theology,
    'Is the honji suijaku monism a genuine discovery of an underlying ontological truth (natural law), or a constructed theological framework that served specific institutional and political interests?',
    'Comparative analysis with other religious syncretisms and historical evidence of power dynamics between Buddhist and Shinto institutions. If the framework''s adoption correlates strongly with Buddhist institutional dominance, it leans towards constructed.',
    'If constructed, the constraint''s ''emerges_naturally'' claim is false, and its classification would shift from Mountain to a more extractive type (e.g., Tangled Rope), reflecting the beneficiaries and the subtle extraction from Shinto traditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_theology, conceptual, 'Ambiguity between ontological discovery and theological construction.').

omega_variable(
    ontological_subordination_vs_integration,
    'To what extent did the honji suijaku framework genuinely integrate kami into a broader cosmology, versus merely subordinating them to Buddhist deities, thereby extracting their independent spiritual authority?',
    'Analysis of ritual practices and popular piety: if kami continued to be worshipped with independent efficacy and agency, it suggests integration; if their worship became primarily mediated through Buddhist rites, it suggests subordination.',
    'If primarily subordination, the extractiveness metric should be higher, and the constraint would lean more towards a Snare or Tangled Rope, reflecting the cost borne by Shinto traditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_subordination_vs_integration, empirical, 'The degree of ontological subordination versus genuine integration.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the low resistance to honji suijaku monism due to its intellectual coherence and spiritual appeal (structural), or due to the institutional power of Buddhism and the suppression of dissenting Shinto voices (internalized)?',
    'Historical analysis of dissenting movements and the consequences for their proponents. If dissent was actively punished or marginalized, it suggests internalized suppression; if it was merely out-argued, it suggests structural acceptance.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, indicating a more coercive environment for alternative interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for theological dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t700, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 700, 0.02).
narrative_ontology:measurement(kami_tr_t900, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 900, 0.03).
narrative_ontology:measurement(kami_tr_t1200, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1200, 0.05).
narrative_ontology:measurement(kami_tr_t1500, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(kami_tr_t1868, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1868, 0.05).

% Extraction over time
narrative_ontology:measurement(kami_be_t700, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 700, 0.1).
narrative_ontology:measurement(kami_be_t900, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 900, 0.15).
narrative_ontology:measurement(kami_be_t1200, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1200, 0.2).
narrative_ontology:measurement(kami_be_t1500, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1500, 0.2).
narrative_ontology:measurement(kami_be_t1868, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1868, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t700, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 700, 0.05).
narrative_ontology:measurement(kami_su_t900, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 900, 0.08).
narrative_ontology:measurement(kami_su_t1200, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1200, 0.1).
narrative_ontology:measurement(kami_su_t1500, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement(kami_su_t1868, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1868, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kami_buddha_ontology' kernel. It represents the dominant pre-modern syncretic view, where kami are traces of buddhas. It contrasts with 'domain_partition' (separate domains) and 'incoherent_bundle' (no coherent system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
