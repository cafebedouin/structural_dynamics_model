% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__ontological_fusion_reading, []).

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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Ontological Fusion Reading of Kami-Buddha Simultaneous Veneration
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint represents the 'ontological fusion' reading of
 *   kami-buddha simultaneous veneration in pre-Meiji Japan, primarily
 *   articulated through honji-suijaku theory. It asserts that kami and
 *   buddhas are fundamentally identical, with kami being local manifestations
 *   of universal Buddhist deities. This reading served to integrate
 *   indigenous religious practices into a Buddhist-dominated framework,
 *   consolidating Buddhist institutional power and interpretive authority.
 *   The constraint is classified as a Tangled Rope because it provided a
 *   genuine coordination function (religious coherence) but also involved
 *   significant asymmetric extraction (subsumption of Shinto autonomy and
 *   resources by Buddhist institutions) maintained through active theological
 *   and institutional enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.85).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.75).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Ontological Fusion Reading of Kami-Buddha Simultaneous Veneration").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious_studies/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, 'aeffe76f-591a-4bda-a116-bcab1b90787f').
narrative_ontology:cs_kernel_codification('aeffe76f-591a-4bda-a116-bcab1b90787f', formalized).
narrative_ontology:cs_authority_grounding('aeffe76f-591a-4bda-a116-bcab1b90787f', lineage).
narrative_ontology:cs_interpretation_layer_present('aeffe76f-591a-4bda-a116-bcab1b90787f').
narrative_ontology:cs_reading_relation('aeffe76f-591a-4bda-a116-bcab1b90787f', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('aeffe76f-591a-4bda-a116-bcab1b90787f', simultaneous_veneration__pragmatic_incoherence_reading, forecloses).
narrative_ontology:cs_axiom('aeffe76f-591a-4bda-a116-bcab1b90787f', foundational, kami_are_buddha_manifestations).
narrative_ontology:cs_axiom_status(kami_are_buddha_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('aeffe76f-591a-4bda-a116-bcab1b90787f', kami_are_buddha_manifestations, theological).
narrative_ontology:cs_axiom('aeffe76f-591a-4bda-a116-bcab1b90787f', foundational, buddhas_are_original_ground).
narrative_ontology:cs_axiom_status(buddhas_are_original_ground, holdable).
narrative_ontology:cs_axiom_grounding('aeffe76f-591a-4bda-a116-bcab1b90787f', buddhas_are_original_ground, theological).
narrative_ontology:cs_reference_frame('aeffe76f-591a-4bda-a116-bcab1b90787f', unified_buddhist_shinto_system).
narrative_ontology:cs_drift_state('aeffe76f-591a-4bda-a116-bcab1b90787f', meiji_restoration_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('aeffe76f-591a-4bda-a116-bcab1b90787f', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, honji_suijaku_scholars).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_autonomy_advocates).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, shinto_priests_pre_meiji).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, lay_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promoted and enforced the honji-suijaku theory, asserting the ontological identity of kami and buddhas, with buddhas as the 'original ground' and kami as their 'manifest traces'. This framework allowed Buddhist institutions to absorb indigenous cults and their resources, consolidating their authority and interpretive monopoly over religious truth in Japan.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).

% Experienced the subsumption of local kami traditions under the Buddhist interpretive framework. Their distinct theological claims and ritual practices were re-explained as manifestations of Buddhist deities, diminishing their independent authority and often diverting resources to Buddhist temples. Exit meant abandoning their ancestral traditions or facing institutional marginalization.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_autonomy_advocates, payer,
    powerless, generational, identity_locked, local).

% Often found their roles and shrines integrated into larger Buddhist temple complexes, with their deities reinterpreted through a Buddhist lens. While some adapted and thrived within this syncretic system, others lost autonomy and saw their traditions diluted. Their ability to assert independent kami theology was constrained by the dominant Buddhist interpretive framework.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, shinto_priests_pre_meiji, payer,
    moderate, biographical, constrained, regional).

% Developed and propagated the honji-suijaku theory, gaining intellectual prestige and institutional support. Their careers and academic legitimacy were tied to the acceptance and elaboration of this interpretive framework, which provided a coherent (from their perspective) explanation for religious syncretism.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, honji_suijaku_scholars, beneficiary,
    organized, generational, mobile, national).

% Benefited from a unified religious worldview that reconciled seemingly disparate deities and practices, providing a sense of coherence and reducing cognitive dissonance. They could venerate both kami and buddhas without perceived contradiction, simplifying their religious life. However, their understanding was shaped by the dominant interpretive frame.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, lay_practitioners, beneficiary,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a coherent theological framework for the simultaneous veneration of indigenous kami and Buddhist deities, integrating diverse religious practices and beliefs into a single, unified system across Japan.
% TRANSFER_FUNCTION: Transferred interpretive authority and often material resources (land, offerings, patronage) from indigenous kami cults and Shinto shrines to Buddhist institutions, by asserting the ontological primacy of buddhas.
% ABSENT_VOICES: Pure Shinto revivalists and scholars who sought to re-establish the independent and distinct nature of kami, free from Buddhist influence, were marginalized or suppressed within the dominant religious discourse until the Meiji era. Their arguments for kami autonomy were systematically excluded from mainstream theological debate.
% DISAPPEARANCE_RATIONALE: If the ontological fusion reading of honji-suijaku theory vanished, the entire religious landscape of pre-Meiji Japan would be fundamentally altered. Buddhist institutions would lose a major source of legitimacy and revenue, Shinto traditions would reclaim their distinct identities, and the syncretic practices of lay practitioners would lose their theological grounding, leading to a profound reorganization of religious authority and practice.
% FOUNDING_PROBLEM: The problem of reconciling the widespread veneration of indigenous kami with the growing influence and theological claims of Buddhism, to create a unified religious system that could accommodate both.
% FOUNDING_PROBLEM_CORROBORATION: The problem of reconciling kami and buddhas was largely 'solved' by the widespread acceptance of honji-suijaku theory. However, the Meiji Restoration's forced separation of Shinto and Buddhism (Shinbutsu Bunri) formally dismantled the institutional structures that enforced this fusion, indicating the 'founding problem' as originally conceived is no longer live in its historical context. Historians and comparative religion scholars, from outside the Buddhist hierarchy, corroborate that the institutional problem was resolved, but the underlying theological tension remained contested.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__ontological_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__ontological_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the theory systematically reinterpreted indigenous kami as subordinate manifestations of buddhas, effectively appropriating their spiritual authority and often their material resources. Suppression is also high (0.75) as this interpretive framework was actively promoted and enforced by powerful Buddhist institutions, marginalizing alternative understandings of kami autonomy. The theater ratio is low (0.20) because the theological work of honji-suijaku was genuinely functional in creating a unified religious system, even if that system was highly extractive. The historical interval spans from the early development of honji-suijaku concepts to the Meiji Restoration, which formally ended this syncretic system.
 *
 * PERSPECTIVAL GAP:
 *   The Buddhist institutional hierarchy experienced this as a legitimate and beneficial coordination mechanism, providing a sophisticated theological explanation for religious diversity. Indigenous kami autonomy advocates and Shinto priests, however, experienced it as a form of cultural and religious subjugation, where their traditions were re-framed and their independence eroded. Lay practitioners might have experienced it as a beneficial simplification of their religious life, reducing cognitive dissonance.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy and honji-suijaku scholars are clear beneficiaries, gaining interpretive monopoly, resources, and intellectual prestige. Indigenous kami autonomy advocates and Shinto priests are victims, losing autonomy and resources. Lay practitioners are beneficiaries of the coordination function (coherence) but also indirectly pay through the subsumption of their local traditions. The constraint's active enforcement ensures this asymmetric flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (reconciling kami and buddhas) was arguably 'resolved' by the widespread acceptance of honji-suijaku theory, but the institutional structures that enforced this reading persisted due to the benefits they conferred on the Buddhist hierarchy. The Meiji Restoration's forced separation of Shinto and Buddhism (Shinbutsu Bunri) effectively ended the constraint's active enforcement, revealing its extractive core. This classification prevents mislabeling the long-standing syncretic system as pure coordination by highlighting the active enforcement and asymmetric extraction inherent in the ontological fusion reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_functional_distinction,
    'Is the distinction between kami and buddhas fundamentally ontological (as this reading claims), or primarily functional (as the domain_partition_reading suggests)?',
    'Analysis of pre-honji-suijaku texts and archaeological evidence for independent kami cults, alongside comparative studies of religious syncretism that distinguish between theological fusion and pragmatic coexistence.',
    'If the distinction is primarily functional, this ''ontological fusion'' reading would be reclassified as a Snare, as its coordination story (metaphysical truth) would be revealed as a cover for pure extraction. If the ontological claim holds, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_functional_distinction, conceptual, 'Ambiguity regarding the fundamental nature of kami-buddha relationship.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of kami autonomy primarily structural (institutional power, resource control) or internalized (acceptance of Buddhist interpretive superiority by local practitioners)?',
    'Analysis of local resistance movements, conversion narratives, and the persistence of distinct kami practices despite official syncretism. If resistance was widespread but ineffective, structural suppression is dominant. If local practices genuinely adapted and integrated, internalized suppression played a larger role.',
    'If internalized suppression was dominant, the constraint''s effective suppression was higher than the structural measure suggests, as the interpretive framework became self-enforcing. If structural suppression was primary, the Tangled Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for kami autonomy.').

omega_variable(
    founding_problem_legitimacy,
    'Was the ''founding problem'' of reconciling kami and buddhas a genuine theological challenge, or was it primarily an institutional problem for Buddhist expansion?',
    'Examination of early Buddhist missionary strategies and the historical context of Buddhist institutional growth in Japan, alongside indigenous responses. If the problem was primarily institutional, the ''founding problem'' narrative serves as a legitimizing cover.',
    'If the problem was primarily institutional, the constraint''s ''coordination function'' would be re-evaluated as a justification for institutional expansion, pushing the classification closer to a Snare. If it was a genuine theological problem, the Tangled Rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_legitimacy, conceptual, 'Legitimacy of the constraint''s stated founding problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t700, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 700, 0.1).
narrative_ontology:measurement(simu_tr_t900, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 900, 0.15).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1200, 0.2).
narrative_ontology:measurement(simu_tr_t1500, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1868, 0.2).

% Extraction over time
narrative_ontology:measurement(simu_be_t700, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 700, 0.6).
narrative_ontology:measurement(simu_be_t900, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 900, 0.7).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1200, 0.8).
narrative_ontology:measurement(simu_be_t1500, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1500, 0.85).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1868, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t700, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 700, 0.5).
narrative_ontology:measurement(simu_su_t900, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 900, 0.6).
narrative_ontology:measurement(simu_su_t1200, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1200, 0.7).
narrative_ontology:measurement(simu_su_t1500, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1500, 0.75).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1868, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'simultaneous_veneration' kernel. Other readings include 'domain_partition_reading' and 'pragmatic_incoherence_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
