% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__syncretic_fusion_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Shinbutsu Coexistence: Honji Suijaku Syncretic Fusion
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint describes the 'syncretic fusion' reading of
 *   Shinbutsu-shugo, where kami are understood as local manifestations of
 *   universal Buddhist truths (honji suijaku). This reading posits a
 *   coherent, unified ontology that integrated indigenous Japanese beliefs
 *   with Buddhism for over a millennium. It was a dominant theological and
 *   institutional framework, particularly through the establishment of
 *   jinguji (shrine-temple complexes). The constraint is classified as a Rope
 *   because it provided a genuine coordination function for religious
 *   practices and beliefs, with moderate extraction primarily in terms of
 *   doctrinal subordination of Shinto to Buddhist frameworks, rather than
 *   overt coercion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.3).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.4).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Shinbutsu Coexistence: Honji Suijaku Syncretic Fusion").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious_studies/philosophy_of_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'bd1823e0-8f69-42b2-a836-a86c90de8c1d').
narrative_ontology:cs_kernel_codification('bd1823e0-8f69-42b2-a836-a86c90de8c1d', formalized).
narrative_ontology:cs_authority_grounding('bd1823e0-8f69-42b2-a836-a86c90de8c1d', lineage).
narrative_ontology:cs_interpretation_layer_present('bd1823e0-8f69-42b2-a836-a86c90de8c1d').
narrative_ontology:cs_reading_relation('bd1823e0-8f69-42b2-a836-a86c90de8c1d', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('bd1823e0-8f69-42b2-a836-a86c90de8c1d', shinbutsu_coexistence_commitment__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('bd1823e0-8f69-42b2-a836-a86c90de8c1d', foundational, kami_as_buddhist_manifestations).
narrative_ontology:cs_axiom_status(kami_as_buddhist_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('bd1823e0-8f69-42b2-a836-a86c90de8c1d', kami_as_buddhist_manifestations, theological).
narrative_ontology:cs_axiom('bd1823e0-8f69-42b2-a836-a86c90de8c1d', foundational, ontological_unity_of_divine).
narrative_ontology:cs_axiom_status(ontological_unity_of_divine, holdable).
narrative_ontology:cs_axiom_grounding('bd1823e0-8f69-42b2-a836-a86c90de8c1d', ontological_unity_of_divine, deontological).
narrative_ontology:cs_reference_frame('bd1823e0-8f69-42b2-a836-a86c90de8c1d', honji_suijaku_unified_ontology).
narrative_ontology:cs_drift_state('bd1823e0-8f69-42b2-a836-a86c90de8c1d', meiji_restoration_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('bd1823e0-8f69-42b2-a836-a86c90de8c1d', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinto_priests).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_universalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpreted and propagated the honji suijaku doctrine, integrating kami worship into Buddhist frameworks. Benefited from increased influence and resources through the establishment of jinguji (shrine-temple complexes).
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_clergy, agenda_setter,
    institutional, generational, constrained, national).

% Benefited from the ideological coherence and social stability provided by a unified religious system, which legitimized imperial authority through divine connections.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court, beneficiary,
    institutional, generational, constrained, national).

% Found spiritual comfort and practical guidance in the syncretic practices, allowing them to venerate local kami while accessing Buddhist soteriology. Experienced a coherent religious worldview.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_communities, beneficiary,
    moderate, biographical, mobile, local).

% While integrated into the syncretic system, their distinct identity and autonomy were often subordinated to Buddhist institutions. Paid in terms of doctrinal independence and institutional control.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinto_priests, payer,
    moderate, biographical, constrained, local).

% Later observed this system as an obstacle to national unity and sought to dismantle it through shinbutsu bunri (separation of kami and Buddhas), viewing it as an 'incoherent bundle'.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, meiji_restoration_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a coherent theological framework that integrated indigenous kami worship with imported Buddhism, resolving potential conflicts and fostering religious harmony across diverse practices and beliefs in Japan.
% TRANSFER_FUNCTION: Transferred spiritual authority and institutional resources towards Buddhist institutions, which then provided a universalistic interpretive lens for local kami cults. It also transferred a sense of unified spiritual identity to the populace.
% ABSENT_VOICES: Early Shinto purists who might have argued for the absolute independence and supremacy of kami, unmediated by Buddhist thought, were largely marginalized or absorbed into the syncretic system. Their voices were suppressed by the dominant theological narrative.
% DISAPPEARANCE_RATIONALE: If the honji suijaku doctrine and its institutional manifestations (jinguji) had vanished overnight during its peak, the religious landscape of Japan would have been profoundly fragmented. Local kami cults and Buddhist sects would have lacked a unifying framework, leading to widespread theological confusion and potential social unrest as communities struggled to reconcile their spiritual practices.
% FOUNDING_PROBLEM: The challenge of integrating indigenous Japanese kami worship with the universalistic claims of Buddhism, which arrived in Japan with a fully developed cosmology and soteriology, without either tradition completely supplanting the other.
% FOUNDING_PROBLEM_CORROBORATION: The problem was largely 'solved' by the widespread acceptance and institutionalization of honji suijaku for centuries. However, the Meiji government's later forced separation (shinbutsu bunri) indicates that the 'solution' was ultimately contested and dismantled, suggesting the founding problem, as originally framed, is no longer live. Historians and scholars of Japanese religion, from an analytical seat, corroborate that the syncretic system was a dominant feature for centuries but was ultimately a historical construct that could be, and was, undone.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.3) as the system primarily coordinated religious practices and beliefs, but with a clear hierarchical bias towards Buddhist interpretations. Suppression is also moderate (0.4) as alternative interpretations (e.g., pure Shinto) were not violently suppressed but rather absorbed or marginalized within the dominant syncretic framework. Theater ratio is low (0.2) because the system was genuinely functional in providing a coherent religious worldview and institutional structure for centuries. Accessibility collapse is moderate (0.6) as while the syncretic view was dominant, alternative interpretations were not entirely impossible, just less prominent. Resistance is low (0.3) because the system was widely accepted and provided benefits to various stakeholders, with only minor, localized resistance from those who preferred a purer Shinto.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Buddhist clergy, this was a highly effective Rope, providing a universal framework for spiritual truth. From the perspective of Shinto priests, it was a more extractive arrangement, as their traditions were often subordinated. Local communities likely experienced it as a beneficial Rope, offering spiritual coherence. The Meiji government, as an analytical observer, later re-framed it as an 'incoherent bundle' or even a 'snare' that needed dismantling.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist clergy and imperial court were primary beneficiaries, gaining authority and stability. Local communities also benefited from spiritual coherence. Shinto priests, while integrated, bore the cost of doctrinal subordination, making them payers. The Meiji government, as an observer, later became a critical target of the system, leading to its dismantling.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to integrate two distinct religious traditions into a coherent system. This mandate was 'dead' by the Meiji Restoration, as the government then viewed the system as an impediment to national unity rather than a solution to religious fragmentation. The classification as a Rope, rather than a Snare or Piton, reflects that for centuries it genuinely served a coordination function, even if it later became obsolete and was forcibly dismantled. The low theater ratio and moderate extractiveness during its active period support this. The Mandatrophy was resolved not by internal adjustment, but by external political force (shinbutsu bunri).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_coherence_vs_ambiguity,
    'To what extent was honji suijaku a genuinely coherent theological system, versus a pragmatic institutional arrangement maintained through deliberate ambiguity?',
    'Detailed textual analysis of a wider range of primary sources (theological treatises, ritual manuals, local shrine records) across different periods and regions, combined with sociological studies of religious practice.',
    'If found to be more ambiguous and pragmatic, the ''syncretic_fusion_reading'' would shift towards the ''incoherent_bundle_reading'', increasing its extractiveness and suppression metrics as the ''coordination'' function would be revealed as a cover for institutional power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_coherence_vs_ambiguity, conceptual, 'Assesses the true theological coherence of honji suijaku.').

omega_variable(
    shinto_subordination_degree,
    'What was the actual degree of subordination of Shinto traditions and institutions under the honji suijaku framework, and how much autonomy did they retain?',
    'Comparative historical studies of specific jinguji complexes, analyzing resource allocation, ritual authority, and doctrinal development within both Shinto and Buddhist components over time.',
    'If Shinto autonomy was significantly higher than currently understood, the extractiveness and suppression metrics for Shinto priests (payers) would decrease, potentially shifting the overall constraint closer to a pure Rope. If subordination was more complete, it would reinforce the current metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shinto_subordination_degree, empirical, 'Quantifies the power balance within the syncretic system.').

omega_variable(
    meiji_motivation_purity_vs_power,
    'Was the Meiji government''s shinbutsu bunri primarily motivated by a genuine desire for religious purity and national identity, or by a strategic move to consolidate state power and dismantle rival institutions?',
    'Analysis of internal government documents, economic records, and political correspondence from the Meiji era, alongside public pronouncements and religious reforms.',
    'If primarily power-motivated, it would suggest the ''incoherent_bundle_reading'' (which emphasizes institutional power) has stronger historical grounding, and the ''syncretic_fusion_reading'' was a more robust system than the Meiji government portrayed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_motivation_purity_vs_power, empirical, 'Examines the underlying drivers of the Meiji separation of kami and Buddhas.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 700, 0.1).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 900, 0.15).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1200, 0.2).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1500, 0.18).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1868, 0.2).

% Extraction over time
narrative_ontology:measurement(shin_be_t700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 700, 0.2).
narrative_ontology:measurement(shin_be_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 900, 0.25).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1200, 0.3).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1500, 0.28).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1868, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 700, 0.3).
narrative_ontology:measurement(shin_su_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 900, 0.35).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1200, 0.4).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1500, 0.38).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1868, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, japanese_imperial_legitimacy_doctrine).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_sectarian_rivalries).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_coexistence_commitment' kernel. This 'syncretic_fusion_reading' emphasizes ontological unity through honji suijaku, contrasting with the 'domain_partition_reading' (separate domains) and the 'incoherent_bundle_reading' (lack of coherence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
