% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__syncretic_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-Suijaku Metaphysics (Syncretic Reading)
 *   domain: religious/historical/ontological
 *
 * SUMMARY:
 *   This constraint describes the 'syncretic reading' of shinbutsu-shugo,
 *   where Kami and buddhas are understood as aspects of one unified
 *   cosmological order under honji-suijaku metaphysics. This framework,
 *   dominant for centuries in Japan, integrated indigenous Shinto deities
 *   into a Buddhist hierarchy, often portraying kami as local manifestations
 *   (suijaku) of universal Buddhist archetypes (honji). The constraint is
 *   claimed as a Tangled Rope because, while it provided a coherent religious
 *   framework (coordination), it also involved significant asymmetric
 *   extraction and suppression of Shinto autonomy for the benefit of Buddhist
 *   institutions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.78).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.82).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Metaphysics (Syncretic Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious/historical/ontological").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '415b7d04-5ccb-40cd-b832-7407c92e86f9').
narrative_ontology:cs_kernel_codification('415b7d04-5ccb-40cd-b832-7407c92e86f9', formalized).
narrative_ontology:cs_authority_grounding('415b7d04-5ccb-40cd-b832-7407c92e86f9', lineage).
narrative_ontology:cs_interpretation_layer_present('415b7d04-5ccb-40cd-b832-7407c92e86f9').
narrative_ontology:cs_reading_relation('415b7d04-5ccb-40cd-b832-7407c92e86f9', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('415b7d04-5ccb-40cd-b832-7407c92e86f9', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('415b7d04-5ccb-40cd-b832-7407c92e86f9', foundational, kami_are_buddha_manifestations).
narrative_ontology:cs_axiom_status(kami_are_buddha_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('415b7d04-5ccb-40cd-b832-7407c92e86f9', kami_are_buddha_manifestations, theological).
narrative_ontology:cs_axiom('415b7d04-5ccb-40cd-b832-7407c92e86f9', foundational, cosmological_unity_is_real).
narrative_ontology:cs_axiom_status(cosmological_unity_is_real, holdable).
narrative_ontology:cs_axiom_grounding('415b7d04-5ccb-40cd-b832-7407c92e86f9', cosmological_unity_is_real, conventional).
narrative_ontology:cs_reference_frame('415b7d04-5ccb-40cd-b832-7407c92e86f9', honji_suijaku_unity_framework).
narrative_ontology:cs_drift_state('415b7d04-5ccb-40cd-b832-7407c92e86f9', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('415b7d04-5ccb-40cd-b832-7407c92e86f9', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, imperial_court).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shinto_shrines_and_priests).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, local_kami_cults).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, buddhist_supremacy_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, cosmological_unity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the theological framework that positioned buddhas as the 'true' forms and kami as their manifestations, granting them authority over Shinto sites and practices, and integrating Shinto deities into Buddhist cosmology. They actively maintained and propagated this metaphysics.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Their autonomy was suppressed, their deities reinterpreted as subordinate manifestations, and their practices often absorbed or controlled by Buddhist temples. While some integration was voluntary, the dominant framework imposed a hierarchical reinterpretation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shinto_shrines_and_priests, payer,
    moderate, biographical, constrained, regional).

% Benefited from a unified religious framework that could be more easily managed and leveraged for political legitimacy and social cohesion, reducing potential religious conflicts.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, imperial_court, beneficiary,
    institutional, generational, mobile, national).

% Their indigenous beliefs and practices were reinterpreted and often subsumed within the honji-suijaku framework, losing their original meaning and independent authority. For many, their identity was fused with these local traditions, making 'exit' unthinkable.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, local_kami_cults, payer,
    powerless, immediate, identity_locked, local).

% Analyze the historical, philosophical, and sociological implications of honji-suijaku metaphysics, often from a modern, secular perspective, seeking to understand its internal coherence and external effects.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, scholarly_interpreters, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutions).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__syncretic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a unified religious framework for a diverse population, integrating local kami worship into a broader Buddhist cosmology, reducing inter-religious conflict and facilitating state control over religious institutions.
% TRANSFER_FUNCTION: Transferred spiritual authority, institutional control, and material resources from Shinto traditions to Buddhist institutions, while reinterpreting Shinto deities within a Buddhist hierarchical structure.
% ABSENT_VOICES: Pure Shinto adherents who rejected Buddhist interpretations, or those who maintained distinct local kami traditions without syncretic reinterpretation. Their voices were marginalized or absorbed into the dominant framework.
% DISAPPEARANCE_RATIONALE: If honji-suijaku metaphysics and its enforcement vanished overnight, it would lead to a re-emergence of distinct Shinto and Buddhist identities, a re-evaluation of historical religious sites, and a reordering of religious authority, as was historically observed during the Meiji separation of Kami and Buddhas.
% FOUNDING_PROBLEM: The need to integrate indigenous Japanese kami worship with the newly introduced, sophisticated Buddhist cosmology, which offered a comprehensive worldview and soteriology, to create a coherent religious landscape.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts from both Buddhist and Shinto traditions (though interpreted differently), and modern religious studies scholars, corroborate the historical problem of religious integration. However, the specific 'unified cosmological order' solution is contested by other readings of shinbutsu-shugo.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the transfer of authority and resources from Shinto to Buddhist institutions. Suppression (0.82) was necessary to maintain the hierarchical interpretation and prevent the re-emergence of independent Shinto identities. The theater ratio (0.45) indicates that while the metaphysical framework had genuine intellectual and spiritual content, a significant portion of its maintenance involved performative aspects to reinforce Buddhist dominance. Accessibility collapse is high (0.75) because alternative, non-syncretic religious paths were structurally difficult to pursue. Resistance (0.55) was present from local traditions but largely overcome by the dominant framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Buddhist institutions, honji-suijaku was a sophisticated theological solution to religious integration, a genuine Rope. From the perspective of many Shinto practitioners, it was an imposed framework that diminished their traditions, operating as a Snare. The engine's computation of a Tangled Rope reflects the hybrid nature of this historical arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutions, as the primary interpreters and enforcers of honji-suijaku, were clear beneficiaries and agenda-setters. The Imperial Court also benefited from the religious unity. Shinto shrines, priests, and local kami cults were the targets, experiencing suppression of their autonomy and reinterpretation of their traditions. Their exit options were constrained or identity-locked, as their spiritual and social roles were deeply intertwined with the syncretic system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_unity_vs_institutional_construct,
    'Was the honji-suijaku unified cosmological order a genuine theological insight, or primarily an institutional construct designed to consolidate Buddhist power?',
    'Comparative theological analysis of pre-syncretic texts and post-syncretic doctrinal developments, alongside sociological studies of institutional power dynamics in medieval Japan.',
    'If primarily an institutional construct, the extractiveness and suppression metrics are more robustly justified; if a genuine theological insight, the coordination function is stronger, potentially shifting the classification closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_unity_vs_institutional_construct, conceptual, 'Ambiguity between theological truth and institutional power in honji-suijaku.').

omega_variable(
    shinto_autonomy_suppression_degree,
    'To what extent was Shinto autonomy truly suppressed versus willingly integrated into the syncretic framework?',
    'Detailed historical analysis of local shrine records, regional resistance movements, and the agency of individual Shinto priests in adapting or resisting Buddhist interpretations.',
    'If integration was largely voluntary, the suppression metric would be lower, and the constraint would lean more towards a Rope; if suppression was widespread and coercive, the Snare-like aspects are amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shinto_autonomy_suppression_degree, empirical, 'Degree of Shinto autonomy suppression vs. voluntary integration.').

omega_variable(
    kernel_reading_accuracy,
    'Is the ''syncretic_reading'' the most accurate interpretation of shinbutsu-shugo, or do the ''partition_reading'' or ''incoherence_reading'' better describe the historical reality?',
    'Further interdisciplinary research combining textual analysis, archaeological evidence, and sociological theory to reconstruct the lived religious experience across different regions and social strata in pre-Meiji Japan.',
    'If a sibling reading is found to be more accurate, this constraint''s classification would be superseded by that of the more accurate reading, potentially altering the perceived extractiveness and coordination functions of shinbutsu-shugo.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_accuracy, conceptual, 'Accuracy of the syncretic reading compared to sibling interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t700, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 700, 0.3).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 900, 0.35).
narrative_ontology:measurement(shin_tr_t1100, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1100, 0.4).
narrative_ontology:measurement(shin_tr_t1300, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1300, 0.42).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1500, 0.44).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1868, 0.45).

% Extraction over time
narrative_ontology:measurement(shin_be_t700, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 700, 0.6).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 900, 0.68).
narrative_ontology:measurement(shin_be_t1100, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1100, 0.72).
narrative_ontology:measurement(shin_be_t1300, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1300, 0.75).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1500, 0.77).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1868, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t700, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 700, 0.65).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 900, 0.7).
narrative_ontology:measurement(shin_su_t1100, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1100, 0.75).
narrative_ontology:measurement(shin_su_t1300, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1300, 0.78).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1500, 0.8).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1868, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_ontological_commitment' kernel, each representing a distinct structural claim about the relationship between Kami and Buddhas in pre-Meiji Japan.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
