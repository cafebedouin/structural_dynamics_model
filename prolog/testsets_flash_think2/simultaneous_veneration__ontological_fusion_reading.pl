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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Ontological Fusion Doctrine
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint represents the 'ontological fusion' reading of the
 *   historical phenomenon of simultaneous veneration in Japan, specifically
 *   through the lens of honji-suijaku theory. This theory asserted that
 *   indigenous kami (deities) were local manifestations (suijaku) of
 *   universal Buddhist figures (honji), thereby integrating Shinto deities
 *   into the Buddhist pantheon. This reading emphasizes the metaphysical
 *   claim of identity and the institutional power dynamics it enabled. The
 *   constraint is claimed as a Tangled Rope because it provided a
 *   coordination function (religious synthesis) but with significant
 *   asymmetric extraction of interpretive authority and spiritual capital by
 *   the Buddhist hierarchy.
 *
 * KEY AGENTS:
 *   - buddhist_institutional_hierarchy: Primary beneficiary/agenda_setter (institutional/arbitrage)
 *   - indigenous_kami_traditions: Primary target/payer (powerless/identity_locked)
 *   - local_shinto_priests: Secondary target/payer (moderate/constrained)
 *   - japanese_state_authorities: Observer (institutional/analytical)
 *   - comparative_religion_scholars: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.85).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.9).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion Doctrine").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious_studies/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, '818cf79d-1edb-441d-9ac9-5b00bda0c9ee').
narrative_ontology:cs_kernel_codification('818cf79d-1edb-441d-9ac9-5b00bda0c9ee', formalized).
narrative_ontology:cs_authority_grounding('818cf79d-1edb-441d-9ac9-5b00bda0c9ee', lineage).
narrative_ontology:cs_interpretation_layer_present('818cf79d-1edb-441d-9ac9-5b00bda0c9ee').
narrative_ontology:cs_reading_relation('818cf79d-1edb-441d-9ac9-5b00bda0c9ee', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('818cf79d-1edb-441d-9ac9-5b00bda0c9ee', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('818cf79d-1edb-441d-9ac9-5b00bda0c9ee', foundational, kami_are_buddhist_manifestations).
narrative_ontology:cs_axiom_status(kami_are_buddhist_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('818cf79d-1edb-441d-9ac9-5b00bda0c9ee', kami_are_buddhist_manifestations, theological).
narrative_ontology:cs_axiom('818cf79d-1edb-441d-9ac9-5b00bda0c9ee', secondary, buddhist_interpretive_supremacy).
narrative_ontology:cs_axiom_status(buddhist_interpretive_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('818cf79d-1edb-441d-9ac9-5b00bda0c9ee', buddhist_interpretive_supremacy, conventional).
narrative_ontology:cs_reference_frame('818cf79d-1edb-441d-9ac9-5b00bda0c9ee', buddhist_syncretic_hegemony).
narrative_ontology:cs_drift_state('818cf79d-1edb-441d-9ac9-5b00bda0c9ee', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('818cf79d-1edb-441d-9ac9-5b00bda0c9ee', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_traditions).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, local_shinto_priests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developed, propagated, and enforced the honji-suijaku doctrine, asserting that kami are manifestations of buddhas. This provided a theological framework that integrated indigenous worship into a Buddhist-centric worldview, consolidating their interpretive authority and spiritual capital.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).

% Experienced their distinct deities and practices being subsumed and reinterpreted as secondary manifestations of Buddhist figures. Their autonomy and unique theological identity were diminished, though local veneration often continued under the new framework.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_traditions, payer,
    powerless, generational, identity_locked, local).

% Continued to administer local kami worship but often had to do so within the interpretive and institutional framework established by the honji-suijaku theory. Their traditional authority was constrained by the overarching Buddhist theological claims.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, local_shinto_priests, payer,
    moderate, biographical, constrained, local).

% Historically, sometimes supported the syncretic framework for social cohesion, and at other times (notably the Meiji Restoration) actively dismantled it, demonstrating the political contingency of the doctrine's enforcement.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, japanese_state_authorities, observer,
    institutional, generational, analytical, national).

% Analyze the historical development, theological implications, and cultural impact of honji-suijaku theory, often highlighting its role in power dynamics between religious institutions.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, comparative_religion_scholars, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a coherent theological framework that reconciled the pre-existing indigenous kami worship with the newly introduced Buddhist doctrines, enabling simultaneous veneration and reducing potential religious conflict.
% TRANSFER_FUNCTION: Transfers interpretive authority, spiritual capital, and institutional dominance from autonomous indigenous kami traditions to the Buddhist institutional hierarchy, by asserting the ontological primacy of Buddhist deities.
% ABSENT_VOICES: Indigenous kami practitioners and local Shinto priests whose distinct traditions were subsumed or reinterpreted. Their voices, if fully present, would have challenged the ontological fusion and asserted the independent divinity of kami.
% DISAPPEARANCE_RATIONALE: If the honji-suijaku doctrine and its enforcement had vanished overnight before the Meiji Restoration, the religious landscape of Japan would have developed along fundamentally different lines, likely with a more distinct and autonomous Shinto tradition. The Meiji separation of kami and buddhas, though enforced by the state, demonstrated the profound impact of dismantling this fusion.
% FOUNDING_PROBLEM: The need to integrate and reconcile the established indigenous kami worship with the newly introduced and rapidly spreading Buddhist doctrines in Japan, to create a unified religious and social order.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts from both Buddhist and Shinto scholars (pre-Meiji) attest to the theological and social pressures for reconciliation. Post-Meiji historians and religious studies scholars corroborate the problem's historical context and its eventual formal 'resolution' by state decree, which dismantled the state-sanctioned fusion.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.85) because the doctrine fundamentally re-framed and subsumed an entire indigenous religious tradition, transferring its spiritual authority to the Buddhist framework. Suppression is also very high (0.90) as alternative, independent interpretations of kami were actively discouraged or reinterpreted, and the institutional power of Buddhist temples grew to encompass many Shinto shrines. Theater ratio is moderate (0.40): while genuine theological and philosophical work went into developing honji-suijaku, a significant portion of its maintenance involved performative assertions of Buddhist supremacy and the suppression of independent kami narratives. The metrics show a gradual increase in extractiveness and suppression over the long interval, reflecting the hardening and institutionalization of the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Buddhist institutional hierarchy, honji-suijaku was a natural and benevolent synthesis, a sophisticated theological solution to religious diversity. From the perspective of indigenous kami traditions and local Shinto priests, it represented a form of spiritual colonization and the diminishment of their unique identity and autonomy. The engine's per-seat classification would highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy is the clear beneficiary, gaining interpretive monopoly and expanding its influence. Indigenous kami traditions and local Shinto priests are the primary targets, as their distinct identities and autonomy were subsumed. Japanese state authorities acted as an observer, sometimes supporting the fusion for social order, sometimes dismantling it for political reasons. Comparative religion scholars observe and analyze the historical dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's initial mandate was to reconcile and unify disparate religious practices. However, over time, it evolved into a mechanism for the Buddhist hierarchy to assert and maintain an interpretive monopoly, extracting spiritual and institutional capital. The founding problem (reconciliation) became 'dead' in the sense that the solution itself became a source of extraction, persisting through institutional inertia and suppression of alternatives, until its formal repudiation by the Meiji state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''ontological_fusion_reading'' of the ''simultaneous_veneration'' kernel, or does it conflate elements of sibling readings?',
    'Detailed textual analysis of primary sources from the period, focusing on explicit theological claims versus pragmatic accommodations or observed incoherence.',
    'If conflated, the extractiveness and suppression metrics might be misattributed, requiring decomposition into more granular constraints reflecting distinct claims. If accurate, it strengthens the analysis of this specific interpretive stance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensures the constraint strictly adheres to the specified kernel reading.').

omega_variable(
    degree_of_genuine_synthesis_vs_dominance,
    'To what extent did honji-suijaku theory represent a genuine, mutually enriching synthesis of religious traditions, versus primarily serving as a theological justification for Buddhist institutional dominance?',
    'Archaeological evidence of shrine-temple complexes, analysis of local folk practices, and comparative studies of religious syncretism to identify instances of reciprocal influence versus one-sided subsumption.',
    'If synthesis was more genuine, the base extractiveness and suppression might be lower, suggesting a more ''rope-like'' function. If dominance was primary, the current high extraction and suppression are further corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_genuine_synthesis_vs_dominance, empirical, 'Distinguishes between genuine religious synthesis and theological justification for power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 800, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 800, 0.2).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1000, 0.25).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1200, 0.3).
narrative_ontology:measurement(simu_tr_t1400, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1400, 0.35).
narrative_ontology:measurement(simu_tr_t1600, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1600, 0.38).
narrative_ontology:measurement(simu_tr_t1868, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1868, 0.4).

% Extraction over time
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 800, 0.6).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1000, 0.7).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1200, 0.78).
narrative_ontology:measurement(simu_be_t1400, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1400, 0.82).
narrative_ontology:measurement(simu_be_t1600, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1600, 0.84).
narrative_ontology:measurement(simu_be_t1868, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1868, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t800, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 800, 0.65).
narrative_ontology:measurement(simu_su_t1000, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1000, 0.75).
narrative_ontology:measurement(simu_su_t1200, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1200, 0.82).
narrative_ontology:measurement(simu_su_t1400, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1400, 0.86).
narrative_ontology:measurement(simu_su_t1600, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1600, 0.88).
narrative_ontology:measurement(simu_su_t1868, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1868, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
