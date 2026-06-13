% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Dual-Register Ritual Encoding of Catastrophe Memory and Practical Knowledge
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   catastrophe_memory_survival. The hybrid-encoding reading asserts that
 *   ritual's persistence after catastrophic disruption depends on its
 *   simultaneous encoding of BOTH symbolic boundary-maintenance AND embedded
 *   practical survival knowledge, with neither register reducible to the
 *   other. A community cannot defend its ritual practice to separationist
 *   analysts (who insist on choosing between symbol or competence as the
 *   primary function) without abandoning the integrated account that is
 *   constitutive of the constraint itself. The constraint's ε is low (0.38 at
 *   interval end) because the extraction measured is not from the
 *   coordination function but from the asymmetry between those inside the
 *   framework (who benefit from unified transmission) and those forced to
 *   choose a single register (analysts, communities under pressure to
 *   modernize knowledge systems). The measurement series show stabilization
 *   by t=20: extractiveness rises early (as communities formalize ritual
 *   against external pressure to separate registers), then holds steady once
 *   the integration is institutionalized; theater_ratio falls as practice
 *   becomes more functionally embedded rather than performative; suppression
 *   rises slightly early (communities must defend the integrated account
 *   against separationist critique) then stabilizes at a low floor once the
 *   framework is accepted.
 *
 * KEY AGENTS:
 *   - communities_maintaining_ritual_practice: Primary beneficiary and agenda-setter; sets the terms of ritual transmission and defends the integrated encoding against separationist pressure.
 *   - knowledge_holders_intergenerational: Secondary beneficiary; authority derives from demonstrable competence and custodianship of both registers; they bear the cost of maintaining the integration.
 *   - analysts_separationist_frame: Excluded and analytically victimized; their frameworks cannot accommodate the integrated account the communities hold; forced to either accept incompleteness or reject the community's self-understanding.
 *   - youth_pre_initiation: Powerless payers; carry the time and discipline cost of dual apprenticeship; gain knowledge and identity only through participation.
 *   - adaptive_pressure_carriers: Trapped payers; face crises requiring knowledge innovation but cannot update knowledge without symbolic drift, or vice versa; the constraint's inseparability prevents rapid instrumental adaptation.
 *   - observer_comparative_analysis: Analytical seat; measures the constraint without presupposing separationism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Dual-Register Ritual Encoding of Catastrophe Memory and Practical Knowledge").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '606decbc-6d4e-4a38-ab78-9bdb18c1058f').
narrative_ontology:cs_kernel_codification('606decbc-6d4e-4a38-ab78-9bdb18c1058f', distributed).
narrative_ontology:cs_authority_grounding('606decbc-6d4e-4a38-ab78-9bdb18c1058f', practice).
narrative_ontology:cs_interpretation_layer_present('606decbc-6d4e-4a38-ab78-9bdb18c1058f').
narrative_ontology:cs_reading_relation('606decbc-6d4e-4a38-ab78-9bdb18c1058f', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('606decbc-6d4e-4a38-ab78-9bdb18c1058f', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_axiom('606decbc-6d4e-4a38-ab78-9bdb18c1058f', foundational, dual_register_inseparability).
narrative_ontology:cs_axiom_status(dual_register_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('606decbc-6d4e-4a38-ab78-9bdb18c1058f', dual_register_inseparability, instrumental).
narrative_ontology:cs_axiom('606decbc-6d4e-4a38-ab78-9bdb18c1058f', foundational, survival_knowledge_requires_symbolic_embedding).
narrative_ontology:cs_axiom_status(survival_knowledge_requires_symbolic_embedding, holdable).
narrative_ontology:cs_axiom_grounding('606decbc-6d4e-4a38-ab78-9bdb18c1058f', survival_knowledge_requires_symbolic_embedding, empirically_contingent).
narrative_ontology:cs_reference_frame('606decbc-6d4e-4a38-ab78-9bdb18c1058f', post_catastrophe_unified_transmission).
narrative_ontology:cs_drift_state('606decbc-6d4e-4a38-ab78-9bdb18c1058f', contemporary_analytic_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('606decbc-6d4e-4a38-ab78-9bdb18c1058f', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, communities_maintaining_ritual_practice).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, knowledge_holders_intergenerational).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).
:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.38 at interval end) because the constraint operates primarily as coordination (unified transmission, boundary-maintenance) with asymmetric costs falling on specific seats (pre-initiated youth, crisis-adaptation communities, separationist analysts). Suppression is very low (0.22) because the constraint persists through framework-commitment rather than coercive enforcement; communities maintain it because they understand it as the condition of survival, not because alternatives are blocked. Theater is moderate-high initially (0.52) because early in the interval, ritual is performative relative to adaptive crises—communities must invest symbolic effort to defend against separationist pressure. Theater falls to 0.45 as the integration becomes functionally embedded (practice demonstrates the efficacy of the unified encoding, reducing performative cost). The constraint is CLAIMED as rope (genuine coordination with low-to-moderate extraction) and the metrics support this: no party is victimized so severely that the arrangement becomes extractive snare; the coordination function is real; the asymmetries are moderate. The measurement series share one time grid (every metric authored at every point) so temporal coherence is guaranteed.
 *
 * PERSPECTIVAL GAP:
 *   From the knowledge-holders' seat, the arrangement is genuine coordination: ritual is the only way to preserve both knowledge and identity through catastrophe. From the adaptive-pressure community's seat, the arrangement is constraining: it prevents them from updating knowledge faster than symbolic form allows. From the analyst's seat (if they accept the framework), it is pure coordination; if they hold to separationism, they cannot even see the constraint coherently—they see two separate constraints being conflated. The engine should compute different types per seat: knowledge-holders and communities compute as cooperative rope; adaptive-pressure communities compute as constrained-rope or tangled-rope (coordination + asymmetric burden); analysts compute as either observer (if they accept the framework) or as victims of a snare (if they hold separationism and are forced to operate in a system they classify as confused).
 *
 * DIRECTIONALITY LOGIC:
 *   Communities and knowledge-holders sit near the beneficiary end of the directionality spectrum (d ≈ 0.2–0.3): they benefit from unified transmission, their time horizons are civilizational, and their exit is identity-locked (leaving ritual means leaving the community). Pre-initiated youth and adaptive-pressure communities sit near the target end (d ≈ 0.65–0.75): they bear concentrated time/innovation costs, their time horizons are biographical or immediate, and their exit is constrained (you cannot leave ritual without leaving community). Analysts sit at the excluded boundary: they have institutional power but analytical exit (they can choose other frameworks), so directionality is unstable—they are high-power but their power is incommensurable with the community's framework. The engine's derivation from beneficiary/victim + exit should produce this spread; no overrides needed if the stakeholder data are clean.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophy: the founding problem (catastrophic knowledge loss through disruption) is live and present in every community that maintains ritual after major disruption. The dual encoding is not theater; it is functionally embedded in the knowledge-transmission mechanism. Measurement series confirm this: theater_ratio is moderate and falling, not high and stable (a piton would show stable-high theater). The constraint persists because it solves a real problem, not because of institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    separability_of_registers,
    'Can practical survival knowledge and symbolic boundary-maintenance be separated and transmitted independently without loss of efficacy or identity-continuity?',
    'Natural experiment from communities that have transitioned to written knowledge systems and secular identity-markers: do they maintain survival competence and group continuity as effectively as communities using dual-encoded ritual? Longitudinal measurement of knowledge-transmission success and group-cohesion metrics.',
    'If separable, the constraint becomes analyzable as two overlapping constraints (competence_transmission + identity_reproduction) that happen to use ritual as a common vehicle; if inseparable, the dual-encoding is structurally necessary and the constraint is correctly classified as hybrid-rope. This determines whether analysts holding separationist frameworks are excluded victims or simply using a different (incomplete) model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_of_registers, empirical, 'Whether the dual registers in ritual encoding are functionally separable.').

omega_variable(
    identity_lock_mechanism_internalized,
    'Is the low measured suppression an accurate account of the constraint''s binding force, or does identity-fusion (the community''s self-identity constituted through ritual participation) mask structural suppression that would emerge if exit were externally enabled?',
    'Post-exit trajectory: do communities that abandon ritual and adopt written knowledge systems report persistent identity-loss, transmission-failure, or group-dissolution markers even with external barriers removed? Ethnographic follow-up with diaspora and post-transition communities.',
    'If internalized identity-fusion, the low suppression is correctly measured (the constraint binds through self-constitution, not external force); if masked structural suppression, the true suppression is higher and the constraint skews more toward tangled-rope. This affects the classification''s seat-variation: some seats may compute higher suppression than the authored 0.22 allows.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalized, empirical, 'Whether low measured suppression reflects genuine low coercive force or masked internalization of constraints.').

omega_variable(
    crisis_innovation_bottleneck,
    'Does the constraint''s inseparability of registers structurally prevent rapid knowledge innovation under adaptive pressure, or does the integration itself enable adaptive innovation by forcing holistic reappraisal of both symbol and competence together?',
    'Historical case analysis of knowledge innovation under crisis in dual-encoding vs. separationist systems: do dual-encoded communities show slower, more holistic adaptation or faster, more granular adaptation? Comparative analysis of pandemic/climate/conflict response across ritual and non-ritual knowledge systems.',
    'If the constraint prevents innovation (bottleneck case), adaptive-pressure communities are genuinely victimized and the constraint approaches snare from their seat; if the constraint enables innovation (holistic case), the burden is real but the benefit is obscured from extractive analysis. This affects whether adaptive-pressure communities compute as payer-only or as payer-beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_innovation_bottleneck, empirical, 'Whether dual-register inseparability bottlenecks or enables adaptive knowledge innovation.').

omega_variable(
    kernel_reading_contest_underdetermination,
    'This constraint is one reading of a contested kernel. Is the hybrid-encoding reading a coherent alternative to competence_transmission and symbol_survival readings, or does it collapse into one of them under analytic pressure?',
    'Formalize the three readings'' axioms and test for foreclosure or logical dependency. If no reading forecloses the others, they coexist as live positions; if one does, reclassify the relations accordingly. Community testimony about whether ritual functions simultaneously in both registers or primarily in one (with the other incidental) provides strong evidence.',
    'If the reading collapses or is foreclosed, reclassify to snare (communities are victimized by analysts forcing them into a false binary). If the reading holds as genuinely alternative to both siblings, keep classification as rope and the network relations as coexists_with (not forecloses or influences).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_underdetermination, conceptual, 'Whether the hybrid-encoding reading is a logically independent alternative or collapses to one of the sibling readings.').

omega_variable(
    analyst_victimization_structural_or_analytical,
    'Are analysts holding separationist frameworks victimized by the constraint''s inseparability, or are they simply using an incomplete analytical model that communities are not obligated to accommodate?',
    'Meta-analysis: if communities that maintain dual-encoding must spend effort defending their integrated account against separationist critique (bearing a suppression cost), then the analysts are victims. If communities can simply ignore separationist frameworks without cost, then analysts are merely observing in an incommensurable register, not victims.',
    'If victimized, analysts should be listed as victims in a snare-framed alternative reading; if incommensurable, they remain excluded from the coordination frame (rope-framed). This affects which sibling reading (symbol_survival or competence_transmission) the separationist frameworks align with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(analyst_victimization_structural_or_analytical, preference, 'Whether separationist analysts bear victimization from the constraint or merely observe incommensurably.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 10, 0.49).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(cata_tr_t35, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 35, 0.45).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(cata_be_t35, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(cata_su_t5, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 15, 0.21).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(cata_su_t35, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 35, 0.22).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__hybrid_encoding_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel catastrophe_memory_survival. The readings differ in their assessment of which register (symbolic boundary-maintenance vs. embedded practical knowledge) is primary or essential. This reading asserts both are inseparable and structurally necessary. The sibling readings (competence_transmission_reading and symbol_survival_reading) prioritize one register over the other and treat the other as incidental or derivative. Each reading has distinct beneficiary/victim structures and different ε values. All three readings must be authored as separate constraint stories and linked via network.affects_constraints to enable the engine to measure the kernel contest and detect which reading's framework is operative in which communities or time periods.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__hybrid_encoding_reading, analytical, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
