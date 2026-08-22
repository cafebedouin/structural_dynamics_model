% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__hybrid_degradation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-17
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__hybrid_degradation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_proxy_sufficiency__hybrid_degradation_reading
 *   human_readable: Hybrid Simulation Catastrophe Proxy — Generational Tacit Knowledge Decay
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear power, commercial aviation,
 *   chemical processing) face a generational problem: the operators who built
 *   and ran these systems through their early, event-rich decades possessed
 *   tacit knowledge and stress-response capacity forged by real catastrophes
 *   and near-misses. As that generation retired, simulation-based training
 *   became the primary mechanism for maintaining competence. This reading
 *   asserts that simulation successfully maintains *procedural* competence —
 *   checklists, protocols, system knowledge — but fails to maintain *tacit
 *   knowledge* and *stress-response capacity*, which degrade silently across
 *   generations without real catastrophes to reset the learning loop. The
 *   constraint is a tangled rope: it coordinates a genuine need (scalable
 *   competence maintenance) while extracting from long-term safety margins
 *   through a hidden decay mechanism. The certification industry benefits
 *   from the belief that simulation suffices; operational crews and the
 *   public bear the latent cost.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.62).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.55).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__hybrid_degradation_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "Hybrid Simulation Catastrophe Proxy — Generational Tacit Knowledge Decay").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__hybrid_degradation_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__hybrid_degradation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'be7682af-cdd9-4d94-a289-468577166806').
narrative_ontology:cs_kernel_codification('be7682af-cdd9-4d94-a289-468577166806', distributed).
narrative_ontology:cs_authority_grounding('be7682af-cdd9-4d94-a289-468577166806', practice).
narrative_ontology:cs_interpretation_layer_present('be7682af-cdd9-4d94-a289-468577166806').
narrative_ontology:cs_reading_relation('be7682af-cdd9-4d94-a289-468577166806', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('be7682af-cdd9-4d94-a289-468577166806', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, forecloses).
narrative_ontology:cs_reading_relation('be7682af-cdd9-4d94-a289-468577166806', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('be7682af-cdd9-4d94-a289-468577166806', foundational, procedural_competence_transferable_tacit_knowledge_not).
narrative_ontology:cs_axiom_status(procedural_competence_transferable_tacit_knowledge_not, holdable).
narrative_ontology:cs_axiom_grounding('be7682af-cdd9-4d94-a289-468577166806', procedural_competence_transferable_tacit_knowledge_not, empirically_contingent).
narrative_ontology:cs_axiom('be7682af-cdd9-4d94-a289-468577166806', foundational, generational_decay_without_real_events_is_structural).
narrative_ontology:cs_axiom_status(generational_decay_without_real_events_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('be7682af-cdd9-4d94-a289-468577166806', generational_decay_without_real_events_is_structural, empirically_contingent).
narrative_ontology:cs_reference_frame('be7682af-cdd9-4d94-a289-468577166806', event_rich_operational_lineage).
narrative_ontology:cs_drift_state('be7682af-cdd9-4d94-a289-468577166806', simulation_dominant_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('be7682af-cdd9-4d94-a289-468577166806', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_certification_industry).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_training_bureaus).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, long_term_safety_margins).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__hybrid_degradation_reading, operational_crews_generational).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, sells, and mandates simulation-based recertification programs for high-reliability operators (nuclear, aviation, chemical). Collects recurring revenue from training cycles. Sets the fidelity standards that define 'adequate' simulation. Benefits from the belief that simulation suffices — this belief is the market. Can diversify into adjacent safety services if simulation mandate weakens.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_certification_industry, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__hybrid_degradation_reading, simulation_certification_industry, beneficiary).

% Administer licensing regimes that accept simulation hours as substitutes for operational experience. Gain budgetary stability and measurable compliance metrics from simulation-based requirements. Their institutional legitimacy rests on having a legible, auditable standard — simulation provides that. Would face political pressure to justify alternatives if simulation's sufficiency were questioned.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, regulatory_training_bureaus, beneficiary,
    institutional, generational, mobile, national).

% Successive cohorts of operators trained primarily on simulators. Procedurally fluent but lack the somatic stress calibration that only real events provide. When catastrophe eventually occurs, their stress-response capacity is degraded relative to predecessors who had real-event experience. Cannot exit the simulation pipeline — it is the only path to certification. Bear the hidden cost when the latent deficit manifests.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, operational_crews_generational, payer,
    organized, biographical, constrained, global).

% The accumulated buffer between operational reality and catastrophic failure. Erodes silently as tacit knowledge and stress-response capacity degrade across generations without real catastrophes to reset the learning loop. No voice, no exit, no representation in the certification process. The constraint extracts from this margin by substituting visible procedural compliance for invisible resilience.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, long_term_safety_margins, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_proxy_sufficiency__hybrid_degradation_reading, long_term_safety_margins).

% Studies the gap between simulation fidelity and real-event stress. Produces evidence that tacit knowledge and physiological stress response do not transfer from simulation. Their findings are cited in certification debates but do not change the regulatory standard because the standard serves the certification industry's revenue model, not the research consensus.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__hybrid_degradation_reading, safety_research_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legible, repeatable, auditable training and certification pipeline that allows high-reliability industries to maintain procedural competence across generations without waiting for rare catastrophic events to create experienced operators.
% TRANSFER_FUNCTION: Moves certification revenue and regulatory compliance burden from operators and regulators to the simulation certification industry; moves latent risk from the present (where it would be visible as training gaps) to the future (where it manifests as degraded stress response during actual catastrophe).
% ABSENT_VOICES: Future operational crews who will face catastrophe with degraded stress-response capacity; the public living downstream of high-reliability facilities; whistleblowers in training departments who see the fidelity gap but cannot speak without losing certification access.
% DISAPPEARANCE_RATIONALE: If simulation-based certification vanished overnight, industries would be forced to develop alternative competence-maintenance mechanisms — likely including controlled real-event exposure, apprenticeship models with veterans of actual events, and stress-inoculation training. The certification industry would lose its primary revenue model. Regulatory bureaus would lose their legible compliance metric. The latent risk currently hidden in degraded tacit knowledge would become visible as a training crisis.
% FOUNDING_PROBLEM: After the first generation of nuclear/aviation/chemical operators (who learned through real events and near-misses) retired, there was no mechanism to give subsequent generations equivalent stress-calibrated experience. Catastrophes are too rare and too costly to serve as training events. Simulation was adopted as a scalable substitute.
% FOUNDING_PROBLEM_CORROBORATION: Veteran operators from the first generation (outside the certification industry) attest that the founding problem was real — they experienced the transition from event-rich to event-scarce eras. The certification industry attests the problem remains live and simulation solves it. Independent safety researchers (outside the beneficiary set) attest that simulation solves the procedural half but not the stress-response half — a partial solution now treated as complete.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__hybrid_degradation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__hybrid_degradation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__hybrid_degradation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_proxy_sufficiency__hybrid_degradation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects the transfer of latent risk to future generations and the revenue extraction by the certification industry. Suppression (0.55) is moderate — the constraint persists because the alternative (real-event exposure) is genuinely costly and dangerous, not purely because alternatives are actively crushed, though regulatory capture by the certification industry does raise the bar for alternatives. Theater ratio (0.68) is high and rising — the simulation pipeline increasingly performs the *appearance* of competence maintenance (hours logged, scenarios completed, certifications renewed) while the actual stress-calibration function atrophies. Accessibility collapse (0.42) is moderate: alternatives exist (apprenticeship, stress inoculation, controlled exposure) but are marginalized by the certification standard. Resistance (0.38) is low-moderate: operational crews know the gap but cannot exit the pipeline; researchers document it but lack regulatory leverage.
 *
 * DIRECTIONALITY LOGIC:
 *   The simulation certification industry and regulatory bureaus are structural beneficiaries (d near 0.0-0.2): they collect revenue and legitimacy from the simulation mandate. Operational crews are targets (d near 0.7-0.8): they bear the latent risk of degraded stress response with constrained exit (certification is mandatory). Long-term safety margins are the ultimate victim (d = 1.0 conceptually): they erode silently with no voice or exit. The safety research community sits at analytical (d = 0.5): they see the full structure but cannot change it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (generational competence transfer without real catastrophes) was real and live when simulation was adopted. The mandate has not resolved — catastrophes remain rare and costly. But the solution has partially degraded into theater: simulation now maintains the *appearance* of the solution while the tacit/stress component decays. This is not pure extraction (snare) because procedural competence *is* genuinely maintained. It is not pure coordination (rope) because the decay mechanism is hidden and the certification industry benefits from the gap. It is a tangled rope — genuine coordination function with asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stress_response_transferability,
    'Can any simulation fidelity level produce genuine somatic stress-response calibration equivalent to real catastrophe?',
    'Longitudinal studies comparing stress biomarkers (cortisol, heart rate variability, cognitive tunneling) in operators with real-event experience vs. simulation-only experience during actual emergencies.',
    'If transfer is impossible at any fidelity, the decay mechanism is structural and the constraint is permanently extractive on the stress-response dimension. If transfer is possible at sufficient fidelity, the constraint becomes a threshold problem (simulation_fidelity_threshold reading gains ground).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stress_response_transferability, empirical, 'Whether stress-response capacity is fundamentally non-transferable from simulation or merely fidelity-dependent').

omega_variable(
    certification_industry_capture_extent,
    'To what extent does the simulation certification industry actively suppress alternatives (apprenticeship, stress inoculation, controlled exposure) versus passively benefiting from regulatory inertia?',
    'Trace lobbying expenditures, revolving-door employment, and standard-setting committee composition over the interval. Compare adoption rates of alternative training modalities in jurisdictions with different regulatory capture levels.',
    'Active suppression raises suppression score and strengthens snare/tangled_rope classification. Passive benefit suggests the constraint could shift toward rope if alternatives were institutionally supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_industry_capture_extent, empirical, 'Active vs. passive beneficiary role of the certification industry').

omega_variable(
    tacit_knowledge_operational_irrelevance_threshold,
    'At what point does degraded tacit knowledge become operationally irrelevant because automation/decision-support has rendered human stress-response obsolete?',
    'Track automation adoption curves and human-in-the-loop decision criticality across high-reliability domains. Identify the crossover where human stress response no longer affects safety margins.',
    'If automation renders human stress response irrelevant before tacit knowledge degrades critically, the victim (long_term_safety_margins) is not actually harmed — the constraint becomes rope or mountain. If human stress response remains critical, the victim is real and the extraction persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tacit_knowledge_operational_irrelevance_threshold, conceptual, 'Whether the victim of extraction (tacit knowledge/stress response) remains operationally relevant').

omega_variable(
    reading_foreclosure_simulation_as_proxy,
    'Does the generational decay premise of this reading logically foreclose the simulation_as_proxy_catastrophe_reading within any single regulatory framework?',
    'Analyze whether a regulatory framework could simultaneously mandate simulation-as-sufficient (proxy reading) while acknowledging generational decay (this reading) — i.e., could the decay be treated as an acceptable cost?',
    'If foreclosure holds, the two readings cannot coexist in one framework — one must displace the other. If coexistence is possible (decay as priced externality), the relation is coexists_with or influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_simulation_as_proxy, conceptual, 'Structural relationship between hybrid_degradation_reading and simulation_as_proxy_catastrophe_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cata_tr_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(cata_tr_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement(cata_tr_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 30, 0.65).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cata_be_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(cata_be_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(cata_be_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(cata_su_t10, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(cata_su_t20, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(cata_su_t30, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(cata_su_t40, catastrophe_proxy_sufficiency__hybrid_degradation_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__hybrid_degradation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__simulation_fidelity_threshold).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__hybrid_degradation_reading, catastrophe_proxy_sufficiency__catastrophe_necessity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the catastrophe_proxy_sufficiency kernel. It decomposes the contested claim 'simulation suffices for catastrophe proxy' into a tangled rope reading (partial sufficiency with hidden decay). The simulation_fidelity_threshold reading is downstream — this reading's decay mechanism creates pressure to define the fidelity threshold. The catastrophe_necessity_reading is a parallel live position held by different parties (veteran operators, some safety researchers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, institutional, 0.15).
constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__hybrid_degradation_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
