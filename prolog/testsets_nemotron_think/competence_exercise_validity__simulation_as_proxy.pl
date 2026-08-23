% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__simulation_as_proxy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__simulation_as_proxy, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: competence_exercise_validity__simulation_as_proxy
 *   human_readable: Simulation Validity as Competence Exercise Proxy
 *   domain: safety_engineering/organizational_learning/competence_retention
 *
 * SUMMARY:
 *   This constraint story captures the 'simulation_as_proxy' reading of the
 *   contested kernel 'competence_exercise_validity.' The reading asserts that
 *   simulation-based drills — proxy-catastrophes — are valid and sufficient
 *   exercises for maintaining operational competence in high-hazard domains
 *   (nuclear, chemical, aviation, oil/gas). The regulatory framework (NRC,
 *   IAEA, OSHA PSM, EASA) codifies this: a successful simulation equals a
 *   valid competence demonstration. The coordination function is real — it
 *   solved the genuine problem of how to exercise readiness without real
 *   disasters. But the extraction tail grew: simulation metrics replaced the
 *   thing they proxy for; scenario fidelity plateaued while regulatory
 *   acceptance hardened; operators optimized for simulation scores rather
 *   than deep competence; vendors shaped the standard to their product. The
 *   claimed_type is tangled_rope because the reading presents itself as pure
 *   coordination (rope) while the metrics reveal asymmetric extraction
 *   (payers: workers/public; beneficiaries: regulators/operators/vendors)
 *   sustained by active enforcement (regulatory compliance).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, 0.65).
domain_priors:suppression_score(competence_exercise_validity__simulation_as_proxy, 0.55).
domain_priors:theater_ratio(competence_exercise_validity__simulation_as_proxy, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, extractiveness, 0.65).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(competence_exercise_validity__simulation_as_proxy, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__simulation_as_proxy, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__simulation_as_proxy, "Simulation Validity as Competence Exercise Proxy").
narrative_ontology:topic_domain(competence_exercise_validity__simulation_as_proxy, "safety_engineering/organizational_learning/competence_retention").

domain_priors:requires_active_enforcement(competence_exercise_validity__simulation_as_proxy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__simulation_as_proxy, '900332e5-316d-4409-bbf5-2737fad420c2').
narrative_ontology:cs_kernel_codification('900332e5-316d-4409-bbf5-2737fad420c2', formalized).
narrative_ontology:cs_authority_grounding('900332e5-316d-4409-bbf5-2737fad420c2', lineage).
narrative_ontology:cs_interpretation_layer_present('900332e5-316d-4409-bbf5-2737fad420c2').
narrative_ontology:cs_reading_relation('900332e5-316d-4409-bbf5-2737fad420c2', competence_exercise_validity__continuous_refresh_hybrid, coexists_with).
narrative_ontology:cs_reading_relation('900332e5-316d-4409-bbf5-2737fad420c2', competence_exercise_validity__real_catastrophe_only, forecloses).
narrative_ontology:cs_axiom('900332e5-316d-4409-bbf5-2737fad420c2', foundational, simulation_sufficiency_for_competence).
narrative_ontology:cs_axiom_status(simulation_sufficiency_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('900332e5-316d-4409-bbf5-2737fad420c2', simulation_sufficiency_for_competence, conventional).
narrative_ontology:cs_axiom('900332e5-316d-4409-bbf5-2737fad420c2', foundational, proxy_catastrophe_equivalence).
narrative_ontology:cs_axiom_status(proxy_catastrophe_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('900332e5-316d-4409-bbf5-2737fad420c2', proxy_catastrophe_equivalence, empirically_contingent).
narrative_ontology:cs_reference_frame('900332e5-316d-4409-bbf5-2737fad420c2', regulatory_simulation_adequacy_framework).
narrative_ontology:cs_drift_state('900332e5-316d-4409-bbf5-2737fad420c2', post_fukushima_deepwater_horizon, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('900332e5-316d-4409-bbf5-2737fad420c2', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, regulators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, operators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__simulation_as_proxy, simulation_vendors).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, frontline_workers).
narrative_ontology:constraint_victim(competence_exercise_validity__simulation_as_proxy, public_communities).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, simulation_validity_doctrine).
narrative_ontology:constraint_vindicates(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce standards that accept simulation metrics as proof of competence retention. They gain regulatory legitimacy and measurable compliance data without requiring costly or risky real exercises. They can point to simulation records to demonstrate oversight effectiveness.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Nuclear, chemical, aviation, and other high-hazard facility operators. They meet regulatory requirements through scheduled simulations rather than full-scale exercises or continuous drill cycles. This is cheaper, less disruptive to operations, and produces auditable records. They bear residual risk if simulation proves inadequate, but that risk is delayed and diffuse.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, operators, beneficiary,
    powerful, biographical, constrained, national).

% Companies that design, build, and run simulation facilities and scenarios. They have a direct financial interest in the regulatory doctrine that simulation counts as valid exercise. They shape scenario design, fidelity standards, and assessment metrics. Their market exists because the constraint creates demand for their product.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, simulation_vendors, beneficiary,
    organized, biographical, mobile, global).

% Control room operators, shift supervisors, maintenance crews, emergency responders. They participate in simulations that are scored as 'successful' but know the gaps: no real radiation, no real toxic release, no real time pressure with lives at stake, no organizational chaos. If a real event occurs, they bear the consequence of any competence gap. Exit means leaving the profession or the facility.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, frontline_workers, payer,
    moderate, biographical, constrained, local).

% Communities surrounding high-hazard facilities. They are told safety is assured because regulators accept simulation records. They have no voice in whether simulation is sufficient, no exit from geographic exposure, and bear the full consequence if competence has decayed beneath the simulation veneer.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, public_communities, payer,
    powerless, generational, trapped, local).

% Independent safety researchers, former regulators, worker representatives, and engineers who argue simulation creates false confidence. They point to events where sim-trained operators failed (Fukushima, Deepwater Horizon, Texas City). They are structurally excluded from the regulatory compliance process that defines 'valid exercise.'
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, safety_critics, excluded,
    moderate, biographical, mobile, national).

% Sees the full structure: a regulatory framework that substituted a measurable proxy (simulation metrics) for the unmeasurable target (actual competence under catastrophe conditions), creating a coordination benefit (standardized, repeatable, auditable) that carries an extraction tail (competence decay shifted to workers and public).
narrative_ontology:constraint_stakeholder(competence_exercise_validity__simulation_as_proxy, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, repeatable, auditable way for high-hazard industries to exercise emergency response competence without waiting for real catastrophes. Solves the genuine problem: how to demonstrate and maintain readiness when real events are (thankfully) rare.
% TRANSFER_FUNCTION: Moves resources (training budget, operational downtime, regulatory attention) from continuous drill cycles and full-scale exercises to discrete simulation events. Moves risk from organizations (who get compliance certificates) to frontline workers and public communities (who bear the consequence if the proxy fails). Moves epistemic authority from operational experience to simulation metrics.
% ABSENT_VOICES: Frontline workers who experience the sim-reality gap daily; communities downstream of facilities who have no say in the validation standard; independent safety researchers whose evidence of proxy failure is treated as anecdotal rather than structural. They are absent from the regulatory rulemaking that defines 'valid exercise' and from the certification audits that accept simulation records.
% DISAPPEARANCE_RATIONALE: If simulation ceased to count as valid exercise overnight, operators would be forced back to continuous drill cycles, full-scale exercises, and alternative competence maintenance regimes. Regulatory frameworks would lose their primary compliance metric. The entire certification and oversight infrastructure would need restructuring. The safety case for many facilities would collapse without the simulation record.
% FOUNDING_PROBLEM: After a sequence of major accidents (Three Mile Island 1979, Bhopal 1984, Challenger 1986, Piper Alpha 1988), regulators and industry needed a way to demonstrate continuous competence retention without requiring real catastrophes. The founding problem was: how to prove readiness when the thing you're readying for must never happen.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies (NRC, IAEA, OSHA, EASA) and operators attest the founding problem is solved: simulation provides auditable, repeatable evidence of competence. Independent safety researchers (e.g., Perrow, Reason, Hollnagel), worker unions (USW, IFPTE), and post-accident investigation boards (Columbia, Deepwater Horizon, Fukushima) attest the founding problem persists: simulation measures performance in a proxy environment, not competence in the target environment. The corroboration is split along the beneficiary/payer line.
narrative_ontology:disappearance_verdict(competence_exercise_validity__simulation_as_proxy, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__simulation_as_proxy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__simulation_as_proxy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_validity__simulation_as_proxy, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__simulation_as_proxy, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__simulation_as_proxy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__simulation_as_proxy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__simulation_as_proxy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the constraint extracts actual competence maintenance and replaces it with simulation compliance — the gap between proxy and target is the extraction. Suppression (0.55) is moderate because alternatives (continuous refresh, full-scale exercises, red-teaming) are not banned but are structurally discouraged: they cost more, lack the auditable metric, and don't satisfy the regulatory checkbox. Theater_ratio (0.45) is rising because an increasing share of simulation activity is performative — scripting scenarios to known parameters, coaching crews, grading on checklist compliance rather than adaptive response. Accessibility_collapse (0.50) reflects that alternatives exist but require swimming upstream against the regulatory current. Resistance (0.55) comes from workers, critics, and post-accident findings, but has not shifted the regulatory baseline.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/operator seat: this is a rope — a genuine coordination mechanism that solved a real problem cheaply and audibly. From the worker/public seat: this is a snare — the coordination story is cover for extracting real readiness and replacing it with a metric that looks like readiness. The engine will compute different types for different seats from the same structural data. That divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulators are agenda_setters with arbitrage-grade exit (they write the rules, face no personal consequence) — d near 0.0 (beneficiary). Operators are beneficiaries with constrained exit (they could push for real exercises but lose competitive parity and regulatory goodwill) — d ~0.25. Simulation_vendors are beneficiaries with mobile exit — d ~0.15. Frontline_workers are payers with constrained exit (profession-specific human capital, facility-specific knowledge) — d ~0.75. Public_communities are payers with trapped exit (geographic, generational) — d ~0.95. Safety_critics are excluded — their structural position is outside the constraint's coordination/extraction calculus. The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (demonstrating competence without real catastrophes) was real and the simulation solution was a genuine coordination innovation. But the mandate has outlived its function: simulation adequacy is now treated as proven rather than provisional; the proxy has become the target; competence decay beneath the simulation veneer is the mandate's exhaust. The constraint is not a pure snare — it still coordinates — but it is not a pure rope either. Tangled_rope captures the hybrid: the coordination function is real but the extraction tail is structural and growing. The mandatrophy is unresolved: the arrangement persists because it solves the regulators' and operators' problem (compliance, cost, liability management) even as it fails the workers' and public's problem (actual competence).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_real_world_gap,
    'Is the gap between simulation performance and real catastrophe response structural (unbridgeable by fidelity improvements) or bridgeable (closable with better scenarios, stress inoculation, VR/AR, AI-driven adaptive opponents)?',
    'Longitudinal studies comparing simulation metrics to real-event performance where real events occur; controlled experiments varying simulation fidelity dimensions; post-accident forensic analysis of operator decisions vs. simulation training records.',
    'If structural, the constraint''s core premise (simulation_sufficiency) is false — the coordination function is a mirage and the constraint is a snare. If bridgeable, the extraction is a maturity gap and the constraint could evolve toward rope with investment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_real_world_gap, empirical, 'Whether the sim-real gap is a fundamental limit or an engineering challenge.').

omega_variable(
    regulatory_capture_of_validity,
    'Does the regulatory framework''s acceptance of simulation as sufficient serve industry convenience and vendor markets over actual safety outcomes?',
    'Trace the history of simulation standard-setting (ANSI/ANS, IAEA NS-G-2.15, NRC RG 1.219): who wrote the standards, who testified, what evidence was required, what alternatives were considered and rejected. Compare regulatory stringency for simulation validation vs. real exercise validation.',
    'If capture is structural, the beneficiary declarations are understated — operators and vendors are not incidental beneficiaries but co-authors of the constraint. The constraint would reclassify toward snare. If the framework is evidence-driven, the current tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_of_validity, conceptual, 'Whether the validation standard is evidence-based or interest-shaped.').

omega_variable(
    competence_decay_measurement,
    'Can we measure actual competence decay under simulation-only regimes, or is the decay inherently unobservable until a real catastrophe reveals it?',
    'Develop latent competence indicators: decision-making latency under novel stressors, cross-scenario adaptation metrics, physiological stress markers, team communication pattern analysis. Validate against simulator-to-real transfer studies in domains with higher event frequency (aviation, military).',
    'If measurable, the extraction becomes visible and contestable — regulators could mandate decay monitoring. If inherently unobservable until failure, the constraint''s extraction is structurally hidden and the mandatrophy is unresolvable without a real event.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_decay_measurement, empirical, 'Whether the extraction (competence decay) is observable or latent.').

omega_variable(
    committer_structure_ambiguity,
    'Is the ''simulation_as_proxy'' reading a stable interpretation of the competence_exercise_validity kernel, or does it contain an internal contradiction: it claims simulation is sufficient while depending on the unproven assumption that simulation metrics track real competence?',
    'Formal analysis of the reading''s axioms: simulation_sufficiency_for_competence (conventional) + proxy_catastrophe_equivalence (empirically_contingent). If the empirical axiom is challenged but the conventional axiom holds, the reading persists by convention despite empirical contradiction — a classic CS drift pattern.',
    'If the reading rests on a conventional axiom that shields an empirically challenged axiom from revision, the CS drift_state (axiom_overriding, unacknowledged) is confirmed. The reading''s legitimacy depends on the interpretation_layer absorbing the empirical challenge without surfacing kernel revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_ambiguity, conceptual, 'Whether the reading''s axiom structure protects it from empirical falsification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__simulation_as_proxy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cev_sim_proxy_tr_t0, competence_exercise_validity__simulation_as_proxy, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cev_sim_proxy_tr_t8, competence_exercise_validity__simulation_as_proxy, theater_ratio, 8, 0.22).
narrative_ontology:measurement(cev_sim_proxy_tr_t16, competence_exercise_validity__simulation_as_proxy, theater_ratio, 16, 0.3).
narrative_ontology:measurement(cev_sim_proxy_tr_t24, competence_exercise_validity__simulation_as_proxy, theater_ratio, 24, 0.38).
narrative_ontology:measurement(cev_sim_proxy_tr_t32, competence_exercise_validity__simulation_as_proxy, theater_ratio, 32, 0.42).
narrative_ontology:measurement(cev_sim_proxy_tr_t40, competence_exercise_validity__simulation_as_proxy, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(cev_sim_proxy_be_t0, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cev_sim_proxy_be_t8, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(cev_sim_proxy_be_t16, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(cev_sim_proxy_be_t24, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(cev_sim_proxy_be_t32, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(cev_sim_proxy_be_t40, competence_exercise_validity__simulation_as_proxy, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cev_sim_proxy_su_t0, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cev_sim_proxy_su_t8, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(cev_sim_proxy_su_t16, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(cev_sim_proxy_su_t24, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(cev_sim_proxy_su_t32, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(cev_sim_proxy_su_t40, competence_exercise_validity__simulation_as_proxy, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__simulation_as_proxy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__simulation_as_proxy, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__continuous_refresh_hybrid).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, competence_exercise_validity__real_catastrophe_only).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, regulatory_compliance_substitution).
narrative_ontology:affects_constraint(competence_exercise_validity__simulation_as_proxy, safety_culture_metrics_as_proxy).

% DUAL FORMULATION NOTE:
% This constraint is one member of the competence_exercise_validity kernel family. The three readings decompose the colloquial 'simulation vs. real exercise' debate into structurally distinct constraints with different ε values, beneficiary/victim structures, and drift profiles. simulation_as_proxy has ε≈0.65 (substantial extraction); continuous_refresh_hybrid likely has lower ε (coordination dominates); real_catastrophe_only likely has near-zero ε (no coordination function, pure epistemic claim). They are linked via network.affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, institutional, 0.1).
constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, powerful, 0.25).
constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, moderate, 0.75).
constraint_indexing:directionality_override(competence_exercise_validity__simulation_as_proxy, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
