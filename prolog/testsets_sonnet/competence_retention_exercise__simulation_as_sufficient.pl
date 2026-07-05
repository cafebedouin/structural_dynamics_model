% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_retention_exercise__simulation_as_sufficient, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: competence_retention_exercise__simulation_as_sufficient
 *   human_readable: Simulation-as-Sufficient Reading of Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story is the 'simulation_as_sufficient' reading of the contested
 *   competence_retention_exercise kernel: high-fidelity simulation is treated
 *   as structurally equivalent to real catastrophic events for the purpose of
 *   maintaining and certifying operator competence. Under this reading,
 *   training infrastructure — not real catastrophic exposure — becomes the
 *   primary competence-maintenance mechanism, catastrophes are things to be
 *   prevented rather than pedagogical necessities, and competence itself is
 *   operationalized as simulator performance metrics. Two sibling readings
 *   exist as separate constraints (catastrophe_as_necessary,
 *   near_miss_as_bridge) and are not described here beyond the omega
 *   variables documenting the contest.
 *
 * KEY AGENTS:
 *   - operating_organization_leadership: agenda-setter, benefits from defensible low-cost certification
 *   - simulator_vendor_industry: beneficiary, revenue scales with doctrine acceptance
 *   - regulatory_certification_bodies: agenda-setter/beneficiary, prefers tractable metrics over unverifiable readiness
 *   - frontline_operators: payer, bears the risk if equivalence claim is wrong
 *   - downstream_public_exposed_to_residual_risk: payer, no voice in standard-setting
 *   - safety_researchers_and_incident_investigators: analytical observer, produces mixed corroboration evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.42).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.38).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "Simulation-as-Sufficient Reading of Competence Retention").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, '2b592082-4fb8-4663-addf-dbfe8fb200e4').
narrative_ontology:cs_kernel_codification('2b592082-4fb8-4663-addf-dbfe8fb200e4', formalized).
narrative_ontology:cs_authority_grounding('2b592082-4fb8-4663-addf-dbfe8fb200e4', expertise).
narrative_ontology:cs_interpretation_layer_present('2b592082-4fb8-4663-addf-dbfe8fb200e4').
narrative_ontology:cs_reading_relation('2b592082-4fb8-4663-addf-dbfe8fb200e4', competence_retention_exercise__catastrophe_as_necessary, coexists_with).
narrative_ontology:cs_reading_relation('2b592082-4fb8-4663-addf-dbfe8fb200e4', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('2b592082-4fb8-4663-addf-dbfe8fb200e4', foundational, simulated_stakes_structurally_equivalent_to_real_stakes).
narrative_ontology:cs_axiom_status(simulated_stakes_structurally_equivalent_to_real_stakes, holdable).
narrative_ontology:cs_axiom_grounding('2b592082-4fb8-4663-addf-dbfe8fb200e4', simulated_stakes_structurally_equivalent_to_real_stakes, empirically_contingent).
narrative_ontology:cs_axiom('2b592082-4fb8-4663-addf-dbfe8fb200e4', secondary, prevented_catastrophe_preserves_more_value_than_experienced_catastrophe_teaches).
narrative_ontology:cs_axiom_status(prevented_catastrophe_preserves_more_value_than_experienced_catastrophe_teaches, holdable).
narrative_ontology:cs_axiom_grounding('2b592082-4fb8-4663-addf-dbfe8fb200e4', prevented_catastrophe_preserves_more_value_than_experienced_catastrophe_teaches, instrumental).
narrative_ontology:cs_reference_frame('2b592082-4fb8-4663-addf-dbfe8fb200e4', simulator_certification_as_competence_proof).
narrative_ontology:cs_drift_state('2b592082-4fb8-4663-addf-dbfe8fb200e4', post_high_fidelity_simulator_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2b592082-4fb8-4663-addf-dbfe8fb200e4', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulator_vendor_industry).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, regulatory_certification_bodies).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, operating_organization_leadership).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, downstream_public_exposed_to_residual_risk).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, high_fidelity_simulation_equivalence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets training policy, decides simulator investment levels, and certifies crews as competent based on simulator performance. Benefits from avoiding the cost, liability, and reputational exposure of running crews through actual catastrophic scenarios, and from a defensible paper trail showing compliance with competence standards.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, operating_organization_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Sells and maintains the high-fidelity training infrastructure that this reading declares equivalent to real events. Revenue scales directly with the doctrine's acceptance; has no exposure if the equivalence claim proves false in an actual event.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulator_vendor_industry, beneficiary,
    organized, biographical, arbitrage, global).

% Writes and enforces the certification standards that accept simulator hours as proof of competence. Benefits from a tractable, auditable metric (simulator performance scores) rather than the much harder problem of assessing readiness for events that by design happen rarely or never during a career.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, regulatory_certification_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, regulatory_certification_bodies, beneficiary).

% Are certified competent through simulator cycles and then bear the actual consequences if the equivalence claim is wrong during a real low-frequency, high-consequence event. Cannot independently verify whether their trained reflexes transfer, and cannot opt out of the certification regime while remaining employed in the role.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Lives, works, or travels within the consequence radius of the systems these operators run. Has no visibility into whether certification reflects genuine catastrophe-avoidance competence or simulator-metric proficiency, and no channel to demand the more expensive validation regime.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, downstream_public_exposed_to_residual_risk, payer,
    powerless, generational, trapped, regional).

% Studies post-incident records to compare simulator-certified performance against real-event performance. Has produced mixed findings: some domains show strong transfer, others show significant gaps in stress physiology, ambiguity tolerance, and multi-team coordination that simulators struggle to reproduce faithfully.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, safety_researchers_and_incident_investigators, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__simulation_as_sufficient, simulator_vendor_industry).
narrative_ontology:fixing_cost_class(competence_retention_exercise__simulation_as_sufficient, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable, repeatable, non-destructive mechanism for maintaining and verifying catastrophe-avoidance skills across a large workforce without requiring anyone to experience an actual catastrophe to be certified competent.
% TRANSFER_FUNCTION: Moves the cost of competence validation from 'accept the risk of real catastrophic exposure' to 'invest in simulator infrastructure and accept the certification it produces' — shifting residual risk from the certifying institutions onto frontline operators and the downstream public if the equivalence claim is imperfect.
% ABSENT_VOICES: Downstream publics exposed to residual risk have no seat in setting certification standards. Frontline operators individually have limited power to demand costlier, higher-fidelity, or catastrophe-adjacent validation even when they privately doubt simulator transfer.
% DISAPPEARANCE_RATIONALE: If the simulation-as-sufficient doctrine were abandoned overnight, certification regimes would need an alternative competence-validation mechanism; simulator vendors would lose their core revenue justification, regulators would face a much harder and more expensive assessment problem, and organizations would either revert to costlier real-exposure training pathways or to the near-miss-as-bridge reading — the entire training-industrial infrastructure built around simulator equivalence would need restructuring.
% FOUNDING_PROBLEM: Real catastrophic events are too rare, too destructive, and too ethically fraught to use as the primary vehicle for training and certifying operator competence, yet organizations still need a way to verify that operators can respond correctly when such events occur.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory bodies and simulator vendors attest the equivalence problem is solved by high-fidelity simulation. Independent safety researchers and incident investigators — outside the beneficiary set — report mixed corroboration: some post-incident analyses show strong skill transfer, others document specific failure modes (stress physiology, novel-scenario improvisation, multi-agency coordination under real uncertainty) that simulator-certified crews handled worse than the doctrine predicted.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__simulation_as_sufficient, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__simulation_as_sufficient_tests).
:- end_tests(competence_retention_exercise__simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than severe because the coordination function is real: simulation genuinely avoids the ethical and material costs of using catastrophes as training vehicles, and much of the skill transfer literature is supportive. But extraction accumulates because the doctrine's institutional convenience (cheaper, auditable, avoids destructive exposure) creates incentive pressure to over-claim equivalence beyond what the evidence supports, especially as simulator vendors and certification bodies gain entrenched interests in the doctrine's acceptance. Theater ratio rises over the interval (0.22 to 0.44) as certification increasingly optimizes for simulator-metric performance rather than the underlying catastrophe-avoidance competence it is meant to proxy — a Goodhart drift signature. Suppression (0.38) reflects that operators cannot easily contest their certification basis without professional consequences, but this is moderate, not severe, since safety research channels do exist and produce genuine countervailing evidence.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (leadership, regulators), the arrangement reads as sound coordination: a scalable, ethical, cost-effective solution to an intractable training problem. From the frontline-operator and downstream-public payer seats, the same structure reads as a substitution of a measurable proxy for the actual unmeasurable target, with the substitution's risk falling on those least able to verify or contest it. The engine should compute divergent seat classifications from this asymmetry in exit options and stakes exposure.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulator vendors and certification bodies sit near the beneficiary end: they capture revenue or administrative tractability from the doctrine's acceptance and bear no direct consequence if it is imperfect. Leadership sits similarly favorable, gaining liability protection and cost savings. Frontline operators sit toward the target end: they are certified under the doctrine's terms and absorb the consequences of any equivalence gap during an actual event, with constrained exit (leaving the profession is costly and doesn't resolve the certification question for those who stay). The downstream public sits furthest toward the target end: powerless, trapped by geography or dependency on the systems in question, with zero input into the certification standard that governs their exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need a competence-validation mechanism that doesn't require destructive real-world exposure) remains genuinely live — this prevents the classification from collapsing to pure snare. What keeps it tangled rather than a clean rope is the asymmetric evolution: as the doctrine matured, the metric (simulator performance) began drifting away from the target (actual catastrophe-avoidance competence) in ways that benefit the parties administering the metric and burden the parties whose competence is being certified. Enforcement (mandatory certification cycles, licensing requirements tied to simulator hours) is required to sustain the doctrine's institutional acceptance, which is why requires_active_enforcement is true — a purely voluntary, self-evidently correct claim would not need enforcement machinery to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_equivalence_gap,
    'Does high-fidelity simulation actually reproduce the cognitive and procedural demands of real catastrophic events, or does it systematically under-train specific failure modes (acute stress physiology, genuine irreversibility, novel-scenario improvisation, multi-agency coordination under real uncertainty)?',
    'Longitudinal comparison of simulator-certified operator performance against actual real-event performance across multiple domains (aviation, nuclear, maritime, emergency medicine), controlling for event rarity and reporting bias.',
    'If the gap is large and systematic, this reading''s core premise (structural equivalence) is empirically false and the constraint functions primarily as institutional cover for a cheaper alternative rather than genuine competence maintenance — pushing the classification toward snare. If the gap is small, the tangled_rope classification is well-supported by a genuine, if imperfect, coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_equivalence_gap, empirical, 'Whether simulator-certified competence actually transfers to real catastrophic events.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the simulation_as_sufficient reading dominant because it is structurally correct, or because it is the reading most compatible with the interests of the parties (vendors, regulators, leadership) who administer the certification system?',
    'Compare adoption patterns of the three kernel readings across domains with different institutional power structures — do domains with weaker vendor/regulator capture show more adoption of near_miss_as_bridge or catastrophe_as_necessary readings?',
    'If reading-selection tracks institutional interest rather than domain-specific evidence about transfer fidelity, the dominance of simulation_as_sufficient is itself an artifact of the extraction dynamic it is meant to describe, reinforcing the tangled_rope reading over a cleaner rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether kernel-reading dominance is evidence-driven or interest-driven.').

omega_variable(
    downstream_public_risk_pricing,
    'Is the residual risk this reading imposes on the downstream public correctly priced into the certification standards, or is it an externality invisible to the parties who set those standards?',
    'Actuarial and regulatory-economics analysis of whether certification-standard-setting bodies internalize downstream-public risk exposure or optimize primarily for organizational liability and cost.',
    'If the risk is unpriced, the constraint''s extraction is understated by metrics that only capture direct operator and organizational costs, and the true victim set is larger than currently modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(downstream_public_risk_pricing, empirical, 'Whether downstream public risk exposure is accounted for in the standard-setting process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.22).
narrative_ontology:measurement(comp_tr_t4, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 4, 0.27).
narrative_ontology:measurement(comp_tr_t8, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 8, 0.31).
narrative_ontology:measurement(comp_tr_t12, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 12, 0.35).
narrative_ontology:measurement(comp_tr_t16, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 16, 0.39).
narrative_ontology:measurement(comp_tr_t20, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 20, 0.42).
narrative_ontology:measurement(comp_tr_t24, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 24, 0.44).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comp_be_t4, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(comp_be_t8, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(comp_be_t12, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(comp_be_t16, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(comp_be_t20, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(comp_be_t24, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 24, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(competence_retention_exercise__simulation_as_sufficient, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__simulation_as_sufficient, 0.12).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This is one of three sibling readings of the competence_retention_exercise kernel. catastrophe_as_necessary holds that only real catastrophic events provide sufficient learning stakes; near_miss_as_bridge holds that near-misses provide adequate real-world calibration without requiring full catastrophes. Each reading is authored as an independent constraint with its own ε, stakeholder set, and classification per the ε-invariance principle; this story's simulation-vendor-heavy beneficiary structure and moderate extractiveness are specific to the simulation_as_sufficient reading and should not be averaged with the siblings' metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
