% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__simulation_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__simulation_sufficiency_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: exercise_as_competence_maintenance__simulation_sufficiency_reading
 *   human_readable: Simulation Sufficiency for Competence Maintenance
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This constraint story instantiates the simulation_sufficiency_reading of
 *   the contested kernel 'exercise_as_competence_maintenance.' The reading
 *   holds that simulated catastrophe — high-fidelity drills, scenario-based
 *   exercises, simulator training — constitutes genuine exercise of the
 *   competence kernel that maintains organizational crisis capability.
 *   Regulatory mandates for regular drills are treated as sufficient
 *   compliance; competence is measured by simulator performance metrics
 *   (scenario completion, decision accuracy under simulated stress,
 *   procedural adherence). The victim set is narrowly defined: only those
 *   harmed when simulation fidelity proves inadequate (e.g., a scenario
 *   omitted a failure mode that later materialized). Two sibling readings
 *   contest this: lived_catastrophe_necessity_reading holds that only actual
 *   catastrophe exercises the kernel; hybrid_decay_reading holds that
 *   simulation exercises procedural competence but not judgment-under-stakes.
 *   This story authors ONLY the simulation_sufficiency_reading as a clean
 *   ε-invariant constraint — the other readings are separate constraints.
 *
 * KEY AGENTS:
 *   - regulatory_bodies: Agenda setter (institutional/analytical) — mandates drill frequency, scenario types, fidelity thresholds; benefits from demonstrable compliance regime
 *   - simulation_vendors: Beneficiary (organized/institutional) — sells platforms, scenarios, certification; revenue scales with mandate scope and fidelity requirements
 *   - safety_executives: Beneficiary/agenda_setter (institutional) — uses drill compliance as evidence of due diligence; career risk reduced by checkbox compliance
 *   - frontline_operators: Payer (moderate/organized) — bears time, cognitive load, and career risk of drill participation; judged on simulator metrics that may not reflect real-stakes competence
 *   - affected_communities: Payer (powerless/local) — bears consequences when simulation-exercised competence fails in actual crisis; excluded from mandate design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.32).
domain_priors:suppression_score(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.41).
domain_priors:theater_ratio(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__simulation_sufficiency_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__simulation_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__simulation_sufficiency_reading, "Simulation Sufficiency for Competence Maintenance").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__simulation_sufficiency_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__simulation_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__simulation_sufficiency_reading, '281c014f-4bb3-4e37-8fab-8a5d154c3474').
narrative_ontology:cs_kernel_codification('281c014f-4bb3-4e37-8fab-8a5d154c3474', formalized).
narrative_ontology:cs_authority_grounding('281c014f-4bb3-4e37-8fab-8a5d154c3474', extraction).
narrative_ontology:cs_interpretation_layer_present('281c014f-4bb3-4e37-8fab-8a5d154c3474').
narrative_ontology:cs_reading_relation('281c014f-4bb3-4e37-8fab-8a5d154c3474', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('281c014f-4bb3-4e37-8fab-8a5d154c3474', exercise_as_competence_maintenance__hybrid_decay_reading, coexists_with).
narrative_ontology:cs_axiom('281c014f-4bb3-4e37-8fab-8a5d154c3474', foundational, simulation_fidelity_sufficiency).
narrative_ontology:cs_axiom_status(simulation_fidelity_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('281c014f-4bb3-4e37-8fab-8a5d154c3474', simulation_fidelity_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('281c014f-4bb3-4e37-8fab-8a5d154c3474', foundational, regulatory_compliance_equivalence).
narrative_ontology:cs_axiom_status(regulatory_compliance_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('281c014f-4bb3-4e37-8fab-8a5d154c3474', regulatory_compliance_equivalence, conventional).
narrative_ontology:cs_reference_frame('281c014f-4bb3-4e37-8fab-8a5d154c3474', drill_mandate_sufficiency_framework).
narrative_ontology:cs_drift_state('281c014f-4bb3-4e37-8fab-8a5d154c3474', contemporary_mandate_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('281c014f-4bb3-4e37-8fab-8a5d154c3474', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_vendors).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_executives).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__simulation_sufficiency_reading, affected_communities).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_fidelity_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_compliance_equivalence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates drill frequency, scenario types, and fidelity thresholds for safety-critical industries (nuclear, aviation, chemical, healthcare). Benefits from a demonstrable compliance regime that satisfies legislative oversight and public assurance demands. Designs mandates through rulemaking processes that simulation vendors and industry associations lobby. Does not bear operational costs of drills.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Sells simulation platforms, scenario libraries, instructor services, and certification programs. Revenue scales directly with mandate scope, frequency, and fidelity requirements. Lobbies for higher-fidelity mandates and broader industry coverage. Can pivot to adjacent markets (training, gaming, digital twins) if mandate regime shifts — high exit optionality.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Uses drill completion rates and simulator pass-rates as evidence of due diligence to boards, regulators, and insurers. Career risk is reduced by checkbox compliance. Has authority to allocate drill budgets and select vendors. Can move between organizations — the compliance skill set is portable.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_executives, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_executives, agenda_setter).

% Bears the time, cognitive load, and career risk of participating in mandated drills. Judged on simulator metrics (scenario completion, procedural adherence, decision latency) that may not reflect real-stakes judgment. Cannot easily opt out — drill completion is a licensing/employment condition. Exit requires leaving the profession or jurisdiction. Experiences the constraint as extraction masked as coordination.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, frontline_operators, payer,
    moderate, biographical, constrained, regional).

% Bears consequences when simulation-exercised competence fails in actual catastrophe (toxic release, radiation exposure, structural collapse). Excluded from mandate design and drill design. Has no exit — cannot relocate away from all safety-critical infrastructure. The victim set declared by this reading (harmed by inadequate fidelity) captures only the fidelity-gap injury, not the false-confidence injury where drills create illusion of preparedness that delays real investment.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, affected_communities, payer,
    powerless, generational, trapped, local).

% Studies competence retention, simulation fidelity, and drill effectiveness across domains. Provides the empirical basis for fidelity metrics and mandate design. Some contest the simulation_sufficiency_reading (citing transfer-of-training gaps, stress-inoculation literature); others support it. Not directly subject to mandates but shapes the epistemic environment.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__simulation_sufficiency_reading, safety_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__simulation_sufficiency_reading, simulation_vendors).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__simulation_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, repeatable, measurable mechanism for organizations to exercise crisis procedures without waiting for actual catastrophe — solves the 'practice without disaster' coordination problem across distributed operators, shifts, and facilities.
% TRANSFER_FUNCTION: Moves operator time, organizational training budgets, and regulatory compliance costs from regulated entities and frontline operators to simulation vendors and compliance administrations, in exchange for drill completion certificates and simulator performance records.
% ABSENT_VOICES: Affected communities (local residents near hazardous facilities) are excluded from mandate design and drill design — they would object to drill regimes that optimize for measurable compliance over actual preparedness. Near-miss analysts and red-team practitioners are structurally marginalized — their methods compete with mandated drills for budget and attention but lack regulatory equivalence.
% DISAPPEARANCE_RATIONALE: If simulation-sufficiency mandates vanished overnight, regulated entities would initially reduce drill frequency and fidelity. Some would revert to live exercises and near-miss analysis; others would do nothing until catastrophe struck. Simulation vendors would lose a primary revenue stream and pivot. Regulatory bodies would lose their primary assurance mechanism and face legislative pressure to replace it. The safety assurance ecosystem would reorganize — possibly toward hybrid_decay_reading's model (simulation for procedure, live exercises for judgment) or lived_catastrophe_necessity_reading's model (accepting that only real events exercise the full kernel).
% FOUNDING_PROBLEM: Post-WWII expansion of hazardous technologies (nuclear, chemical, aviation) created a competence-maintenance crisis: organizations operating novel high-consequence systems had no institutional memory of catastrophe to draw on, and waiting for actual events to build competence was ethically and politically unacceptable. Simulation offered a way to 'exercise the muscle' without the injury.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear industry historians (e.g., Perrow, Reason) attest the founding problem was live in the 1950s-1970s. Contemporary safety scholars (e.g., Dekker, Hollnagel) attest the problem has mutated: the hazard landscape has shifted from 'novel technologies' to 'complex socio-technical systems' where simulation fidelity gaps are structural, not temporary. Simulation vendors and regulatory bodies attest the problem remains live in original form — corroboration from outside the beneficiary set is mixed, confirming contested status.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__simulation_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__simulation_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(exercise_as_competence_maintenance__simulation_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).
:- end_tests(exercise_as_competence_maintenance__simulation_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) is moderate: the constraint transfers operator time and organizational resources to simulation vendors and compliance administrations, but also delivers genuine coordination value (procedural fluency, scenario familiarity). Suppression (0.41) is moderate: alternatives (live exercises, red-teaming, near-miss analysis) are not banned but are structurally disadvantaged by regulatory frameworks that equate drill completion with competence. Theater ratio (0.58) is high and rising: an increasing share of drill activity performs compliance rather than exercises judgment — scenarios become scripted, 'success' becomes probable, fidelity metrics optimize for measurability not realism. Accessibility collapse (0.45) is moderate: the simulation_sufficiency_reading makes alternatives conceptually available but practically marginal — organizations that invest in live exercises still must run mandated drills. Resistance (0.38) is moderate: frontline operators and some safety scholars contest the sufficiency claim, but institutional momentum and vendor capture sustain the regime.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory_bodies and safety_executives seats (beneficiaries/agenda_setters), the constraint appears as rope — genuine coordination solving the 'how do we maintain competence without waiting for catastrophe' problem. From the frontline_operators seat (payer), the constraint appears as tangled_rope — coordination mixed with extraction (time, career risk, false confidence). From the affected_communities seat (payer, powerless), the constraint appears as snare — the coordination story is cover; the victim bears consequences of unexercised judgment-under-stakes. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory_bodies and safety_executives are structural beneficiaries (d near 0.15-0.25): they collect compliance evidence, career protection, and institutional legitimacy. Simulation_vendors are direct financial beneficiaries (d near 0.1): revenue scales with mandate scope. Frontline_operators are targets (d near 0.7-0.8): they pay with time, cognitive load, and the risk that simulator metrics become career gatekeepers divorced from real competence. Affected_communities are trapped targets (d near 0.9): they bear ultimate consequences with zero exit — they cannot leave the jurisdiction of the safety regime. The directionality derivation from beneficiary/victim declarations + power + exit produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — maintaining crisis competence without waiting for actual catastrophe — remains LIVE but the arrangement's fit to it has degraded. Early drill regimes (1970s-1990s) had higher fidelity, lower frequency, and stronger coupling to operational reality. Modern mandate-driven regimes (2000s-present) exhibit mandatrophy: the drill mandate persists and expands while the competence-maintenance function atrophies into compliance theater. The constraint now extracts more than it coordinates. The simulation_sufficiency_reading's axioms (simulation_fidelity_sufficiency, regulatory_compliance_equivalence) are holdable but contested — the engine should detect the drift via theater_ratio accumulation and extraction creep.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the simulation_sufficiency_reading a distinct constraint from the lived_catastrophe_necessity_reading and hybrid_decay_reading, or do they represent different observables of the same constraint?',
    'Trace whether the three readings produce different ε values when assessed against their own referents — the standing arrangement each reading instantiates. If ε differs, they are distinct constraints per ε-invariance (DP-001).',
    'If distinct, each reading gets its own constraint story linked via network.affects_constraints. If not distinct, the label ''exercise_as_competence_maintenance'' conflates multiple constraints and must be decomposed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings of exercise_as_competence_maintenance are ε-invariant distinct constraints').

omega_variable(
    simulation_fidelity_measurement,
    'Can simulation fidelity be measured independently of the competence it purports to exercise, or does the measurement presuppose the reading''s conclusion?',
    'Examine whether fidelity metrics (scenario completeness, stressor realism, decision latency under simulation) correlate with actual crisis performance in domains where both exist — without assuming the simulation_sufficiency_reading''s premise that simulation fidelity determines retention.',
    'If fidelity metrics presuppose the conclusion, the constraint''s coordination function is circular — the ''exercise'' is theater. If independently measurable, the coordination claim has empirical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_measurement, empirical, 'Whether simulation fidelity metrics are independent of the reading''s own competence claims').

omega_variable(
    victim_set_boundary,
    'Does the victim set ''those harmed by inadequate simulation fidelity'' capture all who bear costs, or does it exclude those harmed by the false confidence simulation sufficiency creates?',
    'Compare incident post-mortems where simulation-exercised competence failed against cases where no simulation occurred — trace whether the simulation_sufficiency_reading''s victim boundary systematically excludes the false-confidence injury class.',
    'If the victim boundary is narrower than the actual injury pattern, the constraint''s extraction is undercounted and its claimed_type may compute as snare rather than tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_set_boundary, empirical, 'Whether the declared victim set captures the full injury pattern of simulation-sufficiency regimes').

omega_variable(
    mandate_capture_risk,
    'Are regulatory drill mandates shaped by the simulation_vendor beneficiary group, creating a feedback loop where mandate design serves vendor revenue rather than competence?',
    'Trace regulatory rulemaking history: vendor lobbying expenditure, revolving-door personnel, and whether mandate specificity (scenario types, frequency, fidelity thresholds) exceeds what competence retention requires.',
    'If mandates are vendor-captured, the coordination function is compromised and the constraint shifts toward snare — the ''sufficiency'' claim becomes cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_capture_risk, empirical, 'Whether regulatory drill mandates exhibit vendor capture dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_tr_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_tr_t0, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_tr_t6, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_tr_t6, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_tr_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_tr_t12, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_tr_t18, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 18, 0.52).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_tr_t18, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_tr_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 24, 0.55).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_tr_t24, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_tr_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_be_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_be_t0, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_be_t6, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 6, 0.22).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_be_t6, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_be_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_be_t12, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_be_t18, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 18, 0.29).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_be_t18, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_be_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_be_t24, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_be_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_su_t0, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_su_t0, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_su_t6, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 6, 0.32).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_su_t6, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_su_t12, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_su_t12, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_su_t18, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 18, 0.38).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_su_t18, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_su_t24, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_su_t24, observed).
narrative_ontology:measurement(exercise_as_competence_maintenance__simulation_sufficiency_reading_su_t30, exercise_as_competence_maintenance__simulation_sufficiency_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(exercise_as_competence_maintenance__simulation_sufficiency_reading_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__simulation_sufficiency_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__simulation_sufficiency_reading, 0.08).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__simulation_sufficiency_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint and its two siblings form a constraint family decomposing the natural-language concept 'exercise as competence maintenance.' Each reading instantiates a different ε: simulation_sufficiency_reading (ε=0.32, tangled_rope), lived_catastrophe_necessity_reading (expected higher ε, snare/tangled_rope boundary), hybrid_decay_reading (expected moderate ε, tangled_rope). The ε values differ because the standing arrangements each reading assesses are structurally distinct — different mandate regimes, different victim sets, different coordination/extraction balances. Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
