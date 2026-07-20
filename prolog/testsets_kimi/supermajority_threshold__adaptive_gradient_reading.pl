% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__adaptive_gradient_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__adaptive_gradient_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: supermajority_threshold__adaptive_gradient_reading
 *   human_readable: Supermajority Threshold (Adaptive Gradient Reading)
 *   domain: constitutional_theory/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the adaptive_gradient_reading of the
 *   contested supermajority_threshold kernel. Under this reading, a
 *   supermajority threshold is not inherently legitimate but is a functional
 *   tool whose validity depends on calibration to measurable social consensus
 *   formation rates and reversibility costs. The reading acknowledges that
 *   too low a threshold produces instability (rope-like coordination failure)
 *   while too high a threshold produces ossification (snare-like extraction).
 *   The colloquial label 'supermajority threshold' conflates three
 *   structurally distinct normative claims; this file isolates the adaptive
 *   gradient reading as a separate Îµ-invariant constraint.
 *
 * KEY AGENTS:
 *   - institutional_gatekeepers (institutional/constrained): Administer the threshold and derive procedural authority from it.
 *   - status_quo_defenders (organized/constrained): Benefit from blocked reform when the threshold is miscalibrated high.
 *   - reform_majorities (powerful/constrained): Bear the cost of agenda blockage despite holding electoral majorities.
 *   - constitutional_scholars (analytical/analytical): Evaluate calibration empirically without controlling the rule.
 *   - excluded_policy_beneficiaries (powerless/trapped): Would gain from blocked reforms but have no procedural voice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.58).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.52).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, tangled_rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Supermajority Threshold (Adaptive Gradient Reading)").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "constitutional_theory/political_economy").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, 'f1f8c3b6-2907-4ed9-9c81-69093fa79d3b').
narrative_ontology:cs_kernel_codification('f1f8c3b6-2907-4ed9-9c81-69093fa79d3b', formalized).
narrative_ontology:cs_authority_grounding('f1f8c3b6-2907-4ed9-9c81-69093fa79d3b', lineage).
narrative_ontology:cs_interpretation_layer_present('f1f8c3b6-2907-4ed9-9c81-69093fa79d3b').
narrative_ontology:cs_reading_relation('f1f8c3b6-2907-4ed9-9c81-69093fa79d3b', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1f8c3b6-2907-4ed9-9c81-69093fa79d3b', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_axiom('f1f8c3b6-2907-4ed9-9c81-69093fa79d3b', foundational, legitimacy_contingent_on_empirical_performance).
narrative_ontology:cs_axiom_status(legitimacy_contingent_on_empirical_performance, holdable).
narrative_ontology:cs_axiom_grounding('f1f8c3b6-2907-4ed9-9c81-69093fa79d3b', legitimacy_contingent_on_empirical_performance, instrumental).
narrative_ontology:cs_axiom('f1f8c3b6-2907-4ed9-9c81-69093fa79d3b', secondary, reversibility_costs_shape_threshold_validity).
narrative_ontology:cs_axiom_status(reversibility_costs_shape_threshold_validity, holdable).
narrative_ontology:cs_axiom_grounding('f1f8c3b6-2907-4ed9-9c81-69093fa79d3b', reversibility_costs_shape_threshold_validity, empirically_contingent).
narrative_ontology:cs_reference_frame('f1f8c3b6-2907-4ed9-9c81-69093fa79d3b', calibrated_institutional_equilibrium).
narrative_ontology:cs_drift_state('f1f8c3b6-2907-4ed9-9c81-69093fa79d3b', contemporary_constitutional_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f1f8c3b6-2907-4ed9-9c81-69093fa79d3b', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, status_quo_defenders).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, reform_majorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and interpret supermajority rules in legislative chambers or constitutional conventions; determine procedural compliance and enforce the threshold requirement. Their authority derives from institutional position and procedural control.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, institutional_gatekeepers, agenda_setter,
    institutional, generational, constrained, national).

% Political factions and interest groups that benefit from high thresholds blocking reforms they oppose. They collect the policy stability and institutional inertia the threshold generates, protecting their preferred arrangements against majoritarian challenge.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, status_quo_defenders, beneficiary,
    organized, biographical, constrained, national).

% Electoral or legislative majorities that support constitutional or procedural change but lack the supermajority required to enact it. They bear the cost of blocked agendas and must either abandon reform or invest extraordinary resources to build additional consensus.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, reform_majorities, payer,
    powerful, biographical, constrained, national).

% Analyze whether current threshold levels are calibrated to actual social consensus formation rates and reversibility costs. They produce empirical and normative research but do not directly control the threshold.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% Citizens and groups who would benefit from reforms blocked by the supermajority threshold. They are not represented in the procedural venues where threshold calibration is debated and have no direct avenue to alter the rule.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, excluded_policy_beneficiaries, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__adaptive_gradient_reading, status_quo_defenders).
narrative_ontology:fixing_cost_class(supermajority_threshold__adaptive_gradient_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents hasty constitutional or procedural change by requiring broader agreement than a simple majority, thereby reducing policy volatility and protecting institutional arrangements against transient majoritarian passion.
% TRANSFER_FUNCTION: Moves agenda-control and policy-veto power from electoral majorities to blocking minorities or status quo defenders whenever the supermajority threshold exceeds the actual rate of social consensus formation.
% ABSENT_VOICES: Future generations who will live under potentially ossified institutions; empirical policy analysts who measure reversibility costs but are excluded from normative constitutional debates; minority factions within the blocking coalition who might benefit from reform.
% DISAPPEARANCE_RATIONALE: Without the threshold, majorities could more readily amend constitutions or change procedural rules, altering the distribution of power between reformers and defenders; legislative and constitutional politics would reorganize around simple-majority bargaining.
% FOUNDING_PROBLEM: How to allow constitutional adaptation while preventing rapid, destabilizing change driven by transient or narrow majorities.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars and political historians attest to the stability-flexibility trade-off, though their empirical findings are contested by institutional incumbents who benefit from current thresholds; no neutral party uncontroversially confirms the original problem remains live at current calibration levels.
narrative_ontology:disappearance_verdict(supermajority_threshold__adaptive_gradient_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__adaptive_gradient_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__adaptive_gradient_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__adaptive_gradient_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__adaptive_gradient_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__adaptive_gradient_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__adaptive_gradient_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because supermajority thresholds in practice are often calibrated above actual consensus formation rates, converting a coordination mechanism into a structural veto. Suppression is moderate (0.52) because the rule suppresses majority will procedurally rather than through direct coercion. Theater ratio is moderate (0.42): deliberation under supermajority rules is partly genuine consensus-seeking and partly performative justification of pre-existing opposition. Resistance is elevated (0.62) because blocked majorities regularly contest the threshold's legitimacy. The measurement series tracks rising extraction and theater as political polarization widens the gap between majority preference and supermajority requirements.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (institutional gatekeepers) experiences the threshold as a necessary procedural guardrail maintaining orderly constitutional evolution. The payer seat (reform majorities) experiences the same structure as an illegitimate barrier to democratically backed change. The engine computes this divergence from identical structural data: the gatekeeper's d trends toward beneficiary because they derive institutional authority and stability; the reform majority's d trends toward target because the constraint extracts their electoral power into procedural deadlock.
 *
 * DIRECTIONALITY LOGIC:
 *   Status_quo_defenders are structural beneficiaries: the threshold subsidizes their policy preferences by blocking change they oppose (d near 0.0). Reform_majorities are structural victims: the threshold extracts their majority power and converts it into procedural inertia (d near 1.0). Institutional_gatekeepers sit near symmetric but slightly toward beneficiary because the threshold underwrites their procedural role and institutional stability. Excluded_policy_beneficiaries are trapped targets with no exit.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare captures the reading's core claim: the threshold possesses a genuine coordination function (preventing hasty change) that is inseparable from its asymmetric extraction (blocking majorities). If it were pure snare, the coordination story would be cover; if it were pure rope, there would be no victims. The mandatrophy guard prevents mislabeling: a threshold that has outlived its calibration but persists by inertia would be piton; a threshold with a sunset clause and transitional intent would be scaffold. This threshold lacks both features, remaining an actively enforced hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_formation_rate_ambiguity,
    'What empirical metric captures ''social consensus formation rates'' for threshold calibration, and who authoritatively measures it?',
    'Comparative longitudinal studies of constitutional amendment success rates mapped against social indicators; independent constitutional review boards.',
    'Without agreed measurement, the adaptive gradient reading cannot operationalize its own calibration criterion and risks collapsing into either the consensus safeguard reading (if rates are assumed slow) or the minoritarian veto reading (if rates are assumed impossible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_formation_rate_ambiguity, empirical, 'Empirical ambiguity in measuring the calibration target.').

omega_variable(
    calibration_vs_entrenchment_boundary,
    'When a supermajority threshold persistently blocks majority-backed reforms, does this indicate miscalibration (a fixable pathology) or structural minority veto (an inherent feature)?',
    'Cross-national regression of threshold height against policy change velocity, controlling for polarization and institutional veto points.',
    'If high thresholds inherently produce minority veto regardless of context, the adaptive gradient reading merges empirically with the minoritarian veto reading and loses its distinctive normative claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calibration_vs_entrenchment_boundary, conceptual, 'Boundary between adaptive calibration failure and inherent minoritarian veto.').

omega_variable(
    kernel_reading_decomposition,
    'Does the supermajority_threshold kernel decompose cleanly into three independent constraints, or do the readings collapse into each other under empirical testing?',
    'Empirical calibration testing across multiple jurisdictions; if all high thresholds produce minoritarian veto dynamics, the minoritarian veto reading absorbs the adaptive gradient reading.',
    'Would determine whether the kernel genuinely supports three structurally distinct constraints or merely one constraint with three normative framings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether the kernel decomposition is structurally robust or merely rhetorical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(supe_tr_t50, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(supe_be_t50, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 50, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(supermajority_threshold__adaptive_gradient_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, consensus_safeguard_reading).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, minoritarian_veto_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'supermajority threshold' conflates three structurally distinct normative claims. This file isolates the adaptive_gradient_reading as a separate constraint because its Îµ, beneficiary structure, and legitimacy conditions differ from its siblings. The consensus_safeguard_reading treats the threshold as intrinsically valuable consensus protection with negligible extraction; the minoritarian_veto_reading treats it as structural entrenchment of minority privilege with high extraction. The adaptive gradient reading occupies the intermediate empirical territory where extraction is variable and legitimacy is contingent on calibration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
