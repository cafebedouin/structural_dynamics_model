% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__continuous_refresh_hybrid, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: competence_exercise_validity__continuous_refresh_hybrid
 *   human_readable: Continuous Competence Refresh Drill Cycles
 *   domain: safety_engineering/organizational_learning/competence_retention
 *
 * SUMMARY:
 *   This constraint mandates continuous drill cycles for competence retention
 *   in high-hazard industries (nuclear, chemical, aviation, offshore). The
 *   reading asserts simulation is necessary but not sufficient — competence
 *   is process-dependent, requiring recurring cycles that exercise
 *   decision-making under evolving conditions, not one-time validation. The
 *   safety record of industries with continuous drill regimes (commercial
 *   nuclear, commercial aviation) validates the approach, but the drill
 *   regime itself must adapt to avoid becoming ritualized compliance theater.
 *   This is one reading of the contested kernel
 *   'competence_exercise_validity': the continuous_refresh_hybrid reading.
 *   Sibling readings are simulation_as_proxy (simulation counts as valid
 *   exercise) and real_catastrophe_only (only real events truly exercise
 *   competence).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, 0.35).
domain_priors:suppression_score(competence_exercise_validity__continuous_refresh_hybrid, 0.25).
domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, extractiveness, 0.35).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, scaffold).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Continuous Competence Refresh Drill Cycles").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety_engineering/organizational_learning/competence_retention").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).
narrative_ontology:has_sunset_clause(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, '82283160-04aa-4550-ae5d-c82ab26d6d0a').
narrative_ontology:cs_kernel_codification('82283160-04aa-4550-ae5d-c82ab26d6d0a', distributed).
narrative_ontology:cs_authority_grounding('82283160-04aa-4550-ae5d-c82ab26d6d0a', practice).
narrative_ontology:cs_interpretation_layer_present('82283160-04aa-4550-ae5d-c82ab26d6d0a').
narrative_ontology:cs_reading_relation('82283160-04aa-4550-ae5d-c82ab26d6d0a', competence_exercise_validity__simulation_as_proxy, influences).
narrative_ontology:cs_reading_relation('82283160-04aa-4550-ae5d-c82ab26d6d0a', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_axiom('82283160-04aa-4550-ae5d-c82ab26d6d0a', foundational, competence_requires_recurring_novelty).
narrative_ontology:cs_axiom_status(competence_requires_recurring_novelty, holdable).
narrative_ontology:cs_axiom_grounding('82283160-04aa-4550-ae5d-c82ab26d6d0a', competence_requires_recurring_novelty, empirically_contingent).
narrative_ontology:cs_axiom('82283160-04aa-4550-ae5d-c82ab26d6d0a', foundational, safety_record_validates_process_not_proxy).
narrative_ontology:cs_axiom_status(safety_record_validates_process_not_proxy, holdable).
narrative_ontology:cs_axiom_grounding('82283160-04aa-4550-ae5d-c82ab26d6d0a', safety_record_validates_process_not_proxy, empirically_contingent).
narrative_ontology:cs_reference_frame('82283160-04aa-4550-ae5d-c82ab26d6d0a', continuous_practice_competence_model).
narrative_ontology:cs_drift_state('82283160-04aa-4550-ae5d-c82ab26d6d0a', post_digital_twin_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('82283160-04aa-4550-ae5d-c82ab26d6d0a', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, operating_personnel).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, regulatory_authorities).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, insurance_underwriters).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, public_stakeholders).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, resource_constrained_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, specialist_contractors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, specialist_contractors).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, competence_is_process_dependent).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, safety_record_validates_continuous_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates continuous drill cycles for licensed operators. Sets frequency, scope, and evaluation criteria. Can revoke licenses for non-compliance. Bears political cost if drills prove inadequate during actual events.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_regulator, agenda_setter,
    institutional, generational, analytical, national).

% Participate in mandated drill cycles. Gain maintained competency, reduced error rates, career protection. Cannot easily exit the drill regime without leaving the profession. Cost is time away from production duties.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, operating_personnel, beneficiary,
    organized, biographical, constrained, local).

% Small-to-mid operators who bear disproportionate cost of drill cycles relative to revenue. Must allocate staff time, facility downtime, scenario development. Cannot easily absorb costs or outsource. Exit means ceasing operations.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, resource_constrained_operators, payer,
    moderate, biographical, constrained, local).

% Provide scenario design, facilitation, and evaluation services. Benefit from recurring demand but face competitive pressure on pricing. Must continuously update methodologies. Can exit to adjacent markets but lose domain-specific reputation.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, specialist_contractors, payer,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, specialist_contractors, beneficiary).

% Gain demonstrable safety oversight framework. Drill compliance data supports regulatory legitimacy and budget justification. Cost is enforcement bureaucracy and political exposure when drills miss emerging failure modes.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, regulatory_authorities, beneficiary,
    institutional, generational, analytical, national).

% Use drill compliance and outcomes for risk rating. Lower premiums for operators with strong drill records. Benefit from reduced claim volatility. Can adjust models or exit markets if drill data proves non-predictive.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, insurance_underwriters, beneficiary,
    organized, biographical, arbitrage, global).

% Communities near hazardous facilities. Bear catastrophic risk if competence fails. No meaningful exit. Gain safety assurance from drill regime but have no voice in its design or frequency.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, public_stakeholders, beneficiary,
    powerless, generational, trapped, local).

% Study drill effectiveness, competence decay curves, transfer of training. Publish findings that influence regulatory standards. No direct stake in operational outcomes.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, academic_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures operating personnel maintain competence for low-frequency, high-consequence events through structured, recurring practice cycles that prevent skill decay and organizational forgetting.
% TRANSFER_FUNCTION: Moves operational capacity (staff time, facility access, scenario development resources) from production to drill execution; moves risk reduction (demonstrated competence, regulatory standing) from drill execution to operators and public.
% ABSENT_VOICES: Frontline workers in adjacent industries with different drill regimes; communities that have experienced actual catastrophes and contest drill adequacy; whistleblowers who observed drill gaming or checkbox compliance.
% DISAPPEARANCE_RATIONALE: If continuous drill mandates vanished overnight, operators would revert to episodic or compliance-only training within 1-2 budget cycles. Competence decay would accelerate. First major incident would trigger regulatory overreaction with more prescriptive, less adaptive mandates. Insurance markets would reprice risk immediately.
% FOUNDING_PROBLEM: Post-incident investigations (e.g., Three Mile Island, Bhopal, Deepwater Horizon) revealed that one-time certification and infrequent drills failed to maintain competence for novel failure modes. Organizations forgot lessons between events.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear Regulatory Commission SOARCA studies; IChemE Learning from Incidents database; commercial aviation LOSA data; all from outside the direct beneficiary set of drill vendors and regulators.
narrative_ontology:disappearance_verdict(competence_exercise_validity__continuous_refresh_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__continuous_refresh_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__continuous_refresh_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(competence_exercise_validity__continuous_refresh_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__continuous_refresh_hybrid, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).
:- end_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) and rising: the drill regime extracts increasing resource commitments from operators, especially smaller ones, while the coordination benefit (maintained competence) is real but harder to quantify. Theater ratio is rising (0.4) as drill scenarios become standardized and 'passing' replaces genuine challenge. Suppression is low (0.25) — the constraint operates through professional norms and licensing, not coercion. Accessibility collapse is moderate (0.45): alternatives like pure simulation or pure apprenticeship exist but are not accepted by regulators. Resistance is moderate (0.30): resource-constrained operators push back on frequency and scope.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator's seat, the drill regime is essential coordination infrastructure. From the resource-constrained operator's seat, it is an escalating cost burden with diminishing marginal returns. From the public_stakeholder's seat, it is the only assurance they have — but they cannot evaluate its adequacy. The engine should compute different effective types across these seats: scaffold for the regulator, tangled_rope for the resource-constrained operator, snare-adjacent for public stakeholders if drills prove inadequate.
 *
 * DIRECTIONALITY LOGIC:
 *   The safety_regulator is the agenda_setter (institutional power, analytical exit) — they set the rules but bear political risk if drills fail to predict real events. Operating_personnel are beneficiaries (organized power, constrained exit) — they gain competence maintenance but cannot exit the regime without leaving the profession. Resource_constrained_operators are payers (moderate power, constrained exit) — they bear disproportionate cost. Specialist_contractors are dual-role (payer/beneficiary, mobile exit) — they profit from the regime but face competitive pressure. Regulatory_authorities and insurance_underwriters are beneficiaries with analytical/arbitrage exit. Public_stakeholders are trapped beneficiaries (powerless, no exit) — they bear catastrophic risk but have no voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competence decay between rare events) remains live — major incidents still reveal competence gaps. But the drill regime shows mandatrophy signals: theater ratio rising, scenarios standardizing, compliance substituting for challenge. The sunset clause (periodic regulatory review of drill requirements) exists but has not triggered meaningful reduction — the regime expands rather than contracts. This is a scaffold that has not sunset because the problem it addresses is genuinely persistent, but its current form may have outlived its optimal design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drill_vs_simulation_boundary,
    'Where is the structural boundary between a drill cycle that maintains competence and a simulation exercise that merely certifies compliance?',
    'Longitudinal studies correlating drill fidelity metrics (scenario novelty, decision latency, error recovery) with subsequent operational performance and incident response quality.',
    'If the boundary is sharp, the constraint''s coordination function is real and theater is contamination. If the boundary is porous, the constraint may be a scaffold whose coordination function has been hollowed out by simulation substitution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_vs_simulation_boundary, empirical, 'Whether drill cycles and simulation exercises are structurally distinct competence-maintenance mechanisms.').

omega_variable(
    mandatrophy_threshold,
    'At what point does rising theater ratio indicate the drill regime has become performative maintenance rather than genuine competence retention?',
    'Regulatory review cycles that mandate scenario retirement, novelty injection, and pass/fail criterion recalibration; track whether these interventions reset theater ratio.',
    'If theater ratio cannot be reset by regulatory intervention, the constraint has become a piton. If reset is possible but not done, it is a scaffold with failed sunset mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_threshold, conceptual, 'The threshold where scaffold transitions to piton in continuous drill regimes.').

omega_variable(
    reading_disagreement_locus,
    'What specific structural element do the three readings of competence_exercise_validity disagree on?',
    'Map each reading''s predicted competence decay curve under its prescribed regime; compare against incident databases where regime type is known.',
    'If readings predict measurably different decay curves, the kernel contest is empirically resolvable. If curves converge, the contest is framing rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_disagreement_locus, empirical, 'Structural locus of disagreement between continuous_refresh_hybrid, simulation_as_proxy, and real_catastrophe_only readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 8, 0.25).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 16, 0.3).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 24, 0.35).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 32, 0.38).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(comp_be_t8, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(comp_be_t16, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(comp_be_t24, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 24, 0.32).
narrative_ontology:measurement(comp_be_t32, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 32, 0.34).
narrative_ontology:measurement(comp_be_t40, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 40, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(comp_su_t8, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 8, 0.18).
narrative_ontology:measurement(comp_su_t16, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 16, 0.22).
narrative_ontology:measurement(comp_su_t24, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 24, 0.24).
narrative_ontology:measurement(comp_su_t32, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 32, 0.25).
narrative_ontology:measurement(comp_su_t40, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 40, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__continuous_refresh_hybrid, 0.1).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, regulatory_compliance_burden).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, operator_training_budget_allocation).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, incident_investigation_mandates).

% DUAL FORMULATION NOTE:
% Part of competence_exercise_validity kernel family. continuous_refresh_hybrid reading asserts process-dependent competence retention validated by safety record. simulation_as_proxy reading asserts simulation suffices. real_catastrophe_only reading asserts only real events exercise competence. All three constrain the same operator population but with different extraction/suppression profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
