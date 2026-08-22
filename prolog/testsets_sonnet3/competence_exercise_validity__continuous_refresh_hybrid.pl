% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: competence_exercise_validity__continuous_refresh_hybrid
 *   human_readable: Continuous Drill-Cycle Requirement for Competence Retention (Hybrid Reading)
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   A high-hazard operating domain (drawn from patterns common to nuclear,
 *   aviation, and process-safety regulation) requires organizations to run
 *   recurring competence drills on a fixed cadence rather than relying on a
 *   one-time simulation-based certification. The rule is presented as
 *   coordination — closing the empirically documented gap between
 *   certified-once and competent-now — but it also imposes recurring costs
 *   unevenly across better- and worse-resourced facilities and shifts real
 *   time and attention away from frontline workers on a permanent, renewing
 *   basis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, 0.42).
domain_priors:suppression_score(competence_exercise_validity__continuous_refresh_hybrid, 0.38).
domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, extractiveness, 0.42).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Continuous Drill-Cycle Requirement for Competence Retention (Hybrid Reading)").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, '26a3b0db-e33f-45d7-80c0-ac663ad950fa').
narrative_ontology:cs_kernel_codification('26a3b0db-e33f-45d7-80c0-ac663ad950fa', distributed).
narrative_ontology:cs_authority_grounding('26a3b0db-e33f-45d7-80c0-ac663ad950fa', expertise).
narrative_ontology:cs_interpretation_layer_present('26a3b0db-e33f-45d7-80c0-ac663ad950fa').
narrative_ontology:cs_reading_relation('26a3b0db-e33f-45d7-80c0-ac663ad950fa', competence_exercise_validity__simulation_as_proxy, coexists_with).
narrative_ontology:cs_reading_relation('26a3b0db-e33f-45d7-80c0-ac663ad950fa', competence_exercise_validity__real_catastrophe_only, influences).
narrative_ontology:cs_axiom('26a3b0db-e33f-45d7-80c0-ac663ad950fa', foundational, competence_is_process_not_state).
narrative_ontology:cs_axiom_status(competence_is_process_not_state, holdable).
narrative_ontology:cs_axiom_grounding('26a3b0db-e33f-45d7-80c0-ac663ad950fa', competence_is_process_not_state, empirically_contingent).
narrative_ontology:cs_axiom('26a3b0db-e33f-45d7-80c0-ac663ad950fa', secondary, simulation_necessary_but_insufficient_for_retention).
narrative_ontology:cs_axiom_status(simulation_necessary_but_insufficient_for_retention, holdable).
narrative_ontology:cs_axiom_grounding('26a3b0db-e33f-45d7-80c0-ac663ad950fa', simulation_necessary_but_insufficient_for_retention, empirically_contingent).
narrative_ontology:cs_reference_frame('26a3b0db-e33f-45d7-80c0-ac663ad950fa', post_incident_decay_gap_recognition).
narrative_ontology:cs_drift_state('26a3b0db-e33f-45d7-80c0-ac663ad950fa', contemporary_audit_regime, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('26a3b0db-e33f-45d7-80c0-ac663ad950fa', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, operating_organization).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, regulatory_bodies).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, shift_crews_bearing_drill_load).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, budget_constrained_facilities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, competence_is_process_not_state).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, safety_record_validates_exercise_cadence_not_simulation_fidelity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates a recurring drill calendar (quarterly, annual, cross-shift) as the operative safety standard, justified by the claim that competence decays without repeated exercise regardless of any prior simulation score. Bears the cost of running the program and the liability exposure if it under-schedules drills, but also captures the reputational and insurance benefits of a documented continuous-training regime.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, operating_organization, agenda_setter,
    institutional, generational, constrained, national).

% Write the cadence requirements into licensing conditions and audit against drill logs rather than one-time certification records. Benefit from being able to point to an ongoing compliance record as evidence of oversight, and from shifting the operational burden of proof onto operators rather than onto the regulator's own inspection capacity.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, regulatory_bodies, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, regulatory_bodies, agenda_setter).

% Retain procedural fluency through repeated exposure and are less likely to freeze or misapply steps during a real incident because the drill cycle keeps the sequence fresh. Also lose paid working hours, rest time, or overtime slots to the drill schedule, and face performance scrutiny during each cycle that a one-time-validated peer does not.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators, payer).

% Absorb the recurring disruption of drills layered onto normal shift work, repeatedly, for the life of their employment. Cannot opt out without risking certification lapse or disciplinary action; the requirement is renewed every cycle rather than discharged once, so the cost is permanent rather than a single onboarding expense.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, shift_crews_bearing_drill_load, payer,
    moderate, biographical, constrained, local).

% Smaller or under-resourced operators must fund the same drill cadence as well-capitalized peers without economies of scale, diverting money from equipment maintenance or staffing to keep the recurring exercise program compliant. Cannot exit the requirement without losing licensure, and cannot negotiate a lighter cadence without appearing to argue for lower safety standards.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, budget_constrained_facilities, payer,
    powerless, biographical, trapped, regional).

% Would prefer the kernel resolve toward one-time high-fidelity validation, which is their product's comparative advantage, but their preferred framing has no seat in the recurring-cadence standard-setting process; they can sell into the drill-cycle regime but cannot argue it out of existence from within it.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, simulation_vendors, excluded,
    organized, biographical, constrained, national).

% Study incident records and drill-compliance histories to assess whether continuous cadence correlates with lower real-world failure rates, independent of any single simulation's fidelity score. Their findings feed back into cadence design but they neither run nor pay for the drills.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__continuous_refresh_hybrid, diffuse).
narrative_ontology:fixing_cost_class(competence_exercise_validity__continuous_refresh_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, auditable standard by which an entire industry keeps procedural competence fresh over time, so that no single organization can claim compliance by pointing to a stale one-time credential while its workforce has actually drifted out of practice.
% TRANSFER_FUNCTION: Moves working hours, budget, and administrative attention from operating organizations and shift crews into a recurring exercise program; moves reputational and legal cover toward the operating organization and regulators who can point to an unbroken compliance log.
% ABSENT_VOICES: Simulation vendors whose commercial interest favors one-time high-fidelity certification are not seated in the cadence-setting process. Workers who bear the recurring disruption have limited standing to negotiate cadence relative to management and regulators who set it.
% DISAPPEARANCE_RATIONALE: If the continuous-cadence requirement vanished, organizations would revert to point-in-time certification; budget-constrained facilities would immediately redirect the freed funds elsewhere, licensing audits would lose their recurring evidentiary basis, and the felt time pressure on shift crews would disappear — a substantial and visible rearrangement, not a null change.
% FOUNDING_PROBLEM: Post-incident investigations repeatedly found that operators who had passed a one-time simulation years earlier had visibly decayed procedural competence by the time a real event occurred; the cadence requirement was built to close that decay gap.
% FOUNDING_PROBLEM_CORROBORATION: Independent incident-investigation boards and academic human-factors researchers outside the regulatory and operator community corroborate that skill decay after single validation events is a real, measured phenomenon in high-hazard operations; this is not solely attested by the parties who administer or profit from the drill program.
narrative_ontology:disappearance_verdict(competence_exercise_validity__continuous_refresh_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__continuous_refresh_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__continuous_refresh_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__continuous_refresh_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__continuous_refresh_hybrid, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42 at interval end) because the recurring drill requirement does impose a real, renewing cost disproportionately on budget-constrained facilities and shift crews, but the coordination function is genuine and well-evidenced by post-incident investigation records, not merely asserted. Suppression is moderate (0.38): exit from the cadence requirement is blocked by licensing conditions, but this is closer to a genuine safety floor than to naked coercion. Theater ratio rises modestly over the interval (0.15 to 0.28) as some facilities shift toward documentation-optimized drills that satisfy the audit trail without necessarily improving retention — a mild Goodhart drift worth watching but not yet dominant.
 *
 * PERSPECTIVAL GAP:
 *   From the operating organization's seat, the cadence requirement reads as functional coordination that closes a documented decay gap. From a budget-constrained facility's seat, the same requirement reads closer to an unfunded mandate renewed indefinitely with no sunset — the engine's per-seat computation should reflect that these are structurally different experiences of one arrangement, not a disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Operating organizations and regulators sit near the beneficiary end: they capture compliance cover and oversight legibility from the cadence record. Frontline operators are genuinely dual-positioned — they gain real competence retention (a benefit to their own safety and job security) while paying in disrupted schedules and repeated scrutiny (a cost), which is why they carry both roles. Shift crews as a class and budget-constrained facilities sit nearer the target end because the recurring cost lands on them without a correspondingly recurring capture of the compliance benefit that accrues to the organization as a whole.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (documented skill decay after one-time certification) remains live and is corroborated by parties outside the regulatory and operator community, which is what keeps this from being classified as a captured or zombie mandate — the six_questions mismatch check (status=live, verdict=world_rearranges) shows no capture flag. This distinguishes the constraint from a piton: the cadence requirement still does real work, even though its administration shows early theater-ratio drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cadence_sufficiency_vs_decay_curve,
    'Is the specific drill cadence mandated actually matched to the empirical skill-decay curve for the tasks in question, or is the cadence set by administrative convenience (quarterly/annual cycles) rather than by the measured decay rate?',
    'Longitudinal skill-retention studies comparing drill interval against measured competence decay for the specific procedures being drilled, across multiple facilities and cadences.',
    'If cadence is mismatched to actual decay rates, part of the measured extraction is unnecessary overhead rather than genuine coordination cost, and the tangled_rope classification would shift further toward extraction; if well-matched, more of the cost is defensible coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cadence_sufficiency_vs_decay_curve, empirical, 'Whether mandated drill cadence tracks actual measured skill decay or administrative convenience.').

omega_variable(
    committer_kernel_disagreement_location,
    'This story reads the competence_exercise_validity kernel as process-dependent (continuous exercise required); sibling readings hold simulation alone suffices (simulation_as_proxy) or that only real catastrophe suffices (real_catastrophe_only). Where exactly does the disagreement sit — is it about the decay mechanism (do skills actually decay without repetition), the substitutability of simulated stakes for real stakes, or both?',
    'Decompose the disagreement empirically: decay-curve studies would resolve the first axis; comparative outcome studies of drilled-but-never-incident-exposed personnel versus incident-exposed personnel would speak to the second, though ethically constrained.',
    'If the disagreement is purely about decay (empirically resolvable), this reading and simulation_as_proxy could converge on cadence once decay curves are known. If it is about stakes-substitutability, the disagreement with real_catastrophe_only may be irreducible on current evidence — it touches what ''counts'' as genuine exercise, a conceptual rather than empirical question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_disagreement_location, conceptual, 'Locating whether the kernel disagreement is empirical (decay rates) or conceptual (what counts as valid exercise).').

omega_variable(
    theater_drift_detection_threshold,
    'At what theater_ratio level does documentation-optimized drilling (satisfying the audit trail) begin to functionally replace competence-building drilling (actually retaining skill)?',
    'Compare facilities'' drill documentation quality against independent skill assessments (blind competency tests) to see where the two measures diverge.',
    'If theater ratio continues rising past the current 0.28 without a corresponding independent competency check, the constraint risks drifting from tangled_rope toward piton — recurring cost with diminishing genuine function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_drift_detection_threshold, empirical, 'Whether rising theater ratio signals early piton drift in the drill program.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comp_tr_t4, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 4, 0.17).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 8, 0.2).
narrative_ontology:measurement(comp_tr_t12, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 12, 0.22).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 16, 0.24).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 20, 0.26).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comp_be_t4, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(comp_be_t8, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(comp_be_t12, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(comp_be_t16, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(comp_be_t24, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(comp_su_t4, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(comp_su_t8, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(comp_su_t12, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(comp_su_t16, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(comp_su_t24, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__continuous_refresh_hybrid, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, real_catastrophe_only).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the competence_exercise_validity kernel. simulation_as_proxy holds that a sufficiently fidelitous simulation is itself valid exercise, making recurring drilling largely redundant once fidelity is high; real_catastrophe_only holds that no simulation, however repeated, substitutes for the stakes structure of an actual event. This reading (continuous_refresh_hybrid) occupies the middle position: simulation is necessary infrastructure but retention is process-dependent, requiring the recurring cadence that the other two readings would treat as either unnecessary (simulation_as_proxy, once fidelity is achieved) or insufficient regardless of frequency (real_catastrophe_only). Each reading is authored as an independent ε-invariant constraint; do not average across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
