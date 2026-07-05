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
 *   human_readable: Continuous Drill Cycle Requirement for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the continuous_refresh_hybrid reading of the
 *   competence_exercise_validity kernel: neither the position that simulation
 *   alone permanently validates competence (simulation_as_proxy) nor the
 *   position that only a real catastrophe genuinely exercises it
 *   (real_catastrophe_only), but the claim that competence is a maintained
 *   process requiring recurring drill cycles, where simulation is necessary
 *   infrastructure but any single simulation event is insufficient on its
 *   own. The constraint is the institutional requirement — enforced through
 *   certification cadence, audit, and budget allocation — that drills recur
 *   rather than terminate after initial validation. The coordination function
 *   (retained hazard-response competence) is real and the extraction
 *   (recurring cost falling disproportionately on shift workers and training
 *   budgets, with organizations capturing the risk-reduction benefit) is also
 *   real, which is why this reading is claimed as tangled_rope rather than a
 *   pure rope or pure mountain.
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
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Continuous Drill Cycle Requirement for Competence Retention").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, 'ebc68276-4bee-4bc1-bf34-fc52cab6356b').
narrative_ontology:cs_kernel_codification('ebc68276-4bee-4bc1-bf34-fc52cab6356b', distributed).
narrative_ontology:cs_authority_grounding('ebc68276-4bee-4bc1-bf34-fc52cab6356b', expertise).
narrative_ontology:cs_interpretation_layer_present('ebc68276-4bee-4bc1-bf34-fc52cab6356b').
narrative_ontology:cs_reading_relation('ebc68276-4bee-4bc1-bf34-fc52cab6356b', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('ebc68276-4bee-4bc1-bf34-fc52cab6356b', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_axiom('ebc68276-4bee-4bc1-bf34-fc52cab6356b', foundational, competence_is_process_not_state).
narrative_ontology:cs_axiom_status(competence_is_process_not_state, holdable).
narrative_ontology:cs_axiom_grounding('ebc68276-4bee-4bc1-bf34-fc52cab6356b', competence_is_process_not_state, empirically_contingent).
narrative_ontology:cs_axiom('ebc68276-4bee-4bc1-bf34-fc52cab6356b', foundational, simulation_necessary_but_not_terminal).
narrative_ontology:cs_axiom_status(simulation_necessary_but_not_terminal, holdable).
narrative_ontology:cs_axiom_grounding('ebc68276-4bee-4bc1-bf34-fc52cab6356b', simulation_necessary_but_not_terminal, empirically_contingent).
narrative_ontology:cs_reference_frame('ebc68276-4bee-4bc1-bf34-fc52cab6356b', process_dependent_competence_maintenance).
narrative_ontology:cs_drift_state('ebc68276-4bee-4bc1-bf34-fc52cab6356b', post_high_reliability_organization_research_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ebc68276-4bee-4bc1-bf34-fc52cab6356b', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, operating_organizations).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, public_downstream_of_hazard).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, shift_workers_bearing_drill_burden).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, training_budget_line_managers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators).
narrative_ontology:constraint_vindicates(competence_exercise_validity__continuous_refresh_hybrid, process_dependent_competence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the drill cadence and enforces it through certification requirements, audit schedules, and shift rostering that builds in recurring simulation time. Justifies the recurring cost by pointing to incident data showing skill decay between events. Bears the direct cost of running drills but avoids the far larger cost of a real failure caused by atrophied response capability.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, operating_organizations, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, operating_organizations, beneficiary).

% Undergo repeated simulation cycles that keep procedural memory and muscle memory current. Benefit from genuinely higher survival and success odds during real events, but also bear the fatigue, scheduling disruption, and repeated performance-anxiety cost of drills that never end and are never 'passed' permanently.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators, payer).

% Absorb the recurring time cost of drills stacked onto already demanding shift schedules — lost rest, lost overtime pay opportunities, mandatory attendance regardless of fatigue state. Cannot opt out without risking certification lapse and employment consequences; the continuous-cycle requirement falls hardest on workers with least schedule flexibility.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, shift_workers_bearing_drill_burden, payer,
    moderate, biographical, constrained, local).

% Must fund recurring drill infrastructure indefinitely rather than a one-time validation expense. Face perpetual budget pressure to prove the ongoing spend is still justified against competing capital priorities, without a clean endpoint at which the investment is 'done.'
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, training_budget_line_managers, payer,
    moderate, biographical, constrained, national).

% Lives or works near the hazardous facility or system and depends entirely on operator competence during a real event. Has no visibility into whether drill cycles are actually maintained at effective frequency and no direct lever to demand it beyond regulatory proxies.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, public_downstream_of_hazard, beneficiary,
    powerless, generational, trapped, regional).

% Audits drill records, sets minimum recurrence intervals, and can escalate enforcement when cadence lapses. Draws on incident investigation data across many operators to calibrate what recurrence interval actually correlates with retained competence, rather than trusting any single operator's self-report.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators, agenda_setter).

% Argue drills are proxy-catastrophe events that fully substitute for real exposure and that a single well-designed simulation validates competence indefinitely. Not seated in the cadence-setting process; their position would eliminate the recurring cost this constraint imposes, so they have no institutional voice in setting drill frequency.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, simulation_equals_catastrophe_advocates, excluded,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_validity__continuous_refresh_hybrid, diffuse).
narrative_ontology:fixing_cost_class(competence_exercise_validity__continuous_refresh_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a recurring institutional commitment — funded, scheduled, audited — to keep hazard-response competence at a functional level across personnel turnover, memory decay, and procedure drift, solving the genuine problem that skills learned once erode without practice.
% TRANSFER_FUNCTION: Moves scheduling flexibility, rest time, and discretionary budget from shift workers and training managers into recurring drill infrastructure, in exchange for moving catastrophic-failure risk away from the public and frontline operators during real events.
% ABSENT_VOICES: Advocates of the simulation-as-sufficient-proxy view and advocates of the real-catastrophe-only view are both excluded from the cadence-setting table; the former would argue for eliminating recurring cost after initial certification, the latter would argue no simulation regime is adequate at any cadence. Neither framing shapes the actual recurrence interval regulators set.
% DISAPPEARANCE_RATIONALE: If the continuous-cycle requirement vanished and organizations reverted to one-time validation, certification would become a permanent credential rather than a maintained state; budget lines for recurring drills would be reallocated; and, per the documented skill-decay literature this reading relies on, response competence would degrade between real events with no institutional mechanism to detect or correct the drift until a real incident exposed it.
% FOUNDING_PROBLEM: Early safety regimes validated competence once at initial certification, then discovered during real incidents that responders whose only exposure was a single passed simulation years earlier could not perform under pressure — memory and procedural fluency had decayed silently with no intervening signal.
% FOUNDING_PROBLEM_CORROBORATION: Independent incident-investigation boards across multiple regulatory jurisdictions attest to decay-driven response failures in post-incident reports, and academic skill-retention literature outside the operating organizations' own training departments corroborates that procedural competence measurably decays on timescales shorter than typical one-time-certification intervals.
narrative_ontology:disappearance_verdict(competence_exercise_validity__continuous_refresh_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__continuous_refresh_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__continuous_refresh_hybrid, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.42) reflects that the recurring cost of drill cycles is real and unevenly distributed — organizations get the safety benefit and reputational insurance while shift workers absorb the scheduling and fatigue cost without proportional compensation. Suppression (0.38) is moderate: certification lapse consequences create real coercive pressure on workers and training managers to comply with the cadence, though it is not maximal because the underlying safety rationale is broadly accepted rather than purely coercive. Theater ratio (0.28) is nontrivially above zero and rising over the interval because as drill cycles mature into routine, a growing share of drill time in some regimes shifts toward documentation and box-checking rather than genuinely adversarial rehearsal — this is the Goodhart risk this reading must watch for even while defending the underlying continuous-cycle logic. Accessibility collapse (0.45) and resistance (0.4) are moderate: alternative regimes (one-time validation, or no simulation at all) remain conceivable and are actively argued for by excluded parties, so alternatives have not fully collapsed the way they would for a genuine mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Operating organizations and public downstream beneficiaries sit toward the beneficiary end: the organization avoids catastrophic liability and reputational collapse, and the public receives protection it cannot directly monitor or price. Shift workers and training-budget managers sit toward the target end: they bear the recurring, non-terminating cost of a requirement that has no completion state, with constrained exit because certification lapse threatens employment or institutional standing. Frontline operators are dual-positioned — genuine beneficiaries of improved real-event survival odds, but also payers of the recurring drill burden, which is why they carry a secondary payer role rather than a single clean directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (silent competence decay after one-time validation, discovered through real incident failures) remains live and independently corroborated by incident-investigation boards outside the operating organizations, which is precisely what distinguishes this reading from a stalled mandate: the recurring cost persists because the decay mechanism it defends against continues to operate, not because of institutional inertia alone. Classifying this as tangled_rope rather than snare prevents mislabeling a genuine, evidence-grounded coordination function as pure extraction merely because its ongoing cost falls unevenly — the risk this reading must guard against is the rising theater_ratio trajectory tipping the balance toward performative drilling that no longer tracks retained competence, at which point the same structure would drift toward piton or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decay_rate_empirical_basis,
    'Is the specific drill recurrence interval mandated by regulators actually calibrated to the true skill-decay curve for the hazard type in question, or is it a historically inherited round-number cadence (annual, quarterly) that has never been re-derived from decay data?',
    'Cross-jurisdictional comparison of incident rates against varying drill-interval regimes, controlling for hazard type and workforce turnover, to establish whether the current cadence is evidence-derived or convention-derived.',
    'If the interval is convention-derived rather than decay-derived, the extractiveness of the recurring requirement is higher than the safety justification supports, and the theater_ratio trajectory would be expected to accelerate as the mismatch between mandated cadence and actual decay curve widens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_rate_empirical_basis, empirical, 'Whether drill recurrence intervals are decay-calibrated or historically inherited.').

omega_variable(
    reading_boundary_simulation_sufficiency,
    'At what point, if any, does a sufficiently sophisticated and sufficiently frequent simulation regime become indistinguishable in practice from the continuous_refresh_hybrid reading''s requirements — collapsing the distinction with simulation_as_proxy?',
    'Compare high-fidelity, high-frequency simulation regimes against this reading''s cadence requirements to determine whether the two readings converge in practice even while diverging in stated justification.',
    'If they converge operationally, the kernel dispute is more about justificatory framing (process-dependent vs. state-validated) than about actual drill practice, which would reduce the practical stakes of the reading contest even though the underlying axioms remain distinct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_simulation_sufficiency, conceptual, 'Whether this reading and simulation_as_proxy converge in operational practice despite differing foundational claims.').

omega_variable(
    worker_burden_distribution_fairness,
    'Is the uneven distribution of drill burden onto shift workers with the least schedule flexibility a necessary feature of continuous-cycle competence maintenance, or a remediable allocation choice made by operating organizations to minimize their own scheduling cost?',
    'Compare organizations that compensate drill time at premium rates or rotate drill burden equitably against those that do not, controlling for retained-competence outcomes, to see whether burden distribution is separable from the coordination function.',
    'If separable, a substantial share of the measured extractiveness is a remediable allocation choice rather than an inherent cost of the coordination function, which would support reclassifying the burden-distribution component (not the drill requirement itself) as more purely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_burden_distribution_fairness, preference, 'Whether uneven worker burden is inherent to the coordination function or a separable allocation choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 8, 0.18).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 16, 0.21).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 24, 0.24).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 32, 0.26).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comp_be_t8, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(comp_be_t16, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(comp_be_t24, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(comp_be_t32, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(comp_be_t40, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(comp_su_t8, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(comp_su_t16, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 16, 0.32).
narrative_ontology:measurement(comp_su_t24, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(comp_su_t32, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(comp_su_t40, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__continuous_refresh_hybrid, 0.12).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__continuous_refresh_hybrid, real_catastrophe_only).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the competence_exercise_validity kernel. simulation_as_proxy claims lower extraction and no recurring cost (single validation event treated as sufficient); real_catastrophe_only claims that no simulation regime, however recurring, satisfies the competence claim, and would classify any simulation-based drill regime as insufficient regardless of cadence. This reading (continuous_refresh_hybrid) sits structurally between them: it accepts simulation's necessity (against real_catastrophe_only) while rejecting its one-time sufficiency (against simulation_as_proxy), producing a distinct beneficiary/victim structure (recurring cost distribution) and a distinct ε profile (moderate, tangled_rope) that neither sibling reading shares.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
