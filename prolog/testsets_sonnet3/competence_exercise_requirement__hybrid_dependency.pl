% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__hybrid_dependency, []).

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
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Hybrid Competence Maintenance Regime: Simulation Foundation Plus Real-World Anchoring
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This story instantiates the 'hybrid_dependency' reading of the
 *   competence-exercise-requirement kernel: the claim that neither pure
 *   simulation nor pure real-world exercise is sufficient on its own, and
 *   that competence for rare, catastrophic failure modes is maintained only
 *   by a regime combining simulator foundation with periodic real-world
 *   anchoring — line operations, non-jeopardy audits, and actual aircraft
 *   time. This is a distinct constraint from the sibling readings
 *   'simulation_as_adequate_exercise' (which asserts high-fidelity simulation
 *   alone suffices) and 'catastrophe_as_necessary_anchor' (which asserts only
 *   real catastrophic events provide irreducible exercise). Each reading has
 *   its own ε: this reading's extraction is moderate because the hybrid
 *   regime is a genuinely defensible coordination structure, though cost
 *   pressure creates a persistent temptation to erode the real-world
 *   component toward cheaper simulation, which is where extraction enters —
 *   operators and regulators can quietly shift the ratio without junior crew
 *   or passengers having visibility into the substitution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.38).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.42).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Hybrid Competence Maintenance Regime: Simulation Foundation Plus Real-World Anchoring").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, '4f84a69b-b8a2-4e90-bc89-7aeef593352a').
narrative_ontology:cs_kernel_codification('4f84a69b-b8a2-4e90-bc89-7aeef593352a', formalized).
narrative_ontology:cs_authority_grounding('4f84a69b-b8a2-4e90-bc89-7aeef593352a', expertise).
narrative_ontology:cs_interpretation_layer_present('4f84a69b-b8a2-4e90-bc89-7aeef593352a').
narrative_ontology:cs_reading_relation('4f84a69b-b8a2-4e90-bc89-7aeef593352a', competence_exercise_requirement__simulation_as_adequate_exercise, coexists_with).
narrative_ontology:cs_reading_relation('4f84a69b-b8a2-4e90-bc89-7aeef593352a', competence_exercise_requirement__catastrophe_as_necessary_anchor, influences).
narrative_ontology:cs_axiom('4f84a69b-b8a2-4e90-bc89-7aeef593352a', foundational, simulation_necessary_but_structurally_insufficient).
narrative_ontology:cs_axiom_status(simulation_necessary_but_structurally_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('4f84a69b-b8a2-4e90-bc89-7aeef593352a', simulation_necessary_but_structurally_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('4f84a69b-b8a2-4e90-bc89-7aeef593352a', foundational, non_jeopardy_real_exercise_bridges_transfer_gap).
narrative_ontology:cs_axiom_status(non_jeopardy_real_exercise_bridges_transfer_gap, holdable).
narrative_ontology:cs_axiom_grounding('4f84a69b-b8a2-4e90-bc89-7aeef593352a', non_jeopardy_real_exercise_bridges_transfer_gap, empirically_contingent).
narrative_ontology:cs_reference_frame('4f84a69b-b8a2-4e90-bc89-7aeef593352a', post_simulator_era_mixed_training_standard).
narrative_ontology:cs_drift_state('4f84a69b-b8a2-4e90-bc89-7aeef593352a', contemporary_cost_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4f84a69b-b8a2-4e90-bc89-7aeef593352a', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, airline_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, regulatory_bodies).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, passengers_and_public).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, line_pilots).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, junior_flight_crew).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, line_pilots).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, simulator_technology_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the training curriculum, sets the mix of simulator hours versus line-operating experience, and schedules non-jeopardy audits. Bears the direct cost of simulator time and of pulling aircraft and crew out of revenue service for real-world anchoring, but benefits from the safety record, insurance rates, and regulatory standing that a credible hybrid regime produces. Can lobby to shift the ratio toward cheaper simulation without necessarily bearing the tail-risk cost of doing so.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, airline_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, airline_operators, beneficiary).

% Sets minimum requirements for real-world anchoring (line-operating experience, recurrent checks, non-jeopardy audits) alongside simulator qualification. Benefits from a defensible safety framework but does not itself bear the operational cost of delivering it; can be captured by operator pressure to relax real-world requirements in favor of cheaper, more scalable simulation.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, regulatory_bodies, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, regulatory_bodies, observer).

% Undergoes both simulator training and periodic real-world line checks and audits; carries the personal cost of recurrent evaluation, schedule disruption, and the psychological load of non-jeopardy audits that nonetheless feel jeopardous. Benefits from genuine competence maintenance that protects their own survival and license, but has no control over whether the hybrid ratio is honored or eroded toward pure simulation to save the operator money.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, line_pilots, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, line_pilots, beneficiary).

% Most dependent on receiving real aircraft time and mentored line exposure early in career; if operators quietly substitute cheaper simulator hours for costlier real-world anchoring, junior crew absorb the resulting competence gap first and have the least leverage to object, since career progression itself runs through the same institution setting the ratio.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, junior_flight_crew, payer,
    powerless, biographical, trapped, national).

% Relies entirely on the hybrid regime being honored in substance rather than degraded into simulation-only theater; has no visibility into whether real-world anchoring requirements are being met and no direct means of verifying crew competence before boarding.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, passengers_and_public, beneficiary,
    powerless, immediate, trapped, global).

% Sells increasingly high-fidelity simulator platforms and has a commercial interest in the industry accepting simulation as sufficient on its own, which would expand their market; the hybrid reading structurally caps their addressable share of the competence-maintenance budget by reserving a mandatory portion for real aircraft time.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, simulator_technology_vendors, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__hybrid_dependency, airline_operators).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__hybrid_dependency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that competence for rare, high-consequence failure modes cannot be verified by either channel alone: simulation permits repeated, safe exposure to failure scenarios that cannot ethically be staged in real aircraft, while real-world anchoring verifies that skills transfer under the sensory, organizational, and consequence-bearing texture that no simulator fully replicates.
% TRANSFER_FUNCTION: Moves training and evaluation cost from a cheaper, scalable simulator-only regime onto operators (who must schedule real aircraft time and audits) and onto crew (who bear the personal cost of recurrent real-world evaluation), in exchange for a competence guarantee that protects passengers and the operator's own institutional survival.
% ABSENT_VOICES: Accident investigators and human-factors researchers who study cases where simulation-only training produced fragile, non-transferring competence are largely absent from day-to-day scheduling decisions about the simulation/real-world ratio; their evidence enters the system only after failures, not before.
% DISAPPEARANCE_RATIONALE: If the requirement for periodic real-world anchoring vanished and training reverted to simulation alone, operators would immediately save substantial recurrent-training cost, junior crew progression would restructure around simulator hours only, and the industry's competence-verification claims would rest on an unaudited assumption that simulator fidelity fully substitutes for real-world transfer — a claim the historical record does not uniformly support.
% FOUNDING_PROBLEM: Early reliance on either pure on-the-job real-world training (too slow, too dangerous, too rare to expose crews to genuine emergency scenarios) or, later, pure simulator training (cheap and scalable but repeatedly shown to produce brittle competence that failed to transfer to actual line conditions and unscripted real-world variability) each produced identifiable failure patterns.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards and human-factors researchers outside the airline industry (NTSB, similar bodies internationally) have documented cases where simulator-trained crews failed to transfer competence to real-world conditions, corroborating that the underlying gap the hybrid regime addresses remains active; this is not merely asserted by the operators or regulators who administer the regime.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__hybrid_dependency, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__hybrid_dependency_tests).
:- end_tests(competence_exercise_requirement__hybrid_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the cost-asymmetry: operators and regulators set and administer the required ratio of simulation to real-world exercise, and bear an incentive to drift the ratio toward the cheaper channel over time, while junior crew and eventually passengers bear the downside risk of undetected competence gaps. Suppression (0.42) is moderate — pilots do not choose their training regime and have limited means to contest a quiet erosion of the real-world component, but this is not comparable to a genuinely coercive extraction structure; the regime retains real coordination value. Theater ratio (0.28) captures that non-jeopardy audits can slide toward box-checking if scheduling pressure mounts, which the T17 mechanism would flag if the accumulation continued past this interval. Accessibility collapse (0.5) is moderate: alternatives (pure simulation, more real-world time) remain conceptually available and contested within the industry, unlike a true mountain. Resistance (0.55) reflects active pushback from safety researchers and pilot unions against ratio erosion.
 *
 * DIRECTIONALITY LOGIC:
 *   Airline operators and regulatory bodies set the regime and administer it (low d, beneficiary-leaning) even though they also bear real financial and reputational cost of compliance — this is the coordination half of the tangled-rope structure. Line pilots and especially junior flight crew are structurally positioned as payers: they bear the personal cost of recurrent evaluation and are the first to absorb any competence gap if the real-world anchoring component is quietly eroded, with junior crew nearer the full-target end given their trapped exit options and career dependency on the same institution setting the ratio. Passengers are beneficiaries in principle but have no visibility or agency over whether the regime is honored, making them a diffuse, high-stakes but powerless beneficiary class.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resists mandatrophy in both directions: it does not let simulation-only advocates declare the competence problem solved by fidelity improvements alone (which would be a false summit — declaring a mountain of settled adequacy where beneficiaries, namely simulator vendors and cost-cutting operators, actually exist), and it does not require catastrophic real-world failure as the only legitimate exercise (which would be ethically and practically indefensible as a maintained policy). The tangled-rope classification captures that the coordination function (verified competence transfer) is real and worth defending, while the enforcement and cost-asymmetry around who bears the real-world anchoring burden is where active extraction risk lives — this is precisely the kind of hybrid that a cruder binary (rope vs. snare) would misclassify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ratio_erosion_detectability,
    'Can the actual ratio of simulation hours to real-world anchoring hours, as delivered inside a given operator''s training program, be independently audited, or does the operator''s self-reporting create an undetectable drift toward cheaper simulation over time?',
    'Independent regulatory audit of training logs cross-referenced against flight-hour and simulator-hour records, compared longitudinally across operators and over time to detect systematic ratio drift.',
    'If drift is undetectable by design, the hybrid regime''s coordination function is theater-adjacent and the constraint drifts toward the simulation_as_adequate_exercise reading in practice regardless of stated policy; if detectable and enforced, the tangled-rope classification with moderate extraction holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratio_erosion_detectability, empirical, 'Whether real-world-anchoring ratio compliance is independently verifiable or operator-self-reported.').

omega_variable(
    hybrid_vs_sibling_readings_boundary,
    'Is the boundary between this hybrid_dependency reading and the sibling readings (simulation_as_adequate_exercise, catastrophe_as_necessary_anchor) a matter of empirically resolvable competence-transfer research, or an irreducible policy judgment about acceptable risk?',
    'Longitudinal human-factors research comparing incident/near-miss rates across crews trained under differing simulation-to-real-world ratios, controlled for aircraft type and route complexity, could partially resolve which reading better predicts real-world competence outcomes.',
    'If empirically resolvable, one reading would eventually displace the others as the empirically corroborated kernel reading; if irreducibly a policy/values judgment about acceptable residual risk, the three readings remain permanently coexisting positions rather than converging.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_vs_sibling_readings_boundary, conceptual, 'Whether the kernel contest among the three readings is empirically resolvable or a persistent values disagreement.').

omega_variable(
    junior_crew_bargaining_power,
    'Do junior flight crew, as the class most exposed to real-world-anchoring erosion, have any realistic collective mechanism (union, professional association) to detect and resist a quiet shift toward cheaper simulation-only training?',
    'Review of union contract language and grievance/arbitration records addressing training-hour composition, and interviews with junior crew about visibility into training ratio decisions.',
    'If junior crew have no realistic collective leverage, the extraction directed at this group is closer to the full-target end than the moderate structural derivation currently reflects, and a directionality override may be warranted in a future revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(junior_crew_bargaining_power, empirical, 'Whether junior crew have collective mechanisms to detect and resist real-world-anchoring erosion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comp_tr_t6, competence_exercise_requirement__hybrid_dependency, theater_ratio, 6, 0.19).
narrative_ontology:measurement(comp_tr_t12, competence_exercise_requirement__hybrid_dependency, theater_ratio, 12, 0.22).
narrative_ontology:measurement(comp_tr_t18, competence_exercise_requirement__hybrid_dependency, theater_ratio, 18, 0.25).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__hybrid_dependency, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comp_be_t6, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 6, 0.31).
narrative_ontology:measurement(comp_be_t12, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 12, 0.34).
narrative_ontology:measurement(comp_be_t18, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 18, 0.36).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 24, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comp_su_t6, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 6, 0.34).
narrative_ontology:measurement(comp_su_t12, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(comp_su_t18, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 18, 0.4).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__hybrid_dependency, 0.12).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, catastrophe_as_necessary_anchor).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the competence_exercise_requirement kernel. simulation_as_adequate_exercise asserts high-fidelity simulation alone is sufficient exercise; catastrophe_as_necessary_anchor asserts only genuine catastrophic events provide irreducible exercise; this hybrid_dependency reading asserts both are necessary-but-insufficient in isolation and requires a combined regime. Each story carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle — they are not the same constraint measured three ways, they are three structurally distinct claims about what maintains competence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
