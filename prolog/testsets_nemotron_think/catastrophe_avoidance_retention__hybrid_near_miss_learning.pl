% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__hybrid_near_miss_learning, []).

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
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Hybrid Near-Miss Learning for Catastrophe Avoidance
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   Aviation maintains its ultra-high safety record through a distributed
 *   learning system that pools near-miss reports (ASAP, ASRS), foreign
 *   incident data, and high-fidelity simulator training across competing
 *   airlines, manufacturers, and regulators worldwide. No single organization
 *   experiences enough catastrophes to learn from them directly; the system
 *   solves this by treating every near-miss anywhere as a lesson for
 *   everyone. This constraint story captures the hybrid_near_miss_learning
 *   reading of the catastrophe_avoidance_retention kernel — the claim that
 *   competence retention requires this specific multi-source learning
 *   ecology, not simulation alone and not catastrophe alone.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.18).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.12).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Hybrid Near-Miss Learning for Catastrophe Avoidance").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning/high_reliability_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, '271677a5-f30b-4955-9862-b5b6576f35e2').
narrative_ontology:cs_kernel_codification('271677a5-f30b-4955-9862-b5b6576f35e2', distributed).
narrative_ontology:cs_authority_grounding('271677a5-f30b-4955-9862-b5b6576f35e2', practice).
narrative_ontology:cs_interpretation_layer_present('271677a5-f30b-4955-9862-b5b6576f35e2').
narrative_ontology:cs_reading_relation('271677a5-f30b-4955-9862-b5b6576f35e2', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, forecloses).
narrative_ontology:cs_reading_relation('271677a5-f30b-4955-9862-b5b6576f35e2', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, forecloses).
narrative_ontology:cs_axiom('271677a5-f30b-4955-9862-b5b6576f35e2', foundational, distributed_incident_learning_necessary).
narrative_ontology:cs_axiom_status(distributed_incident_learning_necessary, holdable).
narrative_ontology:cs_axiom_grounding('271677a5-f30b-4955-9862-b5b6576f35e2', distributed_incident_learning_necessary, empirically_contingent).
narrative_ontology:cs_axiom('271677a5-f30b-4955-9862-b5b6576f35e2', foundational, simulation_insufficient_for_competence).
narrative_ontology:cs_axiom_status(simulation_insufficient_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('271677a5-f30b-4955-9862-b5b6576f35e2', simulation_insufficient_for_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('271677a5-f30b-4955-9862-b5b6576f35e2', pre_distributed_learning_era).
narrative_ontology:cs_drift_state('271677a5-f30b-4955-9862-b5b6576f35e2', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('271677a5-f30b-4955-9862-b5b6576f35e2', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, airline_operators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, aircraft_manufacturers).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, aviation_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, pilots_controllers).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, traveling_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, pilots_controllers).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, high_reliability_organization_theory).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, just_culture_principles).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, distributed_learning_effectiveness).
narrative_ontology:constraint_vindicates(catastrophe_avoidance_retention__hybrid_near_miss_learning, near_miss_reporting_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate voluntary safety reporting systems (ASAP, FOQA) and share de-identified incident data across the industry. Invest in training infrastructure and safety management systems. Benefit from pooled learning but bear the cost of reporting infrastructure and operational changes driven by shared lessons.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, airline_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Set regulatory framework enabling voluntary reporting (e.g., FAA ASAP, EASA occurrences) and mandate safety management systems. Use aggregated incident data for rulemaking and oversight. Do not directly collect rents but gain legitimacy and effectiveness from the learning network.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, aviation_regulators, agenda_setter,
    institutional, generational, analytical, global).

% Receive aggregated safety data from operators and regulators worldwide. Use it to improve designs, issue service bulletins, and validate safety assumptions. Benefit enormously from learning they could not generate alone, while contributing engineering expertise back to the network.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, aircraft_manufacturers, beneficiary,
    powerful, generational, mobile, global).

% File voluntary safety reports (ASAP, ASRS) accepting personal vulnerability for systemic gain. Receive safer procedures, better training, and improved equipment derived from collective learning. Bear the time and career risk of reporting, but gain direct survival benefit from the system.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, pilots_controllers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__hybrid_near_miss_learning, pilots_controllers, payer).

% Ultimate beneficiaries of the ultra-high safety record produced by distributed learning. Have no meaningful exit from air travel dependence and no voice in the learning system's governance. Bear ticket prices that embed safety infrastructure costs.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, traveling_public, beneficiary,
    powerless, biographical, trapped, global).

% Lack equivalent cross-organizational incident-sharing infrastructure. Attempts to replicate aviation's model (patient safety organizations, reporting systems) remain fragmented, culturally resisted, and legally unprotected. Would object to being excluded from comparable safety gains but face structural barriers to entry.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, medical_healthcare_systems, excluded,
    institutional, generational, constrained, global).

% Study the aviation learning system as a model for high-reliability organizing. Produce the theoretical frameworks (HRO theory, safety culture, just culture) that legitimize and guide the practice. Neither collect rents nor bear operational costs.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of learning from rare catastrophic events by pooling near-misses, foreign incidents, and high-realism drill data across competitive organizations, creating a shared epistemic resource no single organization could generate alone.
% TRANSFER_FUNCTION: Moves operational safety knowledge and procedural improvements from reporting organizations to the entire network; reporters bear the cost of disclosure and operational change, while all participants (including non-reporters) receive the safety benefits.
% ABSENT_VOICES: Medical/healthcare organizations that lack equivalent incident-sharing infrastructure; patients in high-risk clinical domains who would benefit from distributed learning but have no representation in aviation's governance; whistleblowers in industries where reporting carries career-ending retaliation.
% DISAPPEARANCE_RATIONALE: Without the distributed near-miss learning network, each aviation organization would rely solely on its own rare catastrophic experiences for learning. The industry's ultra-low accident rate (hull loss ~0.2 per million departures) would be unsustainable; safety would revert to pre-1970s trajectories where each carrier learned only from its own hull losses.
% FOUNDING_PROBLEM: Early commercial aviation faced high accident rates; individual carriers could not accumulate enough catastrophic experience to learn effectively before going out of business or losing public trust. The system needed a way to learn from events that had not yet happened to any single organization — near-misses, foreign carrier accidents, and simulated edge cases.
% FOUNDING_PROBLEM_CORROBORATION: NTSB and ICAO historical analyses document the pre-1970s safety crisis and the deliberate construction of voluntary reporting systems. Academic HRO literature (Weick & Sutcliffe 2001, Roberts 1990, La Porte & Consolini 1991) corroborates the founding problem from outside the aviation industry, establishing it as a general principle of high-reliability organizing.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__hybrid_near_miss_learning, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__hybrid_near_miss_learning, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__hybrid_near_miss_learning, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).
:- end_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the arrangement is fundamentally mutualistic: every participant gains more safety knowledge than the marginal cost of their reporting. Suppression is minimal (0.12) — reporting is voluntary and legally protected (ASAP immunity), though organizational culture and fear of regulatory scrutiny create residual friction. Theater ratio is very low (0.08) — the learning function is genuine and measurable in accident rate trajectories. Accessibility collapse is moderate (0.35) — alternatives exist (internal learning only, regulator-only mandates) but are demonstrably less effective. Resistance is low (0.15) — the industry broadly embraces the system because its survival depends on it. The claimed type 'rope' reflects genuine coordination with minimal coercion.
 *
 * PERSPECTIVAL GAP:
 *   From the airline_operator and regulator seats (agenda_setters), the system appears as a successful coordination achievement they built and maintain. From the pilot/controller seat (beneficiary/payer), it appears as a vulnerable trust-based arrangement where personal reporting risk is traded for collective safety. From the traveling_public seat (beneficiary/trapped), it is an invisible infrastructure they depend on entirely but cannot influence. From the medical_healthcare_systems seat (excluded), it is an unattainable ideal blocked by legal, cultural, and economic barriers. The engine computes these divergent seat classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Agenda_setters (operators, regulators) sit near the beneficiary end (d ~0.2) — they control the rules and capture the legitimacy/safety gains. Beneficiaries (manufacturers, pilots/controllers, public) sit at d ~0.1-0.3 — they gain disproportionately to their costs. Pilots_controllers have a secondary payer role (reporting burden, career risk) pulling their d toward 0.4. Medical systems are excluded — they bear the cost of the constraint's absence (higher preventable harm) but have no seat in this constraint's governance. The derivation chain assigns low directionality to all beneficiaries, high directionality to any trapped parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (learning from rare events) remains live — new aircraft types, automation modes, and operational environments continuously generate novel failure paths. The arrangement has not atrophied; it has expanded (adding FOQA, ASIAS, global data exchange). Mandatrophy is not resolved because the coordination function is still necessary and the arrangement still solves it. The constraint would be a piton only if the learning function became performative while the reporting machinery persisted — current metrics show no such drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medicine_structural_barrier_vs_cultural_resistance,
    'Is medicine''s failure to replicate aviation''s distributed learning system due to structural barriers (malpractice liability, fragmented regulation, competitive markets) or cultural resistance (professional autonomy, hierarchy, shame-based error culture)?',
    'Comparative analysis of jurisdictions that have implemented aviation-style reporting (e.g., UK NHS incident reporting, US Patient Safety Organizations) — where structural barriers were lowered, did cultural adoption follow?',
    'If structural, the constraint''s absence in medicine is a policy failure addressable by legal reform. If cultural, the constraint may be non-transferable without generations of professional socialization change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medicine_structural_barrier_vs_cultural_resistance, empirical, 'Whether the aviation learning model''s absence in healthcare is structurally or culturally determined.').

omega_variable(
    simulation_fidelity_threshold,
    'At what fidelity threshold does simulation become a genuine substitute for near-miss learning, rather than a complement?',
    'Longitudinal comparison of safety outcomes in domains adopting high-fidelity simulation without distributed incident sharing (e.g., some surgical simulation programs) versus domains with both.',
    'If simulation reaches functional equivalence, the hybrid reading''s core claim (simulation alone insufficient) is falsified and the simulation_as_proxy_catastrophe reading gains empirical ground.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether advancing simulation technology will eventually obsolete the near-miss component of hybrid learning.').

omega_variable(
    free_rider_sustainability,
    'Can the voluntary reporting system sustain itself if free-riding organizations benefit from pooled data without contributing their own incidents?',
    'Game-theoretic modeling of reporting incentives combined with empirical analysis of reporting rates by carrier size and safety performance.',
    'If free-riding undermines contribution rates, the rope may degrade into a tangled_rope where mandatory reporting (enforcement) becomes necessary to sustain the commons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_rider_sustainability, conceptual, 'Whether the coordination mechanism is vulnerable to free-rider collapse without enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 10, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 20, 0.08).
narrative_ontology:measurement(cata_tr_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 30, 0.08).
narrative_ontology:measurement(cata_tr_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 40, 0.08).
narrative_ontology:measurement(cata_tr_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 50, 0.08).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cata_be_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(cata_be_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(cata_be_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(cata_be_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(cata_be_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 50, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cata_su_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(cata_su_t20, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 20, 0.12).
narrative_ontology:measurement(cata_su_t30, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 30, 0.12).
narrative_ontology:measurement(cata_su_t40, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(cata_su_t50, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, information_standard).
narrative_ontology:boltzmann_floor_override(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.02).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention__catastrophe_as_necessary_selector).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the catastrophe_avoidance_retention kernel into three readings with distinct ε values and structural relationships. hybrid_near_miss_learning (this story, ε≈0.18, rope) treats distributed learning as a coordination commons. simulation_as_proxy_catastrophe (ε≈0.35, tangled_rope) treats simulation infrastructure as a coordination function with extraction via vendor lock-in. catastrophe_as_necessary_selector (ε≈0.65, snare) treats catastrophic selection as an extractive filter that eliminates incompetent organizations at the cost of lives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
