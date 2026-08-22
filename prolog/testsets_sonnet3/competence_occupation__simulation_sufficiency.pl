% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__simulation_sufficiency, []).

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
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation-Sufficiency Reading of Competence Occupation
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   This story instantiates the simulation_sufficiency reading of the
 *   competence_occupation kernel: the claim that scheduled, sufficiently
 *   frequent and fidelitous simulation drills fully occupy the competence
 *   kernel and prevent skill decay, such that no further exercise mechanism
 *   is structurally necessary. Under this reading, training compliance (hours
 *   logged, scenarios passed) becomes the observable that stands in for
 *   actual retained competence. This is a genuine coordination mechanism —
 *   rare catastrophic procedures cannot be practiced live without
 *   unacceptable risk — but it has hardened into a tangled rope because the
 *   same structure that solves the scheduling problem also lets training
 *   departments, compliance officers, vendors, and leadership substitute an
 *   auditable, cheap proxy (simulation hours) for the much harder and more
 *   expensive question of real-condition competence, while the cost of that
 *   substitution's failure mode (a genuinely novel incident the scenario
 *   library never modeled) lands on frontline operators and the public rather
 *   than on anyone in the certifying chain.
 *
 * KEY AGENTS:
 *   - simulation_vendors: primary economic beneficiary of the sufficiency claim, organized/arbitrage
 *   - training_departments: agenda-setter, controls the observable substituted for real competence
 *   - frontline_operators: payer, trapped exit, bears consequence gap at moment of real failure
 *   - downstream_public_affected_by_incidents: powerless payer, no visibility into the substitution
 *   - regulatory_compliance_officers: beneficiary of an auditable proxy, reduces their own exposure
 *   - organizational_leadership: beneficiary of cost savings, insulated from tail-risk accountability
 *   - safety_researchers: analytical observer, contested findings on transfer failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.58).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.52).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.58).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation-Sufficiency Reading of Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "high_reliability_organizations/safety_training").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '97af86cc-365b-4284-bd87-225e3f1a9167').
narrative_ontology:cs_kernel_codification('97af86cc-365b-4284-bd87-225e3f1a9167', distributed).
narrative_ontology:cs_authority_grounding('97af86cc-365b-4284-bd87-225e3f1a9167', practice).
narrative_ontology:cs_interpretation_layer_present('97af86cc-365b-4284-bd87-225e3f1a9167').
narrative_ontology:cs_reading_relation('97af86cc-365b-4284-bd87-225e3f1a9167', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_reading_relation('97af86cc-365b-4284-bd87-225e3f1a9167', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('97af86cc-365b-4284-bd87-225e3f1a9167', foundational, simulation_fidelity_is_functionally_equivalent_to_real_exposure).
narrative_ontology:cs_axiom_status(simulation_fidelity_is_functionally_equivalent_to_real_exposure, holdable).
narrative_ontology:cs_axiom_grounding('97af86cc-365b-4284-bd87-225e3f1a9167', simulation_fidelity_is_functionally_equivalent_to_real_exposure, empirically_contingent).
narrative_ontology:cs_axiom('97af86cc-365b-4284-bd87-225e3f1a9167', secondary, frequency_and_fidelity_optimization_fully_solves_skill_decay).
narrative_ontology:cs_axiom_status(frequency_and_fidelity_optimization_fully_solves_skill_decay, holdable).
narrative_ontology:cs_axiom_grounding('97af86cc-365b-4284-bd87-225e3f1a9167', frequency_and_fidelity_optimization_fully_solves_skill_decay, instrumental).
narrative_ontology:cs_reference_frame('97af86cc-365b-4284-bd87-225e3f1a9167', post_challenger_simulation_adoption_standard).
narrative_ontology:cs_drift_state('97af86cc-365b-4284-bd87-225e3f1a9167', contemporary_high_reliability_org_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('97af86cc-365b-4284-bd87-225e3f1a9167', '').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, training_departments).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, regulatory_compliance_officers).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, organizational_leadership).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, downstream_public_affected_by_incidents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell simulator hardware, software licenses, and scenario libraries to operators. Revenue depends directly on the claim that simulation, if run at sufficient frequency and fidelity, fully occupies the competence kernel. Every certification cycle that accepts simulation hours as sufficient is a renewed contract.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_vendors, beneficiary,
    organized, generational, arbitrage, national).

% Design and administer the drill schedule, set pass/fail thresholds, and report compliance metrics upward. Their institutional survival depends on producing a defensible record of 'hours trained' rather than on the harder-to-measure question of retained skill under real conditions. They control which observable the organization treats as ground truth.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, training_departments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, training_departments, beneficiary).

% Cycle through mandated simulation drills to keep certifications current. They cannot opt out without losing employment eligibility, and they bear the direct consequence if the drills fail to transfer to a real degraded-condition event — the gap between simulated and real stress, fatigue, and consequence weight lands on them at the moment of failure, not on the people who designed the drill calendar.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, frontline_operators, payer,
    moderate, immediate, trapped, local).

% Passengers, patients, residents near the facility, or others whose safety depends on operator competence in a genuine crisis. They have no visibility into whether the certification regime is producing real skill or compliance theater, and no exit from exposure to the operator's competence level.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, downstream_public_affected_by_incidents, payer,
    powerless, immediate, trapped, regional).

% Audit training records against a simulation-hours standard because it is auditable, quantifiable, and defensible in litigation. Accepting simulation sufficiency as the compliance bar reduces their own exposure and workload relative to auditing harder-to-measure real-condition performance.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, regulatory_compliance_officers, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, regulatory_compliance_officers, agenda_setter).

% Approves simulation-based training budgets because they are cheaper, safer, and more schedulable than exposing operators to real degraded conditions or maintaining costly redundant real-world practice systems. Insulated from the consequence if the kernel is not actually occupied, since accountability in a rare catastrophic event is diffused across the institution.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, organizational_leadership, beneficiary,
    institutional, biographical, arbitrage, national).

% Study incident post-mortems and skill-decay literature. Some find simulation-only regimes correlate with degraded improvisation under genuinely novel failure modes not represented in scenario libraries; their findings compete for institutional attention against vendor-funded efficacy studies.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__simulation_sufficiency, simulation_vendors).
narrative_ontology:fixing_cost_class(competence_occupation__simulation_sufficiency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simulation drills genuinely solve a real coordination problem: they let large numbers of operators practice low-frequency, high-consequence procedures repeatedly, safely, and on a predictable schedule, without waiting for or manufacturing real catastrophic events.
% TRANSFER_FUNCTION: Moves training budget and organizational trust from more expensive, harder-to-schedule real-condition exposure mechanisms (live drills, controlled real degraded-mode operation) toward simulation vendors and internally-controlled compliance metrics; moves residual risk from institutions that certify competence onto frontline operators and the public who depend on that competence in an actual event.
% ABSENT_VOICES: Frontline operators who have experienced the gap between simulated and real stress conditions are rarely consulted on whether the drill design captures the actual failure texture; safety researchers whose findings complicate the sufficiency claim compete against vendor-funded studies for institutional airtime and are frequently excluded from certification-standard-setting bodies.
% DISAPPEARANCE_RATIONALE: If simulation-based certification were no longer accepted as sufficient, training departments would need to build costly real-condition or hybrid exposure mechanisms, certification timelines would lengthen, simulation vendor revenue would collapse, and regulatory audit burden would shift from paperwork review to harder real-performance assessment.
% FOUNDING_PROBLEM: Operators of high-consequence systems (nuclear plants, aviation, surgery, emergency response) needed a way to practice rare catastrophic-failure procedures without waiting for real catastrophes or risking real harm during practice.
% FOUNDING_PROBLEM_CORROBORATION: Training departments, vendors, and compliance officers attest the problem is solved: simulation captures the necessary procedural and decision-making elements. Independent safety researchers and several post-incident investigation boards (outside the training/vendor/compliance chain) attest the founding problem is only partially solved — investigations of real incidents have repeatedly found operators competent on simulated scenarios but unable to adapt to conditions the scenario library did not anticipate.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__simulation_sufficiency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__simulation_sufficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and theater_ratio (0.62) both rise across the interval because as certification regimes mature, more of the visible training activity comes to serve compliance-record production rather than skill transfer — this is the Goodhart drift the sufficiency reading is structurally prone to once simulation hours become the metric an entire vendor/compliance ecosystem is built around defending. Suppression (0.52) reflects that operators cannot opt out of the simulation regime without losing certification, but it is only moderate because the drills are not purely coercive theater — they retain real coordination value for procedural and decision-tree competence. accessibility_collapse (0.50) and resistance (0.45) sit at mid-range: alternative exercise mechanisms (real degraded-condition practice, hybrid regimes) are not fully foreclosed — they are the sibling readings — but the sufficiency framing actively displaces them in budget and regulatory attention, and some resistance (from safety researchers, from operators after near-miss experiences) persists but lacks institutional standing to change the standard.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation vendors, training departments, compliance officers, and leadership all sit toward the beneficiary end: they either collect revenue, reduce audit burden, reduce cost, or discharge their institutional mandate by pointing to compliance records. Frontline operators and the downstream public sit toward the target end: operators are trapped (cannot exit certification without losing livelihood) and bear the consequence gap directly in a real event; the public is trapped by definition (they cannot select for operators trained under a different regime) and has zero visibility into whether the substitution has occurred.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (practicing rare catastrophic procedures safely) is not dead — it remains genuinely live, which is why this cannot be classified as a pure snare or piton. The mandatrophy risk here is narrower: the MECHANISM (simulation-hours-as-sufficient-proxy) has been extended past its evidentiary support by the parties who benefit from the extension, while the underlying coordination function it was built to serve continues to operate. Classifying this as tangled_rope rather than snare preserves the real coordination content (rare-event practice under safe conditions) while flagging that the same structure carries asymmetric extraction (cost-avoidance for the certifying chain, risk-transfer to operators and public) that requires active enforcement (mandatory certification, no legal alternative pathway) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_validity,
    'Does simulation-based training, at any achievable frequency and fidelity, actually occupy the competence kernel for genuinely novel failure conditions not represented in the scenario library, or only for anticipated-and-modeled failure modes?',
    'Longitudinal comparison of post-incident investigation findings (real catastrophic events) against the specific scenarios operators were certified on, tracking whether failures cluster in gaps between modeled and encountered conditions.',
    'If transfer validity holds only for modeled conditions, the sufficiency claim is structurally false for the class of events that matter most (novel, unmodeled failures) — the constraint would be substantially extractive with a coordination cover story rather than genuine sufficiency; if transfer validity holds broadly, the claimed_type moves closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_validity, empirical, 'Whether simulation fidelity generalizes to unmodeled real conditions.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the simulation_sufficiency reading the operationally dominant one because it best fits the evidence, or because it is the reading that is cheapest to institutionalize and most defensible in litigation regardless of its evidentiary merit?',
    'Compare adoption patterns of simulation_sufficiency vs. hybrid_occupation across regulatory regimes with different litigation-exposure profiles and different cost constraints; if adoption tracks cost/liability rather than incident outcomes, selection pressure rather than evidentiary merit is driving the reading.',
    'If cost/litigation-driven, this reading''s institutional dominance reflects capture by the beneficiary coalition (vendors, compliance officers, leadership) rather than a genuine resolution of which mechanism occupies the kernel — reinforces tangled_rope classification and supports treating the hybrid reading as underweighted rather than rejected.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether the reading''s dominance reflects evidentiary merit or institutional cost-avoidance.').

omega_variable(
    theater_ratio_trajectory_endpoint,
    'Does the rising theater_ratio trajectory (0.32 to 0.62) represent an asymptote where compliance activity stabilizes at a genuine coordination-plus-overhead mixture, or an unbounded drift where the observable fully decouples from the underlying skill-decay problem?',
    'Extend the measurement interval and compare theater_ratio growth against independently-measured incident rates in facilities under this regime versus hybrid regimes.',
    'An asymptote supports tangled_rope with a stable extraction/coordination ratio; unbounded drift would support reclassification toward snare or piton as the coordination content becomes vestigial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_ratio_trajectory_endpoint, empirical, 'Whether theater ratio growth is bounded or unbounded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.32).
narrative_ontology:measurement(comp_tr_t4, competence_occupation__simulation_sufficiency, theater_ratio, 4, 0.4).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__simulation_sufficiency, theater_ratio, 8, 0.47).
narrative_ontology:measurement(comp_tr_t12, competence_occupation__simulation_sufficiency, theater_ratio, 12, 0.53).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__simulation_sufficiency, theater_ratio, 16, 0.57).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__simulation_sufficiency, theater_ratio, 20, 0.6).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__simulation_sufficiency, theater_ratio, 24, 0.62).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comp_be_t4, competence_occupation__simulation_sufficiency, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(comp_be_t8, competence_occupation__simulation_sufficiency, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(comp_be_t12, competence_occupation__simulation_sufficiency, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(comp_be_t16, competence_occupation__simulation_sufficiency, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(comp_be_t20, competence_occupation__simulation_sufficiency, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(comp_be_t24, competence_occupation__simulation_sufficiency, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comp_su_t4, competence_occupation__simulation_sufficiency, suppression_requirement, 4, 0.39).
narrative_ontology:measurement(comp_su_t8, competence_occupation__simulation_sufficiency, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(comp_su_t12, competence_occupation__simulation_sufficiency, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(comp_su_t16, competence_occupation__simulation_sufficiency, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(comp_su_t20, competence_occupation__simulation_sufficiency, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(comp_su_t24, competence_occupation__simulation_sufficiency, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__simulation_sufficiency, 0.12).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the competence_occupation kernel. simulation_sufficiency (this story) treats simulation drills as structurally sufficient and shows the highest theater_ratio drift because it is the reading most vulnerable to metric substitution (training hours standing in for retained skill). real_incident_necessity treats only genuine catastrophic incidents as adequate, which would carry a very different — likely much lower extraction but much higher direct-harm-risk — profile since it rejects safe practice substitutes almost entirely. hybrid_occupation treats the question as genuinely unresolved and requires multi-mechanism exercise, which likely shows a more moderate extraction profile with higher genuine coordination content but also higher direct cost. All three share the same underlying founding problem (safe practice for rare catastrophic procedures) but diverge sharply on which mechanism the kernel treats as sufficient, and therefore on who benefits from that determination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
