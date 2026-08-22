% ============================================================================
% CONSTRAINT STORY: competence_retention_exercise__simulation_as_sufficient
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: High-Fidelity Simulation as Sufficient Competence Exercise
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear, aviation, chemical, healthcare)
 *   increasingly mandate high-fidelity simulation as the primary mechanism
 *   for maintaining catastrophe-avoidance competence. The reading
 *   instantiated here claims that simulator exercises are structurally
 *   equivalent to real events in their cognitive and procedural demands — the
 *   visceral stakes of actual catastrophe are not necessary for competence
 *   retention. This claim underwrites massive investment in simulation
 *   infrastructure, regulatory frameworks that accept simulator hours as
 *   competence evidence, and organizational policies that treat simulation
 *   completion as sufficient for credentialing. The constraint is authored as
 *   a scaffold with a sunset clause: the transition is toward a regime where
 *   catastrophes are prevented entirely and competence is maintained purely
 *   through simulation. But the theater ratio is rising and extraction is
 *   accumulating, suggesting the transition may be deferred indefinitely
 *   while the simulation apparatus becomes self-justifying.
 *
 * KEY AGENTS:
 *   - simulation_vendors: Primary beneficiary (institutional/arbitrage) — sells simulation platforms, scenarios, and certification services; profits from expanding mandate
 *   - training_departments: Beneficiary/agenda_setter (organized/constrained) — controls training budgets and curricula; institutional identity fused with simulation delivery
 *   - regulatory_bodies: Beneficiary/agenda_setter (institutional/constrained) — accepts simulation metrics as compliance evidence; avoids political cost of real catastrophes
 *   - frontline_operators: Victim (moderate/identity_locked) — bears time, cognitive load, and career risk; cannot exit simulation requirements without losing credential
 *   - operational_units: Victim (organized/constrained) — absorbs opportunity cost of simulation hours; readiness metrics tied to simulator performance not operational outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.38).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.42).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.38).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, scaffold).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "High-Fidelity Simulation as Sufficient Competence Exercise").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).
narrative_ontology:has_sunset_clause(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, '4ba1d2f3-75af-4717-bb66-858f3146d0e4').
narrative_ontology:cs_kernel_codification('4ba1d2f3-75af-4717-bb66-858f3146d0e4', distributed).
narrative_ontology:cs_authority_grounding('4ba1d2f3-75af-4717-bb66-858f3146d0e4', practice).
narrative_ontology:cs_interpretation_layer_present('4ba1d2f3-75af-4717-bb66-858f3146d0e4').
narrative_ontology:cs_reading_relation('4ba1d2f3-75af-4717-bb66-858f3146d0e4', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_reading_relation('4ba1d2f3-75af-4717-bb66-858f3146d0e4', competence_retention_exercise__near_miss_as_bridge, influences).
narrative_ontology:cs_axiom('4ba1d2f3-75af-4717-bb66-858f3146d0e4', foundational, simulation_fidelity_equivalence).
narrative_ontology:cs_axiom_status(simulation_fidelity_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('4ba1d2f3-75af-4717-bb66-858f3146d0e4', simulation_fidelity_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('4ba1d2f3-75af-4717-bb66-858f3146d0e4', foundational, visceral_stakes_not_necessary).
narrative_ontology:cs_axiom_status(visceral_stakes_not_necessary, holdable).
narrative_ontology:cs_axiom_grounding('4ba1d2f3-75af-4717-bb66-858f3146d0e4', visceral_stakes_not_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('4ba1d2f3-75af-4717-bb66-858f3146d0e4', post_tmi_bhopal_challenger_simulation_mandate).
narrative_ontology:cs_drift_state('4ba1d2f3-75af-4717-bb66-858f3146d0e4', contemporary_fidelity_escalation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4ba1d2f3-75af-4717-bb66-858f3146d0e4', '2026-08-15T14:32:00Z').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulation_vendors).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, training_departments).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, regulatory_bodies).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, operational_units).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, simulation_fidelity_equivalence_claim).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, competence_metric_validity_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell simulation platforms, scenario libraries, instructor certification, and data analytics to high-reliability organizations. Revenue scales with regulatory mandates for simulation hours and fidelity tiers. Can pivot across sectors (aviation, nuclear, healthcare, maritime) if one market contracts. Professional identity is not fused to any single client's simulation program.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulation_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Control training budgets, curriculum design, and instructor corps within their organizations. Institutional prestige and budget authority grow with simulation program expansion. Career progression for training leaders depends on simulation metrics (hours delivered, fidelity upgrades, pass rates). Cannot easily exit the simulation paradigm without dismantling the department's core function and identity.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, training_departments, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, training_departments, agenda_setter).

% Set simulation requirements for licensing and continuing certification. Accept simulator performance as evidence of competence, avoiding the political impossibility of mandating real-event exposure. Gain legitimacy from 'evidence-based' regulation. Constrained exit: regulatory mandate is statutory; changing the framework requires legislative action and inter-agency coordination.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, regulatory_bodies, beneficiary).

% Complete mandated simulation hours (often 40-200 hours/year) on top of operational duties. Cognitive load is high but lacks the visceral stakes of real events — no risk of death, environmental release, or public harm. Career progression, license renewal, and peer standing depend on simulator performance metrics. Cannot exit: leaving the simulation track means losing credential, professional identity, and employment. The constraint extracts time, cognitive energy, and career autonomy while delivering unverified competence gains.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, frontline_operators, payer,
    moderate, biographical, identity_locked, regional).

% Absorb opportunity cost of simulation hours (crews offline, facilities occupied, maintenance deferred). Readiness reporting increasingly tied to simulator metrics (scenario pass rates, fidelity utilization) rather than operational outcomes (incident response times, near-miss trends). Constrained exit: regulatory requirements and organizational policy mandate simulation compliance; non-compliance triggers audit findings and operational restrictions.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, operational_units, payer,
    organized, biographical, constrained, regional).

% Study organizational learning across high-reliability sectors. No direct stake in simulation mandates. Provide the external validation (or falsification) of simulation fidelity equivalence claims. Their analyses are cited by all parties but they do not set policy or bear costs.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, independent_safety_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_retention_exercise__simulation_as_sufficient, simulation_vendors).
narrative_ontology:fixing_cost_class(competence_retention_exercise__simulation_as_sufficient, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains catastrophe-avoidance competence across high-reliability organizations without requiring actual catastrophic events — solves the coordination problem of how to keep operators sharp when the events they train for must never happen.
% TRANSFER_FUNCTION: Moves operator time, cognitive effort, organizational resources, and credentialing authority from frontline operations into simulation infrastructure (vendors, training departments, regulatory compliance systems). The transfer is justified as competence insurance; the return is simulator performance metrics.
% ABSENT_VOICES: Operators who have experienced real catastrophes and argue simulation cannot replicate the visceral stakes; organizations that maintain hybrid competence models (simulation + controlled exposure + near-miss integration) but are pressured to conform to simulation-only standards; communities downstream of high-hazard facilities who bear residual risk if simulation-trained operators fail in real events.
% DISAPPEARANCE_RATIONALE: If simulation mandates vanished overnight, organizations would revert to mixed competence models: apprenticeship, controlled exposure, near-miss analysis, and reduced simulation. Regulatory frameworks would face a legitimacy crisis (what replaces simulator hours as competence evidence?). Simulation vendors would lose primary revenue stream. The high-reliability organizational field would reorganize around the question of what actually maintains competence.
% FOUNDING_PROBLEM: After major catastrophes (Three Mile Island, Bhopal, Challenger), high-reliability organizations faced a coordination problem: how to maintain operator competence for events that must never occur again. Real-event training was ethically and politically impossible. Simulation emerged as the coordinated solution.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (competence without catastrophe) is attested as live by simulation vendors, training departments, and regulatory bodies. It is contested by frontline operators who have experienced real events (they attest the problem is misdiagnosed — competence requires visceral stakes), by near-miss advocates (they attest the problem is solvable with less simulation), and by independent safety analysts who document that simulation fidelity equivalence remains empirically unvalidated at the catastrophe-avoidance level.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__simulation_as_sufficient, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.38) is moderate but rising: simulation vendors and training bureaucracies capture value from expanding mandates while the marginal competence gain per simulation hour diminishes. Suppression (0.42) is moderate: the constraint is enforced through credentialing requirements and regulatory acceptance, not direct coercion — but exit is identity-locked for operators whose professional identity is constituted through the simulation pathway. Theater ratio (0.55) is high and rising: a majority of simulation activity now serves credentialing/compliance rather than genuine competence exploration (repeating standardized scenarios, gaming metrics). Accessibility collapse (0.68) is high: once an organization commits to the simulation-as-sufficient framework, alternative competence pathways (controlled real-event exposure, near-miss integration, apprenticeship) are structurally excluded. Resistance (0.31) is low-moderate: operators comply because career progression depends on it; overt resistance is career-limiting.
 *
 * PERSPECTIVAL GAP:
 *   From the simulation_vendor/training_department/regulatory_body seats (beneficiaries/agenda_setters with institutional power and arbitrage/constrained exit), the constraint appears as a genuine coordination scaffold — it solves the problem of maintaining competence without catastrophes. From the frontline_operator/operational_unit seats (victims with moderate/organized power but identity_locked/constrained exit), the same constraint operates as extraction: they invest increasing hours in simulation with diminishing operational relevance, cannot validate their competence against reality, and face career consequences for non-compliance. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (simulation_vendors, training_departments, regulatory_bodies) collect rents, control the agenda, and have exit options (vendors have arbitrage across markets; departments/bodies have constrained but real mobility). Victims (frontline_operators, operational_units) bear the time/cognitive/opportunity costs with identity_locked/constrained exit — operators cannot leave the simulation track without losing professional identity and credential; units cannot opt out without regulatory penalty. The directionality derivation from beneficiary/victim + exit correctly places vendors near d=0.15 (beneficiary), operators near d=0.85 (target).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining catastrophe-avoidance competence without experiencing catastrophes) is contested: catastrophe_as_necessary reading claims it is not solved; near_miss_as_bridge claims it requires real-world feedback. The mandate has not atrophied — the problem is live — but the solution (simulation-as-sufficient) may be extracting more than it coordinates. If simulation fidelity equivalence fails empirically, the constraint is a snare disguised as a scaffold: it extracts from operators by selling false confidence while suppressing the real-event exposure that would reveal the gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_real_equivalence,
    'Are the cognitive and procedural demands of high-fidelity simulation structurally equivalent to real catastrophic events, or does the absence of visceral stakes create a fundamental gap?',
    'Longitudinal studies comparing operator performance in simulation vs. real events; neurophysiological measures of stress response; post-incident analysis of whether simulator-trained operators exhibit degradation under actual catastrophe conditions.',
    'If equivalence fails, the constraint is a false scaffold — training infrastructure cannot substitute for real experience and the constraint extracts from operators by creating false confidence. If equivalence holds, the scaffold is genuinely transitional toward a no-catastrophe regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_real_equivalence, empirical, 'Core empirical claim distinguishing this reading from catastrophe_as_necessary').

omega_variable(
    committer_kernel_framing,
    'This constraint is one reading (simulation_as_sufficient) of the contested kernel competence_retention_exercise. Sibling readings are catastrophe_as_necessary and near_miss_as_bridge. The disagreement is located at: whether simulated experience can fully substitute for visceral stakes in maintaining catastrophe-avoidance competence.',
    'Comparative analysis of organizational learning outcomes across regimes that rely primarily on simulation vs. those that require real-event exposure or near-miss integration.',
    'Classification of this constraint depends on which reading is structurally dominant. If catastrophe_as_necessary is validated, this reading''s scaffold claim collapses into a snare (false confidence extracting from operators). If near_miss_as_bridge dominates, this reading is a partial scaffold requiring hybrid validation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_framing, conceptual, 'Commitment-system framing: kernel identity and reading relations').

omega_variable(
    sunset_clause_credibility,
    'Does the declared sunset clause (transition to no-catastrophe regime) represent a genuine transition plan, or is it a perpetual deferral that masks the scaffold''s conversion into a piton?',
    'Track whether simulator fidelity targets, validation criteria, and transition milestones are met on schedule; assess whether regulatory frameworks actually sunset simulation requirements or expand them.',
    'If the sunset is perpetually deferred while simulation infrastructure expands, the constraint migrates from scaffold to piton (theatrical maintenance of a degraded coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_credibility, empirical, 'Whether the scaffold''s transitional justification is genuine or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_tr_t0, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0, 0.25).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_tr_t5, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 5, 0.35).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_tr_t10, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 10, 0.42).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_tr_t15, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 15, 0.48).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_tr_t20, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 20, 0.52).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_tr_t25, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 25, 0.55).

% Extraction over time
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_be_t0, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_be_t5, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_be_t10, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_be_t15, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 15, 0.33).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_be_t20, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_be_t25, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 25, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_su_t0, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_su_t5, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_su_t10, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_su_t15, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_su_t20, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(competence_retention_exercise__simulation_as_sufficient_su_t25, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__simulation_as_sufficient, 0.08).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__near_miss_as_bridge).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the competence_retention_exercise kernel. The catastrophe_as_necessary reading (constraint_id: competence_retention_exercise__catastrophe_as_necessary) claims epsilon near 0.15 (genuine coordination) with different beneficiaries/victims. The near_miss_as_bridge reading (constraint_id: competence_retention_exercise__near_miss_as_bridge) claims epsilon near 0.25 (tangled_rope) with hybrid validation structure. All three share the kernel but instantiate different constraints with different ε, different stakeholder structures, and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__simulation_as_sufficient, institutional, 0.12).
constraint_indexing:directionality_override(competence_retention_exercise__simulation_as_sufficient, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
