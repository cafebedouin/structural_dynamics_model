% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, []).

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
 *   constraint_id: catastrophe_avoidance_retention__simulation_as_proxy_catastrophe
 *   human_readable: Simulation-as-Proxy-Catastrophe Safety Regime
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the simulation_as_proxy_catastrophe reading
 *   of the catastrophe_avoidance_retention kernel. It asserts that
 *   high-fidelity simulation and scheduled drills are functionally equivalent
 *   to real catastrophic events for the maintenance of operational competence
 *   in high-reliability organizations. The claim underwrites a sprawling
 *   safety-engineering paradigm in which simulation infrastructure is treated
 *   as critical, competence decay is managed through inspectable drill
 *   schedules, and regulatory enforcement is deemed sufficient. The
 *   arrangement delivers genuine coordination valueârepeatable training
 *   without mass casualtiesâwhile concentrating extraction on vendors,
 *   regulators, and training centers that capture the budgets and authority
 *   flowing from the equivalence claim. Frontline operators, operating
 *   organizations, and the general public bear the costs, respectively, as
 *   identity-locked drill subjects, compliance-paying liability-shield
 *   seekers, and residual risk bearers. Sibling readings include
 *   catastrophe_as_necessary_selector and hybrid_near_miss_learning.
 *
 * KEY AGENTS:
 *   - simulation_technology_vendors: Primary beneficiary (powerful/mobile) â captures regulatory-mandated revenue
 *   - safety_regulators: Agenda-setter (institutional/constrained) â expands authority through enforcement
 *   - accredited_training_centers: Secondary beneficiary (organized/constrained) â sells mandated certification
 *   - frontline_operators: Primary target (moderate/identity_locked) â bears drill burden and false-confidence risk
 *   - operating_organizations: Target with secondary benefit (powerful/constrained) â pays for compliance and liability shields
 *   - general_public: Diffuse target (powerless/trapped) â bears residual catastrophic risk
 *   - catastrophe_experience_advocates: Excluded voice (moderate/constrained) â structurally absent from standards bodies
 *   - independent_safety_researchers: Analytical observer (analytical/analytical) â studies transfer validity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.68).
domain_priors:suppression_score(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.7).
domain_priors:theater_ratio(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "Simulation-as-Proxy-Catastrophe Safety Regime").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, '9e16f08d-9849-466a-b31b-7095c2c910d8').
narrative_ontology:cs_kernel_codification('9e16f08d-9849-466a-b31b-7095c2c910d8', formalized).
narrative_ontology:cs_authority_grounding('9e16f08d-9849-466a-b31b-7095c2c910d8', extraction).
narrative_ontology:cs_interpretation_layer_present('9e16f08d-9849-466a-b31b-7095c2c910d8').
narrative_ontology:cs_reading_relation('9e16f08d-9849-466a-b31b-7095c2c910d8', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, forecloses).
narrative_ontology:cs_reading_relation('9e16f08d-9849-466a-b31b-7095c2c910d8', catastrophe_avoidance_retention__hybrid_near_miss_learning, forecloses).
narrative_ontology:cs_axiom('9e16f08d-9849-466a-b31b-7095c2c910d8', foundational, simulation_equivalence_sufficient).
narrative_ontology:cs_axiom_status(simulation_equivalence_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('9e16f08d-9849-466a-b31b-7095c2c910d8', simulation_equivalence_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('9e16f08d-9849-466a-b31b-7095c2c910d8', secondary, regulatory_enforcement_maintains_competence).
narrative_ontology:cs_axiom_status(regulatory_enforcement_maintains_competence, holdable).
narrative_ontology:cs_axiom_grounding('9e16f08d-9849-466a-b31b-7095c2c910d8', regulatory_enforcement_maintains_competence, instrumental).
narrative_ontology:cs_reference_frame('9e16f08d-9849-466a-b31b-7095c2c910d8', simulation_competence_equivalence).
narrative_ontology:cs_drift_state('9e16f08d-9849-466a-b31b-7095c2c910d8', contemporary_safety_regime, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9e16f08d-9849-466a-b31b-7095c2c910d8', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_regulators).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accredited_training_centers).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, general_public).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operating_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operating_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, sell, and maintain high-fidelity simulation systems for safety-critical industries. Revenue streams depend on regulatory mandates that treat simulation hours as equivalent to real catastrophic experience. They lobby for standards that require recurrent drill investments and certify only vendor-approved equipment.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, simulation_technology_vendors, beneficiary,
    powerful, generational, mobile, global).

% Mandate simulation-hour minima for operator licensing and organizational accreditation. Justify institutional budgets and expansion by enforcing the equivalence claim. Their authority is bound to the inspectability of scheduled drills; alternative competence models threaten their standardization framework.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Deliver regulator-certified simulation courses that operators must complete to retain professional standing. Revenue depends on the institutionalized equivalence between drill completion and competence. Cannot pivot to non-simulation curricula without losing accreditation.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, accredited_training_centers, beneficiary,
    organized, biographical, constrained, regional).

% Must complete recurring high-fidelity drills to maintain professional licenses and employment authorization. Bear the time burden, cognitive load, and performance anxiety of artificial scenarios. Their professional identity and legal right to operate are fused to simulation compliance, making exit equivalent to career termination.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, frontline_operators, payer,
    moderate, biographical, identity_locked, national).

% Purchase expensive simulation systems and release personnel for mandatory drill hours to satisfy regulators. Receive liability protection, insurance discounts, and public credibility for compliance. Alternative training investments are not recognized by licensing bodies, constraining their choice set to vendor ecosystems.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operating_organizations, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, operating_organizations, beneficiary).

% Depends on the safety claim that simulated competence equals real readiness. Bears the residual catastrophic risk when simulation fails to capture emergent organizational failures, surprise, or mortality salience that only actual high-stakes events reveal. Has no market or political exit from the regime.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, general_public, payer,
    powerless, biographical, trapped, national).

% Veteran practitioners and researchers who argue that genuine competence requires the irreducible phenomenology of real catastrophesâunscripted chaos, institutional stress, and mortality salience. Their perspective is structurally absent from standards committees because it directly undermines the equivalence claim that funds the regulatory-simulation complex.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_experience_advocates, excluded,
    moderate, generational, constrained, global).

% Study the empirical transfer of learning from simulated to real catastrophic environments. Produce evidence that drill performance often poorly predicts real-event behavior. Their findings are typically absorbed into incremental fidelity improvements rather than treated as falsifications of the equivalence axiom.
narrative_ontology:constraint_stakeholder(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, independent_safety_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains catastrophic-response competence across high-reliability organizations without requiring actual mass-casualty events, by providing repeatable, schedulable, and inspectable training environments.
% TRANSFER_FUNCTION: Moves capital from operating organizations to simulation technology vendors and accredited training centers; moves time and cognitive load from frontline operators to scheduled drill regimes; moves regulatory authority to safety agencies and catastrophic risk to the general public by substituting inspected drill compliance for demonstrated real-event competence.
% ABSENT_VOICES: Catastrophe-experience advocates and frontline operators who report that drill performance fails to predict real-event behavior are structurally excluded from standard-setting bodies; their objections are treated as implementation failures rather than falsifications of the equivalence claim.
% DISAPPEARANCE_RATIONALE: Operating organizations would lose liability shields and insurance premium reductions without the compliance defense; the simulation industry would lose regulatory-mandated revenue streams; safety regulators would lose their primary inspectable metric; the field would revert to contested hybrid or catastrophe-dependent competence models, rearranging budgets, authority, and risk allocation.
% FOUNDING_PROBLEM: Actual catastrophes are too rare, destructive, and ethically fraught to serve as routine training grounds; competence atrophies during long quiescent periods in high-reliability systems.
% FOUNDING_PROBLEM_CORROBORATION: Safety regulators and simulation vendors attest the problem remains live and is addressed by their model. Independent safety researchers and frontline operator unions corroborate that competence decay between events is a genuine risk, though they dispute that simulation equivalence is the correct solution; no party outside the benefiting set asserts the problem no longer exists.
narrative_ontology:disappearance_verdict(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the capture of safety budgets by simulation vendors and the expansion of regulatory authority justified by the equivalence claim. Suppression (0.70) is high because the constraint actively excludes alternative competence modelsâsuch as catastrophe-as-selector or hybrid learningâfrom regulatory legitimacy and licensing pathways. Theater ratio (0.58) captures the ritualization of drills: as the regime matures, an increasing share of simulation activity serves inspectability and compliance documentation rather than genuine skill transfer. Accessibility collapse (0.62) registers the declining legitimacy of non-simulation competence pathways once the equivalence claim is institutionalized. Resistance (0.45) is moderate, coming from excluded catastrophe-experience advocates, some frontline operators, and independent researchers whose findings are absorbed rather than acted upon.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (regulators, vendors, training centers) experience the constraint as a necessary and effective coordination mechanism that prevents catastrophes through scalable training. The payer seats (operators, organizations, public) experience it as a costly enforced regime that may produce compliance theater and false confidence. The engine will compute divergent per-seat classifications from this structural data: the beneficiary side will see a rope or scaffold, while the payer side will see a snare or tangled rope, with the story-level classification settling on tangled_rope to preserve the hybrid reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to simulation_technology_vendors, safety_regulators, and accredited_training_centers, all of whom collect rents, authority, or revenue from the equivalence claim. Victim declarations map to frontline_operators (identity-locked into drill compliance), operating_organizations (constrained to purchase simulation to maintain licenses), and general_public (trapped as residual risk bearers with no exit from the safety regime). Operators have identity_locked exit because their professional certification and legal authorization are fused to simulation hours; organizations have constrained exit because alternative training models are not regulatorily recognized.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a rope would ignore the asymmetric extraction flowing to vendors and regulators and the suppression of alternative learning models. Classifying it as a snare would deny the genuine coordination value of repeatable, low-mortality training. Tangled rope is the only category that admits both the real competence-maintenance function and the real extractionâprovided we also satisfy the active-enforcement gate, which we do through regulatory licensing and accreditation requirements. If the coordination function were to atrophy while the enforcement persisted, the constraint would degrade toward piton; the temporal measurements show rising theater_ratio, which the lifecycle drift system can monitor for this transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_transfer_validity,
    'Does competence demonstrated in high-fidelity simulation environments transfer effectively to real catastrophic events, or does the equivalence claim rest on institutional convenience and vendor interests?',
    'Longitudinal studies comparing teams with simulation-only training versus teams with real-event or near-miss exposure, measuring real-event performance outcomes.',
    'If transfer is weak, the constraint''s extraction component dominates its coordination function, pushing classification toward snare. If transfer is strong, the coordination function is genuine and the classification remains tangled_rope or shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_transfer_validity, empirical, 'Whether simulated competence transfers to real catastrophes.').

omega_variable(
    kernel_reading_structural_delta,
    'If the simulation_as_proxy_catastrophe reading were abandoned for the catastrophe_as_necessary_selector reading, what structural rearrangement would occur in resource allocation and regulatory authority?',
    'Comparative institutional analysis of safety regimes that privilege simulation versus those that privilege post-catastrophe inquiry.',
    'Would reveal whether the current reading''s dominance is driven by empirical superiority or by the path-dependent interests of simulation vendors and regulators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Structural consequence of adopting a sibling reading.').

omega_variable(
    compliance_theater_vs_genuine_learning,
    'To what extent do scheduled drills function as inspectable compliance rituals versus genuine competence maintenance?',
    'Ethnographic observation of drill conduct and post-drill organizational behavior; analysis of discrepancy rates between drill protocols and real-event after-action reports.',
    'A high theater ratio would support piton classification if the coordination function has atrophied; low theater supports the current tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_theater_vs_genuine_learning, empirical, 'Drill ritualization versus actual skill transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sim_proxy_tr_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sim_proxy_tr_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 5, 0.18).
narrative_ontology:measurement(sim_proxy_tr_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 10, 0.28).
narrative_ontology:measurement(sim_proxy_tr_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 15, 0.38).
narrative_ontology:measurement(sim_proxy_tr_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 20, 0.48).
narrative_ontology:measurement(sim_proxy_tr_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 25, 0.54).
narrative_ontology:measurement(sim_proxy_tr_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(sim_proxy_be_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sim_proxy_be_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(sim_proxy_be_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(sim_proxy_be_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(sim_proxy_be_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(sim_proxy_be_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(sim_proxy_be_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sim_proxy_su_t0, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(sim_proxy_su_t5, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(sim_proxy_su_t10, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(sim_proxy_su_t15, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(sim_proxy_su_t20, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(sim_proxy_su_t25, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 25, 0.67).
narrative_ontology:measurement(sim_proxy_su_t30, catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, catastrophe_as_necessary_selector).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, hybrid_near_miss_learning).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_avoidance_retention kernel. Per the Îµ-invariance principle, each reading instantiates a structurally distinct constraint with its own Îµ, stakeholders, and classification. The three sibling readings form a constraint family linked by mutual structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
