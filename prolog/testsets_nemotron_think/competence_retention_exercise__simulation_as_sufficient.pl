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
 *   human_readable: High-Fidelity Simulation as Sufficient Catastrophe-Avoidance Competence Exercise
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   The claim that high-fidelity simulation constitutes genuine exercise of
 *   catastrophe-avoidance competence — that cognitive and procedural demands
 *   are structurally equivalent to real events — is the dominant regulatory
 *   and industry paradigm since the 1970s. It originated in aviation (Link
 *   Trainer, then full-motion simulators) and spread to nuclear, maritime,
 *   and process industries. The constraint coordinates competence maintenance
 *   across distributed organizations without waiting for catastrophes. But it
 *   also extracts: simulator vendors capture mandated revenue, regulators
 *   gain legible compliance, and operators bear costs while absorbing
 *   residual risk. The sibling readings (catastrophe_as_necessary,
 *   near_miss_as_bridge) contest the sufficiency claim. This story
 *   instantiates the simulation_as_sufficient reading only — its ε is
 *   assessed against the standing arrangement (mandated simulation as primary
 *   competence mechanism), not against the alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, 0.65).
domain_priors:suppression_score(competence_retention_exercise__simulation_as_sufficient, 0.55).
domain_priors:theater_ratio(competence_retention_exercise__simulation_as_sufficient, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, extractiveness, 0.65).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(competence_retention_exercise__simulation_as_sufficient, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_retention_exercise__simulation_as_sufficient, tangled_rope).
narrative_ontology:human_readable(competence_retention_exercise__simulation_as_sufficient, "High-Fidelity Simulation as Sufficient Catastrophe-Avoidance Competence Exercise").
narrative_ontology:topic_domain(competence_retention_exercise__simulation_as_sufficient, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_retention_exercise__simulation_as_sufficient).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_retention_exercise__simulation_as_sufficient, 'cf4062e3-76c9-4972-ab74-c883a67c1991').
narrative_ontology:cs_kernel_codification('cf4062e3-76c9-4972-ab74-c883a67c1991', formalized).
narrative_ontology:cs_authority_grounding('cf4062e3-76c9-4972-ab74-c883a67c1991', extraction).
narrative_ontology:cs_interpretation_layer_present('cf4062e3-76c9-4972-ab74-c883a67c1991').
narrative_ontology:cs_reading_relation('cf4062e3-76c9-4972-ab74-c883a67c1991', competence_retention_exercise__catastrophe_as_necessary, forecloses).
narrative_ontology:cs_reading_relation('cf4062e3-76c9-4972-ab74-c883a67c1991', competence_retention_exercise__near_miss_as_bridge, coexists_with).
narrative_ontology:cs_axiom('cf4062e3-76c9-4972-ab74-c883a67c1991', foundational, simulation_fidelity_achieves_cognitive_equivalence).
narrative_ontology:cs_axiom_status(simulation_fidelity_achieves_cognitive_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('cf4062e3-76c9-4972-ab74-c883a67c1991', simulation_fidelity_achieves_cognitive_equivalence, empirically_contingent).
narrative_ontology:cs_axiom('cf4062e3-76c9-4972-ab74-c883a67c1991', secondary, simulator_metrics_validate_competence).
narrative_ontology:cs_axiom_status(simulator_metrics_validate_competence, holdable).
narrative_ontology:cs_axiom_grounding('cf4062e3-76c9-4972-ab74-c883a67c1991', simulator_metrics_validate_competence, conventional).
narrative_ontology:cs_reference_frame('cf4062e3-76c9-4972-ab74-c883a67c1991', simulation_equivalence_framework).
narrative_ontology:cs_drift_state('cf4062e3-76c9-4972-ab74-c883a67c1991', contemporary_post_accident_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cf4062e3-76c9-4972-ab74-c883a67c1991', '').
narrative_ontology:cs_kernel_id(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, simulator_vendors).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, regulatory_bodies).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, training_infrastructure_providers).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, operating_organizations).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, frontline_operators).
narrative_ontology:constraint_victim(competence_retention_exercise__simulation_as_sufficient, public_stakeholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_retention_exercise__simulation_as_sufficient, frontline_operators).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, simulator_fidelity_achieves_cognitive_equivalence).
narrative_ontology:constraint_vindicates(competence_retention_exercise__simulation_as_sufficient, distributed_competence_without_catastrophe).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, sell, and maintain high-fidelity simulators mandated by regulators. Revenue scales with fidelity requirements and recertification cycles. They define what 'high-fidelity' means through standards bodies they dominate. Exit is trivial — they capture the mandate.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, simulator_vendors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, simulator_vendors, agenda_setter).

% Mandate simulator hours for licensure and recurrent training. They gain legible compliance metrics, avoid political blame for accidents ("training was current"), and control the competence definition. They do not bear the cost of simulation infrastructure.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, regulatory_bodies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, regulatory_bodies, beneficiary).

% Operate training centers, employ instructors, sell courseware. They benefit from mandated volume but compete on price and fidelity. Their exit is constrained by capital investment in physical simulators.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, training_infrastructure_providers, beneficiary,
    organized, biographical, mobile, regional).

% Airlines, nuclear plants, chemical facilities — they pay for simulators, instructor time, crew downtime, and recurrent training programs. Costs are substantial (tens of millions annually for major airlines). They cannot exit the mandate without losing operating authority. Some invest in simulation because they believe it works; others comply because they must.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, operating_organizations, payer,
    powerful, biographical, constrained, global).

% Pilots, control room operators, bridge officers. They spend careers in simulators; their licensure, promotion, and professional identity depend on simulator performance. They gain genuine skill rehearsal but also absorb the risk if simulation proves insufficient. Exit means abandoning professional identity and licensure — identity_locked.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, frontline_operators, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(competence_retention_exercise__simulation_as_sufficient, frontline_operators, beneficiary).

% Communities near hazardous facilities, passengers, downstream populations. They bear the consequence if simulation-based competence fails to prevent catastrophe. They have no voice in simulator fidelity standards, no exit from the risk, and no visibility into training adequacy.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, public_stakeholders, payer,
    powerless, generational, trapped, local).

% Academics and independent investigators who study whether simulator performance predicts real-world catastrophe avoidance. They publish critique but have no regulatory authority. Their exit is analytical — they can change research focus.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, safety_culture_researchers, observer,
    analytical, civilizational, analytical, global).

% Families and communities affected by catastrophes that simulator-trained crews failed to prevent. They would object to the sufficiency claim but are structurally excluded from standards-setting and training design. Their exit from the consequence is impossible.
narrative_ontology:constraint_stakeholder(competence_retention_exercise__simulation_as_sufficient, accident_victims_families, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains distributed catastrophe-avoidance competence across high-hazard industries without requiring actual catastrophic failures to provide learning opportunities. Solves the 'rare event learning problem' by substituting structured simulation for stochastic disaster.
% TRANSFER_FUNCTION: Moves financial resources, personnel time, and career capital from operating organizations and frontline operators to simulator vendors, training infrastructure providers, and regulatory compliance regimes. Moves legitimacy from hard-won operational experience to quantifiable simulator metrics (hours, scores, scenario completion rates).
% ABSENT_VOICES: Operators who have survived actual catastrophes and assert the visceral stakes, time pressure, and consequence awareness cannot be simulated. Communities living with residual risk from facilities whose safety case rests on simulator metrics. Both are excluded from fidelity standard-setting bodies.
% DISAPPEARANCE_RATIONALE: If the simulation mandate vanished overnight, operating organizations would redirect billions from simulator contracts to alternative competence maintenance (operational experience programs, near-miss analysis, red-teaming, apprentice-journeyman models). Regulatory frameworks would lose their primary compliance metric. Simulator vendors would face existential revenue collapse. Career paths defined by simulator milestones would restructure.
% FOUNDING_PROBLEM: How to maintain reliable catastrophe-avoidance competence in high-hazard industries (aviation, nuclear, chemical, maritime) when catastrophic events are too rare and too costly to serve as learning opportunities for each generation of operators.
% FOUNDING_PROBLEM_CORROBORATION: Early simulator pioneers (Link, Redifon, CAE) and aviation regulators (FAA, ICAO 1960s-80s) attested the founding problem was real and simulation addressed it. However, accident investigators (Three Mile Island 1979, Challenger 1986, Air France 447 2009, 737 MAX 2018-19) and independent safety researchers (Reason, Perrow, Dekker, Woods) have repeatedly documented cases where simulator-trained crews failed in real catastrophes — challenging whether the founding problem was actually solved or merely displaced.
narrative_ontology:disappearance_verdict(competence_retention_exercise__simulation_as_sufficient, world_rearranges).
narrative_ontology:founding_problem_status(competence_retention_exercise__simulation_as_sufficient, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_retention_exercise__simulation_as_sufficient, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_retention_exercise__simulation_as_sufficient, 'none', 1).
narrative_ontology:epsilon_provenance(competence_retention_exercise__simulation_as_sufficient, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_retention_exercise__simulation_as_sufficient_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_retention_exercise__simulation_as_sufficient, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_retention_exercise__simulation_as_sufficient_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the large, mandated resource transfer from operators to simulation infrastructure that has grown decoupled from demonstrated marginal safety benefit — fidelity requirements escalate (Level D, then Level D+, then VR/AR) without proportional accident reduction. Suppression (0.55) is structural: regulatory mandates make simulation the only licit path; career systems make simulator performance the only promotion path. Theater ratio (0.45) captures the growing share of training that optimizes for scorer-visible metrics (procedure adherence, scenario completion) rather than adaptive expertise. Accessibility collapse (0.5) is moderate: organizations CAN invest in complementary methods (line operations safety audit, near-miss reporting, resilience engineering), but regulatory and insurance incentives pull toward simulation. Resistance (0.6) comes from line operators, some training captains, and researchers who observe simulator-real world gaps.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/vendor seat, the constraint is a rope (genuine coordination, minimal coercion). From the frontline operator seat, it is a tangled rope (coordination benefit real but extraction via identity lock and career capture). From the public stakeholder seat, it approaches a snare (extraction of safety margin via false equivalence, suppression of alternatives). The engine computes this seat divergence from the declared power/exit/role structure — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulator vendors and regulators are structural beneficiaries (d near 0.0): they collect revenue and compliance legitimacy respectively, with arbitrage-grade exit. Operating organizations are payers with constrained exit (d ~0.7): they pay the mandate but cannot leave the industry. Frontline operators are identity_locked payers (d ~0.85): their professional self-concept is fused to simulator performance; exit means identity death. Public stakeholders are trapped payers (d ~0.95): they bear consequence with zero voice. Researchers are analytical observers (d=0.5). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rare-event learning) was real. The simulation solution was a genuine coordination innovation. But the arrangement has accumulated extraction: fidelity mandates escalate beyond evidence, recertification cycles shorten, vendor lock-in deepens, and the equivalence claim has become doctrinal rather than empirical. The mandate persists not because the founding problem is unsolved, but because the solution created its own beneficiary coalition that now defends the mandate's expansion. This is mandatrophy: the coordination function is real but the extraction layer has thickened around it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_equivalence_claim,
    'Is the claimed cognitive and procedural equivalence between high-fidelity simulation and real catastrophe avoidance empirically established, or is it an article of faith that became regulatory doctrine?',
    'Systematic comparison of simulator performance metrics vs. real-event outcomes for the same crews/scenarios; neurocognitive studies of stress response in simulation vs. real emergencies; longitudinal tracking of simulator-trained crews'' first real catastrophe encounters.',
    'If equivalence is not established, the constraint''s coordination function is overstated and its extraction layer (mandated fidelity escalation) lacks justification — reclassification toward snare. If equivalence holds for defined scenario envelopes, the tangled_rope classification stands with narrower coordination scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_equivalence_claim, empirical, 'Whether the core equivalence claim is evidentially grounded or doctrinal').

omega_variable(
    extraction_coordination_boundary,
    'Where does the genuine coordination function of simulation end and the extractive layer (mandated fidelity escalation, vendor lock-in, career capture) begin?',
    'Cost-benefit analysis of marginal fidelity increments; regulatory capture assessment of standards bodies; comparison of simulator training outcomes vs. alternative competence maintenance investments (operational experience, near-miss systems, resilience drills).',
    'If the boundary is early (basic simulation suffices, escalation is rent), the constraint is snare with a thin coordination veneer. If the boundary is late (each fidelity increment has marginal safety value), tangled_rope with substantial coordination. The current metrics assume a mid-range boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_coordination_boundary, conceptual, 'Locating the coordination-extraction boundary within the mandated simulation regime').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative competence-maintenance paths structural (regulatory mandates, insurance requirements) or internalized (operators genuinely believe simulation is the only valid path, having never experienced alternatives)?',
    'Post-deregulation natural experiments: if a jurisdiction relaxes simulator mandates, do operators voluntarily maintain simulation at current levels (internalized) or revert to cheaper alternatives (structural)? Survey operators anonymously about whether they would choose current simulation volume absent mandates.',
    'If internalized, effective suppression is higher than structural measures suggest — operators carry the constraint with them. If purely structural, resistance would materialize rapidly if mandates relaxed. This affects whether the constraint''s persistence is fragile or robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the simulation mandate').

omega_variable(
    kernel_framing_alternatives,
    'Does the kernel ''competence retention exercise'' admit framings beyond the three declared readings — e.g., competence as distributed system property rather than individual skill, or competence as organizational resilience rather than operator performance?',
    'Review resilience engineering and Safety-II literature for alternative framings; map whether they instantiate new readings or recombine existing ones; assess if any reframes the extraction-coordination structure.',
    'If alternative framings exist that change the constraint''s ε or beneficiary structure, this reading''s decomposition is incomplete — additional constraint stories needed. If the three readings exhaust the kernel''s structural space, the family is complete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternatives, conceptual, 'Whether the declared kernel readings exhaust the structural possibilities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_retention_exercise__simulation_as_sufficient, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1960, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(comp_tr_t1975, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(comp_tr_t1990, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(comp_tr_t2005, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(comp_tr_t2015, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 2015, 0.43).
narrative_ontology:measurement(comp_tr_t2025, competence_retention_exercise__simulation_as_sufficient, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(comp_be_t1960, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(comp_be_t1975, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement(comp_be_t1990, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(comp_be_t2005, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(comp_be_t2015, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(comp_be_t2025, competence_retention_exercise__simulation_as_sufficient, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1960, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(comp_su_t1975, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(comp_su_t1990, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(comp_su_t2005, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(comp_su_t2015, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 2015, 0.53).
narrative_ontology:measurement(comp_su_t2025, competence_retention_exercise__simulation_as_sufficient, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_retention_exercise__simulation_as_sufficient, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_retention_exercise__simulation_as_sufficient, 0.08).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__catastrophe_as_necessary).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, competence_retention_exercise__near_miss_as_bridge).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, regulatory_capture__training_standards).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, simulator_vendor_lock_in).
narrative_ontology:affects_constraint(competence_retention_exercise__simulation_as_sufficient, operator_identity_fusion).

% DUAL FORMULATION NOTE:
% This constraint is the simulation_as_sufficient reading of the competence_retention_exercise kernel. It differs from catastrophe_as_necessary (ε≈0.3, claimed mountain/rope) and near_miss_as_bridge (ε≈0.45, claimed rope/tangled_rope) in its ε referent (mandated simulation infrastructure) and beneficiary structure (vendors + regulators). The three stories form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_retention_exercise__simulation_as_sufficient, moderate, 0.85).
constraint_indexing:directionality_override(competence_retention_exercise__simulation_as_sufficient, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
