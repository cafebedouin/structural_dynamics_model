% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__hybrid_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Competence Exercise Requirement: Hybrid Dependency Reading
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid dependency' reading of the
 *   competence exercise requirement kernel. It asserts that operational
 *   competence in high-stakes domains (like aviation) cannot be maintained by
 *   simulation alone, nor can it rely solely on real-world, high-jeopardy
 *   events. Instead, it requires a foundational layer of simulation combined
 *   with periodic, structured real-world anchoring (e.g., line operations,
 *   non-jeopardy audits, actual aircraft time). This reading acknowledges the
 *   necessity of simulation for cost-effectiveness and safety, but critically
 *   emphasizes its insufficiency without real-world exposure to bridge the
 *   'reality gap'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.25).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.4).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.25).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Competence Exercise Requirement: Hybrid Dependency Reading").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, '0063c03c-deb0-4108-bd6e-ba68ca2ff4c9').
narrative_ontology:cs_kernel_codification('0063c03c-deb0-4108-bd6e-ba68ca2ff4c9', formalized).
narrative_ontology:cs_authority_grounding('0063c03c-deb0-4108-bd6e-ba68ca2ff4c9', expertise).
narrative_ontology:cs_interpretation_layer_present('0063c03c-deb0-4108-bd6e-ba68ca2ff4c9').
narrative_ontology:cs_reading_relation('0063c03c-deb0-4108-bd6e-ba68ca2ff4c9', competence_exercise_requirement__simulation_as_adequate_exercise, influences).
narrative_ontology:cs_reading_relation('0063c03c-deb0-4108-bd6e-ba68ca2ff4c9', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_axiom('0063c03c-deb0-4108-bd6e-ba68ca2ff4c9', foundational, simulation_necessary_but_insufficient).
narrative_ontology:cs_axiom_status(simulation_necessary_but_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('0063c03c-deb0-4108-bd6e-ba68ca2ff4c9', simulation_necessary_but_insufficient, empirically_contingent).
narrative_ontology:cs_axiom('0063c03c-deb0-4108-bd6e-ba68ca2ff4c9', foundational, real_world_anchoring_is_irreducible).
narrative_ontology:cs_axiom_status(real_world_anchoring_is_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('0063c03c-deb0-4108-bd6e-ba68ca2ff4c9', real_world_anchoring_is_irreducible, empirically_contingent).
narrative_ontology:cs_reference_frame('0063c03c-deb0-4108-bd6e-ba68ca2ff4c9', integrated_competence_model).
narrative_ontology:cs_drift_state('0063c03c-deb0-4108-bd6e-ba68ca2ff4c9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0063c03c-deb0-4108-bd6e-ba68ca2ff4c9', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, pilots).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, air_traffic_controllers).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, traveling_public).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, airline_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, training_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain high levels of operational competence through a blend of simulation and periodic real-world exposure, ensuring readiness for complex scenarios. They benefit from the safety margins this hybrid approach provides but bear the time and cost of diverse training.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, pilots, beneficiary,
    moderate, biographical, constrained, global).

% Benefit from the robust competence of pilots and other operational staff, which reduces their workload and risk. Their own training regime also follows a hybrid model, balancing simulation with live operational oversight and non-jeopardy audits.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, air_traffic_controllers, beneficiary,
    moderate, biographical, constrained, national).

% Define and enforce the hybrid competence exercise requirements, balancing safety imperatives with operational feasibility. They are responsible for ensuring the system maintains high reliability and for investigating incidents.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Bear the direct costs of implementing and maintaining hybrid training programs, including investment in advanced simulators, scheduling real aircraft time for training, and conducting line audits. They are incentivized by safety records and regulatory compliance.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, airline_operators, payer,
    powerful, biographical, constrained, global).

% Must develop and deliver training programs that meet the hybrid requirements, investing in diverse instructional methods and equipment. They face pressure to optimize costs while maintaining high standards.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, training_organizations, payer,
    organized, biographical, constrained, regional).

% Are the ultimate beneficiaries of the high safety standards ensured by the hybrid competence exercise regime, experiencing reduced risk of aviation incidents. Their trust in the system is paramount.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, traveling_public, beneficiary,
    powerless, immediate, constrained, global).

% Argue that high-fidelity simulation is sufficient for competence exercise and that real-world anchoring is an unnecessary cost. They are excluded from the current regulatory consensus that mandates hybrid training.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, simulation_only_advocates, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a consistent and robust standard of operational competence across a complex, high-stakes system by integrating diverse training modalities, preventing skill decay and over-reliance on simulated environments.
% TRANSFER_FUNCTION: Transfers resources (time, money, personnel) from airline operators and training organizations to the maintenance of a hybrid competence regime, which in turn transfers safety benefits to operational staff and the traveling public.
% ABSENT_VOICES: Advocates for simulation-only training are largely excluded from the regulatory bodies that set these standards, as their arguments for cost reduction are seen as potentially compromising safety. Those who believe only catastrophic events truly anchor competence are also excluded, as their approach is ethically and practically untenable.
% DISAPPEARANCE_RATIONALE: If the hybrid competence exercise requirement vanished, training would likely drift towards cheaper, simulation-only models, leading to a gradual erosion of real-world operational readiness. This would increase the risk of incidents, erode public trust, and force a re-evaluation of safety protocols after a period of increased failures.
% FOUNDING_PROBLEM: The challenge of maintaining high operational competence in complex, high-consequence domains where real-world failures are unacceptable, and pure simulation risks creating 'simulator pilots' who lack critical real-world anchoring.
% FOUNDING_PROBLEM_CORROBORATION: Safety regulators, accident investigators, and independent human factors experts consistently corroborate the ongoing need for hybrid training, citing historical incidents where over-reliance on simulation contributed to errors, and the inherent limitations of even high-fidelity simulators. This is attested by numerous post-accident reports and academic studies.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__hybrid_dependency, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Rope because it genuinely solves a critical coordination problem (maintaining competence for safety) with net benefits for most participants, despite some extraction from operators and training organizations. Extractiveness (0.25) is moderate, reflecting the necessary costs of a robust, multi-modal training system. Suppression (0.4) is present as regulatory mandates enforce compliance, but alternatives (pure simulation, pure real-world) are not suppressed in a coercive sense, but rather deemed inadequate for safety. Theater ratio (0.1) is low, as the activities are genuinely functional. Accessibility collapse (0.7) is high because the 'alternative' of inadequate training is effectively collapsed by safety imperatives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the traveling public and operational staff, this is a clear Rope, ensuring their safety. From the perspective of airline operators and training organizations, it's a costly but necessary burden, with elements of extraction due to the mandated investment. The regulators see it as a balanced, evidence-based approach to high reliability.
 *
 * DIRECTIONALITY LOGIC:
 *   Pilots, air traffic controllers, and the traveling public are clear beneficiaries, gaining safety and competence. Safety regulators act as agenda-setters, defining and enforcing the standards. Airline operators and training organizations are payers, bearing the costs of implementation. Advocates for simulation-only approaches are excluded, as their framing is deemed insufficient for the safety goals.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_hybrid_balance,
    'What is the optimal balance between simulation and real-world anchoring for different operational roles and levels of experience?',
    'Longitudinal studies tracking competence metrics against varying hybrid training ratios, combined with incident analysis to identify skill gaps related to training modality.',
    'Refining the balance could optimize training costs while maintaining or improving safety, potentially shifting the extractiveness for payers and the perceived value for beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_hybrid_balance, empirical, 'Determining the most effective mix of simulation and real-world training.').

omega_variable(
    reality_gap_measurement,
    'How precisely can the ''reality gap'' between high-fidelity simulation and actual operational environments be quantified for different tasks?',
    'Development of standardized metrics and experimental protocols to measure performance degradation or transfer-of-training deficits when moving from simulation to real-world tasks.',
    'A clearer understanding of the reality gap would strengthen the justification for real-world anchoring, potentially increasing suppression on simulation-only alternatives, or conversely, allowing for more targeted and efficient real-world exposure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reality_gap_measurement, empirical, 'Quantifying the difference between simulated and real operational environments.').

omega_variable(
    ethical_cost_of_catastrophe_reading,
    'Is the ''catastrophe_as_necessary_anchor'' reading ethically defensible, given the high human cost of real-world failures?',
    'Philosophical and ethical analysis, combined with a societal consensus on acceptable risk and the value of human life in high-reliability domains.',
    'If deemed ethically indefensible, the ''catastrophe_as_necessary_anchor'' reading would be foreclosed from any legitimate policy consideration, reinforcing the hybrid dependency as the only viable path.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ethical_cost_of_catastrophe_reading, preference, 'Ethical defensibility of relying on catastrophic events for competence anchoring.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.08).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_requirement__hybrid_dependency, theater_ratio, 5, 0.09).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__hybrid_dependency, theater_ratio, 10, 0.09).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_requirement__hybrid_dependency, theater_ratio, 15, 0.1).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__hybrid_dependency, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(comp_be_t5, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 10, 0.23).
narrative_ontology:measurement(comp_be_t15, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 20, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comp_su_t5, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(comp_su_t15, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 15, 0.39).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, aviation_safety_standards).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, pilot_licensing_requirements).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
