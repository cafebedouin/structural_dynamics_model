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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Competence Exercise Requirement: Hybrid Dependency
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This constraint, 'Competence Exercise Requirement: Hybrid Dependency,'
 *   asserts that maintaining high-reliability operational competence requires
 *   a blend of simulation-based training and periodic real-world anchoring
 *   (e.g., line operations, non-jeopardy audits, actual aircraft time). It is
 *   a reading of the broader 'competence_exercise_requirement' kernel,
 *   distinguishing itself from readings that prioritize pure simulation or
 *   catastrophic events as the sole anchors for competence. The constraint is
 *   framed as a Rope due to its clear coordination function and broad
 *   benefits, with moderate extractiveness reflecting the necessary
 *   investment in training infrastructure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.3).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.2).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.3).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Competence Exercise Requirement: Hybrid Dependency").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, '6989c001-18ed-483e-8837-4a63d54fda52').
narrative_ontology:cs_kernel_codification('6989c001-18ed-483e-8837-4a63d54fda52', formalized).
narrative_ontology:cs_authority_grounding('6989c001-18ed-483e-8837-4a63d54fda52', expertise).
narrative_ontology:cs_interpretation_layer_present('6989c001-18ed-483e-8837-4a63d54fda52').
narrative_ontology:cs_reading_relation('6989c001-18ed-483e-8837-4a63d54fda52', competence_exercise_requirement__simulation_as_adequate_exercise, influences).
narrative_ontology:cs_reading_relation('6989c001-18ed-483e-8837-4a63d54fda52', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_axiom('6989c001-18ed-483e-8837-4a63d54fda52', foundational, competence_is_situated_and_dynamic).
narrative_ontology:cs_axiom_status(competence_is_situated_and_dynamic, holdable).
narrative_ontology:cs_axiom_grounding('6989c001-18ed-483e-8837-4a63d54fda52', competence_is_situated_and_dynamic, empirically_contingent).
narrative_ontology:cs_axiom('6989c001-18ed-483e-8837-4a63d54fda52', foundational, catastrophic_learning_is_ethically_unacceptable).
narrative_ontology:cs_axiom_status(catastrophic_learning_is_ethically_unacceptable, holdable).
narrative_ontology:cs_axiom_grounding('6989c001-18ed-483e-8837-4a63d54fda52', catastrophic_learning_is_ethically_unacceptable, deontological).
narrative_ontology:cs_reference_frame('6989c001-18ed-483e-8837-4a63d54fda52', proactive_hybrid_competence_model).
narrative_ontology:cs_drift_state('6989c001-18ed-483e-8837-4a63d54fda52', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6989c001-18ed-483e-8837-4a63d54fda52', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, pilots).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, air_traffic_controllers).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, maintenance_crews).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, airline_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, passengers).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, hybrid_learning_theory).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__hybrid_dependency, situated_cognition_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a robust training and exercise regime that ensures their skills are sharp and adaptable. They participate in both simulation and real-world line operations, with periodic audits. Their professional identity is tied to maintaining high competence standards.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, pilots, beneficiary,
    moderate, biographical, identity_locked, global).

% Rely on a hybrid training model to manage complex and high-stakes situations. Simulation prepares them for rare events, while real-world experience and non-jeopardy audits anchor their decision-making in operational realities. Their role demands continuous, verified competence.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, air_traffic_controllers, beneficiary,
    moderate, biographical, identity_locked, national).

% Benefit from training that combines simulated fault diagnosis with hands-on experience on actual aircraft. This hybrid approach ensures they can troubleshoot effectively under pressure and perform precise repairs, directly impacting safety.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, maintenance_crews, beneficiary,
    moderate, biographical, identity_locked, local).

% Are responsible for implementing and funding the hybrid competence exercise requirements. They balance the costs of advanced simulation and real-world training with the imperative of safety and regulatory compliance. They set the standards and ensure adherence.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, airline_operators, agenda_setter,
    institutional, generational, constrained, global).

% Define and enforce the hybrid competence standards for aviation personnel. They mandate specific hours for simulation, line operations, and audit requirements, ensuring that the industry maintains a high level of safety. They act as the ultimate arbiter of competence definitions.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Are the ultimate beneficiaries of a robust competence exercise regime, as it directly contributes to their safety and trust in air travel. They have no direct input into the system but rely entirely on its effective functioning.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, passengers, beneficiary,
    powerless, immediate, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that all personnel in high-reliability organizations (e.g., aviation) maintain a consistent and high level of operational competence, integrating theoretical knowledge with practical application to manage complex, dynamic, and safety-critical systems.
% TRANSFER_FUNCTION: Transfers resources (time, money, personnel) from airline operators and regulatory bodies into training programs, simulation facilities, and real-world operational oversight, in exchange for verified and maintained operational competence across the workforce.
% ABSENT_VOICES: Advocates for purely simulation-based training (due to cost or risk aversion) or purely experience-based training (due to traditionalism) are marginalized; they would argue for a simpler, less resource-intensive approach but are excluded by the consensus on hybrid dependency.
% DISAPPEARANCE_RATIONALE: If this requirement vanished, competence levels would diverge, leading to a decline in safety, increased incidents, and a loss of public trust in aviation. The entire high-reliability system would need to fundamentally reorganize its approach to training and certification.
% FOUNDING_PROBLEM: Early aviation demonstrated that theoretical knowledge and pure simulation were insufficient for handling real-world operational complexities and unforeseen events; catastrophic failures often stemmed from a lack of 'anchored' competence.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigations, safety reports from independent bodies (e.g., NTSB, EASA), and academic research in human factors and organizational learning consistently corroborate the need for hybrid competence exercise. This is attested by safety engineers and human factors specialists outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).

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
 *   The extractiveness (0.3) is moderate, representing the significant but necessary investment in training, simulation, and operational oversight. Suppression (0.2) is low, as the requirement is largely accepted due to its clear safety benefits, though non-compliance carries severe penalties. Theater ratio (0.1) is low, indicating that the activities mandated are genuinely functional, not merely performative. The accessibility collapse (0.7) is high because there are few viable alternatives to a hybrid approach for maintaining high-reliability competence; resistance (0.15) is low due to broad acceptance of its necessity.
 *
 * PERSPECTIVAL GAP:
 *   While all stakeholders generally agree on the necessity of competence, the airline operators might experience the constraint as more extractive due to the direct financial costs, whereas operational personnel experience it as a beneficial framework for professional development and safety. Regulatory bodies view it as a critical coordination mechanism for public safety.
 *
 * DIRECTIONALITY LOGIC:
 *   Pilots, air traffic controllers, maintenance crews, and passengers are primary beneficiaries, directly gaining from enhanced safety and competence. Airline operators and regulatory bodies act as agenda-setters, bearing the costs of implementation and enforcement but also benefiting from a stable, safe operational environment and public trust. There are no identifiable 'victims' in the traditional sense, as the costs are broadly distributed and accepted as necessary for the collective good.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_threshold,
    'At what level of simulation fidelity and complexity does simulation become ''sufficient'' for certain aspects of competence exercise, reducing the need for real-world anchoring?',
    'Empirical studies comparing performance outcomes of purely high-fidelity simulation training versus hybrid training for specific skill sets, coupled with expert consensus on ''transfer of training'' effectiveness.',
    'If high-fidelity simulation is proven sufficient for more domains, the extractiveness (cost) of the hybrid model could decrease, potentially shifting the constraint towards a purer Rope or even a Mountain for those specific competencies. If not, the hybrid model''s necessity is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Determining the optimal balance between simulation and real-world training based on technological advancements.').

omega_variable(
    catastrophe_necessity_ambiguity,
    'Is the ''catastrophe_as_necessary_anchor'' reading entirely foreclosed by the hybrid approach, or does it represent an irreducible, albeit ethically undesirable, component of ultimate system learning?',
    'Philosophical and ethical debate on the nature of ''ultimate'' learning in complex systems, combined with historical analysis of safety improvements following major incidents. This is a conceptual, not empirical, resolution.',
    'If ''catastrophe_as_necessary_anchor'' is found to hold any irreducible truth, it would introduce a profound ethical tension into the ''hybrid_dependency'' reading, potentially reclassifying aspects of the system as a Snare (unacknowledged victims of learning) or a Tangled Rope (learning at others'' expense).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_necessity_ambiguity, conceptual, 'The conceptual tension between proactive hybrid training and reactive learning from catastrophe.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1950, competence_exercise_requirement__hybrid_dependency, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(comp_tr_t1970, competence_exercise_requirement__hybrid_dependency, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(comp_tr_t1990, competence_exercise_requirement__hybrid_dependency, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(comp_tr_t2010, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(comp_tr_t2024, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comp_be_t1950, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(comp_be_t1970, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(comp_be_t1990, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(comp_be_t2010, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2010, 0.29).
narrative_ontology:measurement(comp_be_t2024, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1950, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(comp_su_t1970, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(comp_su_t1990, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 1990, 0.18).
narrative_ontology:measurement(comp_su_t2010, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2010, 0.19).
narrative_ontology:measurement(comp_su_t2024, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, aviation_safety_regulations).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'competence_exercise_requirement' kernel, focusing on the hybrid dependency of simulation and real-world anchoring. It is linked to sibling readings that emphasize pure simulation or catastrophic events as competence anchors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
