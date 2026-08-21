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
 *   This constraint describes the 'hybrid dependency' reading of competence
 *   exercise in high-reliability organizations, particularly aviation. It
 *   asserts that while simulation is a necessary foundation for training, it
 *   is insufficient on its own. True competence requires periodic anchoring
 *   in real-world operations, non-jeopardy audits, or actual aircraft time to
 *   prevent skill atrophy and ensure readiness for unforeseen circumstances.
 *   This reading acknowledges the ethical and practical impossibility of
 *   relying solely on catastrophic events for competence exercise, as
 *   proposed by a sibling reading, and rejects the idea that simulation alone
 *   is adequate.
 *
 * KEY AGENTS:
 *   - Pilots: Primary beneficiaries, identity-locked to the profession.
 *   - Air Traffic Controllers: Primary beneficiaries, identity-locked to the profession.
 *   - Maintenance Crews: Primary beneficiaries, identity-locked to the profession.
 *   - Airline Operators: Agenda-setters, bear costs, benefit from safety.
 *   - Regulators: Agenda-setters, define and enforce standards.
 *   - Traveling Public: Ultimate beneficiaries of safety.
 *   - Simulation Providers: Beneficiaries, provide training technology.
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
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, '59bdada3-24e8-40d6-aef7-41d0d07dd27c').
narrative_ontology:cs_kernel_codification('59bdada3-24e8-40d6-aef7-41d0d07dd27c', formalized).
narrative_ontology:cs_authority_grounding('59bdada3-24e8-40d6-aef7-41d0d07dd27c', expertise).
narrative_ontology:cs_interpretation_layer_present('59bdada3-24e8-40d6-aef7-41d0d07dd27c').
narrative_ontology:cs_reading_relation('59bdada3-24e8-40d6-aef7-41d0d07dd27c', competence_exercise_requirement__simulation_as_adequate_exercise, influences).
narrative_ontology:cs_reading_relation('59bdada3-24e8-40d6-aef7-41d0d07dd27c', competence_exercise_requirement__catastrophe_as_necessary_anchor, coexists_with).
narrative_ontology:cs_axiom('59bdada3-24e8-40d6-aef7-41d0d07dd27c', foundational, competence_requires_multi_modal_exercise).
narrative_ontology:cs_axiom_status(competence_requires_multi_modal_exercise, holdable).
narrative_ontology:cs_axiom_grounding('59bdada3-24e8-40d6-aef7-41d0d07dd27c', competence_requires_multi_modal_exercise, empirically_contingent).
narrative_ontology:cs_axiom('59bdada3-24e8-40d6-aef7-41d0d07dd27c', foundational, catastrophic_exercise_is_unethical).
narrative_ontology:cs_axiom_status(catastrophic_exercise_is_unethical, holdable).
narrative_ontology:cs_axiom_grounding('59bdada3-24e8-40d6-aef7-41d0d07dd27c', catastrophic_exercise_is_unethical, deontological).
narrative_ontology:cs_reference_frame('59bdada3-24e8-40d6-aef7-41d0d07dd27c', post_automation_surprise_hybrid_model).
narrative_ontology:cs_drift_state('59bdada3-24e8-40d6-aef7-41d0d07dd27c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('59bdada3-24e8-40d6-aef7-41d0d07dd27c', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, pilots).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, air_traffic_controllers).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, maintenance_crews).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, airline_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, regulators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, traveling_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, simulation_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a robust training and exercise regime that maintains their skills and confidence. They are identity-locked to the profession, making adherence to competence standards non-negotiable for career progression and safety.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, pilots, beneficiary,
    moderate, biographical, identity_locked, global).

% Rely on continuous training and periodic real-world exposure to maintain their critical decision-making skills in complex, high-stakes environments. Their professional identity is tied to maintaining safety and efficiency.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, air_traffic_controllers, beneficiary,
    moderate, biographical, identity_locked, national).

% Require hands-on experience with actual aircraft and systems, alongside simulation, to develop and maintain the tactile and diagnostic skills necessary for safety-critical repairs. Their role is essential for operational integrity.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, maintenance_crews, beneficiary,
    moderate, biographical, identity_locked, local).

% Responsible for implementing and funding the hybrid training programs. They benefit from a competent workforce and reduced accident risk, but bear the costs of real-world anchoring exercises. Their reputation and financial viability depend on safety.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, airline_operators, agenda_setter,
    institutional, generational, constrained, global).

% Define and enforce the standards for competence exercise, balancing safety requirements with operational feasibility. They act as the ultimate arbiter of what constitutes adequate training and real-world anchoring.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, regulators, agenda_setter,
    institutional, generational, analytical, national).

% Are the ultimate beneficiaries of a highly competent and well-exercised aviation workforce, experiencing the safety and reliability that results from this constraint. Their exit options are limited to choosing alternative modes of transport or not traveling.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, traveling_public, beneficiary,
    organized, immediate, constrained, global).

% Benefit from the requirement for simulation-based training, as they provide the necessary technology and services. They have an interest in advocating for the continued integration of simulation in competence frameworks.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, simulation_providers, beneficiary,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that critical safety-related competencies across the aviation ecosystem (pilots, ATC, maintenance) are maintained through a balanced regime of simulation and real-world experience, preventing skill decay and ensuring readiness for unexpected events.
% TRANSFER_FUNCTION: Transfers resources (time, money, personnel) from airline operators and regulators to training programs, simulation providers, and line operations for the purpose of competence maintenance. It also transfers risk reduction to the traveling public.
% ABSENT_VOICES: Advocates for purely simulation-based training (who might argue for cost savings and reduced operational complexity) are present but their arguments are largely overridden by the safety imperative for real-world anchoring. Those who might advocate for only real-world training are absent due to the ethical and practical impossibility of such a regime.
% DISAPPEARANCE_RATIONALE: If this hybrid competence requirement vanished, the aviation industry would rapidly degrade. Competence would erode, accident rates would rise, and public trust would collapse, leading to a fundamental reorganization of air travel or its cessation.
% FOUNDING_PROBLEM: The inherent danger of aviation operations requires continuous, high-stakes competence. Early training relied solely on real-world flight, which was costly and dangerous. The introduction of simulation improved safety and efficiency, but over-reliance on it led to concerns about 'automation surprise' and loss of 'stick-and-rudder' skills.
% FOUNDING_PROBLEM_CORROBORATION: Aviation safety boards, accident investigation reports, and expert panels (e.g., NTSB, EASA, ICAO) consistently corroborate the need for both simulation and real-world anchoring, citing incidents where over-reliance on one or the other contributed to safety issues. This is attested by independent safety research and regulatory mandates.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.25) is relatively low, representing the necessary costs of maintaining a high-reliability system through a balanced training approach. This includes the expense of simulators, real-world operational time, and audit processes. Suppression (0.4) is moderate; while there are costs, the benefits of safety and competence are widely accepted, and alternatives (pure simulation or pure real-world) are seen as either inadequate or impractical/unethical. Theater ratio (0.1) is low, indicating that most activities are genuinely functional, though some compliance might be performative. The constraint is claimed as a Rope because it genuinely coordinates safety and competence, with widely distributed benefits and costs that are largely accepted as necessary.
 *
 * PERSPECTIVAL GAP:
 *   While all stakeholders generally agree on the necessity of competence, the airline operators bear the direct financial costs of implementing the hybrid regime, while the traveling public receives the safety benefits without direct cost. Regulators balance these interests. The identity-locked professionals (pilots, ATC, maintenance) experience the constraint as a necessary part of their professional identity and safety, making their 'exit' from the constraint equivalent to exiting their profession.
 *
 * DIRECTIONALITY LOGIC:
 *   Pilots, ATC, maintenance crews, and the traveling public are beneficiaries, as the constraint directly contributes to their safety and professional efficacy. Airline operators and regulators are agenda-setters, bearing the costs of implementation and enforcement while benefiting from a safe and compliant system. There are no direct 'victims' in this reading, as the costs are considered necessary for the collective good of safety. Simulation providers are beneficiaries of the demand for their services.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling by acknowledging the genuine coordination function of a hybrid training regime. It avoids the pitfall of seeing all training costs as 'extraction' when they are, in fact, essential investments in safety. It also avoids the 'false summit' of pure simulation, which might appear cheaper but would lead to degraded competence. The constraint's mandate (maintaining high-reliability competence) is live and continuously validated by operational experience and safety data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_threshold,
    'At what level of simulation fidelity and scenario complexity does simulation become ''sufficient'' for certain competencies, reducing the need for real-world anchoring?',
    'Empirical studies correlating simulation fidelity with real-world performance outcomes, and expert consensus from high-reliability organizations.',
    'If fidelity thresholds are higher than currently assumed, the ''simulation_as_adequate_exercise'' reading gains ground, potentially reducing costs but increasing risk if misjudged. If lower, the ''hybrid_dependency'' reading is further strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Determining the point at which simulation can replace real-world experience for specific skills.').

omega_variable(
    real_world_anchoring_cost_benefit,
    'What is the optimal frequency and intensity of real-world anchoring exercises to maintain competence, balancing cost, risk, and benefit?',
    'Longitudinal studies tracking competence decay rates against different real-world exposure schedules, combined with cost-benefit analysis for operators.',
    'An optimized schedule could reduce operational costs without compromising safety, strengthening the ''hybrid_dependency'' reading''s practical viability. Suboptimal schedules could lead to either excessive cost or insufficient competence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_world_anchoring_cost_benefit, empirical, 'Balancing the costs and benefits of real-world competence anchoring.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint a genuine ''hybrid_dependency'' or is it a ''simulation_as_adequate_exercise'' reading with an overly cautious real-world component, or a ''catastrophe_as_necessary_anchor'' reading with an ethically constrained simulation component?',
    'Analysis of regulatory and industry discourse, accident reports, and training curricula to identify the dominant underlying assumptions about competence acquisition and maintenance.',
    'Reclassification to ''simulation_as_adequate_exercise'' would imply that the real-world anchoring is an unnecessary cost. Reclassification to ''catastrophe_as_necessary_anchor'' would imply that current measures are insufficient and that only extreme events truly test competence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinguishing the ''hybrid_dependency'' reading from its sibling interpretations of competence exercise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1980, competence_exercise_requirement__hybrid_dependency, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(comp_tr_t1990, competence_exercise_requirement__hybrid_dependency, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(comp_tr_t2000, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(comp_tr_t2010, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(comp_tr_t2024, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comp_be_t1980, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(comp_be_t1990, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(comp_be_t2000, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2000, 0.23).
narrative_ontology:measurement(comp_be_t2010, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2010, 0.24).
narrative_ontology:measurement(comp_be_t2024, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1980, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(comp_su_t1990, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(comp_su_t2000, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(comp_su_t2010, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2010, 0.39).
narrative_ontology:measurement(comp_su_t2024, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'competence_exercise_requirement' kernel. It is linked to sibling readings 'simulation_as_adequate_exercise' and 'catastrophe_as_necessary_anchor' through the cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
