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
 *   constraint_id: competence_exercise_requirement__hybrid_dependency
 *   human_readable: Hybrid Competence Exercise Requirement
 *   domain: Safety Engineering / Organizational Learning / High-Reliability Organizations
 *
 * SUMMARY:
 *   This constraint describes the requirement for maintaining operational
 *   competence in high-reliability organizations through a hybrid approach:
 *   combining foundational simulation-based training with periodic real-world
 *   anchoring (e.g., line operations, non-jeopardy audits, actual aircraft
 *   time). It is a reading of the broader 'competence_exercise_requirement'
 *   kernel, specifically arguing against both pure simulation and reliance on
 *   catastrophic events for competence maintenance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.6).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.7).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.6).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Hybrid Competence Exercise Requirement").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "Safety Engineering / Organizational Learning / High-Reliability Organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, '2a0734cb-84a7-4c54-86f4-dd263c00777c').
narrative_ontology:cs_kernel_codification('2a0734cb-84a7-4c54-86f4-dd263c00777c', formalized).
narrative_ontology:cs_authority_grounding('2a0734cb-84a7-4c54-86f4-dd263c00777c', expertise).
narrative_ontology:cs_interpretation_layer_present('2a0734cb-84a7-4c54-86f4-dd263c00777c').
narrative_ontology:cs_reading_relation('2a0734cb-84a7-4c54-86f4-dd263c00777c', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_reading_relation('2a0734cb-84a7-4c54-86f4-dd263c00777c', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_axiom('2a0734cb-84a7-4c54-86f4-dd263c00777c', foundational, competence_requires_real_world_complexity).
narrative_ontology:cs_axiom_status(competence_requires_real_world_complexity, holdable).
narrative_ontology:cs_axiom_grounding('2a0734cb-84a7-4c54-86f4-dd263c00777c', competence_requires_real_world_complexity, empirically_contingent).
narrative_ontology:cs_axiom('2a0734cb-84a7-4c54-86f4-dd263c00777c', foundational, catastrophe_is_unacceptable_exercise).
narrative_ontology:cs_axiom_status(catastrophe_is_unacceptable_exercise, holdable).
narrative_ontology:cs_axiom_grounding('2a0734cb-84a7-4c54-86f4-dd263c00777c', catastrophe_is_unacceptable_exercise, deontological).
narrative_ontology:cs_reference_frame('2a0734cb-84a7-4c54-86f4-dd263c00777c', proactive_safety_management).
narrative_ontology:cs_drift_state('2a0734cb-84a7-4c54-86f4-dd263c00777c', contemporary_cost_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2a0734cb-84a7-4c54-86f4-dd263c00777c', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, high_reliability_organizations).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, public_safety).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, training_departments).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These organizations (e.g., airlines, nuclear power plants) establish and enforce the competence standards, benefiting from reduced risk and maintained operational integrity. They bear the ultimate responsibility for safety outcomes.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, high_reliability_organizations, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, high_reliability_organizations, beneficiary).

% Pilots, engineers, and other frontline personnel who must undergo rigorous training, including both simulation and periodic real-world exercises (line operations, audits, actual aircraft time). They bear the direct time and effort costs of maintaining competence.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, operators, payer,
    moderate, biographical, identity_locked, local).

% Responsible for designing, implementing, and funding the hybrid training programs. They face budget constraints and logistical challenges in integrating real-world anchoring into training curricula, often advocating for more cost-effective simulation-only approaches.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, training_departments, payer,
    organized, biographical, constrained, national).

% Governmental bodies (e.g., FAA, NRC) that mandate and audit competence exercise requirements. They enforce compliance and can impose penalties, ensuring organizations adhere to the hybrid model for public safety.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, regulators, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__hybrid_dependency, regulators, observer).

% The ultimate beneficiary of maintained operational competence, as it directly translates to reduced risk of accidents and catastrophic failures. The public has no direct agency in enforcing the constraint but bears the consequences of its failure.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, public_safety, beneficiary,
    powerless, generational, trapped, universal).

% Researchers, technology providers, and some organizational leaders who argue that high-fidelity simulation, combined with advanced debriefing techniques, is fully adequate for competence exercise and more cost-effective than real-world anchoring. They are excluded from setting the core requirement but influence its implementation.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, simulation_advocates, excluded,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that operational competence in high-stakes domains is robustly maintained by integrating theoretical knowledge, simulated practice, and periodic exposure to the unpredictable complexities of real-world operations, thereby coordinating safety standards across an organization.
% TRANSFER_FUNCTION: Transfers resources (time, budget, personnel effort) from training departments and individual operators towards the implementation and execution of real-world anchoring activities (e.g., line operations, non-jeopardy audits, actual aircraft time), in exchange for verified, resilient operational competence and reduced systemic risk.
% ABSENT_VOICES: Advocates for pure simulation-based training are structurally excluded from defining the core requirement, as their position (simulation as adequate) is foreclosed by this reading. They would argue for greater efficiency and cost savings by eliminating real-world anchoring.
% DISAPPEARANCE_RATIONALE: If this hybrid requirement vanished, organizations would likely shift towards cheaper, simulation-only training. This would lead to a gradual but significant erosion of operational competence, particularly in handling emergent, non-simulatable events, eventually resulting in an increase in safety incidents and catastrophic failures.
% FOUNDING_PROBLEM: The recognition that purely theoretical or simulated training, while efficient, fails to adequately prepare personnel for the unpredictable complexities, emergent properties, and 'dirty' realities of real-world high-stakes operations, leading to competence decay and an increased risk of catastrophic failures.
% FOUNDING_PROBLEM_CORROBORATION: Numerous accident investigations, safety reports, and independent studies across high-reliability domains (e.g., aviation, nuclear power, complex medical procedures) consistently corroborate the necessity of real-world experience beyond simulation for maintaining robust operational competence. This is attested by regulatory bodies and independent safety boards.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(competence_exercise_requirement__hybrid_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__hybrid_dependency, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__hybrid_dependency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__hybrid_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.66 at interval end) due to the significant costs (time, resources, logistical complexity) associated with implementing and maintaining real-world anchoring components. Suppression is high (0.76) because the requirement actively mandates this hybrid approach, suppressing cheaper, less robust alternatives like pure simulation. Theater ratio remains low (0.23) as real-world anchoring is difficult to fake and directly tied to observable operational performance. The metrics show a slight increase in extractiveness and suppression over time, reflecting the increasing cost and hardening of enforcement as the standard becomes more entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of training departments and operators, the constraint can feel highly extractive due to the costs and logistical burdens of real-world anchoring. From the perspective of high-reliability organizations and regulators, it is a necessary, albeit costly, coordination mechanism for ensuring public safety. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations and public safety are the primary beneficiaries, gaining from robust competence and reduced risk. Training departments and individual operators are the payers, bearing the direct costs and time commitments. Regulators act as agenda-setters and observers, enforcing the standard. Simulation advocates are excluded, as their preferred method is deemed insufficient by this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This hybrid approach directly addresses mandatrophy by ensuring that competence exercise remains anchored in the evolving realities of operational environments, preventing the mandate from atrophying into a purely theoretical or performative exercise. By explicitly requiring real-world exposure, it counters the drift towards simulation-only training that could lead to a disconnect between trained skills and actual operational demands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_hybrid_balance,
    'Is the current balance between simulation and real-world anchoring optimal, or does it over- or under-emphasize real-world components relative to cost and risk reduction?',
    'Longitudinal studies comparing competence decay rates and incident frequencies across different hybrid ratios, adjusted for operational complexity and cost.',
    'If the balance is suboptimal, re-calibration could lead to either reduced costs without compromising safety (if over-emphasized) or increased safety at acceptable cost (if under-emphasized), potentially shifting the constraint''s extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_hybrid_balance, empirical, 'Assessing the efficiency and effectiveness of the current hybrid training ratio.').

omega_variable(
    definition_of_competence,
    'How is ''competence'' defined and measured in this context? Does the definition adequately capture the full range of skills required for high-reliability operations, including adaptive expertise and resilience?',
    'Consensus among domain experts, validated by empirical studies of expert performance in both routine and emergent situations, and comparison with definitions from other high-reliability domains.',
    'A narrow definition might lead to under-training in critical areas, making the hybrid requirement less effective. A broader definition might necessitate more extensive real-world anchoring, increasing extractiveness but potentially enhancing safety.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_competence, conceptual, 'Clarifying the scope and measurement of operational competence.').

omega_variable(
    real_world_anchoring_feasibility,
    'To what extent are ''periodic real-world anchoring'' activities truly non-jeopardy and scalable, given increasing operational tempo and safety concerns?',
    'Operational data analysis, logistical feasibility studies, and pilot programs for new anchoring methods. Assessment of regulatory flexibility for alternative anchoring approaches.',
    'If non-jeopardy anchoring becomes increasingly difficult or impossible, the constraint might drift towards either pure simulation (if the ''catastrophe'' reading is rejected) or a de facto reliance on incidents, fundamentally altering its nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(real_world_anchoring_feasibility, empirical, 'Practical limits and scalability of real-world anchoring components.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__hybrid_dependency, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__hybrid_dependency, theater_ratio, 10, 0.19).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__hybrid_dependency, theater_ratio, 20, 0.2).
narrative_ontology:measurement(comp_tr_t30, competence_exercise_requirement__hybrid_dependency, theater_ratio, 30, 0.21).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__hybrid_dependency, theater_ratio, 40, 0.22).
narrative_ontology:measurement(comp_tr_t50, competence_exercise_requirement__hybrid_dependency, theater_ratio, 50, 0.23).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(comp_be_t30, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(comp_be_t50, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 50, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(comp_su_t30, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(comp_su_t50, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 50, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, safety_culture_maintenance).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, regulatory_compliance).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, organizational_learning_frameworks).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
