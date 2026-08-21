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
 *   human_readable: Hybrid Competence Exercise Requirement in High-Reliability Operations
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This constraint describes the requirement in high-reliability
 *   organizations (e.g., aviation, nuclear power) that competence is
 *   maintained not just through high-fidelity simulation, but also through
 *   periodic real-world anchoring activities like line operations,
 *   non-jeopardy audits, and actual equipment time. This constraint is one
 *   reading of the broader 'competence_exercise_requirement' kernel,
 *   emphasizing a hybrid approach to prevent skill fragility and ensure
 *   resilience against unforeseen real-world contingencies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__hybrid_dependency, 0.6).
domain_priors:suppression_score(competence_exercise_requirement__hybrid_dependency, 0.7).
domain_priors:theater_ratio(competence_exercise_requirement__hybrid_dependency, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, extractiveness, 0.6).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(competence_exercise_requirement__hybrid_dependency, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__hybrid_dependency, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__hybrid_dependency, "Hybrid Competence Exercise Requirement in High-Reliability Operations").
narrative_ontology:topic_domain(competence_exercise_requirement__hybrid_dependency, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__hybrid_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__hybrid_dependency, '7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c').
narrative_ontology:cs_kernel_codification('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c', formalized).
narrative_ontology:cs_authority_grounding('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c', expertise).
narrative_ontology:cs_interpretation_layer_present('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c').
narrative_ontology:cs_reading_relation('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c', competence_exercise_requirement__catastrophe_as_necessary_anchor, coexists_with).
narrative_ontology:cs_axiom('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c', foundational, simulation_insufficient_for_full_competence).
narrative_ontology:cs_axiom_status(simulation_insufficient_for_full_competence, holdable).
narrative_ontology:cs_axiom_grounding('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c', simulation_insufficient_for_full_competence, empirically_contingent).
narrative_ontology:cs_axiom('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c', foundational, real_world_anchoring_necessary_for_resilience).
narrative_ontology:cs_axiom_status(real_world_anchoring_necessary_for_resilience, holdable).
narrative_ontology:cs_axiom_grounding('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c', real_world_anchoring_necessary_for_resilience, empirically_contingent).
narrative_ontology:cs_axiom('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c', secondary, catastrophe_unacceptable_primary_exercise).
narrative_ontology:cs_axiom_status(catastrophe_unacceptable_primary_exercise, holdable).
narrative_ontology:cs_axiom_grounding('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c', catastrophe_unacceptable_primary_exercise, deontological).
narrative_ontology:cs_reference_frame('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c', proactive_safety_regime).
narrative_ontology:cs_drift_state('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7ca9d0b1-1c6a-4169-9d8c-a7bb5b5cc40c', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__hybrid_dependency, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, public_safety).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, regulatory_bodies).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, high_reliability_organizations).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, pilots_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__hybrid_dependency, training_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__hybrid_dependency, public_safety_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates and enforces the hybrid training and exercise requirements, ensuring compliance through audits and certification processes. Benefits from enhanced safety records and public trust.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from reduced operational risk, enhanced safety, and improved resilience against unforeseen events. Bears the significant cost of implementing and maintaining hybrid training programs and real-world anchoring activities.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, high_reliability_organizations, beneficiary,
    institutional, generational, constrained, global).

% Must undergo more extensive and varied training, including periodic real-world line operations, non-jeopardy audits, and actual aircraft time, which is more demanding and time-consuming than purely simulation-based training. Their competence is directly maintained by this regime.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, pilots_operators, payer,
    moderate, biographical, constrained, global).

% Must develop and maintain more complex and expensive hybrid training programs, requiring investment in real-world assets, specialized instructors, and advanced curriculum design. They bear the cost of delivering the mandated training.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, training_providers, payer,
    organized, biographical, constrained, global).

% Benefits directly from the increased safety and reliability of critical systems (e.g., aviation, nuclear power) that operate under this competence regime. They advocate for robust safety standards.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, public_safety_advocates, beneficiary,
    organized, generational, analytical, global).

% Argue that high-fidelity simulation is sufficient and more cost-effective for competence maintenance, but their view is not adopted by this constraint, which mandates real-world anchoring. They are excluded from the policy-setting conversation.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__hybrid_dependency, simulation_only_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a consistent, high level of operational competence across critical roles by integrating diverse training modalities, preventing skill decay and over-reliance on simulated environments, thereby coordinating collective safety outcomes.
% TRANSFER_FUNCTION: Transfers resources (time, money, access to real assets) from operators and training providers to the maintenance of a robust, hybrid competence system, ultimately benefiting public safety and organizational reliability by mitigating catastrophic risk.
% ABSENT_VOICES: Advocates for purely simulation-based training, who would argue for cost efficiency and the adequacy of high-fidelity simulators, are structurally excluded from the framing that mandates real-world anchoring. They would object to the additional costs and perceived redundancy.
% DISAPPEARANCE_RATIONALE: If this requirement vanished, organizations would likely revert to cheaper, simulation-only training, leading to a gradual erosion of real-world operational competence, increased risk of human error, and potential catastrophic failures in high-stakes environments. The entire safety ecosystem would reorganize around a lower standard of preparedness.
% FOUNDING_PROBLEM: Historical incidents and near-misses revealed critical gaps in operational competence that purely simulated training failed to address, leading to a fragile understanding of complex operational realities and an inability to handle unforeseen real-world contingencies.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation reports from national safety boards, independent academic studies in human factors and organizational psychology, and expert consensus from high-reliability industry associations consistently corroborate the need for real-world anchoring beyond simulation, from sources outside the direct beneficiaries.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__hybrid_dependency, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__hybrid_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__hybrid_dependency, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (ensuring safety and competence) but imposes significant, asymmetric costs on operators and training providers. Extractiveness (0.6) is moderate-high due to the substantial resources required for real-world anchoring beyond cheaper simulation. Suppression (0.7) is high because it actively mandates specific training modalities and excludes purely simulation-based alternatives through regulatory enforcement. Theater ratio is low (0.1) as the activities are genuinely functional for safety, not merely performative. Resistance is moderate (0.5) from those who prefer more cost-effective, simulation-only approaches.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of regulatory bodies and public safety, this constraint is a necessary and effective coordination mechanism for collective safety. From the perspective of operators and training providers, it is a costly, actively enforced requirement that extracts significant resources, even if they acknowledge its safety benefits. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and public safety advocates are clear beneficiaries, gaining from enhanced safety and reduced risk. High-reliability organizations are also beneficiaries in terms of reduced catastrophic risk, but simultaneously payers due to the implementation costs. Pilots, operators, and training providers are primary payers, bearing the direct costs in time, effort, and financial investment for the more rigorous hybrid training. Simulation-only advocates are excluded, as their preferred method is suppressed by this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_hybrid_balance,
    'What is the optimal balance between simulation and real-world anchoring for competence maintenance, given evolving technology (e.g., VR/AR fidelity) and operational complexity?',
    'Longitudinal studies comparing performance outcomes across different hybrid ratios, cost-benefit analyses, and expert consensus from safety boards and human factors research.',
    'A shift in the optimal balance could lead to adjustments in the mandated hybrid ratio, potentially reallocating costs and benefits among stakeholders, or even challenging the constraint''s current structural form if simulation becomes demonstrably more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_hybrid_balance, empirical, 'Uncertainty about the precise optimal mix of simulation and real-world exercise for competence.').

omega_variable(
    competence_exercise_kernel_reading_validity,
    'Is the ''hybrid_dependency'' reading the most effective and ethically sound approach to competence exercise, compared to purely simulation-based or catastrophe-driven alternatives?',
    'Comparative analysis of safety records, operational resilience, and ethical considerations across organizations and industries adopting different readings of the ''competence_exercise_requirement'' kernel.',
    'If alternative readings prove equally or more effective/ethical, this constraint''s legitimacy and persistence would be challenged, potentially leading to its reclassification or replacement by a different structural approach to competence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_exercise_kernel_reading_validity, conceptual, 'This constraint is one reading of the ''competence_exercise_requirement'' kernel, and its validity relative to sibling readings is an open question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__hybrid_dependency, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t2000, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(comp_tr_t2005, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(comp_tr_t2010, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(comp_tr_t2015, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(comp_tr_t2020, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(comp_tr_t2025, competence_exercise_requirement__hybrid_dependency, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(comp_be_t2000, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(comp_be_t2005, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2005, 0.54).
narrative_ontology:measurement(comp_be_t2010, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(comp_be_t2015, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(comp_be_t2020, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(comp_be_t2025, competence_exercise_requirement__hybrid_dependency, base_extractiveness, 2025, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t2000, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(comp_su_t2005, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(comp_su_t2010, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(comp_su_t2015, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(comp_su_t2020, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(comp_su_t2025, competence_exercise_requirement__hybrid_dependency, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__hybrid_dependency, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, safety_certification_standards).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, operational_risk_management).
narrative_ontology:affects_constraint(competence_exercise_requirement__hybrid_dependency, human_factors_training_protocols).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'competence_exercise_requirement' kernel, each with different structural properties and stakeholder impacts. The other readings are 'simulation_as_adequate_exercise' and 'catastrophe_as_necessary_anchor'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
