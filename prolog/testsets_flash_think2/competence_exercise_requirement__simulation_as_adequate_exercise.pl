% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__simulation_as_adequate_exercise, []).

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
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: Competence Maintained by High-Fidelity Simulation
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This constraint represents the reading that high-fidelity simulation,
 *   coupled with debriefing, is fully adequate for exercising and maintaining
 *   competence in high-reliability organizations. This reading is often
 *   validated by periods of catastrophe-free operation and is supported by
 *   regulatory compliance frameworks. It stands in contrast to sibling
 *   readings that emphasize the necessity of real-world catastrophic
 *   experience or a hybrid approach.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.45).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.65).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.45).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "Competence Maintained by High-Fidelity Simulation").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, '847589cc-c9cb-4e1b-a2a9-87fcea9ed10c').
narrative_ontology:cs_kernel_codification('847589cc-c9cb-4e1b-a2a9-87fcea9ed10c', formalized).
narrative_ontology:cs_authority_grounding('847589cc-c9cb-4e1b-a2a9-87fcea9ed10c', expertise).
narrative_ontology:cs_interpretation_layer_present('847589cc-c9cb-4e1b-a2a9-87fcea9ed10c').
narrative_ontology:cs_reading_relation('847589cc-c9cb-4e1b-a2a9-87fcea9ed10c', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_reading_relation('847589cc-c9cb-4e1b-a2a9-87fcea9ed10c', competence_exercise_requirement__hybrid_dependency, forecloses).
narrative_ontology:cs_axiom('847589cc-c9cb-4e1b-a2a9-87fcea9ed10c', foundational, simulated_experience_transfers_to_real_world).
narrative_ontology:cs_axiom_status(simulated_experience_transfers_to_real_world, holdable).
narrative_ontology:cs_axiom_grounding('847589cc-c9cb-4e1b-a2a9-87fcea9ed10c', simulated_experience_transfers_to_real_world, empirically_contingent).
narrative_ontology:cs_axiom('847589cc-c9cb-4e1b-a2a9-87fcea9ed10c', foundational, catastrophe_is_avoidable_through_training).
narrative_ontology:cs_axiom_status(catastrophe_is_avoidable_through_training, holdable).
narrative_ontology:cs_axiom_grounding('847589cc-c9cb-4e1b-a2a9-87fcea9ed10c', catastrophe_is_avoidable_through_training, empirically_contingent).
narrative_ontology:cs_reference_frame('847589cc-c9cb-4e1b-a2a9-87fcea9ed10c', simulation_centric_competence_model).
narrative_ontology:cs_drift_state('847589cc-c9cb-4e1b-a2a9-87fcea9ed10c', contemporary_safety_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('847589cc-c9cb-4e1b-a2a9-87fcea9ed10c', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, safety_regulators).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_providers).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, high_reliability_organizations).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, organizations_with_alternative_training_models).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce standards for competence exercise, often mandating high-fidelity simulation as the primary method. They benefit from a standardized, auditable compliance pathway.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, safety_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Develop, sell, and maintain high-fidelity simulation systems and services. They directly profit from the widespread adoption and regulatory mandate of simulation for competence maintenance.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_providers, beneficiary,
    powerful, biographical, arbitrage, global).

% Operate in high-stakes environments and rely on simulation programs to maintain operational competence and regulatory compliance. They benefit from a clear, accepted path to demonstrate readiness, but bear the costs of implementation.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, high_reliability_organizations, beneficiary,
    organized, biographical, constrained, global).

% Undergo mandatory simulation training to maintain their professional certifications and operational readiness. They bear the direct burden of training and the potential risk if simulation proves insufficient for real-world challenges.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Advocate for or utilize alternative, non-simulation-centric methods for competence exercise (e.g., on-the-job training, apprenticeship, real-world drills). They are often excluded from regulatory acceptance or face significant hurdles to demonstrate equivalence.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, organizations_with_alternative_training_models, excluded,
    moderate, biographical, constrained, national).

% Academics and safety experts who argue that only real catastrophic events or near-misses provide the irreducible exercise necessary for true competence, viewing simulation as inherently limited.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, catastrophe_advocates, observer,
    analytical, generational, analytical, global).

% Academics and practitioners who advocate for a blend of simulation and periodic real-world anchoring (e.g., line operations, non-jeopardy audits) to maintain competence, seeing simulation as necessary but insufficient.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, hybrid_training_proponents, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_providers).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__simulation_as_adequate_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes and provides a controlled, repeatable environment for competence exercise and validation across high-stakes domains, ensuring a baseline of operational readiness without incurring real-world risks.
% TRANSFER_FUNCTION: Transfers significant financial resources from high-reliability organizations to simulation providers, and transfers a sense of regulatory compliance and operational readiness to the organizations and their operators.
% ABSENT_VOICES: Advocates for real-world catastrophic experience or a hybrid training approach are often marginalized in regulatory and industry discussions, which prioritize the perceived safety and efficiency of simulation-only models.
% DISAPPEARANCE_RATIONALE: If high-fidelity simulation was no longer considered adequate for competence exercise, high-reliability organizations would face a profound crisis in how to maintain and validate operational readiness. This would likely lead to a rapid increase in real-world incidents, a complete overhaul of training paradigms, and significant regulatory instability.
% FOUNDING_PROBLEM: How to maintain and validate operational competence in high-stakes environments without incurring the costs and risks of real-world failures, especially as systems become more complex and failure modes rarer.
% FOUNDING_PROBLEM_CORROBORATION: Simulation providers and safety regulators attest that the founding problem of safe competence maintenance remains live. However, critics (catastrophe advocates, hybrid proponents) acknowledge the problem but dispute the adequacy of simulation as the sole solution, citing ongoing debates in safety science and organizational learning.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).
:- end_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the significant investment required for simulation infrastructure and training, which can create barriers to entry or divert resources from other safety measures. Suppression (0.65) is high due to regulatory mandates that often prioritize or exclusively accept simulation, thereby suppressing alternative competence models. The theater ratio (0.40) indicates a moderate risk of performative compliance, where the focus shifts from genuine learning to merely meeting simulation hours or passing standardized checks. The claimed type 'rope' reflects this reading's internal view of the constraint as a functional coordination mechanism for safety.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, simulation is a robust and sufficient solution, leading to a 'rope' classification. However, from the perspective of those advocating for real-world experience or hybrid models, the same constraint might appear more extractive or even a 'snare' if it creates a false sense of security that leads to real-world failures. The engine's computation of per-seat types will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety regulators and simulation providers are clear beneficiaries, gaining authority and revenue, respectively. High-reliability organizations also benefit from a clear compliance path. Frontline operators and organizations with alternative training models bear the costs, either through mandatory training or exclusion from accepted methods. The 'catastrophe_advocates' and 'hybrid_training_proponents' are analytical observers, excluded from the direct operational loop but providing critical counter-narratives.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_efficacy_gap,
    'Does high-fidelity simulation truly transfer all necessary competence to real-world scenarios, or are there irreducible gaps that only real-world experience or catastrophic events can fill?',
    'Longitudinal studies comparing performance outcomes of simulation-only trained operators versus those with hybrid or real-world anchoring, especially in novel or high-stress situations not perfectly replicable in simulation.',
    'If significant gaps are found, the constraint''s claimed ''adequacy'' would be undermined, potentially reclassifying it towards a ''tangled_rope'' or ''snare'' due to the hidden risks it creates. If no gaps, the ''rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_efficacy_gap, empirical, 'The extent to which simulated experience fully translates to real-world competence.').

omega_variable(
    regulatory_capture_by_simulation_industry,
    'Is the regulatory acceptance of simulation as ''adequate'' driven primarily by genuine safety needs, or by the economic interests of simulation providers and the administrative convenience of regulators?',
    'Analysis of lobbying efforts by simulation providers, financial ties between regulators and the simulation industry, and comparative regulatory frameworks in jurisdictions with less developed simulation markets.',
    'If significant capture is evident, the constraint''s coordination function would be re-evaluated as a cover for extraction, shifting its classification towards a ''tangled_rope'' or ''snare'' for the regulated organizations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_by_simulation_industry, empirical, 'Whether regulatory standards for competence are influenced by industry interests.').

omega_variable(
    competence_definition_ambiguity,
    'Is ''competence'' in high-reliability organizations defined in a way that is amenable to simulation, potentially excluding aspects that are only revealed in real-world, high-consequence events?',
    'Conceptual analysis of competence definitions used in regulatory documents versus those emerging from post-incident analyses or expert elicitation from ''catastrophe advocates''.',
    'If the definition is found to be narrowly tailored to simulation capabilities, the constraint''s ''adequacy'' claim becomes a conceptual artifact, potentially leading to a reclassification as a ''snare'' for operators who are deemed ''competent'' but lack critical real-world skills.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_definition_ambiguity, conceptual, 'Whether the definition of competence itself is shaped by simulation capabilities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 5, 0.33).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 10, 0.36).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 15, 0.38).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(comp_be_t5, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(comp_be_t10, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(comp_be_t15, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(comp_be_t20, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comp_su_t5, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(comp_su_t10, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(comp_su_t15, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(comp_su_t20, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
