% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Competence Exercise Requirement: Catastrophe as Necessary Anchor
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint represents the belief within high-reliability
 *   organizations that only actual catastrophic events or near-misses provide
 *   the 'true' and irreducible exercise necessary to maintain operational
 *   competence. It posits that competence atrophies during long periods
 *   without major incidents, despite simulation or routine training, and that
 *   the first real event will expose this decay. This reading emphasizes
 *   'muscle memory' and the unique learning derived from high-stakes,
 *   real-world consequences, distinguishing it from merely 'knowing about' a
 *   procedure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.6).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.7).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.6).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, mountain).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Competence Exercise Requirement: Catastrophe as Necessary Anchor").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "safety_engineering/organizational_learning").

domain_priors:emerges_naturally(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, '78ebb796-2d2a-4c2b-9f3e-d98f82e31e9e').
narrative_ontology:cs_kernel_codification('78ebb796-2d2a-4c2b-9f3e-d98f82e31e9e', implicit).
narrative_ontology:cs_authority_grounding('78ebb796-2d2a-4c2b-9f3e-d98f82e31e9e', practice).
narrative_ontology:cs_interpretation_layer_present('78ebb796-2d2a-4c2b-9f3e-d98f82e31e9e').
narrative_ontology:cs_reading_relation('78ebb796-2d2a-4c2b-9f3e-d98f82e31e9e', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('78ebb796-2d2a-4c2b-9f3e-d98f82e31e9e', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('78ebb796-2d2a-4c2b-9f3e-d98f82e31e9e', foundational, real_consequence_is_irreducible_teacher).
narrative_ontology:cs_axiom_status(real_consequence_is_irreducible_teacher, holdable).
narrative_ontology:cs_axiom_grounding('78ebb796-2d2a-4c2b-9f3e-d98f82e31e9e', real_consequence_is_irreducible_teacher, empirically_contingent).
narrative_ontology:cs_axiom('78ebb796-2d2a-4c2b-9f3e-d98f82e31e9e', secondary, muscle_memory_requires_live_stress).
narrative_ontology:cs_axiom_status(muscle_memory_requires_live_stress, holdable).
narrative_ontology:cs_axiom_grounding('78ebb796-2d2a-4c2b-9f3e-d98f82e31e9e', muscle_memory_requires_live_stress, empirically_contingent).
narrative_ontology:cs_reference_frame('78ebb796-2d2a-4c2b-9f3e-d98f82e31e9e', catastrophe_driven_learning_cycle).
narrative_ontology:cs_drift_state('78ebb796-2d2a-4c2b-9f3e-d98f82e31e9e', contemporary_simulation_advances, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('78ebb796-2d2a-4c2b-9f3e-d98f82e31e9e', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, high_risk_industries_management).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, front_line_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the perceived 'naturalness' of this constraint, which can justify underinvestment in alternative training methods or downplay the risks of long periods without major incidents. Their professional identity is often tied to managing through crises.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, high_risk_industries_management, beneficiary,
    institutional, biographical, identity_locked, global).

% Bear the direct consequences of competence decay during 'quiet' periods, as they are the first to face the real-world challenges when a catastrophic event occurs. Their training may feel inadequate without the 'real' experience.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, front_line_operators, payer,
    moderate, immediate, constrained, local).

% Observe the outcomes of this dynamic, often struggling to mandate sufficient training or preparedness in the absence of recent catastrophic events. They are tasked with preventing incidents but may be constrained by the prevailing belief in 'real' experience.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_regulators, observer,
    institutional, generational, analytical, national).

% Their offerings are undervalued or underfunded if the core belief is that only real catastrophes provide true competence exercise. They would argue for the efficacy of high-fidelity simulation but are often sidelined by this 'catastrophe-as-anchor' axiom.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_developers, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the understanding of what constitutes 'true' competence in high-stakes environments, implicitly guiding training and preparedness strategies towards a reliance on real-world, high-consequence events.
% TRANSFER_FUNCTION: Transfers the burden of competence maintenance from proactive, continuous training and simulation to the reactive, high-cost learning derived from actual incidents. It also transfers a sense of 'natural' inevitability to the occurrence of such events.
% ABSENT_VOICES: Simulation developers and proponents of continuous, low-stakes operational learning are excluded. They would argue that competence can be built and maintained without waiting for or relying on catastrophic events, but their arguments are often dismissed as not addressing 'real' experience.
% DISAPPEARANCE_RATIONALE: If this belief vanished, high-risk industries would be forced to fundamentally rethink their training, simulation, and operational learning strategies. Investment in proactive competence building would likely increase, and the justification for 'learning by disaster' would erode, leading to a significant reorganization of safety protocols and resource allocation.
% FOUNDING_PROBLEM: The historical observation that complex systems often reveal unforeseen failure modes and competence gaps only under extreme, real-world stress, leading to a belief that such stress is indispensable for true readiness.
% FOUNDING_PROBLEM_CORROBORATION: This problem status is attested by historical accident investigations and the persistent challenge of predicting emergent failures in complex systems, corroborated by safety researchers and accident reconstruction experts, not just industry management.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, ExtMetricName, E),
    domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_exercise_requirement__catastrophe_as_necessary_anchor),
    narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) stems from the implicit cost of waiting for or relying on catastrophes for learning, leading to underinvestment in alternative, proactive competence-building. Suppression (0.7) arises from the difficulty of challenging this deeply ingrained belief, which often dismisses alternative training methods as 'unrealistic.' The theater ratio (0.2) is low because the belief is genuinely held, not merely performative, though it can lead to performative adherence to 'lessons learned' without addressing the underlying competence gap. The claimed type is Mountain because it is often presented as an irreducible truth about human and organizational learning in high-stakes environments.
 *
 * PERSPECTIVAL GAP:
 *   Management in high-risk industries may perceive this as a natural law of organizational learning, justifying current training paradigms. Front-line operators, however, experience the direct consequences of competence decay during quiet periods, facing the real-world challenges with potentially atrophied skills. Safety regulators struggle to mandate sufficient proactive training against this 'natural' belief.
 *
 * DIRECTIONALITY LOGIC:
 *   High-risk industries' management benefits from this constraint by having a justification for the status quo and potentially lower investment in costly, continuous, high-fidelity simulation. Front-line operators and the public are victims, bearing the costs of competence gaps revealed during actual incidents. Simulation developers are excluded, as their solutions are deemed insufficient by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification helps prevent mislabeling a deeply ingrained, potentially harmful belief as a benign 'natural law.' By identifying beneficiaries (management) and victims (operators, public), it highlights the extractive aspects of a constraint that, while appearing 'natural,' may lead to preventable harm. The 'mountain' claim, coupled with beneficiaries, triggers False Summit Mountain detection, indicating a potential reclassification to a more extractive type if the metrics align.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_or_cultural_belief,
    'Is the necessity of catastrophic events for competence exercise a genuine natural law of human learning in complex systems, or a deeply ingrained cultural belief within high-risk industries?',
    'Longitudinal studies of high-fidelity simulation efficacy in preventing real-world competence decay, coupled with cross-cultural comparisons of safety cultures that do not hold this belief.',
    'If a cultural belief, the constraint''s ''emerges_naturally'' flag would be false, reclassifying it from Mountain to a more constructed type (e.g., Snare or Tangled Rope) due to its extractive nature and identifiable beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_or_cultural_belief, conceptual, 'Distinguishing between a natural law and a cultural construct regarding competence maintenance.').

omega_variable(
    simulation_fidelity_threshold,
    'At what level of fidelity and realism can simulation provide ''irreducible exercise'' comparable to real catastrophic events, if at all?',
    'Empirical research comparing physiological, cognitive, and team performance metrics in high-fidelity simulations versus real-world incidents, focusing on transfer of learning and long-term retention.',
    'If a sufficiently high fidelity is achievable, the ''accessibility_collapse'' metric would decrease, and the ''suppression'' of alternative training methods would be challenged, potentially shifting the constraint towards a Rope or Scaffold if alternatives become viable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Determining if simulation can truly replicate the learning from real catastrophes.').

omega_variable(
    competence_decay_rate,
    'What is the actual rate of competence decay during catastrophe-free periods, and how does it vary across different skill sets and organizational contexts?',
    'Systematic measurement of skill retention and performance degradation over time in various operational roles, using objective performance indicators and expert assessments.',
    'A slower-than-assumed decay rate would reduce the perceived ''necessity'' of catastrophic events for re-anchoring competence, weakening the constraint''s hold and potentially reducing its extractiveness by making proactive maintenance more feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_decay_rate, empirical, 'Quantifying the rate at which operational competence degrades without real-world exercise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t1950, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(comp_tr_t1970, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(comp_tr_t1990, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(comp_tr_t2010, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(comp_tr_t2024, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comp_be_t1950, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(comp_be_t1970, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(comp_be_t1990, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(comp_be_t2010, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(comp_be_t2024, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t1950, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(comp_su_t1970, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(comp_su_t1990, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(comp_su_t2010, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(comp_su_t2024, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_budget_allocation).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, training_program_design).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, accident_investigation_framing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
