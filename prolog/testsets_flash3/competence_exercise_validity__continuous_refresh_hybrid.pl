% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__continuous_refresh_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__continuous_refresh_hybrid, []).

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
 *   constraint_id: competence_exercise_validity__continuous_refresh_hybrid
 *   human_readable: Continuous Competence Refresh via Hybrid Drills
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint represents the view that effective competence retention
 *   in high-stakes domains requires a continuous, hybrid approach combining
 *   simulation with other forms of drill and exercise, rather than relying
 *   solely on one-time validation or simulation as a complete proxy for
 *   real-world experience. It is a reading of the
 *   'competence_exercise_validity' kernel, emphasizing process-dependence
 *   over state-validation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__continuous_refresh_hybrid, 0.35).
domain_priors:suppression_score(competence_exercise_validity__continuous_refresh_hybrid, 0.2).
domain_priors:theater_ratio(competence_exercise_validity__continuous_refresh_hybrid, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, extractiveness, 0.35).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(competence_exercise_validity__continuous_refresh_hybrid, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__continuous_refresh_hybrid, rope).
narrative_ontology:human_readable(competence_exercise_validity__continuous_refresh_hybrid, "Continuous Competence Refresh via Hybrid Drills").
narrative_ontology:topic_domain(competence_exercise_validity__continuous_refresh_hybrid, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_validity__continuous_refresh_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__continuous_refresh_hybrid, 'e9173a61-54dd-46ea-bb30-d8daa48c833c').
narrative_ontology:cs_kernel_codification('e9173a61-54dd-46ea-bb30-d8daa48c833c', formalized).
narrative_ontology:cs_authority_grounding('e9173a61-54dd-46ea-bb30-d8daa48c833c', expertise).
narrative_ontology:cs_interpretation_layer_present('e9173a61-54dd-46ea-bb30-d8daa48c833c').
narrative_ontology:cs_reading_relation('e9173a61-54dd-46ea-bb30-d8daa48c833c', competence_exercise_validity__simulation_as_proxy, influences).
narrative_ontology:cs_reading_relation('e9173a61-54dd-46ea-bb30-d8daa48c833c', competence_exercise_validity__real_catastrophe_only, coexists_with).
narrative_ontology:cs_axiom('e9173a61-54dd-46ea-bb30-d8daa48c833c', foundational, competence_is_perishable_and_dynamic).
narrative_ontology:cs_axiom_status(competence_is_perishable_and_dynamic, holdable).
narrative_ontology:cs_axiom_grounding('e9173a61-54dd-46ea-bb30-d8daa48c833c', competence_is_perishable_and_dynamic, empirically_contingent).
narrative_ontology:cs_axiom('e9173a61-54dd-46ea-bb30-d8daa48c833c', foundational, hybrid_exercise_optimizes_transfer_to_real_world).
narrative_ontology:cs_axiom_status(hybrid_exercise_optimizes_transfer_to_real_world, holdable).
narrative_ontology:cs_axiom_grounding('e9173a61-54dd-46ea-bb30-d8daa48c833c', hybrid_exercise_optimizes_transfer_to_real_world, empirically_contingent).
narrative_ontology:cs_reference_frame('e9173a61-54dd-46ea-bb30-d8daa48c833c', post_catastrophe_learning_paradigm).
narrative_ontology:cs_drift_state('e9173a61-54dd-46ea-bb30-d8daa48c833c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e9173a61-54dd-46ea-bb30-d8daa48c833c', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__continuous_refresh_hybrid, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, high_reliability_organizations).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__continuous_refresh_hybrid, training_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These organizations (e.g., nuclear power, aviation, emergency services) implement and fund continuous drill cycles and hybrid training to maintain operational competence. They benefit from reduced incident rates and sustained public trust, but bear the direct costs of training.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, high_reliability_organizations, agenda_setter,
    institutional, generational, constrained, national).

% Operators (pilots, control room staff, first responders) directly participate in drills and simulations. They benefit from enhanced skills, confidence, and safety, but must dedicate significant time and effort to continuous training, sometimes at the expense of other duties.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, frontline_operators, beneficiary,
    moderate, biographical, constrained, local).

% These bodies (e.g., FAA, NRC) mandate and oversee competence retention programs. They benefit from improved safety records and public confidence in regulated industries. Their role is to ensure compliance and effectiveness of training regimes.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, safety_regulators, beneficiary,
    institutional, generational, analytical, national).

% Companies and institutions specializing in simulation and drill design, execution, and assessment. They benefit financially from contracts with HROs and contribute expertise to the continuous refresh model.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, training_providers, beneficiary,
    organized, biographical, mobile, regional).

% These groups argue that high-fidelity simulation is sufficient for competence retention, reducing the need for costly and disruptive live drills. They are excluded from the core decision-making that mandates hybrid approaches.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__continuous_refresh_hybrid, advocates_for_simulation_only, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that complex operational teams and individuals maintain a high level of competence and readiness for rare, high-consequence events through a structured, continuous program of varied exercises.
% TRANSFER_FUNCTION: Transfers resources (time, money, personnel) from operational budgets to training and exercise programs, in exchange for sustained competence and reduced risk of catastrophic failure.
% ABSENT_VOICES: Advocates for simulation-only approaches are often marginalized, arguing that the 'continuous refresh' model is overly burdensome and that advanced simulation can achieve similar outcomes more efficiently. Their perspective is often dismissed by those committed to hybrid methods.
% DISAPPEARANCE_RATIONALE: If the requirement for continuous, hybrid competence refresh vanished, organizations would likely revert to less frequent, less comprehensive training, leading to competence decay, increased risk of incidents, and a decline in safety standards across high-reliability sectors.
% FOUNDING_PROBLEM: Catastrophic failures in complex systems (e.g., Three Mile Island, Challenger disaster) revealed that one-time training and reliance on theoretical knowledge were insufficient to prevent human error and ensure effective response in high-stress situations.
% FOUNDING_PROBLEM_CORROBORATION: Safety regulators, accident investigators, and independent academic researchers consistently corroborate that the underlying problem of competence decay and the need for continuous exercise remains live, citing ongoing near-misses and the inherent complexity of modern systems. This is attested from outside the direct beneficiaries of the training industry.
narrative_ontology:disappearance_verdict(competence_exercise_validity__continuous_refresh_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__continuous_refresh_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__continuous_refresh_hybrid, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(competence_exercise_validity__continuous_refresh_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__continuous_refresh_hybrid, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).
:- end_tests(competence_exercise_validity__continuous_refresh_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the significant but generally accepted costs of continuous training. Suppression (0.20) is low, as organizations largely adopt this model due to its proven safety benefits, not coercion, though regulatory mandates play a role. Theater ratio (0.10) is low, indicating that the drills are genuinely functional, not merely performative. The slight increase in extractiveness and suppression over time reflects the increasing complexity of systems and the corresponding need for more intensive training, which is a genuine cost of maintaining safety.
 *
 * PERSPECTIVAL GAP:
 *   While all stakeholders generally agree on the need for competence, the 'continuous refresh' model is experienced differently. HROs bear the financial and logistical burden, operators the time commitment, while regulators and training providers primarily benefit. The core divergence is with those who believe simulation alone is sufficient, seeing the hybrid model as an unnecessary cost.
 *
 * DIRECTIONALITY LOGIC:
 *   High-reliability organizations and safety regulators are primary beneficiaries, gaining safety and legitimacy. Frontline operators also benefit from enhanced competence and safety, though they bear the direct burden of participation. Training providers benefit financially. There are no direct 'victims' in this reading, as the costs are seen as necessary for the collective good of safety. Advocates for simulation-only are 'excluded' as their alternative is not fully adopted.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_sufficiency_threshold,
    'At what level of fidelity and complexity can simulation become ''sufficient'' for competence retention, reducing the need for other drill types?',
    'Empirical studies comparing long-term competence decay rates and incident rates between simulation-only and hybrid training groups in specific high-reliability domains.',
    'If simulation is proven sufficient at a lower cost, the extractiveness of the ''continuous refresh'' model would be re-evaluated as higher than necessary, potentially shifting the constraint towards a Tangled Rope or Snare for the ''payer'' seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_sufficiency_threshold, empirical, 'Determining the point at which simulation can fully substitute for other forms of competence exercise.').

omega_variable(
    process_vs_state_competence,
    'Is competence fundamentally a dynamic process (requiring continuous exercise) or a measurable state (validated by periodic assessment)?',
    'Conceptual analysis and philosophical debate within safety science, informed by cognitive psychology and organizational theory, to clarify the ontological nature of ''competence'' in complex adaptive systems.',
    'If competence is primarily a state, the ''continuous refresh'' model might be seen as over-engineering, increasing its perceived extractiveness. If it is a process, the model''s justification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(process_vs_state_competence, conceptual, 'The fundamental nature of competence as a process versus a state.').

omega_variable(
    kernel_reading_difference,
    'What are the precise structural differences between the ''continuous_refresh_hybrid'' reading and its siblings (''simulation_as_proxy'', ''real_catastrophe_only'')?',
    'Detailed comparative analysis of the core axioms, beneficiary/victim structures, and proposed enforcement mechanisms of each reading, as instantiated in policy and practice.',
    'Clarifying these differences helps to understand why different parties hold different readings and where the fundamental disagreements lie, informing potential pathways for resolution or managed coexistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Structural differences between this reading and its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__continuous_refresh_hybrid, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 0, 0.08).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 5, 0.09).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 10, 0.1).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 15, 0.1).
narrative_ontology:measurement(comp_tr_t20, competence_exercise_validity__continuous_refresh_hybrid, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comp_be_t5, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(comp_be_t15, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(comp_be_t20, competence_exercise_validity__continuous_refresh_hybrid, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(comp_su_t5, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 5, 0.17).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(comp_su_t15, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(comp_su_t20, competence_exercise_validity__continuous_refresh_hybrid, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__continuous_refresh_hybrid, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
