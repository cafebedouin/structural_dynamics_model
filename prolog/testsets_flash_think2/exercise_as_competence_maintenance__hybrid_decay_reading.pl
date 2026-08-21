% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__hybrid_decay_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Competence Maintenance via Hybrid Decay (Simulation vs. Judgment)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This constraint describes the situation where organizations rely on
 *   simulation exercises to maintain competence for high-stakes operations.
 *   While simulation effectively trains procedural skills and muscle memory,
 *   it fails to exercise judgment and improvisation capacity under real-world
 *   pressure. This 'hybrid decay' reading acknowledges the partial
 *   coordination function of simulation but highlights the hidden extraction
 *   of critical judgment skills, leading to a gap between perceived and
 *   actual preparedness. This is one reading of the
 *   'exercise_as_competence_maintenance' kernel, focusing on the differential
 *   decay of competence components.
 *
 * KEY AGENTS:
 *   - organizational_leadership: Agenda setter (institutional/constrained) — benefits from perceived competence, avoids risk.
 *   - simulation_providers: Beneficiary (organized/arbitrage) — profits from simulation adoption.
 *   - frontline_responders: Payer (organized/identity_locked) — maintains procedural skills, but judgment decays.
 *   - public_safety_advocates: Observer (organized/mobile) — raises concerns about preparedness gaps.
 *   - victims_of_failure: Payer (powerless/trapped) — bears the ultimate cost of judgment failure.
 *   - crisis_management_experts: Observer (analytical/analytical) — provides nuanced analysis of simulation limits.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.78).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.85).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Competence Maintenance via Hybrid Decay (Simulation vs. Judgment)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, 'a5a8c59e-d487-4fa2-a59c-616879b2d7aa').
narrative_ontology:cs_kernel_codification('a5a8c59e-d487-4fa2-a59c-616879b2d7aa', formalized).
narrative_ontology:cs_authority_grounding('a5a8c59e-d487-4fa2-a59c-616879b2d7aa', expertise).
narrative_ontology:cs_interpretation_layer_present('a5a8c59e-d487-4fa2-a59c-616879b2d7aa').
narrative_ontology:cs_reading_relation('a5a8c59e-d487-4fa2-a59c-616879b2d7aa', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('a5a8c59e-d487-4fa2-a59c-616879b2d7aa', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('a5a8c59e-d487-4fa2-a59c-616879b2d7aa', foundational, simulation_retains_procedural_competence).
narrative_ontology:cs_axiom_status(simulation_retains_procedural_competence, holdable).
narrative_ontology:cs_axiom_grounding('a5a8c59e-d487-4fa2-a59c-616879b2d7aa', simulation_retains_procedural_competence, empirically_contingent).
narrative_ontology:cs_axiom('a5a8c59e-d487-4fa2-a59c-616879b2d7aa', foundational, judgment_under_stakes_requires_real_stakes_exercise).
narrative_ontology:cs_axiom_status(judgment_under_stakes_requires_real_stakes_exercise, holdable).
narrative_ontology:cs_axiom_grounding('a5a8c59e-d487-4fa2-a59c-616879b2d7aa', judgment_under_stakes_requires_real_stakes_exercise, empirically_contingent).
narrative_ontology:cs_reference_frame('a5a8c59e-d487-4fa2-a59c-616879b2d7aa', competence_as_hybrid_skillset).
narrative_ontology:cs_drift_state('a5a8c59e-d487-4fa2-a59c-616879b2d7aa', contemporary_safety_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a5a8c59e-d487-4fa2-a59c-616879b2d7aa', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_leadership).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_providers).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, public_safety_advocates).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, victims_of_failure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for ensuring organizational competence and managing risk. They champion simulation as a cost-effective way to maintain skills and demonstrate compliance, benefiting from the perception of preparedness while avoiding the high costs and risks of real-stakes exercises.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Develop and sell simulation technologies and services. They benefit directly from the widespread adoption of simulation as the primary method for competence maintenance, often emphasizing its fidelity and comprehensiveness.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_providers, beneficiary,
    organized, biographical, arbitrage, global).

% Participate in simulation exercises, which effectively maintain their procedural competence and muscle memory. However, they bear the cost of decaying judgment and improvisation skills under real-stakes pressure, which simulations cannot fully replicate. Their professional identity is tied to preparedness, making exit from the training regime difficult.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders, payer,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders, beneficiary).

% Monitor organizational preparedness and advocate for robust safety standards. They observe the gap between simulated competence and real-world performance, often raising concerns about the limitations of simulation and the potential for catastrophic failure.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, public_safety_advocates, observer,
    organized, generational, mobile, national).

% Those who suffer direct harm when organizational judgment fails under real crisis conditions, despite extensive simulation training. They bear the ultimate cost of the unexercised judgment capacity.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, victims_of_failure, payer,
    powerless, immediate, trapped, local).

% Provide analytical insights into organizational learning and crisis response. They understand the nuanced limitations of simulation and the distinct requirements for exercising judgment under stakes, often publishing research that challenges the sufficiency of simulation-only approaches.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, crisis_management_experts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_leadership).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__hybrid_decay_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, repeatable, and relatively safe method for organizations to train and maintain procedural competence and muscle memory for complex tasks, ensuring a baseline level of preparedness across personnel.
% TRANSFER_FUNCTION: Transfers perceived competence and reduced immediate risk to organizational leadership, while transferring the cost of unexercised judgment and improvisation capacity to frontline responders and ultimately to those affected by real-world failures.
% ABSENT_VOICES: The 'future victims' of a crisis where judgment fails are absent from the current conversation about training efficacy. Their perspective would highlight the critical gap in competence maintenance that simulation cannot fill.
% DISAPPEARANCE_RATIONALE: If the constraint (reliance on simulation for competence maintenance) vanished, organizations would face a stark choice: either accept a significant drop in procedural competence or invest in prohibitively expensive and risky real-stakes exercises, fundamentally reorganizing training and preparedness strategies.
% FOUNDING_PROBLEM: The need to maintain high levels of operational competence for high-stakes, low-frequency events without incurring the immense costs, risks, and ethical dilemmas of real-world training scenarios.
% FOUNDING_PROBLEM_CORROBORATION: Organizational leadership and simulation providers attest the problem is live and simulation is the best solution. Crisis management experts and public safety advocates attest that while the original problem of safe training is addressed, a new problem of unexercised judgment has emerged, making the status 'contested'.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates procedural competence (benefiting organizations and responders) but simultaneously extracts the capacity for judgment under stakes (costing responders and potential victims). Extractiveness is high (0.78) due to the significant, often unacknowledged, cost of decaying judgment. Suppression (0.85) is high because the organizational reliance on simulation actively suppresses alternatives for full judgment exercise, often due to the prohibitive cost and risk of real-stakes training. Theater ratio (0.60) is substantial because simulation creates a strong *perception* of full competence maintenance, masking the decay of judgment. Resistance is moderate (0.68) from experts and advocates who challenge the sufficiency of simulation.
 *
 * PERSPECTIVAL GAP:
 *   Organizational leadership and simulation providers perceive the constraint as a highly effective Rope, delivering competence and safety. Frontline responders experience a hybrid: procedural competence is maintained (Rope-like), but the anxiety of unexercised judgment under stakes makes it feel more extractive. Public safety advocates and crisis management experts see the full Tangled Rope structure, recognizing both the coordination and the hidden extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Organizational leadership and simulation providers are beneficiaries, as they gain perceived competence, reduced risk, and profit, respectively. Frontline responders are payers for the decay of judgment, despite benefiting from procedural training. Victims of failure are pure targets. The high suppression ensures that the organizational reliance on simulation is maintained, limiting alternatives for full competence exercise.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (which would ignore the decay of judgment) or a pure Snare (which would ignore the genuine procedural competence gained). By identifying it as a Tangled Rope, the analysis highlights the dual function: a coordination mechanism that, through its very operation, creates an asymmetric cost in a critical, non-simulable domain. The founding problem (safe training for high-stakes events) is partially solved, but the solution itself introduces a new, hidden problem (decay of judgment), leading to a 'contested' status for the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is competence maintenance primarily achieved through simulation (simulation_sufficiency_reading), only through real catastrophe (lived_catastrophe_necessity_reading), or through a hybrid mechanism with differential decay (hybrid_decay_reading)?',
    'Longitudinal studies comparing performance in real crises across organizations with varying simulation reliance and real-stakes exercise frequency, combined with cognitive science research on judgment under extreme pressure.',
    'Resolution would determine the optimal training and preparedness strategies, potentially reclassifying the constraint as a more effective Rope (if simulation is sufficient) or a more severe Snare (if only catastrophe works and simulation is pure theater).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'The fundamental contest over how competence is truly maintained for high-stakes events.').

omega_variable(
    judgment_decay_rate,
    'What is the precise decay rate of judgment and improvisation capacity under stakes, given a specific frequency and fidelity of simulation exercises?',
    'Empirical research combining psychological studies of decision-making under stress with post-incident analysis of real-world failures, correlating training regimes with performance outcomes.',
    'Quantifying the decay rate would allow for more accurate calculation of the ''extraction'' component, potentially shifting the constraint''s classification towards a higher or lower extractiveness, and informing policy on required real-stakes exposure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judgment_decay_rate, empirical, 'Empirical measurement of the rate at which non-simulable competence decays.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (reliance on simulation, avoidance of real-stakes exercise) primarily structural (cost, risk, regulatory barriers) or internalized (organizational culture, belief in simulation''s sufficiency)?',
    'Policy experiments that reduce structural barriers to real-stakes exercises: if reliance on simulation persists, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as organizations carry the suppression with them even if external barriers are removed. This would reinforce the Tangled Rope or even Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in organizational competence maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(exer_tr_t5, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(exer_tr_t10, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(exer_tr_t15, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement(exer_tr_t25, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 25, 0.6).
narrative_ontology:measurement(exer_tr_t30, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 30, 0.6).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(exer_be_t5, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 5, 0.69).
narrative_ontology:measurement(exer_be_t10, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(exer_be_t15, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(exer_be_t25, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement(exer_be_t30, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(exer_su_t5, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(exer_su_t10, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(exer_su_t15, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 15, 0.82).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 20, 0.83).
narrative_ontology:measurement(exer_su_t25, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 25, 0.84).
narrative_ontology:measurement(exer_su_t30, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading (hybrid_decay_reading) of the 'exercise_as_competence_maintenance' kernel, which also includes 'simulation_sufficiency_reading' and 'lived_catastrophe_necessity_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
