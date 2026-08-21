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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Simulation for Competence Maintenance (Hybrid Decay Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This constraint describes the practice of using simulation exercises for
 *   competence maintenance in high-stakes domains, specifically from the
 *   'hybrid decay' reading of the 'exercise_as_competence_maintenance'
 *   kernel. This reading posits that simulation effectively maintains
 *   procedural competence and muscle memory, but fails to adequately exercise
 *   or maintain judgment and improvisation capacity under genuine stakes. The
 *   unaddressed decay in these critical non-procedural components leads to a
 *   hidden, extractive cost borne by those affected by real-world failures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.7).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.65).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, snare).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Simulation for Competence Maintenance (Hybrid Decay Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, '92af07c4-d891-43ed-922d-241c0ecd30a6').
narrative_ontology:cs_kernel_codification('92af07c4-d891-43ed-922d-241c0ecd30a6', formalized).
narrative_ontology:cs_authority_grounding('92af07c4-d891-43ed-922d-241c0ecd30a6', expertise).
narrative_ontology:cs_interpretation_layer_present('92af07c4-d891-43ed-922d-241c0ecd30a6').
narrative_ontology:cs_reading_relation('92af07c4-d891-43ed-922d-241c0ecd30a6', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('92af07c4-d891-43ed-922d-241c0ecd30a6', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_axiom('92af07c4-d891-43ed-922d-241c0ecd30a6', foundational, competence_is_multidimensional).
narrative_ontology:cs_axiom_status(competence_is_multidimensional, holdable).
narrative_ontology:cs_axiom_grounding('92af07c4-d891-43ed-922d-241c0ecd30a6', competence_is_multidimensional, empirically_contingent).
narrative_ontology:cs_axiom('92af07c4-d891-43ed-922d-241c0ecd30a6', foundational, judgment_requires_stakes).
narrative_ontology:cs_axiom_status(judgment_requires_stakes, holdable).
narrative_ontology:cs_axiom_grounding('92af07c4-d891-43ed-922d-241c0ecd30a6', judgment_requires_stakes, empirically_contingent).
narrative_ontology:cs_reference_frame('92af07c4-d891-43ed-922d-241c0ecd30a6', holistic_competence_model).
narrative_ontology:cs_drift_state('92af07c4-d891-43ed-922d-241c0ecd30a6', contemporary_training_paradigm, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('92af07c4-d891-43ed-922d-241c0ecd30a6', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_providers).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_leadership).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, crisis_victims).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and sell simulation platforms and services, benefiting from the widespread adoption of simulation as a training method. Their business model relies on organizations believing in the efficacy of simulation for competence maintenance across all dimensions.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Mandates and funds simulation exercises to meet regulatory requirements and internal safety goals. They benefit from the perception of preparedness and compliance, but are constrained by budget and the difficulty of implementing high-stakes real-world training.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_leadership, agenda_setter,
    institutional, biographical, constrained, national).

% Participate in simulations, developing procedural muscle memory and familiarity with systems. They bear the cost of time and effort, and may develop a false sense of security regarding their readiness for high-stakes judgment calls, as the simulations don't fully prepare them for the psychological and ethical pressures of real crises.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Suffer the consequences when organizational competence, particularly in judgment and improvisation, fails during a real crisis. They are the ultimate bearers of the unaddressed decay in non-simulated competence, often without prior knowledge or consent.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, crisis_victims, payer,
    powerless, immediate, trapped, local).

% Oversee organizational safety protocols, often requiring simulation exercises. They evaluate compliance but may lack the deep empirical data to assess the full efficacy of simulation for all aspects of competence, especially the non-procedural elements like judgment under extreme pressure.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, safety_regulators, observer,
    institutional, generational, analytical, national).

% Study how organizations learn and adapt, including the effectiveness and limitations of simulation. They provide critical analysis of the gap between procedural competence and judgment under stakes, often advocating for more nuanced training approaches.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_learning_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To standardize and regularly refresh procedural competence across large organizations, ensuring a baseline level of operational readiness for predictable scenarios and system interactions.
% TRANSFER_FUNCTION: Transfers financial resources from organizations to simulation providers, and transfers the burden of maintaining full, high-stakes judgment capacity from comprehensive training programs to the individual operators, ultimately transferring unmitigated risk to potential crisis victims.
% ABSENT_VOICES: Advocates for more frequent, high-fidelity, and psychologically realistic training that includes genuine stakes; those who have experienced the gap between simulated and real-world crisis response; and those harmed by failures in judgment during crises.
% DISAPPEARANCE_RATIONALE: Without simulation, even procedural competence would rapidly decay in complex systems, leading to more frequent and severe operational failures. However, the specific *gap* in judgment-under-stakes would become more apparent, potentially forcing a re-evaluation of training paradigms.
% FOUNDING_PROBLEM: How to maintain high levels of operational competence for rare, high-consequence events in complex systems without incurring the prohibitive costs or risks of constant real-world exposure.
% FOUNDING_PROBLEM_CORROBORATION: Simulation providers and organizational leadership attest to the ongoing need for procedural training. Organizational learning theorists and crisis victims (or their advocates) corroborate the contested status, highlighting the persistent gap in judgment-under-stakes, which is often unacknowledged by those benefiting from the current system.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.7) because the unacknowledged decay in judgment capacity leads to real harms and costs during crises, effectively extracting safety and resilience from the system. Suppression is moderate-high (0.65) as the widespread belief in simulation's sufficiency, coupled with the difficulty and cost of alternatives, suppresses calls for more comprehensive training. Theater ratio is moderate (0.4) because while some simulation activity genuinely contributes to procedural readiness, a significant portion serves to fulfill compliance requirements and maintain the illusion of full preparedness, rather than addressing the judgment gap. Accessibility collapse is moderate (0.5) because while alternatives like real-stakes training exist, they are often prohibitively expensive or risky. Resistance is low-moderate (0.3) because the benefits of simulation (procedural competence, compliance) are visible, while the costs of judgment decay are diffuse and only apparent during rare, high-consequence events.
 *
 * PERSPECTIVAL GAP:
 *   Organizational leadership and simulation providers perceive this constraint as a necessary and effective coordination mechanism for safety. In contrast, frontline operators may experience a gap between simulated and real-world performance, and crisis victims bear the full, unacknowledged cost of this gap. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation providers are clear beneficiaries, profiting from the widespread adoption of simulation. Organizational leadership benefits from perceived compliance and preparedness. Frontline operators are payers, investing time and effort, and potentially suffering from a false sense of security regarding their full competence. Crisis victims are the primary targets, bearing the ultimate costs of unaddressed competence decay. Safety regulators and organizational learning theorists act as observers, with the latter often highlighting the structural asymmetries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_for_judgment,
    'To what extent can simulation fidelity be increased to effectively exercise judgment and improvisation under genuine stakes, rather than just procedural competence?',
    'Longitudinal studies comparing performance in high-fidelity, psychologically stressful simulations with real-world crisis outcomes, or controlled experiments with genuine consequence mechanisms.',
    'If high fidelity can bridge the gap, the constraint''s extractiveness (from decay) would decrease, and its classification might shift towards a Rope; if not, the current approach remains a Snare due to unaddressed competence decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_for_judgment, empirical, 'Whether simulation can truly replicate the conditions for exercising judgment under stakes.').

omega_variable(
    internalized_sufficiency_belief,
    'Is the organizational belief in simulation''s sufficiency for all competence aspects a structural constraint (lack of viable alternatives) or an internalized cognitive pattern (failure to perceive the gap due to confirmation bias or institutional inertia)?',
    'Introducing external, independent audits that specifically test judgment-under-stakes, or providing alternative training methods that explicitly address this gap and observing organizational uptake.',
    'If internalized, the effective suppression is higher, as organizations resist acknowledging the problem even when alternatives are presented, making the constraint more resilient to external pressure for change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_sufficiency_belief, conceptual, 'Structural vs. internalized nature of the belief in simulation''s full efficacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(exer_tr_t5, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(exer_tr_t10, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(exer_tr_t15, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(exer_be_t5, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(exer_be_t10, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(exer_be_t15, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(exer_su_t5, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(exer_su_t10, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(exer_su_t15, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
