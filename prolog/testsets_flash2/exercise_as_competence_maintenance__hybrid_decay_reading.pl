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
 *   human_readable: Hybrid Decay Reading: Simulation for Procedural Competence, Lived Experience for Judgment
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid decay' reading of competence
 *   maintenance, where simulation effectively exercises procedural competence
 *   (muscle memory, checklists) but fails to exercise judgment and
 *   improvisation under genuine stakes. The kernel has two components: one
 *   that can be simulated, and one that decays without lived experience. The
 *   constraint is claimed as a Rope by its proponents (effective coordination
 *   of training) but operates as a Tangled Rope due to the asymmetric
 *   extraction of risk from frontline operators and the public, who bear the
 *   cost of unexercised judgment. The metrics reflect this hybrid nature,
 *   with moderate extraction and suppression, and a notable theater ratio due
 *   to the performative aspect of 'full' preparedness.
 *
 * KEY AGENTS:
 *   - organizational_leadership: Agenda setter (institutional/constrained) — prioritizes simulation for cost/safety.
 *   - training_providers: Beneficiary (organized/mobile) — profits from simulation contracts.
 *   - frontline_operators: Payer (moderate/constrained) — maintains procedural skills, bears judgment decay risk.
 *   - affected_public: Payer (powerless/trapped) — bears consequences of judgment failures.
 *   - safety_regulators: Observer (institutional/analytical) — assesses compliance, may miss judgment gap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.6).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.7).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Hybrid Decay Reading: Simulation for Procedural Competence, Lived Experience for Judgment").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, '90bd5dac-6343-4b73-a1b4-1d942288c66d').
narrative_ontology:cs_kernel_codification('90bd5dac-6343-4b73-a1b4-1d942288c66d', formalized).
narrative_ontology:cs_authority_grounding('90bd5dac-6343-4b73-a1b4-1d942288c66d', lineage).
narrative_ontology:cs_interpretation_layer_present('90bd5dac-6343-4b73-a1b4-1d942288c66d').
narrative_ontology:cs_reading_relation('90bd5dac-6343-4b73-a1b4-1d942288c66d', exercise_as_competence_maintenance__simulation_sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('90bd5dac-6343-4b73-a1b4-1d942288c66d', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('90bd5dac-6343-4b73-a1b4-1d942288c66d', foundational, competence_is_multicomponent).
narrative_ontology:cs_axiom_status(competence_is_multicomponent, holdable).
narrative_ontology:cs_axiom_grounding('90bd5dac-6343-4b73-a1b4-1d942288c66d', competence_is_multicomponent, empirically_contingent).
narrative_ontology:cs_axiom('90bd5dac-6343-4b73-a1b4-1d942288c66d', foundational, simulation_exercises_only_procedural_component).
narrative_ontology:cs_axiom_status(simulation_exercises_only_procedural_component, holdable).
narrative_ontology:cs_axiom_grounding('90bd5dac-6343-4b73-a1b4-1d942288c66d', simulation_exercises_only_procedural_component, empirically_contingent).
narrative_ontology:cs_reference_frame('90bd5dac-6343-4b73-a1b4-1d942288c66d', dual_component_competence_framework).
narrative_ontology:cs_drift_state('90bd5dac-6343-4b73-a1b4-1d942288c66d', contemporary_training_practices, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('90bd5dac-6343-4b73-a1b4-1d942288c66d', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_leadership).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, training_providers).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, affected_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets policies for competence maintenance, often prioritizing simulation-based training due to cost and safety. Benefits from the appearance of preparedness and compliance with regulatory standards, while bearing the long-term risk of unexercised judgment.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Develop and deliver simulation exercises, profiting from contracts to maintain procedural competence. Their business model is tied to the acceptance of simulation as a primary exercise method.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, training_providers, beneficiary,
    organized, biographical, mobile, regional).

% Participate in simulations, maintaining their procedural skills and muscle memory. They bear the cost of unexercised judgment and improvisation capacity, which can lead to personal and professional consequences during real crises.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators, payer,
    moderate, biographical, constrained, local).

% Are the ultimate victims of failures in judgment and improvisation during crises, as they bear the direct consequences of inadequate preparedness that simulation alone cannot address.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, affected_public, payer,
    powerless, immediate, trapped, local).

% Oversee competence maintenance programs, often relying on metrics derived from simulation performance. They are tasked with ensuring public safety but may lack the data to fully assess the gap between simulated and real-world judgment.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, safety_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the maintenance of operational competence within high-stakes organizations by providing structured, repeatable training environments for procedural skills.
% TRANSFER_FUNCTION: Transfers resources (funding, time) from organizational budgets to training providers, and transfers a sense of preparedness and compliance to leadership and regulators, while transferring the risk of unexercised judgment to frontline operators and the public.
% ABSENT_VOICES: Those who have experienced actual catastrophes and understand the unique demands of judgment under extreme stakes are often absent from the design of competence maintenance programs, leading to an over-reliance on simulation.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, organizations would immediately face a crisis in how to maintain and certify operational competence. The current system, despite its flaws, provides a structured approach that would need to be replaced, leading to a significant reorganization of training and safety protocols.
% FOUNDING_PROBLEM: The need to maintain complex operational skills and ensure readiness for rare, high-consequence events without exposing personnel to actual danger during training.
% FOUNDING_PROBLEM_CORROBORATION: Organizational leadership and training providers attest that the problem is live, citing the ongoing need for skill maintenance and safe training environments. Frontline operators and safety regulators acknowledge the problem but contest the sufficiency of the current solution, pointing to real-world incidents where judgment failed despite simulation training.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.6) arises from the transfer of risk and the cost of unexercised judgment to operators and the public, while leadership and training providers benefit. Suppression (0.7) is due to the institutional inertia and regulatory frameworks that favor simulation, limiting alternatives for comprehensive training. The theater ratio (0.4) reflects the performative aspect of 'full preparedness' through simulation, which masks the decay in judgment capacity. The slight dip in extractiveness and suppression at the end of the interval could reflect increased awareness or minor reforms, but the core hybrid decay dynamic persists.
 *
 * PERSPECTIVAL GAP:
 *   Organizational leadership and training providers perceive this as a functional Rope, effectively coordinating training and ensuring safety. Frontline operators and the affected public, however, experience it as a Tangled Rope, where the coordination function (procedural training) is intertwined with an extractive element (unexercised judgment capacity leading to risk transfer). The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Organizational leadership and training providers are beneficiaries (low d) as they gain from efficient, safe training and perceived compliance. Frontline operators and the affected public are targets (high d) as they bear the unmitigated risks of judgment decay. Safety regulators are observers (analytical d) who aim to ensure safety but may be constrained by the available metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (as claimed by some beneficiaries) by highlighting the asymmetric extraction of risk. It also avoids mislabeling it as a pure Snare by acknowledging the genuine coordination function of procedural skill maintenance. The 'hybrid decay' reading emphasizes that the mandate for competence maintenance is partially fulfilled, but a critical component (judgment under stakes) is atrophying, leading to a Mandatrophy-like state in that specific dimension, even if the overall founding problem is still 'live'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judgment_decay_measurement,
    'How can judgment and improvisation capacity under genuine stakes be reliably measured and tracked, independent of procedural competence?',
    'Development of novel assessment methodologies (e.g., high-fidelity, high-stakes virtual reality scenarios with physiological stress indicators, or post-incident analysis focused on decision-making under uncertainty) that isolate and quantify judgment.',
    'If measurable, the decay in judgment could be directly integrated into competence metrics, potentially reclassifying the constraint towards a Snare if the gap is severe and unaddressed, or a Scaffold if a clear path to remediation is identified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judgment_decay_measurement, empirical, 'The empirical challenge of measuring the non-simulable component of competence.').

omega_variable(
    simulation_fidelity_threshold,
    'At what level of simulation fidelity and stress induction does simulated exercise begin to genuinely engage and maintain judgment under stakes?',
    'Controlled experimental studies comparing performance in high-fidelity simulations with performance in real-world, high-stakes scenarios, focusing on decision-making quality and adaptive behavior rather than procedural adherence.',
    'If a threshold exists, it could inform new regulatory standards for ''judgment-exercising'' simulations, potentially shifting the constraint towards a more effective Rope or Scaffold. If no such threshold is found, it reinforces the ''hybrid decay'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'The empirical limits of simulation for exercising judgment.').

omega_variable(
    kernel_reading_difference,
    'This constraint is one reading of the ''exercise_as_competence_maintenance'' kernel. What would change if a sibling reading (e.g., ''lived_catastrophe_necessity_reading'') were adopted?',
    'Analysis of policy changes, resource allocation shifts, and regulatory enforcement patterns in organizations that explicitly adopt the ''lived catastrophe'' premise (e.g., increased emphasis on real-world deployments, live exercises, or acceptance of higher training risks).',
    'Adopting the ''lived catastrophe'' reading would likely increase perceived extractiveness and suppression for organizational leadership (higher costs, greater risks) but decrease it for frontline operators (more complete competence maintenance). The constraint''s classification would likely shift towards a Snare for leadership, as they would be forced to bear more direct costs and risks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference, conceptual, 'Impact of adopting a different reading of the competence maintenance kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(exer_tr_t5, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(exer_tr_t10, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(exer_tr_t15, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(exer_be_t5, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(exer_be_t10, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(exer_be_t15, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(exer_su_t5, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(exer_su_t10, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(exer_su_t15, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 20, 0.7).


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
