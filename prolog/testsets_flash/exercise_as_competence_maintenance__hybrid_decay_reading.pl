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
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Hybrid Decay Reading: Simulation for Procedural Competence, Decay for Judgment Under Stakes
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This constraint describes the 'hybrid decay' reading of competence
 *   maintenance, where simulation exercises procedural competence (e.g.,
 *   muscle memory, checklist adherence) but fails to exercise or maintain
 *   judgment-under-stakes and improvisational capacity. The kernel of
 *   'competence' is thus split into two components with different exercise
 *   requirements and decay rates. The constraint is framed as a Tangled Rope
 *   because it provides a genuine coordination function (procedural training)
 *   but extracts from those who rely on it for full competence by allowing
 *   the judgment component to decay, leading to potential failures in real
 *   crises. The victim set includes those harmed by these failures.
 *
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
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Hybrid Decay Reading: Simulation for Procedural Competence, Decay for Judgment Under Stakes").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, 'cd47f665-aa52-4cd7-a5c3-17d2e7432364').
narrative_ontology:cs_kernel_codification('cd47f665-aa52-4cd7-a5c3-17d2e7432364', implicit).
narrative_ontology:cs_authority_grounding('cd47f665-aa52-4cd7-a5c3-17d2e7432364', practice).
narrative_ontology:cs_interpretation_layer_present('cd47f665-aa52-4cd7-a5c3-17d2e7432364').
narrative_ontology:cs_reading_relation('cd47f665-aa52-4cd7-a5c3-17d2e7432364', exercise_as_competence_maintenance__simulation_sufficiency_reading, influences).
narrative_ontology:cs_reading_relation('cd47f665-aa52-4cd7-a5c3-17d2e7432364', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('cd47f665-aa52-4cd7-a5c3-17d2e7432364', foundational, competence_is_multicomponent).
narrative_ontology:cs_axiom_status(competence_is_multicomponent, holdable).
narrative_ontology:cs_axiom_grounding('cd47f665-aa52-4cd7-a5c3-17d2e7432364', competence_is_multicomponent, empirically_contingent).
narrative_ontology:cs_axiom('cd47f665-aa52-4cd7-a5c3-17d2e7432364', foundational, simulation_exercises_only_procedural_component).
narrative_ontology:cs_axiom_status(simulation_exercises_only_procedural_component, holdable).
narrative_ontology:cs_axiom_grounding('cd47f665-aa52-4cd7-a5c3-17d2e7432364', simulation_exercises_only_procedural_component, empirically_contingent).
narrative_ontology:cs_reference_frame('cd47f665-aa52-4cd7-a5c3-17d2e7432364', dual_component_competence_model).
narrative_ontology:cs_drift_state('cd47f665-aa52-4cd7-a5c3-17d2e7432364', contemporary_organizational_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cd47f665-aa52-4cd7-a5c3-17d2e7432364', '').
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

% Sets training policies and budgets, relying on simulation to demonstrate compliance and readiness. Benefits from perceived competence without bearing the full cost of judgment decay in crises.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Participate in simulations, maintaining procedural skills. However, their judgment-under-stakes and improvisational capacity decay due to lack of real-stakes exercise, leaving them vulnerable in actual crises. Their professional identity is tied to organizational readiness.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators, payer,
    moderate, biographical, identity_locked, local).

% Develop and deliver simulation programs, generating revenue and expertise. Their business model is supported by the current understanding of simulation's role in competence maintenance.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, training_providers, beneficiary,
    organized, biographical, mobile, national).

% Oversee compliance with training requirements, often accepting simulation as sufficient. They have the power to mandate changes but may lack the deep understanding of competence decay to do so proactively.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, regulators, observer,
    institutional, generational, analytical, national).

% Are the ultimate victims of organizational failures in crises, which can be exacerbated by decayed judgment-under-stakes among operators. They have no direct control over training regimes.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, affected_public, payer,
    powerless, generational, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_leadership).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__hybrid_decay_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the maintenance of procedural competence across an organization through standardized simulation exercises, ensuring a baseline level of operational readiness.
% TRANSFER_FUNCTION: Transfers resources (time, budget, personnel) from operational departments to training programs, and transfers the risk of unexercised judgment from organizational leadership to frontline operators and the affected public.
% ABSENT_VOICES: Future victims of crises (the affected public) are absent from the design of training regimes, as are those who would advocate for more costly, high-fidelity, or real-stakes training to maintain judgment capacity.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, organizations would likely cease or drastically reduce simulation exercises, leading to a rapid decay in both procedural competence and judgment-under-stakes, increasing the likelihood and severity of failures in real crises. The entire safety and preparedness ecosystem would need to be rethought.
% FOUNDING_PROBLEM: The problem of maintaining complex operational competence in high-stakes environments where real-world incidents are rare but catastrophic, requiring a method to practice skills without incurring real-world risks.
% FOUNDING_PROBLEM_CORROBORATION: Organizational leadership and training providers attest the problem is live, citing the ongoing need for skill maintenance. Frontline operators and safety researchers, while acknowledging the problem, corroborate that the current solution only partially addresses it, leaving the judgment component vulnerable.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).

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
 *   The extractiveness (0.6) stems from the gap between claimed competence (from simulation) and actual competence (lacking judgment-under-stakes). Organizations invest in simulations, believing they fully maintain readiness, but this reading suggests a critical component atrophies. Suppression (0.7) is high because the organizational culture often suppresses dissent about simulation efficacy, and frontline operators have limited options to demand more realistic or comprehensive training. The theater ratio (0.4) reflects that while simulations have real value, a significant portion of their perceived utility is performative, masking the unaddressed decay of judgment. Accessibility collapse is moderate (0.3) as some alternatives (e.g., more frequent real-world drills, higher-fidelity simulations for judgment) exist but are costly or resisted. Resistance is low (0.2) because the perceived benefits of simulation often outweigh the diffuse costs of unexercised judgment until a crisis hits.
 *
 * PERSPECTIVAL GAP:
 *   Organizational leadership perceives the constraint as a Rope, effectively maintaining competence through simulation. Frontline operators, however, experience it as a Tangled Rope or even a Snare, as they are coordinated into a training regime that leaves them vulnerable in high-stakes situations where judgment is paramount. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Organizational leadership and training providers are beneficiaries (d near 0.0) as they fulfill regulatory requirements, demonstrate 'readiness,' and generate revenue/budgets through simulation programs. Frontline operators are payers (d near 1.0) because they bear the risk of unexercised judgment in real crises, despite participating in simulations. The affected public are also payers/victims (d near 1.0) as they suffer the consequences of organizational failures stemming from decayed judgment. Regulators are observers (d near 0.5) as they oversee compliance but may not fully grasp the nuance of competence decay.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to ensure competence. This reading suggests a partial mandatrophy: the procedural competence mandate is met, but the judgment-under-stakes mandate is not. The classification as Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the unaddressed decay and victim set) or a pure Snare (which would ignore the genuine procedural coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''hybrid decay'' reading of competence maintenance, or is it better described by ''simulation sufficiency'' or ''lived catastrophe necessity''?',
    'Empirical studies on crisis response outcomes correlating with simulation frequency vs. real-world incident exposure, specifically measuring procedural vs. improvisational success.',
    'If ''simulation sufficiency'' is true, the constraint is less extractive than currently assessed; if ''lived catastrophe necessity'' is true, the constraint is more extractive and the current simulation regime is largely theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity in the true nature of competence maintenance exercise.').

omega_variable(
    procedural_vs_judgment_decay_rate,
    'What is the precise decay rate for procedural competence vs. judgment-under-stakes, and how does simulation impact each?',
    'Longitudinal studies tracking performance metrics in high-stakes environments, comparing teams with varying simulation exposure and real-world incident rates.',
    'More rapid decay of judgment-under-stakes would increase the effective extractiveness of the current regime, as it leaves a critical competence gap unaddressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_vs_judgment_decay_rate, empirical, 'Uncertainty in the differential decay rates of competence components.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(exer_tr_t10, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(exer_tr_t30, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(exer_tr_t40, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(exer_tr_t50, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(exer_be_t10, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(exer_be_t30, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(exer_be_t40, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(exer_be_t50, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 50, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(exer_su_t10, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(exer_su_t30, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(exer_su_t40, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(exer_su_t50, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'exercise_as_competence_maintenance' kernel. This 'hybrid decay' reading posits that simulation maintains procedural competence but not judgment-under-stakes, leading to a decay in the latter. The other readings offer different views on simulation's efficacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
