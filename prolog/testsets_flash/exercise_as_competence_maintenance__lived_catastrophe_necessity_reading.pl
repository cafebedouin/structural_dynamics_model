% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
 *   human_readable: Lived Catastrophe Necessity for Competence Maintenance
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This constraint embodies the belief that true operational competence,
 *   especially in high-stakes environments, can only be forged and maintained
 *   through actual, lived catastrophe. Simulation is seen as a necessary but
 *   ultimately insufficient rehearsal, not the 'thing itself.' Without
 *   real-stakes activation, competence is assumed to atrophy covertly,
 *   leading to a victim set that includes all those exposed to operators
 *   whose skills have not been tested under genuine catastrophic conditions.
 *   This reading often leads to a reluctance to invest in high-fidelity
 *   simulation or to acknowledge its efficacy, as it is deemed 'not real.'
 *
 * KEY AGENTS:
 *   - operators_claiming_competence: Primary beneficiary (institutional/constrained) — benefits from lower training costs and the 'untestable' nature of their competence.
 *   - institutions_avoiding_simulation_costs: Primary beneficiary (institutional/arbitrage) — benefits from reduced investment in expensive, high-fidelity simulation infrastructure.
 *   - public_exposed_to_untested_competence: Primary victim (powerless/trapped) — bears the risk of operators whose competence has not been fully 'exercised' by real events.
 *   - safety_regulators: Agenda setter/Observer (institutional/analytical) — tasked with ensuring safety but constrained by the difficulty of proving 'unexercised' competence decay.
 *   - simulation_engineers: Excluded (organized/constrained) — their proposed solutions are deemed insufficient by this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.6).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.7).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, snare).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Lived Catastrophe Necessity for Competence Maintenance").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '0f101643-9608-4b77-8589-30e4b14bf080').
narrative_ontology:cs_kernel_codification('0f101643-9608-4b77-8589-30e4b14bf080', implicit).
narrative_ontology:cs_authority_grounding('0f101643-9608-4b77-8589-30e4b14bf080', practice).
narrative_ontology:cs_interpretation_layer_present('0f101643-9608-4b77-8589-30e4b14bf080').
narrative_ontology:cs_reading_relation('0f101643-9608-4b77-8589-30e4b14bf080', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('0f101643-9608-4b77-8589-30e4b14bf080', exercise_as_competence_maintenance__hybrid_decay_reading, coexists_with).
narrative_ontology:cs_axiom('0f101643-9608-4b77-8589-30e4b14bf080', foundational, real_stakes_are_irreducible).
narrative_ontology:cs_axiom_status(real_stakes_are_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('0f101643-9608-4b77-8589-30e4b14bf080', real_stakes_are_irreducible, deontological).
narrative_ontology:cs_axiom('0f101643-9608-4b77-8589-30e4b14bf080', foundational, competence_decays_covertly_without_activation).
narrative_ontology:cs_axiom_status(competence_decays_covertly_without_activation, holdable).
narrative_ontology:cs_axiom_grounding('0f101643-9608-4b77-8589-30e4b14bf080', competence_decays_covertly_without_activation, empirically_contingent).
narrative_ontology:cs_reference_frame('0f101643-9608-4b77-8589-30e4b14bf080', catastrophe_as_ultimate_test).
narrative_ontology:cs_drift_state('0f101643-9608-4b77-8589-30e4b14bf080', contemporary_safety_science_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0f101643-9608-4b77-8589-30e4b14bf080', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, operators_claiming_competence).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, institutions_avoiding_simulation_costs).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, public_exposed_to_untested_competence).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, junior_operators_lacking_real_stakes_experience).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the understanding of how operational competence is truly maintained in high-stakes environments, ensuring that operators are genuinely prepared for rare, critical events.
% TRANSFER_FUNCTION: Transfers the burden of 'proving' competence from proactive investment in high-fidelity simulation and training to the reactive experience of actual catastrophe. It transfers risk from institutions to the public.
% ABSENT_VOICES: Simulation engineers and proponents of advanced training methodologies are often marginalized, as their solutions are deemed 'not real' enough. The public, who bears the ultimate risk, is also an absent voice, as they are not typically involved in defining competence standards.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the discourse around competence maintenance would shift dramatically. There would be increased pressure to invest in and validate high-fidelity simulations, and a greater focus on proactive risk mitigation rather than relying on 'baptism by fire.' Resource allocation for training and preparedness would fundamentally reorganize.
% FOUNDING_PROBLEM: The problem of ensuring genuine operational competence for rare, high-consequence events, where traditional training methods might not fully prepare individuals for the psychological and cognitive demands of real crisis.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by historical accounts of catastrophic failures where operators were deemed 'unprepared' despite extensive training. However, the 'necessity of lived catastrophe' as the *only* solution is contested by safety science researchers and simulation experts, who provide evidence for the efficacy of advanced simulation. The corroboration for the *problem* is strong, but for *this reading's solution* is weak outside of the benefiting parties.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) stems from the implicit cost borne by the public due to the acceptance of unexercised competence, and the foregone benefits of more effective training. Suppression (0.7) is high because this reading actively suppresses alternative views on competence maintenance (e.g., simulation sufficiency) by framing them as 'unreal' or 'insufficient.' The theater ratio (0.4) reflects that while some training occurs, a significant portion of 'competence maintenance' is performative, relying on past events or theoretical knowledge rather than active, real-stakes exercise. Accessibility collapse (0.6) is moderate, as alternatives (high-fidelity simulation) exist but are dismissed or underfunded. Resistance (0.3) is low because the 'untestable' nature of competence decay makes it hard to challenge directly until a catastrophe occurs.
 *
 * PERSPECTIVAL GAP:
 *   Operators and institutions adhering to this reading perceive it as a 'mountain' of human nature or an 'unavoidable truth' about crisis performance, justifying lower investment in simulation. The public, however, experiences it as a 'snare' where their safety is compromised by an untested system. Safety regulators are caught between these perspectives, struggling to enforce a standard for competence that this reading claims can only be proven by disaster.
 *
 * DIRECTIONALITY LOGIC:
 *   Operators claiming competence and institutions avoiding simulation costs are beneficiaries (d near 0.0-0.2) as they benefit from lower training overhead and the deferral of accountability. The public exposed to untested competence and junior operators lacking real-stakes experience are victims (d near 0.8-1.0) as they bear the direct and indirect costs of this approach. Safety regulators are agenda-setters (d near 0.5) as they administer the system but are also constrained by its underlying assumptions. Simulation engineers are excluded (d near 1.0) as their solutions are actively suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling a genuine coordination problem (how to maintain competence) as pure extraction. However, by asserting the necessity of lived catastrophe, it risks becoming a snare that extracts safety from the public by rationalizing insufficient preparedness. The classification as a snare, despite the 'competence maintenance' framing, highlights the asymmetric cost-bearing and the suppression of alternatives. If the 'founding problem' of competence decay is still live, but the 'solution' (waiting for catastrophe) is demonstrably harmful, it points to a mandatrophy where the original mandate has been corrupted into an extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of competence dynamics, or a rationalization for avoiding costly, high-fidelity simulation?',
    'Empirical studies on operator performance in real crises vs. high-fidelity simulations, controlling for training hours and scenario complexity.',
    'If a rationalization, the constraint is more extractive (snare) than its current classification suggests, as it actively suppresses effective training alternatives. If genuine, it''s a mountain or tangled rope reflecting an irreducible aspect of human performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''lived_catastrophe_necessity_reading'' of the ''exercise_as_competence_maintenance'' kernel. It asserts that only real-stakes catastrophe exercises true competence, implying that simulation is insufficient. Sibling readings (''simulation_sufficiency_reading'', ''hybrid_decay_reading'') offer alternative views on competence maintenance.').

omega_variable(
    covert_competence_decay_measurement,
    'How can the covert decay of competence, posited by this reading, be empirically measured without a real catastrophe?',
    'Development of advanced psychometric and neurocognitive assessments that can detect subtle degradation in decision-making under simulated stress, or analysis of near-miss incidents for early warning signs.',
    'If measurable, the ''covert decay'' becomes an empirically contingent claim, potentially shifting the constraint''s grounding type. If unmeasurable, it remains a deontological or conventional axiom, making the constraint less susceptible to empirical challenge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covert_competence_decay_measurement, empirical, 'This reading posits competence atrophies covertly without real-stakes activation. The unobservability of this decay is a key feature, making it difficult to challenge without a catastrophe.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(exer_tr_t5, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(exer_tr_t10, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(exer_tr_t15, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(exer_be_t5, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(exer_be_t10, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(exer_be_t15, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(exer_su_t5, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(exer_su_t10, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(exer_su_t15, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, identity_coordination).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'exercise_as_competence_maintenance' kernel. It asserts that only actual catastrophe exercises the competence kernel, and simulation is insufficient. Sibling readings ('simulation_sufficiency_reading', 'hybrid_decay_reading') offer alternative views on how competence is maintained.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
