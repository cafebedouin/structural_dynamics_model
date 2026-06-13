% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__deterrence_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution Authority (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence reading' of state execution
 *   authority, where capital punishment is justified by its ability to
 *   prevent future murders by raising the cost of capital crimes. It is one
 *   reading of the broader 'state_execution_authority' kernel, alongside
 *   retributive and abolitionist readings. The core claim is instrumental:
 *   execution is a means to a safer society. The constraint operates as a
 *   Tangled Rope, as it purports to coordinate public safety while extracting
 *   the lives of offenders, with the effectiveness of the coordination
 *   (deterrence) being highly contested.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.45).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.7).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority (Deterrence Reading)").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '709336a9-2497-452e-b327-83892bab4697').
narrative_ontology:cs_kernel_codification('709336a9-2497-452e-b327-83892bab4697', formalized).
narrative_ontology:cs_authority_grounding('709336a9-2497-452e-b327-83892bab4697', lineage).
narrative_ontology:cs_interpretation_layer_present('709336a9-2497-452e-b327-83892bab4697').
narrative_ontology:cs_reading_relation('709336a9-2497-452e-b327-83892bab4697', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('709336a9-2497-452e-b327-83892bab4697', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_axiom('709336a9-2497-452e-b327-83892bab4697', foundational, execution_as_crime_deterrent).
narrative_ontology:cs_axiom_status(execution_as_crime_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('709336a9-2497-452e-b327-83892bab4697', execution_as_crime_deterrent, empirically_contingent).
narrative_ontology:cs_axiom('709336a9-2497-452e-b327-83892bab4697', secondary, state_has_right_to_protect_citizens_by_deterrence).
narrative_ontology:cs_axiom_status(state_has_right_to_protect_citizens_by_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('709336a9-2497-452e-b327-83892bab4697', state_has_right_to_protect_citizens_by_deterrence, instrumental).
narrative_ontology:cs_reference_frame('709336a9-2497-452e-b327-83892bab4697', effective_crime_deterrence_framework).
narrative_ontology:cs_drift_state('709336a9-2497-452e-b327-83892bab4697', contemporary_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('709336a9-2497-452e-b327-83892bab4697', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, potential_victims_of_capital_crimes).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, state_prosecutors).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, death_row_inmates).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, families_of_executed_offenders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and repeal capital punishment statutes, defining which crimes are eligible and the procedures for execution. Their decisions are influenced by public opinion, judicial rulings, and perceived deterrence effects.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Interprets capital punishment statutes and constitutional limits, setting procedural safeguards and reviewing death sentences. Their rulings shape the application of the deterrence principle.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Seek death sentences for capital crimes, arguing for their deterrent effect on potential future offenders. The availability of capital punishment strengthens their bargaining position in plea negotiations.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_prosecutors, beneficiary,
    organized, biographical, constrained, local).

% Are theoretically protected by the deterrent effect of capital punishment, as the 'cost' of committing a capital crime is raised. Their safety is the primary justification for the deterrence reading.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, potential_victims_of_capital_crimes, beneficiary,
    powerless, immediate, trapped, local).

% Bear the ultimate cost of the constraint, their lives taken as an instrumental means to deter others. Their individual guilt is established, but their execution is justified by its supposed societal benefit.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Live under the constant threat of execution, serving as the visible 'cost' that is meant to deter others. Their appeals process is a mechanism to minimize error, but not to question the deterrence principle itself.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, death_row_inmates, payer,
    powerless, biographical, trapped, local).

% Bear the emotional and social costs of the execution, often experiencing stigma and grief. They are not considered in the utilitarian calculus of deterrence.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, families_of_executed_offenders, payer,
    powerless, generational, trapped, local).

% Argue against capital punishment on moral and ethical grounds, often citing its irreversibility and the risk of wrongful execution. Their arguments are often framed as outside the deterrence calculus.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% Conduct empirical studies on the deterrent effect of capital punishment. Their findings often show no significant deterrent effect, challenging the foundational premise of this reading.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, criminologists_and_statisticians, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate societal behavior by establishing a clear, severe consequence for capital crimes, thereby deterring potential offenders and ensuring public safety.
% TRANSFER_FUNCTION: Transfers the lives of convicted capital offenders (and the associated costs of their execution) from the state to the societal benefit of reduced future murders.
% ABSENT_VOICES: Abolitionist advocates and the families of executed offenders are often marginalized in the public discourse that justifies capital punishment on deterrence grounds; they would argue that the moral costs outweigh any unproven benefits.
% DISAPPEARANCE_RATIONALE: If the authority to execute for deterrence vanished, the criminal justice system would need to re-evaluate sentencing for capital crimes, potentially shifting to life imprisonment without parole. Public safety debates would intensify, and the perceived 'cost' of murder would need to be re-established through other means.
% FOUNDING_PROBLEM: The problem of preventing heinous crimes and ensuring public safety by imposing the ultimate penalty to deter others.
% FOUNDING_PROBLEM_CORROBORATION: State prosecutors and some segments of the public attest the problem is live and execution is a necessary deterrent. Criminologists and abolitionist groups, citing decades of empirical research, corroborate that the deterrence effect is unproven or non-existent, suggesting the founding problem is either dead or better addressed by other means.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).
:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because the 'cost' (execution) is high, but its 'benefit' (deterrence) is empirically weak or non-existent, making the transfer inefficient. Suppression (0.70) is high due to the state's monopoly on legitimate force and the irreversible nature of execution. Theater ratio (0.60) is significant and rising, reflecting the increasing performative aspect of executions in the face of declining empirical support for deterrence and the decreasing number of actual executions. The accessibility collapse (0.30) is low because alternative punishments (life without parole) exist and are widely used, and resistance (0.80) is high due to ongoing legal and social challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state prosecutors and some segments of the public, this constraint is a necessary, albeit severe, tool for public safety. From the perspective of criminologists, it is an ineffective and costly policy. From the perspective of abolitionists and the condemned, it is an unjust act of state violence. The engine's classification will highlight the divergence between the claimed coordination function and the actual extractive operation.
 *
 * DIRECTIONALITY LOGIC:
 *   Potential victims are beneficiaries (d=0.0) as they are theoretically protected. State prosecutors also benefit (d=0.1) from the leverage capital punishment provides. Executed offenders and death row inmates are clear targets (d=1.0), bearing the ultimate cost. Families of executed offenders are also targets (d=0.9) due to the severe, uncompensated costs they bear. Criminologists are analytical observers (d=0.5), evaluating the constraint's efficacy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_ambiguity,
    'Does capital punishment genuinely deter future murders more effectively than life imprisonment without parole?',
    'Longitudinal, controlled empirical studies comparing murder rates in jurisdictions with and without capital punishment, controlling for socioeconomic factors and other criminal justice policies.',
    'If no significant deterrent effect is found, the primary justification for this reading collapses, reclassifying it closer to a Snare or Piton, as the coordination function (deterrence) is absent or purely theatrical. If a deterrent effect is proven, the extractiveness would be re-evaluated against the societal benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_ambiguity, empirical, 'Empirical evidence for the deterrent effect of capital punishment is highly contested and largely unproven.').

omega_variable(
    wrongful_execution_cost_ambiguity,
    'How should the cost of wrongful execution be factored into the deterrence calculus, and what is the true rate of such errors?',
    'Comprehensive review of all capital cases, including post-conviction DNA testing and re-examination of evidence, to establish a reliable wrongful conviction rate. Philosophical debate on the moral weight of an irreversible error in a utilitarian framework.',
    'A high rate of wrongful execution, combined with the irreversible nature of the penalty, would significantly increase the perceived extractiveness and undermine the legitimacy of the deterrence claim, pushing the classification towards Snare. It would also challenge the ''instrumental'' grounding of the deterrence axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_cost_ambiguity, conceptual, 'The utilitarian cost of wrongful execution is difficult to quantify and often excluded from deterrence arguments.').

omega_variable(
    substitutability_of_alternatives,
    'Is life imprisonment without parole an equally effective or superior deterrent to capital punishment?',
    'Comparative analysis of recidivism rates and crime trends in jurisdictions that have abolished capital punishment in favor of life without parole.',
    'If life without parole is found to be an equally effective deterrent, the ''necessity'' of capital punishment for deterrence collapses, making its continued use purely extractive and pushing the classification towards Snare or Piton. This would directly challenge the instrumental axiom of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitutability_of_alternatives, empirical, 'Whether alternative punishments achieve similar deterrence outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_execution_authority__deterrence_reading, theater_ratio, 1976, 0.3).
narrative_ontology:measurement(stat_tr_t1986, state_execution_authority__deterrence_reading, theater_ratio, 1986, 0.4).
narrative_ontology:measurement(stat_tr_t1996, state_execution_authority__deterrence_reading, theater_ratio, 1996, 0.5).
narrative_ontology:measurement(stat_tr_t2006, state_execution_authority__deterrence_reading, theater_ratio, 2006, 0.55).
narrative_ontology:measurement(stat_tr_t2016, state_execution_authority__deterrence_reading, theater_ratio, 2016, 0.58).
narrative_ontology:measurement(stat_tr_t2024, state_execution_authority__deterrence_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_execution_authority__deterrence_reading, base_extractiveness, 1976, 0.55).
narrative_ontology:measurement(stat_be_t1986, state_execution_authority__deterrence_reading, base_extractiveness, 1986, 0.5).
narrative_ontology:measurement(stat_be_t1996, state_execution_authority__deterrence_reading, base_extractiveness, 1996, 0.48).
narrative_ontology:measurement(stat_be_t2006, state_execution_authority__deterrence_reading, base_extractiveness, 2006, 0.46).
narrative_ontology:measurement(stat_be_t2016, state_execution_authority__deterrence_reading, base_extractiveness, 2016, 0.45).
narrative_ontology:measurement(stat_be_t2024, state_execution_authority__deterrence_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_execution_authority__deterrence_reading, suppression_requirement, 1976, 0.8).
narrative_ontology:measurement(stat_su_t1986, state_execution_authority__deterrence_reading, suppression_requirement, 1986, 0.78).
narrative_ontology:measurement(stat_su_t1996, state_execution_authority__deterrence_reading, suppression_requirement, 1996, 0.75).
narrative_ontology:measurement(stat_su_t2006, state_execution_authority__deterrence_reading, suppression_requirement, 2006, 0.72).
narrative_ontology:measurement(stat_su_t2016, state_execution_authority__deterrence_reading, suppression_requirement, 2016, 0.7).
narrative_ontology:measurement(stat_su_t2024, state_execution_authority__deterrence_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_execution_authority' kernel. Its core claim is instrumental deterrence, distinct from the retributive (moral balance) and abolitionist (categorical impermissibility) readings. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
