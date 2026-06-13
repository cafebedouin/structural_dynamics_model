% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__retributive_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority (Retributive Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the retributive reading of state execution
 *   authority, where capital punishment is justified as a means to restore
 *   moral balance by imposing proportionate punishment for heinous crimes. It
 *   is one reading of the broader 'state_execution_authority' kernel. From
 *   this perspective, the execution of an offender is a legitimate cost, and
 *   the moral satisfaction of victims' families is a key benefit. The
 *   constraint operates with high extractiveness and suppression, as it
 *   involves the ultimate deprivation of life and requires significant state
 *   power to enforce.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.85).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.9).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, snare).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority (Retributive Reading)").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, 'd0389b26-0fde-4a6c-9506-b7a85e70042d').
narrative_ontology:cs_kernel_codification('d0389b26-0fde-4a6c-9506-b7a85e70042d', formalized).
narrative_ontology:cs_authority_grounding('d0389b26-0fde-4a6c-9506-b7a85e70042d', lineage).
narrative_ontology:cs_interpretation_layer_present('d0389b26-0fde-4a6c-9506-b7a85e70042d').
narrative_ontology:cs_reading_relation('d0389b26-0fde-4a6c-9506-b7a85e70042d', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0389b26-0fde-4a6c-9506-b7a85e70042d', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_axiom('d0389b26-0fde-4a6c-9506-b7a85e70042d', foundational, punishment_must_be_proportionate_to_crime).
narrative_ontology:cs_axiom_status(punishment_must_be_proportionate_to_crime, holdable).
narrative_ontology:cs_axiom_grounding('d0389b26-0fde-4a6c-9506-b7a85e70042d', punishment_must_be_proportionate_to_crime, deontological).
narrative_ontology:cs_axiom('d0389b26-0fde-4a6c-9506-b7a85e70042d', foundational, execution_restores_moral_order).
narrative_ontology:cs_axiom_status(execution_restores_moral_order, holdable).
narrative_ontology:cs_axiom_grounding('d0389b26-0fde-4a6c-9506-b7a85e70042d', execution_restores_moral_order, deontological).
narrative_ontology:cs_reference_frame('d0389b26-0fde-4a6c-9506-b7a85e70042d', classical_retributive_justice).
narrative_ontology:cs_drift_state('d0389b26-0fde-4a6c-9506-b7a85e70042d', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d0389b26-0fde-4a6c-9506-b7a85e70042d', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, state_judicial_system).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, death_row_inmates).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, lex_talionis_principle).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, moral_order_restoration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal framework for capital punishment, including trials, appeals, and execution protocols. Benefits from upholding the perceived moral authority of the state and the justice system.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Seek closure and a sense of justice through the execution of offenders. Their moral satisfaction is a primary justification for the retributive reading of capital punishment.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families, beneficiary,
    organized, biographical, constrained, local).

% Bear the ultimate cost of the constraint, losing their lives. From the retributive perspective, this is a proportionate and legitimate cost for heinous crimes.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Live under the constant threat of execution, enduring psychological and physical suffering. They are the direct targets of the state's retributive power.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, death_row_inmates, payer,
    powerless, immediate, trapped, local).

% Argue against capital punishment on moral and ethical grounds, regardless of its retributive or deterrent effects. Their arguments are often dismissed by proponents of the retributive reading as irrelevant to the moral balance.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, global).

% Monitor and critique the use of capital punishment, often advocating for its abolition. Their observations provide an external perspective on the constraint's operation and legitimacy.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's response to heinous crimes by providing a mechanism for society to express moral outrage and restore a perceived moral equilibrium, ensuring that punishment 'fits' the crime.
% TRANSFER_FUNCTION: Transfers the life of the offender from the individual to the state, in exchange for the perceived restoration of moral order and justice for victims' families and society.
% ABSENT_VOICES: The executed offenders themselves, and those who would argue for their inherent right to life regardless of their crimes, are silenced by the execution process. Abolitionist advocates are often marginalized in the discourse surrounding retribution.
% DISAPPEARANCE_RATIONALE: If state execution authority vanished overnight, the criminal justice system would need to fundamentally re-evaluate its sentencing for heinous crimes, potentially leading to widespread public dissatisfaction among victims' families and a perceived breakdown of moral order. The state's punitive power would be significantly curtailed.
% FOUNDING_PROBLEM: The problem of how to justly and proportionately punish individuals who commit crimes so heinous that they are perceived to disrupt the fundamental moral order of society, and how to provide ultimate justice for victims.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of capital punishment, including many victims' families and some legal scholars, attest that the problem of moral imbalance after heinous crimes remains live. Opponents, including abolitionist groups and human rights organizations, contest the premise that execution restores moral balance, but acknowledge the societal demand for ultimate justice persists.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint demands the ultimate cost (life) from the offender, which is considered a necessary and proportionate transfer for moral balance. Suppression is also high (0.90) due to the state's monopoly on legitimate force and the legal mechanisms that prevent escape or refusal of punishment. Theater ratio is low (0.20) because the primary function of moral restoration is genuinely pursued, though the process involves significant ritual and performance. Resistance is high (0.70) due to ongoing legal challenges and abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and victims' families, this is a legitimate and necessary exercise of justice. From the perspective of the condemned and abolitionists, it is an unjust and cruel act. The engine's classification will highlight this divergence by showing high extraction for the targets, even if the claimed type is 'snare' (which acknowledges the extractive nature).
 *
 * DIRECTIONALITY LOGIC:
 *   The state judicial system and victims' families are beneficiaries, as they achieve their goals of moral order and justice. Executed offenders and death row inmates are the clear targets, bearing the full cost. Abolitionist advocates are excluded, as their arguments are outside the retributive framework's core justification. International human rights bodies act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_balance_objectivity,
    'Is ''moral balance'' an objective state that can be restored by state action, or a subjective perception?',
    'Philosophical consensus on meta-ethics, or empirical study of societal perceptions of justice post-execution vs. post-life-imprisonment.',
    'If subjective, the ''restoration'' claim is a cover for vengeance, increasing effective extractiveness; if objective, the claim holds, justifying the constraint''s severity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_balance_objectivity, conceptual, 'Objectivity of moral balance as a justification for execution.').

omega_variable(
    wrongful_execution_impact,
    'Does the possibility of wrongful execution fundamentally undermine the retributive framework, or is it a tragic but acceptable error?',
    'Legal and philosophical rulings on the ''irreversibility principle'' and its implications for justice systems.',
    'If it fundamentally undermines the framework, the constraint''s legitimacy collapses, and its classification shifts towards pure snare; if an acceptable error, the framework persists, albeit with acknowledged flaws.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_impact, conceptual, 'Impact of wrongful execution on retributive justice.').

omega_variable(
    proportionality_measurement,
    'How is ''proportionate punishment'' objectively measured for heinous crimes, and is execution the only or best means to achieve it?',
    'Development of a universally accepted scale for crime severity vs. punishment, or comparative analysis of alternative punishments (e.g., life without parole) in achieving perceived proportionality.',
    'If execution is not uniquely proportionate, its necessity is challenged, reducing its claimed coordination function and increasing its perceived extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement, empirical, 'Measurement of proportionality in capital punishment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_execution_authority__retributive_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(stat_tr_t1990, state_execution_authority__retributive_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(stat_tr_t2000, state_execution_authority__retributive_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(stat_tr_t2010, state_execution_authority__retributive_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(stat_tr_t2024, state_execution_authority__retributive_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_execution_authority__retributive_reading, base_extractiveness, 1976, 0.8).
narrative_ontology:measurement(stat_be_t1990, state_execution_authority__retributive_reading, base_extractiveness, 1990, 0.88).
narrative_ontology:measurement(stat_be_t2000, state_execution_authority__retributive_reading, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement(stat_be_t2010, state_execution_authority__retributive_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(stat_be_t2024, state_execution_authority__retributive_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_execution_authority__retributive_reading, suppression_requirement, 1976, 0.85).
narrative_ontology:measurement(stat_su_t1990, state_execution_authority__retributive_reading, suppression_requirement, 1990, 0.92).
narrative_ontology:measurement(stat_su_t2000, state_execution_authority__retributive_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(stat_su_t2010, state_execution_authority__retributive_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(stat_su_t2024, state_execution_authority__retributive_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is the retributive reading of state execution authority. It is linked to the deterrence and abolition readings as part of a constraint family, reflecting different justifications and critiques of capital punishment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
