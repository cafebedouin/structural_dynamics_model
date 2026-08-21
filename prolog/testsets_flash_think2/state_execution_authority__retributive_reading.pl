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
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority (Retributive Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the retributive reading of state execution
 *   authority, where the primary justification is the restoration of moral
 *   balance through proportionate punishment for heinous crimes. It asserts
 *   that certain offenses demand the ultimate penalty to satisfy justice and
 *   provide closure for victims' families and society. The executed offender
 *   is considered to bear a legitimate cost within this framework, and the
 *   possibility of wrongful execution, while tragic, does not inherently
 *   invalidate the framework's core premise.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.85).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.95).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority (Retributive Reading)").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, 'c9f9cac5-0d47-4757-87c1-dfe2ab58e60c').
narrative_ontology:cs_kernel_codification('c9f9cac5-0d47-4757-87c1-dfe2ab58e60c', formalized).
narrative_ontology:cs_authority_grounding('c9f9cac5-0d47-4757-87c1-dfe2ab58e60c', lineage).
narrative_ontology:cs_interpretation_layer_present('c9f9cac5-0d47-4757-87c1-dfe2ab58e60c').
narrative_ontology:cs_reading_relation('c9f9cac5-0d47-4757-87c1-dfe2ab58e60c', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9f9cac5-0d47-4757-87c1-dfe2ab58e60c', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_axiom('c9f9cac5-0d47-4757-87c1-dfe2ab58e60c', foundational, punishment_must_be_proportionate).
narrative_ontology:cs_axiom_status(punishment_must_be_proportionate, holdable).
narrative_ontology:cs_axiom_grounding('c9f9cac5-0d47-4757-87c1-dfe2ab58e60c', punishment_must_be_proportionate, deontological).
narrative_ontology:cs_axiom('c9f9cac5-0d47-4757-87c1-dfe2ab58e60c', foundational, moral_desert_justifies_punishment).
narrative_ontology:cs_axiom_status(moral_desert_justifies_punishment, holdable).
narrative_ontology:cs_axiom_grounding('c9f9cac5-0d47-4757-87c1-dfe2ab58e60c', moral_desert_justifies_punishment, deontological).
narrative_ontology:cs_reference_frame('c9f9cac5-0d47-4757-87c1-dfe2ab58e60c', lex_talionis_justice_framework).
narrative_ontology:cs_drift_state('c9f9cac5-0d47-4757-87c1-dfe2ab58e60c', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c9f9cac5-0d47-4757-87c1-dfe2ab58e60c', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, society_at_large).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_offenders).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, lex_talionis_principle).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, moral_desert_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal framework for capital punishment, including trials, appeals, and execution protocols. Justifies its role as upholding justice and moral order through proportionate punishment.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_judicial_system, agenda_setter,
    institutional, civilizational, constrained, national).

% Receive a sense of moral balance and justice through the execution of offenders who committed heinous crimes against their loved ones. Their participation in the process often centers on seeking this form of retribution.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families, beneficiary,
    moderate, biographical, constrained, local).

% Bear the ultimate cost of the constraint, their life, as a proportionate punishment for their crimes. They are legally and physically trapped within the system once convicted and sentenced to death.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Benefits from the perceived restoration of moral order and the affirmation of fundamental societal values through the application of proportionate punishment for heinous crimes. This contributes to a sense of collective justice.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, society_at_large, beneficiary,
    organized, generational, constrained, national).

% Actively campaign against capital punishment, arguing it is morally wrong regardless of the crime. They are excluded from the retributive framework's internal logic, which views execution as a legitimate and necessary form of justice.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the legal, ethical, and philosophical underpinnings of capital punishment, including its retributive justifications. They provide critical commentary and research but do not directly participate in the enforcement or experience the direct costs/benefits.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__retributive_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restores societal moral order and provides a sense of justice and closure for victims' families by ensuring punishment is proportionate to the heinousness of the crime, thereby affirming fundamental values.
% TRANSFER_FUNCTION: Transfers the ultimate cost (life) from the state (acting on behalf of society and victims' families) to the offender, in exchange for the perceived restoration of moral balance and justice.
% ABSENT_VOICES: Abolitionist advocates, international human rights organizations, and the executed offenders themselves (whose voices are often legally silenced post-conviction) are excluded from the retributive framework's internal justification.
% DISAPPEARANCE_RATIONALE: If the authority to execute for retribution vanished overnight, the criminal justice system would need to fundamentally rethink its sentencing philosophy for heinous crimes, potentially leading to increased life sentences without parole, and a significant shift in how society perceives justice for such acts. The moral and emotional landscape for victims' families would also be profoundly altered.
% FOUNDING_PROBLEM: How to justly respond to crimes that violate fundamental moral order and inflict irreparable harm, ensuring that the punishment 'fits' the crime and restores societal equilibrium and a sense of justice for victims.
% FOUNDING_PROBLEM_CORROBORATION: Many victims' families, a segment of the public, and some legal and moral philosophers attest that the problem of proportionate punishment for heinous crimes remains live, and that execution is the only truly proportionate response. This is often contested by abolitionist groups and those who argue for rehabilitation or life imprisonment as sufficient, but within the retributive framework, the problem is considered unresolved by lesser penalties.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.85) because the constraint imposes the ultimate cost (life) on the offender. Suppression is also very high (0.95) due to the state's monopoly on legitimate force and the complete lack of exit options for the condemned. Theater ratio is low (0.1) because, within this reading, the act of execution is considered a direct, functional application of justice, not a performance. Accessibility collapse is high (0.9) as there are no alternatives for the offender once the legal process is exhausted. Resistance is moderate (0.4) reflecting ongoing, but not universally successful, challenges from abolitionist movements and legal appeals.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state, victims' families, and a segment of society, this constraint functions as a necessary mechanism for justice and moral restoration. From the perspective of the executed offender, it is the ultimate act of state extraction and suppression. Abolitionist advocates view it as an illegitimate exercise of state power, regardless of the crime.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims' families and society are declared beneficiaries, as they receive the perceived moral balance and justice. The executed offender is the clear victim/payer, bearing the ultimate cost. The state judicial system acts as the agenda-setter, enforcing the constraint. Abolitionist advocates are excluded, as their fundamental opposition places them outside the framework's internal logic.
 *
 * MANDATROPHY ANALYSIS:
 *   Within the retributive reading, the mandate for execution is tied to an enduring philosophical concept of justice and moral desert. As long as society perceives certain crimes as heinous and demanding ultimate retribution, the mandate remains 'live'. Mandatrophy is less likely to occur from within this reading, though external challenges from other readings (e.g., abolitionist arguments) constantly contest its legitimacy and application.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retribution_vs_cruelty,
    'Is execution, even for heinous crimes, truly proportionate punishment that restores moral balance, or does it constitute cruel and unusual punishment that violates fundamental human dignity?',
    'Ongoing philosophical debate, evolving international human rights norms, and judicial interpretations of constitutional prohibitions against cruel and unusual punishment.',
    'If deemed cruel and unusual, the retributive framework''s legitimacy would be fundamentally undermined, leading to its reclassification as a Snare; if affirmed as proportionate, its Tangled Rope classification would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retribution_vs_cruelty, conceptual, 'Ambiguity regarding the moral and legal status of execution as proportionate punishment.').

omega_variable(
    moral_balance_empirical_status,
    'Can ''moral balance'' be empirically restored or measured, or is its restoration a subjective perception for beneficiaries?',
    'Sociological and psychological studies on the impact of executions on victims'' families and societal perceptions of justice; philosophical analysis of the concept of moral equilibrium.',
    'If moral balance is purely subjective and unmeasurable, the coordination function claimed by this reading is weakened, potentially shifting it closer to a Snare; if it has demonstrable societal effects, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_balance_empirical_status, empirical, 'The empirical verifiability of the ''moral balance'' coordination function.').

omega_variable(
    wrongful_execution_invalidation,
    'Does the undeniable possibility of wrongful execution fundamentally invalidate the retributive framework, even if it is considered a tragic error?',
    'Legal reforms establishing higher evidentiary standards, post-conviction DNA testing, and re-evaluation of the finality of capital sentences in light of new evidence.',
    'If wrongful executions are deemed to fundamentally undermine the framework, the constraint''s legitimacy collapses, pushing it towards a Snare; if the framework can accommodate such errors without invalidation, its current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_invalidation, conceptual, 'Impact of wrongful executions on the retributive framework''s validity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_execution_authority__retributive_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(stat_tr_t1988, state_execution_authority__retributive_reading, theater_ratio, 1988, 0.08).
narrative_ontology:measurement(stat_tr_t2000, state_execution_authority__retributive_reading, theater_ratio, 2000, 0.07).
narrative_ontology:measurement(stat_tr_t2012, state_execution_authority__retributive_reading, theater_ratio, 2012, 0.09).
narrative_ontology:measurement(stat_tr_t2024, state_execution_authority__retributive_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_execution_authority__retributive_reading, base_extractiveness, 1976, 0.8).
narrative_ontology:measurement(stat_be_t1988, state_execution_authority__retributive_reading, base_extractiveness, 1988, 0.85).
narrative_ontology:measurement(stat_be_t2000, state_execution_authority__retributive_reading, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(stat_be_t2012, state_execution_authority__retributive_reading, base_extractiveness, 2012, 0.87).
narrative_ontology:measurement(stat_be_t2024, state_execution_authority__retributive_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_execution_authority__retributive_reading, suppression_requirement, 1976, 0.9).
narrative_ontology:measurement(stat_su_t1988, state_execution_authority__retributive_reading, suppression_requirement, 1988, 0.93).
narrative_ontology:measurement(stat_su_t2000, state_execution_authority__retributive_reading, suppression_requirement, 2000, 0.96).
narrative_ontology:measurement(stat_su_t2012, state_execution_authority__retributive_reading, suppression_requirement, 2012, 0.95).
narrative_ontology:measurement(stat_su_t2024, state_execution_authority__retributive_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'state_execution_authority' kernel, each representing a distinct justification or opposition to capital punishment. This retributive reading focuses on proportionate punishment and moral balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
