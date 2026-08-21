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
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority (Retributive Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the retributive reading of state execution
 *   authority, where the state's power to execute is justified by the need to
 *   restore moral balance through proportionate punishment for heinous
 *   crimes. This reading views the executed offender as a legitimate cost of
 *   justice, and the victims' families as primary beneficiaries who receive
 *   moral satisfaction. The possibility of wrongful execution is acknowledged
 *   as a tragic error but does not, within this framework, invalidate the
 *   underlying authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.85).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.9).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, snare).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority (Retributive Reading)").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, 'af8d6907-19a6-4692-a282-951af697bcce').
narrative_ontology:cs_kernel_codification('af8d6907-19a6-4692-a282-951af697bcce', formalized).
narrative_ontology:cs_authority_grounding('af8d6907-19a6-4692-a282-951af697bcce', lineage).
narrative_ontology:cs_interpretation_layer_present('af8d6907-19a6-4692-a282-951af697bcce').
narrative_ontology:cs_reading_relation('af8d6907-19a6-4692-a282-951af697bcce', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('af8d6907-19a6-4692-a282-951af697bcce', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_axiom('af8d6907-19a6-4692-a282-951af697bcce', foundational, punishment_must_be_proportionate_to_crime).
narrative_ontology:cs_axiom_status(punishment_must_be_proportionate_to_crime, holdable).
narrative_ontology:cs_axiom_grounding('af8d6907-19a6-4692-a282-951af697bcce', punishment_must_be_proportionate_to_crime, deontological).
narrative_ontology:cs_axiom('af8d6907-19a6-4692-a282-951af697bcce', foundational, execution_restores_moral_balance).
narrative_ontology:cs_axiom_status(execution_restores_moral_balance, holdable).
narrative_ontology:cs_axiom_grounding('af8d6907-19a6-4692-a282-951af697bcce', execution_restores_moral_balance, deontological).
narrative_ontology:cs_reference_frame('af8d6907-19a6-4692-a282-951af697bcce', classical_retributive_justice).
narrative_ontology:cs_drift_state('af8d6907-19a6-4692-a282-951af697bcce', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('af8d6907-19a6-4692-a282-951af697bcce', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, state_prosecutors).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, retributive_theorists).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, death_row_inmates).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, wrongfully_convicted).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and implement capital punishment, believing it serves justice by imposing proportionate punishment. Their careers and public standing are often tied to securing death sentences in high-profile cases. They administer the legal framework for execution.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_prosecutors, agenda_setter,
    institutional, biographical, constrained, national).

% Experience a sense of moral balance and closure from the execution of offenders who committed heinous crimes against their loved ones. For many, this is the only outcome that feels like true justice, restoring a perceived imbalance caused by the crime.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families, beneficiary,
    organized, generational, constrained, local).

% Are the direct targets of the constraint, losing their lives as the ultimate form of punishment. Their agency is entirely removed, and their fate is determined by the state's retributive framework.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Live under the constant threat of execution, enduring psychological and physical suffering. They bear the costs of the system through prolonged incarceration and the ultimate loss of life, even if their execution is delayed or commuted.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, death_row_inmates, payer,
    powerless, biographical, trapped, local).

% Are individuals who are sentenced to death but later found innocent. They represent the ultimate failure of the system, bearing the cost of a punishment that cannot be reversed, even if their innocence is proven before execution.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, wrongfully_convicted, payer,
    powerless, biographical, trapped, local).

% Provide the philosophical justification for capital punishment based on principles of just deserts and moral proportionality. Their theories are vindicated by the state's exercise of retributive justice, reinforcing their intellectual framework.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, retributive_theorists, beneficiary,
    analytical, civilizational, analytical, universal).

% Actively campaign against capital punishment, viewing it as a violation of fundamental human rights and an inhumane practice. They are excluded from the core decision-making processes of the retributive system, despite their organized resistance.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for the state to respond to heinous crimes with a punishment deemed morally proportionate, coordinating the state's punitive actions with societal demands for justice and retribution.
% TRANSFER_FUNCTION: Transfers the life of the convicted offender to the state, in exchange for a perceived restoration of moral balance and justice for victims' families and society.
% ABSENT_VOICES: Abolitionist advocates are systematically excluded from the core decision-making processes, despite their organized efforts. They would argue that the state's authority does not extend to taking a human life, regardless of the crime.
% DISAPPEARANCE_RATIONALE: If the authority for state execution vanished overnight, the criminal justice system would undergo a profound transformation. Sentencing for capital crimes would shift to life imprisonment without parole, victims' families would seek alternative forms of justice or closure, and the philosophical debate around punishment would intensify, reorganizing the entire punitive landscape.
% FOUNDING_PROBLEM: The problem of how to justly and proportionately punish individuals who commit crimes of extreme moral gravity, satisfying societal demands for retribution and moral balance.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by victims' families, state prosecutors, and a significant portion of the public, who believe that only execution can provide true justice for certain crimes. This is corroborated by public opinion polls and the continued pursuit of capital cases in many jurisdictions.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is high (0.85) because the ultimate cost (life) is taken from the offender, and the moral balance is 'restored' through this irreversible act. Suppression is also high (0.90) as the state actively enforces its monopoly on violence and suppresses any alternatives to its punitive authority. The theater ratio is low (0.10) because, from this reading's perspective, the act of execution is a direct and functional expression of justice, not a performance. The claimed type is 'snare' because the coordination story (restoring moral balance) is seen as a cover for the state's ultimate extraction of life, with identifiable victims and active suppression of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of victims' families and retributive theorists, the constraint is a necessary mechanism for justice and moral order. From the perspective of the executed and death row inmates, it is a pure act of state violence and extraction. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims' families and retributive theorists are beneficiaries (d near 0.0) as they gain moral satisfaction and theoretical vindication. State prosecutors are agenda-setters and beneficiaries, as they wield this power. Executed offenders, death row inmates, and the wrongfully convicted are clear targets (d near 1.0), bearing the ultimate cost. Abolitionist advocates are excluded, their arguments suppressed by the system's operational logic.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a 'rope' or 'scaffold' by highlighting the high extractiveness and suppression inherent in the retributive framework. The 'snare' classification emphasizes that while a 'moral balance' narrative exists, the actual operation involves irreversible extraction and active suppression of alternatives, with clear victims. The founding problem is considered 'live' by proponents, but the high extractiveness and suppression suggest that the 'solution' itself is highly coercive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_balance_objectivity,
    'Is ''moral balance'' an objectively measurable state that execution restores, or a subjective perception of justice for specific parties?',
    'Philosophical consensus on the nature of retributive justice, or empirical studies on the long-term psychological impact on victims'' families with and without execution.',
    'If subjective, the ''moral balance'' claim weakens, potentially reclassifying the constraint as a pure snare where the coordination story is less robust. If objective, the retributive justification gains stronger footing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_balance_objectivity, conceptual, 'Ambiguity of ''moral balance'' as a justification for execution.').

omega_variable(
    wrongful_execution_invalidation,
    'Does the occurrence of wrongful executions fundamentally invalidate the retributive framework for capital punishment, or is it an acceptable, albeit tragic, error?',
    'Legal and philosophical rulings on the ''fallibility principle'' in capital justice, or a shift in societal values that prioritizes the prevention of wrongful death over retributive satisfaction.',
    'If wrongful executions are deemed to invalidate the framework, the constraint''s legitimacy collapses, leading to reclassification as a pure snare with no defensible coordination function. If accepted as tragic error, the framework persists, but with increased pressure for safeguards.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_invalidation, preference, 'Impact of wrongful executions on the retributive justification.').

omega_variable(
    proportionality_measurement,
    'How is ''proportionate punishment'' objectively measured for heinous crimes, and is execution the only or best means to achieve it?',
    'Development of a universally accepted metric for crime severity and punishment proportionality, or a demonstration that life imprisonment without parole achieves equivalent proportionality.',
    'If proportionality can be achieved by less extractive means, the ''necessity'' of execution for moral balance is undermined, pushing the constraint towards a higher extractiveness score and a clearer snare classification. If execution is unique, the retributive claim is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_measurement, empirical, 'Objectivity and uniqueness of execution for proportionate punishment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_execution_authority__retributive_reading, theater_ratio, 1976, 0.05).
narrative_ontology:measurement(stat_tr_t1986, state_execution_authority__retributive_reading, theater_ratio, 1986, 0.08).
narrative_ontology:measurement(stat_tr_t1996, state_execution_authority__retributive_reading, theater_ratio, 1996, 0.1).
narrative_ontology:measurement(stat_tr_t2006, state_execution_authority__retributive_reading, theater_ratio, 2006, 0.12).
narrative_ontology:measurement(stat_tr_t2016, state_execution_authority__retributive_reading, theater_ratio, 2016, 0.11).
narrative_ontology:measurement(stat_tr_t2024, state_execution_authority__retributive_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_execution_authority__retributive_reading, base_extractiveness, 1976, 0.75).
narrative_ontology:measurement(stat_be_t1986, state_execution_authority__retributive_reading, base_extractiveness, 1986, 0.8).
narrative_ontology:measurement(stat_be_t1996, state_execution_authority__retributive_reading, base_extractiveness, 1996, 0.85).
narrative_ontology:measurement(stat_be_t2006, state_execution_authority__retributive_reading, base_extractiveness, 2006, 0.88).
narrative_ontology:measurement(stat_be_t2016, state_execution_authority__retributive_reading, base_extractiveness, 2016, 0.87).
narrative_ontology:measurement(stat_be_t2024, state_execution_authority__retributive_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_execution_authority__retributive_reading, suppression_requirement, 1976, 0.8).
narrative_ontology:measurement(stat_su_t1986, state_execution_authority__retributive_reading, suppression_requirement, 1986, 0.85).
narrative_ontology:measurement(stat_su_t1996, state_execution_authority__retributive_reading, suppression_requirement, 1996, 0.9).
narrative_ontology:measurement(stat_su_t2006, state_execution_authority__retributive_reading, suppression_requirement, 2006, 0.92).
narrative_ontology:measurement(stat_su_t2016, state_execution_authority__retributive_reading, suppression_requirement, 2016, 0.91).
narrative_ontology:measurement(stat_su_t2024, state_execution_authority__retributive_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
