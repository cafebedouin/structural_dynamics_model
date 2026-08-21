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
 *   authority, where capital punishment is justified as a proportionate
 *   response to heinous crimes, restoring moral balance. It is one reading of
 *   the 'state_execution_authority' kernel. The other readings are
 *   'deterrence_reading' (execution prevents future crimes) and
 *   'abolition_reading' (execution is categorically impermissible). This
 *   reading instantiates a Snare because it involves high extraction (the
 *   offender's life) and high suppression (the state's power to execute),
 *   with a coordination story (moral balance) that serves as cover for the
 *   extraction. Wrongful execution, while tragic, is framed as a procedural
 *   error rather than an invalidation of the underlying retributive
 *   principle.
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
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, snare).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority (Retributive Reading)").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, '2feff345-0b80-4205-8775-86a0daf906eb').
narrative_ontology:cs_kernel_codification('2feff345-0b80-4205-8775-86a0daf906eb', formalized).
narrative_ontology:cs_authority_grounding('2feff345-0b80-4205-8775-86a0daf906eb', lineage).
narrative_ontology:cs_interpretation_layer_present('2feff345-0b80-4205-8775-86a0daf906eb').
narrative_ontology:cs_reading_relation('2feff345-0b80-4205-8775-86a0daf906eb', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('2feff345-0b80-4205-8775-86a0daf906eb', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('2feff345-0b80-4205-8775-86a0daf906eb', foundational, punishment_must_be_proportionate_to_crime).
narrative_ontology:cs_axiom_status(punishment_must_be_proportionate_to_crime, holdable).
narrative_ontology:cs_axiom_grounding('2feff345-0b80-4205-8775-86a0daf906eb', punishment_must_be_proportionate_to_crime, deontological).
narrative_ontology:cs_axiom('2feff345-0b80-4205-8775-86a0daf906eb', foundational, execution_restores_moral_equilibrium).
narrative_ontology:cs_axiom_status(execution_restores_moral_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('2feff345-0b80-4205-8775-86a0daf906eb', execution_restores_moral_equilibrium, deontological).
narrative_ontology:cs_reference_frame('2feff345-0b80-4205-8775-86a0daf906eb', classical_retributive_justice).
narrative_ontology:cs_drift_state('2feff345-0b80-4205-8775-86a0daf906eb', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2feff345-0b80-4205-8775-86a0daf906eb', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, state_justice_system).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, death_row_inmates).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, defense_attorneys).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the legal framework for capital punishment, including trials, appeals, and executions. Benefits from the perceived moral authority and public trust derived from imposing 'just' punishment. Its legitimacy is tied to upholding the retributive principle.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_justice_system, agenda_setter,
    institutional, generational, constrained, national).

% Experience a sense of moral balance and closure from the state's execution of those who committed heinous crimes against their loved ones. Their advocacy often reinforces the retributive justification for capital punishment.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families, beneficiary,
    organized, biographical, constrained, local).

% Bear the ultimate cost of the constraint, their lives. From the retributive perspective, this is a proportionate and just outcome for their crimes, not an 'extraction' in the conventional sense, but a necessary restoration of moral order.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Live under the constant threat of execution, enduring psychological and physical suffering. They are the direct targets of the constraint's punitive function, awaiting the 'restoration of balance'.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, death_row_inmates, payer,
    powerless, immediate, trapped, local).

% Bear the professional and emotional burden of defending clients against the state's ultimate penalty. They operate within a system designed to impose this punishment, often facing immense pressure and limited resources.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, defense_attorneys, payer,
    moderate, biographical, constrained, national).

% Argue that state execution is a violation of fundamental human rights and an irreversible injustice, regardless of retributive claims. Their arguments are often dismissed or marginalized within the retributive framework as irrelevant to the principle of just deserts.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, global).

% Monitor and condemn capital punishment as a violation of international law and human rights norms. Their observations challenge the legitimacy of the retributive reading from an external, universalist perspective.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's response to heinous crimes by providing a framework for imposing a punishment believed to be proportionate to the offense, thereby restoring a perceived moral equilibrium in society.
% TRANSFER_FUNCTION: Transfers the life of the offender to the state as a form of moral payment for the crime, from the offender to the victims' families and the broader society, in the name of justice.
% ABSENT_VOICES: Abolitionist advocates and international human rights bodies are largely excluded from the core retributive discourse, as their arguments against capital punishment are often framed as irrelevant to the principle of just deserts. The executed offender's voice is permanently silenced.
% DISAPPEARANCE_RATIONALE: If state execution authority vanished overnight, the criminal justice system would undergo a profound re-evaluation of its punitive philosophy. Victims' families would lose a source of perceived justice, and the state would need to find alternative means to address the moral outrage of heinous crimes, leading to significant societal and legal reorganization.
% FOUNDING_PROBLEM: The problem of how to justly respond to crimes that are so heinous they are perceived to shatter the moral order, demanding a punishment that restores balance and affirms the value of the victim's life.
% FOUNDING_PROBLEM_CORROBORATION: Victims' families and a significant portion of the public attest that the problem of moral imbalance from heinous crimes remains live and requires capital punishment for resolution. This is corroborated by public opinion polls and the continued legislative and judicial support for capital punishment in many jurisdictions, reflecting a societal demand for retributive justice.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because the constraint demands the ultimate cost (life) from the offender, which is considered a necessary 'payment' for moral restoration. Suppression is also high (0.90) due to the state's monopoly on legitimate force and the irreversible nature of the punishment. Theater ratio is low (0.20) as the retributive function is genuinely believed and actively pursued by its proponents, not merely performed. The high accessibility_collapse (0.70) reflects the limited legal and practical alternatives for those facing capital charges, while high resistance (0.75) comes from defense attorneys and abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and victims' families, this is a just and necessary mechanism for moral order. From the perspective of the condemned and abolitionists, it is an act of state violence and irreversible injustice. The engine's classification will highlight this divergence, showing a Snare from the payer/excluded seats and a more 'justified' (though still extractive) constraint from the beneficiary seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The state justice system and victims' families are beneficiaries, gaining moral satisfaction and perceived justice. Executed offenders and death row inmates are the ultimate targets, bearing the full cost. Defense attorneys, while part of the system, are structurally positioned as payers due to the adversarial nature of capital cases. Abolitionist advocates are excluded, their arguments structurally outside the retributive framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_balance_quantification,
    'Is ''moral balance'' a quantifiable or objectively verifiable state, or is its restoration primarily a subjective experience for beneficiaries?',
    'Sociological studies on long-term societal impacts of capital punishment vs. life imprisonment, and psychological studies on closure for victims'' families, seeking objective correlates for ''moral balance''.',
    'If purely subjective, the coordination story (restoring balance) is weaker, potentially reclassifying the constraint closer to pure extraction. If objective correlates exist, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_balance_quantification, conceptual, 'The objective vs. subjective nature of ''moral balance'' as a coordination outcome.').

omega_variable(
    wrongful_execution_impact,
    'How many wrongful executions would be required to invalidate the retributive framework for capital punishment, and is this threshold acknowledged by proponents?',
    'Analysis of legal and philosophical texts from retributive proponents to identify any stated or implied threshold for systemic error, and empirical tracking of exonerations post-execution.',
    'If a threshold exists and is exceeded, the constraint''s legitimacy within its own framework collapses, potentially reclassifying it as a Piton or even a Snare with a broken coordination story. If no threshold is acknowledged, it highlights the framework''s resilience to empirical challenge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wrongful_execution_impact, conceptual, 'The impact of wrongful executions on the validity of the retributive framework.').

omega_variable(
    alternative_punishment_efficacy,
    'Can alternative punishments (e.g., life imprisonment without parole) achieve the same ''moral balance'' as execution from a retributive perspective?',
    'Philosophical and legal arguments comparing the retributive ''just deserts'' achieved by different forms of punishment, and public discourse analysis on societal satisfaction with alternatives.',
    'If alternatives are found to be equally effective in restoring moral balance, the necessity of execution for this reading is undermined, potentially reducing its claimed coordination function and increasing its perceived extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_punishment_efficacy, preference, 'The retributive equivalence of alternative punishments to execution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__retributive_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__retributive_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__retributive_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__retributive_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__retributive_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__retributive_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__retributive_reading, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__retributive_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__retributive_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__retributive_reading, base_extractiveness, 30, 0.84).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__retributive_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__retributive_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__retributive_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__retributive_reading, suppression_requirement, 10, 0.87).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__retributive_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__retributive_reading, suppression_requirement, 30, 0.89).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__retributive_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__retributive_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
