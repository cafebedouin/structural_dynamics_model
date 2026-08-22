% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__abolition_reading, []).

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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: State Execution Authority (Abolition Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story models the abolitionist reading of state execution
 *   authority: the standing arrangement by which states deliberately kill
 *   condemned persons. From this reading, the arrangement is pure extraction
 *   dressed in the language of deterrence and retribution; all executed
 *   persons are victims, including the guilty, because the state lacks
 *   categorical authority to take life. The reading rejects substitution
 *   arguments (life imprisonment is qualitatively different) and treats every
 *   wrongful execution as proof of systemic illegitimacy. This is one reading
 *   of the contested kernel state_execution_authority, structurally distinct
 *   from retributive and deterrence siblings.
 *
 * KEY AGENTS:
 *   - executed_persons: Primary targets (powerless/trapped) â bear the ultimate extraction of life.
 *   - death_row_population: Secondary targets (powerless/trapped) â bear the anticipatory extraction of life under sentence.
 *   - wrongfully_convicted_condemned: Exemplary targets (powerless/trapped) â demonstrate the irreversible error of the system.
 *   - state_execution_apparatus: Agenda setter (institutional/constrained) â administers the lethal machinery.
 *   - abolitionist_legal_movement: Analytical observer (organized/analytical) â documents failures and advocates repeal.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.92).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.82).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "State Execution Authority (Abolition Reading)").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, '9c5da796-d73d-40f4-a7e2-72b12157296b').
narrative_ontology:cs_kernel_codification('9c5da796-d73d-40f4-a7e2-72b12157296b', formalized).
narrative_ontology:cs_authority_grounding('9c5da796-d73d-40f4-a7e2-72b12157296b', lineage).
narrative_ontology:cs_interpretation_layer_present('9c5da796-d73d-40f4-a7e2-72b12157296b').
narrative_ontology:cs_reading_relation('9c5da796-d73d-40f4-a7e2-72b12157296b', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c5da796-d73d-40f4-a7e2-72b12157296b', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('9c5da796-d73d-40f4-a7e2-72b12157296b', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('9c5da796-d73d-40f4-a7e2-72b12157296b', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('9c5da796-d73d-40f4-a7e2-72b12157296b', secondary, life_imprisonment_qualitatively_distinct).
narrative_ontology:cs_axiom_status(life_imprisonment_qualitatively_distinct, holdable).
narrative_ontology:cs_axiom_grounding('9c5da796-d73d-40f4-a7e2-72b12157296b', life_imprisonment_qualitatively_distinct, deontological).
narrative_ontology:cs_reference_frame('9c5da796-d73d-40f4-a7e2-72b12157296b', dignity_based_penal_order).
narrative_ontology:cs_drift_state('9c5da796-d73d-40f4-a7e2-72b12157296b', contemporary_retentionist_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9c5da796-d73d-40f4-a7e2-72b12157296b', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, death_row_population).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, wrongfully_convicted_condemned).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lose their lives by deliberate state killing; no exit from the sentence once the execution is carried out.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, national).

% Confined in maximum security under sentence of death, awaiting execution while pursuing appeals that rarely result in exoneration or commutation.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, death_row_population, payer,
    powerless, immediate, trapped, national).

% Innocent persons trapped in the capital process; their wrongful conviction and execution demonstrate the irreversible error built into the arrangement.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, wrongfully_convicted_condemned, payer,
    powerless, immediate, trapped, national).

% Courts, corrections departments, and executive officers who schedule, review, and carry out death sentences under statutory authority; bound by legal mandate but holding discretionary power over clemency and execution methods.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Human rights organizations, capital defense litigators, and innocence projects that document wrongful convictions, challenge execution methods, and advocate for abolition.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolitionist_legal_movement, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, there is no genuine coordination function; the claimed functions of deterrence and retribution are rejected as post-hoc moral justifications for state violence. Historically, the arrangement claimed to solve extreme offending through lethal incapacitation and symbolic moral balancing, but the abolition reading treats these claims as cover stories.
% TRANSFER_FUNCTION: Moves life itself from condemned individuals to the state's assertion of ultimate penal sovereignty; the transfer is irreversible and extracts existence as its commodity.
% ABSENT_VOICES: The executed cannot speak after the sentence is carried out; wrongfully convicted persons on death row are structurally disbelieved; future abolitionist majorities in retentionist jurisdictions are excluded from current lawmaking; international human rights monitors are excluded from domestic clemency processes.
% DISAPPEARANCE_RATIONALE: Maximum penal sentences would revert to life imprisonment; prosecutorial charging practices would shift away from capital-eligible offenses; the symbolic assertion of state power over life would collapse; and the carceral system's upper boundary would be redefined.
% FOUNDING_PROBLEM: How to punish the most severe crimes in an era before secure long-term incarceration, and how to satisfy communal demands for retributive vengeance against heinous offenders.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary penology and criminology from outside the retentionist state apparatus (e.g., innocence project data, comparative murder-rate studies in abolitionist versus retentionist jurisdictions) attest that long-term incarceration achieves incapacitation without execution, and that the retributive demand can be satisfied through non-lethal sentences. Abolitionist legal scholars and international human rights bodies corroborate that the founding problems are either disproven or soluble without execution.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.92, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-maximum (0.92) because the constraint extracts life itself, an irreversible and total transfer. Suppression is high (0.82) because the arrangement persists only where the state actively suppresses abolition alternatives, excludes rival penal frameworks, and maintains lethal infrastructure despite international human-rights consensus. Theater ratio is substantial (0.55) because elaborate procedural safeguards (automatic appeals, method-of-execution review, clemency hearings) function as ritual performance that rarely alter outcomes but legitimize the extraction. Accessibility collapse is high (0.88) because, once the machinery reaches the individual condemned person, no alternative exit exists; resistance (0.72) reflects sustained abolitionist litigation and international condemnation.
 *
 * PERSPECTIVAL GAP:
 *   The executed person and the state apparatus occupy diametrically opposed seats: the former experiences total extraction with zero exit, while the latter experiences institutional power and administrative discretion. The abolitionist observer seat computes the constraint as snare; the retentionist apparatus seat might compute it as tangled_rope (claiming coordination through deterrence). The engine captures this divergence from the same structural data. The abolition reading specifically rejects the tangled_rope possibility by denying the legitimacy of the coordination claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Executed persons, death-row populations, and the wrongfully convicted are declared victims (structural targets, d near 1.0). No beneficiaries are declared because the abolition reading rejects retribution and deterrence as producing legitimate benefits; the state apparatus is the agenda_setter (d near 0.5 symmetric or slightly beneficiary for institutional power, but overridden by the reading's rejection of legitimacy). The automatic derivation would position the apparatus near beneficiary; the reading's structural rejection of all benefit claims suppresses beneficiary declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolition reading prevents mislabeling by rejecting the coordination story entirely. Where a naive analysis might see deterrence or retribution as genuine coordination functions (yielding tangled_rope), the abolition reading's categorical prohibition treats these as retrospective cover, forcing the classification toward snare. The R5 genealogy interview supports this: the founding problem (incapacitation/retribution via death) is assessed as dead, meaning the arrangement persists without the functional justification that would make it coordination. The mismatch between dead founding_problem and world_rearranges disappearance_verdict signals capture: the constraint has outlived its mandate but continues to extract.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is state execution authority best modeled as a contested kernel with structurally distinct readings, or as a single constraint with observer-dependent disagreement?',
    'Comparative analysis of whether retributive, deterrence, and abolition readings produce different epsilon values, victim sets, and beneficiary structures when authored as separate constraints.',
    'If the readings are structurally distinct, each requires its own constraint story; if observer-relative, a single story with perspectival divergence suffices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether this kernel decomposes into multiple constraints or one contested constraint.').

omega_variable(
    deterrence_empirical_status,
    'Does capital punishment deter homicide at rates exceeding life imprisonment?',
    'Meta-analysis of econometric and criminological panel studies comparing murder rates in matched jurisdictions with and without the death penalty.',
    'If deterrence is empirically demonstrated, the abolition reading''s claim of pure extraction faces a coordination-function challenge; if deterrence is disproven, the snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_empirical_status, empirical, 'Empirical status of deterrence under the abolition reading.').

omega_variable(
    wrongful_execution_rate,
    'What is the rate of wrongful execution under contemporary procedural safeguards?',
    'Posthumous DNA testing, innocence commission reviews, and statistical estimation of false conviction rates in capital cases.',
    'A measurable wrongful execution rate above zero confirms the abolition reading''s claim that procedural safeguards cannot render the constraint non-extractive; a rate near zero would weaken the categorical claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_rate, empirical, 'Rate of irreversible error in capital cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__abolition_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__abolition_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__abolition_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__abolition_reading, theater_ratio, 40, 0.6).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__abolition_reading, base_extractiveness, 10, 0.91).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__abolition_reading, base_extractiveness, 20, 0.92).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__abolition_reading, base_extractiveness, 30, 0.93).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__abolition_reading, base_extractiveness, 40, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__abolition_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__abolition_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__abolition_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__abolition_reading, suppression_requirement, 40, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, state_execution_authority__deterrence_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the state_execution_authority kernel. The retributive and deterrence readings instantiate structurally distinct constraints from the same kernel, differing in beneficiary/victim structure and epsilon assessment. The abolition reading denies all coordination function and claims maximal extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
