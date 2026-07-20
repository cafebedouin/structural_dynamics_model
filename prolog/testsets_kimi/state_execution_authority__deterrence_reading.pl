% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution Authority (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the deterrence reading of the state
 *   execution authority kernel: the claim that capital punishment is
 *   justified because it prevents future murders by raising the cost of
 *   heinous crime. Under this reading, future potential victims enter the
 *   beneficiary set, the executed offender is treated as an instrumental
 *   cost, and wrongful execution is a utilitarian loss to be minimized. The
 *   constraint is a contested commitment system: its legitimacy depends on
 *   empirical claims about deterrent efficacy that have been substantially
 *   challenged by criminological research, while its operation produces
 *   irreversible extraction from a trapped, powerless payer set.
 *
 * KEY AGENTS:
 *   - state_execution_apparatus: Agenda-setter (institutional/constrained) â administers the constraint and justifies it via deterrence research
 *   - future_potential_victims: Beneficiary (powerless/constrained) â purportedly protected by elevated cost signal
 *   - executed_offenders: Payer (powerless/trapped) â bear the ultimate cost of the deterrent signal
 *   - wrongfully_convicted_persons: Payer (powerless/trapped) â absorb the system's error rate as utilitarian loss
 *   - abolitionist_advocates: Excluded (organized/mobile) â structurally sidelined in risk-calculus frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.58).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.72).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution Authority (Deterrence Reading)").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, 'd5ad3866-61f5-44a2-b0d1-3db44ce5c53e').
narrative_ontology:cs_kernel_codification('d5ad3866-61f5-44a2-b0d1-3db44ce5c53e', formalized).
narrative_ontology:cs_authority_grounding('d5ad3866-61f5-44a2-b0d1-3db44ce5c53e', lineage).
narrative_ontology:cs_interpretation_layer_present('d5ad3866-61f5-44a2-b0d1-3db44ce5c53e').
narrative_ontology:cs_reading_relation('d5ad3866-61f5-44a2-b0d1-3db44ce5c53e', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('d5ad3866-61f5-44a2-b0d1-3db44ce5c53e', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('d5ad3866-61f5-44a2-b0d1-3db44ce5c53e', foundational, execution_permissible_if_deterrent_effect_proven).
narrative_ontology:cs_axiom_status(execution_permissible_if_deterrent_effect_proven, holdable).
narrative_ontology:cs_axiom_grounding('d5ad3866-61f5-44a2-b0d1-3db44ce5c53e', execution_permissible_if_deterrent_effect_proven, empirically_contingent).
narrative_ontology:cs_reference_frame('d5ad3866-61f5-44a2-b0d1-3db44ce5c53e', instrumental_punishment_authority).
narrative_ontology:cs_drift_state('d5ad3866-61f5-44a2-b0d1-3db44ce5c53e', contemporary_empirical_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d5ad3866-61f5-44a2-b0d1-3db44ce5c53e', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, future_potential_victims).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers capital sentencing and execution protocols; commissions and cites criminological studies on deterrent effect to justify continued operation; controls the machinery of death and sets procedural rules for selecting and executing condemned persons.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Diffuse population of persons whose risk of being murdered is purportedly reduced by the credible threat of execution; they do not choose this protection, cannot verify the deterrent effect themselves, and have no direct voice in sentencing or execution decisions.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, future_potential_victims, beneficiary,
    powerless, immediate, constrained, national).

% Bear the ultimate cost of the deterrence signal; convicted of capital crimes and subjected to state execution after exhaustion of appeals; have no exit from the sentence and no ability to convert the cost into any other form.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Persons factually innocent of the capital crime who are nonetheless convicted and executed; represent a known error-rate byproduct of the system whose cost the deterrence reading must absorb as a utilitarian loss; have no effective exit once the machinery has convicted them.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_convicted_persons, payer,
    powerless, immediate, trapped, national).

% Moral and legal advocates who argue execution is categorically impermissible; structurally sidelined in legislative frameworks that treat execution as a risk-management instrument rather than a rights violation; their objections are treated as external to the empirical cost-benefit calculus.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents future homicides by elevating the expected cost of committing capital crimes beyond what non-capital sanctions provide, thereby coordinating prospective offenders away from murder through credible threat of death.
% TRANSFER_FUNCTION: Transfers the life of the condemned offender, and by error some wrongfully convicted persons, into a state-administered deterrent signal; also transfers prosecutorial and judicial resources from other correctional functions to the capital process.
% ABSENT_VOICES: Executed offenders cannot speak after sentence; wrongfully convicted persons are heard only posthumously if at all; abolitionist moral philosophers are treated as normatively external to the empirical risk-calculus; jurisdictions without capital punishment are excluded from the comparative deterrence dataset by selection.
% DISAPPEARANCE_RATIONALE: If execution authority vanished, capital-case plea bargaining dynamics would shift toward life-without-parole, the carceral population would increase under substitution sentences, the execution bureaucracy would dissolve, and the empirical deterrence signal (if genuine) would degrade; the criminal justice system would rearrange around long-term incarceration.
% FOUNDING_PROBLEM: Homicide rates perceived as insufficiently restrained by existing penalties, requiring a sanction of maximum severity to deter rational would-be murderers.
% FOUNDING_PROBLEM_CORROBORATION: Criminologists and economists outside the prosecutorial apparatus contest whether the founding problem (inadequate deterrence from non-capital sanctions) is real or solved by execution; the National Research Council and independent meta-analyses report negligible or unidentifiable deterrent effect, corroborating that the problem status is contested rather than settled.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint takes livesâa maximal costâbut the reading claims an offsetting coordination benefit (saved future lives) that keeps the net extraction below snare levels. Suppression is high (0.72) because the constraint's persistence depends on active state enforcement: apprehension, capital conviction, incarceration, and execution, with limited exit for the condemned. Theater ratio rises over the interval (0.42 at end) as executions become rarer and more ritualized in retentionist jurisdictions, increasing the performative component relative to the claimed deterrent function. Accessibility collapse is moderate (0.50): life-without-parole is a known alternative, but the legal system treats it as substitutable only after empirical proof of equivalent deterrence, which is methodologically contested and rarely accepted. Resistance is substantial (0.62) from abolitionist movements, wrongful-exoneration advocacy, and shifting prosecutorial norms.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state execution apparatus) experiences the constraint as a difficult but necessary administrative function serving public safety; its directionality is moderated by institutional obligation rather than personal gain. The payer seats (executed offenders and wrongfully convicted persons) experience total extraction with no offsetting benefit; their directionality sits at the full-target end. The beneficiary seat (future potential victims) is diffuse, powerless, and unable to verify the benefit, yielding a low directionality but one that is structurally inertâthey do not coordinate the constraint, they merely receive its purported protection.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (future_potential_victims) drive low directionality for that seat. Victim declarations (executed_offenders, wrongfully_convicted_persons) drive high directionality toward the full-target end. The state_execution_apparatus is neither beneficiary nor victim; as agenda_setter with constrained exit, it defaults toward moderate directionality reflecting its structural role in maintaining the mechanism rather than capturing its gains. Abolitionist_advocates are excluded from the calculus, receiving no directionality assignment in the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling the constraint as pure coordination (rope) by requiring declared victims and active enforcement, which the execution mechanism plainly has. It also prevents mislabeling as pure extraction (snare) by preserving the empirical deterrence claim as a live (if contested) coordination function; the engine will test whether the coordination is genuine or cover. The rising theater ratio and contested founding problem status signal drift toward piton or snare if the deterrence claim continues to erode without institutional sunset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_empirical_status,
    'Does execution provide marginal deterrence for homicide beyond what life-without-parole provides?',
    'Panel-data meta-analysis comparing homicide trends in matched jurisdictions before and after death penalty abolition or adoption, controlling for enforcement intensity and demographic covariates.',
    'If no marginal deterrent effect is found, the reading''s foundational axiom is falsified, collapsing the constraint toward pure extraction (snare) or inertial maintenance (piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical_status, empirical, 'Whether the deterrence claim survives empirical scrutiny').

omega_variable(
    kernel_reading_ambiguity,
    'Is the constraint better read as an instrumental deterrence mechanism or as a retributive expression with deterrence as ex post rationalization?',
    'Legislative-history and prosecutorial-charging pattern analysis: do retentionist jurisdictions adopt or retain execution only where deterrence is emphasized, or equally where retributive norms dominate?',
    'If retributive motives dominate, the deterrence reading is a surface rationalization rather than the operative constraint, and the true classification aligns with the retributive reading''s distinct Îµ profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether deterrence is the operative justification or a cover for retribution').

omega_variable(
    wrongful_execution_rate_uncertainty,
    'What is the actual rate of wrongful execution, and does it exceed the threshold where the utilitarian calculus inverts?',
    'Post-hoc DNA exoneration rates in capital cases, projected to executed populations via capture-recapture statistical methods.',
    'If the wrongful execution rate is higher than the deterrence benefit measured in lives saved, the reading''s own utilitarian framework demands abolition or extreme procedural restriction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wrongful_execution_rate_uncertainty, empirical, 'Whether wrongful executions swamp the claimed deterrence benefit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(state_exec_deterrence_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(state_exec_deterrence_tr_t8, state_execution_authority__deterrence_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(state_exec_deterrence_tr_t16, state_execution_authority__deterrence_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(state_exec_deterrence_tr_t24, state_execution_authority__deterrence_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(state_exec_deterrence_tr_t32, state_execution_authority__deterrence_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(state_exec_deterrence_tr_t40, state_execution_authority__deterrence_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(state_exec_deterrence_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(state_exec_deterrence_be_t8, state_execution_authority__deterrence_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(state_exec_deterrence_be_t16, state_execution_authority__deterrence_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(state_exec_deterrence_be_t24, state_execution_authority__deterrence_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(state_exec_deterrence_be_t32, state_execution_authority__deterrence_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(state_exec_deterrence_be_t40, state_execution_authority__deterrence_reading, base_extractiveness, 40, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(state_execution_authority__deterrence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% The kernel 'state_execution_authority' conflates three structurally distinct justifications: deterrence (instrumental, empirically contingent), retribution (deontological proportionality), and abolition (categorical prohibition). Each reading instantiates a different constraint with distinct beneficiary/victim structures, Îµ profiles, and axioms. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
