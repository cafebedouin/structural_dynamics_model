% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: State Execution Authority â Retributive Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the retributive_reading of the
 *   state_execution_authority kernel: the claim that state execution restores
 *   moral balance by imposing proportionate punishment for heinous crimes.
 *   The kernel is contested by the deterrence_reading (execution prevents
 *   future murders) and the abolition_reading (execution is categorically
 *   impermissible). In this reading, victims' families enter the beneficiary
 *   set as recipients of moral restoration, while executed offenders are the
 *   structural targets who bear the legitimate cost of proportionate justice.
 *   Wrongful execution is acknowledged as tragic error but does not
 *   invalidate the framework. The constraint is actively enforced by the
 *   state carceral apparatus and requires continuous legal and procedural
 *   maintenance to exclude abolitionist alternatives.
 *
 * KEY AGENTS:
 *   - state_execution_apparatus: Primary agenda setter (institutional / identity_locked) â administers and enforces the retributive framework
 *   - victims_families: Primary beneficiary (moderate / constrained) â receives closure and moral balance
 *   - executed_offenders: Primary target (powerless / trapped) â bears the ultimate cost
 *   - wrongfully_convicted: Secondary target (powerless / trapped) â bears the ultimate cost despite innocence
 *   - abolitionist_advocates: Excluded voice (organized / constrained) â would abolish the constraint but is overruled
 *   - constitutional_interpreter: Analytical observer (analytical / analytical) â evaluates jurisprudential consistency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.82).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.78).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority â Retributive Reading").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, 'ea51228a-9ba5-4d4a-90b4-b374a0d4be64').
narrative_ontology:cs_kernel_codification('ea51228a-9ba5-4d4a-90b4-b374a0d4be64', fixed_text).
narrative_ontology:cs_authority_grounding('ea51228a-9ba5-4d4a-90b4-b374a0d4be64', lineage).
narrative_ontology:cs_interpretation_layer_present('ea51228a-9ba5-4d4a-90b4-b374a0d4be64').
narrative_ontology:cs_reading_relation('ea51228a-9ba5-4d4a-90b4-b374a0d4be64', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_reading_relation('ea51228a-9ba5-4d4a-90b4-b374a0d4be64', state_execution_authority__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('ea51228a-9ba5-4d4a-90b4-b374a0d4be64', foundational, moral_balance_requires_proportionate_punishment).
narrative_ontology:cs_axiom_status(moral_balance_requires_proportionate_punishment, holdable).
narrative_ontology:cs_axiom_grounding('ea51228a-9ba5-4d4a-90b4-b374a0d4be64', moral_balance_requires_proportionate_punishment, deontological).
narrative_ontology:cs_axiom('ea51228a-9ba5-4d4a-90b4-b374a0d4be64', foundational, execution_is_proportionate_for_heinous_murder).
narrative_ontology:cs_axiom_status(execution_is_proportionate_for_heinous_murder, holdable).
narrative_ontology:cs_axiom_grounding('ea51228a-9ba5-4d4a-90b4-b374a0d4be64', execution_is_proportionate_for_heinous_murder, deontological).
narrative_ontology:cs_axiom('ea51228a-9ba5-4d4a-90b4-b374a0d4be64', secondary, wrongful_error_does_not_invalidate_framework).
narrative_ontology:cs_axiom_status(wrongful_error_does_not_invalidate_framework, holdable).
narrative_ontology:cs_axiom_grounding('ea51228a-9ba5-4d4a-90b4-b374a0d4be64', wrongful_error_does_not_invalidate_framework, conventional).
narrative_ontology:cs_reference_frame('ea51228a-9ba5-4d4a-90b4-b374a0d4be64', proportional_punishment_framework).
narrative_ontology:cs_drift_state('ea51228a-9ba5-4d4a-90b4-b374a0d4be64', contemporary_constitutional_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ea51228a-9ba5-4d4a-90b4-b374a0d4be64', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, retributive_public).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, wrongfully_convicted).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, retributive_justice_doctrine).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, proportionate_punishment_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers capital punishment through courts, carceral systems, and execution protocols. Justifies the practice as the state's duty to impose proportionate punishment for heinous crimes. Bound by statutory and constitutional mandates that fuse institutional identity with punitive authority.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_execution_apparatus, agenda_setter,
    institutional, generational, identity_locked, national).

% Receive the promised good of moral balance and closure through the execution of the offender. Their satisfaction is contingent on the state completing the sentence; they cannot achieve this closure through private action.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families, beneficiary,
    moderate, biographical, constrained, national).

% Derives social and moral order from the belief that heinous crimes receive proportionate, ultimate punishment. Supports the legal framework electorally and culturally; relies on the state to perform the retributive function.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, retributive_public, beneficiary,
    organized, generational, constrained, national).

% Bear the ultimate cost of the constraint: loss of life. They are the structural target of the retributive framework, with no exit from the sentence once imposed.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% Bear the same ultimate cost despite factual innocence. The retributive reading admits this as tragic error but maintains the framework; they are structurally indistinguishable from the guilty until exoneration, which often occurs too late.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, wrongfully_convicted, payer,
    powerless, immediate, trapped, local).

% Advance abolitionist arguments that execution is categorically impermissible. Structurally excluded from the retributive framework's legitimacy calculus; their objections are treated as morally irrelevant to proportionate punishment.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, national).

% Analyzes the constitutional and jurisprudential validity of the retributive framework under Eighth Amendment and penal law doctrine. Evaluates whether the practice remains consistent with evolving standards of decency.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, constitutional_interpreter, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__retributive_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restores moral balance and expresses societal condemnation of heinous crimes; provides a normative framework for proportional punishment and a structured mechanism for victims' families to achieve closure through state-administered justice.
% TRANSFER_FUNCTION: Transfers the ultimate cost â the offender's life â from the moral ledger of the community to the executed offender, while transferring moral restoration, closure, and social-order affirmation to victims' families and the retributive public.
% ABSENT_VOICES: The executed offenders and wrongfully convicted are silenced by the sentence itself; abolitionist advocates are present in public discourse but structurally excluded from the retributive legitimacy framework, treated as outside the moral calculus of just deserts.
% DISAPPEARANCE_RATIONALE: If execution vanished, the retributive edifice would collapse: the moral-balance framework requires death as the proportionate response to heinous murder, and substitution by life imprisonment would force a fundamental reconstitution of the penal philosophy. Victims' families would lose the specific closure mechanism, and the state's retributive authority would shift to incarceration.
% FOUNDING_PROBLEM: How to impose punishment that respects the moral gravity of heinous crimes and gives meaning to 'just deserts' when the offense is murder, ensuring that the offender does not retain life while victims have lost theirs.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist advocates and empirical criminal justice scholars from outside the retributive tradition attest that the founding problem is solvable through life imprisonment without parole, and that the retributive demand for death is a metaphysical preference rather than a practical necessity. No independent corroborating source outside the beneficiary set affirms that execution is the only viable response to heinous crime.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because the retributive framework demands the non-substitutable price of life; imprisonment is read as morally inadequate, so the extraction is total and ultimate. Suppression is high (0.78) because the constraint persists only through active legal exclusion of abolitionist alternatives, procedural barriers to commutation, and statutory mandates. Theater ratio is moderate (0.25): the execution ritual carries genuine symbolic weight for retributive believers but also serves performative state functions of demonstrating resolve. Accessibility collapse is high (0.80) because once the retributive framework is accepted, restorative justice and simple imprisonment collapse as morally credible alternatives for heinous crimes. Resistance is substantial (0.70) due to persistent abolitionist movements, civil liberties litigation, and exoneration evidence. Temporal measurements share a single grid and show gradual intensification as political polarization hardens enforcement machinery.
 *
 * PERSPECTIVAL GAP:
 *   The state_execution_apparatus and victims_families seats compute the constraint as coordination: moral balance restored, societal order affirmed, and proportionate justice delivered. The executed_offenders and wrongfully_convicted seats compute it as lethal extraction with no exit. The abolitionist_advocates seat computes it as a snare dressed in moral language. The engine derives this divergence from the same structural data: beneficiary declarations for victims_families and retributive_public, victim declarations for executed_offenders, and the extreme power/exit asymmetry between institutional agenda-setters and trapped, powerless payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims_families and retributive_public are declared beneficiaries (d near 0.0), receiving moral closure and social order. Executed_offenders and wrongfully_convicted are declared victims (d near 1.0), bearing the cost of life. The state_execution_apparatus is the agenda setter with identity_locked exit (d near 0.0) because it administers and legitimates the constraint. Abolitionist_advocates are excluded from the framework's beneficiary structure and experience high directionality toward the constraint as a target of their opposition, though they are not direct payers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â how to impose proportionate punishment for heinous crimes â is contested but still live in retentive jurisdictions. The retributive justification is actively invoked in judicial opinions, political campaigns, and victim-advocate discourse, so the constraint has not atrophied into a piton. Mandatrophy would occur if executions continued primarily for bureaucratic or political inertia after the retributive public abandoned the moral-balance rationale; temporal measurements show theater_ratio stable below 0.30 and extractiveness rising, indicating active ideological investment rather than inertial maintenance. The R5 genealogy (founding_problem_status: contested) supports active contestation without obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the retributive reading of the state_execution_authority kernel. Does the coexistence of retributive, deterrence, and abolition readings indicate that the kernel is underdetermined by evidence, or that the state deploys multiple inconsistent justifications for the same extractive practice?',
    'Corpus analysis of whether the readings converge on the same structural beneficiaries and victim sets despite divergent normative premises; if all readings preserve the same extraction pattern while shifting rationales, the kernel is likely a cover for institutional power.',
    'If the kernel is underdetermined, the retributive reading''s classification as tangled_rope may be stable; if the state deploys inconsistent justifications while preserving extraction, the kernel computes as a snare-family pattern regardless of the sincerity of individual readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the kernel''s multiple readings represent genuine normative pluralism or strategic justification layering.').

omega_variable(
    substitutability_of_execution,
    'Can life imprisonment without parole genuinely substitute for execution in achieving the retributive goal of moral balance, or does the retributive framework logically require death as the proportionate response to heinous murder?',
    'Comparative jurisprudential analysis of jurisdictions that abolished execution: whether retributive publics in those jurisdictions report moral-balance deficits or adapt to incarceration as sufficient proportionality.',
    'If imprisonment is structurally substitutable, the retributive reading''s high extractiveness is a contingent policy choice rather than a logically necessary feature, suggesting the coordination function could be decoupled from the lethal extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substitutability_of_execution, conceptual, 'Whether the retributive constraint''s lethal extraction is logically necessary or contingently chosen.').

omega_variable(
    wrongful_execution_validity,
    'Does the acknowledged possibility of wrongful execution structurally undermine the retributive reading''s proportionality claim, or is it a tragic but institutionally tolerable error rate within a legitimate moral framework?',
    'Empirical measurement of exoneration rates post-execution and comparative analysis of whether error-tolerance thresholds differ between retributive and non-retributive legal frameworks.',
    'If wrongful execution is structurally intolerable to proportionality, the retributive reading collapses toward a snare (pure extraction with a broken coordination story); if tolerable, the tangled_rope classification holds despite the moral cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_validity, empirical, 'Whether wrongful execution error invalidates the retributive coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__retributive_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__retributive_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__retributive_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__retributive_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__retributive_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__retributive_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__retributive_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__retributive_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__retributive_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__retributive_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__retributive_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__retributive_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__retributive_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__retributive_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__retributive_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__retributive_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__retributive_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__retributive_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the state_execution_authority kernel, decomposed from the natural-language concept of state execution authority into three structurally distinct claims per the epsilon-invariance principle. The retributive reading is linked to its siblings for corpus navigation and family analysis; the structural relationships are specified in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
