% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: State Execution Authority — Retributive Justice Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the retributive reading of the state execution
 *   authority kernel: the claim that execution restores moral balance by
 *   imposing a proportionate punishment for the most heinous crimes,
 *   independent of any deterrent effect. Under this reading, victims'
 *   families who seek execution as the only commensurate response enter the
 *   beneficiary set directly — their moral claim is what the mechanism exists
 *   to satisfy. The executed offender's death is treated by the doctrine as a
 *   legitimate cost, not an extraction to be minimized, which is structurally
 *   distinct from the deterrence reading (where execution is instrumentally
 *   justified by future-crime prevention) and directly opposed to the
 *   abolition reading (which treats execution as categorically impermissible
 *   regardless of proportionality claims). Wrongful executions are treated
 *   within this reading as tragic procedural failures that do not invalidate
 *   the underlying moral-balance premise — this is itself a structural
 *   feature of the reading, not an oversight, and is the central locus of
 *   contest with the abolition reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.72).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.68).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority — Retributive Justice Reading").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, '759d336c-b39e-4353-beea-6752faa0ba40').
narrative_ontology:cs_kernel_codification('759d336c-b39e-4353-beea-6752faa0ba40', formalized).
narrative_ontology:cs_authority_grounding('759d336c-b39e-4353-beea-6752faa0ba40', lineage).
narrative_ontology:cs_interpretation_layer_present('759d336c-b39e-4353-beea-6752faa0ba40').
narrative_ontology:cs_reading_relation('759d336c-b39e-4353-beea-6752faa0ba40', state_execution_authority__abolition_reading, forecloses).
narrative_ontology:cs_reading_relation('759d336c-b39e-4353-beea-6752faa0ba40', state_execution_authority__deterrence_reading, influences).
narrative_ontology:cs_axiom('759d336c-b39e-4353-beea-6752faa0ba40', foundational, death_is_commensurate_desert_for_heinous_crime).
narrative_ontology:cs_axiom_status(death_is_commensurate_desert_for_heinous_crime, holdable).
narrative_ontology:cs_axiom_grounding('759d336c-b39e-4353-beea-6752faa0ba40', death_is_commensurate_desert_for_heinous_crime, deontological).
narrative_ontology:cs_axiom('759d336c-b39e-4353-beea-6752faa0ba40', secondary, procedural_error_does_not_invalidate_moral_balance_premise).
narrative_ontology:cs_axiom_status(procedural_error_does_not_invalidate_moral_balance_premise, holdable).
narrative_ontology:cs_axiom_grounding('759d336c-b39e-4353-beea-6752faa0ba40', procedural_error_does_not_invalidate_moral_balance_premise, conventional).
narrative_ontology:cs_reference_frame('759d336c-b39e-4353-beea-6752faa0ba40', classical_retributive_desert_tradition).
narrative_ontology:cs_drift_state('759d336c-b39e-4353-beea-6752faa0ba40', contemporary_innocence_project_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('759d336c-b39e-4353-beea-6752faa0ba40', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families_seeking_retribution).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, retributive_justice_doctrine_adherents).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, prosecutorial_offices_seeking_closure_narrative).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, condemned_offenders).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, wrongfully_convicted_executed).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, death_row_populations_disproportionately_poor_and_minority).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, proportionate_desert_doctrine).
narrative_ontology:constraint_vindicates(state_execution_authority__retributive_reading, moral_balance_restoration_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have lost a family member to a heinous crime and seek execution as the only punishment they experience as proportionate to their loss. The retributive framework gives their grief institutional standing and a concrete endpoint (the execution) that imprisonment does not offer. They participate in clemency hearings and victim-impact statements; the state's execution authority is the mechanism through which their claim to moral restoration is honored.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, victims_families_seeking_retribution, beneficiary,
    moderate, biographical, constrained, regional).

% Convicted of capital crimes and sentenced to death under a framework that treats their execution as the legitimate, morally required cost of restoring balance. They have no exit once conviction and sentence are affirmed on appeal; their only recourse is clemency or judicial reversal, both rare. The framework treats their death as payment owed, not as extraction to be minimized.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, condemned_offenders, payer,
    powerless, immediate, trapped, local).

% A subset of the condemned who did not commit the crime for which they were executed. The retributive framework, once execution occurs, has no mechanism to reverse the harm; the framework's own doctrine treats such cases as tragic procedural error rather than evidence against the moral-balance premise. They bear the full and irreversible cost with zero recourse.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, wrongfully_convicted_executed, payer,
    powerless, immediate, trapped, local).

% Statistically overrepresented on death row relative to population share, largely due to disparities in legal representation quality, prosecutorial charging discretion, and jury composition. The retributive framework's proportionality claim is applied unevenly across this population even though the doctrine asserts uniform desert-based justification.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, death_row_populations_disproportionately_poor_and_minority, payer,
    powerless, biographical, trapped, national).

% Charges capital cases, seeks death sentences, and carries out executions under statutory authority. Justifies the practice as restoring moral balance on behalf of the community and victims. Controls charging discretion, plea bargaining leverage (death as a bargaining chip for lesser pleas), and the execution timeline. Bears none of the offender's cost and captures institutional legitimacy from appearing to deliver justice.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, state_prosecutorial_and_judicial_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The philosophical and legal tradition holding that proportionate punishment is intrinsically required by justice, independent of deterrent effect. This tradition's continued authority is vindicated each time the state carries out an execution framed in retributive terms; it is not an actor that collects rents but a doctrine whose legitimacy is reinforced by the practice.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, retributive_justice_doctrine_adherents, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(state_execution_authority__retributive_reading, retributive_justice_doctrine_adherents).

% Argue that irreversibility, documented wrongful convictions, and disparate application make execution categorically indefensible regardless of retributive framing. Present extensively in public discourse and litigation but structurally excluded from the retributive framework's own internal accounting, which treats wrongful execution as an implementation flaw rather than grounds for reconsidering the premise.
narrative_ontology:constraint_stakeholder(state_execution_authority__retributive_reading, abolitionist_advocates_and_wrongful_conviction_organizations, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__retributive_reading, victims_families_seeking_retribution).
narrative_ontology:fixing_cost_class(state_execution_authority__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared societal mechanism for expressing that certain crimes are so severe that only the offender's death is a commensurate response, channeling private vengeance impulses into a state-administered, procedurally bounded process rather than extrajudicial retaliation.
% TRANSFER_FUNCTION: Moves the offender's life, as a resource of moral accounting, to the satisfaction of victims' families and the broader retributive claim that desert has been paid — administered and enforced by the state's prosecutorial and correctional apparatus.
% ABSENT_VOICES: Wrongfully convicted individuals who were executed cannot testify to their innocence after the fact; abolitionist and innocence-project advocates are present in public debate but are structurally excluded from the retributive doctrine's own internal justification, which does not treat their evidence as premise-invalidating.
% DISAPPEARANCE_RATIONALE: Victims' families who have organized around the promise of execution would experience its disappearance as a rupture in the justice they were promised; the retributive doctrine's institutional legitimacy would be significantly diminished. Abolitionists and much of the condemned population would experience it as harm prevented. Whether 'the world rearranges' or 'stays the same' depends entirely on which party's framework is used to evaluate the change — this is precisely the kernel contest the sibling readings exist to capture.
% FOUNDING_PROBLEM: Historically, unaddressed heinous crimes (particularly murder) generated private vengeance, blood feuds, and vigilante justice that destabilized communities; retributive execution was constructed as a state-controlled substitute that channels the demand for proportionate response through legal process.
% FOUNDING_PROBLEM_CORROBORATION: Victims'-rights organizations and retributive legal scholars attest the problem remains live — that unaddressed heinous crime still generates a legitimate demand for proportionate response that only capital punishment satisfies. Independent criminological research and judicial commissions studying wrongful convictions (e.g., innocence-project exoneration audits, state moratorium commission reports) attest from outside the beneficiary set that the empirical premises underlying proportionality claims (accuracy of conviction, uniform application) are not reliably met in practice, casting doubt on whether the founding problem is being solved rather than merely re-enacted.
narrative_ontology:disappearance_verdict(state_execution_authority__retributive_reading, contested).
narrative_ontology:founding_problem_status(state_execution_authority__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__retributive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__retributive_reading, 0.72, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is high (0.72) because the retributive premise requires the offender's death specifically — imprisonment cannot substitute without abandoning the moral-balance claim, meaning the mechanism cannot be diluted without ceasing to be this reading. Suppression (0.68) reflects the procedural and political apparatus required to maintain capital sentencing and execution against sustained abolitionist and innocence-project pressure. Theater ratio is moderate-low (0.28): the retributive function is substantively real for the families and doctrine it serves, though appellate and clemency processes carry some performative weight relative to actual reversal rates. Accessibility collapse is moderate (0.40) — alternatives (life imprisonment, restorative justice frameworks) remain visible and actively argued, unlike a genuine mountain where alternatives collapse near-completely. Resistance is substantial (0.62), driven by sustained organized abolitionist advocacy and wrongful-conviction evidence.
 *
 * DIRECTIONALITY LOGIC:
 *   Victims' families and the retributive doctrine sit near the beneficiary end: the mechanism exists to satisfy their claim to moral restoration, and the doctrine's authority is reinforced by each execution carried out in its name. Condemned offenders, and especially the wrongfully convicted among them, sit at the full-target end: trapped, immediate horizon, bearing an irreversible cost the framework classifies as legitimate rather than extractive. The state's prosecutorial apparatus is the agenda-setter — it does not itself bear cost and captures institutional legitimacy from administering the process, giving it arbitrage-grade exit relative to the population it sentences.
 *
 * MANDATROPHY ANALYSIS:
 *   The retributive reading resists mandatrophy analysis in an unusual way: its founding problem (channeling vengeance demand into state process) is treated by the doctrine as permanently live rather than resolvable, since heinous crime is not expected to cease. This makes founding_problem_status genuinely contested rather than a case where a live-vs-dead judgment cleanly applies — the mismatch consumer should read the corroboration carefully: retributive scholars assert permanent live status while wrongful-conviction audits question whether the mechanism as practiced reliably serves even its own stated premise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retributive_versus_deterrence_premise_independence,
    'Is the retributive justification genuinely independent of deterrence claims, or does public and judicial support for execution actually depend on an implicit belief that it also deters — meaning the two readings are less separable in practice than in doctrine?',
    'Survey and judicial-opinion analysis isolating retributive-only justifications from mixed retributive-deterrence reasoning in capital sentencing opinions and public polling on capital punishment support.',
    'If retributive support is substantially parasitic on implicit deterrence belief, the retributive reading''s claimed autonomy from empirical deterrence evidence is weaker than the doctrine asserts, and the two readings'' fates in public legitimacy are more coupled than the kernel decomposition suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retributive_versus_deterrence_premise_independence, empirical, 'Whether retributive justification is empirically independent of deterrence belief in practice.').

omega_variable(
    wrongful_execution_as_premise_invalidating,
    'Does documented wrongful execution constitute evidence against the moral-balance premise itself, or is it correctly classified (within this reading) as an implementation failure external to the premise''s validity?',
    'Philosophical and legal analysis of whether proportionality claims are conditional on accurate guilt determination as a logical prerequisite, versus independent of it; comparison with how other retributive-adjacent legal doctrines treat foundational accuracy requirements.',
    'If wrongful execution is premise-invalidating rather than merely an implementation flaw, the retributive reading as currently practiced may not be coherently distinguishable from a version that has already been structurally undermined by its own error rate — this would push the reading toward reclassification as substantially more extractive (snare-adjacent) than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wrongful_execution_as_premise_invalidating, conceptual, 'Whether wrongful execution evidence is internal or external to the retributive premise''s validity.').

omega_variable(
    disparate_application_versus_uniform_desert_doctrine,
    'Can the retributive doctrine''s claim of uniform, proportionate desert survive documented, persistent disparities in capital sentencing by race and socioeconomic status of the offender?',
    'Longitudinal sentencing-disparity studies controlling for crime severity, cross-referenced against the doctrine''s own internal standard of proportionality-to-offense rather than proportionality-to-offender-characteristics.',
    'Persistent, unexplained disparity would suggest the mechanism functions differently from its stated doctrine in practice, supporting a beneficiary structure (state apparatus, doctrine legitimacy) distinct from the universalist moral claim it makes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disparate_application_versus_uniform_desert_doctrine, empirical, 'Whether documented sentencing disparities undermine the doctrine''s uniform-desert claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__retributive_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__retributive_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__retributive_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(stat_tr_t24, state_execution_authority__retributive_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(stat_tr_t32, state_execution_authority__retributive_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__retributive_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__retributive_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__retributive_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__retributive_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(stat_be_t24, state_execution_authority__retributive_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement(stat_be_t32, state_execution_authority__retributive_reading, base_extractiveness, 32, 0.71).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__retributive_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__retributive_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__retributive_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__retributive_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(stat_su_t24, state_execution_authority__retributive_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(stat_su_t32, state_execution_authority__retributive_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__retributive_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the state_execution_authority kernel. The retributive reading (this file) grounds legitimacy in proportionate desert independent of consequentialist outcomes, giving it a high, non-substitutable ε (only death satisfies the moral-balance claim). The deterrence_reading shares enforcement machinery but rests on an empirically falsifiable causal claim about crime prevention, making its legitimacy conditional on deterrence evidence in a way this reading's is not. The abolition_reading directly forecloses this reading's core premise — the two cannot coexist within a single legal framework, only across competing frameworks held by different parties in the same jurisdiction's ongoing political contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
