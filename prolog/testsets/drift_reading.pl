% ============================================================================
% CONSTRAINT STORY: drift_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_drift_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: drift_reading
 *   human_readable: Drift Reading: Bravado Register as Self-Model Modifying Cause
 *   domain: social_epistemology/signaling_theory/conflict_economics
 *
 * SUMMARY:
 *   This story instantiates the drift_reading of the unsettled_claim_ontology
 *   kernel: the claim that sustained occupancy of a bravado register is not
 *   merely stance-taking or filtered self-presentation but a genuine causal
 *   process that reshapes the underlying self-model. Where sibling readings
 *   hold that the gap between private calibration and public confidence is
 *   recoverable under the right incentive conditions (stance-without-belief,
 *   register performance, or impression management with intact private
 *   estimates), this reading predicts that a meaningful subpopulation of
 *   long-run occupants will show a gap that does NOT collapse under
 *   incentive-compression testing — because there is no longer a distinct,
 *   more-accurate private estimate to recover. The theater_ratio trajectory
 *   falls over the interval precisely because the story's own claim is that
 *   what starts as performance (high theater, low genuine extraction) becomes
 *   progressively less performative and more genuinely, structurally
 *   miscalibrated (rising extraction, falling theater) as drift consolidates.
 *
 * KEY AGENTS:
 *   - drifted_speakers: primary target (moderate/identity_locked) — internalizes the register into genuine miscalibrated belief and bears the downstream cost
 *   - downstream_decision_targets: secondary victims (powerless/trapped) — act on speakers' degraded private signal without means to detect drift
 *   - high_status_bravado_occupants: primary beneficiary (powerful/arbitrage) — captures status returns while institutionally buffered from drift's accuracy costs
 *   - reputation_market_incumbents: agenda-setting beneficiary (institutional/arbitrage) — sets the incentive gradient rewarding confident register over calibrated register
 *   - low_status_challengers: exposed payer (powerless/constrained) — drifts faster, pays more, without institutional cushioning
 *   - calibration_researchers: analytical observer (analytical) — measures the bimodal collapse/no-collapse population this reading predicts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(drift_reading, 0.58).
domain_priors:suppression_score(drift_reading, 0.42).
domain_priors:theater_ratio(drift_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(drift_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(drift_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(drift_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(drift_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(drift_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(drift_reading, tangled_rope).
narrative_ontology:human_readable(drift_reading, "Drift Reading: Bravado Register as Self-Model Modifying Cause").
narrative_ontology:topic_domain(drift_reading, "social_epistemology/signaling_theory/conflict_economics").

domain_priors:requires_active_enforcement(drift_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(drift_reading, 'a7642366-0c02-4221-a109-788303659042').
narrative_ontology:cs_kernel_codification('a7642366-0c02-4221-a109-788303659042', distributed).
narrative_ontology:cs_authority_grounding('a7642366-0c02-4221-a109-788303659042', distributed).
narrative_ontology:cs_reading_relation('a7642366-0c02-4221-a109-788303659042', unsettled_claim_ontology__stance_reading, forecloses).
narrative_ontology:cs_reading_relation('a7642366-0c02-4221-a109-788303659042', unsettled_claim_ontology__register_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7642366-0c02-4221-a109-788303659042', unsettled_claim_ontology__filter_reading, coexists_with).
narrative_ontology:cs_reading_relation('a7642366-0c02-4221-a109-788303659042', unsettled_claim_ontology__impression_management_reading, influences).
narrative_ontology:cs_axiom('a7642366-0c02-4221-a109-788303659042', foundational, sustained_performance_reshapes_self_model).
narrative_ontology:cs_axiom_status(sustained_performance_reshapes_self_model, holdable).
narrative_ontology:cs_axiom_grounding('a7642366-0c02-4221-a109-788303659042', sustained_performance_reshapes_self_model, empirically_contingent).
narrative_ontology:cs_axiom('a7642366-0c02-4221-a109-788303659042', secondary, compression_testing_reveals_bimodal_not_unimodal_population).
narrative_ontology:cs_axiom_status(compression_testing_reveals_bimodal_not_unimodal_population, holdable).
narrative_ontology:cs_axiom_grounding('a7642366-0c02-4221-a109-788303659042', compression_testing_reveals_bimodal_not_unimodal_population, empirically_contingent).
narrative_ontology:cs_reference_frame('a7642366-0c02-4221-a109-788303659042', recoverable_private_estimate_baseline).
narrative_ontology:cs_drift_state('a7642366-0c02-4221-a109-788303659042', post_incentive_compression_testing_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a7642366-0c02-4221-a109-788303659042', '').
narrative_ontology:cs_kernel_id(drift_reading, unsettled_claim_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(drift_reading, reputation_market_incumbents).
narrative_ontology:constraint_beneficiary(drift_reading, high_status_bravado_occupants).
narrative_ontology:constraint_victim(drift_reading, drifted_speakers).
narrative_ontology:constraint_victim(drift_reading, downstream_decision_targets).
narrative_ontology:constraint_victim(drift_reading, low_status_challengers).
narrative_ontology:constraint_vindicates(drift_reading, belief_revision_is_use_dependent).
narrative_ontology:constraint_vindicates(drift_reading, self_model_plasticity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopted the bravado register initially as tactical stance-taking in status contests, expecting to retain the ability to recalibrate privately. Over repeated occupancy, their private confidence estimates converge toward their public claims — under incentive-compression testing (private stakes, anonymous elicitation, no audience) their stated certainty no longer reverts to a lower baseline. They now genuinely believe claims they once merely performed, and bear the downstream cost of acting on miscalibrated confidence: bad bets, bad predictions, damaged credibility when reality intervenes.
narrative_ontology:constraint_stakeholder(drift_reading, drifted_speakers, payer,
    moderate, biographical, identity_locked, national).

% Rely on drifted speakers' confident claims to make real decisions (investment, medical, policy, interpersonal) without a reliable way to distinguish drifted miscalibration from genuine expertise, since the speaker's own private signal has degraded along with the public one. They pay the cost of others' internalized bravado in the form of misallocated trust.
narrative_ontology:constraint_stakeholder(drift_reading, downstream_decision_targets, payer,
    powerless, immediate, trapped, regional).

% Occupy high-confidence registers as a durable status strategy; their institutional position insulates them from the accuracy costs of drift (staff absorb error, reputational buffers are thick), so they capture the status and audience benefits of bravado while externalizing the miscalibration cost onto subordinates and audiences. For them the register never fully becomes belief-threatening because feedback loops that would force recalibration are structurally weak.
narrative_ontology:constraint_stakeholder(drift_reading, high_status_bravado_occupants, beneficiary,
    powerful, generational, arbitrage, national).

% Media platforms, punditry markets, and expert-signaling institutions that reward confident register over calibrated register, and thereby set the incentive gradient that pushes speakers into sustained bravado occupancy. They administer the attention economy that makes drift profitable for early adopters and structurally likely for anyone who stays in the register long enough.
narrative_ontology:constraint_stakeholder(drift_reading, reputation_market_incumbents, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(drift_reading, reputation_market_incumbents, agenda_setter).

% Attempt to compete in confidence-signaling markets without the institutional buffer that protects incumbents from drift's costs; when they occupy bravado registers to compete, they drift into genuine miscalibration faster and pay for it in career and reputational terms without the cushion that shields powerful occupants.
narrative_ontology:constraint_stakeholder(drift_reading, low_status_challengers, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(drift_reading, low_status_challengers, excluded).

% Run incentive-compression tests (private stakes, anonymized elicitation) to distinguish register-only performers (gap collapses under compression) from genuinely drifted speakers (gap survives compression). They observe the bimodal population this reading predicts but have no power to alter the incentive structure that produces it.
narrative_ontology:constraint_stakeholder(drift_reading, calibration_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Confident public assertion economizes on listeners' evaluation costs — a speaker who commits to a stance saves the audience the work of weighing hedged uncertainty, and social status contests are resolved faster when someone commits to a register rather than everyone hedging indefinitely.
% TRANSFER_FUNCTION: Moves accuracy costs from the speaker occupying the bravado register (who is protected from the reputational consequences of overclaiming, especially if institutionally buffered) to the downstream targets who act on the speaker's now-genuinely-miscalibrated confidence, and to the speaker's own later, less-buffered self when the drift becomes visible under compression.
% ABSENT_VOICES: The pre-drift version of the speaker — the version whose private calibration was still separable from public performance — has no standing to object; that self no longer exists in any interrogable form once drift has occurred, which is precisely what distinguishes this reading from stance/register readings where a recoverable private estimate persists.
% DISAPPEARANCE_RATIONALE: If long-run bravado occupancy stopped causing genuine belief drift (i.e., if the drift_reading were false and one of the sibling readings were correct instead), incentive-compression testing would show all gaps collapsing under private, low-stakes elicitation — no genuinely miscalibrated residue. Whether the world 'rearranges' if this specific causal mechanism disappeared is exactly the contested empirical question the reading stakes out; proponents of stance/register readings would say nothing changes because no such mechanism exists, while this reading holds that removing it would restore a population of speakers whose private and public confidence diverge cleanly again.
% FOUNDING_PROBLEM: Explaining why some long-term confident communicators appear to become non-recoverable even under conditions designed to strip away audience incentives — why hedging that should reappear under anonymity sometimes does not.
% FOUNDING_PROBLEM_CORROBORATION: Calibration researchers running incentive-compression protocols attest to a bimodal empirical pattern (some gaps collapse, some survive) that is consistent with genuine drift for a subpopulation; this corroboration comes from an analytical seat outside the beneficiary set (reputation market incumbents and high-status occupants have no stake in the drift mechanism being real or false, and reputation incumbents in particular would prefer the stance_reading, since it implies no downstream liability for having produced miscalibrated speakers).
narrative_ontology:disappearance_verdict(drift_reading, contested).
narrative_ontology:founding_problem_status(drift_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(drift_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(drift_reading, 'none', 1).
narrative_ontology:epsilon_provenance(drift_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(drift_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(drift_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(drift_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.28 to 0.58 across the interval to model drift consolidating: early occupancy is mostly performance riding on a coordination function (fast status resolution, audience evaluation savings — hence high initial theater_ratio at 0.75), but as the self-model reshapes with repeated occupancy, the extraction becomes real rather than performed — private calibration genuinely degrades, and the cost this imposes on downstream decision targets and on the speaker's own later, less-buffered self is a real transfer, not theater. Theater_ratio falls correspondingly from 0.75 to 0.40 as the mask literally becomes the face: less of what's happening is performance, more of it is genuine (if harmful) belief. Suppression (0.42) is moderate rather than extreme because the mechanism operates through reinforcement and status incentive rather than coercive barrier — no one is forced to occupy the register, but the incentive gradient set by reputation_market_incumbents makes exit costly once status accrues to it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat (reputation_market_incumbents), the register looks like healthy, functioning status signaling — a rope. From the drifted_speakers' own retrospective seat (were such a seat available, which is itself contested under this reading), the arrangement looks like an extraction that consumed their own calibration capacity. From downstream_decision_targets, it looks like an information asymmetry they cannot resolve because even the speaker no longer has privileged access to a more-accurate private estimate.
 *
 * DIRECTIONALITY LOGIC:
 *   High_status_bravado_occupants and reputation_market_incumbents sit near the beneficiary end: they capture the status/attention returns of confident registers while institutional buffering (staff, reputational reserves, structural insulation) prevents drift's accuracy costs from landing on them directly — their exit option is arbitrage because they can shift register or narrative without bearing consequences. Drifted_speakers sit near the target end with identity_locked exit: once genuine drift has occurred, there is no recoverable prior calibration to 'exit back to' — this is the defining structural signature of this reading versus its siblings, where stance/register/filter readings would assign these same agents constrained or mobile exit because a recoverable private estimate persists. Downstream_decision_targets and low_status_challengers are trapped/constrained payers who bear costs without institutional protection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (why hedging fails to reappear even under anonymity-protected elicitation) remains genuinely contested rather than resolved either way — this is not a case of a coordination function that clearly outlived its purpose, but a case where the coordination function (fast status resolution via confident assertion) may still be doing real work for some occupants (register-only performers whose gap collapses under compression) while for others (genuinely drifted occupants) the same surface behavior has become pure extraction with no recoverable private signal underneath. Classifying this as tangled_rope rather than snare or rope preserves that mixture: the coordination function is real for part of the population and the extraction is real for another part, and conflating them into a single clean type would erase exactly the bimodal structure this reading predicts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compression_test_population_structure,
    'Does incentive-compression testing (private stakes, anonymous elicitation, stripped audience incentive) actually reveal a bimodal population — some gaps collapsing (register-only) and some surviving (genuinely drifted) — or does it reveal a unimodal population consistent with one of the sibling readings alone?',
    'Longitudinal incentive-compression study tracking the same speakers'' confidence gaps under audience-present and audience-stripped conditions over years of register occupancy; bimodality in the residual gap distribution would corroborate this reading over its unimodal-collapse siblings.',
    'A confirmed bimodal population validates the drift mechanism as real for a subpopulation and supports treating this as a distinct constraint (tangled_rope) from the sibling readings; a unimodal collapse-to-zero result would falsify this reading and support stance_reading or register_reading instead, at which point this story should be retired or reclassified toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compression_test_population_structure, empirical, 'Whether the predicted bimodal collapse/no-collapse population under incentive compression actually exists.').

omega_variable(
    recoverability_of_pre_drift_calibration,
    'Once drift has occurred, is there any intervention (extended isolation from audience, explicit recalibration training, adversarial feedback) that restores the pre-drift private estimate, or is the change genuinely irreversible within the timeframe studied?',
    'Intervention studies removing drifted speakers from bravado-rewarding contexts for extended periods and measuring whether calibration gaps re-open (indicating partial recoverability) or remain closed (indicating genuine, durable self-model change).',
    'If recoverable given enough time/intervention, this reading''s ''genuine drift'' claim weakens toward a register/filter reading with slow reversion; if durably irreversible, it strengthens the case that this is a distinct causal mechanism warranting separate constraint status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recoverability_of_pre_drift_calibration, empirical, 'Whether drifted calibration is ever recoverable, bearing on whether this reading names a genuinely distinct mechanism.').

omega_variable(
    institutional_buffering_as_confound,
    'Is the observed asymmetry between high-status occupants (protected from drift''s costs) and low-status challengers (exposed to them) evidence of differential drift itself, or simply differential consequence-exposure with identical underlying drift rates?',
    'Compare drift rates (via compression testing) across status levels holding occupancy duration constant, to separate ''drifts less'' from ''pays less when drifted.''',
    'If high-status occupants drift at the same rate but simply bear fewer consequences, the beneficiary/victim structure in this story should be revised to reflect consequence-buffering rather than differential drift susceptibility — a distinct but related structural claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_buffering_as_confound, conceptual, 'Whether status-based asymmetry in this story reflects differential drift or differential consequence exposure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(drift_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(drif_tr_t0, drift_reading, theater_ratio, 0, 0.75).
narrative_ontology:measurement(drif_tr_t4, drift_reading, theater_ratio, 4, 0.68).
narrative_ontology:measurement(drif_tr_t8, drift_reading, theater_ratio, 8, 0.6).
narrative_ontology:measurement(drif_tr_t12, drift_reading, theater_ratio, 12, 0.52).
narrative_ontology:measurement(drif_tr_t16, drift_reading, theater_ratio, 16, 0.47).
narrative_ontology:measurement(drif_tr_t20, drift_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(drif_tr_t24, drift_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(drif_be_t0, drift_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(drif_be_t4, drift_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(drif_be_t8, drift_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(drif_be_t12, drift_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement(drif_be_t16, drift_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(drif_be_t20, drift_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(drif_be_t24, drift_reading, base_extractiveness, 24, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(drift_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(drift_reading, identity_coordination).
narrative_ontology:affects_constraint(drift_reading, stance_reading).
narrative_ontology:affects_constraint(drift_reading, register_reading).
narrative_ontology:affects_constraint(drift_reading, filter_reading).
narrative_ontology:affects_constraint(drift_reading, impression_management_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the unsettled_claim_ontology kernel, each authored as its own ε-invariant constraint per the ε-invariance principle. The natural-language phrase 'the bravado register' conflates at least these five structurally distinct claims about what sustained confident performance does to the underlying belief state. This reading (drift_reading) claims a genuine, non-recoverable causal effect on the self-model and is authored with a substantially extractive, tangled_rope profile reflecting that if true, real and growing harm accrues to drifted speakers and those who rely on them. The sibling readings should be authored with their own distinct ε values reflecting their own structural claims (e.g., stance_reading, if the private estimate is fully recoverable, should show much lower and non-accumulating extractiveness). All five are linked via affects_constraints to preserve the family structure; none should be read as agreeing on a single ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
