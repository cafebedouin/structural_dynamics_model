% ============================================================================
% CONSTRAINT STORY: stance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stance_reading, []).

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
 *   constraint_id: stance_reading
 *   human_readable: Zero-Settlement Stance Reading of Unsettled Claims
 *   domain: social epistemology / signaling theory / conflict economics
 *
 * SUMMARY:
 *   This story instantiates the stance_reading of the
 *   unsettled_claim_ontology kernel: in domains where assertions are never
 *   checked against outcomes (trash talk, competitive boasting, informal
 *   status jockeying), there is no fact of self-assessment underneath the
 *   claim at all. The assertion is a social commitment guiding behavior, not
 *   a miscalibrated or calibrated belief. Truth-tracking machinery never
 *   formed because settlement was never called for. The predicted signature
 *   is confusion rather than moral injury when an outside institution
 *   suddenly imposes settlement (demanding the population 'stand behind' or
 *   'defend' the prior claim as if it had been a calibrated bet all along) —
 *   the population has no internal answer because the question was never live
 *   for them.
 *
 * KEY AGENTS:
 *   - trash_talking_participants: primary beneficiaries (moderate/mobile) — signal cheaply, stake nothing
 *   - informal_status_arenas: agenda-setting beneficiary (organized/mobile) — sustain the zero-settlement norm
 *   - outside_observers: excluded analytical seat — mistake stance for calibrated claim, confused rather than harmed
 *   - settlement_imposers: excluded institutional seat — retroactively demand calibration the exchange never produced
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stance_reading, 0.12).
domain_priors:suppression_score(stance_reading, 0.08).
domain_priors:theater_ratio(stance_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stance_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(stance_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(stance_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stance_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(stance_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stance_reading, rope).
narrative_ontology:human_readable(stance_reading, "Zero-Settlement Stance Reading of Unsettled Claims").
narrative_ontology:topic_domain(stance_reading, "social epistemology / signaling theory / conflict economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stance_reading, '26cfce70-b904-4695-a883-f91594bb1a1d').
narrative_ontology:cs_kernel_codification('26cfce70-b904-4695-a883-f91594bb1a1d', distributed).
narrative_ontology:cs_authority_grounding('26cfce70-b904-4695-a883-f91594bb1a1d', practice).
narrative_ontology:cs_interpretation_layer_present('26cfce70-b904-4695-a883-f91594bb1a1d').
narrative_ontology:cs_reading_relation('26cfce70-b904-4695-a883-f91594bb1a1d', unsettled_claim_ontology__register_reading, coexists_with).
narrative_ontology:cs_reading_relation('26cfce70-b904-4695-a883-f91594bb1a1d', unsettled_claim_ontology__drift_reading, coexists_with).
narrative_ontology:cs_reading_relation('26cfce70-b904-4695-a883-f91594bb1a1d', unsettled_claim_ontology__filter_reading, coexists_with).
narrative_ontology:cs_reading_relation('26cfce70-b904-4695-a883-f91594bb1a1d', unsettled_claim_ontology__impression_management_reading, influences).
narrative_ontology:cs_axiom('26cfce70-b904-4695-a883-f91594bb1a1d', foundational, no_calibration_target_beneath_assertion).
narrative_ontology:cs_axiom_status(no_calibration_target_beneath_assertion, holdable).
narrative_ontology:cs_axiom_grounding('26cfce70-b904-4695-a883-f91594bb1a1d', no_calibration_target_beneath_assertion, empirically_contingent).
narrative_ontology:cs_axiom('26cfce70-b904-4695-a883-f91594bb1a1d', secondary, settlement_imposition_is_category_error_not_correction).
narrative_ontology:cs_axiom_status(settlement_imposition_is_category_error_not_correction, holdable).
narrative_ontology:cs_axiom_grounding('26cfce70-b904-4695-a883-f91594bb1a1d', settlement_imposition_is_category_error_not_correction, conventional).
narrative_ontology:cs_reference_frame('26cfce70-b904-4695-a883-f91594bb1a1d', assertion_without_calibration_target).
narrative_ontology:cs_drift_state('26cfce70-b904-4695-a883-f91594bb1a1d', external_settlement_imposition, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('26cfce70-b904-4695-a883-f91594bb1a1d', '').
narrative_ontology:cs_kernel_id(stance_reading, unsettled_claim_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stance_reading, trash_talking_participants).
narrative_ontology:constraint_beneficiary(stance_reading, informal_status_arenas).
narrative_ontology:constraint_beneficiary(stance_reading, banter_communities).
narrative_ontology:constraint_vindicates(stance_reading, assertion_without_underlying_calibration_target).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trade boasts, trash talk, and competitive claims (about skill, toughness, prospects) in contexts where no one will ever check the claim against a settled fact. They get to signal confidence, build rapport through shared performance, and jockey for status, all without the claim ever being cashed out. Leaving the exchange costs nothing; the stance was never a bet they staked anything real on.
narrative_ontology:constraint_stakeholder(stance_reading, trash_talking_participants, beneficiary,
    moderate, immediate, mobile, local).

% The social spaces themselves (locker rooms, group chats, barstools) sustain the norm that these assertions are stances, not calibrated beliefs. They set the informal rule that no one is expected to track accuracy here, which is what lets the practice run cheaply and keep people engaged.
narrative_ontology:constraint_stakeholder(stance_reading, informal_status_arenas, beneficiary,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(stance_reading, informal_status_arenas, agenda_setter).

% People unfamiliar with the zero-settlement norm sometimes mistake the assertions for genuine calibrated claims and are confused when no one defends or revises them under scrutiny. They aren't harmed, just outside the frame that makes the practice legible; their confusion is a signal of the mismatch, not a grievance.
narrative_ontology:constraint_stakeholder(stance_reading, outside_observers, excluded,
    analytical, immediate, analytical, local).

% Institutions (courts, prediction markets, employers demanding performance reviews) that suddenly require the zero-settlement population to stand behind their prior stances as if they had always been calibrated claims. The population being asked has no internal answer to 'could you actually?' because that question was never live for them; the imposer is applying a different kernel reading onto data that never generated it.
narrative_ontology:constraint_stakeholder(stance_reading, settlement_imposers, excluded,
    institutional, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Lets people signal confidence, affiliation, and competitive posture cheaply, without requiring the cognitive or social overhead of tracking whether the claim is actually true — a genuine coordination good in contexts where accuracy was never the point.
% TRANSFER_FUNCTION: Nothing is transferred in the extractive sense: status and rapport circulate among participants who all understand the stance-not-belief framing; no party pays a cost that another collects.
% ABSENT_VOICES: Settlement-imposing institutions are structurally absent from the zero-settlement arena itself — they only appear after the fact, when they try to apply accuracy standards the original exchange never adopted, and by then the population has no calibrated fact to produce.
% DISAPPEARANCE_RATIONALE: If the zero-settlement norm vanished overnight, the same social spaces would either fall silent (losing a cheap signaling channel) or migrate the practice elsewhere; no arrangement of resources, obligations, or records depends on the assertions remaining unsettled — nothing rearranges because nothing was ever staked.
% FOUNDING_PROBLEM: People needed a low-cost way to signal status, confidence, and social bonding in contexts where no verification mechanism existed and none was worth building.
% FOUNDING_PROBLEM_CORROBORATION: Linguists and sociolinguists studying banter, trash talk, and informal boasting (outside the participant communities themselves) corroborate that these speech acts function as social performatives rather than truth-apt assertions in the relevant contexts; no settlement-imposing institution disputes that the original exchange lacked a verification mechanism — they dispute only whether one should now be retrofitted.
narrative_ontology:disappearance_verdict(stance_reading, world_unchanged).
narrative_ontology:founding_problem_status(stance_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stance_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(stance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(stance_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stance_reading_tests).
:- end_tests(stance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression are both low and flat across the interval: this is a genuine low-overhead coordination good, not a cover story for extraction. Theater ratio is low because there is no performative accuracy-tracking machinery to perform in the first place — the absence of truth-tracking is not concealed, it is simply structurally absent. Accessibility collapse is moderate (0.3) because alternative signaling norms (e.g., calibrated betting, formal claims) remain fully available elsewhere; nothing suppresses them, the zero-settlement domain just doesn't use them. Resistance is low because no one inside the domain is fighting to introduce settlement — resistance would only appear when an outside imposer arrives, which this story treats as an excluded seat rather than an internal dynamic.
 *
 * DIRECTIONALITY LOGIC:
 *   Participants and the arenas that host them are the clear beneficiaries: low cost, high signaling value, no accuracy debt incurred. There are no victims within the domain because nothing is transferred at anyone's expense — the coordination good is genuinely cheap. Settlement_imposers and outside_observers are marked excluded rather than payer/beneficiary because they are outside the frame that generates the stance in the first place; their confusion or demand for retroactive calibration is a category mismatch with the domain, not an extraction the domain performs on them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cheap status/confidence signaling without verification overhead) remains live — this is not a case of an arrangement outliving its function. The risk of mislabeling runs the other direction: someone could mistake the zero-settlement practice for a snare (people making claims they can't back up) when in fact no claim was ever staked in the settlement sense. Classifying this as rope, with the coordination function explicit, prevents importing an accuracy standard the domain never adopted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stance_vs_belief_underdetermination,
    'Is it ever possible to distinguish, from the outside, a genuine zero-settlement stance (no underlying belief) from a belief that is merely being strategically concealed (impression_management_reading) or one that is decaying uncalibrated over time (drift_reading)?',
    'Behavioral test: offer the speaker a real, low-cost settlement opportunity (a private, consequence-free bet) and observe whether they treat the question as live or as a non-sequitur. A stance-reading speaker should find the question genuinely confusing, not merely inconvenient.',
    'If speakers consistently treat private settlement offers as coherent (even if declined for other reasons), the domain is better modeled as impression_management_reading or drift_reading, not stance_reading — a calibration target existed and was being managed or had decayed, not never formed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stance_vs_belief_underdetermination, empirical, 'Whether zero-settlement assertions genuinely lack an underlying calibration target or merely conceal/decay one.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given the same corpus of trash-talk and boasting utterances, what observable feature licenses choosing stance_reading over the sibling readings (register, drift, filter, impression management)?',
    'Cross-reading comparison: catalog which reading best predicts the confusion-not-surprise signature when settlement is externally imposed; stance_reading predicts genuine bafflement, filter_reading predicts irritation at a category error, impression_management_reading predicts embarrassment or backpedaling.',
    'Choosing the wrong reading for a given population misattributes the population''s later behavior under settlement pressure — treating filter-reading speakers as stance-reading speakers would misread their irritation as confusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which structural signal distinguishes the stance reading from its sibling readings of the same kernel.').

omega_variable(
    false_mountain_of_naturalness,
    'Is the zero-settlement norm a naturally occurring feature of low-stakes social speech (no one ever needed truth-tracking machinery), or is it a constructed convention actively maintained by informal status arenas because settlement would deflate the signaling value the arenas currently extract in reputational terms?',
    'Compare zero-settlement domains with structurally similar domains where settlement WAS introduced (e.g., prediction-market gamification of trash talk) and observe whether participation and signaling value survive the introduction of accountability.',
    'If signaling value collapses once settlement is introduced, informal_status_arenas benefit from a constructed absence of accountability, which would push the classification toward a mild tangled_rope rather than a clean rope. This story''s beneficiaries list makes the mountain-adjacent worry moot since claimed_type is rope, not mountain — but the ambiguity about naturalness vs. maintained convention remains open.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_mountain_of_naturalness, conceptual, 'Whether the absence of truth-tracking machinery is a natural feature of low-stakes speech or an actively preserved convention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stance_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stan_tr_t0, stance_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(stan_tr_t4, stance_reading, theater_ratio, 4, 0.08).
narrative_ontology:measurement(stan_tr_t8, stance_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(stan_tr_t12, stance_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement(stan_tr_t16, stance_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(stan_tr_t20, stance_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(stan_tr_t24, stance_reading, theater_ratio, 24, 0.1).

% Extraction over time
narrative_ontology:measurement(stan_be_t0, stance_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(stan_be_t4, stance_reading, base_extractiveness, 4, 0.1).
narrative_ontology:measurement(stan_be_t8, stance_reading, base_extractiveness, 8, 0.11).
narrative_ontology:measurement(stan_be_t12, stance_reading, base_extractiveness, 12, 0.11).
narrative_ontology:measurement(stan_be_t16, stance_reading, base_extractiveness, 16, 0.12).
narrative_ontology:measurement(stan_be_t20, stance_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(stan_be_t24, stance_reading, base_extractiveness, 24, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(stance_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stance_reading, identity_coordination).
narrative_ontology:affects_constraint(stance_reading, register_reading).
narrative_ontology:affects_constraint(stance_reading, drift_reading).
narrative_ontology:affects_constraint(stance_reading, filter_reading).
narrative_ontology:affects_constraint(stance_reading, impression_management_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the unsettled_claim_ontology kernel, decomposed per the ε-invariance principle: 'unsettled claims' is a natural-language label covering structurally distinct populations (zero-settlement stances, register-governed claims, calibration-decayed claims, interlocutor-filtering claims, and strategically managed beliefs). Each reading gets its own ε, beneficiary structure, and classification; this reading's ε (0.12, low and flat) reflects a domain where no calibration target exists to extract against, distinguishing it sharply from impression_management_reading (where an underlying belief IS being managed, implying higher potential extractiveness) and drift_reading (where decay implies a target that once existed).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
