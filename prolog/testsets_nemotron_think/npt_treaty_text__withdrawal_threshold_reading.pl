% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__withdrawal_threshold_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold Interpretation
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   Article X of the NPT permits withdrawal if 'extraordinary events, related
 *   to the subject matter of this Treaty, have jeopardized the supreme
 *   interests of its country.' This constraint story captures the
 *   interpretive contest over what threshold those words impose. The
 *   high-threshold reading (regime stability priority) requires objective,
 *   treaty-wide extraordinary events — effectively a collective security
 *   determination. The low-threshold reading (sovereignty preservation
 *   priority) treats 'supreme interests' as self-judging — each state
 *   decides. North Korea's 2003 withdrawal, accepted by some as valid and
 *   rejected by others, created a precedent that neither reading can fully
 *   absorb. The ambiguity benefits threshold states (Iran, Japan, South
 *   Korea) by keeping their exit option credible without invoking it — a
 *   classic tangled rope: genuine coordination (regime stability requires
 *   some threshold) fused with asymmetric extraction (threshold states
 *   harvest diplomatic value from the ambiguity).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.48).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.52).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold Interpretation").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '9ad1a761-96e1-43a6-93d1-2421a28587c7').
narrative_ontology:cs_kernel_codification('9ad1a761-96e1-43a6-93d1-2421a28587c7', formalized).
narrative_ontology:cs_authority_grounding('9ad1a761-96e1-43a6-93d1-2421a28587c7', lineage).
narrative_ontology:cs_interpretation_layer_present('9ad1a761-96e1-43a6-93d1-2421a28587c7').
narrative_ontology:cs_reading_relation('9ad1a761-96e1-43a6-93d1-2421a28587c7', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ad1a761-96e1-43a6-93d1-2421a28587c7', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_axiom('9ad1a761-96e1-43a6-93d1-2421a28587c7', foundational, sovereign_withdrawal_right).
narrative_ontology:cs_axiom_status(sovereign_withdrawal_right, holdable).
narrative_ontology:cs_axiom_grounding('9ad1a761-96e1-43a6-93d1-2421a28587c7', sovereign_withdrawal_right, deontological).
narrative_ontology:cs_axiom('9ad1a761-96e1-43a6-93d1-2421a28587c7', foundational, regime_stability_primacy).
narrative_ontology:cs_axiom_status(regime_stability_primacy, holdable).
narrative_ontology:cs_axiom_grounding('9ad1a761-96e1-43a6-93d1-2421a28587c7', regime_stability_primacy, conventional).
narrative_ontology:cs_reference_frame('9ad1a761-96e1-43a6-93d1-2421a28587c7', treaty_text_as_written).
narrative_ontology:cs_drift_state('9ad1a761-96e1-43a6-93d1-2421a28587c7', post_north_korea_withdrawal, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ad1a761-96e1-43a6-93d1-2421a28587c7', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, verification_regime).
narrative_ontology:constraint_vindicates(npt_treaty_text__withdrawal_threshold_reading, sovereign_withdrawal_right).
narrative_ontology:constraint_vindicates(npt_treaty_text__withdrawal_threshold_reading, regime_stability_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with latent nuclear capacity (Iran, Japan, South Korea) who maintain the credible threat of withdrawal as diplomatic leverage. They benefit from the ambiguity of Article X's 'extraordinary events' threshold — a low threshold preserves their sovereignty option; a high threshold constrains it. Their exit from the constraint is not leaving the treaty but invoking Article X, which the ambiguity makes more credible.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states, beneficiary,
    moderate, biographical, constrained, global).

% The five NPT-recognized NWS (US, Russia, UK, France, China) who interpret Article X to require a high threshold for withdrawal to preserve regime stability. They set the interpretive agenda through Security Council resolutions, IAEA governance, and diplomatic practice. They bear minimal direct cost from the constraint — their nuclear status is unaffected — but invest diplomatic capital in maintaining the high-threshold interpretation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% The 186 NNWS parties who rely on the NPT's stability for their security assurances. They bear the costs when withdrawal ambiguity encourages proliferation hedging or actual breakout — regional security deteriorates, verification resources stretch, and the bargain (non-proliferation for disarmament) erodes. Their exit options are constrained: leaving the treaty isolates them diplomatically and triggers supplier restrictions.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% The IAEA safeguards system and its supporting institutional architecture. Withdrawal ambiguity directly degrades verification credibility — when a state invokes Article X, inspectors lose access, continuity of knowledge breaks, and the regime's deterrent value diminishes. The regime cannot 'exit' the constraint; it absorbs the degradation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, verification_regime, payer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, verification_regime, observer).

% The enforcement authority that translates withdrawal events into Chapter VII responses. Its credibility depends on the clarity of the threshold — ambiguous thresholds produce paralyzed or inconsistent responses (e.g., North Korea 2003 vs Iran post-2018). It sets the agenda through resolutions but is constrained by P5 veto dynamics.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, un_security_council, agenda_setter,
    institutional, biographical, analytical, global).

% The only state to have invoked Article X withdrawal (2003). Its precedent is cited by all sides but its legal effect is contested — was it a valid withdrawal? Did the 90-day notice suffice? Were 'extraordinary events' demonstrated? The precedent sits outside the current interpretive community, neither fully accepted nor rejected, and cannot participate in the interpretation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, north_korea_precedent, excluded,
    powerful, immediate, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the stability of the non-proliferation regime by establishing a shared, if contested, interpretive baseline for the supreme exit clause. Without a threshold interpretation, any state could withdraw on minimal pretext, collapsing the assurance structure that makes non-proliferation bargains credible.
% TRANSFER_FUNCTION: Transfers interpretive authority and exit-option credibility from the collective regime (NNWS, verification system) to threshold states (Iran, Japan, South Korea). The ambiguity of 'extraordinary events' and 'supreme interests' functions as a resource that threshold states draw on for diplomatic leverage; the regime bears the cost in degraded verification and assurance.
% ABSENT_VOICES: Future proliferators and states not yet at threshold capacity — they would object to a high threshold that forecloses their future options, but they are not in the room. The North Korea precedent is structurally excluded: it cannot defend its withdrawal interpretation, yet its precedent shapes the constraint for everyone.
% DISAPPEARANCE_RATIONALE: If the withdrawal threshold interpretation vanished overnight, the NPT's exit clause would revert to unmediated textual reading — each state would judge its own 'extraordinary events.' Threshold states would gain unchallengeable exit credibility; the verification regime would lose its primary deterrent against breakout; the Security Council would lose its agreed basis for Chapter VII action. The regime would reorganize around unilateral withdrawal assessments.
% FOUNDING_PROBLEM: The NPT negotiators (1965-1968) needed an exit clause to secure ratification by sovereign states unwilling to bind themselves irreversibly, but feared an easy exit would make the treaty a 'scrap of paper.' Article X's 'extraordinary events' and 'supreme interests' language was the compromise — a threshold high enough to deter casual withdrawal, low enough to preserve sovereign prerogative.
% FOUNDING_PROBLEM_CORROBORATION: The negotiating record (UNGA First Committee 1968, ENDC/PV.410) confirms the dual intent: sovereign exit right vs regime stability. The 1995 Review and Extension Conference documentation shows parties understood the threshold as operational, not vestigial. No single party's self-assertion resolves the contest — the corroboration comes from the textual compromise itself, recorded by all negotiating parties.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate: the constraint extracts interpretive compliance from NNWS and the verification regime (they must operate under threshold uncertainty) while transferring exit-option value to threshold states. Suppression (0.52) is moderate: no state is physically prevented from withdrawing, but the diplomatic, legal, and supply consequences of a contested withdrawal suppress unilateral action. Theater ratio (0.38) reflects that Security Council resolutions and IAEA board statements perform high-threshold enforcement while the underlying ambiguity persists — the performance of regime stability exceeds its substance. Accessibility collapse (0.42) is partial: alternative interpretations (collective determination, self-judging, hybrid) remain live. Resistance (0.58) is significant: NNWS coalitions (NAM, NAC) actively resist high-threshold interpretations that constrain their sovereign exit right.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS/agenda-setter seat, the constraint is a rope — genuine coordination preventing regime collapse. From the threshold-state/beneficiary seat, it is a low-extraction coordination that preserves their sovereign option. From the NNWS/payer seat, it is a snare — they bear the costs of ambiguity (degraded verification, regional instability) without the exit-option benefit. The engine computes this divergence from the structural data: beneficiaries (threshold_states) have constrained exit but low power; payers (NNWS, verification_regime) have organized power but constrained exit; agenda_setters (NWS, UNSC) have institutional power and arbitrage exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states are structural beneficiaries: they collect exit-option credibility from the ambiguity without paying the regime-stability costs. Their d is low (~0.25). NWS are agenda-setters with arbitrage exit (they can interpret, enforce, or ignore — d ~0.35). NNWS are payers: they bear verification degradation and assurance erosion with constrained exit — d ~0.75. Verification regime is a payer with analytical exit — it absorbs degradation but cannot leave — d ~0.80. North Korea precedent is excluded: trapped, powerful, but voiceless in the interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sovereign exit vs regime stability) remains contested — not dead, not live in a settled sense. The constraint has not atrophied into a piton because the ambiguity is actively maintained: threshold states need it, NWS contest it, NNWS resist it. The theater ratio rise (0.15→0.38) reflects increasing performative enforcement (resolutions, statements) over substantive threshold clarification — but the constraint still coordinates the baseline expectation that withdrawal is exceptional, not routine. Mandatrophy is unresolved: the original compromise persists because no party can force a authoritative interpretation without risking the treaty's survival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_identity,
    'Is this constraint a distinct reading of the npt_treaty_text kernel, or does it collapse into the nws_reading or nnws_reading?',
    'Structural decomposition: if the withdrawal threshold interpretation has beneficiaries, victims, and coordination/extraction dynamics distinct from the disarmament/non-proliferation bargain readings, it is a separate constraint. The ε-invariance test: does changing the observable (withdrawal threshold vs disarmament compliance) change ε? Yes — withdrawal threshold ε is moderate (0.48); disarmament compliance ε is higher for NNWS, lower for NWS.',
    'If not a distinct reading, the withdrawal threshold should be modeled as a sub-constraint of nws_reading or nnws_reading. If distinct, it requires its own classification and network links to the sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Whether withdrawal_threshold_reading is a structurally independent constraint within the npt_treaty_text kernel').

omega_variable(
    north_korea_precedent_legal_effect,
    'Does North Korea''s 2003 withdrawal establish a customary international law precedent lowering the Article X threshold, or was it a unique case with no precedential value?',
    'ICJ advisory opinion or widespread state practice convergence. Absent that, the precedent remains in the contested zone — cited by all sides, binding on none.',
    'If precedential, the low-threshold reading gains legal weight and the constraint shifts toward rope (coordination around a clarified lower threshold). If non-precedential, the ambiguity persists and extraction continues to flow to threshold states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(north_korea_precedent_legal_effect, empirical, 'Legal status of the sole Article X invocation in state practice').

omega_variable(
    threshold_state_coordination_extraction_boundary,
    'Is the exit-option credibility that threshold states derive from Article X ambiguity a genuine coordination benefit (deterrence stability) or pure extraction (diplomatic leverage without security value)?',
    'Counterfactual analysis: if Article X had a clarified high threshold, would threshold states'' security diminish? If yes, coordination function is real. If no — they only lose diplomatic leverage — the benefit is extractive.',
    'If pure extraction, the constraint reclassifies toward snare for the threshold-state seat. If genuine coordination, tangled_rope holds. The engine''s Boltzmann analysis will test this via cross_index_coupling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_coordination_extraction_boundary, conceptual, 'Whether the threshold-state benefit is coordination or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_withdrawal_tr_t1968, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(npt_withdrawal_tr_t1995, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(npt_withdrawal_tr_t2003, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2003, 0.35).
narrative_ontology:measurement(npt_withdrawal_tr_t2015, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2015, 0.33).
narrative_ontology:measurement(npt_withdrawal_tr_t2024, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(npt_withdrawal_be_t1968, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1968, 0.22).
narrative_ontology:measurement(npt_withdrawal_be_t1995, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(npt_withdrawal_be_t2003, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2003, 0.45).
narrative_ontology:measurement(npt_withdrawal_be_t2015, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(npt_withdrawal_be_t2024, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(npt_withdrawal_su_t1968, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1968, 0.3).
narrative_ontology:measurement(npt_withdrawal_su_t1995, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(npt_withdrawal_su_t2003, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2003, 0.55).
narrative_ontology:measurement(npt_withdrawal_su_t2015, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(npt_withdrawal_su_t2024, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__withdrawal_threshold_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).

% DUAL FORMULATION NOTE:
% NPT treaty text kernel decomposes into three constraint stories: nws_reading (non-proliferation binding on NNWS), nnws_reading (disarmament binding on NWS), withdrawal_threshold_reading (Article X threshold contest). This reading's ε (0.48) differs from nws_reading (ε~0.35 for NNWS) and nnws_reading (ε~0.65 for NWS) because the referent differs: withdrawal threshold vs disarmament compliance vs non-proliferation compliance. All three share the same treaty text but instantiate different constraints with different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, moderate, 0.25).
constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, institutional, 0.35).
constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
