% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: NPT Article X Withdrawal Threshold — Sovereignty-Preservation Reading
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This constraint isolates ONE reading of the NPT's Article X withdrawal
 *   clause — the sovereignty-preservation reading, which treats the low,
 *   largely self-certified withdrawal threshold as the operative legal
 *   standard. Under this reading, a state's 90-day notice citing
 *   'extraordinary events jeopardizing its supreme interests' is not subject
 *   to binding external adjudication; the 2003 DPRK withdrawal, never
 *   formally reversed or penalized to a definitive legal conclusion by the
 *   Security Council, stands as the load-bearing precedent. This reading
 *   structurally favors threshold states (Iran being the clearest present
 *   case) by preserving a credible, low-cost exit option they can hold in
 *   reserve as leverage. The sibling reading (regime-stability priority,
 *   treating withdrawal as requiring something closer to external
 *   adjudication or at minimum unanimous non-objection) is a DIFFERENT
 *   constraint with a different epsilon and different victim set — it is not
 *   represented here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.52).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.58).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold — Sovereignty-Preservation Reading").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '9eddb0bf-53e2-4db3-a943-ab01014c76f4').
narrative_ontology:cs_kernel_codification('9eddb0bf-53e2-4db3-a943-ab01014c76f4', fixed_text).
narrative_ontology:cs_authority_grounding('9eddb0bf-53e2-4db3-a943-ab01014c76f4', distributed).
narrative_ontology:cs_reading_relation('9eddb0bf-53e2-4db3-a943-ab01014c76f4', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('9eddb0bf-53e2-4db3-a943-ab01014c76f4', npt_treaty_text__nnws_reading, influences).
narrative_ontology:cs_axiom('9eddb0bf-53e2-4db3-a943-ab01014c76f4', foundational, self_certified_national_interest_determination_is_sufficient).
narrative_ontology:cs_axiom_status(self_certified_national_interest_determination_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('9eddb0bf-53e2-4db3-a943-ab01014c76f4', self_certified_national_interest_determination_is_sufficient, conventional).
narrative_ontology:cs_axiom('9eddb0bf-53e2-4db3-a943-ab01014c76f4', secondary, sovereignty_over_existential_security_decisions_is_non_derogable).
narrative_ontology:cs_axiom_status(sovereignty_over_existential_security_decisions_is_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('9eddb0bf-53e2-4db3-a943-ab01014c76f4', sovereignty_over_existential_security_decisions_is_non_derogable, deontological).
narrative_ontology:cs_reference_frame('9eddb0bf-53e2-4db3-a943-ab01014c76f4', id_1968_negotiated_accession_bargain).
narrative_ontology:cs_drift_state('9eddb0bf-53e2-4db3-a943-ab01014c76f4', post_dprk_withdrawal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9eddb0bf-53e2-4db3-a943-ab01014c76f4', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, withdrawal_precedent_beneficiaries).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, non_proliferation_regime_architects).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, iaea_verification_apparatus).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, neighboring_non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with advanced but not yet weaponized nuclear programs (Iran being the paradigm case, with Japan and South Korea cited as latent capacity holders) benefit from Article X's low-threshold reading because it preserves a credible exit option: a 90-day notice citing 'extraordinary events jeopardizing supreme national interests' with no external adjudication of that judgment. The 2003 DPRK withdrawal, never formally contested to a binding conclusion by the Security Council, sets the operative precedent that self-certified withdrawal survives. Retaining this exit option is itself a bargaining asset in negotiations over sanctions relief, security guarantees, and enrichment rights — the option's value lies partly in never being exercised.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states, beneficiary,
    moderate, generational, constrained, national).

% Not an actor but the standing legal precedent itself — the 2003 DPRK withdrawal's lack of formal international legal consequence functions as a durable structural asset for any future withdrawing state. Listed for completeness; it collects no rents but its persistence is what threshold states actually rely upon.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, withdrawal_precedent_beneficiaries, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_text__withdrawal_threshold_reading, withdrawal_precedent_beneficiaries).

% The P5 depositary states and the broader coalition that built the 1968 bargain bear the cost of an unenforceable withdrawal clause: every ambiguous exit erodes the credibility of the regime's central promise (non-proliferation in exchange for eventual disarmament and peaceful-use access). They cannot rewrite Article X without reopening the entire treaty to renegotiation, which most calculate as more dangerous than living with the ambiguity. Their leverage is diplomatic and economic (sanctions, Security Council referral) rather than textual — they cannot compel a higher threshold through interpretation alone.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, non_proliferation_regime_architects, payer,
    institutional, generational, constrained, global).

% The IAEA safeguards system bears the direct operational cost of an ambiguous withdrawal threshold: when a state withdraws, verification access ends immediately and the agency loses visibility into facilities it may have been monitoring for years, with no formal mechanism to complete an investigation begun pre-withdrawal (as occurred with the DPRK). The agency has no vote on treaty text and no capacity to impose a higher threshold; it can only report the fact of withdrawal to the Security Council, which may or may not act.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, iaea_verification_apparatus, payer,
    institutional, biographical, trapped, global).

% States geographically proximate to a threshold state (South Korea and Japan relative to North Korea; Gulf states and Israel relative to Iran) absorb the security externality of low-threshold withdrawal without having any say in the withdrawing state's internal 'supreme national interest' determination. Their exit options are constrained to their own alliance-seeking or counter-proliferation responses, which they must undertake regardless of what the withdrawing state's legal justification was.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, neighboring_non_nuclear_states, payer,
    moderate, biographical, constrained, regional).

% Formally the body to which withdrawal notice and IAEA non-compliance reports are referred, holding in principle the power to treat a self-certified withdrawal as a threat to international peace and security. In practice, P5 veto dynamics (China and Russia's reluctance to escalate against the DPRK; divided views on Iran) mean the Council has never converted a withdrawal dispute into binding threshold-setting precedent, leaving the low-threshold reading operative by default rather than by affirmation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, un_security_council, agenda_setter,
    institutional, immediate, arbitrage, global).

% Analyze the gap between Article X's text (a low bar: self-judged extraordinary events, three months' notice) and the regime-stability function the drafters plausibly intended, without power to resolve the ambiguity themselves. They document precedent and structural drift for policymakers and future negotiators.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, arms_control_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__withdrawal_threshold_reading, diffuse).
narrative_ontology:fixing_cost_class(npt_treaty_text__withdrawal_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article X coordinates an exit valve into the treaty design itself: without SOME withdrawal mechanism, no sovereign state would have joined a permanent, non-negotiable commitment restricting its security options indefinitely. The clause exists to make initial accession possible by promising an escape hatch.
% TRANSFER_FUNCTION: The low-threshold reading transfers negotiating leverage and strategic ambiguity FROM the collective non-proliferation regime and its verification apparatus TO individual threshold states, who can hold credible exit as a bargaining chip while remaining nominally inside the treaty, and TO no one in particular from the regime's degraded deterrent value — the cost is diffused across every remaining member state's confidence in the bargain.
% ABSENT_VOICES: The IAEA itself has no seat at the interpretive table for what counts as an 'extraordinary event' — it can only report facts after the fact. Populations of neighboring states bearing the security externality have no forum to contest a withdrawing state's self-certified justification. Future generations who would live under a further-proliferated regime are not represented in any present negotiation.
% DISAPPEARANCE_RATIONALE: If the low-threshold reading of Article X disappeared — replaced overnight by a binding, externally-adjudicated high threshold — threshold states would lose a credible unilateral exit option, altering the calculus behind years of negotiating posture (Iran's JCPOA-era brinkmanship, the DPRK's original 2003 move). The regime-stability architects would gain leverage they currently lack; the practical value of 'the option to threaten withdrawal' as a bargaining tool would collapse, reshaping ongoing diplomatic dynamics substantially.
% FOUNDING_PROBLEM: In 1968, no sovereign state would ratify a treaty permanently foreclosing its right to acquire nuclear weapons without some guaranteed exit if circumstances changed catastrophically — Article X was the price of getting near-universal accession at all, particularly from states wary of ceding sovereignty over existential security decisions.
% FOUNDING_PROBLEM_CORROBORATION: Threshold states and their legal advocates attest the founding problem (sovereignty preservation against an indefinite, non-negotiable restriction) remains fully live. Independent treaty historians and several P5 diplomats on record at NPT Review Conferences attest the clause's original 'safety valve for extraordinary circumstances' function has been stretched by the DPRK precedent into something closer to a standing strategic option — a shift acknowledged even by scholars sympathetic to sovereignty claims, and documented in UN Institute for Disarmament Research analyses independent of any state party's direct interest.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.15 at treaty inception to 0.52 by 2024, tracking the DPRK precedent's crystallization (2003) as the inflection point where the low-threshold reading went from theoretical to operationally demonstrated. Suppression (0.58) reflects the P5's structural inability to convert Security Council referral into a binding higher standard, given veto dynamics — the regime architects cannot suppress the low-threshold reading even though they bear its costs. Theater ratio (0.4) captures the substantial gap between formal IAEA reporting procedures (real activity) and their practical inability to alter outcomes once notice is given (performative residue).
 *
 * PERSPECTIVAL GAP:
 *   From a threshold state's seat, the low-threshold reading is a legitimate sovereignty safeguard exercised by the treaty's own text — this reads close to a rope (coordination that lets sovereign states join at all). From the regime-architect and IAEA seats, the same textual ambiguity is an actively-defended extraction of bargaining leverage that degrades collective security at their expense. The tangled_rope classification models this exact divergence: coordination function (accession was only possible because of Article X) coexists with asymmetric extraction (the ambiguity's costs and benefits are not evenly distributed) — this is the engine's per-seat computation, not an authored reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states are declared beneficiaries because the low-threshold reading's chief value to them is optionality, not exercise — most threshold states never actually withdraw, they simply hold the credible threat, which the engine should read as a low-d beneficiary position (subsidized bargaining power) rather than the high-extraction position a completed withdrawal would represent. Regime architects, the IAEA apparatus, and neighboring states are declared victims/payers because the erosion of the regime's deterrent credibility and the loss of verification continuity are costs borne without any corresponding say in the interpretive question.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sovereignty preservation making initial accession possible in 1968) is genuinely contested as live vs. dead: it remains live for states that view any nuclear posture restriction as an existential sovereignty question, but arguably dead or badly mutated for the broader regime, where the clause has drifted from 'safety valve for extraordinary circumstances' toward 'standing strategic option regardless of circumstance severity.' Classifying this as tangled_rope rather than snare preserves the genuine 1968 coordination function (no treaty without an exit clause) while still registering the asymmetric cost structure that has emerged since 2003 — collapsing it to snare would erase the real accession-enabling function; collapsing it to rope would erase the documented victim set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dprk_precedent_binding_force,
    'Does the 2003 DPRK withdrawal, absent a definitive Security Council or ICJ ruling on its legality, constitute binding customary interpretation of Article X''s threshold, or merely an unresolved incident that sets no formal precedent?',
    'A future contested withdrawal (e.g., a formal Iranian withdrawal notice) adjudicated to a definitive Security Council resolution or ICJ advisory opinion would resolve whether self-certification survives legal challenge or whether an external threshold test can actually be imposed.',
    'If DPRK sets binding precedent, the low-threshold reading is legally entrenched and this constraint''s extraction profile is likely to rise further; if a future case establishes a binding external test, this reading collapses toward the sibling regime-stability reading and this story''s beneficiary/victim structure would need re-authoring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dprk_precedent_binding_force, empirical, 'Whether the DPRK withdrawal is binding precedent or an unresolved incident.').

omega_variable(
    withdrawal_optionality_vs_exercise,
    'Is the value threshold states derive from Article X primarily the standing option to withdraw (bargaining leverage) or the credible prospect of actual exercise?',
    'Comparative analysis of negotiating outcomes (sanctions relief terms, enrichment concessions) correlated with withdrawal-threat timing versus actual withdrawal announcements would separate option-value from exercise-value.',
    'If value is overwhelmingly option-value, the beneficiary classification (low d, subsidized leverage) is well-grounded; if states are trending toward actual exercise, directionality should shift toward a more symmetric or even victim-adjacent reading for the withdrawing state itself (post-withdrawal isolation costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_optionality_vs_exercise, empirical, 'Whether threshold-state benefit is chiefly the unexercised option or actual withdrawal capacity.').

omega_variable(
    kernel_framing_disaggregation,
    'Is the withdrawal-threshold contest genuinely a distinct kernel axis from the VI/X disarmament-vs-restraint contest, or are they entangled because the same states (Iran, DPRK) figure centrally in both?',
    'Trace whether withdrawal-threshold disputes and disarmament-obligation disputes move together in negotiating history (e.g., do NNWS disarmament grievances correlate with withdrawal-threat frequency) or vary independently across different state pairs.',
    'If entangled, treating them as fully separate constraints (as this decomposition does) risks under-counting a shared causal driver; if independent, the decomposition is clean and each story''s epsilon stands alone as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_disaggregation, conceptual, 'Whether the withdrawal-threshold and disarmament-obligation kernel readings are structurally independent or share an unmodeled common driver.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(npt__tr_t2003, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2003, 0.35).
narrative_ontology:measurement(npt__tr_t2012, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2012, 0.38).
narrative_ontology:measurement(npt__tr_t2018, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2018, 0.4).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1985, 0.2).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(npt__be_t2003, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2003, 0.45).
narrative_ontology:measurement(npt__be_t2012, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2012, 0.48).
narrative_ontology:measurement(npt__be_t2018, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2018, 0.5).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1985, 0.32).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(npt__su_t2003, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2003, 0.55).
narrative_ontology:measurement(npt__su_t2012, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2012, 0.56).
narrative_ontology:measurement(npt__su_t2018, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2018, 0.57).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the colloquial label 'the NPT' under the npt_treaty_text kernel. nws_reading and nnws_reading address the Article VI disarmament-vs-restraint axis; this story addresses only the Article X withdrawal-threshold axis. The withdrawal-threshold reading interacts with both disarmament readings insofar as withdrawal credibility functions as leverage in disarmament-obligation disputes (an NNWS threatening withdrawal to pressure NWS disarmament compliance), but each story retains its own epsilon, beneficiary/victim set, and classification per the epsilon-invariance principle — they are linked, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
