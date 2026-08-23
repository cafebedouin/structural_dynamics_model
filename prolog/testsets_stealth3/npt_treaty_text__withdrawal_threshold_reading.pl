% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: NPT Article X Withdrawal Threshold — Regime-Stability vs. Sovereignty Reading
 *   domain: international law / arms control / treaty interpretation
 *
 * SUMMARY:
 *   Article X of the Non-Proliferation Treaty lets a party withdraw on three
 *   months' notice if 'extraordinary events... have jeopardized the supreme
 *   interests of its country' — a self-judged, substantively unreviewable
 *   exit as drafted. Since the DPRK's 2003 withdrawal, the operative
 *   threshold is unsettled: the regime's practice (Security Council handling
 *   of the DPRK, IAEA continuity assertions, review-conference language
 *   fights) asserts a substantive, justiciable standard, while the treaty
 *   text and the sovereignty-preservation pole hold exit to unilateral
 *   judgment. This story authors that standing arrangement — the maintained
 *   ambiguity of the exit threshold — with the North Korean precedent as the
 *   case that made the ambiguity operative and the threshold states' hedge as
 *   the value it now prices. The arrangement is a genuine coordination device
 *   (the exit valve made near-universal accession possible and still holds
 *   security-anxious states inside) that also carries an asymmetric cost
 *   structure (unpriced exit risk falls on states that cannot use the door,
 *   while the option value accrues to states with latent capability). Claim
 *   and metrics are authored independently: claimed tangled_rope; the metrics
 *   describe moderately extractive operation whose enforcement machinery has
 *   intensified since 2003.
 *
 * KEY AGENTS:
 *   - threshold_latent_states: Primary beneficiary (powerful/arbitrage) — hold compliance plus a credible, never-settled exit threat; Iran most prominently, with the Japan/South Korea/Germany latent-capability class behind them
 *   - npt_nuclear_weapon_states: Beneficiary and agenda-setter (institutional/arbitrage) — as P5 they decide the collective response to any withdrawal notice and have never accepted a binding rule on what withdrawal does
 *   - fully_committed_nnws: Primary payer (organized/trapped) — the committed majority bearing the regime-fragility risk the ambiguity generates
 *   - disarmed_former_proliferators: Payer (moderate/trapped) — South Africa, Ukraine, Kazakhstan: completed restraint repriced downward by the standing exit option
 *   - iaea_safeguards_system: Payer (institutional/constrained) — verification architecture any party can dissolve by notice letter
 *   - nonparty_nuclear_states: Excluded (powerful/arbitrage) — India, Pakistan, Israel: outside the conversation, inside its logic
 *   - un_office_of_legal_affairs: Analytical observer (institutional/analytical) — documents that no authoritative ruling on withdrawal's effect exists
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.58).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.58).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold — Regime-Stability vs. Sovereignty Reading").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international law / arms control / treaty interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, 'aa153892-32eb-4cab-9adc-c3ca3e900348').
narrative_ontology:cs_kernel_codification('aa153892-32eb-4cab-9adc-c3ca3e900348', fixed_text).
narrative_ontology:cs_authority_grounding('aa153892-32eb-4cab-9adc-c3ca3e900348', distributed).
narrative_ontology:cs_reading_relation('aa153892-32eb-4cab-9adc-c3ca3e900348', npt_treaty_text__nws_reading, influences).
narrative_ontology:cs_reading_relation('aa153892-32eb-4cab-9adc-c3ca3e900348', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_axiom('aa153892-32eb-4cab-9adc-c3ca3e900348', foundational, withdrawal_threshold_is_operative_hinge).
narrative_ontology:cs_axiom_status(withdrawal_threshold_is_operative_hinge, holdable).
narrative_ontology:cs_axiom_grounding('aa153892-32eb-4cab-9adc-c3ca3e900348', withdrawal_threshold_is_operative_hinge, conventional).
narrative_ontology:cs_axiom('aa153892-32eb-4cab-9adc-c3ca3e900348', foundational, exit_ambiguity_maintains_accession_equilibrium).
narrative_ontology:cs_axiom_status(exit_ambiguity_maintains_accession_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('aa153892-32eb-4cab-9adc-c3ca3e900348', exit_ambiguity_maintains_accession_equilibrium, instrumental).
narrative_ontology:cs_reference_frame('aa153892-32eb-4cab-9adc-c3ca3e900348', self_judging_supreme_interests_clause).
narrative_ontology:cs_drift_state('aa153892-32eb-4cab-9adc-c3ca3e900348', post_dprk_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('aa153892-32eb-4cab-9adc-c3ca3e900348', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_latent_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, npt_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, fully_committed_nnws).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, disarmed_former_proliferators).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, iaea_safeguards_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate enrichment or reprocessing-capable programs (Iran most prominently; Japan, South Korea, and Germany hold the industrial base to move quickly) while remaining inside the treaty. Their compliance is priced: they trade enrichment limits and inspection access for sanctions relief, technology, and security assurances, and they keep a credible exit threat in reserve because no one has settled whether leaving is legally clean. The never-settled door is worth more to them than a settled one would be in either direction.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_latent_states, beneficiary,
    powerful, biographical, arbitrage, regional).

% Hold nuclear arsenals outside the treaty's renunciation and, as permanent Security Council members, decide how the collective responds when any party serves a withdrawal notice. Since the DPRK's 2003 exit they have answered ad hoc — sanctions, resolutions asserting obligations continue — and have never accepted a binding rule on what withdrawal does or triggers. The unresolved threshold preserves that case-by-case freedom; they also carry the risk that a regime members can leave on short notice invites new proliferators near their interests.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, npt_nuclear_weapon_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, npt_nuclear_weapon_states, agenda_setter).

% The treaty's broad majority — states that renounced the weapons option without the industrial base to reconsider quickly. They cannot credibly threaten exit: leaving would cost them the peaceful-cooperation benefits and gain them no breakout capability. They carry the security risk that a neighboring or rival party converts compliance into weapons on three months' notice, and at review conferences they push for language making withdrawal costly — so far without a binding result.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, fully_committed_nnws, payer,
    organized, generational, trapped, global).

% South Africa dismantled its arsenal; Ukraine and Kazakhstan surrendered Soviet weapons in exchange for membership, assurances, and integration. Their completed restraint is the arrangement's proof of concept, and it is repriced every year the exit door stays open: the assurances they accepted are backed by a regime any member can leave on notice. Ukraine's post-2014 position, citing the Budapest Memorandum's failure, is the sharpest statement of this exposure.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, disarmed_former_proliferators, payer,
    moderate, generational, trapped, regional).

% Runs the inspection architecture that gives the treaty its verification reach, under agreements any party can dissolve by notice letter. The DPRK case left the status of a withdrawing state's safeguards agreement formally unresolved — the board found continued obligations, the agency's access ended anyway. Every year the threshold stays unsettled, the system carries the risk that its legal basis in any given state can be switched off unilaterally.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, iaea_safeguards_system, payer,
    institutional, generational, constrained, global).

% India, Pakistan, and Israel never joined; their arsenals are the standing demonstration that the restraint the treaty sells is conditional and that staying outside is survivable. They are absent from the conferences and councils where the withdrawal threshold is argued, yet the fact of their arsenals is what gives the threshold question its force — every argument about exit is shadowed by the example of states that never entered.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nonparty_nuclear_states, excluded,
    powerful, generational, arbitrage, regional).

% Produces legal analyses of withdrawal's effect and of safeguards continuity, and its memoranda are the clearest record that no authoritative ruling exists: the office can describe the competing positions but holds no seat that settles them. It observes the dispute it cannot adjudicate.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, un_office_of_legal_affairs, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__withdrawal_threshold_reading, threshold_latent_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__withdrawal_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the accession-credibility problem: sovereign states facing unpredictable security futures will not accept perpetual renunciation without an escape, and the Article X valve converted a treaty most security-anxious states would have refused into one nearly every state joined. It also lets states under extended deterrence or regional threat join while holding a conditional door open, which is what makes their inspections and cooperation agreements possible at all.
% TRANSFER_FUNCTION: Moves option value — a credible, never-settled threat of nuclear breakout — to states with latent capability, financed by the committed majority's security: every year the threshold stays unsettled converts the fully committed states' restraint into cheaper hedging for the threshold minority, and reprices downward the completed disarmament of former proliferators. A secondary flow runs to the P5, who collect response flexibility they have never had to define or bound.
% ABSENT_VOICES: The disarmed states and the fully committed majority have the weakest seats: they bear the arrangement's risk but cannot price it — no mechanism exists by which a state that gave up arsenals or options charges the regime for the exit risk it now carries. Nonparty nuclear states are outside the conversation entirely, though their arsenals are the fact that gives the threshold its teeth. Review-conference procedure gives every party a voice but gives none of the risk-bearers a vote that binds; their organized bloc pressure produces language, never rules.
% DISAPPEARANCE_RATIONALE: If the withdrawal clause vanished overnight, the accession calculus breaks: states facing security uncertainty would refuse perpetual commitment or demand renegotiation; threshold states would either harden into open hedging or head for the exits; the near-universal membership that gives the regime its verification reach would fragment. The clause is load-bearing, not vestigial — its removal rearranges who is inside the regime, on what terms, and with what inspection coverage.
% FOUNDING_PROBLEM: The 1968 negotiators needed to make a permanent renunciation of nuclear weapons acceptable to sovereign states facing unpredictable security futures; the 'supreme interests' exit clause was the price of accession, drafted so that security-anxious states could join without surrendering the sovereign judgment that circumstances might someday force them out.
% FOUNDING_PROBLEM_CORROBORATION: The 1968 Eighteen-Nation Committee negotiating record and the depositaries' contemporaneous statements attest the founding problem, but the strongest corroboration comes from outside the benefiting parties: India, Pakistan, and Israel — nonparty states — have repeatedly given sovereignty-and-conditionality reasons for never acceding, and the treaty-law literature treating the supreme-interests clause as a genuine sovereignty safeguard rather than a drafting artifact is extensive. Ukraine's post-2014 statements corroborate the other side of the founding bargain: that the exit door's openness now devalues the assurances once given.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate (0.58 at interval end) because the arrangement's cost — regime fragility and unpriced neighbor-breakout risk — is real but diffuse, and its benefit — hedge value — accrues to a minority with latent capability. Suppression (0.58) is the active maintenance of interpretive non-crystallization: the machinery that keeps the threshold unsettled (Security Council case-by-case responses, IAEA continuity assertions, review-conference text that restates rather than resolves) has intensified since 2003. Suppression is authored as a raw structural property — the enforcement machinery's intensity — and is not scaled by any context dimension; extractiveness is what the engine scales by directionality and scope. Theater is moderate (0.48): a growing share of the review process's withdrawal output is restated ambiguity rather than settlement. Accessibility collapse is moderate-low (0.40): the alternatives — a binding consequences doctrine, collective security assurances for committed states, safeguards-continuity protocols — remain visible and partly pursued; the ambiguity outcompetes them rather than erasing them. Resistance is substantial (0.60): committed-state blocs push for settlement at every review conference, and threshold states resist crystallization in either direction. The three measurement series share one time grid (0, 10, 20, 33, 45, 55 — 1970 to 2025), with suppression_requirement authored because the story specifically tracks enforcement intensification (the post-DPRK ratchet), not merely extraction drift. All three metrics rise together after 2003: the ambiguity becomes the operative fact, the enforcement needed to keep it unsettled grows, and the review process's performative share grows with it.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from the same text. From a threshold state's position the clause is a sovereignty guarantee — the valve that made accession rational and keeps its restraint conditional rather than surrendered. From a fully committed NNWS position the same clause is a standing defection license held by better-armed neighbors, and the organized bloc's inability to convert review-conference majorities into binding rules is the structural fact that keeps them exposed. From the P5 seat it is flexibility: no binding rule on withdrawal consequences has ever been accepted, so every exit can be answered ad hoc. The engine computes these per-seat types from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: threshold_latent_states (hedge value accrues to them directly; arbitrage-grade exploitation of the text/practice gap places them near the beneficiary end of d) and npt_nuclear_weapon_states (the unsettled threshold preserves case-by-case response flexibility — they collect flexibility rather than rents, so their d sits low but not at the pure-beneficiary pole, since they also carry regime-erosion risk). Payers: fully_committed_nnws (bear breakout risk they cannot hedge — trapped, high d), disarmed_former_proliferators (their completed restraint is repriced downward every year the door stays credible — trapped, high d), iaea_safeguards_system (its verification architecture is hostage to the unresolved continuity question — high d). No directionality_overrides were authored: the beneficiary/victim-plus-exit derivation places every seat correctly, and an override keyed on the institutional power atom would corrupt one of the two institutional seats — the P5 (beneficiary, low d) and the IAEA (payer, high d) share that atom with opposite structural relationships, so a per-atom override cannot serve both.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents both mislabels. A pure-rope reading would miss the asymmetric cost structure: the valve's option value is collected by the states positioned to use it, while its risk is carried by states that cannot — and that asymmetry has grown since 2003. A pure-snare reading would miss the coordination function: the exit clause is why the treaty achieved near-universal membership at all, and the maintained ambiguity still performs accession-equilibrium work — settling it in either direction would break something (a high settlement destroys the hedge that keeps threshold states inside; a low settlement spikes the committed majority's exposure). The founding problem — sovereign states will not accept perpetual renunciation without a supreme-interest escape — is live, so no mandatrophy is declared: the arrangement still does what it was built for, and has simply added an extraction layer on top since 2003. The temporal series shows that addition rather than atrophy: extraction and enforcement rising together is accumulation, not decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    withdrawal_threshold_kernel_reading,
    'This constraint is one reading of the npt_treaty_text kernel — the reading that centers the Article X exit threshold as the regime''s operative hinge. What would change structurally if the story were authored from the nws_reading or the nnws_reading instead?',
    'Author the sibling stories and compare: under nws_reading the threshold is an enforcement barrier on NNWS (epsilon keyed to constraint strength, NWS as beneficiaries); under nnws_reading the threshold is the NNWS bloc''s leverage against NWS disarmament defaults (exit threats as bargaining chips). Compare epsilon, beneficiary structure, and computed types across the three files.',
    'Under nws_reading a high threshold reads as coordination-serving and a low one as extraction; under nnws_reading the polarity partially inverts. This reading''s moderate tangled_rope profile is specific to centering the exit door itself — the disagreement is located in which element of the bargain is load-bearing, and the classification is not portable across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(withdrawal_threshold_kernel_reading, conceptual, 'Kernel-level reading contest: which element of the NPT bargain is the operative constraint.').

omega_variable(
    dprk_withdrawal_legal_effect,
    'Is the DPRK''s 2003 withdrawal legally effective — extinguishing its safeguards agreement and treaty obligations — or ineffective, leaving obligations intact notwithstanding exit?',
    'An authoritative ruling: an ICJ advisory opinion, a binding Security Council determination, or an accepted member-state consensus on safeguards continuity. Absent that, state practice and the IAEA board''s DPRK findings remain the only evidence, and they point in both directions.',
    'If effective, Article X is a real exit valve and the arrangement''s cost is mostly option-value leakage; if ineffective, the regime asserts obligations without a mechanism — the enforcement gap widens, the payer seats'' exposure deepens, and the computed type drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dprk_withdrawal_legal_effect, empirical, 'The unresolved legal effect of the DPRK withdrawal precedent.').

omega_variable(
    threshold_position_capability_coupling,
    'Are states'' positions on the withdrawal threshold principled (regime stability versus sovereignty) or positional — tracking latent capability, with threshold states favoring ambiguity and committed states favoring closure?',
    'Compare stated positions against capability profiles across review-conference records; look for states whose position changed when their capability changed (post-withdrawal South Africa, states acquiring enrichment capability, states that lost extended-deterrence confidence).',
    'If positions track capability, the high/low debate is cover and the ambiguity functions as a hedge-subsidy scheme — the coordination story thins and the computed type drifts toward snare. If genuinely principled, the tangled_rope reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_position_capability_coupling, empirical, 'Whether the threshold debate is principled or positional.').

omega_variable(
    threshold_settlement_classification_swing,
    'If the threshold were authoritatively settled — high (substantive justiciable standard) or low (pure sovereignty) — would the arrangement''s extraction fall or rise relative to the maintained ambiguity?',
    'Counterfactual analysis of both settlement branches: a high settlement destroys threshold-state hedge value (preemptive exits or refused accession become rational); a low settlement spikes committed-state exposure. Model epsilon under each branch and compare against the ambiguous status quo.',
    'If either settlement lowers system-wide cost, the ambiguity itself is the extractive element and the tangled_rope classification is confirmed. If both settlements raise it, the ambiguity is load-bearing coordination and the type drifts toward rope — the maintained contest, not either pole, would be doing the work.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_settlement_classification_swing, conceptual, 'Whether the maintained ambiguity is the cost or the coordination.').

omega_variable(
    hedge_value_vs_compliance_drivers,
    'Is exit-option credibility actually the binding benefit for threshold states, or do security assurances and Article IV technology access dominate their compliance calculus?',
    'Revealed-preference analysis: threshold-state behavior when exit threats were made (Iran through the 2000s-2010s, DPRK before 2003) versus when assurances or technology access were at stake; differential compliance under sanctions relief tied to enrichment limits.',
    'If the hedge is marginal, the beneficiary structure thins — the arrangement''s value has no concentrated receiver, and the story reads closer to a rope with a leaky valve. If the hedge dominates, the accrual of value to threshold states is confirmed as structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hedge_value_vs_compliance_drivers, empirical, 'What actually buys threshold-state compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(npt__tr_t10, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(npt__tr_t20, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(npt__tr_t33, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 33, 0.3).
narrative_ontology:measurement(npt__tr_t45, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement(npt__tr_t55, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 55, 0.48).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(npt__be_t10, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(npt__be_t20, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(npt__be_t33, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 33, 0.48).
narrative_ontology:measurement(npt__be_t45, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(npt__be_t55, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 55, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(npt__su_t10, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(npt__su_t20, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(npt__su_t33, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 33, 0.45).
narrative_ontology:measurement(npt__su_t45, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(npt__su_t55, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 55, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, nnws_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the NPT bargain' decomposes into three structurally distinct constraints with different epsilon, per the epsilon-invariance principle: nws_reading (restraint imposed on NNWS, NWS disarmament aspirational), nnws_reading (conditional restraint purchasing NWS compliance under Article VI), and this file — the Article X withdrawal threshold as the regime's exit barrier. This reading is upstream of nws_reading: the exit barrier's strength conditions how much restraint the NWS reading can enforce, which is why the edge runs from this reading to nws_reading. The nnws_reading coexists with this one: the same state blocs hold both the threshold contest and the Article VI bindingness claim simultaneously, and neither premise rules the other out. Each file carries its own epsilon, beneficiary structure, and claimed type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
