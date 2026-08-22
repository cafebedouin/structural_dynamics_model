% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: NPT Article X Withdrawal Threshold — Ambiguous Self-Judging Reading
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   Article X of the Non-Proliferation Treaty grants any party a right to
 *   withdraw on three months' notice if it decides that extraordinary events
 *   related to the treaty's subject matter have jeopardized its supreme
 *   interests. Nobody has ever adjudicated what qualifies. This story
 *   instantiates ONE reading of the NPT treaty-text kernel — the
 *   withdrawal_threshold_reading — under which the regime's operative force
 *   runs through exactly that unadjudicated threshold. The standing
 *   arrangement under contest (the ε referent, assessed by this reading's own
 *   lights) is the ambiguous self-judging exit clause as it has actually
 *   operated: a real coordination protocol for lawful departure, wrapped
 *   around an extraction channel through which threshold states convert exit
 *   credibility into concessions while compliant parties bear the devaluation
 *   of their own restraint. The 2003 withdrawal demonstrated that exit is
 *   survivable; the subsequent decade demonstrated that the threat of exit is
 *   profitable. Sibling readings (nws_reading, nnws_reading) are separate
 *   constraints with their own ε values and are linked, not averaged, here.
 *   The claim/metrics gap is deliberate: the arrangement is CLAIMED as
 *   tangled_rope while the authored metrics describe moderately extractive,
 *   actively-but-discretionarily enforced operation — the engine measures the
 *   divergence. KEY AGENTS (by structural relationship): -
 *   npt_threshold_states: Primary beneficiary (moderate/constrained) — holds
 *   the credible exit card, converts ambiguity into concessions -
 *   p5_nuclear_weapon_states: Agenda setter and secondary beneficiary
 *   (institutional/arbitrage) — administers case-by-case withdrawal
 *   treatment, profits from uncoded discretion -
 *   fully_compliant_nnws_parties: Primary payer (moderate/constrained) —
 *   bears verification burden and devalued restraint -
 *   withdrawal_neighbor_states: Dual-positioned payer/beneficiary
 *   (powerful/identity_locked) — absorbs the demonstrated exit's insecurity
 *   while keeping their own hedge alive - iaea_secretariat: Administrative
 *   observer (institutional/analytical) — verifies and reports, cannot
 *   adjudicate - nonparty_nuclear_armed_states: Excluded voice
 *   (powerful/mobile) — outside the treaty, shaped by its interpretation
 *   without consent
 *
 * KEY AGENTS:
 *   - npt_threshold_states: primary beneficiary (moderate/constrained) — exit-card holder converting ambiguity into leverage
 *   - p5_nuclear_weapon_states: agenda setter, secondary beneficiary (institutional/arbitrage) — discretionary enforcement authority
 *   - fully_compliant_nnws_parties: primary payer (moderate/constrained) — restraint devalued by each demonstrated exit
 *   - withdrawal_neighbor_states: payer with beneficiary secondary role (powerful/identity_locked) — DPRK-precedent front line
 *   - iaea_secretariat: observer (institutional/analytical) — verification without adjudication authority
 *   - nonparty_nuclear_armed_states: excluded (powerful/mobile) — affected non-parties with no seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.62).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.52).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold — Ambiguous Self-Judging Reading").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '54b9021b-8459-4be3-815e-f4a5bcc9328f').
narrative_ontology:cs_kernel_codification('54b9021b-8459-4be3-815e-f4a5bcc9328f', fixed_text).
narrative_ontology:cs_authority_grounding('54b9021b-8459-4be3-815e-f4a5bcc9328f', distributed).
narrative_ontology:cs_reading_relation('54b9021b-8459-4be3-815e-f4a5bcc9328f', npt_treaty_text__nws_reading, influences).
narrative_ontology:cs_reading_relation('54b9021b-8459-4be3-815e-f4a5bcc9328f', npt_treaty_text__nnws_reading, influences).
narrative_ontology:cs_axiom('54b9021b-8459-4be3-815e-f4a5bcc9328f', foundational, supreme_interests_qualifier_is_self_judging).
narrative_ontology:cs_axiom_status(supreme_interests_qualifier_is_self_judging, holdable).
narrative_ontology:cs_axiom_grounding('54b9021b-8459-4be3-815e-f4a5bcc9328f', supreme_interests_qualifier_is_self_judging, conventional).
narrative_ontology:cs_axiom('54b9021b-8459-4be3-815e-f4a5bcc9328f', foundational, exit_option_credibility_sustains_membership).
narrative_ontology:cs_axiom_status(exit_option_credibility_sustains_membership, holdable).
narrative_ontology:cs_axiom_grounding('54b9021b-8459-4be3-815e-f4a5bcc9328f', exit_option_credibility_sustains_membership, instrumental).
narrative_ontology:cs_reference_frame('54b9021b-8459-4be3-815e-f4a5bcc9328f', self_judging_sovereign_exit_with_supreme_interests_qualifier).
narrative_ontology:cs_drift_state('54b9021b-8459-4be3-815e-f4a5bcc9328f', contemporary_post_dprk_precedent, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('54b9021b-8459-4be3-815e-f4a5bcc9328f', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, npt_threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, p5_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, fully_compliant_nnws_parties).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, withdrawal_neighbor_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, withdrawal_neighbor_states).
narrative_ontology:constraint_vindicates(npt_treaty_text__withdrawal_threshold_reading, supreme_interests_self_judging_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_text__withdrawal_threshold_reading, sovereign_equality_reservation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains a latent weapons capability under partial safeguards and periodically signals willingness to invoke Article X when cornered diplomatically. Because the supreme-interests qualifier has never been adjudicated, the credible exit threat converts directly into negotiation leverage — sanctions relief, technology access, security assurances — without ever paying the full price of actually leaving. Its entire bargaining position depends on the threshold staying uncodified.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, npt_threshold_states, beneficiary,
    moderate, generational, constrained, regional).

% Drafted the treaty, act as depositary guardians, and decide case-by-case how each withdrawal is treated — a sanctions architecture for one withdrawing state, negotiated engagement for another. They benefit from regime stability and equally from the threshold remaining unwritten, since codification in either direction would strip away their discretionary handling. They absorb the proliferation cost whenever an exit actually succeeds.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, p5_nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, p5_nuclear_weapon_states, beneficiary).

% Accept full-scope safeguards, host inspections, and permanently forgo the weapons option. Each demonstrated cheap exit devalues the security and cooperation returns on their restraint, since the regime's protection erodes with every member who walks out armed. Exiting themselves would forfeit peaceful-cooperation benefits and invite proliferation suspicion, so they remain bound while the exit card circulates among stronger states.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, fully_compliant_nnws_parties, payer,
    moderate, generational, constrained, global).

% Live adjacent to the demonstrated withdrawal: they absorb the insecurity of a neighbor that exited and weaponized, while justifying their own hedging infrastructure — reprocessing capacity, warhead latency — by the same ambiguity that produced it. Non-nuclear identity commitments fused to an ally's extended deterrence lock their own exit option shut in practice even as its theoretical credibility anchors their regional bargaining.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, withdrawal_neighbor_states, payer,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, withdrawal_neighbor_states, beneficiary).

% Verifies compliance, monitors safeguard agreements, and reports withdrawal cases to the Security Council; it was expelled wholesale from the withdrawing state in 2009 and could do nothing but record the fact. It cannot adjudicate whether any given supreme-interests claim meets the threshold — its findings feed whatever political treatment the permanent members choose.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, iaea_secretariat, observer,
    institutional, generational, analytical, global).

% Remain entirely outside the treaty while every interpretation of its exit clause reshapes the regional balances they inhabit. They would object to any codification that universalizes obligations onto them, and observe that the regime's rules have never bound them in the first place — a standing rebuttal to claims that the arrangement rests on universal consent.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nonparty_nuclear_armed_states, excluded,
    powerful, generational, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__withdrawal_threshold_reading, npt_threshold_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__withdrawal_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared lawful-exit protocol — three months' notice, written statement of extraordinary events jeopardizing supreme interests, depositary notification — so that departures proceed through recognized formality rather than unilateral repudiation, preserving mutual membership expectations while the treaty persists.
% TRANSFER_FUNCTION: Moves bargaining concessions (sanctions relief, technology access, security assurances) from the permanent members and the compliant majority to threshold states holding the exit card; moves verification burden and foregone-option cost onto fully compliant non-nuclear parties; moves proliferation risk outward from any withdrawing state onto its neighbors.
% ABSENT_VOICES: Non-party nuclear-armed states (India, Pakistan, Israel) have no seat in any forum interpreting the clause. Advocates of codified withdrawal consequences are structurally outmaneuvered by consensus rules at review conferences, where a single objection kills language. The withdrawn state itself — whose conduct defines the operative precedent — participates in nothing and answers to no interpretation.
% DISAPPEARANCE_RATIONALE: If the ambiguous threshold arrangement vanished overnight — clause deleted, or codified in either direction — membership calculations would visibly reprice within months: threshold states would either escalate toward decision or capitulate to inspection demands, compliant parties would demand a renegotiated return on their restraint, and the permanent members would lose the case-by-case discretion that currently substitutes for a rule.
% FOUNDING_PROBLEM: The 1960s drafters needed states to accept effectively permanent non-proliferation obligations despite sovereign-equality objections. A guaranteed lawful exit — short notice, self-judged justification — was the price of broad ratification, assured publicly to skeptical legislatures and intended as a rarely-used safety valve rather than a standing strategic instrument.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by the public negotiating record (depository assurances to allied parliaments that withdrawal remained available), by international-law scholarship treating Article X as a deliberate sovereignty reservation rather than drafting debris, and by compliant-NNWS bloc statements at review conferences attesting the valve rationale. The depositories' own contemporaneous assurances corroborate the founding problem, though the depositories are now beneficiaries — the independent weight sits with the academic and non-beneficiary party record.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is moderate (0.62 at interval end) because the extraction is real but indirect: no tariff is levied, yet each unadjudicated exit threat transfers concession value to the threatener and devalues the restraint of compliers — a rent collected in bargaining space rather than currency. Suppression (0.52) is authored as a raw structural property, unscaled: the arrangement suppresses through conditional cooperation and sanctions presumption rather than direct coercion, and the engine scales only extractiveness. Theater ratio (0.54) reflects review-conference ritualism: every cycle produces 'strengthening withdrawal-clause' language that dies under consensus rules, while the actual mechanism — discretionary P5 handling — never changes. Accessibility collapse is low-moderate (0.40): understanding the constraint does not close alternatives, since exit itself remains open and non-party status remains available. Resistance is substantial (0.60): sovereignty-doctrine states actively resist codification, threshold states brandish invocation, and the compliant bloc presses (unsuccessfully) for consequence language. The temporal series run on one shared grid (T0=1995 post-indefinite-extension, T10=2003 withdrawal, T20=2015 post-agreement review-conference failure, T30=2025). The suppression_requirement series traces a genuine enforcement arc — ratchet upward as the sanctions architecture was built after the demonstrated exit, then decay as enforcement fractured along permanent-member lines — which is why it is tracked rather than left static. Extractiveness rises monotonically across the same interval: enforcement machinery was erected and then hollowed while the underlying transfer of option-value continued regardless. Base_properties values are end-state (T30) measurements.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the threshold-state seat the arrangement is strategic autonomy: an unpriced insurance policy against encirclement. From the compliant-majority seat the same clause is a slow devaluation of everything they surrendered, administered by states that will never need the exit themselves. From the permanent-member seat it is managed flexibility — the absence of a rule IS the asset. The sharpest identity-lock sits with the withdrawal_neighbor_states: Japan's non-nuclear principles fused to extended deterrence constitute an institutional identity in which exercising the exit option is unthinkable even while its theoretical credibility is load-bearing for regional bargaining. If that identity frame broke — an assurance collapse, a domestic political rupture — the neighbor seat would convert from hedged beneficiary into active threshold actor, and the constraint's beneficiary structure would shift materially. The engine derives these divergent classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: npt_threshold_states (declared beneficiary, constrained exit) derive near the beneficiary end — the constraint subsidizes them with option value. p5_nuclear_weapon_states carry agenda_setter with beneficiary secondary: they collect regime stability and discretion, damped toward but not to the beneficiary pole by the proliferation costs they absorb when exit succeeds. Victim declarations drive the opposite pole: fully_compliant_nnws_parties (trapped by cooperation dependence) sit near the full-target end — they pay in devalued restraint and cannot leave without self-harm. withdrawal_neighbor_states are genuinely dual-positioned; the derivation averages their payer insecurity against their beneficiary hedge, which is the honest reading of their situation. No directionality overrides are used: the beneficiary/victim declarations plus exit options already capture every structural relationship in this story, and the one dual-positioned agent is correctly handled by averaging rather than correction. Observers and the excluded seat take canonical fallbacks, as they should — they neither collect nor pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters here because every mislabel is politically loaded. Calling this a snare ignores the real coordination function: a lawful-exit protocol beats unilateral repudiation, and the notice-and-statement formality does genuine work in keeping departures legible. Calling it a rope ignores the identifiable extraction: gains demonstrably concentrate on the threshold-state seat while costs diffuse across the compliant majority — this is not a piton cell, because someone profits enough to defend the ambiguity, and the permanent members profit enough from discretion to block its codification. Calling it a scaffold fails on the sunset test: no sunset exists, none is imagined, and the founding problem (reconciling permanent obligation with sovereign exit) is live, not transitional — the R5 interview confirms status=live with world_rearranges, so no obsolescence mismatch fires. Tangled_rope holds both truths simultaneously: genuine coordination and asymmetric extraction through the same clause, held together by active (if discretionary) enforcement. Mandatrophy is not resolved and is not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (withdrawal_threshold_reading) of the npt_treaty_text kernel; would instantiating a sibling reading — nws_reading (binding force in the Articles I/II covenants) or nnws_reading (binding force in the Article VI bargain) — relocate the regime''s extraction surface and change who counts as victim?',
    'Comparative classification once the sibling stories are authored: the disagreement is located in which provision carries the regime''s binding weight, so the sibling epsilon values and victim sets can be compared directly against this reading''s.',
    'Under nnws_reading the extraction surface becomes NWS non-compliance and the victim set shifts to the non-nuclear majority in a different configuration; under nws_reading the target becomes NNWS breakout propensity. This reading''s tangled_rope verdict does not transfer to either sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which article of the treaty text carries binding force is the contested structural element across the kernel''s readings.').

omega_variable(
    threshold_codification_direction,
    'If the withdrawal threshold were ever codified, would it codify high (effective Security Council approval — regime-stability priority) or low (explicit self-judging — sovereignty-preservation priority), and which classification does each direction produce?',
    'A review-conference breakthrough on withdrawal-consequence language, or a formal amendment attempt under Article VIII, or an advisory opinion request on the scope of the supreme-interests qualifier.',
    'High-threshold codification converts the arrangement toward entrapment of non-nuclear parties (snare-leaning profile, extraction concentrated on the compliant majority); low-threshold codification converts it toward a pure sovereignty protocol (rope-leaning, extraction largely evaporates). The current ambiguous steady state is what generates the tangled_rope profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_codification_direction, conceptual, 'The direction of any future codification flips the constraint''s structural character in opposite ways.').

omega_variable(
    dprk_withdrawal_legality,
    'Was the 2003 withdrawal legally effective — did the state cease to be a party under Article X, or did the defective notice sequence leave its party status formally unresolved?',
    'Depositary records, Security Council presidential statements of the period, and settled scholarly analysis of the notice-sequence dispute.',
    'If effective, exit is proven cheap and the precedent''s demonstration value is maximal (supports the upper extractiveness range); if ineffective, the precedent weakens, the exit card discounts, and measured extraction falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dprk_withdrawal_legality, empirical, 'The legal effectiveness of the defining precedent determines the price of exit that all threshold-state pricing rests on.').

omega_variable(
    threshold_state_exit_intent,
    'Do threshold states hold the exit card purely as a bargaining instrument (never intending to play it) or as a genuine contingency plan maintained for real encirclement scenarios?',
    'Revealed preference across pressure episodes: whether invocation signals track referral and sanction moments (consistent with bargaining) or track security-threat assessments independent of regime pressure (consistent with contingency).',
    'Pure bargaining-instrument reading confirms the leverage-extraction mechanism and the tangled_rope verdict; a genuine-contingency reading recasts the clause as a functioning safety valve and implies the measured extraction is partly overstated — some of what reads as rent is insurance premium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_exit_intent, empirical, 'Whether the exit threat is instrument or insurance changes how much of the measured extraction is rent versus priced risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(npt__tr_t0, observed).
narrative_ontology:measurement(npt__tr_t5, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement_basis(npt__tr_t5, observed).
narrative_ontology:measurement(npt__tr_t10, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(npt__tr_t10, observed).
narrative_ontology:measurement(npt__tr_t15, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement_basis(npt__tr_t15, observed).
narrative_ontology:measurement(npt__tr_t20, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(npt__tr_t20, observed).
narrative_ontology:measurement(npt__tr_t25, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement_basis(npt__tr_t25, observed).
narrative_ontology:measurement(npt__tr_t30, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 30, 0.54).
narrative_ontology:measurement_basis(npt__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(npt__be_t0, observed).
narrative_ontology:measurement(npt__be_t5, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(npt__be_t5, observed).
narrative_ontology:measurement(npt__be_t10, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(npt__be_t10, observed).
narrative_ontology:measurement(npt__be_t15, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(npt__be_t15, observed).
narrative_ontology:measurement(npt__be_t20, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(npt__be_t20, observed).
narrative_ontology:measurement(npt__be_t25, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(npt__be_t25, observed).
narrative_ontology:measurement(npt__be_t30, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(npt__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(npt__su_t0, observed).
narrative_ontology:measurement(npt__su_t5, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(npt__su_t5, observed).
narrative_ontology:measurement(npt__su_t10, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(npt__su_t10, observed).
narrative_ontology:measurement(npt__su_t15, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(npt__su_t15, observed).
narrative_ontology:measurement(npt__su_t20, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(npt__su_t20, observed).
narrative_ontology:measurement(npt__su_t25, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(npt__su_t25, observed).
narrative_ontology:measurement(npt__su_t30, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(npt__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, nnws_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the NPT bargain' decomposes into three structurally distinct readings of one kernel (npt_treaty_text), per the epsilon-invariance principle. This member authors epsilon for the Article X withdrawal arrangement alone (referent: the standing ambiguous self-judging threshold as operated); nws_reading authors epsilon for the covenant-allocation arrangement; nnws_reading authors epsilon for the Article VI conditional-bargain arrangement. The epsilon values differ because the referents differ — they are not one constraint viewed from angles. Directionality of the family: this reading is mechanically upstream of both siblings, because the cost of exit determines whether the covenants can bind (nws_reading's premise) and whether Article VI conditionality has teeth (nnws_reading's premise). Each story links the others through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
