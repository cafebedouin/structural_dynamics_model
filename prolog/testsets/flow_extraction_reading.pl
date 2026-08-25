% ============================================================================
% CONSTRAINT STORY: flow_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flow_extraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: flow_extraction_reading
 *   human_readable: Pari-Mutuel Takeout as Outcome-Invariant Liquidity Toll
 *   domain: quantitative_finance/gambling_theory/market_microstructure
 *
 * SUMMARY:
 *   This story authors the 'flow extraction' reading of the
 *   beatability-of-the-take kernel: the pari-mutuel takeout is a structural,
 *   outcome-invariant toll levied on liquidity moving through the wagering
 *   pool, not a wager the house makes against bettors' predictive skill.
 *   Under this reading the house (track operator, tote vendor, regulator) is
 *   categorically indifferent to which horse wins — its revenue is a fixed
 *   percentage of handle regardless of outcome distribution. The question
 *   'can the take be beaten' is treated here as a category error: the house
 *   was never playing the prediction game, so no victim/beneficiary structure
 *   attaches to outcome accuracy itself. The victims of THIS reading's
 *   extraction are bettors as flow-generators (their money passing through
 *   the pool triggers the toll), not bettors as failed predictors. Sibling
 *   readings — public_risk_reading (house as risk-bearer against informed
 *   bettors), folk_mountain_reading (takeout as an unchangeable fact of
 *   wagering), and meta_prediction_reading (the take as itself a bet on
 *   aggregate bettor behavior) — are separate constraints with their own ε
 *   and stakeholder structures; this file does not average over them.
 *
 * KEY AGENTS:
 *   - track_operators: agenda_setter/beneficiary (institutional/arbitrage) — sets and collects the outcome-invariant toll
 *   - state_racing_authorities: beneficiary (institutional/constrained) — statutory share of takeout
 *   - tote_system_vendors: beneficiary (organized/mobile) — paid per-transaction on volume, indifferent to outcomes
 *   - recreational_bettors: payer (powerless/mobile) — pays toll on every wager regardless of skill
 *   - professional_handicappers: payer (moderate/constrained) — toll compounds against any edge, structural rather than outcome-based extraction
 *   - market_microstructure_analysts: observer (analytical/analytical) — treats take as a spread-like structural fee
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flow_extraction_reading, 0.68).
domain_priors:suppression_score(flow_extraction_reading, 0.35).
domain_priors:theater_ratio(flow_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flow_extraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(flow_extraction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(flow_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flow_extraction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(flow_extraction_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flow_extraction_reading, tangled_rope).
narrative_ontology:human_readable(flow_extraction_reading, "Pari-Mutuel Takeout as Outcome-Invariant Liquidity Toll").
narrative_ontology:topic_domain(flow_extraction_reading, "quantitative_finance/gambling_theory/market_microstructure").

domain_priors:requires_active_enforcement(flow_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flow_extraction_reading, 'fb4ae3cc-6731-46da-ac5f-4039f201e0df').
narrative_ontology:cs_kernel_codification('fb4ae3cc-6731-46da-ac5f-4039f201e0df', distributed).
narrative_ontology:cs_authority_grounding('fb4ae3cc-6731-46da-ac5f-4039f201e0df', practice).
narrative_ontology:cs_interpretation_layer_present('fb4ae3cc-6731-46da-ac5f-4039f201e0df').
narrative_ontology:cs_reading_relation('fb4ae3cc-6731-46da-ac5f-4039f201e0df', flow_extraction_reading__public_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb4ae3cc-6731-46da-ac5f-4039f201e0df', flow_extraction_reading__folk_mountain_reading, influences).
narrative_ontology:cs_reading_relation('fb4ae3cc-6731-46da-ac5f-4039f201e0df', flow_extraction_reading__meta_prediction_reading, coexists_with).
narrative_ontology:cs_axiom('fb4ae3cc-6731-46da-ac5f-4039f201e0df', foundational, house_indifference_to_outcome_distribution).
narrative_ontology:cs_axiom_status(house_indifference_to_outcome_distribution, holdable).
narrative_ontology:cs_axiom_grounding('fb4ae3cc-6731-46da-ac5f-4039f201e0df', house_indifference_to_outcome_distribution, empirically_contingent).
narrative_ontology:cs_axiom('fb4ae3cc-6731-46da-ac5f-4039f201e0df', foundational, extraction_indexed_to_flow_not_accuracy).
narrative_ontology:cs_axiom_status(extraction_indexed_to_flow_not_accuracy, holdable).
narrative_ontology:cs_axiom_grounding('fb4ae3cc-6731-46da-ac5f-4039f201e0df', extraction_indexed_to_flow_not_accuracy, conventional).
narrative_ontology:cs_reference_frame('fb4ae3cc-6731-46da-ac5f-4039f201e0df', pooled_pricing_without_bookmaker_counterparty).
narrative_ontology:cs_drift_state('fb4ae3cc-6731-46da-ac5f-4039f201e0df', contemporary_takeout_rate_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fb4ae3cc-6731-46da-ac5f-4039f201e0df', '').
narrative_ontology:cs_kernel_id(flow_extraction_reading, beatability_of_the_take).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flow_extraction_reading, track_operators).
narrative_ontology:constraint_beneficiary(flow_extraction_reading, state_racing_authorities).
narrative_ontology:constraint_beneficiary(flow_extraction_reading, tote_system_vendors).
narrative_ontology:constraint_victim(flow_extraction_reading, recreational_bettors).
narrative_ontology:constraint_victim(flow_extraction_reading, professional_handicappers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the takeout rate on every pool (win, place, show, exotics) and collects it automatically regardless of which horse wins, which bettor is skilled, or how sharp the closing odds turn out to be. The rate is negotiated with regulators and is structurally the same whether the pool is efficient or wildly mispriced. Exit for the operator is arbitrage-grade: it can raise exotic-wager takeout, shift toward simulcast/ADW volume, or exit unprofitable meets entirely.
narrative_ontology:constraint_stakeholder(flow_extraction_reading, track_operators, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(flow_extraction_reading, track_operators, beneficiary).

% Levies a statutory share of the takeout for purses, breeding funds, and general revenue. Has a structural interest in the pool's continued existence, not in any bettor's outcome; its cut arrives identically whether the favorite or the longshot wins.
narrative_ontology:constraint_stakeholder(flow_extraction_reading, state_racing_authorities, beneficiary,
    institutional, generational, constrained, regional).

% Operates the pooling infrastructure and is paid per-transaction or as a percentage of handle. Indifferent to race outcomes by design; the vendor's revenue is a function of volume flowing through the pool, not of who cashes tickets.
narrative_ontology:constraint_stakeholder(flow_extraction_reading, tote_system_vendors, beneficiary,
    organized, biographical, mobile, national).

% Wagers into pools where a fixed percentage is removed before payouts are calculated, on every bet, win or lose. Can walk away from the track entirely (mobile exit at the individual level) but cannot negotiate the rate or avoid it while participating; the toll is levied on the act of betting, not on being wrong.
narrative_ontology:constraint_stakeholder(flow_extraction_reading, recreational_bettors, payer,
    powerless, immediate, mobile, local).

% Attempts to find structurally mispriced pools and bet large volume against public money. Even a bettor who correctly predicts outcomes at a rate that would be profitable in a zero-toll market can be driven net-negative by the takeout compounding across the volume required to exploit small edges. Exit means leaving pari-mutuel wagering for fixed-odds or private markets, which are themselves scarce or illegal in many jurisdictions.
narrative_ontology:constraint_stakeholder(flow_extraction_reading, professional_handicappers, payer,
    moderate, biographical, constrained, national).

% Studies the take as a structural feature of pooled-liquidity markets comparable to bid-ask spread or exchange fees — a toll on flow, not a bet on outcomes. Does not participate in the pool and has no stake in who wins any given race.
narrative_ontology:constraint_stakeholder(flow_extraction_reading, market_microstructure_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flow_extraction_reading, diffuse).
narrative_ontology:fixing_cost_class(flow_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools dispersed, uncoordinated wagers into a single liquidity structure so that odds can be derived endogenously from bettor behavior rather than requiring a bookmaker to set and defend prices against informed action; the take funds the infrastructure (tote systems, regulatory oversight, purse structures) that makes pooling possible at scale.
% TRANSFER_FUNCTION: Moves a fixed percentage of every dollar wagered — independent of race outcome — from the pool of bettors collectively to track operators, racing authorities, and infrastructure vendors, before any payout is calculated.
% ABSENT_VOICES: Bettors as a class have no seat in setting the takeout rate; the rate is negotiated between track operators and regulators without a bettor representative present in most jurisdictions. Professional handicappers occasionally testify at rate hearings but rarely alter outcomes.
% DISAPPEARANCE_RATIONALE: If the takeout vanished overnight, tracks would lose their primary revenue mechanism, purses would collapse without an alternate funding source, and the pool structure itself would likely be replaced by fixed-odds or exchange-based wagering within a short period — the arrangement is load-bearing for the entire pari-mutuel industry's existence, not incidental to it.
% FOUNDING_PROBLEM: Early horse racing wagering had no reliable mechanism to aggregate dispersed bets into fair odds or to fund track operations and purses; the pari-mutuel pool solved both by letting the crowd set prices and skimming a fixed share to fund the infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Independent market microstructure researchers and gaming economists outside the racing industry corroborate that pooled-liquidity funding remains structurally necessary wherever no counterparty is willing to hold outcome risk at scale; however, several state auditors' reports (outside track-operator or regulator self-interest) note that takeout rates have risen well above what infrastructure cost alone would require, suggesting the funding function persists alongside a rent component.
narrative_ontology:disappearance_verdict(flow_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flow_extraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flow_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(flow_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flow_extraction_reading, 0.68, 'claude-sonnet-5', 'benter_hkjc_parimutuel_2026_20260825_125025', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flow_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flow_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flow_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.68 by interval end because the take removes a fixed percentage of all handle before any payout math occurs — this is a volume tax, not a skill tax, and it compounds against every bettor including those with genuine predictive edges. Suppression is moderate (0.35): bettors are not coerced into wagering, but once in the pool there is no mechanism to avoid the toll on the wagered dollar, and jurisdictions restrict fixed-odds alternatives that would let bettors escape pooled pricing. Theater ratio rises across the interval (0.20 to 0.42) as takeout increasingly funds general state revenue and administrative overhead rather than the purse/infrastructure functions it was originally levied to support — a Goodhart-style drift from funding mechanism toward embedded revenue stream. Accessibility collapse is moderate (0.40): alternatives exist (fixed-odds books, exchanges, offshore markets) but are legally restricted or unavailable in most US racing jurisdictions, so alternatives are suppressed by regulation rather than eliminated by necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Track operators, racing authorities, and tote vendors sit at the beneficiary end of directionality: their revenue is a direct, guaranteed function of handle, decoupled from any bettor's success. Recreational bettors and professional handicappers sit toward the target end: the take extracts from the wagered dollar regardless of the bettor's skill, meaning even a handicapper with genuine positive expected value pre-takeout can be rendered net-negative by the structural toll. This is the reading's key move: the extraction is defined over FLOW (dollars wagered), not over OUTCOME (predictions correct), so the victim/beneficiary structure tracks participation in the pool, not accuracy of forecasts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (funding pari-mutuel infrastructure and purses through a crowd-priced pool) remains partially live, which prevents a clean mandatrophy verdict — but the rising theater_ratio and the auditor-corroborated gap between infrastructure cost and current takeout rates suggest the mandate has partially drifted from its founding function toward embedded state and operator revenue. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (pooled pricing without a bookmaker) while still registering the asymmetric extraction that the enforcement (regulatory takeout floors, restrictions on alternative markets) makes possible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    flow_toll_vs_prediction_market_category,
    'Is the pari-mutuel take structurally more like an exchange fee (levied on flow, indifferent to outcome) or more like a bookmaker''s vigorish (levied in a way that is sensitive to informed betting pressure)?',
    'Compare takeout behavior across pool types: if takeout rates and structures are invariant to the presence of sharp/informed money in the pool (i.e., tracks do not adjust takeout in response to detected skilled action), the flow-toll reading is supported; if takeout or rule changes correlate with skilled-bettor activity, the public_risk_reading gains support instead.',
    'If the flow-toll reading is vindicated, no victim/beneficiary structure should attach to outcome-prediction accuracy at all — professional handicappers are not ''targets'' because they predict well or poorly, only because they generate flow. If the public_risk_reading is vindicated instead, the victim structure would need to be re-authored around informed-vs-uninformed bettor asymmetry, which is a structurally different constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flow_toll_vs_prediction_market_category, conceptual, 'Whether the take''s structural target is flow or informed prediction — the central kernel disagreement this reading resolves one way.').

omega_variable(
    takeout_rate_setting_capture,
    'Are takeout rates set primarily to cover the genuine coordination costs of pooling (tote infrastructure, regulatory oversight, purses) or do they reflect regulatory capture by track operators seeking rent beyond those costs?',
    'Independent cost-accounting audit of tote infrastructure and purse funding requirements versus actual takeout revenue collected, compared across jurisdictions with different regulatory capture profiles.',
    'If takeout tracks cost closely, the coordination function dominates and a rope classification would be more defensible; if takeout substantially exceeds funding need, the tangled_rope (or even snare) reading is reinforced and the rising theater_ratio measurement is diagnostic of extraction rather than noise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(takeout_rate_setting_capture, empirical, 'Whether the takeout rate reflects genuine funding need or captured rent-setting.').

omega_variable(
    sibling_reading_boundary_location,
    'Where exactly does the flow_extraction_reading''s premise (outcome-invariant toll) stop being defensible and the public_risk_reading''s premise (house as risk-bearing counterparty) become more accurate — e.g., in exotic wagers (trifectas, superfectas) where takeout is often higher and pool dynamics are more sensitive to sharp money concentrating on specific combinations?',
    'Disaggregate takeout structural analysis by bet type (win/place/show vs. exotic wagers) and test whether the outcome-invariance premise holds equally across bet types, or whether exotic pools show behavior more consistent with the public_risk_reading.',
    'If exotic wagers behave differently, this constraint''s scope may need to be narrowed to win/place/show pools specifically, with a separate constraint story authored for exotic-wager takeout under a different reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary_location, conceptual, 'Where the flow-extraction premise''s applicability boundary sits within the broader pari-mutuel system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flow_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flow_tr_t0, flow_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(flow_tr_t8, flow_extraction_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(flow_tr_t16, flow_extraction_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(flow_tr_t24, flow_extraction_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(flow_tr_t32, flow_extraction_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(flow_tr_t40, flow_extraction_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(flow_be_t0, flow_extraction_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(flow_be_t8, flow_extraction_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(flow_be_t16, flow_extraction_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(flow_be_t24, flow_extraction_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(flow_be_t32, flow_extraction_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(flow_be_t40, flow_extraction_reading, base_extractiveness, 40, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(flow_extraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flow_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flow_extraction_reading, 0.15).
narrative_ontology:affects_constraint(flow_extraction_reading, public_risk_reading).
narrative_ontology:affects_constraint(flow_extraction_reading, folk_mountain_reading).
narrative_ontology:affects_constraint(flow_extraction_reading, meta_prediction_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from the colloquial 'beatability of the take' kernel (beatability_of_the_take). Each reading treats a structurally distinct claim: flow_extraction_reading (this file) holds the take is an outcome-invariant liquidity toll with no accuracy-linked victim structure; public_risk_reading holds the house is a genuine risk-bearing counterparty exposed to informed betting; folk_mountain_reading holds the take is an unchallengeable structural fact of wagering (candidate false-summit mountain); meta_prediction_reading holds the take is itself an implicit bet on aggregate market efficiency. Each carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; they are linked via network edges, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
