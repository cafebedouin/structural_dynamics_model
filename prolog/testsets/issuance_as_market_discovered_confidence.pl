% ============================================================================
% CONSTRAINT STORY: issuance_as_market_discovered_confidence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_issuance_as_market_discovered_confidence, []).

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
 *   constraint_id: issuance_as_market_discovered_confidence
 *   human_readable: Market-Discovered Confidence as the Legitimating Test for Sovereign and Corporate Future Claims
 *   domain: constitutional_political_economy/monetary_theory/corporate_property_law
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the
 *   future_claims_present_resources kernel: that a proposed future claim on
 *   present resources acquires legitimacy exclusively through decentralized
 *   market pricing — yields, spreads, exchange-rate movements — rather than
 *   through any constituted body's deliberative vote or any
 *   physical-commodity test administered ex ante. Under this reading, the
 *   exchange-rate signal referenced in §36 as merely 'delivering information'
 *   about external accounts is reinterpreted as the PRIMARY legitimating
 *   mechanism for all claims generally, not a residual check. The Monetary
 *   Organ's formal deliberation is, on this reading, itself a distortion of
 *   what should be pure price discovery. This story authors ONLY this
 *   reading; the deliberative-judgment, endogenous-credit-multiplication, and
 *   physical-backing readings are separate constraints
 *   (issuance_as_deliberative_judgment,
 *   issuance_as_endogenous_credit_multiplication,
 *   issuance_as_physical_backing) linked via network.affects_constraints,
 *   each with its own ε and its own legitimating test.
 *
 * KEY AGENTS:
 *   - sophisticated_bond_market_participants: primary structural beneficiary (institutional/arbitrage) — captures rents from constituting the discovery signal
 *   - reserve_currency_issuing_sovereigns: beneficiary and de facto agenda-setter (institutional/arbitrage) — benchmarks the whole system, faces inelastic demand regardless of fundamentals
 *   - large_dealer_banks: agenda-setter (institutional/arbitrage) — physically executes the price discovery process and can distort it through liquidity provision decisions
 *   - peripheral_sovereign_borrowers: primary target (moderate/constrained) — bears sudden-stop risk from sentiment shifts unconnected to fundamentals
 *   - small_currency_households: primary target (powerless/trapped) — absorbs depreciation with no seat at the discovery table
 *   - monetary_organ_deliberative_body: excluded party (institutional/constrained) — nominally the constituted authority, subordinated under this reading to the market's verdict
 *   - constitutional_political_economists: analytical observer — assesses whether the discovery process is genuine information aggregation or self-referential capital preference laundering
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(issuance_as_market_discovered_confidence, 0.58).
domain_priors:suppression_score(issuance_as_market_discovered_confidence, 0.44).
domain_priors:theater_ratio(issuance_as_market_discovered_confidence, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(issuance_as_market_discovered_confidence, extractiveness, 0.58).
narrative_ontology:constraint_metric(issuance_as_market_discovered_confidence, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(issuance_as_market_discovered_confidence, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(issuance_as_market_discovered_confidence, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(issuance_as_market_discovered_confidence, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(issuance_as_market_discovered_confidence, tangled_rope).
narrative_ontology:human_readable(issuance_as_market_discovered_confidence, "Market-Discovered Confidence as the Legitimating Test for Sovereign and Corporate Future Claims").
narrative_ontology:topic_domain(issuance_as_market_discovered_confidence, "constitutional_political_economy/monetary_theory/corporate_property_law").

domain_priors:requires_active_enforcement(issuance_as_market_discovered_confidence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(issuance_as_market_discovered_confidence, '82521384-3d38-44af-acdc-b1f539c735ca').
narrative_ontology:cs_kernel_codification('82521384-3d38-44af-acdc-b1f539c735ca', distributed).
narrative_ontology:cs_authority_grounding('82521384-3d38-44af-acdc-b1f539c735ca', distributed).
narrative_ontology:cs_reading_relation('82521384-3d38-44af-acdc-b1f539c735ca', issuance_as_market_discovered_confidence__issuance_as_deliberative_judgment, influences).
narrative_ontology:cs_reading_relation('82521384-3d38-44af-acdc-b1f539c735ca', issuance_as_market_discovered_confidence__issuance_as_endogenous_credit_multiplication, coexists_with).
narrative_ontology:cs_reading_relation('82521384-3d38-44af-acdc-b1f539c735ca', issuance_as_market_discovered_confidence__issuance_as_physical_backing, forecloses).
narrative_ontology:cs_axiom('82521384-3d38-44af-acdc-b1f539c735ca', foundational, price_discovery_is_the_legitimating_test).
narrative_ontology:cs_axiom_status(price_discovery_is_the_legitimating_test, holdable).
narrative_ontology:cs_axiom_grounding('82521384-3d38-44af-acdc-b1f539c735ca', price_discovery_is_the_legitimating_test, instrumental).
narrative_ontology:cs_axiom('82521384-3d38-44af-acdc-b1f539c735ca', secondary, deliberative_vote_is_distortion_not_codetermination).
narrative_ontology:cs_axiom_status(deliberative_vote_is_distortion_not_codetermination, holdable).
narrative_ontology:cs_axiom_grounding('82521384-3d38-44af-acdc-b1f539c735ca', deliberative_vote_is_distortion_not_codetermination, empirically_contingent).
narrative_ontology:cs_reference_frame('82521384-3d38-44af-acdc-b1f539c735ca', bretton_woods_administered_parity_regime).
narrative_ontology:cs_drift_state('82521384-3d38-44af-acdc-b1f539c735ca', post_1971_floating_rate_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('82521384-3d38-44af-acdc-b1f539c735ca', '').
narrative_ontology:cs_kernel_id(issuance_as_market_discovered_confidence, future_claims_present_resources).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(issuance_as_market_discovered_confidence, sophisticated_bond_market_participants).
narrative_ontology:constraint_beneficiary(issuance_as_market_discovered_confidence, reserve_currency_issuing_sovereigns).
narrative_ontology:constraint_beneficiary(issuance_as_market_discovered_confidence, large_dealer_banks).
narrative_ontology:constraint_victim(issuance_as_market_discovered_confidence, peripheral_sovereign_borrowers).
narrative_ontology:constraint_victim(issuance_as_market_discovered_confidence, small_currency_households).
narrative_ontology:constraint_victim(issuance_as_market_discovered_confidence, future_taxpayers_of_depreciating_states).
narrative_ontology:constraint_vindicates(issuance_as_market_discovered_confidence, price_discovery_superiority_doctrine).
narrative_ontology:constraint_vindicates(issuance_as_market_discovered_confidence, market_efficiency_of_sovereign_credit).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Prices sovereign and corporate promises continuously through yield spreads, CDS pricing, and exchange-rate moves. Extracts a structural rent from being the entity whose voluntary willingness to lend IS the legitimating signal; can reposition across currencies and issuers within minutes when a reading of confidence shifts, capturing the spread between rapid repricing and slower-moving political and fiscal responses.
narrative_ontology:constraint_stakeholder(issuance_as_market_discovered_confidence, sophisticated_bond_market_participants, beneficiary,
    institutional, biographical, arbitrage, global).

% Issues claims on future resources that the market discounts favorably regardless of underlying fiscal trajectory, because global demand for the reserve asset itself is inelastic. Effectively sets the terms of the discovery game by being the asset against which all other currencies and yields are benchmarked.
narrative_ontology:constraint_stakeholder(issuance_as_market_discovered_confidence, reserve_currency_issuing_sovereigns, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(issuance_as_market_discovered_confidence, reserve_currency_issuing_sovereigns, agenda_setter).

% Makes the markets in which yields, spreads, and exchange rates are actually formed — provides the liquidity and inventory that constitutes 'the market' as an observable object. Collects bid-ask spread and inventory rents from being the mechanism through which price discovery is physically executed, and can widen spreads or withdraw liquidity in stress, itself altering what the discovery process reports.
narrative_ontology:constraint_stakeholder(issuance_as_market_discovered_confidence, large_dealer_banks, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(issuance_as_market_discovered_confidence, large_dealer_banks, beneficiary).

% Their future claims are priced by the same market mechanism but without reserve-currency inelastic demand backing them; a shift in sentiment (not a change in underlying fiscal fundamentals) can trigger a sudden stop, forcing austerity or default. They cannot exit the discovery mechanism — no alternative legitimation channel exists once the deliberative and physical-backing alternatives have been displaced.
narrative_ontology:constraint_stakeholder(issuance_as_market_discovered_confidence, peripheral_sovereign_borrowers, payer,
    moderate, generational, constrained, national).

% Absorbs currency depreciation as imported inflation whenever market participants collectively reprice their sovereign's credibility, with no vote, no seat at the discovery table, and typically no foreign-currency savings to hedge with. Their real wages and savings move with a signal set by trades they never participate in and cannot observe forming.
narrative_ontology:constraint_stakeholder(issuance_as_market_discovered_confidence, small_currency_households, payer,
    powerless, immediate, trapped, national).

% Will service, through taxation or inflation tax, whatever confidence-discount the market currently assigns to their state's future claims — a discount set by present-day traders who will not bear the eventual fiscal consequence. Cannot participate in or contest the discovery process that determines their future burden.
narrative_ontology:constraint_stakeholder(issuance_as_market_discovered_confidence, future_taxpayers_of_depreciating_states, payer,
    powerless, generational, trapped, national).

% Under this reading, its formal votes on rates and reserve backing are treated as noise or distortion relative to the 'real' signal the market discovers independently — its deliberative judgment is structurally subordinated to whatever the yield curve says, even though it is nominally the constituted authority over the currency.
narrative_ontology:constraint_stakeholder(issuance_as_market_discovered_confidence, monetary_organ_deliberative_body, excluded,
    institutional, generational, constrained, national).

% Studies whether decentralized price discovery is a genuine legitimating test of sovereign promises or a self-referential mechanism that launders the preferences of whoever already holds capital into an appearance of neutral market verdict.
narrative_ontology:constraint_stakeholder(issuance_as_market_discovered_confidence, constitutional_political_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(issuance_as_market_discovered_confidence, diffuse).
narrative_ontology:fixing_cost_class(issuance_as_market_discovered_confidence, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates dispersed private information about the credibility of a sovereign or corporate promise into a single continuously updated price (yield, spread, exchange rate), avoiding the need for any single body to adjudicate creditworthiness by fiat and allowing capital to flow toward promises the aggregate of informed participants judges most likely to be honored.
% TRANSFER_FUNCTION: Moves financing capacity from states and firms whose promises the market discounts toward those it favors; moves real income from populations whose currency depreciates (import-price inflation, debt-service burden) toward holders of the assets that appreciate or that were sold short ahead of the repricing, and toward the intermediaries who execute the repricing trades.
% ABSENT_VOICES: Households and future taxpayers who will bear the consequences of a confidence discount have no representation in the market process that sets it; the deliberative Monetary Organ, though constitutionally charged with judgment, is treated under this reading as a source of distortion to be deferred around rather than a legitimate co-determinant of the outcome.
% DISAPPEARANCE_RATIONALE: If market-discovered pricing were displaced as the legitimating test (e.g., by a return to deliberative fiat-backed determination or a physical-commodity anchor), sovereign borrowing costs would decouple from real-time sentiment, capital flows would reallocate according to the substitute legitimating test, and the intermediary rents currently captured by dealers and sophisticated participants in constituting 'the market' would collapse or migrate to whatever institution replaced the discovery function.
% FOUNDING_PROBLEM: No central planner or constituted body has complete information about the true probability that a given sovereign or firm will honor a future claim; centralized ex ante vetting (physical backing, deliberative committee judgment) is slow, capturable, and has historically failed to prevent both under- and over-issuance. Decentralized markets were held to solve the information-aggregation problem by pricing continuously against real capital at stake.
% FOUNDING_PROBLEM_CORROBORATION: Market participants and reserve-currency treasuries attest the discovery mechanism remains the best available test of credibility, given the historical failure of centrally administered credit allocation. Independent monetary economists and BIS/IMF crisis retrospectives (external to the benefiting dealer and sovereign-issuer seats) document repeated instances of self-fulfilling sudden stops and contagion in which the 'market signal' amplified rather than measured underlying fundamentals — evidence the discovery mechanism sometimes manufactures the very confidence collapse it claims only to observe.
narrative_ontology:disappearance_verdict(issuance_as_market_discovered_confidence, world_rearranges).
narrative_ontology:founding_problem_status(issuance_as_market_discovered_confidence, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(issuance_as_market_discovered_confidence, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-08',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(issuance_as_market_discovered_confidence, 'none', 1).
narrative_ontology:epsilon_provenance(issuance_as_market_discovered_confidence, 0.58, 'claude-sonnet-5', 'c2_monetary_architecture_2026_20260808_170220', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(issuance_as_market_discovered_confidence_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(issuance_as_market_discovered_confidence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(issuance_as_market_discovered_confidence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) reflects that market pricing genuinely aggregates dispersed information (real coordination value) but also structurally advantages whoever already holds mobile capital and market-making infrastructure — the same mechanism that legitimates claims also lets its most capable participants profit from volatility they can trade around while the powerless cannot. Suppression is moderate (0.44) rather than high: no one is coercively barred from participating in markets, but the ALTERNATIVE legitimating tests (deliberative vote, physical backing) have been institutionally displaced for large classes of decisions, which is a suppression of alternatives rather than of participation itself. Theater ratio (0.40) captures that constituted bodies (central banks, legislatures) continue to hold votes and issue statements that are largely priced-in or ignored relative to what the market has already discovered — a genuine but partially performative deliberative layer sits atop the real discovery mechanism. Accessibility collapse (0.50) and resistance (0.62) reflect that this is a contested, not settled, arrangement — states and economists actively contest whether market discipline is legitimate governance or capital's veto.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (sophisticated market participants, reserve-currency sovereigns, dealer banks) sit near the low end of directionality because the mechanism either directly enriches them (spread capture, arbitrage) or structurally favors them regardless of merit (reserve-currency inelastic demand). Victims (peripheral borrowers, small-currency households, future taxpayers) sit near the high end: they are trapped or constrained, cannot exit the pricing mechanism that determines their fiscal or real-income fate, and bear costs generated by decisions made by parties who face no matching consequence. The Monetary Organ occupies an anomalous excluded position — nominally powerful but structurally subordinated under this specific reading, which is the crux of the kernel dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no central planner has complete information to vet future claims) remains partially live — genuine information-aggregation value exists and central planning has real historical failure modes. But the reading's claim that deliberative judgment is ALWAYS distortion relative to market discovery is contested by external evidence (BIS/IMF retrospectives) showing markets can manufacture rather than merely observe confidence collapses. This is exactly the tangled-rope signature: real coordination function (dispersed information aggregation) coexisting with asymmetric extraction (rent capture by those constituting the signal, cost borne by those priced by it) requiring active institutional enforcement (legal primacy of market-clearing prices over administered alternatives) to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discovery_versus_self_fulfilling_panic,
    'When market pricing shifts sharply against a sovereign''s future claims, is this the discovery mechanism correctly detecting deteriorating fundamentals, or is it a self-fulfilling contagion in which the pricing signal manufactures the very insolvency it claims only to observe?',
    'Compare pre-crisis fundamentals against post-crisis outcomes across a panel of sudden-stop episodes; if repricing magnitude systematically exceeds what fundamentals alone predict and correlates with herding/liquidity-withdrawal dynamics among dealers, the discovery claim is undermined for at least a subset of cases.',
    'If self-fulfilling dynamics dominate in a substantial share of cases, the ''market discovers legitimacy'' framing collapses into a description of concentrated capital''s veto power dressed as neutral information aggregation — pushing the classification toward snare for peripheral borrowers specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovery_versus_self_fulfilling_panic, empirical, 'Whether market-signaled loss of confidence measures or manufactures sovereign credit deterioration.').

omega_variable(
    kernel_reading_exclusivity,
    'Does treating market price discovery as THE primary legitimating mechanism for all future claims logically foreclose the deliberative-judgment reading, or can both operate as parallel, mutually informing tests within the same constitutional order?',
    'Examine whether any actual monetary regime has successfully operated with formally co-equal deliberative and market tests (neither subordinated to the other) without one eventually being read as decisive; historical case comparison across currency regimes.',
    'If genuine coexistence is empirically achievable, this reading''s implicit subordination of the Monetary Organ is a policy choice rather than a structural necessity, weakening the ''discovery is primary'' axiom''s claim to inevitability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_exclusivity, conceptual, 'Whether market-discovery primacy structurally requires subordinating deliberative judgment or merely one contingent institutional arrangement among others.').

omega_variable(
    section_36_generalization_scope,
    'Does §36''s statement that the exchange-rate signal ''delivers information'' about external accounts license generalizing that signal into the PRIMARY legitimating test for all future claims (not just external-account-relevant ones), or is this reading over-extending a narrow technical observation into a general constitutional principle?',
    'Textual and drafting-history analysis of §36''s surrounding provisions: was the exchange-rate-information clause drafted with an external-accounts scope limitation, or with open generality that this reading is entitled to invoke?',
    'If §36 was scoped narrowly to external accounts, this reading''s central move — generalizing a residual check into THE primary legitimating mechanism — is an interpretive overreach, weakening the reading''s textual grounding relative to the deliberative-judgment and physical-backing readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_36_generalization_scope, conceptual, 'Whether this reading''s core generalizing move from §36 is textually licensed or an overreach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(issuance_as_market_discovered_confidence, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(issu_tr_t0, issuance_as_market_discovered_confidence, theater_ratio, 0, 0.22).
narrative_ontology:measurement(issu_tr_t8, issuance_as_market_discovered_confidence, theater_ratio, 8, 0.27).
narrative_ontology:measurement(issu_tr_t16, issuance_as_market_discovered_confidence, theater_ratio, 16, 0.31).
narrative_ontology:measurement(issu_tr_t24, issuance_as_market_discovered_confidence, theater_ratio, 24, 0.35).
narrative_ontology:measurement(issu_tr_t32, issuance_as_market_discovered_confidence, theater_ratio, 32, 0.38).
narrative_ontology:measurement(issu_tr_t40, issuance_as_market_discovered_confidence, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(issu_be_t0, issuance_as_market_discovered_confidence, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(issu_be_t8, issuance_as_market_discovered_confidence, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(issu_be_t16, issuance_as_market_discovered_confidence, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(issu_be_t24, issuance_as_market_discovered_confidence, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(issu_be_t32, issuance_as_market_discovered_confidence, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(issu_be_t40, issuance_as_market_discovered_confidence, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(issu_su_t0, issuance_as_market_discovered_confidence, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(issu_su_t8, issuance_as_market_discovered_confidence, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(issu_su_t16, issuance_as_market_discovered_confidence, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(issu_su_t24, issuance_as_market_discovered_confidence, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(issu_su_t32, issuance_as_market_discovered_confidence, suppression_requirement, 32, 0.42).
narrative_ontology:measurement(issu_su_t40, issuance_as_market_discovered_confidence, suppression_requirement, 40, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(issuance_as_market_discovered_confidence, resource_allocation).
narrative_ontology:boltzmann_floor_override(issuance_as_market_discovered_confidence, 0.12).
narrative_ontology:affects_constraint(issuance_as_market_discovered_confidence, issuance_as_deliberative_judgment).
narrative_ontology:affects_constraint(issuance_as_market_discovered_confidence, issuance_as_endogenous_credit_multiplication).
narrative_ontology:affects_constraint(issuance_as_market_discovered_confidence, issuance_as_physical_backing).

% DUAL FORMULATION NOTE:
% This story is one of four siblings decomposing the single natural-language concept 'what legitimates a future claim on present resources' (the future_claims_present_resources kernel) into structurally distinct constraints, each with its own legitimating observable and hence its own ε: market price discovery (this story), deliberative institutional vote (issuance_as_deliberative_judgment), endogenous bank-credit dynamics (issuance_as_endogenous_credit_multiplication), and physical/commodity backing (issuance_as_physical_backing). Per the ε-invariance principle, these are not one constraint measured four ways but four constraints sharing a contested kernel text; each authors independent beneficiary/victim structure and independent stakeholder seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(issuance_as_market_discovered_confidence, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
