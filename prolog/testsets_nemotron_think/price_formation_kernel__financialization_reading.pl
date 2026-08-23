% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__financialization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__financialization_reading, []).

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
 *   constraint_id: price_formation_kernel__financialization_reading
 *   human_readable: Price Formation via Financialization
 *   domain: economic/political
 *
 * SUMMARY:
 *   This constraint story captures the financialization reading of the
 *   price_formation_kernel: the claim that housing prices are primarily
 *   driven by credit expansion, asset-price feedback loops, and demand for
 *   housing as a financial asset rather than shelter. The financial sector
 *   (banks, shadow banks, GSEs) benefits from origination volume and interest
 *   spreads; real estate investors capture leveraged appreciation; existing
 *   leveraged homeowners gain paper equity. First-time buyers, renters, and
 *   mortgage-burdened households bear the costs — higher debt service,
 *   pricing out, crash risk. The coordination function (capital allocation to
 *   housing) is real but has atrophied: mortgage debt now vastly exceeds
 *   construction finance needs. The engine should compute this as
 *   tangled_rope — genuine coordination overlaid with asymmetric extraction
 *   requiring active enforcement (monetary policy, regulatory forbearance,
 *   bailout expectations).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.78).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.65).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Price Formation via Financialization").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "economic/political").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, 'a9c7d873-1d9c-439c-975b-482068d177a9').
narrative_ontology:cs_kernel_codification('a9c7d873-1d9c-439c-975b-482068d177a9', distributed).
narrative_ontology:cs_authority_grounding('a9c7d873-1d9c-439c-975b-482068d177a9', extraction).
narrative_ontology:cs_interpretation_layer_present('a9c7d873-1d9c-439c-975b-482068d177a9').
narrative_ontology:cs_reading_relation('a9c7d873-1d9c-439c-975b-482068d177a9', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9c7d873-1d9c-439c-975b-482068d177a9', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('a9c7d873-1d9c-439c-975b-482068d177a9', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_axiom('a9c7d873-1d9c-439c-975b-482068d177a9', foundational, credit_drives_asset_prices).
narrative_ontology:cs_axiom_status(credit_drives_asset_prices, holdable).
narrative_ontology:cs_axiom_grounding('a9c7d873-1d9c-439c-975b-482068d177a9', credit_drives_asset_prices, empirically_contingent).
narrative_ontology:cs_axiom('a9c7d873-1d9c-439c-975b-482068d177a9', foundational, housing_as_financial_asset_primacy).
narrative_ontology:cs_axiom_status(housing_as_financial_asset_primacy, holdable).
narrative_ontology:cs_axiom_grounding('a9c7d873-1d9c-439c-975b-482068d177a9', housing_as_financial_asset_primacy, conventional).
narrative_ontology:cs_reference_frame('a9c7d873-1d9c-439c-975b-482068d177a9', postwar_construction_finance_framework).
narrative_ontology:cs_drift_state('a9c7d873-1d9c-439c-975b-482068d177a9', contemporary_financialized_housing, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a9c7d873-1d9c-439c-975b-482068d177a9', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, financial_sector).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, real_estate_investors).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, leveraged_homeowners).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_buyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, households_with_mortgage_debt).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, leveraged_homeowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets lending standards, originates and securitizes mortgage debt, profits from transaction volume and interest spreads. Benefits from central bank liquidity backstops and regulatory frameworks that treat housing debt as low-risk. Can redirect capital across asset classes if housing becomes unprofitable.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, financial_sector, agenda_setter,
    institutional, generational, arbitrage, global).

% Acquires housing as financial asset using leveraged credit. Captures capital gains from price appreciation driven by credit expansion. Benefits from tax treatment favoring property investment. Can exit to other asset classes or geographies when local conditions deteriorate.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, real_estate_investors, beneficiary,
    organized, biographical, mobile, national).

% Existing owners with mortgages benefit from nominal price appreciation that increases equity. But they also bear debt service costs and refinancing risk. Exit requires selling into the same market — realizing gains means buying back in at elevated prices or leaving the housing system entirely.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, leveraged_homeowners, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, leveraged_homeowners, payer).

% Face prices detached from local income, driven by credit availability they cannot influence. Must take on high debt-to-income ratios or remain renters. No meaningful exit — delaying purchase means facing higher prices later; buying means locking in elevated debt service for decades.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_buyers, payer,
    powerless, biographical, trapped, local).

% Pay rents that track asset prices set by leveraged buyers, without building equity. No access to the credit expansion that drives prices. Exit options limited to moving to lower-cost areas (often with fewer opportunities) or household formation delay.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, renters, payer,
    powerless, biographical, constrained, local).

% Carry debt service burdens calibrated to peak prices. Vulnerable to interest rate resets and income shocks. Negative equity risk if prices correct. Cannot easily exit without default or bringing cash to close — both carry severe consequences.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, households_with_mortgage_debt, payer,
    moderate, biographical, constrained, regional).

% Set monetary policy that determines credit cost and availability. Mandate financial stability but often treat rising asset prices as stability. Capture by financial sector thinking creates structural bias toward accommodation. Could change framework but face institutional inertia and revolving-door dynamics.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, central_bank_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Study the structural relationship between credit, prices, and distribution. No direct stake in outcomes but their frameworks shape policy discourse. See the full architecture: credit expansion → price feedback → wealth transfer → political capture.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, housing_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation to housing via credit creation — solves the problem of matching long-term shelter demand with upfront construction finance by using banks' maturity transformation and central bank liquidity backstops.
% TRANSFER_FUNCTION: Moves wealth from households (down payments, debt service, foregone consumption) to financial sector (origination fees, interest, securitization profits) and asset-holding classes (capital gains on leveraged positions). The transfer scales with credit expansion velocity and leverage ratios.
% ABSENT_VOICES: Future households not yet formed — they inherit the price level and debt overhang. Unborn generations who will face the resolution of accumulated imbalances. Those permanently excluded from ownership in high-cost regions — their absence from the credit market is structural, not voluntary.
% DISAPPEARANCE_RATIONALE: If credit-driven price formation vanished overnight, housing prices would reset toward shelter-value fundamentals within 2-3 years. Financial sector would lose 30-40% of lending volume. Highly leveraged households would face negative equity crises. Rent trajectories would decouple from asset prices. The political economy of housing would reorganize around use-value rather than exchange-value.
% FOUNDING_PROBLEM: Post-war housing shortage required massive construction finance. Banks lacked long-term funding; central banks provided liquidity backstops; government created GSEs to securitize mortgages. The arrangement was built to solve a production problem — getting capital to builders.
% FOUNDING_PROBLEM_CORROBORATION: Central bank archives (Federal Reserve 1950s-60s minutes) show explicit construction-finance intent. Housing historians (Galbraith, Glaeser, Shiller) document the shift from production finance to asset inflation after 1980s deregulation. No independent analyst argues the current credit volume is needed for construction — housing starts are flat while mortgage debt triples.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__financialization_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__financialization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(price_formation_kernel__financialization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the financial sector captures a structural rent: the spread between credit creation cost (near-zero with central bank backstops) and mortgage rates, multiplied by volumes inflated by the very price appreciation the credit fuels. Suppression (0.65) reflects regulatory capture — macroprudential tools exist but are deployed asymmetrically (tightened for households, loosened for institutions). Theater ratio (0.45) captures that the 'housing finance' cover story still funds some construction but increasingly finances asset churn. Accessibility collapse (0.60) — alternatives (public housing, cooperative models, rent control) exist but are politically suppressed. Resistance (0.55) — YIMBY movements, tenant unions, and some central bank dissent exist but haven't altered the credit-price feedback loop.
 *
 * PERSPECTIVAL GAP:
 *   From the financial sector seat, this is a rope — they provide liquidity, manage risk, enable homeownership. From the first-time buyer seat, it's a snare — prices are engineered to extract maximum debt service. From the leveraged homeowner seat, it's a tangled rope — they benefit from the very structure that traps their children. The analytical observer sees the full feedback loop: credit → prices → collateral → more credit. The engine computes this seat divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial sector and central banks are structural beneficiaries (d near 0.0-0.2) — they control the credit spigot and collect the spread. Real estate investors are beneficiaries with mobile exit (d ~0.25). Leveraged homeowners are dual: beneficiaries of appreciation (d ~0.3) but payers of debt service (d ~0.7) — net directionality depends on leverage and time horizon. First-time buyers and renters are full targets (d ~0.9-1.0) — trapped, no arbitrage, bear the full price level. Mortgage-burdened households are high targets (d ~0.8) — constrained exit, crash exposure. The engine's derivation from beneficiary/victim + exit should capture this gradient; override for central_bank_regulators (see directionality_overrides) captures their captured-agenda-setter position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (construction finance) is dead — housing starts per capita are lower now than 1970 while mortgage debt/GDP has tripled. The arrangement persists because the financial sector extracts enough to defend it, and the political cost of unwinding (negative equity, bank losses) is prohibitive. This is classic mandatrophy: a coordination scaffold that became a tangled rope, now hardening toward snare as extraction concentrates and coordination atrophies. The founding_problem_status = dead + disappearance_verdict = world_rearranges mismatch should flag this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the price_formation_kernel a single contested commitment with multiple readings, or are these genuinely distinct constraints erroneously sharing a label?',
    'Test ε-invariance: if changing the observable (credit volumes vs. zoning restrictiveness vs. land values vs. construction costs) changes the authored extractiveness for a single story, the kernel decomposes. Each reading must author its own ε for its own structural claim.',
    'If the kernel is a single commitment, the readings are competing framings of one constraint — the engine''s cross-reading analysis applies. If distinct constraints, they should be separate stories linked by network.affects_constraints only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the price_formation_kernel is one commitment with multiple readings or a conflation of distinct constraints.').

omega_variable(
    extraction_coordination_boundary,
    'Is the credit-allocation coordination function structurally separable from the extraction function, or has the coordination function atrophied to pure cover?',
    'Counterfactual: if credit creation were restricted to construction finance only (via directed lending, public development banks), would housing production meet demand at lower prices? Historical evidence from post-war directed-credit regimes and contemporary public development banks.',
    'If separable, the constraint is a tangled rope with a genuine coordination core that could be preserved while stripping extraction. If inseparable, the coordination story is fully instrumental — the constraint is a snare wearing a rope''s skin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_coordination_boundary, empirical, 'Whether the coordination and extraction components of financialized price formation are structurally separable.').

omega_variable(
    naturalist_reading_ambiguity,
    'Does a natural equilibrium price exist beneath the financialization layer, or is ''natural equilibrium'' itself a construct produced by the financialized system?',
    'Long-run historical data: periods with constrained credit (1950s-70s, post-crisis 2009-12) show price-to-income ratios reverting to 3-4x. But the financialized system may have permanently altered preferences, expectations, and institutional memory such that ''natural'' is no longer reachable.',
    'If a natural equilibrium exists and is reachable, the financialization reading is a contingent distortion — scaffold→tangled_rope trajectory. If not, the naturalist_reading is a mountain claim falsified by history — the kernel itself is constructed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalist_reading_ambiguity, conceptual, 'Whether a pre-financialization natural price equilibrium exists and is reachable.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternatives (public housing, rent control, cooperative ownership) structural (policy capture, capital mobility) or internalized (households believe financialized ownership is the only path to security)?',
    'Post-exit suppression trajectory: in jurisdictions that implemented strong alternatives (Vienna social housing, Singapore HDB), did demand for financialized ownership persist? Survey data on housing preferences across regimes.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them. If structural, policy change alone could open alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in financialized housing markets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1970, price_formation_kernel__financialization_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__financialization_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(pric_tr_t1990, price_formation_kernel__financialization_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(pric_tr_t2000, price_formation_kernel__financialization_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(pric_tr_t2010, price_formation_kernel__financialization_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(pric_tr_t2020, price_formation_kernel__financialization_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(pric_tr_t2025, price_formation_kernel__financialization_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(pric_be_t1970, price_formation_kernel__financialization_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__financialization_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(pric_be_t1990, price_formation_kernel__financialization_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(pric_be_t2000, price_formation_kernel__financialization_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(pric_be_t2010, price_formation_kernel__financialization_reading, base_extractiveness, 2010, 0.71).
narrative_ontology:measurement(pric_be_t2020, price_formation_kernel__financialization_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(pric_be_t2025, price_formation_kernel__financialization_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1970, price_formation_kernel__financialization_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__financialization_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(pric_su_t1990, price_formation_kernel__financialization_reading, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement(pric_su_t2000, price_formation_kernel__financialization_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(pric_su_t2010, price_formation_kernel__financialization_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(pric_su_t2020, price_formation_kernel__financialization_reading, suppression_requirement, 2020, 0.64).
narrative_ontology:measurement(pric_su_t2025, price_formation_kernel__financialization_reading, suppression_requirement, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__financialization_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, mortgage_market_structure).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, housing_supply_constraints).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, wealth_inequality_dynamics).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, intergenerational_wealth_transfer).

% DUAL FORMULATION NOTE:
% This constraint is the financialization_reading of the price_formation_kernel. The naturalist_reading, institutional_reading, and georgist_reading are sibling constraints with different ε values and different structural claims about what drives prices. They are linked as a constraint family via network.affects_constraints. The financialization reading asserts credit expansion as primary driver (high ε); naturalist asserts natural equilibrium (ε≈0); institutional asserts policy design (moderate ε, different beneficiaries); georgist asserts land monopoly (high ε, different victim structure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__financialization_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
