% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__financialization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: price_formation_kernel__financialization_reading
 *   human_readable: Credit-Driven Housing Price Formation (Financialization Reading)
 *   domain: political economy/housing markets/institutional analysis
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   housing price formation; this file carries the financialization_reading:
 *   the claim that prices are set at the margin by credit expansion,
 *   amplified by asset-price feedback loops, and anchored by demand for
 *   housing as a financial asset rather than as shelter. The standing
 *   arrangement under contest is the housing-finance system itself: mortgage
 *   credit channels savings into owner-occupied housing, price levels scale
 *   with leverage availability, and homes function first as collateral.
 *   Assessed by this reading's own lights, the arrangement is substantially
 *   extractive: the financial complex collects returns scaled to credit
 *   volume and transaction churn, while households bear debt service sized to
 *   financialized prices and carry the downside when the feedback loop
 *   reverses. Per the epsilon-invariance principle, the sibling readings
 *   (naturalist, institutional, georgist) are separate constraints with their
 *   own epsilon values, beneficiary/victim structures, and classifications;
 *   nothing about them is averaged into this file. The claim/metric
 *   independence rule is honored: claimed_type is authored from the
 *   structural judgment that the arrangement possesses BOTH a genuine
 *   coordination function (intertemporal consumption smoothing, savings
 *   intermediation) AND asymmetric extraction running through the same
 *   structure, while the metrics are authored from the descriptive record of
 *   its operation.
 *
 * KEY AGENTS:
 *   - - central_banks_and_regulators: Agenda-setter (institutional/identity_locked) — administers the rate and collateral conditions the arrangement runs on; post-2008 mandate fused with asset-price support
 *   - - mortgage_finance_sector: Primary beneficiary (institutional/arbitrage) — collects interest, fees, and trading revenue scaled to credit volume; co-writes underwriting standards
 *   - - institutional_asset_managers: Secondary beneficiary (institutional/arbitrage) — holds housing collateral as yield for managed pools; fee income scales with assets
 *   - - existing_equity_households: Incumbent beneficiary (organized/identity_locked) — collects appreciation usable as collateral; bears crash exposure and defends the arrangement politically
 *   - - first_time_buyer_households: Primary target (moderate/trapped) — bears record income-multiple debt service; every access route runs through the financialized market
 *   - - priced_out_renter_households: Target (powerless/trapped) — pays rents anchored to leveraged purchase prices without accumulating the asset
 *   - - young_saver_households: Target (moderate/trapped) — deposit hurdle appreciates faster than saving; delay converts to financialized rent
 *   - - community_land_trust_advocates: Excluded voice (moderate/constrained) — organizes decommodified tenure alternatives with no seat in credit-policy forums
 *   - - financial_stability_economists: Analytical observer (analytical/analytical) — documents the credit-price nexus across countries and centuries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.78).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.62).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Credit-Driven Housing Price Formation (Financialization Reading)").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "political economy/housing markets/institutional analysis").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, '646ae075-533b-4e0d-a56c-b4be38c40b94').
narrative_ontology:cs_kernel_codification('646ae075-533b-4e0d-a56c-b4be38c40b94', distributed).
narrative_ontology:cs_authority_grounding('646ae075-533b-4e0d-a56c-b4be38c40b94', expertise).
narrative_ontology:cs_interpretation_layer_present('646ae075-533b-4e0d-a56c-b4be38c40b94').
narrative_ontology:cs_reading_relation('646ae075-533b-4e0d-a56c-b4be38c40b94', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('646ae075-533b-4e0d-a56c-b4be38c40b94', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('646ae075-533b-4e0d-a56c-b4be38c40b94', price_formation_kernel__georgist_reading, influences).
narrative_ontology:cs_axiom('646ae075-533b-4e0d-a56c-b4be38c40b94', foundational, credit_expansion_drives_price_formation).
narrative_ontology:cs_axiom_status(credit_expansion_drives_price_formation, holdable).
narrative_ontology:cs_axiom_grounding('646ae075-533b-4e0d-a56c-b4be38c40b94', credit_expansion_drives_price_formation, empirically_contingent).
narrative_ontology:cs_axiom('646ae075-533b-4e0d-a56c-b4be38c40b94', foundational, housing_valued_as_collateral_first).
narrative_ontology:cs_axiom_status(housing_valued_as_collateral_first, holdable).
narrative_ontology:cs_axiom_grounding('646ae075-533b-4e0d-a56c-b4be38c40b94', housing_valued_as_collateral_first, empirically_contingent).
narrative_ontology:cs_axiom('646ae075-533b-4e0d-a56c-b4be38c40b94', secondary, asset_feedback_loops_amplify_credit_cycles).
narrative_ontology:cs_axiom_status(asset_feedback_loops_amplify_credit_cycles, holdable).
narrative_ontology:cs_axiom_grounding('646ae075-533b-4e0d-a56c-b4be38c40b94', asset_feedback_loops_amplify_credit_cycles, empirically_contingent).
narrative_ontology:cs_reference_frame('646ae075-533b-4e0d-a56c-b4be38c40b94', credit_endogenous_price_formation).
narrative_ontology:cs_drift_state('646ae075-533b-4e0d-a56c-b4be38c40b94', post_2008_financial_stability_turn, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('646ae075-533b-4e0d-a56c-b4be38c40b94', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, mortgage_finance_sector).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, institutional_asset_managers).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, existing_equity_households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_buyer_households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, priced_out_renter_households).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, young_saver_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, existing_equity_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the policy rates and collateral frameworks that determine how much credit can expand against housing collateral, and supervise the institutions that originate it. Since the 2008 crisis their mandates have fused with asset-price stability: their toolkit now includes purchasing mortgage-backed securities and backstopping funding markets, and their public legitimacy rests on deploying it. Stepping out of the support role would mean dismantling instruments their institutional identity has grown around.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, central_banks_and_regulators, agenda_setter,
    institutional, generational, identity_locked, global).

% Originates, services, securitizes, and trades housing debt; earns interest spreads, origination fees, and trading revenue proportional to credit volume and transaction churn. Participates in drafting the underwriting and capital standards it operates under through comment processes and lobbying. Can restructure products, shift portfolios across borders, and rotate into adjacent lending markets if housing credit turns unprofitable.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, mortgage_finance_sector, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, mortgage_finance_sector, agenda_setter).

% Hold housing-backed securities and rental portfolios as yield assets for pension funds, insurers, and sovereign wealth pools; management fees scale with assets under management, which housing collateral expands. Allocate globally and can reduce housing exposure when relative yield deteriorates, passing timing risk to less mobile holders.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, institutional_asset_managers, beneficiary,
    institutional, generational, arbitrage, global).

% Own homes acquired before the major run-ups; their balance sheets carry appreciation gains usable as collateral for consumption and further investment, and they vote and organize to defend measures that protect those gains. Their wealth is concentrated in the house itself and their self-conception as owners is bound to its value; selling means re-entering the same market as a buyer. They also carry property taxes, maintenance, and crash exposure on the concentrated position.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, existing_equity_households, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__financialization_reading, existing_equity_households, payer).

% Take on record multiples of household income in mortgage debt to enter ownership; the deposit hurdle grows faster than saving because prices rise with credit availability. Every access route — borrowing to buy, or renting while saving — runs through the financialized market, and delaying entry converts the wait into years of financialized rent.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_buyer_households, payer,
    moderate, biographical, trapped, national).

% Pay rents set by landlord cost structures anchored to leveraged purchase prices; excluded from ownership by deposit hurdles, they bear the financialized price level without accumulating the asset. Mobility is limited by local labor markets, family ties, and the fact that destination cities price on the same logic.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, priced_out_renter_households, payer,
    powerless, biographical, trapped, national).

% Save toward deposits that appreciate more slowly than the target asset; each year of disciplined saving buys a smaller share of a house. Opting out of ownership leaves them exposed to the same rent levels, so the savings strategy is a losing position under either branch.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, young_saver_households, payer,
    moderate, biographical, trapped, national).

% Organize permanently affordable, collectively held tenure models that remove land from speculative circulation. They bring structural alternatives to planning hearings and legislative consultations but hold no seat in the credit-policy forums where the agenda is set by monetary authorities, prudential regulators, and industry participants.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, community_land_trust_advocates, excluded,
    moderate, generational, constrained, local).

% Research the credit-price nexus across countries and centuries; document boom-bust regularities, advise central banks and international bodies, and publish the findings that periodically reshape the policy conversation. Their seat observes the whole structure without collecting from it or paying into it.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, financial_stability_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__financialization_reading, mortgage_finance_sector).
narrative_ontology:fixing_cost_class(price_formation_kernel__financialization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mortgage credit solves an intertemporal coordination problem: households consume shelter now against future income, pooled savings are intermediated into long-lived housing assets, and standardized collateral documentation lets the credit system price and trade housing-linked obligations at scale.
% TRANSFER_FUNCTION: Moves debt-service payments (interest, fees, trading margins) from mortgaged households to the financial complex, proportional to credit volume; moves appreciation gains to incumbent owners and asset holders; and transfers crash losses downward onto leveraged households and outward onto taxpayers through backstop mechanisms.
% ABSENT_VOICES: Future buyer cohorts, priced-out renters, and decommodified-tenure organizers (community land trusts, limited-equity cooperatives) would object that the arrangement prices a necessity off shelter value; they are absent from credit-policy forums, where seats are held by monetary authorities, prudential regulators, and industry participants. Their objections surface only episodically, through housing protests and affordability politics, and are not represented in the standard-setting process.
% DISAPPEARANCE_RATIONALE: If the credit-price feedback loop ceased overnight, price levels would fall toward income-anchored shelter value, existing mortgage collateral would be marked down forcing balance-sheet restructuring across the financial complex, incumbent equity would compress, and ownership patterns would shift toward cash-purchasers and non-financialized tenure forms. The arrangement's disappearance rearranges household balance sheets, financial-sector revenue, and the political coalition around housing simultaneously.
% FOUNDING_PROBLEM: Post-war housing policy faced a mass-access problem: broadening owner-occupation required intermediating household savings into long-term mortgage credit at scale, with standardized underwriting to make the loans safe to hold.
% FOUNDING_PROBLEM_CORROBORATION: Industry and government guarantor voices attest the access problem is live, citing down-payment barriers and underserved markets. Corroborating sources OUTSIDE the benefiting parties cut both ways: financial-history research (cross-country studies of credit booms and crises published in the financial-stability literature) attests that the arrangement now systematically produces price decoupling and crash regularity beyond its access function, while tenant federations and affordability researchers attest the access function has been subordinated to asset-market function. No party outside the dispute denies that the original intermediation problem existed; the contest is over whether it remains the arrangement's operative purpose.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__financialization_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.78 at interval end) because the price level itself is the extraction surface: households pay finance-mediated prices for a necessity, and debt service scales to those prices rather than to shelter value. Suppression is moderate-high (0.62) and structural rather than coercive: shelter is non-optional, every tenure channel routes through the financialized market, and the deposit-plus-rent double bind closes the waiting strategy. Theater ratio (0.42) reflects a real but partially performative functional layer: underwriting, appraisal, and risk management do allocate credit, but a growing share of activity is compliance and risk-model ceremony layered over an extractive core — peaking around the crisis when model theater was most exposed, then partially subsiding as compliance routines normalized. The temporal series share one grid (1980, 1988, 1996, 2004, 2008, 2012, 2025) with every tracked metric authored at every point. Extraction rises monotonically with the financialization era, spikes at 2008 when losses crystallized on households while the arrangement was preserved, dips during post-crisis deleveraging, then resumes climbing. Suppression_requirement traces enforcement-capacity change specifically: steady build-up through securitization expansion, an emergency peak in 2008-2012 (facility creation, guarantee extensions, asset purchases), then partial normalization to a permanently higher plateau than the pre-crisis baseline — the backstop apparatus did not demobilize. Coalition potential among the victim seats is weak: debt contracts are individualized, incumbents are organized as a defensive voting bloc, and entrants are dispersed across cohorts and geographies, which is why moderate-power victims do not convert numbers into structural power.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the mortgage_finance_sector seat, the arrangement is a functioning intermediation machine it built and staffed — coordination with profitable margins, experienced as rope-flavored. From the first_time_buyer and renter seats, the identical structure operates as enforced extraction: the same credit expansion that generates the sector's revenue generates their debt burden and entry denial — snare-flavored experience riding a real coordination core. The incumbent-owner seat experiences the arrangement as wealth preservation and identity continuity, and reads reform proposals as expropriation. The central-bank seat experiences the arrangement as its stabilization duty: having fused its mandate to asset-price support, it perceives the feedback loop as a fragility to manage rather than a distribution to adjudicate. The engine computes these divergent per-seat classifications from the structural data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the financial complex near the beneficiary end: mortgage_finance_sector and institutional_asset_managers combine beneficiary position with arbitrage-grade exit (portfolio rotation, product restructuring, cross-border allocation), damping their effective extraction toward subsidy. existing_equity_households are beneficiaries with identity_locked exit — their d sits low, but the lock-in amplifies their defensive weight rather than their extraction exposure. Victim declarations place the household seats near the target end: trapped exit (no non-financialized tenure channel at scale) pushes first_time_buyers, renters, and young savers toward the full-target pole, so effective extraction is amplified for them relative to mobile agents bearing nominally similar costs. The central-bank seat declares neither beneficiary nor victim position; its structural dependency on the arrangement's continuity places it partway toward the beneficiary side, below the financier seats. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and by the national-to-global scope of the arrangement, which the engine applies.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the arrangement as a pure snare would erase its genuine coordination function: mortgage credit really does solve an intertemporal problem (consume shelter now against lifetime income; intermediate pooled savings into long-lived assets), and abolishing the credit channel outright would not return prices to shelter value but would collapse access smoothing. Reading it as a piton would erase the extraction: the arrangement is not maintained by inertia or performance — it is actively enforced through collateral frameworks, guarantee institutions, and crisis backstops, and its beneficiaries capture concentrated, growing rents. The tangled_rope classification holds both facts in one structure. Mandatrophy is NOT resolved: the founding problem (broadening ownership access through credit intermediation) is contested rather than dead — access expansion persists at the margin while the arrangement's center of gravity has migrated to asset-market function — so no sunset or retirement logic applies, and the persistence question stays open pending the kernel-level omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the financialization_reading of price_formation_kernel; if the naturalist_reading, institutional_reading, or georgist_reading were adopted instead, how would the beneficiary/victim structure and epsilon of the standing arrangement be restructured?',
    'Comparative causal-attribution analysis across readings: decompose observed price growth into credit-supply, land-rent, regulatory, and scarcity components using the same panel data, then re-author each reading''s story from the shared decomposition.',
    'Under the naturalist_reading, measured extraction collapses toward the coordination floor (prices reflect objective scarcity; finance merely intermediates). Under the georgist_reading, the victim set shifts toward improvers taxed via capitalized land rent and the beneficiary set toward landholders. Under the institutional_reading, the agenda_setter set expands to zoning authorities and tax legislators. This file''s classification holds only for the financialization attribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Kernel-level contestation: which reading of price formation governs the structural analysis.').

omega_variable(
    credit_causality_direction,
    'Does credit expansion causally drive housing price growth, or does credit merely accommodate price growth generated by exogenous supply constraints and demand?',
    'Identification through credit-supply shocks exogenous to local housing demand: branching-deregulation discontinuities, sudden lender exits, and cross-country variation in loan-to-value regimes under fixed supply elasticities.',
    'If credit accommodates rather than drives, the financial sector''s extraction attribution weakens substantially, epsilon falls, and explanatory weight shifts toward the institutional_reading sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_causality_direction, empirical, 'Direction of causality between credit expansion and price formation.').

omega_variable(
    decoupled_shelter_value_magnitude,
    'How large is the wedge between financialized price levels and the counterfactual price levels a shelter-use market would clear at?',
    'Cross-country and cross-era comparison of price-to-income and price-to-rent ratios under varying leverage availability, controlling for construction costs, interest rates, and supply elasticity.',
    'A large wedge confirms that the price level itself is the extraction surface (households pay finance-mediated prices for shelter); a small wedge implies most measured extraction is ordinary intermediation cost and the arrangement sits nearer the rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupled_shelter_value_magnitude, empirical, 'Magnitude of decoupling between financialized prices and shelter value.').

omega_variable(
    crash_loss_incidence,
    'When the asset-price feedback loop reverses, do losses ultimately rest on leveraged households, or are they socialized onto taxpayers through bailout and backstop mechanisms?',
    'Fiscal-cost accounting of crisis interventions set against household wealth destruction and foreclosure counts across successive downturns.',
    'Full socialization adds taxpayers to the victim set and strengthens the enforcement-dependence of the arrangement; full private incidence concentrates losses on the declared household victims and simplifies the directional structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crash_loss_incidence, empirical, 'Ultimate incidence of crash losses across households and taxpayers.').

omega_variable(
    homeownership_identity_fusion,
    'Is incumbent-owner defense of the arrangement a wealth-protection preference, or identity fusion in which the self-conception of being a homeowner is constituted by the asset''s value?',
    'Behavioral and survey divergence tests: whether owner political behavior tracks net equity positions alone, or persists when wealth effects are neutralized (negative-equity owners defending price-support policy).',
    'Identity fusion raises the effective lock-in of the incumbent seat beyond wealth calculation, deepening the asymmetry between incumbent and entrant seats; pure wealth preference would make incumbent opposition negotiable through compensation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homeownership_identity_fusion, conceptual, 'Whether incumbent-owner attachment is preference or identity fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__financialization_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(pric_tr_t1988, price_formation_kernel__financialization_reading, theater_ratio, 1988, 0.23).
narrative_ontology:measurement(pric_tr_t1996, price_formation_kernel__financialization_reading, theater_ratio, 1996, 0.27).
narrative_ontology:measurement(pric_tr_t2004, price_formation_kernel__financialization_reading, theater_ratio, 2004, 0.35).
narrative_ontology:measurement(pric_tr_t2008, price_formation_kernel__financialization_reading, theater_ratio, 2008, 0.44).
narrative_ontology:measurement(pric_tr_t2012, price_formation_kernel__financialization_reading, theater_ratio, 2012, 0.47).
narrative_ontology:measurement(pric_tr_t2025, price_formation_kernel__financialization_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__financialization_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(pric_be_t1988, price_formation_kernel__financialization_reading, base_extractiveness, 1988, 0.45).
narrative_ontology:measurement(pric_be_t1996, price_formation_kernel__financialization_reading, base_extractiveness, 1996, 0.53).
narrative_ontology:measurement(pric_be_t2004, price_formation_kernel__financialization_reading, base_extractiveness, 2004, 0.67).
narrative_ontology:measurement(pric_be_t2008, price_formation_kernel__financialization_reading, base_extractiveness, 2008, 0.73).
narrative_ontology:measurement(pric_be_t2012, price_formation_kernel__financialization_reading, base_extractiveness, 2012, 0.69).
narrative_ontology:measurement(pric_be_t2025, price_formation_kernel__financialization_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__financialization_reading, suppression_requirement, 1980, 0.32).
narrative_ontology:measurement(pric_su_t1988, price_formation_kernel__financialization_reading, suppression_requirement, 1988, 0.39).
narrative_ontology:measurement(pric_su_t1996, price_formation_kernel__financialization_reading, suppression_requirement, 1996, 0.45).
narrative_ontology:measurement(pric_su_t2004, price_formation_kernel__financialization_reading, suppression_requirement, 2004, 0.55).
narrative_ontology:measurement(pric_su_t2008, price_formation_kernel__financialization_reading, suppression_requirement, 2008, 0.71).
narrative_ontology:measurement(pric_su_t2012, price_formation_kernel__financialization_reading, suppression_requirement, 2012, 0.74).
narrative_ontology:measurement(pric_su_t2025, price_formation_kernel__financialization_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__georgist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how housing prices form' decomposes into four structurally distinct readings of one kernel, each with its own epsilon and stakeholder structure. This story (financialization_reading) is the contested middle of the family: the naturalist_reading is its upstream sibling (efficient-markets framing historically cited AGAINST financialization claims), while the institutional_reading and georgist_reading are downstream siblings whose variables this reading reframes as transmission channels (regulation and land rent as the plumbing through which credit flows). All four stories link one another via affects_constraints; epsilon divergence across the family is the measurement the family exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
