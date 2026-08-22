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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Housing Price Formation via Credit Expansion and Asset-Price Feedback (Financialization Reading)
 *   domain: political_economy/housing_markets/finance
 *
 * SUMMARY:
 *   This constraint story instantiates the FINANCIALIZATION READING of the
 *   price-formation kernel: housing prices are understood as driven by credit
 *   expansion, asset-price feedback loops, and institutional-investor demand
 *   for housing as a financial asset class. Under this reading, price
 *   formation is decoupled from shelter fundamentals (household income,
 *   construction costs, population growth) and instead determined by credit
 *   availability, leverage ratios, securitization capacity, and institutional
 *   capital allocation. The financial sector (banks, originators,
 *   securitizers, asset managers) benefits from origination volume, servicing
 *   fees, trading spreads, and secondary-market liquidity. Households that
 *   are early entrants or asset holders benefit from price appreciation
 *   without corresponding earning power. First-time buyers, renters, and
 *   labor-constrained households bear debt service, crash risk, and
 *   suppressed alternatives. The constraint is CLAIMED as tangled rope
 *   (genuine credit-market coordination benefit exists) while the authored
 *   metrics describe substantially extractive, actively enforced operation
 *   (suppression at 0.71, extractiveness trajectory rising to 0.78) — this
 *   divergence is measured by the engine, not reconciled in the authoring.
 *
 * KEY AGENTS:
 *   - financial_sector_institutions: agenda-setter (institutional power), sets credit terms, determines leverage access, designs securitization structures; primary beneficiary
 *   - existing_asset_holders: beneficiary (powerful), accrues wealth from appreciation without earning power
 *   - institutional_investors: beneficiary (institutional power), arbitrage across markets, leverage to amplify yields
 *   - first_time_homebuyers: payer (moderate power, identity-locked exit), forced into leverage at prices set by financial markets
 *   - renters: payer (powerless, trapped exit), excluded from appreciation and bear cost inflation from landlord leverage
 *   - mortgage_backed_security_investors: beneficiary (institutional power), profit from securitization and tail-risk transfer
 *   - regional_housing_regulators: observer (institutional power, analytical exit), hold leverage-control tools but face political constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__financialization_reading, 0.78).
domain_priors:suppression_score(price_formation_kernel__financialization_reading, 0.71).
domain_priors:theater_ratio(price_formation_kernel__financialization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(price_formation_kernel__financialization_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__financialization_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__financialization_reading, "Housing Price Formation via Credit Expansion and Asset-Price Feedback (Financialization Reading)").
narrative_ontology:topic_domain(price_formation_kernel__financialization_reading, "political_economy/housing_markets/finance").

domain_priors:requires_active_enforcement(price_formation_kernel__financialization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__financialization_reading, '0b65cd01-dd42-4d15-b678-ef66e9eee388').
narrative_ontology:cs_kernel_codification('0b65cd01-dd42-4d15-b678-ef66e9eee388', distributed).
narrative_ontology:cs_authority_grounding('0b65cd01-dd42-4d15-b678-ef66e9eee388', extraction).
narrative_ontology:cs_interpretation_layer_present('0b65cd01-dd42-4d15-b678-ef66e9eee388').
narrative_ontology:cs_reading_relation('0b65cd01-dd42-4d15-b678-ef66e9eee388', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b65cd01-dd42-4d15-b678-ef66e9eee388', price_formation_kernel__institutional_reading, influences).
narrative_ontology:cs_reading_relation('0b65cd01-dd42-4d15-b678-ef66e9eee388', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_axiom('0b65cd01-dd42-4d15-b678-ef66e9eee388', foundational, credit_expansion_is_causal_price_driver).
narrative_ontology:cs_axiom_status(credit_expansion_is_causal_price_driver, holdable).
narrative_ontology:cs_axiom_grounding('0b65cd01-dd42-4d15-b678-ef66e9eee388', credit_expansion_is_causal_price_driver, empirically_contingent).
narrative_ontology:cs_axiom('0b65cd01-dd42-4d15-b678-ef66e9eee388', foundational, financial_sector_benefits_from_leverage_volume).
narrative_ontology:cs_axiom_status(financial_sector_benefits_from_leverage_volume, holdable).
narrative_ontology:cs_axiom_grounding('0b65cd01-dd42-4d15-b678-ef66e9eee388', financial_sector_benefits_from_leverage_volume, empirically_contingent).
narrative_ontology:cs_reference_frame('0b65cd01-dd42-4d15-b678-ef66e9eee388', credit_mediated_homeownership_framework).
narrative_ontology:cs_drift_state('0b65cd01-dd42-4d15-b678-ef66e9eee388', contemporary_institutional_investor_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0b65cd01-dd42-4d15-b678-ef66e9eee388', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__financialization_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, financial_sector_institutions).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, institutional_investors).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, existing_asset_holders).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, first_time_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, leverage_constrained_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__financialization_reading, mortgage_backed_security_investors).
narrative_ontology:constraint_victim(price_formation_kernel__financialization_reading, marginally_creditworthy_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Banks, mortgage originators, asset-backed securitizers, and institutional investors that profit from housing credit creation and volume. They set underwriting standards, determine who gets leverage and at what cost, and can package mortgages into tradable securities that transfer tail risk to bond investors. Their interest aligns with expanding credit access (higher origination volume), maintaining asset-price appreciation (ensures collateral and secondary market liquidity), and defending the institutional-investor preference for housing as a financial asset class. Exit is near-costless: capital rotates to other asset classes or geographies if housing margins compress.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, financial_sector_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Households that own housing already, older cohorts, and investors holding residential property. They benefit from price appreciation driven by credit expansion and leverage demand (wealth accumulation without corresponding earning power). Their exit from the constraint is optional: they can sell into a rising market and redeploy capital. The constraint's persistence increases their asset values annually; they have every reason to defend it politically and socially.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, existing_asset_holders, beneficiary,
    powerful, biographical, mobile, national).

% Pension funds, insurance companies, sovereign wealth funds, and real-estate investment trusts that treat housing as a financial asset and can deploy capital across markets. They profit from price appreciation, rental yields enhanced by leverage-inflated prices, and secondary mortgage markets. They are net beneficiaries of credit expansion because it creates both demand (institutional buyers can leverage up) and supply (mortgaged properties yield higher rents). Exit is trivial: reallocation to equities, bonds, or alternate geographies.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, institutional_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Households attempting to buy a primary residence for the first time, typically younger cohorts with less accumulated wealth. They face prices elevated by credit expansion and institutional capital demand. They are forced to take on leverage at levels set by a banking system optimized for volume and secondary-market liquidity, not for borrower sustainability. They bear debt service, crash risk (negative equity, forced sale during downturns), and the psychological burden of obligation. Exit is identity-fused: homeownership is culturally constructed as adult achievement and family stability; renting is treated as failure or temporary; the exit from attempting to buy is narrated as life-plan collapse. Their power to resist price offers and financing terms is minimal: the choice set is constrained by banking gatekeeping and cultural expectation.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, first_time_homebuyers, payer,
    moderate, biographical, identity_locked, national).

% Households that do not own housing and cannot or will not enter the leverage-based ownership market. They are excluded from credit-driven price appreciation (no asset accumulation path) and bear rising rents as landlords capitalize the rising asset values and leverage available to property investors. They have no coalition power (atomized renters), no exit (cannot leave the housing market), and no alternative (buying requires leverage they cannot access due to income constraints, credit history, or age). They pay higher rents in markets where institutional investors can leverage up and outbid owner-occupants.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, renters, payer,
    powerless, immediate, trapped, local).

% Households with credit scores, income, or down-payment resources that fall below lending standards during periods when financial institutions tighten credit gatekeeping (post-crisis, regulatory-tightening periods). They are excluded from credit expansion benefits but still face leverage-inflated prices. They cannot buy into price appreciation and are stuck renting until financial conditions loosen again (at which point they are reintroduced to the market at higher prices). They bear crash risk indirectly through reduced housing mobility and forced long-term renting.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, marginally_creditworthy_households, payer,
    powerless, biographical, constrained, national).

% Bond investors holding mortgage-backed securities and collateralized debt obligations: pension funds, insurance companies, foreign central banks, etc. They profit from securitization spreads and payment flows; the tail risk (default concentration, negative-convexity crashes) is transferred away from originators to them. They benefit from high origination volume (more securities to trade) and price appreciation (backing asset values provide yield enhancement). Exit is by selling the securities or rotating to other fixed-income instruments.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, mortgage_backed_security_investors, beneficiary,
    institutional, generational, arbitrage, global).

% Central banks and financial regulators (Federal Reserve, national banking authorities, mortgage market regulators) that set interest rates, leverage limits, lending standards, and capital requirements. They have the technical capacity to tighten credit and alter price-formation dynamics but face political constraints: tightening credit slows nominal growth and household net worth accumulation, which triggers resistance from existing asset holders and financial-sector incumbents. Their 'exit' is choosing a different regulatory stance, but that choice has broad macroeconomic consequences they bear responsibility for.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, regional_housing_regulators, observer,
    institutional, generational, analytical, national).

% Households whose income trajectories are insufficient to service debt at leverage-inflated prices, even if credit were made available to them. They are structurally excluded from the housing market because their earning power never catches up to what credit expansion has done to price levels. If they were included in credit markets, they would be prime candidates for default and negative-amortization spirals. They are kept out not by explicit gatekeeping but by the mathematics of leverage and income distribution.
narrative_ontology:constraint_stakeholder(price_formation_kernel__financialization_reading, labor_supply_constrained_market_participants, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__financialization_reading, financial_sector_institutions).
narrative_ontology:fixing_cost_class(price_formation_kernel__financialization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Credit intermediation: borrowers and lenders are matched at scale, transaction costs are reduced (centralized underwriting, standardized documentation, secondary markets), and the temporal mismatch between young households' housing need and their wealth accumulation is bridged by financial intermediaries borrowing long on short time frames. Price discovery happens through market clearing (supply, demand, credit conditions interact).
% TRANSFER_FUNCTION: Moves wealth from future earnings (debt service, interest payments) to financial institutions, existing asset holders, and institutional investors through: (a) origination and servicing fees, (b) interest-rate spreads, (c) asset-price appreciation captured by early holders, (d) reduced rental yields for non-leveraged renters because institutional leverage inflates asset-base requirement. The flow is from households with uncertain future income or no asset base (first-time buyers, renters) to institutions with stable funding and capital markets access.
% ABSENT_VOICES: Labor-constrained households unable to service any offered leverage (excluded by mathematics, not policy) would object to prices and underwriting standards but are kept outside the credit conversation. Land-value philosophers (Georgist reading) would argue the constraint misattributes price formation to finance when the root is unearned location rent. Naturalist reading proponents would argue credit merely accelerates discovery of true scarcity-driven values and is not itself extractive. These voices are not at the table when credit and underwriting standards are set.
% DISAPPEARANCE_RATIONALE: If credit expansion and institutional-investor participation in housing were removed overnight, prices would collapse to levels supportable by owner-occupant fundamentals (household income multiples, savings rates, actual shelter demand). The financial sector would lose origination volume, securitization profits, and asset-management fees. Existing asset holders would experience negative wealth shocks. First-time buyers would face lower entry prices but also lower credit availability (less favorable terms, higher down-payment requirements). The constraint's disappearance would reorganize the entire housing market around rental demand, savings accumulation, and income-based affordability rather than leverage availability.
% FOUNDING_PROBLEM: Post-WWII and accelerating post-1980: households wanted to own homes but lacked capital; credit intermediaries could bridge that gap; financial deregulation and securitization technology made credit scaling cheap and profitable. The founding problem is real: young households have legitimate housing needs that precede wealth accumulation. The question this reading contests is whether the solution is built on sustainable fundamentals or is parasitic on ever-accelerating credit creation.
% FOUNDING_PROBLEM_CORROBORATION: Financial regulators and economists from the financial sector attest the founding problem is live and credit expansion is the solution. Household financial stress data (negative savings rates, high payment-to-income ratios, low down payments) from Federal Reserve surveys and labor statistics attests the problem persists. Economic historians and heterodox economists attest that the problem WAS solved circa 1960–1980 (housing-price-to-income ratios were stable), then credit expansion decoupled from income growth starting in the 1980s, suggesting the original problem was solved and the current credit escalation is capturing economic rents rather than enabling homeownership. This corroboration is contested across institutional boundaries, not agreed upon.
narrative_ontology:disappearance_verdict(price_formation_kernel__financialization_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__financialization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__financialization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__financialization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__financialization_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.38 at interval start to 0.78 at end, reflecting the historical trajectory from post-WWII credit expansion (solving a real coordination problem: young households need housing capital) to contemporary financialization (credit expansion driving prices independent of income growth, institutional leverage amplifying asset-price feedback, financial-sector gains decoupling from borrower welfare). Theater ratio rises modestly from 0.18 to 0.42: the coordination justification (credit intermediation, price discovery) is real and visible, but an increasing share of activity (underwriting complexity, documentation, securitization repackaging) exists to defend extractive structures rather than improve borrower outcomes. Suppression is high (0.71) and stable after t=20: alternatives collapse (renters cannot save for down payments in leverage-inflated markets; first-time buyers cannot exit the constraint because identity-fusion with homeownership is cultural; labor-constrained households are excluded by mathematics). The stability of suppression after t=20 reflects that the constraint has matured — it no longer needs to intensify suppressive machinery because the structural exclusion is complete. One shared time grid: every metric is authored at every time point (0, 5, 10, 15, 20, 25, 30, 35, 40), enabling drift analysis across the full interval.
 *
 * PERSPECTIVAL GAP:
 *   The financial-sector agenda-setter and the first-time-buyer payer compute radically different types from identical structural data. From the agenda-setter's seat, the constraint is rope: they coordinate credit markets, enable homeownership, reduce transaction costs, and operate within regulatory bounds. From the first-time-buyer's seat, the constraint is snare: prices are set by leverage availability (not income), debt service is mandatory, exit is identity-fused, and alternatives (renting, waiting to save) are suppressed by cultural and market forces. Existing asset holders occupy a third perspective: for them, the constraint is mountain-like or pure benefit (natural appreciation), because price dynamics align with their interests. The engine computes this divergence from power levels, directionality, and exit options: institutional agenda-setter with arbitrage exit sees coordination; moderate-power payer with identity-locked exit sees extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial-sector institutions: powerful, institutional, arbitrage exit. They set terms (high d-power), collect fees and spreads independent of borrower fate (beneficiary, d near 0.0). Directionality: 0.1 (full beneficiary). Existing asset holders: powerful, can exit by selling, benefit from appreciation without productive contribution. Directionality: 0.15 (beneficiary, with option value). Institutional investors: institutional power, global exit via reallocation. Directionality: 0.15 (beneficiary). First-time homebuyers: moderate power, identity-locked exit (cannot exit without narrative collapse), pay debt service, bear crash risk, price set by markets they don't control. Directionality: 0.82 (target). Renters: powerless, trapped exit, bear cost inflation, no appreciation path. Directionality: 0.88 (full target). Labor-constrained households: powerless, mathematically excluded (mathematically trapped is distinct from contractually trapped). Directionality: 0.90 (deeper target — excluded by structure). Mortgage-backed security investors: institutional, arbitrage exit, profit from spreads. Directionality: 0.12 (beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows incipient mandatrophy structure: the founding problem (young households need capital to buy homes) was largely solved by 1980 (price-to-income ratios were stable 1960–1980). The constraint persists not because the founding problem is live but because credit expansion is now a self-sustaining institutional system: financial institutions profit from volume, existing asset holders benefit from appreciation, and regulatory bodies face political pressure from wealth-effect benefits. The theater_ratio trajectory rising to 0.42 indicates performative activity increasing: regulatory compliance, stress testing, loan documentation, and securitization repackaging exist to maintain the arrangement, not to improve borrower outcomes. However, the constraint has not yet crossed into full piton status because the founding-problem_status is contested (some believe homeownership access is still the binding constraint) and disappearance_verdict is world_rearranges (the arrangement is not purely inertial; it genuinely structures the market). A pure piton would show disappearance_verdict approaching world_unchanged. The classification holds at tangled_rope with high extractiveness pending resolution of the contested founding-problem status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is price formation driven by credit expansion and financial leverage cycles, or does leverage merely accelerate a fundamentally natural scarcity-driven process?',
    'Comparative historical analysis: periods of restricted credit access with held constant zoning, demand, and labor dynamics. Structural VAR analysis isolating credit shocks from demand/supply shocks. Cross-jurisdictional evidence of leverage-invariant price formation.',
    'If credit expansion is primary driver independent of fundamentals, the financial-sector beneficiary structure and extractive classification hold; if credit merely accelerates natural equilibrium, the constraint reclassifies toward rope or mountain depending on whether the equilibrium itself is natural or constructed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether credit and leverage are structural drivers or accelerators of price formation.').

omega_variable(
    extraction_vs_coordination_boundary,
    'What portion of financial-sector gains from housing credit expansion represents rent extraction (leveraging information asymmetry, regulatory arbitrage, or captive markets) versus coordination gains (true reduction in transaction costs or information costs of credit intermediation)?',
    'Decompose financial-sector profits into: (a) spread revenue (difference between borrowing and lending rates), (b) origination fees, (c) servicing fees, (d) trading profits on mortgage-backed securities. Compare spreads and fees to equivalent services in fully transparent, competitive markets (e.g., peer-to-peer lending platforms, index funds). Measure whether borrower outcomes improve or degrade as financial-sector consolidation increases (a tell for whether coordination gains are being captured).',
    'High extraction component (rent-seeking) strengthens tangled-rope classification; high coordination component (genuine intermediation efficiency) could shift toward rope. The measurement at story write is 0.78 extractiveness; this omega documents how that figure divides.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Decomposition of financial-sector gains between extraction and genuine coordination.').

omega_variable(
    credit_access_suppression_mechanism,
    'Is suppression of alternatives (renters'' ability to accumulate equity, first-time buyers'' access to leverage, households'' ability to exit into other asset classes) structural (legal/regulatory barriers, lender gatekeeping, information asymmetry) or internalized (belief that homeownership via leverage is the only legitimate path to security)?',
    'Post-exit trajectory analysis: households that exit the housing market (migrate, downsize, rent indefinitely) report suppression levels at entry-exit boundaries. Interview research on decision-making constraints: do renters report that alternatives were unavailable or unthinkable? Credit rationing experiments comparing borrower outcomes under transparent vs. opaque lending criteria.',
    'If suppression is primarily structural, policy remedies target lending standards and information disclosure; if primarily internalized, cultural narrative change is required (and may be necessary but insufficient). The story''s 0.71 suppression metric does not distinguish; this omega addresses the mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_access_suppression_mechanism, empirical, 'Whether housing-credit suppression is structural or internalized.').

omega_variable(
    asset_price_feedback_exogeneity,
    'Are asset-price feedback loops (rising prices attract institutional capital, institutional demand drives prices higher) endogenous to financial structure or exogenous shocks in institutional preferences and regulatory arbitrage?',
    'Structural identification in multivariate models: do institutional inflows predict price appreciation after controlling for credit conditions, or are both driven by third factors (regulatory changes, monetary policy, supply constraints)? Natural experiments from policy changes (LIBOR floor removal, leverage ratio rules, REIT tax treatment changes) that affect institutional incentives without changing fundamentals.',
    'If feedback loops are endogenous to credit expansion, tightening credit should dampen the cycle; if exogenous, price dynamics persist even under credit constraints. Classification stability depends on whether the constraint''s persistence relies on active enforcement of credit availability or emerges from uncontrollable institutional preferences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asset_price_feedback_exogeneity, empirical, 'Whether asset-price feedback is endogenous to credit or driven by external institutional demand.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__financialization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__financialization_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(pric_tr_t0, observed).
narrative_ontology:measurement(pric_tr_t5, price_formation_kernel__financialization_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(pric_tr_t5, observed).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__financialization_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(pric_tr_t10, observed).
narrative_ontology:measurement(pric_tr_t15, price_formation_kernel__financialization_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement_basis(pric_tr_t15, observed).
narrative_ontology:measurement(pric_tr_t20, price_formation_kernel__financialization_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(pric_tr_t20, observed).
narrative_ontology:measurement(pric_tr_t25, price_formation_kernel__financialization_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement_basis(pric_tr_t25, observed).
narrative_ontology:measurement(pric_tr_t30, price_formation_kernel__financialization_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(pric_tr_t30, observed).
narrative_ontology:measurement(pric_tr_t35, price_formation_kernel__financialization_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(pric_tr_t35, observed).
narrative_ontology:measurement(pric_tr_t40, price_formation_kernel__financialization_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(pric_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__financialization_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(pric_be_t0, observed).
narrative_ontology:measurement(pric_be_t5, price_formation_kernel__financialization_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(pric_be_t5, observed).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__financialization_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(pric_be_t10, observed).
narrative_ontology:measurement(pric_be_t15, price_formation_kernel__financialization_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(pric_be_t15, observed).
narrative_ontology:measurement(pric_be_t20, price_formation_kernel__financialization_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(pric_be_t20, observed).
narrative_ontology:measurement(pric_be_t25, price_formation_kernel__financialization_reading, base_extractiveness, 25, 0.73).
narrative_ontology:measurement_basis(pric_be_t25, observed).
narrative_ontology:measurement(pric_be_t30, price_formation_kernel__financialization_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement_basis(pric_be_t30, observed).
narrative_ontology:measurement(pric_be_t35, price_formation_kernel__financialization_reading, base_extractiveness, 35, 0.77).
narrative_ontology:measurement_basis(pric_be_t35, observed).
narrative_ontology:measurement(pric_be_t40, price_formation_kernel__financialization_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(pric_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__financialization_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(pric_su_t0, observed).
narrative_ontology:measurement(pric_su_t5, price_formation_kernel__financialization_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(pric_su_t5, observed).
narrative_ontology:measurement(pric_su_t10, price_formation_kernel__financialization_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(pric_su_t10, observed).
narrative_ontology:measurement(pric_su_t15, price_formation_kernel__financialization_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(pric_su_t15, observed).
narrative_ontology:measurement(pric_su_t20, price_formation_kernel__financialization_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(pric_su_t20, observed).
narrative_ontology:measurement(pric_su_t25, price_formation_kernel__financialization_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(pric_su_t25, observed).
narrative_ontology:measurement(pric_su_t30, price_formation_kernel__financialization_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(pric_su_t30, observed).
narrative_ontology:measurement(pric_su_t35, price_formation_kernel__financialization_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(pric_su_t35, observed).
narrative_ontology:measurement(pric_su_t40, price_formation_kernel__financialization_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(pric_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__financialization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__financialization_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, mortgage_backed_securities_extraction).
narrative_ontology:affects_constraint(price_formation_kernel__financialization_reading, household_debt_service_vulnerability).

% DUAL FORMULATION NOTE:
% The price_formation_kernel has four structurally distinct readings (financialization, naturalist, institutional, Georgist), each with different ε values, beneficiary/victim sets, and type classifications. This story (financialization_reading) authors the constraint as tangled_rope with high extractiveness (ε=0.78): the referent is the standing arrangement of credit-driven price formation, assessed by the financialization reading's own lights. The naturalist_reading authors price formation as mountain (ε≈0.15): prices reflect objective scarcity, with credit playing an accelerative but not causal role. The institutional_reading authors price formation as snare or piton (ε≈0.65): prices are constructed by policy choice, with financial actors as secondary to regulatory/zoning choices. The Georgist_reading authors land-price formation as snare (ε≈0.72): the reading separates earned improvement value (rope-like, legitimate) from unearned location rent (snare-like, extractive). All four readings share the same referent (the standing arrangement of how housing prices form) but author different ε values because they have different analyses of what drives prices and who benefits. Each story links to the other three via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__financialization_reading, moderate, 0.82).
constraint_indexing:directionality_override(price_formation_kernel__financialization_reading, powerless, 0.89).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
