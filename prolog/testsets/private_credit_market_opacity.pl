% ============================================================================
% CONSTRAINT STORY: private_credit_market_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_private_credit_market_opacity, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: private_credit_market_opacity
 *   human_readable: Opacity and Systemic Risk Externalization in Private Credit Markets
 *   domain: economic/financial_regulation
 *
 * SUMMARY:
 *   The private credit market has grown from roughly $500 billion in 2012 to
 *   over $1.7 trillion by 2024, channeling capital from yield-seeking
 *   institutional investors (pension funds, sovereign wealth funds, insurance
 *   companies) directly to corporate borrowers, explicitly bypassing
 *   regulated public markets and the disclosure/monitoring infrastructure
 *   that evolved to contain systemic risk. This constraint exhibits classical
 *   tangled-rope structure: private credit managers coordinate capital
 *   allocation to mid-market and leveraged-buyout borrowers that regulated
 *   banks cannot serve (genuine coordination function), while simultaneously
 *   externalizing systemic risk onto the broader financial system through
 *   opacity that prevents monitoring, concentration risk measurement, and
 *   cross-institutional contagion mapping. The extractiveness has risen from
 *   0.32 to 0.58 over the interval as asset growth has concentrated capital
 *   flows and as borrower leverage has increased without corresponding
 *   transparency increase. Theater ratio (0.55) reflects that institutional
 *   investors receive glossy quarterly reports and annual valuations that
 *   simulate transparency while actual underlying leverage, credit quality,
 *   and correlation exposures remain opaque. Retail investors trapped in
 *   private credit vehicles experience even higher theater: marketing
 *   narratives about superior covenant protections and direct lending
 *   efficiency mask the systematic opacity that enables rent extraction.
 *
 * KEY AGENTS:
 *   - Private Credit Managers: Primary beneficiary (institutional/arbitrage) — capture management fees (1-2% annually), performance fees, and interest rate spreads on unregulated leverage
 *   - Institutional Borrowers (Mid-market corporates, LBOs): Secondary beneficiary (institutional/arbitrage) — access capital unavailable from regulated banking system; enjoy covenant flexibility and discretionary timing
 *   - Institutional Investors (Pension funds, SWFs): Constrained victim (organized/constrained) — yield-seeking mandate forces allocation into private credit despite illiquidity and concentration risk; cannot easily exit without portfolio rebalancing
 *   - Retail Investors: Primary victim (powerless/trapped) — invested in opaque private credit vehicles through pension plans or wealth managers; bear tail risk with no access to underlying data or exit liquidity
 *   - Financial Stability Framework: Systemic victim (powerless/trapped) — regulatory supervisors cannot monitor off-balance-sheet leverage, correlated defaults, or contagion channels; lack granular data on credit quality and interconnectedness
 *   - Bank-Regulated Credit System: Secondary victim (institutional/constrained) — competes with private credit on uneven regulatory field; if private credit deleverages, contagion flows to banking system
 *   - Financial Regulator: Constrained enforcer (institutional/constrained) — politically pressured to not restrict capital flows; jurisdictionally limited (private credit flows globally); can regulate bank-affiliated credit but not non-bank managers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(private_credit_market_opacity, 0.58).
domain_priors:suppression_score(private_credit_market_opacity, 0.68).
domain_priors:theater_ratio(private_credit_market_opacity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(private_credit_market_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(private_credit_market_opacity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(private_credit_market_opacity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(private_credit_market_opacity, tangled_rope).
narrative_ontology:human_readable(private_credit_market_opacity, "Opacity and Systemic Risk Externalization in Private Credit Markets").
narrative_ontology:topic_domain(private_credit_market_opacity, "economic/financial_regulation").

domain_priors:requires_active_enforcement(private_credit_market_opacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(private_credit_market_opacity, private_credit_managers).
narrative_ontology:constraint_beneficiary(private_credit_market_opacity, institutional_borrowers).
narrative_ontology:constraint_victim(private_credit_market_opacity, retail_investors).
narrative_ontology:constraint_victim(private_credit_market_opacity, financial_stability).
narrative_ontology:constraint_victim(private_credit_market_opacity, systemic_risk_containment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Trapped in opaque information environment with no exit. Cannot assess counterparty risk, leverage ratios, or liquidity profiles of private credit vehicles. Bearer of tail risks without access to due diligence infrastructure. Maximum extraction: capital locked in illiquid structures, exposed to unreported leverage and valuation opacity.
constraint_indexing:constraint_classification(private_credit_market_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FINANCIAL STABILITY FRAMEWORK (SNARE) — Systemic risk containment apparatus is structurally trapped: cannot monitor off-balance-sheet leverage, interconnectedness, or correlated failure modes across the $1.7T private credit market. Regulatory supervisors lack granular data on credit quality, concentration, and pro-cyclicality. Bearing full cost of systemic fragility with no ability to exit or redirect the system.
constraint_indexing:constraint_classification(private_credit_market_opacity, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL INVESTOR (TANGLED ROPE) — Constrained by yield-seeking mandate: traditional public bond yields have compressed, pushing pension funds and sovereign wealth funds into private credit for return enhancement. Benefits from access to higher-yielding borrowers and stronger covenant protections than public markets. Also bears concentration risk, liquidity risk, and tail-event exposure. Constrained exit because divesting from private credit would require recalibration of entire portfolio allocation.
constraint_indexing:constraint_classification(private_credit_market_opacity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIVATE CREDIT MANAGER (ROPE) — Primary beneficiary with arbitrage exit. Captures management fees (typically 1-2% annually), performance fees, and access to higher-margin lending opportunities than regulated banking. Coordination function: private credit managers efficiently allocate capital to mid-market and leveraged-buyout borrowers that traditional banks cannot serve due to regulatory constraints. Low effective extraction because the extraction runs toward the manager, and managers have full exit optionality (can wind down or redeploy capital).
constraint_indexing:constraint_classification(private_credit_market_opacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FINANCIAL REGULATOR (TANGLED ROPE) — Constrained by jurisdictional limits and political pressure to not stifle capital markets. Benefits from tax revenue and economic activity generated by private credit growth. Also bears political/institutional cost of being unable to regulate off-balance-sheet leverage. Constrained exit: regulators cannot unilaterally restrict private credit without cross-border coordination (private credit flows globally). Enforcement is active but applies only to bank-related credit, not to non-bank private credit managers.
constraint_indexing:constraint_classification(private_credit_market_opacity, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PUBLIC CREDIT DISCLOSURE REGIME (PITON) — The traditional public market transparency regime (SEC filings, credit ratings, financial statement audits) is largely theatrical for private credit. Public markets require real-time disclosure and have verification mechanisms (audits, ratings agencies); private credit operates on investor-generated due diligence with no standardized verification. The public regime persists through regulatory inertia but has lost functional control over most credit growth. Theater ratio is high because regulators maintain disclosure rules that do not apply to the fastest-growing credit segment.
constraint_indexing:constraint_classification(private_credit_market_opacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational scope, the private credit market exhibits both genuine coordination (efficient capital allocation to borrowers unserved by regulated banking) and asymmetric extraction (opacity that externalizes systemic risk onto the broader financial system). The constraint is hybrid: managers benefit from lack of disclosure and regulatory scope; retail investors and systemic risk managers bear opaqueness. The classification reflects the irreducible hybrid structure — real coordination function, real extraction mechanism, active enforcement by managers to maintain opacity.
constraint_indexing:constraint_classification(private_credit_market_opacity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(private_credit_market_opacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(private_credit_market_opacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(private_credit_market_opacity, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(private_credit_market_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(private_credit_market_opacity, TR),
    TR >= 0.70.

:- end_tests(private_credit_market_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The market has genuine coordination value — private credit managers do efficiently allocate capital to borrowers underserved by regulated banking, and institutional investors do achieve higher returns than available in public markets. However, extractiveness has risen as (1) leverage embedded in portfolio companies has increased without corresponding borrower disclosure, (2) private credit managers have gained market power over borrowers, and (3) opacity has shifted tail-risk exposure from managers to institutional and retail investors. At 0.58, the constraint is extractive but not purely extractive — real coordination is happening. Suppression (0.68): High. Opacity is enforced through multiple channels: (a) no standardized reporting of leverage ratios, interest coverage, or covenant terms across private credit funds; (b) secondary markets are thin with large bid-ask spreads (illiquidity enforces information asymmetry); (c) fund valuations are unaudited and pro-cyclical; (d) interconnectedness data is fragmented across unregulated managers. Retail and regulatory victims face severe information suppression. Theater ratio (0.55): Moderate. Institutional investors receive detailed quarterly reports and annual valuations, creating appearance of transparency. However, these reports use mark-to-management valuations and do not reveal cross-fund borrower exposures, fund-to-fund leverage, or systemic correlation. Theater has risen as marketing narratives about 'direct lending alpha' and 'strong covenant protections' have proliferated without corresponding evidence. The narrative substitutes for actual transparency.
 *
 * PERSPECTIVAL GAP:
 *   The private credit manager sees Rope — a coordination mechanism connecting capital with borrowers. The institutional investor sees Tangled Rope — higher returns available but constrained by illiquidity and concentration. The retail investor sees Snare — capital locked in opaque vehicles with no exit. The financial stability framework sees Snare — systemic risk externalization without monitoring capacity. The regulator sees Tangled Rope — benefits from economic activity but constrained by regulatory arbitrage and lack of information. The piton perspective sees a degraded disclosure regime — public market transparency rules that no longer apply to most credit growth. The analytical observer sees Tangled Rope — genuine coordination with built-in extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Private credit managers occupy a beneficiary position with full arbitrage exit (d ≈ 0.10): they can redeploy capital, wind down funds, or recapture fees through performance fees on outperformance. The sigmoid f(d) at d=0.10 produces low or negative effective extraction for managers — the constraint subsidizes them. Institutional investors are constrained by their yield-seeking mandate and asset-allocation strategy — they have moderate exit optionality (can divest but with portfolio cost) and victim status (concentrated risk exposure), placing them at d ≈ 0.55. Retail investors are trapped victims with no information or liquidity exit, placing them at d ≈ 0.95 — maximum experienced extraction. Financial stability framework is a powerless victim in a structural sense — supervisors cannot exit or reorganize markets, placing it at d ≈ 1.0. The regulator is constrained by jurisdictional and political limits, with dual status (partial beneficiary from economic activity, partial victim from concentration risk), placing it at d ≈ 0.60. The directionality asymmetry is large: beneficiaries (managers at d=0.10) have low chi; victims (retail at d=0.95, stability at d=1.0) have high chi. This asymmetry is the signature of the tangled-rope structure — real coordination, active enforcement to maintain opacity, and significant extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint exhibits classical tangled-rope mandatrophy — the same market structure is labeled 'efficient private credit market' by managers and institutional investors (coordination frame) and 'systemic risk externalization' by regulators and retail investors (extraction frame). The resolution lies in the structural asymmetry: private credit does provide real coordination benefits (efficient capital allocation to underserved borrowers), but these benefits are only available to institutional actors and borrowers with access to large capital pools and sophisticated due diligence. Retail investors and the financial stability framework are excluded from the coordination benefits while bearing the extraction costs (opacity, concentration risk, fire-sale liquidity cascades). The mandate is resolved by recognizing that the same constraint has different classification from different positions: it is Rope for those with arbitage exit and information access, Tangled Rope for those with constrained exit but some agency, and Snare for those with no exit or information. The engine computes all four perspectival classifications and reports the presheaf: the constraint is not a single type but a sheaf over the observation site, with classification depending on observer position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    systemic_correlation_threshold,
    'What leverage and correlation threshold in private credit markets would trigger contagion to regulated banking system in a stress scenario?',
    'Stress testing with micro-level data on leverage, interconnectedness, and correlated borrower exposures; counter-party analysis of private credit fund linkages to bank balance sheets',
    'If threshold is low (< 1.5x leverage or < 30% bank connection): private credit is a direct systemic risk channel. If threshold is high (> 3x leverage and > 60% isolated): private credit is a risk-transfer mechanism that absorbs systemic shocks. Classification shifts from Snare to Rope for financial stability victim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(systemic_correlation_threshold, empirical, 'Leverage and correlation threshold for systemic contagion').

omega_variable(
    borrower_covenant_quality_divergence,
    'Do private credit borrowers actually receive stronger covenant protections and monitoring than public market peers, or is this marketing narrative masking comparable covenant creep?',
    'Historical comparison of covenant packages (financial maintenance covenants, change-of-control provisions, reporting requirements) between private credit and public bond issuers matched by sector, leverage, and credit quality',
    'If covenants are materially stronger: private credit is genuine coordination benefit to borrowers (justifies institutional investor belief in higher return/risk tradeoff). If covenants are comparable or weaker: higher returns are extracted rent, not coordination-enabled return.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(borrower_covenant_quality_divergence, empirical, 'Whether private credit covenants exceed public market standards').

omega_variable(
    opacity_necessity_for_pricing,
    'Is the lack of real-time pricing transparency a feature of private credit economics (illiquidity premium is earned opacity) or a bug that extracts retail investor rents?',
    'Comparison of returns on private credit strategies with publicly disclosed leverage/concentration vs strategies with equivalent structures but hidden leverage. Longitudinal analysis of whether retail investors hold private credit in illiquid vehicles vs liquid ETFs with same underlying exposure.',
    'If opacity-as-feature: illiquidity premium is fair compensation, suppression is justified. If opacity-as-bug: retail extraction is the business model, and suppression is predatory. Classification implications for managed asset holders shift from constrained to trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_necessity_for_pricing, empirical, 'Whether pricing opacity is inherent to private credit or extractive mechanism').

omega_variable(
    regulatory_arbitrage_sustainability,
    'Is private credit''s growth sustained by genuine economic efficiency gains or by regulatory arbitrage that will collapse if disclosure requirements are harmonized?',
    'Analysis of private credit fund performance and market growth rates under different regulatory regimes (US vs EU AIFMD transparency standards); modeling of cost structure and returns under hypothetical full transparency',
    'If efficiency-driven: growth is sustainable even with full transparency, beneficiary perspective is correct, classification stable. If arbitrage-driven: growth collapses with transparency, market structure is revealed as pure rent extraction, snare classification dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_arbitrage_sustainability, empirical, 'Whether private credit growth is driven by efficiency or regulatory arbitrage').

omega_variable(
    fire_sale_liquidity_cascade,
    'In a stress scenario (rising rates, credit deterioration), what fraction of private credit positions would require simultaneous fire sales, and how deep is the buyer-liquidity pool?',
    'Scenario analysis with mark-to-market stress testing; estimation of secondary market bid-ask spreads under stress; modeling of deleveraging paths across funds with common lenders or borrowers',
    'If cascade is high-impact: private credit creates hidden leverage embedded in institutional portfolios. Snare classification strengthens for financial stability victim. If cascade is contained: private credit absorbs shocks without amplification. Rope classification strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fire_sale_liquidity_cascade, empirical, 'Scale of fire-sale liquidity cascades in private credit stress').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(private_credit_market_opacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pricmo_tr_t0, private_credit_market_opacity, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pricmo_tr_t5, private_credit_market_opacity, theater_ratio, 5, 0.48).
narrative_ontology:measurement(pricmo_tr_t10, private_credit_market_opacity, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(pricmo_be_t0, private_credit_market_opacity, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pricmo_be_t5, private_credit_market_opacity, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(pricmo_be_t10, private_credit_market_opacity, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(private_credit_market_opacity, resource_allocation).
narrative_ontology:affects_constraint(private_credit_market_opacity, bank_regulatory_capital_arbitrage).
narrative_ontology:affects_constraint(private_credit_market_opacity, institutional_investor_duration_mismatch).
narrative_ontology:affects_constraint(private_credit_market_opacity, shadow_banking_leverage_amplification).

% DUAL FORMULATION NOTE:
% Private credit market opacity decomposes into two structurally distinct constraints: (1) information asymmetry (ε=0.42, Tangled Rope) — the inherent illiquidity of private credit creates pricing opacity that is difficult to eliminate without destroying the illiquidity premium; (2) regulatory arbitrage (ε=0.68, Snare) — the deliberate non-regulation of non-bank private credit managers relative to banks creates incentive to shift leverage off-balance-sheet. The first is a partial-equilibrium coordination problem; the second is a systemic extraction mechanism. This story focuses on the integrated structure. For detailed treatment of the regulatory arbitrage component, see constraint_shadow_banking_regulatory_exemption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
