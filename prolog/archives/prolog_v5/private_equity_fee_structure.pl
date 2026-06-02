% ============================================================================
% CONSTRAINT STORY: private_equity_fee_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_private_equity_fee_structure, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: private_equity_fee_structure
 *   human_readable: Private Equity Fee Structure: Management Fees, Carried Interest, and Portfolio Extraction
 *   domain: finance/investment/economics
 *
 * SUMMARY:
 *   The private equity fee structure (2% annual management fee + 20% carried
 *   interest) has become the dominant coordination mechanism for large-scale
 *   capital deployment and portfolio acquisition over the past 20 years, but
 *   exhibits strong Tangled Rope characteristics: genuine coordination of
 *   portfolio monitoring and management oversight exists alongside systematic
 *   extraction from limited partners and portfolio company stakeholders. The
 *   constraint displays different classifications from different structural
 *   positions. General partners experience it as pure coordination (Rope) — a
 *   necessary incentive alignment mechanism. Pension funds committed to
 *   10-year lock-ups experience it as extraction (Snare) — they cannot exit
 *   and pay fees regardless of returns. Portfolio company workers experience
 *   it as structural coercion (Snare) — cost-cutting and financial
 *   engineering driven by fee dynamics constrain employment and community
 *   outcomes. The analytical observer risks naturalizing the fee structure as
 *   a law of capital markets ('you cannot motivate sophisticated managers
 *   without carries'), but comparative analysis reveals alternative models,
 *   indicating the current structure is contingent institutional design
 *   optimized for GP benefit. The extractiveness has increased from 0.38
 *   (2004) to 0.58 (2024) as PE capital has grown to $10+ trillion in AUM,
 *   fee bases have expanded, and concentration of wealth in GP hands has
 *   increased. Theater has also risen (0.35 to 0.55) as regulatory compliance
 *   and ESG overlays have created performative compliance without
 *   constraining core fee mechanisms.
 *
 * KEY AGENTS:
 *   - General Partners (Institutional/Arbitrage): Beneficiaries — capture 2% AUM annually plus 20% of profits; can exit by raising new funds or deploying elsewhere; experience constraint as coordination mechanism
 *   - Limited Partners — Pension Funds (Powerless/Trapped): Victims — committed to 10+ year lock-ups; pay fees regardless of returns; cannot exit without significant cost; face regulatory/fiduciary pressure to invest
 *   - Limited Partners — Secondary Market Investors (Moderate/Constrained): Mixed — gain partial liquidity and diversification but still pay full fee burden; can exit at market-determined cost
 *   - Portfolio Company Workers (Powerless/Trapped): Victims — experience cost-cutting and leverage increase driven by fee structures; cannot exit without relocation or career disruption
 *   - Reform Coalition (Organized/Constrained): Organized agents (CalPERS, endowments, labor unions) negotiate fee transparency and clawback provisions; have partial countervailing power but cannot eliminate core fee asymmetry
 *   - Regulatory Authorities (Institutional/Arbitrage): Theater maintainers — issue disclosure rules and conflict-of-interest prohibitions but enforcement is minimal; regulations exist performatively
 *   - Analytical Observer (Analytical/Analytical): Risks naturalizing contingent institutional arrangement as immutable market necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(private_equity_fee_structure, 0.58).
domain_priors:suppression_score(private_equity_fee_structure, 0.68).
domain_priors:theater_ratio(private_equity_fee_structure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(private_equity_fee_structure, extractiveness, 0.58).
narrative_ontology:constraint_metric(private_equity_fee_structure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(private_equity_fee_structure, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(private_equity_fee_structure, tangled_rope).
narrative_ontology:human_readable(private_equity_fee_structure, "Private Equity Fee Structure: Management Fees, Carried Interest, and Portfolio Extraction").
narrative_ontology:topic_domain(private_equity_fee_structure, "finance/investment/economics").

domain_priors:requires_active_enforcement(private_equity_fee_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(private_equity_fee_structure, general_partners).
narrative_ontology:constraint_beneficiary(private_equity_fee_structure, carried_interest_recipients).
narrative_ontology:constraint_victim(private_equity_fee_structure, limited_partners).
narrative_ontology:constraint_victim(private_equity_fee_structure, portfolio_companies).
narrative_ontology:constraint_victim(private_equity_fee_structure, workers_and_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED LIMITED PARTNER (SNARE) — Large pension funds and institutional investors face regulatory/fiduciary pressure to diversify into PE; once capital is committed, exit is blocked for 10+ years. Management fees (typically 2% annually) and carried interest (20% of profits) flow directly to GPs regardless of returns. LP cannot exit and bears extraction with no control mechanism. Maximum experienced extraction.
constraint_indexing:constraint_classification(private_equity_fee_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PORTFOLIO COMPANY WORKFORCE (SNARE) — Workers in acquired companies experience cost-cutting, outsourcing, and financial engineering (leveraged recapitalizations) designed to extract value for GP fees and carried interest. Workforces typically cannot exit without geographic relocation or career disruption. No explicit exit clause; extraction mechanisms are embedded in operational decisions driven by fee structures.
constraint_indexing:constraint_classification(private_equity_fee_structure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SECONDARY MARKET LP (TANGLED ROPE) — Institutional investors buying into PE funds on the secondary market benefit from liquidity access (partial exit) and portfolio diversification (coordination), but still pay 2% management fees and carry structures claw back 20% of returns. Constrained by liquidity timing and market conditions; can achieve some exit at cost. Mixed experience: genuine coordination function (access to PE returns) alongside asymmetric extraction (fee burden).
constraint_indexing:constraint_classification(private_equity_fee_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GENERAL PARTNER (ROPE) — GPs experience the fee structure as pure coordination: 2% annual management fee covers staffing, operations, and fund administration; carried interest (20% of profits) aligns incentives between GP and LP. GPs can exit by raising new funds or deploying capital elsewhere. Net beneficiary with genuine ability to arbitrage between PE and alternative strategies. The constraint solves the agency problem of monitoring and managing complex portfolios.
constraint_indexing:constraint_classification(private_equity_fee_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM COALITION (TANGLED ROPE) — Organized agents (CalPERS, University endowments, labor unions) negotiate for fee transparency, clawback provisions, and alignment mechanisms (LP co-investment). They benefit from PE participation (coordination) while extracting concessions on fee structures (constrained exit through collective power). Effective extraction is moderate because organization creates countervailing power, but fundamental fee asymmetry remains.
constraint_indexing:constraint_classification(private_equity_fee_structure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY THEATER (PITON) — SEC rules require fee disclosures and prohibit certain practices, but enforcement is minimal and the rules have been stable for 20+ years despite massive PE growth. Regulations exist performatively — they create the appearance of oversight without constraining the core fee extraction mechanisms (2%/20% structure persists unchanged). Theater_ratio is high because regulatory compliance dominates discussion while actual economic dynamics remain unchanged.
constraint_indexing:constraint_classification(private_equity_fee_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — From a civilizational view, capital deployment requires agent alignment, and agent alignment requires incentives; the 2%/20% structure is sometimes presented as a natural law of financial coordination. This perspective sees the fee structure as immutable because 'you cannot motivate sophisticated capital managers without carries.' However, the structural data contradicts this: other models exist (flat fee + performance bonus, AUM-only with clawback, fixed profit-share), showing the current structure is contingent institutional design, not natural law.
constraint_indexing:constraint_classification(private_equity_fee_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(private_equity_fee_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(private_equity_fee_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(private_equity_fee_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(private_equity_fee_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(private_equity_fee_structure, TR),
    TR >= 0.70.

:- end_tests(private_equity_fee_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial but not maximal. The 2% management fee is above operational cost in most cases (estimated 0.8-1.2% for actual staffing and monitoring), yielding 0.8-1.2% rent per year. The 20% carried interest is extremely asymmetric — GPs capture 20% of all profits while LPs bear 100% of losses. However, extractiveness is not at snare levels (≥0.66) because: (a) LPs do receive genuine returns and value creation, (b) some carried interest is performance-sensitive and subject to clawback, (c) alternative models exist showing contingency rather than structural necessity, (d) long-term LP demand for PE capital suggests some net positive value, though heavily skewed toward GPs. Suppression (0.68): High. Multiple binding mechanisms prevent LP exit: (a) capital lock-up periods (10+ years), (b) lack of secondary liquidity (improving but still constrained), (c) regulatory/fiduciary mandates pushing capital into PE despite concerns, (d) information asymmetry (LPs cannot independently verify portfolio company valuations or operational improvements), (e) for portfolio company workers, geographic and employment barriers to exit create structural suppression. Theater ratio (0.55): Moderate. Significant performative elements: regulatory compliance (SEC disclosures, conflict-of-interest certifications) creates appearance of oversight without constraining core extraction; ESG/impact frameworks are increasingly theatrical (documenting environmental or social benefits without tying them to compensation); carried interest structures are complex specifically to obscure the asymmetry (through GP-side fees, management company margins, distribution waterfalls). But some elements are functional: portfolio monitoring, operational due diligence, and fund administration are genuinely coordinating. The increase from 0.35 to 0.55 over 20 years reflects regulatory theater growing faster than operational substance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full divergence between beneficiary and victim perspectives. The GP (institutional/arbitrage) sees Rope: the fee structure elegantly solves the capital deployment problem and incentivizes careful portfolio management. The pension fund LP (powerless/trapped) sees Snare: they are locked in for 10 years and pay extraction regardless of returns. The portfolio company worker (powerless/trapped) sees Snare: cost-cutting driven by fee structures constrains employment and wages. The organized coalition (organized/constrained) sees Tangled Rope: they have partial countervailing power but cannot eliminate the core asymmetry. The regulatory authority (institutional/arbitrage) sees the system working per design — disclosures are made, conflicts are documented — missing that the rules themselves are theater. The analytical observer risks seeing Mountain (immutable capital market necessity) when the structural data reveals Tangled Rope: the current fee structure is one possible coordination mechanism, not the only one, and its distributional asymmetry cannot be justified as natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position in the extraction flow. GPs are beneficiaries with high exit options (arbitrage) — they can redeploy capital if LP terms become unfavorable, or raise new funds. This yields low d (d ≈ 0.15), producing negative or minimal f(d), making GP-side chi near zero or negative (extraction flows toward them). Pension fund LPs are victims with zero exit options (trapped for 10 years) — capital is committed regardless of returns, information asymmetry prevents independent verification, and regulatory/fiduciary mandates create path dependence. This yields high d (d ≈ 0.90), producing high f(d) ≈ 1.25, making LP-side chi substantial. Portfolio company workers are victims with high suppression and constrained exit (employment alternatives are geography-dependent, skill-specific, or require relocation). Secondary market LPs have partial exit (liquidity at market rates) but are still trapped during fund life, yielding d ≈ 0.65. The organized coalition has created partial exit mechanisms (secondary markets, direct co-investment participation) but cannot exit the core fee structure, yielding d ≈ 0.50-0.55. The directionality override list is empty because the structural derivation captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE MANDATE: This classification resolves mandatrophy by declaring that the PE fee structure simultaneously coordinates capital deployment AND extracts asymmetrically. The coordination function is genuine: portfolio monitoring, due diligence, and management oversight are real services that reduce agency costs between capital providers and portfolio companies. Without some incentive structure, GPs would not perform these functions with care. However, the current form (2%/20%) is not the minimal coordination structure — alternative models (AUM-only with performance clawback, profit-share with symmetric risk, tiered carries declining at scale) would coordinate equally or better while reducing extraction. The current structure persists because it maximizes GP wealth capture, not because it optimally solves the coordination problem. The constraint is Tangled Rope because: (1) beneficiaries (GPs) are declared and do experience the structure as beneficial coordination, (2) victims (LPs, workers) are declared and do experience it as extraction with suppression, (3) active enforcement is required to maintain the structure (through capital lock-ups, fee opacity, and regulatory theater), (4) removing the constraint would require institutional redesign (new fee models, transparency requirements, codetermination in portfolio decisions), not just allowing actors to exit. The mandate prevents misclassification as pure Rope (which would require zero victimization or zero enforcement) or pure Snare (which would require near-zero coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fee_structure_necessity,
    'Is the 2%/20% fee structure structurally necessary to coordinate capital deployment and GP incentives, or is it a contingent arrangement optimized for GP extraction?',
    'Comparative institutional analysis: examine alternative compensation models (fixed fees + performance bonus, tiered carry structures, LP co-investment gates) and their outcomes on fund performance, LP returns, and portfolio company productivity across jurisdictions and fund cohorts',
    'If necessary: current structure is Rope (pure coordination). If contingent: current structure is Tangled Rope or Snare depending on outside option quality. This is the core mandate ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fee_structure_necessity, empirical, 'Whether 2%/20% fee structure is structurally necessary or contingent institutional design').

omega_variable(
    lp_outside_option_quality,
    'What are the actual outside options for LP capital deployment, and how constraining is PE relative to alternatives (index funds, direct investment, private credit)?',
    'Performance benchmarking across 20+ years: net-of-fee returns (LP IRR) vs broad market indices and peer investment classes; analysis of forced allocation (regulatory mandate vs genuine superior return expectations)',
    'If LPs have credible outside options with comparable returns: LP exit option is ''mobile'' or ''arbitrage'' (not trapped). If outside options are inferior or unavailable: LP is trapped regardless of fee size. Exit option determines d and chi.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lp_outside_option_quality, empirical, 'Quality and availability of LP outside investment options relative to PE returns').

omega_variable(
    gp_profit_allocation_mechanism,
    'Does carried interest represent fair risk-reward alignment or an asymmetric wealth transfer mechanism that persists regardless of LP value creation?',
    'Decomposition of carried interest pools: (a) what share derives from genuine outperformance vs market exposure, (b) what share is sensitive to fund-level clawbacks and claw-in provisions, (c) how many GPs retain carried interest after 2x+ multiple threshold regardless of LP satisfaction',
    'If highly sensitive to performance and widely clawed back: carry is coordination mechanism. If largely insensitive and rarely clawed back: carry is extraction mechanism. This determines whether the classification is Rope or Snare at the GP perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gp_profit_allocation_mechanism, empirical, 'Whether carried interest functions as performance incentive or asymmetric wealth transfer').

omega_variable(
    management_fee_justification,
    'Do the 2% management fees actually cover the stated operational costs (staffing, due diligence, monitoring) or do they represent pure rent extraction?',
    'Cost accounting analysis: GP operational cost as percentage of AUM across fund vintage years and geographies; comparison with operational costs of alternative investment vehicles (mutual funds, hedge funds, direct corporate investment offices)',
    'If fees < actual costs: compensation is below-market and fee structure is coordination. If fees >> actual costs: excess captures rents and fee structure includes extraction component. This affects baseline extractiveness calculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(management_fee_justification, empirical, 'Whether management fees align with stated operational costs or exceed them').

omega_variable(
    portfolio_company_extraction_mechanism,
    'Are portfolio company actions (cost-cutting, leverage increases, dividend recaps) genuine value creation or pure value extraction driven by fee structures?',
    'Longitudinal analysis of acquired companies: (a) operational improvements vs cost reductions, (b) debt incurred vs dividend distributions, (c) long-term productivity/employment/innovation metrics post-exit vs pre-acquisition, (d) comparison with control group companies in same industry',
    'If largely value creation: suppression of workers is justified by returns that benefit all stakeholders. If largely extraction: suppression is pure redistribution and extraction rate increases. This drives the suppression metric and impacts whether victims are voluntary or coerced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portfolio_company_extraction_mechanism, empirical, 'Whether portfolio company operational changes produce genuine value creation or pure extraction').

omega_variable(
    regulatory_enforcement_gap,
    'Do existing SEC regulations and state-level oversight actually constrain PE fee extraction, or is enforcement insufficient to create real barriers?',
    'Enforcement action analysis: count and severity of SEC/state actions against PE firms on fee disclosure, conflicts of interest, and clawback violations across 10-year period; measure deterrent effect (post-action compliance changes)',
    'If enforcement is strong: regulations create real suppression and theater is justified. If weak: regulations are pure theater and should drive piton classification higher. This affects the theater_ratio interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_enforcement_gap, empirical, 'Whether regulatory enforcement actually constrains PE fee extraction or is largely theatrical').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(private_equity_fee_structure, 2004, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pefee_tr_t0, private_equity_fee_structure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pefee_tr_t10, private_equity_fee_structure, theater_ratio, 10, 0.45).
narrative_ontology:measurement(pefee_tr_t20, private_equity_fee_structure, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(pefee_be_t0, private_equity_fee_structure, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pefee_be_t10, private_equity_fee_structure, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pefee_be_t20, private_equity_fee_structure, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(private_equity_fee_structure, resource_allocation).
narrative_ontology:boltzmann_floor_override(private_equity_fee_structure, 0.18).
narrative_ontology:affects_constraint(private_equity_fee_structure, portfolio_company_labor_extraction).
narrative_ontology:affects_constraint(private_equity_fee_structure, pension_fund_fiduciary_mandate).
narrative_ontology:affects_constraint(private_equity_fee_structure, secondary_market_liquidity_constraint).
narrative_ontology:affects_constraint(private_equity_fee_structure, carried_interest_tax_structure).

% DUAL FORMULATION NOTE:
% The PE fee structure is the primary institutional constraint that shapes downstream extraction in portfolio companies, shapes the forced capital commitment structure for pension funds, and interfaces with tax law structures around carried interest. These constraints form a causal family where changes to one (e.g., carried interest taxation, secondary market regulations) directly influence the others. The fee structure itself could decompose further into (1) management fee extraction, (2) carried interest asymmetry, and (3) hidden fees/management company margins, each with different ε and classification profiles; the current story treats them as integrated but decomposition would provide higher resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
