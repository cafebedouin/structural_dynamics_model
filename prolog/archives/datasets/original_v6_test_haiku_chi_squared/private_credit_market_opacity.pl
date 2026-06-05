% ============================================================================
% CONSTRAINT STORY: private_credit_market_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    constraint_indexing:directionality_override/3,
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
 *   The private credit market has grown to $1.7+ trillion by positioning
 *   itself as an alternative to regulated public credit markets, offering
 *   institutional investors higher yields in exchange for reduced
 *   transparency, longer lockup periods, and acceptance of information
 *   asymmetry. The core structural feature is that private credit fund
 *   managers control access to deal information, borrower identity, leverage
 *   ratios, and portfolio composition — information that would be mandated
 *   disclosure in public markets. This opacity extraction mechanism benefits
 *   fund managers through reduced competitive pressure and enables systematic
 *   overpricing of risk relative to actual default probability. The
 *   constraint manifests as a Snare because: (1) the victims (pension fund
 *   beneficiaries, systemic stability) have no exit and cannot organize, (2)
 *   suppression is high (information barriers prevent independent
 *   assessment), (3) extractiveness is substantial (fund managers capture
 *   yield differential that reflects risk underpricing), and (4) the
 *   mechanism relies on regulatory arbitrage rather than coordination
 *   function. The theater ratio (0.55) reflects that the constraint is
 *   partially obscured by marketing framing ('yield enhancement,' 'financial
 *   engineering') and institutional legitimacy (major asset managers operate
 *   private credit platforms). From the analytical observer perspective, this
 *   is clearly extraction: the same borrower obtains credit at lower cost in
 *   private markets than public markets, not because the risk is lower (it
 *   isn't), but because institutional investors accept information opacity
 *   for yield in a yield-constrained environment (post-2022 interest rate
 *   hikes).
 *
 * KEY AGENTS:
 *   - Private Credit Fund Managers: Primary beneficiary (institutional/arbitrage) — extract rents through information asymmetry and regulatory arbitrage; control deal selection and pricing
 *   - Institutional Investors (Pension Funds, Sovereign Wealth Funds): Secondary beneficiary with victim characteristics (organized/arbitrage) — gain yield but accept opacity and leverage risks; cannot meaningfully exercise governance
 *   - Pension Fund Beneficiaries: Primary victim (powerless/trapped) — exposure without consent or meaningful transparency; no exit mechanism from pension fund allocation decisions
 *   - Regulatory Authorities: Victim (moderate/constrained) — tasked with systemic stability but lack real-time visibility; constrained by jurisdictional limits and regulatory arbitrage
 *   - Systemic Financial Stability: Abstract victim (powerless/trapped) — exposed to correlated default risks and potential cascade effects; no agency or exit
 *   - Public Credit Markets: Degraded institution (institutional/arbitrage) — lose creditworthy borrowers to private markets, leaving public markets with worse credit quality and reduced price discovery function
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
narrative_ontology:constraint_claim(private_credit_market_opacity, snare).
narrative_ontology:human_readable(private_credit_market_opacity, "Opacity and Systemic Risk Externalization in Private Credit Markets").
narrative_ontology:topic_domain(private_credit_market_opacity, "economic/financial_regulation").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(private_credit_market_opacity, private_credit_fund_managers).
narrative_ontology:constraint_beneficiary(private_credit_market_opacity, specialized_lenders).
narrative_ontology:constraint_victim(private_credit_market_opacity, pension_fund_beneficiaries).
narrative_ontology:constraint_victim(private_credit_market_opacity, systemic_financial_stability).
narrative_ontology:constraint_victim(private_credit_market_opacity, regulatory_authorities).
narrative_ontology:constraint_victim(private_credit_market_opacity, public_credit_markets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PENSION FUND BENEFICIARIES (SNARE) — Beneficiaries exposed to private credit risk through pension fund allocation decisions made without meaningful transparency or consent. No exit mechanism; trapped in the outcome of fund manager decisions. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(private_credit_market_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGULATORY AUTHORITIES (SNARE) — Tasked with maintaining financial stability but lack real-time visibility into private credit portfolios, leverage ratios, concentration risks, and interconnectedness. Constrained by jurisdictional limits and regulatory arbitrage. d≈0.88, f(d)≈1.32, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(private_credit_market_opacity, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SYSTEMIC FINANCIAL STABILITY (SNARE) — Abstract collective good with no agency, no exit, and full exposure to correlated default risk across opaque portfolios. Cannot organize or advocate. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98.
constraint_indexing:constraint_classification(private_credit_market_opacity, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL INVESTORS (TANGLED ROPE) — Large sovereign wealth funds and institutional investors benefit from higher yields and portfolio diversification, but also face information asymmetry and counterparty risk. Have arbitrage exit (can reallocate capital) but accept the opacity trade-off for yield. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.22.
constraint_indexing:constraint_classification(private_credit_market_opacity, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PRIVATE CREDIT FUND MANAGERS (ROPE) — Experience opacity as coordination: exclusive access to deal information and borrower data is the core value proposition. Opacity enables higher margins and reduces competitive pressure from public markets. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(private_credit_market_opacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PUBLIC CREDIT MARKETS (PITON) — Regulated public credit markets (corporate bonds, loan syndication) once functioned as price discovery and risk assessment mechanisms. Private credit has degraded this function — creditworthy borrowers bypass public markets entirely, leaving public markets with worse credit quality and reduced information signaling. Theater persists through regulatory appearances and disclosure requirements that no longer capture reality. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(private_credit_market_opacity, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational perspective, the opacity is not an inherent feature of private credit but a structural extraction mechanism. The constraint exhibits all hallmarks of pure extraction: high suppression (information barriers prevent exit), high extractiveness (fund manager rents), asymmetric beneficiary/victim relationship, and reliance on regulatory arbitrage rather than coordination function. d≈0.70, f(d)≈1.10, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(private_credit_market_opacity, snare,
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
 *   Extractiveness (0.58): High and growing. Fund managers extract rents through three mechanisms: (1) yield capture differential (borrowers pay higher rates in private markets despite lower risk-free-rate environment), (2) fee extraction (management fees 1.5-2% annually plus performance fees), and (3) information monopoly (exclusive access to deal data). The trajectory from 0.35 to 0.58 reflects the market's growth and increasing concentration of capital with fewer fund managers, reducing borrower competition. Suppression (0.68): High. Information barriers are intentional design features: fund managers control borrower identity, leverage ratios, covenant terms, and default timelines. Pension fund investors receive aggregated portfolio metrics but not underlying loan-level details. Regulatory authorities have no real-time visibility into private credit positions across their jurisdictions. This suppression is enforced through contractual confidentiality and the absence of standardized reporting requirements. Theater ratio (0.55): Moderate. The constraint includes performative elements — fund managers market themselves as 'sophisticated alternative investment managers' with superior deal sourcing and credit analysis, implying that opacity reflects specialized expertise rather than regulatory arbitrage. Marketing materials emphasize 'access' and 'yield generation' rather than information asymmetry. However, this is not pure theater (like traditional peer review); there is genuine fund manager activity (deal sourcing, due diligence, portfolio monitoring). The theater has increased from 0.32 to 0.55 as the market has professionalized and adopted more elaborate governance narratives.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between beneficiaries and victims. Fund managers and institutional investors (perspectives 4-5) experience private credit as solving a genuine coordination problem: matching capital supply (institutional investors seeking yield) with demand (mid-market borrowers excluded from public markets by SEC requirements and credit rating thresholds). But pension fund beneficiaries, regulatory authorities, and systemic stability (perspectives 1-3) experience the same mechanism as extraction without coordination benefit — they bear risks they did not choose and cannot assess. The public credit market perspective (6) sees degradation: the exit of creditworthy borrowers has hollowed out price discovery in public markets, leaving them with adverse selection (lower-quality borrowers). The analytical observer (7) identifies this as Snare because the coordination narrative ('serving borrowers excluded from public markets') is subordinate to the extraction mechanism ('capturing yield differential through information opacity'). If coordination were primary, private credit would include standardized reporting and leverage limits; instead, opacity and leverage are features, not bugs.
 *
 * DIRECTIONALITY LOGIC:
 *   Pension fund beneficiaries: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximum extraction. Regulatory authorities: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction; they are tasked with oversight but lack information. Systemic stability: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Institutional investors: Beneficiary + arbitrage → d≈0.35, f(d)≈0.32. However, the beneficiary status is nuanced — institutional investors have agency (can reallocate capital) and awareness of risks, so they are not pure victims. The Tangled Rope classification reflects this: they coordinate with fund managers (gain access) while accepting asymmetric information. Private credit fund managers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary with low effective extraction because they have full agency and benefit from the constraint. Public credit markets: Symmetric/victim split → d≈0.50, f(d)≈0.65. Piton classification reflects that they once functioned as coordination (price discovery) but are now largely performative (survival through regulatory requirements without real function).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the mandatrophy between coordination and extraction by clarifying the structural distinction: private credit DOES solve a coordination problem (matching capital to borrowers excluded from public markets), but the solution is contaminated by extraction (opacity enables yield differential extraction). The Snare classification is correct not because coordination is absent but because extraction is the dominant mechanism. If the constraint were Tangled Rope, we would expect to see: (1) transparent reporting of leverage and concentration, (2) standardized covenants limiting downside for beneficiaries, (3) real governance rights for institutional investors. Instead, we observe the opposite: opacity and minimal governance. The institutional investor perspective (Tangled Rope) reflects their choice to accept this trade-off for yield; the beneficiary and systemic stability perspectives reflect being trapped with risks they did not choose. The mandatrophy is resolved by distinguishing who is benefiting (fund managers via arbitrage) from who is extracting (fund managers via information monopoly) — they are the same actor, but the mechanism is extraction, not coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_necessity_threshold,
    'Is opacity structurally necessary for private credit to function as a yield-generation mechanism, or is it contingent on regulatory arbitrage and fund manager rents?',
    'Comparative analysis of transparent private credit funds vs opaque peers; correlation between transparency level and fund returns; simulation of regulatory regimes requiring standardized reporting',
    'If structurally necessary: opacity is coordination cost (Rope from some perspectives). If contingent: opacity is pure extraction mechanism (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_necessity_threshold, empirical, 'Whether opacity is structurally necessary or contingent on rent extraction').

omega_variable(
    systemic_risk_correlation,
    'Are private credit portfolios sufficiently correlated with public credit markets and macro risks to trigger systemic cascade effects, or are they isolated from broad financial system shocks?',
    'Portfolio correlation analysis under stress scenarios; stress-test modeling of shared borrower exposures; empirical analysis of 2023-2024 credit market dislocations and private credit exposure',
    'If highly correlated: snare classification confirmed (systemic stability is victim). If isolated: risk is private (Tangled Rope from investor perspective). This determines whether externalization to public system occurs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_risk_correlation, empirical, 'Systemic risk correlation of private credit to public markets').

omega_variable(
    regulatory_arbitrage_sustainability,
    'Can regulatory frameworks close the arbitrage window through standardized reporting and prudential limits, or is the opacity structural to how private credit fund economics operate?',
    'Analysis of SEC, FCA, EBA regulatory proposals and implementation timelines; modeling of fund manager response to transparency mandates; comparison with regulated alternatives (BDCs, loan ETFs)',
    'If closable: scaffold perspective is valid (regulatory sunset possible). If structural: snare is permanent unless market structure fundamentally changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_sustainability, empirical, 'Whether regulatory arbitrage can be closed through policy').

omega_variable(
    beneficiary_knowledge_asymmetry,
    'Do institutional investors (pension funds, endowments) genuinely understand the opacity, leverage, and correlation risks they are accepting, or does marketing and complexity obscure true risk profile?',
    'Survey of institutional investor risk assessment practices; analysis of due diligence documentation; interviews with pension fund CIOs and risk officers; comparison of stated risk tolerance vs actual portfolio composition',
    'If well-informed: constraint is Tangled Rope (beneficiary consciously accepts trade-off). If obscured: constraint is closer to Snare (beneficiary is victimized by information asymmetry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_knowledge_asymmetry, empirical, 'Institutional investor knowledge of private credit risks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(private_credit_market_opacity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pcmo_tr_t0, private_credit_market_opacity, theater_ratio, 0, 0.32).
narrative_ontology:measurement(pcmo_tr_t8, private_credit_market_opacity, theater_ratio, 8, 0.44).
narrative_ontology:measurement(pcmo_tr_t15, private_credit_market_opacity, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(pcmo_be_t0, private_credit_market_opacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pcmo_be_t8, private_credit_market_opacity, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(pcmo_be_t15, private_credit_market_opacity, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(private_credit_market_opacity, resource_allocation).
narrative_ontology:affects_constraint(private_credit_market_opacity, leverage_cascade_systemic_risk).
narrative_ontology:affects_constraint(private_credit_market_opacity, public_bond_market_degradation).
narrative_ontology:affects_constraint(private_credit_market_opacity, regulatory_arbitrage_capital_flight).

% DUAL FORMULATION NOTE:
% Private credit market opacity can be decomposed into at least two structurally distinct claims: (1) information asymmetry as necessary for market function (ε≈0.25, Rope perspective), vs (2) information asymmetry as extraction mechanism for fund manager rents (ε≈0.58, Snare perspective). The story presented here emphasizes the extraction reading (ε=0.58) because the empirical trajectory (growth from 0.35 to 0.58) and the absence of offsetting transparency safeguards indicate extraction is dominant. A pure Rope formulation would require evidence that opacity serves a coordination function that cannot be achieved through standardized reporting; the data does not support this.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(private_credit_market_opacity, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
