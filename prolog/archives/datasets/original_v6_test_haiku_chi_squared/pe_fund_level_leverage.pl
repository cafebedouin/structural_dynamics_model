% ============================================================================
% CONSTRAINT STORY: pe_fund_level_leverage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pe_fund_level_leverage, []).

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
 *   constraint_id: pe_fund_level_leverage
 *   human_readable: Shadow Leverage via fund-level debt in Private Equity
 *   domain: economic/financial_engineering
 *
 * SUMMARY:
 *   Fund-level leverage in private equity, particularly through Net Asset
 *   Value (NAV) loans and similar structures, represents a form of 'shadow
 *   leverage' that concentrates risk while distributing extraction across
 *   multiple victim classes. General Partners use borrowed capital against
 *   entire fund portfolios to deploy capital without selling existing
 *   positions, amplify returns through leverage, and extract fees on a larger
 *   asset base. The constraint exhibits a stark perspectival divergence: GPs
 *   experience it as coordination (efficient capital deployment); LPs
 *   experience it as Snare (trapped capital with hidden downside); employees
 *   and creditors experience it as extraction risk with minimal voice;
 *   regulators experience it as mixed coordination/systemic risk; the
 *   analytical observer sees theatrical risk concentration masked by
 *   'efficient capital deployment' narratives. The theater ratio (0.55)
 *   reflects that fund-level leverage relies heavily on narrative
 *   justification (market timing, operational synergies) rather than
 *   transparent risk management. The constraint's extractiveness has risen
 *   from 0.38 to 0.58 over the measured interval as leverage ratios have
 *   increased and information asymmetries have deepened.
 *
 * KEY AGENTS:
 *   - General Partners: Primary beneficiary (institutional/arbitrage) — capture fee extraction on enlarged AUM, leverage amplification of returns, liquidity for distributions
 *   - Limited Partners: Primary victim (powerless/trapped) — institutional investors (pensions, endowments) locked into funds with limited visibility into leverage structures and refinancing risk
 *   - Portfolio Company Employees: Secondary victim (powerless/trapped) — workers in acquired firms bear downside from debt service prioritization and cascading layoffs
 *   - Creditor Banks/Debt Investors: Secondary victim (moderate/constrained) — face information asymmetry, securitization opacity, and constrained exit in downturn scenarios
 *   - Regulatory Authorities: Organized participant (organized/constrained) — possess tools to constrain leverage but face coordination failure and political pressure; constrained by offshore arbitrage threat
 *   - Financial System (Macro-prudential): Institutional actor (institutional/constrained) — sees both coordination benefits and systemic fragility; constrained by interconnection with shadow banking
 *   - Analytical Observer: Civilizational view (analytical/analytical) — observes inertial theatrical persistence of pre-2008 engineering norms maintained through regulatory arbitrage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pe_fund_level_leverage, 0.58).
domain_priors:suppression_score(pe_fund_level_leverage, 0.68).
domain_priors:theater_ratio(pe_fund_level_leverage, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pe_fund_level_leverage, extractiveness, 0.58).
narrative_ontology:constraint_metric(pe_fund_level_leverage, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(pe_fund_level_leverage, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pe_fund_level_leverage, snare).
narrative_ontology:human_readable(pe_fund_level_leverage, "Shadow Leverage via fund-level debt in Private Equity").
narrative_ontology:topic_domain(pe_fund_level_leverage, "economic/financial_engineering").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pe_fund_level_leverage, general_partners).
narrative_ontology:constraint_beneficiary(pe_fund_level_leverage, fund_sponsors).
narrative_ontology:constraint_victim(pe_fund_level_leverage, limited_partners).
narrative_ontology:constraint_victim(pe_fund_level_leverage, portfolio_company_employees).
narrative_ontology:constraint_victim(pe_fund_level_leverage, creditors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LIMITED PARTNERS (SNARE) — Institutional investors (pensions, endowments, insurance funds) commit capital to PE funds with limited visibility into NAV loan structures and refinancing risks. Exit is contractually constrained (lock-up periods); alternative is fund liquidation at substantial loss. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.97. High extraction with minimal alternatives.
constraint_indexing:constraint_classification(pe_fund_level_leverage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PORTFOLIO COMPANY EMPLOYEES (SNARE) — Workers in acquired companies bear downside risk from fund-level debt cascades: layoffs, wage suppression, pension underfunding when portfolio companies service debt rather than invest in operations. No participation in leverage decisions; exit costs are job loss and relocation. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.81. Structural extraction without voice.
constraint_indexing:constraint_classification(pe_fund_level_leverage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AUTHORITIES (TANGLED ROPE) — SEC, Federal Reserve, and banking regulators see fund-level leverage as both coordination mechanism (efficient capital deployment) and extraction risk (systemic fragility, moral hazard). Constrained by political pressure and industry lobbying; possess regulatory tools but face coordination problem: unilateral tightening drives capital offshore. d≈0.55, f(d)≈0.75, σ=1.1 → χ≈0.48. Mixed enforcement and coordination.
constraint_indexing:constraint_classification(pe_fund_level_leverage, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: GENERAL PARTNERS (ROPE) — GPs benefit from NAV loans through: (1) fee extraction (management fees on borrowed capital increase AUM), (2) leverage amplification (returns on deployed capital are magnified), (3) liquidity for dividends without portfolio sale. Experiences constraint as pure coordination solution: borrowing enables capital deployment that matches market timing. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.06. Negative effective extraction = net beneficiary. Can exit via refinancing or portfolio sale.
constraint_indexing:constraint_classification(pe_fund_level_leverage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDITOR BANKS AND DEBT INVESTORS (SNARE) — Lenders to PE funds face opacity: NAV loans are securitized rapidly, tranched, and sold to institutional investors with limited recourse visibility. Constrained exit (bonds trade at steep discounts; banks face regulatory capital charges for early writedowns). Information asymmetry favors borrower. d≈0.88, f(d)≈1.35, σ=1.1 → χ≈0.87. High extraction via leverage and information opacity.
constraint_indexing:constraint_classification(pe_fund_level_leverage, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: FINANCIAL SYSTEM STABILITY (TANGLED ROPE) — Macro-prudential perspective: fund-level leverage is both coordination mechanism (efficient matching of capital supply to deployment) and extraction mechanism (concentration of systemic risk, procyclical deleveraging in downturns). Constrained by interconnection (shadow banking is integral to modern capital markets); has regulatory tools but faces coordination failure: unilateral action fragments markets. d≈0.60, f(d)≈0.80, σ=1.2 → χ≈0.55. Mixed extraction and coordination.
constraint_indexing:constraint_classification(pe_fund_level_leverage, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (PITON) — From a systemic timescale, fund-level leverage appears as an inertial survival of pre-2008 financial engineering norms, now maintained through regulatory arbitrage and accounting opacity rather than genuine functional necessity. Theater_ratio=0.55: the 'efficient capital deployment' narrative masks risk concentration. The constraint persists because alternatives (direct fund-to-LP transparency, dynamic margin requirements) would redistribute power. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.78. High effective extraction but largely theatrical.
constraint_indexing:constraint_classification(pe_fund_level_leverage, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pe_fund_level_leverage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pe_fund_level_leverage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pe_fund_level_leverage, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pe_fund_level_leverage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pe_fund_level_leverage, TR),
    TR >= 0.70.

:- end_tests(pe_fund_level_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. GPs extract through multiple channels: (1) management fees on borrowed capital increase AUM and fee base even when underlying investments are unproven, (2) leverage amplifies returns, shifting downside to LPs/creditors, (3) liquidity for distributions without portfolio realization event, (4) information asymmetry—LP knowledge of NAV loan terms, refinancing risk, and asset coverage ratios is severely limited. The value has risen from 0.38 as leverage ratios have climbed and securitization has reduced transparency. Suppression (0.68): High. Multiple barriers prevent LP and creditor exit or resistance: (1) contractual lock-up periods and redemption gates restrict LP exit, (2) rapid securitization distributes NAV loans across institutional investor base, fragmenting creditor coordination, (3) accounting standards obscure leverage ratios from comparative analysis, (4) regulatory arbitrage (offshore vehicles, alternative investment fund loopholes) punishes transparency. Theater ratio (0.55): Moderate. The constraint relies substantially on narrative: 'market timing efficiency,' 'strategic capital deployment,' 'return amplification for LPs.' But functional justification is weaker than claimed—fund-level leverage primarily extracts from LPs and creditors, with distribution to GPs. The rise from 0.42 reflects increasing emphasis on 'efficient capital markets' rhetoric to justify elevated leverage in late-cycle fund vintages.
 *
 * PERSPECTIVAL GAP:
 *   Stark perspectival divergence across seven perspectives. GPs (Rope) experience efficient capital coordination—they can access liquidity without forced portfolio sales, matching capital deployment to market timing. LPs and employees (Snare) experience pure extraction—constrained exit, no leverage decision voice, bearing downside risk concentrated in downturns. Creditors (Snare) experience information asymmetry and constrained exit through securitization fragmentation. Regulators (Tangled Rope) experience mixed coordination (efficient capital deployment) and systemic extraction (concentration of leveraged deleverage risk). Financial system (Tangled Rope) sees coordination benefits offset by procyclical deleveraging amplification. The analytical observer (Piton) sees theatrical risk concentration maintained through inertia and regulatory arbitrage. The perspectival divergence is most pronounced between the beneficiary (GP, Rope, negative effective extraction) and the primary victim (LP, Snare, extraction 0.97).
 *
 * DIRECTIONALITY LOGIC:
 *   Limited Partners: Victim + trapped → d≈0.92, f(d)≈1.40. Contractual lock-up and no practical exit option. General Partners: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Multiple exit options (refinancing, portfolio sale, fund closure). Portfolio Company Employees: Victim + trapped → d≈0.95, f(d)≈1.42. Job loss is exit cost; no leverage decision participation. Creditors: Victim + constrained → d≈0.88, f(d)≈1.35. Securitization fragments creditor base; exit via secondary market at steep loss. Regulators: Mixed (victim of systemic risk + beneficiary of efficient capital markets) + constrained → d≈0.55, f(d)≈0.75. Political pressure prevents aggressive action; international coordination barriers. Financial System: Mixed + constrained → d≈0.60, f(d)≈0.80. Both benefits from capital efficiency and risks systemic fragility; constrained by market interconnection. Analytical Observer: analytical → d≈0.72, f(d)≈1.15. Observes theatrical maintenance of constraint despite functional necessity being questionable.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: Fund-level leverage is a genuine Snare from the LP and employee perspectives (pure extraction, high suppression, multiple victim classes, no coordinating benefits). The GP perspective (Rope) represents their structural experience as a beneficiary with exit options, not a true classification dispute. The mandate contradiction—'is this coordination or extraction?'—is resolved by perspectival decomposition: it IS pure coordination for the beneficiary (GP) and pure extraction for the victims (LP, employees, creditors). The regulatory perspective (Tangled Rope) is legitimate because regulators genuinely face a mixed problem: they benefit from capital market efficiency AND are forced to manage systemic risk concentration. The constraint's ε=0.58 reflects the preponderance of extraction harm (affecting trapped powerless agents) over coordination benefit (accruing to institutional beneficiaries). Mandatrophy is resolved because the taxonomy does not claim a single 'true' classification—the Snare classification is the system's answer when asking 'what is the structural relationship from the perspective of the primary victim?' The Rope classification is the answer when asking 'from the perspective of the primary beneficiary?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    systemic_cascading_risk_threshold,
    'At what fund-level leverage ratio does systemic risk transition from contained portfolio drawdown to cascading margin calls and fire-sale deleveraging?',
    'Stress testing across fund cohorts; analysis of 2008 PE deleveraging sequences and 2020 COVID margin event patterns; correlation of leverage ratios with contagion spread',
    'If threshold is well-defined and widely understood: leverage becomes manageable constraint (Rope/Tangled Rope). If threshold is opaque and non-linear: leverage remains Snare with hidden extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_cascading_risk_threshold, empirical, 'Systemic cascading risk threshold in fund leverage').

omega_variable(
    information_asymmetry_resolution,
    'Can regulatory disclosure requirements (detailed NAV loan terms, asset coverage ratios, stress test results) materially reduce LP extraction without driving capital offshore?',
    'Comparison of disclosed vs opaque fund performance metrics; analysis of GP response to disclosure regimes in EU vs US; measurement of LP redemption volatility post-disclosure',
    'If disclosure effective: constraint reverts toward Tangled Rope with balanced extraction/coordination. If GPs relocate offshore: constraint shifts to regulatory arbitrage (Snare at global level).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_resolution, empirical, 'Information asymmetry reducibility via disclosure').

omega_variable(
    alternative_coordination_mechanism_viability,
    'Would direct LP co-investment in portfolio companies (co-ownership model) or dynamic margin requirements replace fund-level leverage as a capital deployment mechanism?',
    'Pilot programs with LP consortia; analysis of Canadian pension fund co-investment outcomes; modeling of capital efficiency under alternative structures',
    'If viable alternatives exist: current Snare is contingent institutional arrangement (Rope/Scaffold possible). If GP leverage is structural necessity: extraction is inherent to PE model (Snare confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_mechanism_viability, conceptual, 'Viability of alternative fund capital deployment mechanisms').

omega_variable(
    moral_hazard_magnitude,
    'How much of GP leverage behavior reflects genuine market timing / capital deployment efficiency vs anticipated bailout / regulatory forbearance in downturns?',
    'Comparative analysis of leverage ratios pre vs post-2008/2020 rescue events; correlation of leverage timing with regulatory relief announcements; exit behavior studies (GPs repaying vs defaulting)',
    'If moral hazard is dominant: Snare classification confirmed (GPs extract via systemic risk creation). If efficiency is dominant: Tangled Rope classification more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_magnitude, empirical, 'Moral hazard component in GP leverage behavior').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pe_fund_level_leverage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pelev_tr_t0, pe_fund_level_leverage, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pelev_tr_t5, pe_fund_level_leverage, theater_ratio, 5, 0.5).
narrative_ontology:measurement(pelev_tr_t10, pe_fund_level_leverage, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(pelev_be_t0, pe_fund_level_leverage, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pelev_be_t5, pe_fund_level_leverage, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(pelev_be_t10, pe_fund_level_leverage, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pe_fund_level_leverage, resource_allocation).
narrative_ontology:boltzmann_floor_override(pe_fund_level_leverage, 0.45).
narrative_ontology:affects_constraint(pe_fund_level_leverage, portfolio_company_capital_structure).
narrative_ontology:affects_constraint(pe_fund_level_leverage, lp_redemption_gate_opacity).
narrative_ontology:affects_constraint(pe_fund_level_leverage, leveraged_buyout_employment_volatility).
narrative_ontology:affects_constraint(pe_fund_level_leverage, shadow_banking_systemic_risk).

% DUAL FORMULATION NOTE:
% Fund-level leverage decomposes into two structurally distinct claims: (1) efficiency hypothesis (NAV loans enable optimal capital deployment matching market timing) and (2) extraction hypothesis (leverage amplifies downside concentration, transferring risk from GPs to LPs/creditors). First is partially true (coordination function); second is dominant (extraction magnitude). The stories are linked because empirical validation of the efficiency hypothesis would lower ε; evidence of procyclical cascade risk raises ε and deepens Snare classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
