% ============================================================================
% CONSTRAINT STORY: uk_help_to_buy_scheme
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_help_to_buy_scheme, []).

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
 *   constraint_id: uk_help_to_buy_scheme
 *   human_readable: UK 'Help to Buy' Equity Loan Scheme
 *   domain: economic/housing_finance
 *
 * SUMMARY:
 *   The UK Help to Buy scheme (2013-2023) exemplifies a constraint system
 *   where government attempts to coordinate housing market access but
 *   systematically enables extraction of the subsidy value by builders and
 *   existing asset holders. The scheme offered government equity loans
 *   covering up to 20% of new-build home purchases, allowing first-time
 *   buyers to enter the market with 5% deposits. Ostensibly coordination for
 *   housing supply, the constraint's structural properties reveal tangled
 *   asymmetry: the scheme coordination function (matching supply with
 *   subsidized demand, boosting construction employment) is real, but the
 *   extraction mechanism (builders capturing price inflation, government
 *   accumulating contingent liability, first-time buyers locked into debt) is
 *   equally structural. The scheme's theater (narrative of 'access' and
 *   'aspiration') obscures that subsidized demand in inelastic markets
 *   transfers wealth upward, not outward. The constraint demonstrates how
 *   positive intentions and real coordination benefits can coexist with
 *   systematic extraction when the underlying structural problem (inelastic
 *   housing supply, capital concentration in asset ownership) remains
 *   unaddressed.
 *
 * KEY AGENTS:
 *   - First-time Buyers: Primary victims (powerless/trapped) — locked into equity loan obligation with contingent tax liability and limited exit options
 *   - House Builders and Developers: Primary beneficiaries (institutional/arbitrage) — capture demand subsidy through price inflation with minimal downside risk
 *   - Government Fiscal Authority: Secondary beneficiary/victim (organized/constrained) — short-term fiscal benefit (tax receipts, employment), long-term contingent liability (loan book losses, house price risk)
 *   - Private Rental Market Participants: Secondary victims (moderate/constrained) — crowded out by subsidized ownership demand, depressed rental yields
 *   - Housing Policy Establishment: Institutional actors (institutional/arbitrage) — maintain scheme narrative despite evidence of price capture and limited supply elasticity
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — sees scheme as systemic extraction mechanism within inelastic housing market
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_help_to_buy_scheme, 0.58).
domain_priors:suppression_score(uk_help_to_buy_scheme, 0.62).
domain_priors:theater_ratio(uk_help_to_buy_scheme, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_help_to_buy_scheme, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_help_to_buy_scheme, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(uk_help_to_buy_scheme, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_help_to_buy_scheme, tangled_rope).
narrative_ontology:human_readable(uk_help_to_buy_scheme, "UK 'Help to Buy' Equity Loan Scheme").
narrative_ontology:topic_domain(uk_help_to_buy_scheme, "economic/housing_finance").

domain_priors:requires_active_enforcement(uk_help_to_buy_scheme).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_help_to_buy_scheme, house_builders).
narrative_ontology:constraint_beneficiary(uk_help_to_buy_scheme, government_short_term_fiscal_balance).
narrative_ontology:constraint_victim(uk_help_to_buy_scheme, first_time_buyers).
narrative_ontology:constraint_victim(uk_help_to_buy_scheme, private_rental_market_participants).
narrative_ontology:constraint_victim(uk_help_to_buy_scheme, fiscal_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIRST-TIME BUYER (SNARE) — Trapped by housing market inflation and insufficient savings. The equity loan creates apparent access but locks the buyer into mortgage dependency. Exit options severely constrained: cannot repay the equity loan without selling (triggering capital gains tax liability to government), cannot escape the housing market without losing accumulated equity. Experiences maximum extraction through debt obligation and contingent tax liability.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HOUSE BUILDERS (ROPE) — Primary beneficiary with high institutional power and exit options. Scheme functions as pure coordination for them: government guarantees demand, absorbs house-price inflation risk, and provides subsidized equity capital. Builders experience the constraint as market-making coordination — they capture turnover benefits and price appreciation without bearing buyer default risk. Net extraction flows toward them.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIVATE RENTAL MARKET (TANGLED ROPE) — Constrained by the scheme's displacement of demand toward ownership. Rental providers see coordination benefit (stable owner-occupancy reduces investor competition for properties) but experience extraction through depressed rental yields as government-subsidized ownership crowds them out. Extraction is moderate but persistent — they benefit from reduced competition but lose potential tenants to subsidized mortgages.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GOVERNMENT FISCAL AUTHORITY (TANGLED ROPE) — Experiences mixed coordination and extraction. Short-term coordination benefit: boosts construction employment, housing starts, and tax receipts (stamp duty, income tax on construction jobs). Long-term extraction cost: government holds contingent liability on equity loans, faces capital losses if house prices fall or borrowers default, accumulates fiscal pressure as the loan book matures. Constrained exit: cannot unwind without political cost or market disruption. Active enforcement required through loan servicing, valuation monitoring, and default procedures.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HOUSING POLICY NARRATIVE (PITON) — The scheme's justification rests on 'expanding home ownership' and 'supporting first-time buyers,' but the mechanism systematically favors builders and inflates house prices. The performative content (equity loan branding) masks the actual function (subsidized demand for new builds at higher price points). Theater ratio (0.68) reflects that scheme marketing emphasizes access and aspiration while the structural outcome is price capture and debt concentration. The ritual persists through institutional inertia — each government continuation represents reputational commitment rather than evidence of efficacy.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: HOUSING SUPPLY RESPONSE (SCAFFOLD) — If the scheme's intended function is genuinely to boost housing supply until market dynamics self-correct (reduced inflation), then it qualifies as temporary coordination support with a sunset clause implicit in its design horizon (2013-2023 explicit; theoretical sunset when supply-demand equilibrium stabilizes). This perspective sees moderate extraction but declining over time as supply-response success reduces need for subsidy. However, empirical evidence shows house prices continued rising despite increased completions, suggesting the sunset was aspirational rather than structural.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational, global perspective, the scheme represents a structural extraction mechanism within the broader housing market constraint system. Government equity loans do not reduce house prices; they absorb and redistribute them. Builders capture the subsidy value through price inflation. First-time buyers experience debt and contingent liability. Extraction persists because exit options for market participants are systemically constrained by inelastic housing supply and capital concentration. The scheme's theater (access narrative) masks structural inevitability: subsidized demand in inelastic markets transfers wealth to asset holders, not to buyers.
constraint_indexing:constraint_classification(uk_help_to_buy_scheme, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_help_to_buy_scheme_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_help_to_buy_scheme, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_help_to_buy_scheme, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_help_to_buy_scheme, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_help_to_buy_scheme, TR),
    TR >= 0.70.

:- end_tests(uk_help_to_buy_scheme_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting strong builder benefit and first-time buyer debt concentration with limited upside after equity loan repayment. The value represents the share of subsidy captured through price inflation and debt burden rather than genuine affordability expansion. The measurement trajectory (0.35→0.58 over interval) reflects increasing price capture as scheme matured and builder margins expanded. Suppression (0.62): Moderately high. First-time buyers face constrained exit options: cannot repay equity loan without selling (triggering capital gains tax on government's share), cannot escape housing market without losing accumulated equity, cannot renegotiate loan terms. Rental market participants suppressed through demand displacement. Government suppressed through contingent liability and political difficulty of unwinding. Theater ratio (0.68): Moderately high. Scheme marketing emphasizes 'access to homeownership' and 'helping first-time buyers,' but the mechanism primarily inflates prices and transfers subsidy to builders. The performative content (equity loan branding as 'support') masks structural outcome (subsidized demand → price capture). Theater increased over interval as gap between narrative (affordability boost) and reality (price inflation outpaced income growth) became more visible.
 *
 * PERSPECTIVAL GAP:
 *   The scheme demonstrates maximum perspectival divergence. First-time buyers experience pure extraction (Snare) — the equity loan feels like access but functions as debt lock-in. Builders experience pure coordination (Rope) — demand aggregation and risk absorption. Government experiences mixed benefit and extraction (Tangled Rope) — short-term fiscal benefits, long-term contingent losses. Rental market experiences displacement extraction (Tangled Rope) — some coordination benefit from reduced investor competition, significant extraction from lost tenants. The policy narrative experiences performative ritual persistence (Piton) — continues because exit costs (political credibility, admission of design failure) exceed costs of perpetuation. The analytical observer sees systemic extraction (Snare) — scheme cannot solve inelastic supply problem, so subsidized demand transfers wealth to asset holders. The scaffold perspective (temporary supply boost) is theoretically possible but empirically contradicted: housing completions increased, but house prices inflated faster, indicating demand-side subsidy without supply-side elasticity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit constraints. First-time buyers occupy maximum extraction position (d ≈ 0.92): they are declared victims, face trapped exit (cannot repay equity loan without selling, cannot exit housing market), and possess minimal power. House builders occupy minimum extraction position (d ≈ 0.08): they are declared beneficiaries, hold arbitrage options (can redirect capital to other projects), and wield institutional power. Government occupies mid-extraction position (d ≈ 0.50): declared as both beneficiary (short-term tax/employment gains) and victim (long-term liability), with constrained exit (political difficulty of unwinding, financial system stability constraints). Private rental participants occupy moderate extraction position (d ≈ 0.65): declared victims of demand displacement, with constrained but real exit options (conversion to ownership models, geographic relocation, strategic repositioning). These structural relationships are stable across measurement period; directionality overrides are not required.
 *
 * MANDATROPHY ANALYSIS:
 *   The scheme's classification as Tangled Rope is justified by the joint presence of (1) genuine coordination function — matching subsidized demand with increased housing supply, creating construction employment and stabilizing fiscal position short-term, and (2) genuine asymmetric extraction — builders capture price inflation, first-time buyers lock into debt with contingent tax liability, government accumulates contingent losses. The scheme prevents misclassification as pure Rope (which would deny extraction) or pure Snare (which would deny coordination benefits). However, the empirical evidence suggests the extraction component has dominated and grown over time (theater_ratio 0.55→0.68, extractiveness 0.35→0.58), raising the question of whether the constraint is degrading from Tangled Rope toward Snare. The mandatrophy remains unresolved because the counterfactual is ambiguous: would first-time buyers have been better off renting, saving longer, or under alternative policy regimes? The scheme's shutdown in 2023 without replacement suggests institutional recognition of extraction dominance, but the retrospective classification of participants' welfare remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_response_efficacy,
    'Did the Help to Buy scheme genuinely increase net housing supply, or did it primarily inflate prices without expanding the housing stock?',
    'Time-series analysis of housing completions with/without scheme; correlation between scheme rollout and house price inflation; cross-regional comparison of areas with high vs low scheme uptake',
    'If supply response is real and sufficient: Scaffold classification confirmed — temporary support with sunset logic. If supply response is negligible: Snare classification confirmed — pure extraction with performative supply narrative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_response_efficacy, empirical, 'Whether scheme increased net housing supply or primarily inflated prices').

omega_variable(
    buyer_welfare_net_positive,
    'Did participating first-time buyers experience net positive welfare compared to renting or saving longer for conventional mortgages?',
    'Longitudinal tracking of buyer outcomes: equity loan repayment rates, house price performance relative to buyer entry point, default rates, buyer net wealth after repayment vs counterfactual rental/savings path',
    'If net positive: scheme delivers genuine access benefit, reducing extraction classification. If net negative: buyers trapped in worse position than alternatives, confirming Snare classification from buyer perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(buyer_welfare_net_positive, empirical, 'Net welfare impact on participating first-time buyers').

omega_variable(
    builder_price_capture_mechanism,
    'What fraction of the scheme''s subsidy value (equity loan plus induced demand) was captured by builders through price inflation vs passed to buyers through genuine affordability gains?',
    'Price trajectory analysis pre- and post-scheme; builder margin analysis for Help to Buy schemes vs non-eligible properties; hedonic pricing models isolating scheme-driven premiums',
    'If capture > 70%: confirms builders as primary beneficiary, snare for buyers. If capture < 30%: meaningful affordability transfer occurred, tangled_rope classification more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(builder_price_capture_mechanism, empirical, 'Share of scheme subsidy captured by builders through prices').

omega_variable(
    fiscal_liability_materialization,
    'Will government equity loan book losses materialize when house prices decline or borrowers default in economic downturns?',
    'Macroeconomic stress testing; house price scenario analysis; default rate modeling for Help to Buy borrowers under recession conditions; government balance sheet contingency accounting',
    'If losses materialize: government fiscal extraction cost confirmed, pushing government perspective from tangled_rope toward snare. If losses absorbed by sustained housing inflation: fiscal sustainability remains contingent, scaffold sunset logic fails.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_liability_materialization, empirical, 'Whether government equity loan losses will materialize').

omega_variable(
    counterfactual_policy_effectiveness,
    'Would alternative policies (supply-side investment, land value taxation, direct rental subsidies, inclusionary zoning) have produced better housing outcomes at lower fiscal cost?',
    'Comparative policy analysis using housing econometrics; international benchmarking of outcomes under alternative policy regimes; cost-per-unit-supplied analysis across policy options',
    'If alternatives clearly superior: scheme represents institutional path dependence (piton). If alternatives comparable/inferior: scheme''s design choices become defensible, reducing piton classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_policy_effectiveness, conceptual, 'Comparative effectiveness of alternative housing policy options').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_help_to_buy_scheme, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(htb_tr_t0, uk_help_to_buy_scheme, theater_ratio, 0, 0.55).
narrative_ontology:measurement(htb_tr_t5, uk_help_to_buy_scheme, theater_ratio, 5, 0.62).
narrative_ontology:measurement(htb_tr_t10, uk_help_to_buy_scheme, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(htb_be_t0, uk_help_to_buy_scheme, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(htb_be_t5, uk_help_to_buy_scheme, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(htb_be_t10, uk_help_to_buy_scheme, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_help_to_buy_scheme, resource_allocation).
narrative_ontology:affects_constraint(uk_help_to_buy_scheme, uk_housing_supply_inelasticity).
narrative_ontology:affects_constraint(uk_help_to_buy_scheme, housing_wealth_inequality_concentration).

% DUAL FORMULATION NOTE:
% The Help to Buy scheme is downstream of the broader UK housing supply constraint (inelastic supply, planning restrictions, capital concentration). The scheme's extractiveness is amplified by upstream supply inelasticity — subsidized demand without supply response forces price inflation that captures the subsidy value. Related constraints: housing_wealth_inequality_concentration (scheme redistributes subsidy to asset holders, widening wealth gaps), planning_permission_bottleneck (upstream constraint on supply elasticity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
