% ============================================================================
% CONSTRAINT STORY: mortgage_market_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mortgage_market_concentration, []).

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
 *   constraint_id: mortgage_market_concentration
 *   human_readable: Mortgage Market Concentration and Extractive Financial Intermediation
 *   domain: financial_regulation/housing_economics
 *
 * SUMMARY:
 *   The U.S. mortgage market exhibits structural concentration that has
 *   increased significantly since 2008. Four large originators (JPMorgan
 *   Chase, Bank of America, Wells Fargo, Rocket Companies) control
 *   approximately 55-60% of origination volume. This concentration creates a
 *   tangled coordination-extraction hybrid: genuine coordination function
 *   exists (matching borrowers to lenders, distributing credit risk across
 *   capital markets, enabling homeownership access for millions), but
 *   significant asymmetric extraction manifests through higher rates, wider
 *   spreads, and reduced product variety for borrowers with constrained exit
 *   options. The constraint combines regulatory barriers to entry (capital
 *   requirements, compliance costs favor scale), network effects in
 *   securitization (larger originators access better secondary market terms),
 *   and first-mover advantages consolidated during the post-2008 recovery
 *   when large banks absorbed failing originators. The theater ratio (0.48)
 *   reflects that regulatory compliance (Dodd-Frank stress testing,
 *   anti-predatory lending rules) has become partly ritualistic — compliance
 *   is observable, but actual prevention of extractive pricing is ambiguous.
 *   The extractiveness has increased over the decade as originators
 *   internalize gains from scale and as refinancing windows compress,
 *   reducing the frequency at which borrowers can escape rate locks.
 *
 * KEY AGENTS:
 *   - Prospective Homebuyers: Primary victim (powerless/trapped) — must navigate concentrated market; bear extraction as higher rates and fees during origination; no meaningful alternative financing sources
 *   - Refinancing Borrowers: Secondary victim (powerless/trapped) — locked into existing rates during windows; bear high exit costs when refinancing market deteriorates; benefit when rates fall but face concentrated servicer pricing
 *   - Regional Community Banks: Constrained actor (moderate/constrained) — capacity-constrained by regulatory capital requirements; participate in secondary markets; benefit from deposit franchise; bear margin compression from national originators
 *   - Large Mortgage Originators (JPMorgan, BofA, Wells, Rocket): Primary beneficiary (institutional/arbitrage) — set pricing in concentrated market; access securitization and capital markets; arbitrage regulatory differences; capture origination and servicing spreads
 *   - Institutional Investors (Pension Funds, Insurers, Asset Managers): Secondary beneficiary (institutional/arbitrage) — benefit from liquid MBS markets; can arbitrage across securities; access to diversified mortgage pools; minimal extraction experience
 *   - Housing Affordability Coalition (HUD, State Regulators, Consumer Advocates): Organized reformers (organized/constrained) — working toward policy solutions (affordable housing programs, FHA reform, downpayment assistance); facing political barriers but not blocked
 *   - Federal Reserve Regulatory System: Institutional enforcer (institutional/arbitrage) — maintains Dodd-Frank framework but framework has become partly ritualized; uses stress testing to absorb incumbent banks' capital costs; smaller competitors cannot compete even if compliant
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both coordination (mortgage access achieved) and extraction (at concentration-inflated prices); classification basis for engine's constraint_claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mortgage_market_concentration, 0.58).
domain_priors:suppression_score(mortgage_market_concentration, 0.62).
domain_priors:theater_ratio(mortgage_market_concentration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mortgage_market_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(mortgage_market_concentration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(mortgage_market_concentration, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mortgage_market_concentration, tangled_rope).
narrative_ontology:human_readable(mortgage_market_concentration, "Mortgage Market Concentration and Extractive Financial Intermediation").
narrative_ontology:topic_domain(mortgage_market_concentration, "financial_regulation/housing_economics").

domain_priors:requires_active_enforcement(mortgage_market_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mortgage_market_concentration, large_mortgage_originators).
narrative_ontology:constraint_beneficiary(mortgage_market_concentration, institutional_investors).
narrative_ontology:constraint_beneficiary(mortgage_market_concentration, shadow_banking_intermediaries).
narrative_ontology:constraint_victim(mortgage_market_concentration, prospective_homebuyers).
narrative_ontology:constraint_victim(mortgage_market_concentration, refinancing_borrowers).
narrative_ontology:constraint_victim(mortgage_market_concentration, regional_credit_availability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROSPECTIVE HOMEBUYER (SNARE) — No meaningful exit from the concentrated market. Must accept terms set by oligopoly originators. Trapped by geographic immobility, time constraints (pregnancy, job relocation), and lack of alternative financing sources. Bears full extraction as higher rates, fees, and reduced product variety. Cannot organize or coordinate around alternatives.
constraint_indexing:constraint_classification(mortgage_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL COMMUNITY BANK (TANGLED ROPE) — Constrained by regulatory capital requirements, compliance costs, and inability to compete on rate/scale with national originators. But genuine coordination function exists: community banks serve credit-constrained borrowers and maintain local lending relationships that national servicers abandon. Benefits from regulatory forbearance and deposit franchise; bears extraction through margin compression and mandatory participation in securitization chains.
constraint_indexing:constraint_classification(mortgage_market_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE MORTGAGE ORIGINATOR (ROPE) — Primary beneficiary with maximum arbitrage options. Experiences the concentration as beneficial coordination: scale enables securitization, risk distribution, and secondary market access. Can exit individual markets; can arbitrage regulatory differences across states. Extraction flows toward this agent — they set pricing, originate volume, and capture servicing spread.
constraint_indexing:constraint_classification(mortgage_market_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL INVESTOR (ROPE) — Pension funds, insurers, and asset managers benefit from mortgage-backed securities concentration. Large, liquid pools of standardized mortgages enable efficient portfolio construction. Arbitrage options (can buy/sell in secondary markets, can switch to alternative assets). Experiences concentration as beneficial coordination mechanism with minimal friction.
constraint_indexing:constraint_classification(mortgage_market_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HOUSING AFFORDABILITY COALITION (SCAFFOLD) — Organized agents (state regulators, housing advocates, consumer protection bureaus) see concentration as a temporary policy problem with a sunset: affordable housing policy, downpayment assistance programs, first-time buyer initiatives, and potential FHA/Fannie Mae reform offer pathways to deconcentrate origination. Suppression is significant (political opposition from incumbents) but declining as demographics and affordability crises create political pressure. Coalition members experience extraction but see exit strategy.
constraint_indexing:constraint_classification(mortgage_market_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL RESERVE REGULATORY FRAMEWORK (PITON) — Post-2008 Dodd-Frank stress testing, capital requirements, and anti-predatory lending rules were designed to prevent concentration and extraction. But the rules became ritualized: large banks absorb the compliance costs; smaller competitors cannot. The regulatory framework persists through institutional inertia (the rules remain on the books) but the original coordination function — preventing predatory extraction — has been partly displaced by the rules themselves becoming barriers to entry. Theater ratio high: regulatory compliance theater replaces actual prevention.
constraint_indexing:constraint_classification(mortgage_market_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The mortgage market is fundamentally a coordination mechanism (matching borrowers to lenders across geographic and temporal gaps, distributing credit risk to those who can bear it). But the current concentration structure exhibits significant asymmetric extraction: borrowers have constrained exit options, originators have arbitrage options, and the distribution of gains is heavily skewed. The analytical perspective sees both functions simultaneously — genuine coordination (borrowers do access mortgages; risk is distributed) and genuine extraction (at concentration-inflated rates, fees, and restricted product variety). This perspective serves as the basis for the engine's computed constraint_claim.
constraint_indexing:constraint_classification(mortgage_market_concentration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mortgage_market_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mortgage_market_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mortgage_market_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mortgage_market_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mortgage_market_concentration, TR),
    TR >= 0.70.

:- end_tests(mortgage_market_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts value from constrained borrowers through higher rates, fees, and reduced product variety, but extraction is not absolute — borrowers do access credit, and large originators also bear real costs (regulatory compliance, capital requirements, default risk). The extraction has increased over the measurement interval as concentration has deepened and as refinancing windows compressed post-2021 rate rises. Historical data shows that mortgage spreads have widened as concentration has increased; originator profitability has risen while origination volume remained stable, indicating price-setting power rather than service expansion. Suppression (0.62): Moderate-high. Significant barriers to exit include: (1) geographic immobility (most borrowers cannot relocate to access alternative credit markets), (2) time constraints (job changes, family events force origination timing), (3) information asymmetry (complex mortgage products limit shopping effectiveness), (4) regulatory barriers to entry (capital requirements of $500M+ for national licensure), (5) network effects (largest originators access best secondary market rates, creating competitive advantages unrelated to service quality). However, suppression is not total — some exit exists (borrowers can delay, shop intensively, use mortgage brokers, access FHA), and some regional competition persists. Theater ratio (0.48): Moderate. Regulatory compliance (stress testing, anti-predatory lending rule documentation) is observable but its correlation with actual prevention of extractive pricing is ambiguous. Large originators demonstrate compliance but maintain pricing power; smaller competitors demonstrate compliance but cannot compete. The theater has increased as regulatory frameworks have proliferated without clear impact on consumer injury rates.
 *
 * PERSPECTIVAL GAP:
 *   The mortgage market concentration constraint exhibits a clean perspectival gap between beneficiaries and victims. Large originators (institutional/arbitrage) perceive rope — they experience the concentration as beneficial coordination with minimal enforcement cost. Prospective homebuyers (powerless/trapped) perceive snare — they experience extraction with no exit. Regional banks (moderate/constrained) perceive tangled rope — genuine coordination function (community credit access) but extraction through margin compression and competitive disadvantage. Institutional investors (institutional/arbitrage) perceive rope — they benefit from liquid secondary markets. Organized reformers (organized/constrained) perceive scaffold — they see concentration as a policy problem with an exit strategy (housing affordability programs, alternative lending models, regulatory reform). The regulatory framework (institutional/arbitrage) perceives piton — stress testing and anti-predatory lending rules persist through institutional inertia even as their original prevention function has been partly achieved and partly supplanted by the rules themselves acting as barriers to entry. The analytical observer perceives tangled rope — both coordination (borrowers access credit) and extraction (at concentration-inflated rates) are structurally real. The gap reveals that concentration benefits those with arbitrage options and harms those with trapped options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural relationship to the extraction flow. Large originators have arbitrage options (can originate in multiple states, can exit individual markets, can securitize) — d ≈ 0.10-0.15 → f(d) ≈ -0.05 to -0.01, producing negative or near-zero χ (they experience the constraint as beneficial coordination). Prospective homebuyers are trapped (cannot relocate, face time constraints, have no alternative financing) — d ≈ 0.90 → f(d) ≈ 1.42, producing high χ (they bear maximum extraction). Regional banks are constrained (face capital requirements but can retain portfolio, can participate in limited secondary markets) — d ≈ 0.55-0.65 → f(d) ≈ 0.75-1.00, producing moderate-high χ. Institutional investors have arbitrage options — d ≈ 0.15-0.25 → f(d) ≈ 0.02-0.15, producing near-zero to low χ. The scope modifier σ(S) = 1.0 for national scope, so χ = ε × f(d) × 1.0. For the powerless borrower, χ ≈ 0.58 × 1.42 × 1.0 ≈ 0.82 (high effective extraction). For the institutional originator, χ ≈ 0.58 × (-0.05) × 1.0 ≈ -0.03 (extraction runs toward this agent, not away). This directionality explains why the same base_properties produce different classifications: perspectives differ in power, exit options, and time horizon, which map to different d values through the derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The mortgage market concentration satisfies all three Tangled Rope requirements. (1) BENEFICIARIES DECLARED: large_mortgage_originators, institutional_investors, shadow_banking_intermediaries — these actors benefit from concentration's scale, securitization access, and pricing power. (2) VICTIMS DECLARED: prospective_homebuyers, refinancing_borrowers, regional_credit_availability — these entities bear extraction through higher rates, compressed margins, reduced product variety, and constrained market access. (3) ACTIVE ENFORCEMENT DECLARED: requires_active_enforcement = true — the constraint is maintained through regulatory frameworks (licensing, capital requirements, compliance costs) and market structure (network effects in securitization), not through spontaneous coordination. The mandatrophy is resolved by showing that the beneficiary's rope classification (they see genuine coordination at favorable terms) and the victim's snare classification (they see extraction with no exit) are both correct within their respective structural positions. The analytical observer sees tangled rope because the full system includes both functions simultaneously. The constraint is NOT a snare (pure extraction) because genuine coordination does occur — borrowers do access mortgages, credit is distributed, risk is shared. It is NOT a rope (pure coordination) because asymmetric extraction is significant — originator profitability has risen as competition has declined, borrowers face higher rates than fundamental costs justify, and smaller competitors are excluded. It is tangled rope because the same mechanism (large-scale securitization, regulatory capital requirements, network effects) that enables coordination also enables extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    concentration_causal_mechanism,
    'Does mortgage market concentration itself cause higher rates and fees, or does concentration reflect rational response to underlying credit risk and capital costs?',
    'Comparative analysis of loan terms (rates, fees, approval rates) across markets with different concentration levels, controlling for borrower creditworthiness, local economic conditions, and securitization costs. Instrumental variable analysis using regulatory mergers and market consolidations.',
    'If concentration causes extraction (rates > fundamental cost): snare classification is correct. If concentration is correlated with other factors (credit risk, regulatory burden): extraction may be overstated, classification shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concentration_causal_mechanism, empirical, 'Whether concentration itself causes extraction or reflects rational responses to risk').

omega_variable(
    regulatory_barrier_endogeneity,
    'Do large originators benefit from regulation because they can absorb compliance costs, or does regulation genuinely prevent predatory extraction?',
    'Historical comparison of non-bank lending (less regulated) vs bank lending; longitudinal analysis of compliance costs by institution size; tracking of consumer injury rates pre- and post-regulation.',
    'If regulation primarily raises barriers to entry: piton perspective is correct, concentration persists due to institutional inertia. If regulation genuinely prevents predatory harms: scaffold perspective is correct, rules work but need secondary market reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_barrier_endogeneity, empirical, 'Whether regulation causes or prevents mortgage market concentration').

omega_variable(
    secondary_market_necessity,
    'Is securitization (converting mortgages to tradeable securities) a necessary coordination mechanism, or a rent-extraction layer that could be replaced by alternative financing?',
    'Analysis of securitization cost (originator fees, servicer spreads, rating agency fees) vs securitization benefit (ability to distribute risk, access capital markets). Comparison to countries using alternative models (retained-portfolio lending, covered bonds, state-backed mortgage guarantee).',
    'If securitization is necessary: concentration of originators is inevitable (only large firms can securitize efficiently). If securitization is rent-extraction: decentralized lending or alternative instruments could reduce concentration without financial instability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_market_necessity, empirical, 'Whether securitization is necessary or is a rent-extraction layer').

omega_variable(
    political_economy_of_reform,
    'Can housing policy (downpayment assistance, state lending, alternative mortgage products) actually reduce concentration, or is political capture sufficient to prevent reform?',
    'Historical analysis of previous concentration-reduction attempts (FHA reform, state lending initiatives, non-bank lender regulations). Track regulatory capture indicators (lobbying spend, political contributions, regulatory agency staffing) correlating with policy reversals.',
    'If reform is politically feasible: scaffold sunset is real. If capture is sufficient to block all reform: concentration persists indefinitely, constraint degrades to piton (maintained by political inertia rather than policy choice).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_economy_of_reform, preference, 'Whether housing policy reform can overcome political capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mortgage_market_concentration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mortg_tr_t0, mortgage_market_concentration, theater_ratio, 0, 0.28).
narrative_ontology:measurement(mortg_tr_t5, mortgage_market_concentration, theater_ratio, 5, 0.38).
narrative_ontology:measurement(mortg_tr_t10, mortgage_market_concentration, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(mortg_be_t0, mortgage_market_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mortg_be_t5, mortgage_market_concentration, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(mortg_be_t10, mortgage_market_concentration, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mortgage_market_concentration, resource_allocation).
narrative_ontology:boltzmann_floor_override(mortgage_market_concentration, 0.18).
narrative_ontology:affects_constraint(mortgage_market_concentration, housing_affordability_crisis).
narrative_ontology:affects_constraint(mortgage_market_concentration, predatory_lending_externality).
narrative_ontology:affects_constraint(mortgage_market_concentration, securitization_opacity).

% DUAL FORMULATION NOTE:
% Mortgage market concentration is a constraint family with structural dependents: housing_affordability_crisis (downstream — concentrated origination raises entry costs for homebuyers), predatory_lending_externality (sibling — concentration enables predatory product proliferation by reducing reputational cost across wide market), and securitization_opacity (upstream — secondary market opacity enables concentration by reducing pricing transparency). Each story has distinct ε values reflecting different measurable aspects of the mortgage system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
