% ============================================================================
% CONSTRAINT STORY: monetary_regime_transition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_regime_transition, []).

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
 *   constraint_id: monetary_regime_transition
 *   human_readable: Sovereign Fiat Currency Regime
 *   domain: economic/political
 *
 * SUMMARY:
 *   The sovereign fiat currency regime represents the dominant global
 *   monetary system since the collapse of the Bretton Woods gold standard in
 *   1971. It combines elements of pure coordination (enabling large-scale
 *   commerce, storing value, facilitating price signals) with systematic
 *   extraction (seigniorage to the sovereign, inflation tax on currency
 *   holders, monopoly control over money creation). The constraint exhibits
 *   all nine perspectives mapped above, demonstrating that the regime's
 *   classification depends entirely on the observer's structural position and
 *   exit options. A currency user trapped by legal tender laws experiences
 *   snare. A commercial borrower with some hedging capacity experiences
 *   tangled rope. The sovereign issuer experiences rope. An emerging market
 *   debtor borrowing in foreign currency experiences snare. The regime's
 *   extractiveness has increased over the 50-year interval as monetary policy
 *   has become more accommodative and inflation more persistent, while the
 *   theater ratio has risen as central banks have adopted forward guidance,
 *   quantitative easing, and narrative-based policy tools that rely heavily
 *   on managing expectations rather than mechanical control. The rise of
 *   alternative currencies (crypto, stablecoins, central bank digital
 *   currencies) represents an organized coalition with a generational sunset
 *   clause — the fiat monopoly is not inevitable, but contingent on the
 *   coordination failure of alternatives.
 *
 * KEY AGENTS:
 *   - Sovereign Issuer / Central Government: Primary beneficiary (institutional/arbitrage) — captures seigniorage, monetary policy autonomy, and coordination benefits
 *   - Central Bank: Co-beneficiary (institutional/arbitrage) — controls monetary lever and inflation expectations
 *   - Commercial Banking System: Secondary beneficiary (organized/arbitrage) — benefits from seigniorage distribution, spreads from monetary control, and fractional reserve privilege
 *   - Currency Users (Daily Commerce): Primary victim (powerless/trapped) — cannot exit nominal system, bear inflation tax, lack hedging options
 *   - Fixed-Income Earners and Savers: Secondary victim (moderate/constrained) — lose real purchasing power through inflation without full exit option
 *   - Emerging Market Debtors: Tertiary victim (powerless/trapped) — borrowed in foreign currency, face extraction via exchange rate depreciation and interest rate shocks
 *   - Cryptocurrency and Digital Finance Coalition: Organized alternative (organized/constrained) — building technical escape routes with generational sunset timeline
 *   - Bretton Woods Legacy Institutions (IMF/World Bank): Institutional theater (institutional/constrained) — maintain narrative authority but limited functional control
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional arrangement as inherent monetary law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_regime_transition, 0.58).
domain_priors:suppression_score(monetary_regime_transition, 0.65).
domain_priors:theater_ratio(monetary_regime_transition, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_regime_transition, extractiveness, 0.58).
narrative_ontology:constraint_metric(monetary_regime_transition, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(monetary_regime_transition, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_regime_transition, tangled_rope).
narrative_ontology:human_readable(monetary_regime_transition, "Sovereign Fiat Currency Regime").
narrative_ontology:topic_domain(monetary_regime_transition, "economic/political").

domain_priors:requires_active_enforcement(monetary_regime_transition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_regime_transition, sovereign_issuer).
narrative_ontology:constraint_beneficiary(monetary_regime_transition, commercial_banking_system).
narrative_ontology:constraint_beneficiary(monetary_regime_transition, deficit_spending_governments).
narrative_ontology:constraint_victim(monetary_regime_transition, currency_users).
narrative_ontology:constraint_victim(monetary_regime_transition, fixed_income_earners).
narrative_ontology:constraint_victim(monetary_regime_transition, emerging_market_debtors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CURRENCY USER (SNARE) — Ordinary citizens cannot exit the national monetary system. They must denominate wages, savings, and debt in the regime currency with no alternative. Fixed-income earners and savers bear extraction through inflation without exit option. Suppression is total: legal tender laws, tax systems denominated in fiat, and the practical impossibility of conducting daily life outside the regime. This agent experiences the maximum structural extraction.
constraint_indexing:constraint_classification(monetary_regime_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMERCIAL BORROWER (TANGLED ROPE) — Non-financial firms borrow in the regime currency and benefit from low real interest rates when inflation is high, but face currency volatility, unexpected tightening cycles, and interest rate shocks. They have constrained exit (can denominate internationally, but with transaction costs and hedging overhead). Mixed experience: coordination benefit (stable monetary system enables commerce) and extraction (monetary policy surprises, inflation tax on long-term contracts).
constraint_indexing:constraint_classification(monetary_regime_transition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SOVEREIGN ISSUER (ROPE) — The state benefits from seigniorage (ability to finance spending via money creation), flexibility in monetary policy, and coordination of economic activity through central banking. Experiences low or negative extraction relative to their power. The regime is a coordination mechanism that enables large-scale economic organization. Exit option is arbitrage: a government can switch regimes (adopt foreign currency, issue crypto, return to commodity backing) but retains de facto control of the current system.
constraint_indexing:constraint_classification(monetary_regime_transition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CENTRAL BANK (ROPE) — The central banking authority has de facto control over money supply, interest rates, and inflation via monetary policy levers. Experiences the regime as pure coordination of the broader economy. Exit option is institutional (can be reformed, replaced, or subordinated, but controls the current lever). Net beneficiary — extraction runs toward this actor.
constraint_indexing:constraint_classification(monetary_regime_transition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CRYPTOCURRENCY AND ALTERNATIVE CURRENCY COALITION (SCAFFOLD) — Decentralized finance, stablecoins, and alternative currency systems (Bitcoin, Ethereum, and private digital currencies) represent an emergent bypass of the traditional fiat regime. These agents see the extraction (inflation, capital controls, negative real interest rates) and are building technical alternatives with decentralized governance. Scaffold classification reflects the generational horizon: as digital alternatives mature and gain adoption, they erode the sovereign issuer's monopoly on money creation. Estimated sunset: 10-30 years as central bank digital currencies and decentralized alternatives reach critical adoption thresholds.
constraint_indexing:constraint_classification(monetary_regime_transition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: BRETTON WOODS LEGACY SYSTEM (PITON) — The post-WWII international monetary order (IMF, World Bank, fixed-parity regimes that broke down in 1971) persists as a largely theatrical coordinating structure even though its original function (fixed exchange rates, capital controls) has been replaced by floating regimes and capital mobility. The International Monetary Fund continues to exist and manage crises, but primarily through narrative authority and conditional lending rather than functional monetary coordination. Theater ratio is high because the institutions perform legitimacy and oversight but have limited enforcement of actual monetary discipline. This is coordination infrastructure maintained through institutional inertia.
constraint_indexing:constraint_classification(monetary_regime_transition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: FIXED-INCOME EARNER / SAVER (TANGLED ROPE) — Retirees, bondholders, and savers on fixed nominal income experience the regime as mixed extraction and coordination. Coordination benefit: the regime enables predictable long-term contracts and borrowing. Extraction cost: inflation erodes real purchasing power without exit option. If inflation is moderate, the constraint is experienced as fair coordination (nominal returns, stable institutions). If inflation accelerates, the constraint becomes pure extraction (purchasing power destroyed, no exit). The classification depends on inflation trajectory — moderate power because these agents can partially hedge via asset allocation, but constrained exit because they cannot leave the nominal system.
constraint_indexing:constraint_classification(monetary_regime_transition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: EMERGING MARKET DEBTOR (SNARE) — Developing nations that borrowed in foreign fiat currencies (US dollars, euros) experience the regime as a snare: they cannot exit the debt obligation (captured in foreign currency), cannot issue replacement currency at will, and face extraction through exchange rate depreciation. When the issuer country raises interest rates, emerging market debt becomes more expensive to service. These actors bear maximum extraction with no exit option. The constraint is asymmetrically international: the issuer benefits from currency primacy, while non-issuer sovereigns suffer extraction.
constraint_indexing:constraint_classification(monetary_regime_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, the fiat currency regime appears to be a necessary institution of complex economies. Some might argue that any large-scale economy requires a shared store of value and medium of exchange, making the fiat regime a natural law of economic coordination. However, this perspective risks false summit classification: the specific institutional form (monopoly sovereign issuance, central banking, legal tender laws) is not inherent to money itself — alternative systems (commodity money, cryptocurrency, competing private currencies) have existed historically and exist today. The mountain classification here is a test of whether the framework's false summit detector correctly identifies naturalization of contingent institutional arrangements.
constraint_indexing:constraint_classification(monetary_regime_transition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_regime_transition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monetary_regime_transition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monetary_regime_transition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_regime_transition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(monetary_regime_transition, TR),
    TR >= 0.70.

:- end_tests(monetary_regime_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximal. The fiat regime extracts value from currency users through inflation, seigniorage, and monetary policy surprises, but also provides genuine coordination benefits (stable medium of exchange, price stability when well-managed, ability to conduct large-scale commerce). The 0.58 value reflects that extractiveness varies with inflation rate and distributional policy — it is 0.35 under low-inflation, well-managed regimes (e.g., Switzerland 1980-2000) and approaches 0.75 under high-inflation regimes (e.g., Argentina 2018-2023). Suppression (0.65): Moderate-high. Legal tender laws, tax systems denominated in fiat, capital controls, and the practical impossibility of conducting daily life outside the regime create significant suppression. However, suppression is not absolute: cryptocurrency and dollar-ization provide partial exits for those with sufficient wealth or international access. Ordinary users face near-total suppression; wealthy actors have constrained exit. Theater ratio (0.68): High. Monetary policy relies increasingly on narrative (forward guidance, inflation expectations management) rather than mechanical control. Central bank communications, quantitative easing, and unconventional policy tools emphasize appearance of control more than actual constraint on inflation. The theater has increased over the interval as policy became more accommodative.
 *
 * PERSPECTIVAL GAP:
 *   The nine perspectives reveal the fiat regime as a radically perspective-dependent constraint. The beneficiary (sovereign issuer, central bank) genuinely experiences rope: the coordination benefits are real, and their extracted position is net-positive. The trapped currency user genuinely experiences snare: they cannot exit and bear the full inflation tax. The fixed-income earner's tangled_rope is not a middle ground between snare and rope — it is a distinct structural position where both the coordination benefit and the extraction are real and compete. The emerging market debtor's snare is structurally different from the currency user's snare because the foreign currency denominator removes the possibility of monetary policy relief. The cryptocurrency coalition's scaffold is structurally different from all other perspectives because they have the rarest resource: an exit path that is becoming viable over a generational horizon. The piton perspective (Bretton Woods institutions) is the only one that sees degradation — these institutions maintain theater (holding summits, issuing reports) but lack functional control in the modern floating regime. The analytical observer's mountain is a false summit test: the framework should reject the claim that fiat money is inherent to economics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in the fiat regime is primarily determined by four factors: (1) structural beneficiary/victim status, (2) exit options, (3) time horizon, and (4) inflation rate. At low inflation and stable expectations, the regime is experienced as more rope by most agents because the coordination benefits dominate. At high inflation, it shifts toward snare because the extraction becomes visible and the coordination benefit deteriorates. The sovereign issuer's d-value is around 0.05 (canonical institutional beneficiary) — they have arbitrage exit (can switch regimes, issue alternative currencies) but retain de facto control. The currency user's d-value is around 0.95 (canonical powerless victim with trapped exit) — they cannot exit and bear extraction. The emerging market debtor's d-value is around 0.92 despite having institutional power in their home country, because they are foreign-currency-trapped: the foreign currency denominator makes their structural position equivalent to a powerless trapped agent. The fixed-income earner's d-value is around 0.70 (moderate victim with constrained exit) — they can partially hedge via asset allocation but cannot fully exit the nominal system. The cryptocurrency coalition's d-value is around 0.50-0.55 (organized victim/beneficiary in transition) — they experience extraction (excluded from seigniorage, face regulatory barriers) but also benefit from alternative coordination mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   The fiat regime presents a mandatrophy ambiguity that resolves by recognizing that BOTH the snare interpretation (extraction-focused) AND the rope interpretation (coordination-focused) are structurally correct from their respective perspectives. The mandatrophy resolution is NOT 'which is true?' but 'under what conditions does each hold?' At low inflation with stable expectations and efficient monetary policy: the regime functions as rope for most agents (genuine coordination, manageable extraction). At high inflation with unstable expectations and accommodative policy: the regime functions as snare for fixed-income agents (extraction dominates coordination). The Bretton Woods piton perspective reveals the institutional inertia mechanism: the regime persists partly because alternatives have not yet achieved critical adoption, and partly because the sovereign issuer has an interest in maintaining the monopoly. The cryptocurrency scaffold reveals the generational transition: as digital alternatives mature, the fiat regime's extraction mechanism (monopoly on money creation) becomes optional rather than imposed. The false summit detection on the mountain perspective is critical: if the analyst claims fiat money is a law of nature (like gravity or logic), the framework should reject that claim and flag it as naturalization of a contingent institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_sustainability_threshold,
    'At what sustained inflation rate does the fiat regime transition from tangled_rope (mixed coordination-extraction) to pure snare (extraction without coordination benefit)?',
    'Longitudinal analysis of purchasing power loss vs. real economic growth; measurement of inflation expectations and asset price movements; tracking of currency substitution behavior (dollarization, cryptocurrency adoption) as inflation rises',
    'If threshold < 5% annual inflation: many historical episodes misclassified as rope when they should be snare. If threshold > 20% annual: regime appears stable longer than empirical exit behavior suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_sustainability_threshold, empirical, 'Inflation rate threshold distinguishing tangled_rope from pure snare').

omega_variable(
    sovereign_monetary_policy_autonomy,
    'Is monetary policy autonomy (the core beneficiary extraction mechanism) actually constrained by capital mobility and foreign exchange markets, making the sovereign issuer''s power illusory?',
    'Comparison of intended monetary policy targets vs. actual outcomes under different capital flow regimes; analysis of currency crises and forced policy reversals; measurement of central bank independence correlation with inflation outcomes',
    'If autonomy is illusory: sovereign issuer should be reclassified as constrained, and the regime''s extraction mechanism is weaker than modeled. If autonomy is real: beneficiary power is confirmed, and the tangled_rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereign_monetary_policy_autonomy, empirical, 'Whether monetary policy autonomy is genuine or constrained by capital markets').

omega_variable(
    cryptocurrency_regime_replacement_timeline,
    'Will decentralized currency systems (Bitcoin, Ethereum, central bank digital currencies) achieve sufficient adoption to functionally replace the traditional fiat monopoly within 30 years?',
    'Adoption curve analysis of digital currencies; measurement of transaction volume and store-of-value share over time; policy responses by sovereign issuers (acceptance vs. prohibition); identification of tipping points for alternative system stability',
    'If replacement occurs: the scaffold perspective is confirmed, and the fiat regime is structurally temporary. If replacement fails: scaffold is aspirational, and fiat regime exhibits no sunset even from a generational horizon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cryptocurrency_regime_replacement_timeline, empirical, 'Probability and timeline of functional replacement of fiat regime by decentralized alternatives').

omega_variable(
    extraction_distribution_asymmetry,
    'Is the extraction from fixed-income earners and savers (via inflation) actually proportional to the coordination benefits they receive, or is it systematically skewed toward asset holders and debtors?',
    'Wealth distribution analysis over inflationary periods; tracking of real asset returns vs. nominal fixed-income returns; measurement of inflation incidence by asset class and demographic group',
    'If symmetric: the regime is more rope than snare from the saver perspective. If asymmetric: extraction is a deliberate policy choice, confirming tangled_rope or snare classification for savers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_distribution_asymmetry, empirical, 'Distributional asymmetry of inflation extraction between asset holders and savers').

omega_variable(
    legal_tender_enforcement_necessity,
    'Is legal tender law (the primary suppression mechanism preventing exit) structurally necessary for the fiat regime to function, or is it ceremonial enforcement of an already-entrenched system?',
    'Historical comparison of regimes with/without legal tender enforcement; analysis of alternative currency adoption rates under different legal frameworks; measurement of tax compliance and currency acceptance in jurisdictions with weak legal tender enforcement',
    'If necessary: suppression is structural and justified. If ceremonial: suppression is pure coercion, and the regime is more snare than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_tender_enforcement_necessity, conceptual, 'Whether legal tender enforcement is structurally necessary or ceremonial').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_regime_transition, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monet_tr_t0, monetary_regime_transition, theater_ratio, 0, 0.55).
narrative_ontology:measurement(monet_tr_t25, monetary_regime_transition, theater_ratio, 25, 0.62).
narrative_ontology:measurement(monet_tr_t50, monetary_regime_transition, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(monet_be_t0, monetary_regime_transition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(monet_be_t25, monetary_regime_transition, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(monet_be_t50, monetary_regime_transition, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_regime_transition, resource_allocation).
narrative_ontology:affects_constraint(monetary_regime_transition, central_bank_independence).
narrative_ontology:affects_constraint(monetary_regime_transition, inflation_targeting_regime).
narrative_ontology:affects_constraint(monetary_regime_transition, currency_hierarchy).
narrative_ontology:affects_constraint(monetary_regime_transition, emerging_market_debt_trap).

% DUAL FORMULATION NOTE:
% The fiat regime is a parent constraint that affects multiple downstream constraints. Central bank independence determines how much monetary autonomy the sovereign has. Inflation targeting represents an attempt to constrain the regime's extractiveness through rules-based policy. Currency hierarchy (dollar dominance, euro primacy) distributes the regime's extraction asymmetrically across nations. Emerging market debt traps are a specific downstream consequence of the regime's foreign currency dimension. Each of these should be modeled as separate constraint stories with their own extractiveness and perspectives, linked back to this regime story via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_regime_transition, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
