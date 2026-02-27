% ============================================================================
% CONSTRAINT STORY: capital_misallocation_spiral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capital_misallocation_spiral, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: capital_misallocation_spiral
 *   human_readable: The Zombie Asset Loop
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The zombie asset loop is a structural constraint that emerges from the
 *   intersection of monetary policy (sustained low rates or quantitative
 *   easing) and institutional capital mandates (pension funds, insurance
 *   companies, and asset managers required to find yield). When policy rates
 *   fall below market-clearing levels, capital seeking returns must flow into
 *   increasingly unproductive assets — firms that cannot generate sufficient
 *   profit to justify their capital costs under normal market conditions but
 *   can survive indefinitely under monetary accommodation. The constraint
 *   exhibits a perspectival gap spanning all six DR types: asset managers see
 *   a coordination solution (rope), young entrepreneurs see entrapment
 *   (snare), central banks see a stability mandate that has become a trap
 *   (tangled rope), incumbent zombie firms see salvation (rope), real economy
 *   productivity sees starvation (snare), market price discovery sees
 *   degradation (piton), and alternative finance mechanisms see a temporary
 *   problem with a sunset (scaffold). The constraint's theater ratio has
 *   risen from 0.42 to 0.68 over the interval, reflecting increasing
 *   performative activity: quarterly earnings management, regulatory
 *   arbitrage, and financial engineering masking the underlying
 *   unproductivity. Base extractiveness has risen from 0.32 to 0.58 as zombie
 *   populations have expanded and capital misallocation has widened.
 *
 * KEY AGENTS:
 *   - Young Entrepreneurs: Primary victim (powerless/trapped) — unable to access productive capital; financing channels redirected to zombie yields
 *   - Real Economy Productivity: Primary victim (powerless/trapped) — R&D, manufacturing, infrastructure starved; aggregate productivity growth suppressed
 *   - Asset Managers and Fund Operators: Primary beneficiary (institutional/arbitrage) — stable yields, fee generation, spread capture from zombie asset arbitrage
 *   - Incumbent Zombie Firms: Mixed (organized/constrained) — benefit from capital availability but dependent on policy continuation
 *   - Central Banks: Mixed (institutional/constrained) — maintain financial stability but trapped in low-rate regime to prevent zombie defaults
 *   - Retail Savers: Victim (powerless/trapped) — real returns suppressed; pension and insurance funds decline in real value despite notional growth
 *   - Market Price Discovery: Piton (institutional/arbitrage) — mechanism persists performatively but function degraded by artificial asset support
 *   - DeFi Coalition: Organized actors (organized/mobile) — creating alternative allocation pathways with sunset potential
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capital_misallocation_spiral, 0.58).
domain_priors:suppression_score(capital_misallocation_spiral, 0.62).
domain_priors:theater_ratio(capital_misallocation_spiral, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capital_misallocation_spiral, extractiveness, 0.58).
narrative_ontology:constraint_metric(capital_misallocation_spiral, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(capital_misallocation_spiral, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capital_misallocation_spiral, tangled_rope).
narrative_ontology:human_readable(capital_misallocation_spiral, "The Zombie Asset Loop").
narrative_ontology:topic_domain(capital_misallocation_spiral, "economic/technological").

domain_priors:requires_active_enforcement(capital_misallocation_spiral).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capital_misallocation_spiral, asset_managers).
narrative_ontology:constraint_beneficiary(capital_misallocation_spiral, zombie_firm_incumbents).
narrative_ontology:constraint_beneficiary(capital_misallocation_spiral, central_banks).
narrative_ontology:constraint_victim(capital_misallocation_spiral, productive_capital_allocation).
narrative_ontology:constraint_victim(capital_misallocation_spiral, young_entrepreneurs).
narrative_ontology:constraint_victim(capital_misallocation_spiral, retail_savers).
narrative_ontology:constraint_victim(capital_misallocation_spiral, real_economy_growth).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNG ENTREPRENEUR (SNARE) — Cannot access capital at productive rates. Venture lending dries up because institutional capital flows toward zombie assets with guaranteed returns via monetary accommodation. No exit: entrepreneurship requires patient capital, which has been redirected. Experiences maximum extraction — locked out of viable financing channels.
constraint_indexing:constraint_classification(capital_misallocation_spiral, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REAL ECONOMY PRODUCTIVITY (SNARE) — The structural constraint on capital allocation prevents genuine productive investment. Capital that would flow to R&D, manufacturing, infrastructure improvement gets captured in zombie asset loops. The economy-wide productive capacity is structurally subordinated to financial engineering. No exit mechanism; bears full extraction cost.
constraint_indexing:constraint_classification(capital_misallocation_spiral, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ASSET MANAGERS (ROPE) — Experience the constraint as pure coordination: low rates and mandates create predictable yields on zombie assets. They are solving the problem of finding yield in a low-rate environment. Net beneficiary through arbitrage: can move capital between asset classes to capture spreads. The constraint generates fees and stable returns.
constraint_indexing:constraint_classification(capital_misallocation_spiral, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INCUMBENT ZOMBIE FIRMS (TANGLED ROPE) — Benefit from continued capital availability at suppressed rates (coordination: they survive). But also subject to enforcement: regulators and central banks maintain the low-rate regime that keeps them alive. Active enforcement of the regime (negative real rates, quantitative easing) is required. Exit is constrained — if rates rise or stimulus ends, many become insolvent. Mixed: they coordinate to lobby for continued accommodation while being extracted from in terms of productivity pressure.
constraint_indexing:constraint_classification(capital_misallocation_spiral, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CENTRAL BANKS (TANGLED ROPE) — Benefit from coordination (their mandates to maintain financial stability are met; zombie firms do not collapse). But subject to extraction: they become trapped maintaining low rates to prevent systemic defaults. Active enforcement (keeping rates suppressed despite inflation) is required. Exit is constrained by institutional mandate — can they raise rates without triggering cascade of zombie firm failures? Mixed: coordination function (stability) and extraction cost (foregone normal monetary policy).
constraint_indexing:constraint_classification(capital_misallocation_spiral, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MARKET PRICE DISCOVERY (PITON) — Theoretically coordinates capital allocation. In reality, the constraint degrades this function: zombie assets are mispriced (kept alive artificially), true asset values are obscured, and normal price signals fail. The price discovery mechanism persists (markets still trade) but is largely performative — prices no longer reflect fundamental productivity. Theater ratio high because markets appear to function while fundamental mechanism is broken.
constraint_indexing:constraint_classification(capital_misallocation_spiral, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: DEFI COALITION (SCAFFOLD) — Organized agents (crypto platforms, lending protocols, alternative asset venues) are creating parallel capital allocation pathways with different mechanics: no central rate-setting, transparent protocols, algorithmic rather than discretionary pricing. These create a sunset path — as DeFi mechanisms mature and institutional adoption increases, dependence on traditional zombie asset loop decreases. Temporary coordination problem with a visible exit route.
constraint_indexing:constraint_classification(capital_misallocation_spiral, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational analytical perspective, the zombie asset loop appears as an immutable consequence of fiat monetary systems combined with information asymmetry: any system that suppresses rates below market-clearing levels necessarily redirects capital away from efficiency toward politically favored preservation. This perspective risks naturalizing what is actually a policy choice.
constraint_indexing:constraint_classification(capital_misallocation_spiral, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capital_misallocation_spiral_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capital_misallocation_spiral, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capital_misallocation_spiral, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capital_misallocation_spiral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(capital_misallocation_spiral, TR),
    TR >= 0.70.

:- end_tests(capital_misallocation_spiral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts capital from productive uses and redirects it toward financial engineering and unproductive asset preservation. The extraction is significant but not maximal (0.70+) because some zombie firms retain marginal productivity, some capital still flows to genuine ventures, and the extraction is not absolute — it is a tilt in allocation, not a complete blockade. Suppression (0.62): Moderate-high. Multiple barriers prevent exit: (a) monetary policy setting creates artificial yield constraints that drive capital toward zombies, (b) institutional mandates (pension funds must find yield) force participation, (c) regulatory capture by incumbent firms resists rate normalization, (d) information asymmetry obscures true asset quality. But suppression is not total — some capital escapes to venture, international markets, and alternative mechanisms. Theater ratio (0.68): High and rising. Asset valuations are performative; quarterly earnings management and financial engineering mask unproductivity; 'zombie' classification itself is only visible to sophisticated analysts. The constraint maintains itself partly through obscurity — market participants tell themselves the assets are undervalued, not dying.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits all six types from distinct observational positions. Asset managers' Rope perspective is genuine — they are solving a real problem (how to achieve returns in a low-rate environment). Young entrepreneurs' Snare perspective is also genuine — they are locked out of capital markets. Central banks' Tangled Rope perspective captures their actual dilemma: they maintain stability (coordination) but are trapped in a policy equilibrium they cannot unwind. The zombie firms' experience is Rope (cheap capital) layered with Tangled Rope constraint (dependency on policy continuation). Real economy productivity's Snare perspective reveals the aggregate cost: capital flows away from productive investment. The Piton perspective on price discovery captures the degradation of the market mechanism itself — prices no longer signal scarcity or productivity because of artificial support. The Scaffold perspective from DeFi reveals that alternative allocation mechanisms are creating a sunset path: as blockchain-based lending and decentralized capital markets mature, dependence on traditional zombie asset loops declines. The analytical observer's Mountain perspective (naturalizing the zombie loop as an inevitable consequence of fiat money) is a false summit — the constraint is a policy equilibrium, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position: (1) Asset managers benefit from the low-rate regime that creates yield scarcity — their d is low (beneficiary + arbitrage), producing negative chi (they gain from extraction). (2) Young entrepreneurs face capital scarcity — their d is high (victim + trapped), producing high chi (they experience severe extraction). (3) Central banks maintain policy out of stability mandate, not profit — their d is mid-high (institutional + constrained), producing moderate chi (trapped in a policy they set but cannot easily unwind). (4) Zombie firms benefit from cheap capital but depend on it — their d is mid (organized + constrained), producing moderate chi (mixed: benefit from coordination, cost from dependency). (5) Real economy productivity is structural victim — its d is high (powerless + trapped), producing high chi (suffers complete extraction of capital). The derivation chain produces perspectival gaps: the beneficiary sees rope, the victim sees snare, the stabilizer sees tangled rope, the alternative mechanism sees scaffold.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED via perspectival decomposition. The mandatrophy question is: 'Is this a coordination mechanism (rope) that solves the yield-scarcity problem, or an extraction mechanism (snare) that starves productive investment?' The resolution: it is genuinely both. From the asset manager's perspective, it is coordination (Rope) — they are solving a real collective action problem (how to deploy capital when rates are suppressed). From the young entrepreneur's perspective, it is extraction (Snare) — they are locked out of capital markets. From the central bank's perspective, it is a hybrid trap (Tangled Rope) — they provide coordination function (maintain stability) but experience extraction cost (trapped in low-rate regime). The constraint does not collapse to a single type because the mandatrophy is real: the same institutional arrangement solves one coordination problem (yield scarcity for asset allocators) while creating an extraction problem (capital misallocation away from productivity). Resolution: classify as Tangled Rope at the system level (coordination + asymmetric extraction) while recognizing that beneficiary and victim perspectives experience it as pure types (Rope and Snare respectively). The false natural law risk: treating the zombie asset loop as inevitable consequence of monetary systems, when it is actually a policy equilibrium maintained by low rates, quantitative easing, and regulatory forbearance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zombie_firm_threshold,
    'What operational or financial threshold distinguishes a legitimately undervalued firm from a true ''zombie'' that survives only through monetary accommodation?',
    'Longitudinal analysis of firms with negative equity or sub-cost-of-capital returns; correlation between survival and central bank accommodation cycles; counterfactual: which firms would be insolvent at natural market rates?',
    'If threshold is strict (clearly negative-EBITDA): zombie population is small, misallocation is moderate (Rope dominates). If threshold is loose (any firm below cost-of-capital): zombie population is large, misallocation is severe (Snare dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zombie_firm_threshold, empirical, 'Threshold distinguishing legitimate undervaluation from zombie survival').

omega_variable(
    rate_sensitivity_collapse,
    'If central banks normalize rates to historical averages, what fraction of currently-financed zombie assets become insolvent?',
    'Stress testing across asset portfolios; analysis of duration exposure and negative-carry thresholds; survey data on break-even refinancing rates for zombie firms',
    'If < 10%: system can normalize smoothly (Scaffold perspective correct). If > 30%: normalization triggers cascade, central banks trapped (Tangled Rope/Snare for systemic actors).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rate_sensitivity_collapse, empirical, 'Fraction of zombie assets that become insolvent at normalized rates').

omega_variable(
    alternative_allocation_mechanisms,
    'Do decentralized finance or non-traditional capital allocation mechanisms (venture capital, peer lending, equity crowdfunding) achieve comparable or better capital efficiency than traditional banking for productive projects?',
    'Comparative ROI analysis; failure rates; time-to-productivity for capital deployed through DeFi vs traditional banks; ecosystem maturity analysis',
    'If DeFi/alternative mechanisms are more efficient: scaffold sunset is real, path to constraint relaxation exists. If traditional mechanisms remain superior: scaffold is aspirational, zombies persist due to structural, not merely policy, factors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_allocation_mechanisms, empirical, 'Whether alternative mechanisms achieve comparable capital efficiency').

omega_variable(
    monetary_accommodation_necessity,
    'Is continued monetary accommodation necessary to prevent systemic financial collapse, or is it primarily maintaining zombie firms whose collapse would be manageable?',
    'Scenario analysis of controlled zombie firm liquidation; analysis of systemic interconnection; study of historical precedents (Sweden 1990s, Japan 1990s-2010s); counterfactual: what happens if we stop?',
    'If necessary for systemic stability: Central Banks are genuine victims (Tangled Rope). If maintaining zombies is policy choice: Central Banks are complicit beneficiaries (Rope or Snare depending on frame).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monetary_accommodation_necessity, conceptual, 'Whether monetary accommodation is necessary for systemic stability or maintaining zombies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capital_misallocation_spiral, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zomb_tr_t0, capital_misallocation_spiral, theater_ratio, 0, 0.42).
narrative_ontology:measurement(zomb_tr_t5, capital_misallocation_spiral, theater_ratio, 5, 0.55).
narrative_ontology:measurement(zomb_tr_t10, capital_misallocation_spiral, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(zomb_be_t0, capital_misallocation_spiral, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(zomb_be_t5, capital_misallocation_spiral, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(zomb_be_t10, capital_misallocation_spiral, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capital_misallocation_spiral, resource_allocation).
narrative_ontology:boltzmann_floor_override(capital_misallocation_spiral, 0.35).
narrative_ontology:affects_constraint(capital_misallocation_spiral, financial_repression).
narrative_ontology:affects_constraint(capital_misallocation_spiral, regulatory_forbearance_spiral).
narrative_ontology:affects_constraint(capital_misallocation_spiral, venture_capital_scarcity).
narrative_ontology:affects_constraint(capital_misallocation_spiral, real_interest_rate_suppression).

% DUAL FORMULATION NOTE:
% The zombie asset loop decomposes into distinct structural constraints: (1) Real interest rate suppression (ε=0.25, Mountain from policy perspective) — central banks set rates below market-clearing levels via policy choice, (2) Zombie asset loop (ε=0.58, Tangled Rope at system level) — the capital misallocation spiral that results from rate suppression combined with institutional mandates, (3) Productive investment starvation (ε=0.62, Snare from entrepreneur perspective) — the outcome experienced by actors cut off from capital. Each is a distinct constraint with different ε values and different resolution pathways. The zombie asset loop is downstream of rate suppression but represents a distinct structural failure in capital allocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(capital_misallocation_spiral, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
