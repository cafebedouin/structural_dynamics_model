% ============================================================================
% CONSTRAINT STORY: central_bank_balance_sheet_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_central_bank_balance_sheet_expansion, []).

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
 *   constraint_id: central_bank_balance_sheet_expansion
 *   human_readable: Central Bank Balance Sheet Expansion as Monetary Policy Mechanism
 *   domain: monetary_policy/financial_systems
 *
 * SUMMARY:
 *   Central bank balance sheet expansion — the accumulation of government
 *   debt and other assets on the central bank's balance sheet through open
 *   market operations and quantitative easing — emerged as a major policy
 *   tool following the 2008 financial crisis and has persisted through
 *   subsequent crises (2020 COVID, 2022 debt-ceiling uncertainties). The
 *   mechanism operates as a hybrid constraint: it coordinates credit
 *   provision and financial stability (enabling banks to lend, stabilizing
 *   asset prices, preventing cascade defaults) while simultaneously
 *   extracting value from savers, wage earners, and price-stability
 *   commitments through inflation taxation and negative real interest rates.
 *   This constraint exemplifies the Tangled Rope class — neither pure
 *   coordination nor pure extraction, but both simultaneously, with
 *   asymmetric distribution of benefits and costs. The structural ambiguity
 *   about whether expansion is temporary crisis accommodation or permanent
 *   feature of modern monetary systems generates significant omega
 *   uncertainty. The constraint's theater ratio (0.61) reflects that central
 *   banks present expansions as 'data-dependent' and 'temporary' while the
 *   political economy of fiscal accommodation and financial system stress
 *   relief often make reversal politically difficult or economically
 *   destabilizing.
 *
 * KEY AGENTS:
 *   - Fixed-income savers and retirees: Primary victims (powerless/trapped) — bear inflation tax with no coordination benefit; real purchasing power erodes as negative real rates persist
 *   - Wage earners with sticky nominal wages: Secondary victims (powerless/trapped) — real wages decline during expansion; wage adjustment lags price adjustment; information asymmetry suppresses wage negotiation power
 *   - Asset holders and equity investors: Primary beneficiaries (institutional/arbitrage) — gain from portfolio rebalancing and capital appreciation; experience constraint as coordination mechanism enabling investment opportunities
 *   - Large financial institutions: Secondary beneficiary (moderate/constrained) — benefit from liquidity and reduced funding costs but constrained by margin compression and regulatory limits
 *   - Government fiscal authority: Mixed beneficiary (powerful/mobile) — benefits from reduced borrowing costs and fiscal space but constrained by inflation dynamics and electoral cycles
 *   - Central bank institution: Institutional actor (institutional/arbitrage) — operates the mechanism; experiences tension between inflation mandate and financial stability pressure; increasingly identity-fused with fiscal accommodation
 *   - Inflation-targeting framework advocates: Organized reformers (organized/constrained) — see expansion as temporary with sunset; working toward normalization and constraint resolution
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy discretion as immutable law; false summit detection identifies naturalization risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(central_bank_balance_sheet_expansion, 0.58).
domain_priors:suppression_score(central_bank_balance_sheet_expansion, 0.65).
domain_priors:theater_ratio(central_bank_balance_sheet_expansion, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(central_bank_balance_sheet_expansion, extractiveness, 0.58).
narrative_ontology:constraint_metric(central_bank_balance_sheet_expansion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(central_bank_balance_sheet_expansion, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(central_bank_balance_sheet_expansion, tangled_rope).
narrative_ontology:human_readable(central_bank_balance_sheet_expansion, "Central Bank Balance Sheet Expansion as Monetary Policy Mechanism").
narrative_ontology:topic_domain(central_bank_balance_sheet_expansion, "monetary_policy/financial_systems").

domain_priors:requires_active_enforcement(central_bank_balance_sheet_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(central_bank_balance_sheet_expansion, asset_holders).
narrative_ontology:constraint_beneficiary(central_bank_balance_sheet_expansion, large_financial_institutions).
narrative_ontology:constraint_beneficiary(central_bank_balance_sheet_expansion, government_fiscal_capacity).
narrative_ontology:constraint_victim(central_bank_balance_sheet_expansion, savers_on_fixed_income).
narrative_ontology:constraint_victim(central_bank_balance_sheet_expansion, wage_earners_with_delayed_adjustment).
narrative_ontology:constraint_victim(central_bank_balance_sheet_expansion, price_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIXED-INCOME SAVERS (SNARE) — Trapped in currency zone with no exit. Real purchasing power erodes as balance sheet expansion reduces real returns on savings. Cannot arbitrage away; cannot organize collective defense. Experience maximum extraction through inflation tax with no offsetting benefit. No coordination function serves them.
constraint_indexing:constraint_classification(central_bank_balance_sheet_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WAGE EARNERS WITH STICKY ADJUSTMENT (SNARE) — Real wages decline during expansion phase as prices rise faster than nominal wage adjustment. Trapped in labor contracts with delayed wage updates. Suppression operates through information asymmetry and power imbalance in wage negotiations. Extraction is direct and uncompensated.
constraint_indexing:constraint_classification(central_bank_balance_sheet_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ASSET HOLDERS (ROPE) — Benefit from balance sheet expansion through asset price appreciation (portfolio rebalancing effect). Experience the constraint as coordination mechanism: central bank liquidity provision enables credit flows and investment opportunities. Net beneficiary with arbitrage capacity — can shift between asset classes, currencies, or geographic markets. Extraction flows toward this agent.
constraint_indexing:constraint_classification(central_bank_balance_sheet_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMERCIAL BANKS (TANGLED ROPE) — Benefit from near-zero funding costs and loan demand (coordination function). But also face compressed net interest margins and regulatory constraints on leverage. Constrained by capital requirements and supervision. Mixed experience: enabled by central bank liquidity but squeezed by rates-based business model compression. Both coordination and extraction present.
constraint_indexing:constraint_classification(central_bank_balance_sheet_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GOVERNMENT FISCAL AUTHORITY (TANGLED ROPE) — Benefits from balance sheet expansion through reduced borrowing costs (coordination) and increased fiscal capacity to service debt. But also constrained by inflation dynamics and political pressure. Mobile in principle (can exit via fiscal discipline) but constrained by electoral cycles and path dependence. Extraction and coordination both present at different time horizons.
constraint_indexing:constraint_classification(central_bank_balance_sheet_expansion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: PRICE STABILITY MANDATE (PITON) — The formal inflation target and price stability objective persist as institutional rhetoric while balance sheet expansion operates in tension with these mandates. Theater ratio is high: central banks present expansions as 'temporary' and 'data-dependent' while the structural function is to accommodate fiscal needs and prevent financial stress. The mandate degrades over time as it becomes performative cover for political accommodation.
constraint_indexing:constraint_classification(central_bank_balance_sheet_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: INFLATION-TARGETING REFORM (SCAFFOLD) — Organized actors (some central bankers, inflation hawks, fiscal discipline advocates) see balance sheet expansion as a temporary crisis response with intended sunset. They maintain frameworks for eventual balance sheet normalization and quantitative tightening. Extraction is tolerated because the mechanism has stated time bounds and exit pathway. Constrained by political economy but working toward constraint resolution.
constraint_indexing:constraint_classification(central_bank_balance_sheet_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal, civilizational horizon, central bank balance sheet expansion might appear as an immutable constraint: the necessity for large-scale liquidity provision in complex financial systems, the irreducibility of inflation-employment tradeoffs, or the structural requirement for monetary accommodation during financial stress. This perspective risks naturalizing what are actually contingent institutional choices and political outcomes. The engine's false summit detector will identify this as naturalization of discretionary policy.
constraint_indexing:constraint_classification(central_bank_balance_sheet_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(central_bank_balance_sheet_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(central_bank_balance_sheet_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(central_bank_balance_sheet_expansion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(central_bank_balance_sheet_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(central_bank_balance_sheet_expansion, TR),
    TR >= 0.70.

:- end_tests(central_bank_balance_sheet_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts value from savers and wage earners through negative real interest rates and inflation taxation. But extraction is not as severe as pure predatory mechanisms (snares typically ε ≥ 0.72) because the central bank's coordination function is real — liquidity provision and financial stability are genuine public goods that prevent worse outcomes. The moderate-high value reflects that coordination benefits are genuine but asymmetrically distributed; victims bear real costs while beneficiaries gain disproportionately. Suppression (0.65): High. Savers and wage earners face substantial barriers to exit: they cannot switch currencies easily (transaction costs, foreign exchange risk, capital controls), cannot escape nominal wage stickiness (labor market power imbalance), and face information barriers to real asset hedging (complexity, transactions costs, knowledge requirements). Suppression has increased as central banks have deployed larger expansions with longer expected durations. Theater ratio (0.61): Moderate-high. Central banks present balance sheet expansion as 'data-dependent' and 'temporary emergency response' while the structural political economy often makes reversal difficult. Quantitative Tightening phases have been announced multiple times with limited follow-through (2018 failed taper, 2022 reversal). The theater has increased over the measurement interval as central banks' rhetoric has become increasingly disconnected from actual policy stance — the same institutions announcing inflation-fighting tightening have maintained accommodative stances or re-expanded when stress appeared.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits dramatic perspectival disagreement across agents. Asset holders see Rope — a coordination mechanism that benefits them and enables investment. Government sees Tangled Rope — both benefit (fiscal accommodation) and are constrained (inflation pressure). Banks see Tangled Rope — benefit from liquidity but constrained by margin compression. Wage earners see Snare — pure extraction through real wage decline with no offsetting coordination benefit. Savers see Snare — inflation taxation with no escape. The inflation-targeting framework sees its own degradation as a Piton — the formal mandate persists but through increasingly theatrical rhetoric. Reformers see Scaffold — temporary crisis response with eventual normalization pathway. The analytical observer risks false summit (Mountain) by naturalization. This perspectival range from pure coordination (rope) to pure extraction (snare) to institutional degradation (piton) to natural law (mountain) is precisely what triggers mandatrophy resolution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from each agent's structural position relative to the constraint. Fixed-income savers: powerless exit (trapped), victim status → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction. Wage earners with sticky wages: powerless exit (trapped), victim status → d ≈ 0.95. Asset holders: institutional power, arbitrage exit, beneficiary status → d ≈ 0.05 → f(d) ≈ -0.12 → negative experienced extraction (they benefit). Large financial institutions: moderate power, constrained exit, mixed beneficiary/victim → d ≈ 0.50 → f(d) ≈ 0.65 → moderate extraction. Government fiscal authority: powerful, mobile exit but politically constrained → d ≈ 0.50 → moderate extraction despite power. The scope modifier σ(S) amplifies extractiveness at global scope (σ = 1.2) because the constraint's effects are felt across all currency zones and asset classes participating in global capital markets. A powerless saver in any participating economy experiences extraction; there is no safe harbor within the system.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE MANDATROPHY RESOLUTION: The constraint is classified as Tangled Rope because it demonstrably possesses both genuine coordination (credit provision enabling legitimate investment, financial stability preventing cascade failures, fiscal accommodation preventing austerity spirals that would worsen recession) and asymmetric extraction (savers and wage earners bear inflation tax; benefits accrue to asset holders and government). The coordination function is not theater — credit does flow, stability is maintained, and austerity is averted. But the coordination is entangled with extraction such that beneficiaries capture disproportionate gains while victims absorb costs. The mandatrophy resolution prevents mislabeling: (1) Pure Rope mislabeling (denying extraction reality) would rationalize the constraint as 'just necessary monetary policy' and ignore distributional harm. (2) Pure Snare mislabeling (denying coordination) would ignore real financial stability benefits and would miss the mechanism's hybrid nature. The Tangled Rope classification accurately captures that this is both coordination and extraction, that both functions are real, and that the tension between them is structural not incidental. Mandatrophy_resolved: true because all six perspectives are now reconciled — they are all legitimate readings of a single hybrid constraint that exhibits all of them from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monetary_vs_fiscal_causality,
    'Does balance sheet expansion drive inflation directly, or primarily through enabling fiscal deficits and fiscal pressure on aggregate demand?',
    'Counterfactual analysis: balance sheet expansions in countries with constrained fiscal deficits vs. unconstrained deficits; vector autoregression studies isolating monetary and fiscal shocks; international comparison of expansion magnitude vs. inflation outcomes',
    'If primarily monetary: central banks bear direct responsibility for distributional extraction (inflation tax). If primarily fiscal accommodation: the constraint is a hybrid monetary-fiscal mechanism, and the central bank is partly a victim of fiscal pressure. Classification shifts from pure snare (powerless agents see direct extraction) to tangled rope (coordination with financial institutions enables fiscal needs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monetary_vs_fiscal_causality, empirical, 'Whether balance sheet expansion drives inflation directly or via fiscal accommodation').

omega_variable(
    alternative_coordination_pathways,
    'Could the coordination functions served by balance sheet expansion (credit provision, asset price stabilization, financial stress relief) be achieved through alternative mechanisms with lower extractive cost?',
    'Theoretical modeling of alternatives (lending facilities, regulatory forbearance, fiscal transfers); historical comparison with coordination mechanisms in non-inflation-targeting regimes; simulation of macroprudential tools with lower distributional impact',
    'If alternatives exist with substantially lower extractive cost: the classification shifts toward pure snare (extraction with minimal coordination justification). If alternatives are structurally inadequate: tangled rope classification confirmed (extraction is real cost of coordination benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_pathways, conceptual, 'Whether alternative coordination mechanisms could achieve same functions with lower extraction').

omega_variable(
    exit_capacity_distribution,
    'Do savers and wage earners have realistic exit options (currency switching, geographic relocation, real asset hedging) that would reduce their structural trappedness?',
    'Empirical analysis of currency substitution and asset class shift in high-inflation episodes; measurement of transaction costs and information barriers to real asset hedging; survey data on actual exit behavior among different income groups',
    'If exit options are genuinely unavailable: powerless agents truly trapped, snare classification confirmed. If exit options exist but are costly or informationally opaque: reclassify to constrained (high-cost exit), which raises d slightly and reduces experienced extraction chi. If exit options are actually accessible but unused due to identity lock (savers unable to abandon inflation-eroded savings, wage earners unable to imagine switching careers): classify as identity_locked.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_capacity_distribution, empirical, 'Whether trapped agents have realistic exit options for inflation protection').

omega_variable(
    temporality_of_extraction,
    'Is balance sheet expansion a temporary crisis response (extraction with sunset) or a permanent feature of monetary operations (extraction without terminal date)?',
    'Historical analysis of balance sheet normalization: frequency, duration, and completeness of prior cycles; institutional announcements and forward guidance on normalization plans; structural comparison with pre-2008 monetary regimes',
    'If temporary with credible sunset: scaffold classification appropriate (constrained agents see exit pathway). If permanent: snare and tangled rope classifications dominate, and the suppression metric should increase (trapped agents lose hope of reversal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporality_of_extraction, empirical, 'Whether balance sheet expansion has credible sunset or is permanent policy').

omega_variable(
    suppression_mechanism_internalization,
    'Do trapped agents internalize the extraction as necessary (identity lock) or perceive it as external imposition?',
    'Survey and interview data on inflation expectations formation and acceptance; analysis of political rhetoric around central bank independence vs. accountability; measurement of inflation surprise (actual inflation - expected inflation) as proxy for internalization failure',
    'If internalized: add omega variable for identity lock at household level; classify powerless agents as identity_locked rather than trapped on some trajectories. If perceived as external imposition: snare classification confirmed; high suppression is structural, not cognitive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether inflation extraction is internalized as necessity or perceived as imposed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(central_bank_balance_sheet_expansion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbbs_tr_t0, central_bank_balance_sheet_expansion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cbbs_tr_t3, central_bank_balance_sheet_expansion, theater_ratio, 3, 0.5).
narrative_ontology:measurement(cbbs_tr_t6, central_bank_balance_sheet_expansion, theater_ratio, 6, 0.61).
narrative_ontology:measurement(cbbs_tr_t9, central_bank_balance_sheet_expansion, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(cbbs_be_t0, central_bank_balance_sheet_expansion, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cbbs_be_t3, central_bank_balance_sheet_expansion, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(cbbs_be_t6, central_bank_balance_sheet_expansion, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(cbbs_be_t9, central_bank_balance_sheet_expansion, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(central_bank_balance_sheet_expansion, resource_allocation).
narrative_ontology:affects_constraint(central_bank_balance_sheet_expansion, fiscal_discipline_erosion).
narrative_ontology:affects_constraint(central_bank_balance_sheet_expansion, financial_asset_bubble_formation).
narrative_ontology:affects_constraint(central_bank_balance_sheet_expansion, savings_rate_compression).
narrative_ontology:affects_constraint(central_bank_balance_sheet_expansion, real_wage_growth_stagnation).

% DUAL FORMULATION NOTE:
% Balance sheet expansion is the monetary mechanism implementing fiscal accommodation and financial system backstopping. It is upstream of asset bubble formation (which depends on low rates from expansion) and savings rate compression (which depends on negative real returns from expansion). It is downstream of fiscal pressure (which drives central bank accommodation) and financial stress cycles (which trigger expansion). Constraint family members should be linked to show causal chains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(central_bank_balance_sheet_expansion, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
