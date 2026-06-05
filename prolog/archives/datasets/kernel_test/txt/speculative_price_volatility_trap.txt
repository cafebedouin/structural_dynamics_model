% ============================================================================
% CONSTRAINT STORY: speculative_price_volatility_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speculative_price_volatility_trap, []).

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
 *   constraint_id: speculative_price_volatility_trap
 *   human_readable: Speculative Price Volatility Trap in Cryptocurrency Markets
 *   domain: monetary_theory/technology_governance/political_economy
 *
 * SUMMARY:
 *   Cryptocurrency represents a contested kernel with three candidate
 *   readings: (1) sound-money ideology — inflation hedge and store of value
 *   grounded in algorithmic scarcity and peer-to-peer settlement, (2)
 *   speculative-asset vehicle — price appreciation mechanism exploited by
 *   leverage-enabled traders, and (3) decentralization-ideology — censorship
 *   resistance and financial sovereignty through distributed consensus. The
 *   speculative price volatility trap emerges from the structural
 *   incompatibility of these readings when embedded in a single technological
 *   substrate. The constraint exhibits Tangled Rope structure at the systemic
 *   level: genuine coordination functions (peer-to-peer settlement,
 *   censorship resistance, price discovery) coexist with systematic
 *   asymmetric extraction (early adopters/exchange operators benefit; retail
 *   investors bear volatility costs; monetary stability ecosystem
 *   internalizes contagion risk). The theater ratio (0.68) reflects that
 *   decentralization rhetoric persists despite functional capture by
 *   speculative trading — institutional inertia maintains legitimacy claims
 *   the system no longer instantiates. The extractiveness value (0.58)
 *   increased from 0.35 over the 9-year interval, driven by growing leverage
 *   markets, retail participation, and exchange concentration.
 *   Stablecoin/layer-2 alternatives represent a genuine sunset mechanism, but
 *   maturity remains questionable — the scaffold reading is structurally
 *   present but not yet dominant in transaction settlement.
 *
 * KEY AGENTS:
 *   - Retail Investors: Primary victims (powerless/trapped) — enter for sound-money or decentralization beliefs but experience pure extraction through volatility and leverage-driven liquidations
 *   - Early Adopters: Primary beneficiaries (institutional/arbitrage) — captured extreme wealth concentration through early accumulation; benefit from ongoing volatility and leveraged inflows from new cohorts
 *   - Exchange Operators: Secondary beneficiaries (institutional/arbitrage) — capture fees from volatility-driven trading volume; can exit to other asset classes if crypto volatility declines
 *   - Leveraged Traders: Secondary beneficiaries (powerful/mobile) — exploit retail volatility for alpha; require ongoing volatility to function; can move capital to other derivatives markets
 *   - Use-Case Believers (Constrained Participants): Mixed position (moderate/constrained) — benefit from payment network effects and censorship resistance, bear cost of volatility for pricing/settlement
 *   - Stablecoin/L2 Coalition: Organized alternative (organized/constrained) — building abstraction layers that bypass base-layer volatility; sunset logic if maturity accelerates
 *   - Central Banks/Regulatory Bodies: Institutional constraint (institutional/constrained) — face spillover risk but cannot simply exit; must regulate contagion pathways
 *   - Monetary Stability Ecosystem: Systemic victim (analytical/analytical) — abstract collective bearing contagion risk through derivatives leverage, stablecoin backing, and exchange counterparty exposure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speculative_price_volatility_trap, 0.58).
domain_priors:suppression_score(speculative_price_volatility_trap, 0.62).
domain_priors:theater_ratio(speculative_price_volatility_trap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speculative_price_volatility_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(speculative_price_volatility_trap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speculative_price_volatility_trap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speculative_price_volatility_trap, tangled_rope).
narrative_ontology:human_readable(speculative_price_volatility_trap, "Speculative Price Volatility Trap in Cryptocurrency Markets").
narrative_ontology:topic_domain(speculative_price_volatility_trap, "monetary_theory/technology_governance/political_economy").

domain_priors:requires_active_enforcement(speculative_price_volatility_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speculative_price_volatility_trap, early_adopters).
narrative_ontology:constraint_beneficiary(speculative_price_volatility_trap, exchange_operators).
narrative_ontology:constraint_beneficiary(speculative_price_volatility_trap, leveraged_traders).
narrative_ontology:constraint_victim(speculative_price_volatility_trap, retail_investors).
narrative_ontology:constraint_victim(speculative_price_volatility_trap, use_case_believers).
narrative_ontology:constraint_victim(speculative_price_volatility_trap, monetary_stability_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Enters cryptocurrency believing in store-of-value or payment narrative but experiences pure extraction through volatility and liquidity asymmetry. Cannot exit without realizing losses; suppression is high (information asymmetry, flash crashes, exchange insolvencies). The trap is structural: retail participants lack the capital reserves to weather volatility that professional traders exploit. Maximum extraction, minimal coordination benefit.
constraint_indexing:constraint_classification(speculative_price_volatility_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: USE-CASE BELIEVER (TANGLED ROPE) — Genuine coordination function exists: peer-to-peer transactions, cross-border settlement, censorship resistance. But this function is embedded within and obscured by speculative overlay. The believer bears cost of volatility (cannot reliably price goods in cryptocurrency) and receives benefit of technological coordination. Exit is costly (abandons investment thesis, rebuilds infrastructure) but possible. Suppression is high but not total — organized communities build payment networks despite volatility.
constraint_indexing:constraint_classification(speculative_price_volatility_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXCHANGE OPERATOR (ROPE) — Experiences the constraint as pure coordination: volatility drives trading volume, transaction fees, and liquidity provision. Benefits from the technical problem (price discovery) without bearing extraction costs. Can arbitrage across geographies and time horizons. Net beneficiary with full exit capacity — can shift to other assets if crypto volatility declines.
constraint_indexing:constraint_classification(speculative_price_volatility_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STABLECOIN/LAYER-2 COALITION (SCAFFOLD) — Organized technical response: stablecoins (USDC, USDT) and layer-2 networks (Lightning, Polygon) provide transaction settlement without volatility exposure. Theater is moderate (these systems require trust in issuers/operators, unlike the stated decentralization ideal). Sunset logic applies: as stablecoins and payment-specific networks mature, the volatility trap becomes optional rather than structural. Exit path exists — abstraction layers bypass the base-layer volatility.
constraint_indexing:constraint_classification(speculative_price_volatility_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DECENTRALIZATION IDEOLOGY (PITON) — Original claim was that distributed consensus produces censorship-resistant value transfer. But the lived function has become speculative pricing and wealth concentration (early adopters accumulating, retail liquidated in bear markets). The decentralization narrative persists (theater ratio 0.68) as institutional inertia — conferences, marketing, governance theater — despite the base layer being financially captured by leverage-enabled traders. The ideology maintains the system's legitimacy through performative commitment to principles it no longer instantiates.
constraint_indexing:constraint_classification(speculative_price_volatility_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MONETARY STABILITY ECOSYSTEM (TANGLED ROPE) — Central banks and regulatory bodies face coordination problem: cryptocurrency volatility creates contagion risk (through leverage, derivatives, stablecoin exposure) without corresponding stability benefit. The ecosystem is trapped (constrained exit) because crypto's footprint in traditional finance grows with each adoption cycle, requiring active regulation to manage spillover. Genuine coordination function (price discovery, new settlement layer) exists but is obscured by extraction (systemic risk propagation). Institutional actors cannot simply exit; they must regulate or internalize spillover costs.
constraint_indexing:constraint_classification(speculative_price_volatility_trap, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL VOLATILITY VIEW (MOUNTAIN) — From a civilizational perspective, cryptocurrency volatility could appear as an immutable property: decentralized systems without central issuer face inherent coordination problems in establishing agreed-upon value. No authority can suppress volatility by fiat. This perspective risks false-summit naturalization — treating a contingent institutional outcome (leverage-enabled traders, retail participation, exchange concentration) as an inherent feature of decentralized currency. The engine will identify this as FSM candidate via beneficiary declarations.
constraint_indexing:constraint_classification(speculative_price_volatility_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speculative_price_volatility_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speculative_price_volatility_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speculative_price_volatility_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(speculative_price_volatility_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(speculative_price_volatility_trap, TR),
    TR >= 0.70.

:- end_tests(speculative_price_volatility_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not extreme. Base value reflects the core asymmetry: price volatility enables wealth transfer from retail to traders/operators through leverage mechanics and liquidation cascades. The value is moderate rather than maximum (snare-threshold 0.66) because genuine coordination functions exist (peer-to-peer settlement works, censorship resistance is real, price discovery happens). The coordination prevents pure extraction — retail participants do receive some value (transaction capability, store-of-value hedge in high-inflation countries), but asymmetrically. The temporal trajectory (0.35→0.62→0.58) shows rise through 2017-2021 as leverage expanded, then plateau as regulatory scrutiny increased margin requirements. Suppression (0.62): High. Barriers to exit include: (1) information asymmetry — retail cannot identify volatility sources or trader manipulation; (2) sunk-cost lock-in — accumulated losses create psychological/financial barriers; (3) exchange insolvency risk — capital trapped in opaque custodial systems; (4) economic dependency — for participants in high-inflation or capital-control jurisdictions, even poor alternatives beat local currency. Theater ratio (0.68): High but concentrated. The decentralization narrative and governance theater (DAOs, community votes, protocol governance) are performative — they don't prevent volatility or regulate leverage. Transaction narratives (payment, settlement) are functional but subordinate to price-speculation narratives in actual usage. The theater declined from 0.72 in 2021 (peak ideological claims) to 0.68 as repeated bear markets exposed the speculative dominance, but institutional inertia maintains the framing.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is exceptionally wide, revealing a kernel collision rather than a simple perspectival difference. Sound-money believers (use-case believers) see Tangled Rope — genuine coordination embedded in extraction. Speculators see pure Rope — technical problem (price discovery) with no extraction cost to them. Retail see Snare — extraction with minimal coordination benefit. Stablecoin builders see Scaffold with a real sunset — volatility is a solvable architectural problem. Institutions see Tangled Rope at institutional level — coordination function (settlement layer) plus institutional extraction cost (contagion risk). The central fallacy in single-reading analysis is that the system cannot simultaneously optimize for sound-money (requiring stable purchasing power and low volatility) and speculative-asset (requiring price appreciation and liquidity through leverage). The three readings require three different technical systems. The volatility trap emerges when a single system tries to satisfy all three, and leveraged speculation dominates through capital efficiency and marketing reach.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across agent types, revealing the asymmetry. Early adopters with arbitrage-exit capacity have d≈0.05 (full beneficiary, low chi). Retail investors trapped by sunk losses have d≈0.95 (full target, high chi). Exchange operators with institutional/arbitrage have d≈0.10 (beneficiary, low chi). Use-case believers with moderate power but constrained exit have d≈0.65 (mixed position, moderate chi). Regulatory bodies with institutional power but constrained exit have d≈0.70 (bearing spillover costs, moderate-high chi). The engine's directionality derivation from beneficiary/victim declarations plus exit options captures these differences: victims + trapped → high d; victims + constrained → moderate d; beneficiaries + arbitrage → low d. The sharp stratification by power level is the diagnostic signal: this constraint concentrates extraction on powerless agents and distributes benefits to institutional actors, which is the definition of asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the tangled_rope classification at the system level (genuine coordination + asymmetric extraction) coexists with snare classification at the retail level (no coordination perceived, pure extraction experienced). Both are correct from their respective perspectives — the trap is that retail enters believing in the sound-money or decentralization readings but experiences the speculative-asset reading. The constraint is not misclassified; it is a collision of incompatible readings embedded in one system. The false-summit mountain reading (volatility is inherent to decentralization) is revealed as naturalization by the beneficiary declarations — early adopters and traders benefit from volatility, so claims that volatility is 'natural' serve their interests. The mandatrophy resolves into: 'What would happen if we separated the three readings into three systems?' Answer: stablecoins for sound-money function, derivatives markets for speculative-asset function, protocol development for decentralization-ideology function. The original Bitcoin attempt to do all three simultaneously created the volatility trap. Maturation paths (L2 networks, stablecoins, governance mechanisms) are structural attempts to decompose the kernel into separate systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_sound_money_vs_speculation,
    'Are the sound-money reading and the speculative-asset reading two readings of a single shared kernel, or two distinct kernels that happen to share Bitcoin''s technological substrate?',
    'Textual/institutional analysis: do advocates of one reading acknowledge the other as a valid interpretation of the same system, or do they insist the other reading is a misuse/corruption? Do protocol changes (e.g., transaction throughput limits) reflect compromise between readings or rejection of one reading in favor of another?',
    'If single kernel with two readings: the volatility trap is a failure of committer-system interpretation (the system can support both readings but does so badly for retail under speculative dominance). If two distinct kernels: the volatility trap reflects kernel collision — the technical system cannot simultaneously instantiate sound-money coordination AND speculative asset dynamics without instability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_sound_money_vs_speculation, conceptual, 'Whether sound-money and speculative-asset are readings of one kernel or distinct kernels').

omega_variable(
    leverage_driven_volatility_source,
    'What proportion of observed volatility is endogenous to decentralized consensus (price-discovery lag) versus exogenous to leverage and derivatives markets (using cryptocurrency as collateral for leveraged positions)?',
    'Time-series decomposition: correlation between spot volatility and derivatives open interest; analysis of flash crash sequences (e.g., March 2020, June 2022) tracing causality from leverage unwinding to spot price movement; comparison of volatility in low-leverage periods (e.g., bear markets with reduced participation) versus high-leverage periods.',
    'If leverage-driven (>60%): volatility trap is architectural to current market structure, not to decentralized consensus itself. Institutional regulation of leverage can reduce suppression without changing base protocol. If consensus-driven (>40%): volatility is inherent to coordination failure; stablecoins/layer-2 represent genuine escape, not just financial engineering.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(leverage_driven_volatility_source, empirical, 'Proportion of volatility driven by leverage versus consensus mechanisms').

omega_variable(
    retail_exit_cost_temporal_dynamics,
    'What is the distribution of exit costs for retail investors across different entry cohorts (early adopters at $100, 2017 entrants at $5k, 2021 entrants at $30k, 2022+ entrants at $15k)? Do losses persist or recover?',
    'Longitudinal portfolio analysis by cohort; calculation of break-even prices for each cohort; measurement of hold duration vs. realized loss rates; comparison of buy-and-hold losses to active trading losses.',
    'If losses concentrate in recent cohorts: retail is a structural sink for speculative overflow from professional traders (snare confirmed). If losses are temporary/recoverable: classification shifts toward constrained (tangled rope). If early cohorts retain large unrealized gains: wealth concentration between cohorts is extreme, and beneficiary identification is sharp.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_exit_cost_temporal_dynamics, empirical, 'Exit cost distribution across retail investor cohorts').

omega_variable(
    stablecoin_maturity_and_sunset_timing,
    'Are stablecoins (USDC, USDT, USDC-E) and layer-2 networks approaching the maturity to actually absorb transaction settlement currently priced in base-layer volatility? Or is the scaffold reading aspirational rather than structural?',
    'Measurement of stablecoin adoption velocity for merchant payments (not just trading pairs); comparison of transaction cost and settlement time on L2 networks to base layer; analysis of whether merchant acceptance of stablecoins exceeds or lags merchant acceptance of base-layer cryptocurrency.',
    'If mature/approaching: scaffold perspective is structurally grounded — the volatility trap is genuinely temporary, sunset is real, institutional actors can rely on this escape path. If immature: scaffold is aspirational, retail is trapped longer, snare dominates the medium-term (10-year) horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stablecoin_maturity_and_sunset_timing, empirical, 'Stablecoin and L2 maturity for transaction settlement').

omega_variable(
    reading_dominance_by_participant_cohort,
    'Which reading (sound-money, speculative-asset, decentralization-ideology) dominates among different participant groups? Are retail investors predominantly sound-money believers while professionals trade the speculative-asset reading?',
    'Survey analysis of stated motivation by participant type (retail, professional, merchant, HODLer, trader); sentiment analysis of cryptocurrency discourse by participant group; cross-tabulation of participant classification (trader vs holder) with professed reading.',
    'If readings perfectly stratify by power/sophistication: the volatility trap is a coordination problem between incompatible readings (snare for believers, rope for speculators). If readings overlap: the trap is internal to individuals (identity_locked at personal psychological level), not just institutional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_dominance_by_participant_cohort, empirical, 'Distribution of readings across participant cohorts').

omega_variable(
    false_summit_naturalization_mechanism,
    'Is the mountain perspective (volatility as inherent to decentralization) a genuine natural law claim or a naturalization of contingent market structure to defend the status quo against regulatory scrutiny?',
    'Textual analysis of who makes the ''volatility is natural'' claim and under what conditions (e.g., during regulatory hearings, in advocacy literature, in academic framing); comparison to similar claims in other decentralized systems (DNS, open-source software) where volatility is not present; identification of whether the claim requires leverage-driven speculation to hold.',
    'If genuine natural law: mountain classification is correct; volatility is not reducible. If naturalization: false-summit detection fires, engine reclassifies to tangled_rope, beneficiary claims are revealed as structural rather than inherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization_mechanism, conceptual, 'Whether volatility naturalness is genuine law or constructed defense').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speculative_price_volatility_trap, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spvt_tr_t0, speculative_price_volatility_trap, theater_ratio, 0, 0.42).
narrative_ontology:measurement(spvt_tr_t3, speculative_price_volatility_trap, theater_ratio, 3, 0.55).
narrative_ontology:measurement(spvt_tr_t6, speculative_price_volatility_trap, theater_ratio, 6, 0.72).
narrative_ontology:measurement(spvt_tr_t9, speculative_price_volatility_trap, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(spvt_be_t0, speculative_price_volatility_trap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spvt_be_t3, speculative_price_volatility_trap, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(spvt_be_t6, speculative_price_volatility_trap, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(spvt_be_t9, speculative_price_volatility_trap, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speculative_price_volatility_trap, resource_allocation).
narrative_ontology:affects_constraint(speculative_price_volatility_trap, leverage_cascade_contagion).
narrative_ontology:affects_constraint(speculative_price_volatility_trap, exchange_operational_risk).
narrative_ontology:affects_constraint(speculative_price_volatility_trap, merchant_adoption_barrier).

% DUAL FORMULATION NOTE:
% The speculative price volatility trap should be decomposed into three constraint stories if full kernel analysis is required: (1) sound_money_coordination (ε≈0.30, genuine peer-to-peer settlement coordination), (2) speculative_asset_extraction (ε≈0.72, pure leverage-driven extraction), (3) decentralization_ideology_performance (ε≈0.68, governance theater maintaining legitimacy). The present story captures the system-level tangled rope that results from their collision. The three decomposed stories would each have different beneficiary/victim sets and would link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speculative_price_volatility_trap, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
