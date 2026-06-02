% ============================================================================
% CONSTRAINT STORY: institutional_speculation_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_speculation_extraction, []).

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
 *   constraint_id: institutional_speculation_extraction
 *   human_readable: Institutional Speculation Extraction in Cryptocurrency Markets
 *   domain: political_economy/monetary_theory/technology_governance
 *
 * SUMMARY:
 *   Cryptocurrency operates as a contested institutional space where three
 *   incompatible success criteria collide: the sound-money reading (fixed
 *   supply, censorship resistance, alternative to fiat), the speculation
 *   reading (volatility-driven asset returns, institutional portfolio
 *   allocation), and the decentralization reading (peer-to-peer
 *   disintermediation, governance through protocol not institutions). This
 *   constraint story models the institutional-speculation-extraction
 *   mechanism that emerges from this collision—specifically, how
 *   institutional adoption and market structure create asymmetric extraction
 *   from retail investors while maintaining performative alignment with
 *   coordination goals (monetary function, decentralization). The measurement
 *   trajectory shows extractiveness rising from 0.28 to 0.58 over ten years
 *   as institutional participation increases, regulatory clarity lags, and
 *   market depth attracts retail FOMO entry. Theater ratio rises from 0.35 to
 *   0.68 as institutional legitimacy (spot ETFs, custody services, major bank
 *   offerings) creates performative regulatory compliance while extraction
 *   mechanisms accelerate. The constraint exhibits all six DR types from
 *   different structural positions: the retail investor experiences pure
 *   extraction (snare); institutional investors experience coordination
 *   benefits (rope); regulatory authorities experience mixed
 *   coordination-extraction (tangled rope); the decentralization coalition
 *   experiences extraction that contradicts their stated goals (tangled
 *   rope); the traditional banking system performs theatrical resistance
 *   while profiting (piton); and the analytical observer risks naturalizing
 *   contingent market dynamics as immutable laws of capital allocation
 *   (mountain).
 *
 * KEY AGENTS:
 *   - Institutional Investors: Primary beneficiary (institutional/arbitrage) — capture liquidity, portfolio diversification benefits, regulatory legitimacy, and exit at favorable spreads during institutional entry phase
 *   - Retail Investors: Primary victim (powerless/trapped) — enter during volatility spikes driven by institutional adoption narratives; experience losses from information asymmetry, exchange fee structures, and inability to exit without realizing losses
 *   - Exchange Operators: Beneficiary (institutional/arbitrage) — profit from transaction fees, spread capture, lending on deposited collateral, and market-making advantages; control narrative through platform design
 *   - Regulatory Authorities: Mixed (moderate/constrained) — maintain genuine coordination function (custody standards, AML prevention) but benefit from uncertainty (regulatory rents, jurisdictional competition); constrained by lobbying from institutional entrants and jurisdictional races
 *   - Decentralization Coalition: Victim (organized/constrained) — maintain genuine protocol coordination function (peer-to-peer settling, censorship resistance) but experience extraction through speculative market hijacking, founder wealth concentration, and regulatory pressure requiring institutional compromise
 *   - Traditional Banking System: Beneficiary (institutional/arbitrage) — perform theatrical resistance while offering custody and trading services; profit from volatility and retail losses; maintain sector boundary through anti-crypto narrative
 *   - Analytical Observer: Risk of false summit (analytical/analytical) — may naturalize contingent institutional arrangements (exchange incentives, retail information barriers, regulatory capture) as immutable market laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_speculation_extraction, 0.58).
domain_priors:suppression_score(institutional_speculation_extraction, 0.62).
domain_priors:theater_ratio(institutional_speculation_extraction, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_speculation_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_speculation_extraction, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(institutional_speculation_extraction, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_speculation_extraction, tangled_rope).
narrative_ontology:human_readable(institutional_speculation_extraction, "Institutional Speculation Extraction in Cryptocurrency Markets").
narrative_ontology:topic_domain(institutional_speculation_extraction, "political_economy/monetary_theory/technology_governance").

domain_priors:requires_active_enforcement(institutional_speculation_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_speculation_extraction, institutional_investors).
narrative_ontology:constraint_beneficiary(institutional_speculation_extraction, exchange_operators).
narrative_ontology:constraint_beneficiary(institutional_speculation_extraction, early_token_holders).
narrative_ontology:constraint_victim(institutional_speculation_extraction, retail_investors).
narrative_ontology:constraint_victim(institutional_speculation_extraction, monetary_stability).
narrative_ontology:constraint_victim(institutional_speculation_extraction, decentralization_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Powerless, trapped in asymmetric information environment. Entry friction is low (exchange accessibility) but exit is economically devastating after price collapse. Experiences maximum extraction: limited technical capacity to assess risk, access to only public narratives (hype and counterargument), no exit option without realizing losses. Suppression mechanism combines information asymmetry, volatility-driven panic, and sunk-cost psychology.
constraint_indexing:constraint_classification(institutional_speculation_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGULATORY AUTHORITY (TANGLED ROPE) — Moderate power, constrained exit. Genuine coordination function: crypto markets require infrastructure rules (custody standards, exchange conduct, money-laundering prevention). But asymmetric extraction exists: regulators benefit from financial sector compliance expertise; the constraint extracts deference from smaller jurisdictions to follow regulatory standard-setting. Institutional players use regulatory uncertainty as extraction mechanism — maintaining ambiguity about whether crypto is commodity, security, or currency lengthens the speculation window.
constraint_indexing:constraint_classification(institutional_speculation_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL INVESTOR (ROPE) — Institutional power, arbitrage exit options. Experiences the constraint primarily as coordination: access to market liquidity, portfolio diversification, and regulatory clarity as institutions enter enables the market function. Net beneficiary — extraction flows toward this agent. Can exit at algorithmic bid-ask spreads; benefits from early legitimization (index inclusion, spot ETF approval, regulatory clarity). Sees constraint as solving market-depth and legitimacy coordination.
constraint_indexing:constraint_classification(institutional_speculation_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZATION IDEOLOGY COALITION (TANGLED ROPE) — Organized agents (developers, node operators, privacy advocates) maintain genuine coordination function: peer-to-peer transaction settling and censorship resistance. But severe asymmetric extraction: speculative markets hijack the protocol's legitimacy, capturing narrative and developer energy. Token valuations create founder wealth concentration; institutional adoption requires regulatory capitulation. The coalition experiences suppression through market dynamics (volatility making the currency function impossible) and through ideological pressure (institutional adoption contradicts decentralization goals).
constraint_indexing:constraint_classification(institutional_speculation_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL BANKING SYSTEM (PITON) — Institutional power, arbitrage options. Maintains performative resistance to crypto (regulatory campaigns, anti-crypto rhetoric) while simultaneously offering custody, trading, and settlement services. The resistance ritual is theatrical — banks benefit from crypto volatility and institutional migration into spot ETFs. The anti-crypto narrative persists through institutional inertia, not functional necessity. Theater maintains the appearance of sector boundary while infrastructure integration proceeds.
constraint_indexing:constraint_classification(institutional_speculation_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, price volatility in speculative markets is an unchangeable feature of how capital allocation works under uncertainty. Viewing the constraint as natural law (immutable market dynamics, fundamental to how decentralized systems attract capital) naturalizes what is actually a contingent institutional arrangement (exchange operator incentive alignment, retail investor information barriers, regulatory capture preventing alternative token designs). The mountain classification is a false summit revealing naturalization of extraction.
constraint_indexing:constraint_classification(institutional_speculation_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_speculation_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_speculation_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_speculation_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_speculation_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_speculation_extraction, TR),
    TR >= 0.70.

:- end_tests(institutional_speculation_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from retail investors through information asymmetry, exchange fee structures, and volatility-driven panic selling. However, extractiveness is not maximal (≥0.66) because: (1) Institutional investors genuinely benefit from market liquidity and coordination (rope function exists), (2) Some retail investors profit or break even, and (3) The decentralization coalition maintains some real protocol functionality. The rising trajectory (0.28 → 0.58) reflects institutional entry increasing market legitimacy while simultaneously amplifying extraction mechanisms through volatility and information barriers. Suppression (0.62): High. Multiple suppression layers: (1) Technical barriers (wallet management, custody complexity) limit retail exit capacity, (2) Psychological barriers (sunk-cost, loss aversion, FOMO) prevent rational exit timing, (3) Institutional barriers (trading halts during volatility, exchange rate slippage during panic), and (4) Informational barriers (retail access to only public narratives versus institutional private signals). Theater ratio (0.68): High. Multiple performative elements: (1) Regulatory compliance theater (exchanges claiming AML/KYC compliance while processing high-velocity transactions), (2) Institutional legitimacy theater (spot ETF offerings as signal of maturity despite underlying volatility), (3) Decentralization narrative theater (protocol claims of disintermediation despite exchange intermediation dominating retail access), and (4) Banking sector resistance theater (anti-crypto rhetoric while profiting from crypto services). The rising trajectory (0.35 → 0.68) reflects that as market matures, performative legitimacy replaces functional verification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The retail investor (powerless/trapped) sees a snare—entered through accessible narrative and exit-blocking losses. The institutional investor (institutional/arbitrage) sees rope—coordination solving market depth and legitimacy. The regulatory authority (moderate/constrained) sees tangled rope—genuine coordination (custody standards) mixed with extractive uncertainty (regulatory capture). The decentralization coalition (organized/constrained) sees tangled rope but inverted—genuine protocol coordination captured by extraction (speculation hijacking the legitimacy). The banking system (institutional/arbitrage) sees and performs piton—theatrical resistance while profiting, maintaining the ritual through inertia. The analytical observer (analytical/analytical) risks mountain—naturalizing volatility as immutable market law rather than contingent institutional arrangement. The perspectival gaps are not measurement artifacts but structural consequences of different power levels, exit capacities, and beneficiary positions relative to the extraction flow. No agent sees the full constraint; the framework reveals what remains hidden within single positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from each agent's power level, exit options, and relationship to the extraction flow. The retail investor (powerless + trapped) has d ≈ 0.95 (full target), experiencing maximum f(d) ≈ 1.42 amplification. The institutional investor (institutional + arbitrage) has d ≈ 0.05 (near-full beneficiary), experiencing negative f(d) ≈ -0.12, with effective extraction flowing away from them. The regulatory authority (moderate + constrained) has d ≈ 0.60, straddling beneficiary and target positions, with f(d) ≈ 0.85. The decentralization coalition (organized + constrained) has d ≈ 0.75 (strong target), with f(d) ≈ 1.10, because they nominally benefit from protocol coordination but experientially bear extraction costs from market hijacking. The banking system (institutional + arbitrage) has d ≈ 0.08 (beneficiary), with f(d) ≈ -0.10. The analytical observer (analytical) has canonical d ≈ 0.73, f(d) ≈ 1.15, revealing the observer's own position is not neutral—the framework used to analyze the constraint is itself shaped by the market structure being analyzed (the efficiency hypothesis naturalizes extraction). These directionality values are not overridden but are derived from structural declarations and exit options. The gap between institutional and retail d values (0.05 vs 0.95) is the core asymmetry enabling extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by decomposing the 'cryptocurrency' label into three structurally distinct objects masquerading under one name. The sound-money reading (fixed supply, censorship resistance) has ε ≈ 0.15 and appears as rope or scaffold—genuine coordination without significant extraction. The speculation reading (volatility-driven returns, institutional profit) has ε ≈ 0.58 (current story) and appears as tangled rope to snare—genuine coordination (market depth) mixed with or dominated by extraction. The decentralization reading (peer-to-peer disintermediation, governance through protocol) has ε ≈ 0.72 and appears as snare—the speculative market structure makes decentralization functionally impossible (you cannot run a currency where price volatility exceeds 10% daily). These are not three readings of one kernel. They have incompatible success criteria. The sound-money reading succeeds by stable purchasing power. The speculation reading succeeds by volatile price discovery. The decentralization reading succeeds by removing intermediaries—which the speculation mechanism requires (exchanges, custodians, liquidation layers). The current constraint story (institutional_speculation_extraction) models the specification reading's domination over the other two, revealing extraction through that dominance. Mandatrophy is resolved by recognizing that the institutional constraint we experience is not 'cryptocurrency' in general but specifically the institutional-speculation-extraction mechanism that has captured the namespace.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_decomposition_vs_readings,
    'Is cryptocurrency one kernel with three readings (sound money, speculation, decentralization) or three distinct kernels sharing a label?',
    'Examine success criteria independence: if the three readings have incompatible goals (fixed-supply immutability vs speculative price discovery vs decentralization governance), and if success for one reading requires failure for another, the objects are distinct kernels, not readings of one kernel.',
    'If three kernels: each deserves its own constraint story with separate ε values and beneficiary/victim structures. If one kernel: the current story is correct but misses the kernel_codification (the ''Bitcoin whitepaper'' is the kernel, but it does not mandate any single reading). Current analysis treats as three kernels per structural decomposition principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_decomposition_vs_readings, conceptual, 'Whether cryptocurrency represents one kernel with three readings or three distinct kernels').

omega_variable(
    institutional_entry_credibility_transfer,
    'Does institutional adoption (spot ETFs, BlackRock, bank custody) transfer legitimacy to the speculation mechanism, or does speculation contaminate institutional participation?',
    'Timeline analysis: institutional entry timing relative to major volatility events; retail loss correlations with institutional inflow; post-ETF volatility signature changes relative to pre-ETF baseline.',
    'If institutional entry stabilizes: extractiveness decreases as market structure matures; current tangled_rope downshifts toward rope over generational horizon. If institutional entry amplifies volatility: extractiveness increases; current assessment understates extraction at institutional power level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_entry_credibility_transfer, empirical, 'Effect of institutional adoption on volatility and retail extraction').

omega_variable(
    decentralization_vs_speculation_incompatibility,
    'Can a system simultaneously optimize for currency stability (decentralization goal) and price volatility (speculation incentive)? Or are these structurally incompatible?',
    'Protocol analysis: examine whether mechanisms that increase price volatility (low transaction velocity, supply constraints, transaction fees as lottery) are necessary for the decentralized operation claimed by the protocol. Compare against stable-value token designs (algorithmic stablecoins, collateralized models) and their adoption rates.',
    'If incompatible: the protocol architecture itself constrains the specification reading (decentralization) to perpetually lose against the speculation reading. Current extractiveness (0.58) understates structural lock-in. If compatible: alternative token designs are feasible, and the current constraint reflects institutional choice rather than technical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_vs_speculation_incompatibility, empirical, 'Structural compatibility of currency stability and speculation incentives').

omega_variable(
    retail_information_barrier_mechanism,
    'What fraction of retail investor losses derive from exploitable information asymmetry (institutional speed advantage, access to private signals) versus rational risk-taking?',
    'Cohort analysis: track retail trader subpopulations by trading frequency, hold duration, and loss distribution. Compare against institutional trading patterns; identify whether retail losses exceed expected value of passive position over same period.',
    'If high asymmetry fraction: snare classification is conservative; suppression and extractiveness are understated. If low asymmetry: losses reflect high-variance risk participation rather than extraction; snare reclassifies toward constrained-option or even mobile-option (if losses are clearly understood risks). Current assessment assumes moderate asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_information_barrier_mechanism, empirical, 'Attribution of retail losses to information asymmetry vs risk participation').

omega_variable(
    regulatory_clarity_as_extraction_tool,
    'Does regulatory ambiguity (whether crypto is commodity, security, or currency) primarily prevent extraction (by limiting market depth and legitimacy) or enable extraction (by allowing different rules to apply in different jurisdictions)?',
    'Comparative analysis: jurisdictions with clear regulatory frameworks (El Salvador commodity/currency, Switzerland securities) versus ambiguous frameworks (US enforcement-based). Measure volatility, institutional participation, and retail loss concentration across frameworks.',
    'If ambiguity enables extraction: institutional actors benefit from regulatory uncertainty to extend the speculation window. Clearing regulatory status would downshift extractiveness. If ambiguity prevents extraction: institutional actors would accelerate adoption if clarity improved, and extractiveness would increase post-clarification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_clarity_as_extraction_tool, empirical, 'Whether regulatory ambiguity enables or prevents extraction').

omega_variable(
    false_summit_natural_law,
    'Is market volatility in speculative assets a genuine natural law (immutable feature of capital allocation under uncertainty) or a naturalized institutional arrangement (contingent outcome of exchange operator incentives, retail information barriers, and regulatory capture)?',
    'Historical comparison: examine volatility signatures in markets with different institutional structures (high-frequency trading ban, transaction taxes, retail access restrictions, algorithmic trading regulation). If volatility is reducible through institutional design changes, the mountain classification is false.',
    'If naturalized arrangement: current mountain perspective instantiates the oracle gap (Theorem 4). The analytical framework itself is captured by the efficiency hypothesis. Reclassifies from mountain to tangled_rope or snare depending on who controls institutional design. If genuine natural law: analytical mountain classification is correct; the constraint structure is immutable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, empirical, 'Whether market volatility is immutable natural law or naturalized institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_speculation_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_spec_tr_t0, institutional_speculation_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(inst_spec_tr_t3, institutional_speculation_extraction, theater_ratio, 3, 0.52).
narrative_ontology:measurement(inst_spec_tr_t6, institutional_speculation_extraction, theater_ratio, 6, 0.64).
narrative_ontology:measurement(inst_spec_tr_t10, institutional_speculation_extraction, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(inst_spec_be_t0, institutional_speculation_extraction, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(inst_spec_be_t3, institutional_speculation_extraction, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(inst_spec_be_t6, institutional_speculation_extraction, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(inst_spec_be_t10, institutional_speculation_extraction, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_speculation_extraction, resource_allocation).
narrative_ontology:affects_constraint(institutional_speculation_extraction, central_bank_digital_currency_adoption).
narrative_ontology:affects_constraint(institutional_speculation_extraction, stablecoin_regulatory_capture).
narrative_ontology:affects_constraint(institutional_speculation_extraction, defi_liquidation_contagion).

% DUAL FORMULATION NOTE:
% This constraint is one of three structurally related stories decomposing 'cryptocurrency' by success-criteria incompatibility. (1) crypto_sound_money_alternative (ε≈0.15, rope) — the fixed-supply monetary coordination mechanism, (2) institutional_speculation_extraction (ε≈0.58, tangled_rope/snare, current story) — the volatility-driven capital allocation mechanism, and (3) decentralization_governance_protocol (ε≈0.72, snare) — the peer-to-peer disintermediation mechanism. These stories share institutional actors but have incompatible success conditions. The current story models how institutional-speculation-extraction dominates and instrumentalizes the other two readings' legitimacy narratives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_speculation_extraction, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
