% ============================================================================
% CONSTRAINT STORY: capital_rotation_ai_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capital_rotation_ai_narrative, []).

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
 *   constraint_id: capital_rotation_ai_narrative
 *   human_readable: Market Narrative: US AI Stock Unsustainability
 *   domain: economic/financial_markets
 *
 * SUMMARY:
 *   The dominant market narrative positing unsustainability of the US AI
 *   stock rally functions as a constraint on capital formation and
 *   allocation. This narrative operates through suppression (information
 *   asymmetry between institutional short-sellers and retail investors,
 *   difficulty in modeling AI economics), active enforcement
 *   (consensus-building by sell-side research, index rebalancing), and
 *   extraction (narrative arbitrageurs capture profits from sentiment
 *   reversals while real AI firms face elevated cost of capital). The
 *   constraint is not reducible to either pure coordination (Rope) or pure
 *   extraction (Snare) — it exhibits both simultaneously. Retail investors
 *   trapped in momentum strategies see it as Snare; institutional narrative
 *   traders see it as Rope; market makers extract while coordinating; real AI
 *   capital formation is suppressed; and from a long-term analytical view it
 *   appears as a natural law of markets. The theater ratio (0.68) reflects
 *   the performative content of consensus narratives in financial media and
 *   research departments — actual predictive power is low relative to the
 *   volume of narrative construction.
 *
 * KEY AGENTS:
 *   - Retail Investors: Primary victims (powerless/trapped) — concentrated in momentum funds, lack technical literacy on AI economics, cannot exit without severe losses during narrative reversals
 *   - Momentum Fund Managers: Secondary victims (moderate/constrained) — benefit from narrative while it sustains but face redemption pressure and career risk from narrative breaks
 *   - Narrative Arbitrageurs: Primary beneficiaries (institutional/arbitrage) — hedge funds, macro allocators, derivatives traders who profit from narrative spreads and volatility
 *   - Short Sellers: Secondary beneficiaries (institutional/arbitrage) — extract value from narrative confirmation, enjoy asymmetric payoff when sentiment reverses
 *   - Alternative Asset Managers: Tertiary beneficiaries (powerful/arbitrage) — can maintain positions across narrative cycles using derivatives and financing strategies
 *   - AI Sector Capital Formation: Primary victim (moderate/constrained) — real AI research companies and infrastructure firms face elevated cost of capital during negative narrative periods
 *   - Sell-Side Research & Market-Making: Enforcer (powerful/arbitrage) — extract rents from volatility while providing the liquidity and narrative consensus that sustains the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capital_rotation_ai_narrative, 0.58).
domain_priors:suppression_score(capital_rotation_ai_narrative, 0.65).
domain_priors:theater_ratio(capital_rotation_ai_narrative, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capital_rotation_ai_narrative, extractiveness, 0.58).
narrative_ontology:constraint_metric(capital_rotation_ai_narrative, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(capital_rotation_ai_narrative, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capital_rotation_ai_narrative, tangled_rope).
narrative_ontology:human_readable(capital_rotation_ai_narrative, "Market Narrative: US AI Stock Unsustainability").
narrative_ontology:topic_domain(capital_rotation_ai_narrative, "economic/financial_markets").

domain_priors:requires_active_enforcement(capital_rotation_ai_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capital_rotation_ai_narrative, narrative_arbitrageurs).
narrative_ontology:constraint_beneficiary(capital_rotation_ai_narrative, short_sellers).
narrative_ontology:constraint_beneficiary(capital_rotation_ai_narrative, alternative_asset_managers).
narrative_ontology:constraint_victim(capital_rotation_ai_narrative, retail_investors).
narrative_ontology:constraint_victim(capital_rotation_ai_narrative, momentum_fund_managers).
narrative_ontology:constraint_victim(capital_rotation_ai_narrative, ai_sector_capital_formation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Entry barriers are low but exit during narrative collapse is punishing. Retail investors caught in AI momentum funds face severe suppression: lack of technical literacy regarding AI economics, dependence on retail-friendly brokers with algorithmic order flow, and information asymmetry relative to institutional short-sellers. Cannot exit without realizing losses. The narrative itself is the constraint mechanism — it extracts through mark-to-market losses when the sentiment reverses.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MOMENTUM FUND MANAGER (TANGLED ROPE) — Benefits from the upward narrative while it sustains (performance fees, AUM growth), but constrained by redemption pressure and career risk if the narrative shifts unexpectedly. The constraint provides coordination (shared belief system enabling capital flow) while extracting through narrative-dependent valuation. Must maintain exposure to keep up with benchmark but trapped if narrative breaks before they can rotate.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NARRATIVE ARBITRAGEUR (ROPE) — Professional investors (hedge funds, macro allocators, derivatives traders) benefit from the narrative spread between bulls and bears. They capture coordination value by synthesizing and distributing narrative variants, while their arbitrage options allow them to switch sides of the trade as sentiment shifts. The constraint provides them pure coordination function: it creates a common epistemic frame that enables profitable positioning.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AI SECTOR CAPITAL FORMATION (SNARE) — The narrative operates as a constraint on real AI company financing. When narrative sentiment is negative, genuine AI research companies and infrastructure providers face higher cost of capital despite unchanged fundamentals. The suppression is severe: equity market access closes, debt becomes expensive, and acquisition multiples collapse. The extraction runs from real productive capacity toward narrative traders. Exit is impossible — the constraint is the market signal itself.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SELL-SIDE RESEARCH & MARKET MAKING (TANGLED ROPE) — Investment banks and market makers benefit from narrative volatility (wider bid-ask spreads, higher trading volume) while also providing the liquidity infrastructure that coordinates capital flow. This is a pure extraction-with-coordination pattern: they extract rents through information asymmetry while coordinat­ing the market's price discovery process. Active enforcement of their position occurs through analyst consensus formation and order flow routing.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / EFFICIENT MARKET VIEW (MOUNTAIN) — From a long-term analytical perspective, market narratives are immutable features of capital allocation: all equity prices reflect some combination of fundamentals and sentiment, and separating these is provably impossible (Grossman-Stiglitz paradox). The narrative is not a constraint imposed on markets but an intrinsic component of how markets function. This perspective sees the unsustainability narrative itself as a natural law of financial systems. However, the structural data (active enforcement, clear beneficiaries and victims, high suppression) reveals this as a false summit naturalizing a contingent social arrangement.
constraint_indexing:constraint_classification(capital_rotation_ai_narrative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capital_rotation_ai_narrative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capital_rotation_ai_narrative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capital_rotation_ai_narrative, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capital_rotation_ai_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(capital_rotation_ai_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The narrative creates measurable extraction flows: (1) mark-to-market losses for retail momentum investors and their managers during sentiment reversals; (2) realized gains for short-sellers and narrative arbitrageurs who position ahead of sentiment shifts; (3) elevated cost of capital for genuine AI firms due to narrative-induced volatility premium. The extraction is not total because some retail investors hold through cycles and benefit from narrative upswings. The initial extractiveness (0.38) reflects a period when the narrative was less dominant in consensus — it has increased to 0.58 as the unsustainability claim has become more prominent in institutional discourse. Suppression (0.65): High. Multiple suppression mechanisms: retail investors lack technical models of AI economics and depend on algorithmic order flow; information asymmetry is severe between institutional traders who construct and profit from narratives and retail who receive them post-facto; publication bias favors sensational unsustainability claims over nuanced analysis; regulatory frameworks have not yet fully integrated AI capability measurement, leaving narrative as the primary price-setting mechanism. The suppression is not complete because some retail investors can access advanced information sources and a small fraction actively manage away from narrative traps. Theater ratio (0.68): High and increasing. Consensus narratives about AI unsustainability are constructed largely through financial media, sell-side research consensus, and conference keynotes — outlets with strong incentives to generate attention-capturing narratives. The actual empirical content (real AI capability measurement, deployment rates, capex ROI) is sparse and contested. The theater has increased from 0.52 to 0.68 as the narrative has become more institutionalized in media and research consensus while underlying empirical measurement has lagged.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival divergence. The retail investor trapped in momentum funds sees pure extraction (Snare) — they enter during narrative bullishness, get trapped by narrative reversal, and suffer mark-to-market losses while exit is expensive. The momentum fund manager sees mixed coordination-extraction (Tangled Rope) — the narrative provides legitimate shared uncertainty about AI economics but also extracts through career risk and redemption pressure. The narrative arbitrageur sees pure coordination (Rope) — they profit from the shared belief system without being trapped by it, experiencing the constraint as enabling rational positioning. The real AI firm sees pure extraction (Snare) — the narrative directly raises their cost of capital regardless of fundamentals. The sell-side research apparatus sees profit opportunity (Tangled Rope) — they extract rents from volatility while claiming to coordinate price discovery. The long-term analytical observer risks seeing natural law (Mountain) — believing that narratives are immutable features of capital markets — but the structural data reveals this as a false summit: the narrative is actively constructed, enforced, and can be regulated differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) derives from their structural position relative to the narrative constraint. Retail investors occupy the position of full targets: they lack arbitrage options (trapped), possess limited power to independently verify AI claims (powerless), and face biographical time horizons matching the narrative's volatility cycle. They derive high d (toward 1.0) from trapped exit + victim status, producing high f(d) and thus high experienced extraction. Institutional narrative traders occupy the position of full beneficiaries: they have arbitrage options (can switch sides of trades), institutional power to construct and amplify narratives, and immediate time horizons matching their trading cycles. They derive low d (toward 0.0) from arbitrage exit + beneficiary status, producing low or negative f(d) and thus low or negative experienced extraction. Momentum managers occupy the middle: they benefit while the narrative sustains but are constrained by redemption pressure, deriving d ≈ 0.5 from mixed beneficiary/victim status and constrained exit. The sell-side research apparatus derives its directionality from institutional power + arbitrage (they can trade alongside their research) + enforcer role, placing them as high-power beneficiaries with d ≈ 0.2-0.3. Real AI firms derive their directionality as victims of narrative-driven capital constraints, independent of their actual fundamentals — high d despite potentially having some market exit options, because the constraint is the market signal itself.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE PARADOX RESOLUTION: The 'unsustainability narrative' initially appears to be a coordination mechanism (Rope) — multiple agents benefit from sharing a common model of AI market risk, enabling rational hedging and portfolio construction. However, the structural data reveals active enforcement, asymmetric extraction (beneficiaries are narrative traders; victims are retail investors and real AI firms), and high theater (performative consensus-building relative to empirical content). This forces the classification toward Tangled Rope: it IS coordination (shared epistemic frame) but ALSO extraction (narrative arbitrageurs profit from retail pessimism while retail bears mark-to-market losses). The mandate paradox is resolved by recognizing that the constraint performs a coordination function (providing a common language for discussing AI market risk) while the mechanism of coordination itself is weaponized for extraction (those who know the narrative is performative can position against the crowd). The false summit (mountain view from analytical perspective) is the belief that narratives are natural laws of market operation — the structural data shows they are contingent institutional arrangements (sell-side consensus, media incentives, regulatory gaps) that could be redesigned through transparency mandates, AI capability benchmarking, and decentralized price discovery mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrative_vs_fundamental_decomposition,
    'To what extent does the ''unsustainability'' claim reflect genuine fundamental uncertainty about AI economics versus narrative-driven sentiment reversal?',
    'Empirical decomposition: Compare AI company earnings surprises, capex multiples, and revenue growth rates during periods of high vs low narrative confidence. Isolate sentiment contribution via equity risk premium estimation and narrative sentiment indices.',
    'If fundamentals dominate: the constraint is primarily Rope (coordination around legitimate uncertainty). If sentiment dominates: the constraint is primarily Snare (narrative extraction from retail via suppression). Current data suggests 40/60 split, supporting Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_vs_fundamental_decomposition, empirical, 'Decomposition of narrative sentiment from fundamental AI economics').

omega_variable(
    short_squeeze_endogeneity,
    'Does the unsustainability narrative itself create reflexive short-squeeze dynamics that temporarily contradict the narrative''s truth value?',
    'Historical analysis of narrative-peak vs price-peak timing; correlation between short interest and narrative sentiment; identification of reflexive cycles where narrative prediction triggers crowd dynamics opposite to the narrative''s direction.',
    'If reflexive: the narrative is self-referential (oscillating between Rope and Snare depending on time horizon). If exogenous: the narrative is signal-like (sustainably predictive). Current evidence suggests strong reflexivity, supporting shorter-cycle oscillation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(short_squeeze_endogeneity, empirical, 'Degree of reflexive short-squeeze dynamics triggered by unsustainability narrative').

omega_variable(
    institutionalization_of_narrative,
    'Is the unsustainability narrative becoming institutionalized as a structural feature of capital allocation (e.g., ESG/risk frameworks incorporating AI narrative risk), or is it remain­ing a temporary sentiment phenomenon?',
    'Tracking of narrative encoding in regulatory frameworks, institutional policy documents, and index construction methodologies. Measurement of persistence of narrative-based exclusions across market cycles.',
    'If institutionalized: the constraint transitions toward Scaffold (sunset clause dependent on regulatory adoption) or Tangled Rope (permanent extraction structure). If sentiment-only: the constraint remains high-theater Piton (performative narrative with decaying function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutionalization_of_narrative, empirical, 'Degree of institutionalization of AI narrative risk in capital allocation frameworks').

omega_variable(
    real_ai_capability_frontier,
    'What is the true capability frontier and deployment rate of AI systems, and how does it compare to the narrative''s implicit assumptions?',
    'Empirical benchmarking of AI system performance (inference speed, accuracy, energy efficiency, task generality). Measurement of actual adoption rates in corporate capex and deployment. Comparison to narrative-implied capability curves.',
    'If capability frontier matches narrative pessimism: the constraint is primarily Rope (shared accurate model of uncertainty). If capability frontier exceeds narrative assumptions: the constraint is primarily Snare (narrative extraction from genuine retail pessimism). If capability frontier lags narrative optimism: constraint is primarily Tangled Rope (mixed true uncertainty and extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(real_ai_capability_frontier, empirical, 'True AI capability frontier versus narrative-implied frontier').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capital_rotation_ai_narrative, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cairn_tr_t0, capital_rotation_ai_narrative, theater_ratio, 0, 0.52).
narrative_ontology:measurement(cairn_tr_t6, capital_rotation_ai_narrative, theater_ratio, 6, 0.68).
narrative_ontology:measurement(cairn_tr_t12, capital_rotation_ai_narrative, theater_ratio, 12, 0.68).

% Extraction over time
narrative_ontology:measurement(cairn_be_t0, capital_rotation_ai_narrative, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cairn_be_t6, capital_rotation_ai_narrative, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(cairn_be_t12, capital_rotation_ai_narrative, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capital_rotation_ai_narrative, information_standard).
narrative_ontology:affects_constraint(capital_rotation_ai_narrative, ai_capex_productivity_puzzle).
narrative_ontology:affects_constraint(capital_rotation_ai_narrative, semiconductor_supply_concentration).
narrative_ontology:affects_constraint(capital_rotation_ai_narrative, energy_constraint_large_ai_models).

% DUAL FORMULATION NOTE:
% The unsustainability narrative is downstream of actual AI capability and economics constraints (capex productivity, semiconductor supply, energy limitations) but represents a distinct structural constraint at the capital formation level. The narrative's extractiveness value (0.58) is independent of whether the underlying claims are empirically true — it reflects the information asymmetry and suppression mechanisms in the market's narrative construction process, not the validity of the narrative's content.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(capital_rotation_ai_narrative, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
