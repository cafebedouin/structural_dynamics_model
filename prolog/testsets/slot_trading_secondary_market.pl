% ============================================================================
% CONSTRAINT STORY: slot_trading_secondary_market
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_slot_trading_secondary_market, []).

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
 *   constraint_id: slot_trading_secondary_market
 *   human_readable: Slot Trading Secondary Market Coordination with Extraction
 *   domain: economic/market_structure
 *
 * SUMMARY:
 *   Secondary slot trading markets create a structurally hybrid constraint
 *   that solves genuine coordination problems while enabling asymmetric
 *   extraction from capacity-constrained operators. The secondary market
 *   enables operators with excess capacity to relinquish slots without
 *   permanent loss; operators needing additional slots can acquire them
 *   without waiting for new primary allocation rounds; prices provide signals
 *   for capacity optimization. However, this coordination mechanism exists
 *   within a fixed primary allocation framework that creates scarcity rents.
 *   Large operators with capital can acquire slots at any price and use them
 *   flexibly; small operators face binding constraints on capital deployment;
 *   new entrants face a two-tier entry cost (primary allocation + secondary
 *   premium). The constraint exhibits all six DR types from different
 *   perspectives, revealing how market mechanisms can simultaneously
 *   coordinate and extract. The theater ratio (0.35) reflects relatively low
 *   performative content — secondary markets function primarily through price
 *   signals and transactions rather than ritual or compliance theater. The
 *   extractiveness trajectory (0.28 → 0.52 over 10 periods) indicates rising
 *   rent extraction as market maturity increases and operators accumulate
 *   slot holdings.
 *
 * KEY AGENTS:
 *   - New Market Entrants: Primary victims (powerless/trapped) — face binding capital constraints and two-tier entry cost through secondary market; cannot exit without external capital
 *   - Small Regional Operators: Secondary victims (moderate/constrained) — benefit from flexible slot access but face rising operational costs and profitability compression
 *   - Large Multi-National Operators: Primary beneficiaries (institutional/arbitrage) — use secondary market as efficient asset management tool; can exit any regional market; capture rising rents from slot holdings
 *   - Slot Brokers and Market Makers: Secondary beneficiaries (institutional/arbitrage) — capture bid-ask spread and provide genuine liquidity; pure arbitrage capacity
 *   - Regulatory Authority: Institutional mediator (organized/constrained) — enforces coordination function while managing entry barriers; constrained by competing mandates for efficiency vs. fairness
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent allocation policy as inherent scarcity constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(slot_trading_secondary_market, 0.52).
domain_priors:suppression_score(slot_trading_secondary_market, 0.48).
domain_priors:theater_ratio(slot_trading_secondary_market, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(slot_trading_secondary_market, extractiveness, 0.52).
narrative_ontology:constraint_metric(slot_trading_secondary_market, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(slot_trading_secondary_market, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(slot_trading_secondary_market, tangled_rope).
narrative_ontology:human_readable(slot_trading_secondary_market, "Slot Trading Secondary Market Coordination with Extraction").
narrative_ontology:topic_domain(slot_trading_secondary_market, "economic/market_structure").

domain_priors:requires_active_enforcement(slot_trading_secondary_market).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(slot_trading_secondary_market, large_operators_with_capital).
narrative_ontology:constraint_beneficiary(slot_trading_secondary_market, slot_brokers).
narrative_ontology:constraint_victim(slot_trading_secondary_market, new_entrants).
narrative_ontology:constraint_victim(slot_trading_secondary_market, small_operators).
narrative_ontology:constraint_victim(slot_trading_secondary_market, slot_allocation_fairness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW MARKET ENTRANT (SNARE) — Faces a two-tier entry cost: primary allocation cost plus secondary market premium. Capital-constrained entrants cannot acquire sufficient slots to reach viable scale. Trapped by the coordination mechanism itself — slots exist only through the secondary market at inflated prices. No exit without capital infusion or regulatory intervention.
constraint_indexing:constraint_classification(slot_trading_secondary_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL REGIONAL OPERATOR (TANGLED ROPE) — Genuinely benefits from slot trading: can acquire marginal capacity without waiting for new allocation rounds. But constrained by capital barriers — buying slots competes with operational investment. Secondary market provides coordination function but with asymmetric distribution of benefits. Can remain viable but at reduced profitability relative to large operators.
constraint_indexing:constraint_classification(slot_trading_secondary_market, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LARGE MULTI-NATIONAL OPERATOR (ROPE) — Pure coordinator and beneficiary. Has capital to acquire slots at any price; trades excess capacity for immediate liquidity; uses secondary market as flexible asset management tool. Experiences the constraint as pure coordination: buying and selling slots optimizes network utilization. Maximum arbitrage capacity — can exit any regional market while retaining global presence.
constraint_indexing:constraint_classification(slot_trading_secondary_market, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SLOT BROKER/MARKET MAKER (ROPE) — Primary beneficiary of the market structure itself. Captures bid-ask spread; provides genuine liquidity function by warehousing slots and matching buyers to sellers. Low enforcement cost — market participants self-enforce through transaction incentives. Experiences secondary market as efficient coordination mechanism with built-in profit opportunity. Pure arbitrage capacity — can enter and exit any market segment.
constraint_indexing:constraint_classification(slot_trading_secondary_market, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AUTHORITY (TANGLED ROPE) — Faces competing mandates: secondary markets coordinate supply efficiently (genuine benefit), but also create barrier to entry that concentrates market power. Enforces slot trading rules that enable extraction while providing coordination function. Constrained by political pressure from new entrants vs. efficiency benefits for large operators. Active enforcement required to prevent predatory behavior (slot hoarding, price collusion).
constraint_indexing:constraint_classification(slot_trading_secondary_market, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, secondary markets for scarce allocated resources (spectrum, landing slots, environmental permits) appear as an immutable feature of coordination when primary allocation is fixed. Scarcity + reallocation = market mechanism. This perspective risks naturalizing a contingent institutional choice (fixed primary allocation) as an inherent law. The engine will identify this as a false summit — the 'immutability' rests on the assumption that primary allocation remains fixed.
constraint_indexing:constraint_classification(slot_trading_secondary_market, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(slot_trading_secondary_market_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(slot_trading_secondary_market, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(slot_trading_secondary_market, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(slot_trading_secondary_market, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(slot_trading_secondary_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The secondary market enables genuine coordination (matching supply to demand dynamically) but within a fixed primary allocation framework that creates scarcity rents. New entrants and small operators bear the full cost of this rent through higher slot acquisition prices. The extractiveness reflects that slot brokers and large operators capture value that would otherwise distribute to smaller competitors or new entrants. The measurement trajectory shows rising extractiveness as markets mature and sophisticated operators accumulate holdings — this is not a measurement error but evidence that the extraction mechanism strengthens over time as market structure consolidates. Suppression (0.48): Moderate. Barriers to competition include capital requirements, information asymmetries in slot value assessment, and regulatory restrictions on slot accumulation. Suppression is active enforcement by brokers (who gatekeep transactions) and regulators (who enforce slot ownership limits). But suppression is not total — secondary markets themselves reduce barriers relative to waiting for new primary allocations. Theater ratio (0.35): Low. Secondary markets operate primarily through price mechanisms rather than performative compliance. Minimal ritual or symbolic content compared to allocation committee processes or regulatory review procedures.
 *
 * PERSPECTIVAL GAP:
 *   Each perspective captures a different relationship to the scarcity and coordination function. The beneficiaries experience primarily the coordination benefit (secondary markets solve their flexibility problem). The victims experience primarily the extraction cost (secondary markets make entry impossible). The mediator (regulatory authority) experiences both simultaneously and cannot resolve the tension without policy change. The analytical observer risks collapsing this structural complexity into a natural law (scarcity is inevitable) rather than recognizing it as an institutional choice (primary allocation is fixed by policy).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for each perspective are determined by the agent's capital position, exit options, and structural relationship to the scarcity rent. Large operators with arbitrage options experience low d (0.15-0.25) — they are beneficiaries with multiple exit paths. Slot brokers experience low d (0.20) — their function IS providing liquidity, and they can exit any market segment. Small operators with constrained capital experience moderate d (0.55-0.65) — they are partly victims of the scarcity rent but partly benefit from the coordination mechanism. New entrants face high d (0.85-0.95) — they are trapped by capital requirements and cannot access the market through any mechanism other than secondary market premium payment. Regulatory authorities experience moderate-high d (0.60-0.70) — they bear costs of enforcement without capturing the coordination benefits that accrue to operators. The analytical observer at the civilizational context experiences high d (0.72) — the perspective risks naturalizing a contingent policy as universal law, creating responsibility for perpetuating the extraction mechanism without recognizing it as policy choice.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that market mechanisms can be simultaneously coordination solutions and extraction mechanisms depending on the structural framework. The mandatrophy question is NOT 'is the secondary market good or bad?' but 'good for whom, and at what cost to whom else?' The large operator's rope is genuine coordination from their perspective. The new entrant's snare is genuine extraction from their perspective. The regulatory authority's tangled rope is the true structural position — they must enforce rules that enable both coordination and extraction simultaneously. The false summit (analytical mountain) reveals the core mandatrophy: calling secondary markets 'natural' or 'immutable' naturalizes the primary allocation policy that creates the scarcity they then coordinate. The resolution is transparency about which policy choices generate which distributional outcomes, not a single definitive classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    primary_allocation_contingency,
    'Is the scarcity that drives secondary market extraction inherent to the resource, or contingent on the choice to cap primary allocation?',
    'Historical comparison: markets where primary allocation expanded vs. those with fixed caps; counterfactual analysis of per-operator slot costs under different primary allocation policies',
    'If contingent on allocation policy: mountain classification is false summit; the constraint is policy choice (tangled rope at best). If inherent to resource: scarcity is genuine natural constraint on coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(primary_allocation_contingency, empirical, 'Whether secondary market extraction is inherent to scarcity or contingent on allocation policy').

omega_variable(
    coordination_vs_rent_extraction_boundary,
    'What portion of secondary market pricing reflects genuine coordination cost (matching, settlement, inventory holding) vs. pure rent extraction from capacity scarcity?',
    'Regression analysis of slot prices on: operator size, regional scarcity, broker spread, settlement time, inventory cost, collateral requirements. Decompose price premium into coordination cost and rent components.',
    'If coordination >> rent (>80%): constraint is primarily Rope for most agents. If rent >> coordination (>70%): constraint is primarily Snare for capacity-constrained agents. If balanced (40-60%): tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_rent_extraction_boundary, empirical, 'Proportion of secondary market pricing that reflects coordination vs. rent extraction').

omega_variable(
    entry_substitutability_alternatives,
    'Can new entrants bypass secondary market extraction through alternative market access mechanisms (partnerships, shared infrastructure, regulatory exemptions)?',
    'Comparative analysis of entrant pathways: secondary market purchase vs. code-share partnerships, capacity-sharing agreements, or exemptions from primary allocation caps. Track entrant survival rates across pathways.',
    'If rich alternatives exist: secondary market is coordination tool, not trap (Rope/Tangled Rope classification softens). If secondary market is sole viable pathway: extraction is binding, snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entry_substitutability_alternatives, empirical, 'Whether alternative entry pathways can substitute for secondary market access').

omega_variable(
    price_discovery_efficiency,
    'Do secondary market prices efficiently reflect true operational value of slots, or do they reflect speculative dynamics and capital-availability inequalities among operators?',
    'Time-series analysis of slot prices vs. utilization metrics, fuel costs, and demand; identification of price volatility not explained by operational fundamentals; comparison of buy-side vs. sell-side prices.',
    'If prices efficient: market mechanism allocates slots to highest-value users (coordination advantage). If prices reflect speculation/capital inequality: extraction persists even with market clearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_discovery_efficiency, empirical, 'Whether secondary market prices reflect operational value or speculative dynamics').

omega_variable(
    regulatory_arbitrage_opportunity,
    'Can operators engineer regulatory arbitrage by shifting slot holdings across jurisdictions with different secondary market rules or tax treatment?',
    'Network analysis of cross-border slot transfers; comparison of slot price differentials across jurisdictions; identification of transfers correlated with tax/regulatory changes.',
    'If arbitrage possible: secondary market extraction is amplified by regulatory gaps (enforcement burden rises). If markets are arbitrage-tight: enforcement is more containable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_opportunity, empirical, 'Whether regulatory arbitrage amplifies secondary market extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(slot_trading_secondary_market, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slot_tr_t0, slot_trading_secondary_market, theater_ratio, 0, 0.3).
narrative_ontology:measurement(slot_tr_t5, slot_trading_secondary_market, theater_ratio, 5, 0.32).
narrative_ontology:measurement(slot_tr_t10, slot_trading_secondary_market, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(slot_be_t0, slot_trading_secondary_market, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(slot_be_t5, slot_trading_secondary_market, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(slot_be_t10, slot_trading_secondary_market, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(slot_su_t0, slot_trading_secondary_market, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(slot_su_t5, slot_trading_secondary_market, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(slot_su_t10, slot_trading_secondary_market, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(slot_trading_secondary_market, resource_allocation).
narrative_ontology:affects_constraint(slot_trading_secondary_market, primary_allocation_rule_design).
narrative_ontology:affects_constraint(slot_trading_secondary_market, market_concentration_through_slot_accumulation).

% DUAL FORMULATION NOTE:
% The secondary slot market is structurally dependent on primary allocation policy. If primary allocation were dynamic (expanding with demand), secondary market extractiveness would collapse to near zero — slots would be abundant and markets would be thin or absent. The constraint family includes: (1) primary_allocation_rule_design (ε~0.35, tangled_rope/rope depending on primary allocation generosity), (2) slot_trading_secondary_market (ε~0.52, tangled_rope), (3) market_concentration_through_slot_accumulation (ε~0.68, snare). These are causally linked: the primary allocation policy determines whether secondary markets exist; secondary market dynamics determine whether concentration accumulates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(slot_trading_secondary_market, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
