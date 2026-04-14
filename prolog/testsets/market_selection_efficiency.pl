% ============================================================================
% CONSTRAINT STORY: market_selection_efficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_selection_efficiency, []).

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
 *   constraint_id: market_selection_efficiency
 *   human_readable: Market Selection Efficiency and Coordination
 *   domain: economics/market_structure
 *
 * SUMMARY:
 *   Market selection efficiency describes the mechanism by which capital
 *   flows toward productive uses through price signals and information-driven
 *   competition. This constraint exhibits the central tension of capitalist
 *   coordination: markets excel at aggregating dispersed information through
 *   competition, yet this same competitive pressure creates incentives to
 *   extract information asymmetries. The constraint demonstrates genuine
 *   coordination function (price discovery, capital allocation) layered with
 *   extraction mechanisms (information advantage, front-running, regulatory
 *   arbitrage). Over the measurement interval (0-30 years), extractiveness
 *   has risen from 0.28 to 0.58 as high-frequency trading, algorithmic
 *   advantage, and information technology have increased information access
 *   barriers. Theater ratio has risen from 0.18 to 0.35 as regulatory
 *   frameworks (disclosure requirements, fair trading rules) have become
 *   increasingly theatrical — compliance activity persists despite
 *   high-frequency trading circumventing the intent of transparency rules.
 *
 * KEY AGENTS:
 *   - Capital Allocators: Primary beneficiaries (institutional/arbitrage) — extract information advantages and deploy capital with superior timing; can arbitrage across pricing regimes
 *   - Non-Advantaged Competitors: Primary victims (powerless/trapped) — face persistent information disadvantage in capital allocation; cannot exit market participation without abandoning economic participation
 *   - Retail Participants: Secondary victims (moderate/constrained) — benefit from price signals but bear extraction through information lag and transaction costs; constrained by limited capital and access
 *   - Regulatory Frameworks: Institutional actor (institutional/mobile) — maintain transparency theater despite degradation; see own function as weakened by financial innovation
 *   - Alternative Market Structure Coalition: Organized agents (organized/constrained) — DeFi, blockchain, direct matching platforms represent temporary scaffold that may bypass traditional market selection
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing information asymmetry as inherent economic law rather than policy-constructed barrier
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_selection_efficiency, 0.52).
domain_priors:suppression_score(market_selection_efficiency, 0.48).
domain_priors:theater_ratio(market_selection_efficiency, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_selection_efficiency, extractiveness, 0.52).
narrative_ontology:constraint_metric(market_selection_efficiency, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(market_selection_efficiency, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_selection_efficiency, tangled_rope).
narrative_ontology:human_readable(market_selection_efficiency, "Market Selection Efficiency and Coordination").
narrative_ontology:topic_domain(market_selection_efficiency, "economics/market_structure").

domain_priors:requires_active_enforcement(market_selection_efficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_selection_efficiency, capital_allocators).
narrative_ontology:constraint_beneficiary(market_selection_efficiency, information_arbitrageurs).
narrative_ontology:constraint_victim(market_selection_efficiency, non_information_advantaged_competitors).
narrative_ontology:constraint_victim(market_selection_efficiency, market_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ADVANTAGED COMPETITOR (SNARE) — Trapped in asymmetric information environment. Cannot access real-time pricing signals, proprietary data, or algorithmic insights available to large capital allocators. Bears full extraction cost through persistent disadvantage in capital allocation and market timing. No meaningful exit — exit would mean leaving markets entirely.
constraint_indexing:constraint_classification(market_selection_efficiency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CAPITAL ALLOCATOR (ROPE) — Benefits from information asymmetry and can arbitrage across markets. Experiences market selection as enabling coordination: price discovery and capital flow toward productive uses. Has arbitrage exit — can redeploy capital, adjust strategy, exploit pricing gaps. Net beneficiary with low suppression relative to position.
constraint_indexing:constraint_classification(market_selection_efficiency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: RETAIL PARTICIPANT (TANGLED ROPE) — Experiences market selection as both coordination mechanism (price signals guide allocation decisions) and extraction mechanism (information lag and transaction costs consume returns). Constrained by limited capital, information access, and time — can exit retail markets but at cost of foregone participation. Mixed experience: some benefit, substantial extraction.
constraint_indexing:constraint_classification(market_selection_efficiency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY FRAMEWORK (PITON) — Market transparency regulations, disclosure requirements, and fair trading rules once enforced genuine market function. Now largely theatrical: high-frequency trading front-runs disclosed information, dark pools bypass transparency, regulatory arbitrage allows evasion. The framework persists through institutional inertia — regulators maintain the apparatus despite degraded function. Theater ratio high (0.65+) because compliance activity exceeds actual market integrity contribution.
constraint_indexing:constraint_classification(market_selection_efficiency, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE MARKET STRUCTURE COALITION (SCAFFOLD) — Decentralized finance, blockchain markets, direct peer-to-peer platforms represent temporary scaffolding that circumvents traditional market selection. These alternatives have genuine sunset clauses: they either mature into stable infrastructure (becoming rope or tangled rope themselves) or collapse when real-world constraints (custody, custody, regulatory pressure) make them unviable. Current extraction is suppressed by alternative pathway availability, but sunset is uncertain.
constraint_indexing:constraint_classification(market_selection_efficiency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, market selection efficiency is an immutable consequence of dispersed knowledge: no agent can process all relevant information, so some information asymmetry is inherent to coordination via prices. Emergence of selection mechanisms is a law of economic organization. However, structural data reveals this naturalizes contingent institutional features — information access barriers are policy-constructed, not physical laws.
constraint_indexing:constraint_classification(market_selection_efficiency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_selection_efficiency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_selection_efficiency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_selection_efficiency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_selection_efficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_selection_efficiency, TR),
    TR >= 0.70.

:- end_tests(market_selection_efficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Market selection creates genuine coordination value through price discovery and capital allocation toward productive uses, but this coordination is layered with extraction mechanisms. Information-advantaged agents capture return premiums that reflect not superior insight but information access barriers. Over the measurement interval, extractiveness has risen as technology has enabled finer information asymmetries (high-frequency trading, dark pools, proprietary data access). The rising trajectory indicates that institutional innovations increasingly permit extraction beyond coordination cost. Suppression (0.48): Moderate. Barriers to exit include the requirement of capital participation for economic security, information access costs that constrain independent verification, and regulatory frameworks that enforce particular market structures. But suppression is not maximal — traders can exit specific markets, switch brokers, or access alternative platforms. Behavioral patterns show adaptation (passive indexing, algorithmic trading by smaller actors) indicating partial, not total, suppression. Theater ratio (0.35): Moderate. Market transparency regulations (disclosure requirements, fair trading rules) have meaningful function in preventing egregious manipulation, but much regulatory activity is theatrical — compliance with transparency rules proves ineffective against front-running and algorithmic predation. Theater ratio has risen from 0.18 as alternatives (dark pools, high-frequency trading) have outpaced regulatory enforcement.
 *
 * PERSPECTIVAL GAP:
 *   Capital allocators perceive market selection as pure coordination (Rope) — they see price signals guiding efficient capital deployment. The organized coalition perceives it as temporary scaffolding with a sunset path (Scaffold) — alternative platforms can replace traditional markets. Regulatory frameworks perceive it as degraded ritual (Piton) — transparency rules persist but are increasingly circumvented. Retail participants perceive mixed coordination and extraction (Tangled Rope) — they benefit from price signals but bear information lag costs. Non-advantaged competitors perceive pure extraction (Snare) — they face unavoidable information disadvantage with no exit. The civilizational analytical perspective risks perceiving immutable natural law (Mountain) — information asymmetry seems inherent to any coordination system — but structural data reveals this as false naturalization: much of the asymmetry is policy-constructed (market access rules, data ownership, algorithmic trading regulation).
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) reflects their structural position in the information flow. Capital allocators with arbitrage options derive low d — they are net beneficiaries receiving the extraction flow, experiencing low or negative chi. Non-advantaged competitors with trapped exit derive high d — they cannot avoid information disadvantage, experiencing maximum chi from their perspective. Retail participants with constrained exit derive moderate d — they can reduce exposure but at cost (passive funds have lower returns, avoiding markets eliminates wealth-building mechanism). The derivation chain confirms that beneficiary status (capital allocators) reduces d while victim status (non-advantaged competitors) increases d. This structural differentiation is the mechanism through which market selection extracts asymmetric returns.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing that market selection efficiency contains BOTH genuine coordination (price discovery, capital allocation toward productivity) and extraction (information-based advantage, suppression of alternative mechanisms). The constraint is not 'is market selection good or bad' but 'which institutional choices determine the balance between coordination and extraction?' The tangled rope classification is correct and non-degraded: the constraint manifestly requires active enforcement (regulatory frameworks, market structure rules), has beneficiaries (capital allocators) and victims (non-advantaged competitors), and contains measurable coordination function alongside measurable extraction. The rising extractiveness over the measurement interval reflects policy choices (allowing high-frequency trading, permitting dark pools, enabling algorithmic predation) not inherent economic law. The false summit (mountain perspective) is identified by noting that alternative allocation mechanisms (planning, matching, mutual funds) achieve comparable efficiency without comparable extraction, proving the current asymmetry is constructed, not inherent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_asymmetry_scope,
    'What portion of observed market return differential is due to inherent information lags versus policy-constructed barriers to information access?',
    'Comparative analysis of markets with different regulatory transparency regimes; measurement of return patterns before/after disclosure reforms; cross-jurisdiction correlation of information access costs with alpha capture',
    'If inherent (>70%): mountain classification is supported, extraction is coordination cost. If policy-constructed (<30%): snare classification is supported, extraction is institutional choice. Intermediate cases reveal tangled rope with variable suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_scope, empirical, 'Inherent vs policy-constructed information asymmetry in market selection').

omega_variable(
    alternative_allocation_efficiency,
    'Do alternative market structures (DeFi, direct peer matching, non-price-based allocation) achieve competitive capital allocation efficiency despite lower theater?',
    'Comparison of capital allocation outcomes across market types; measurement of deadweight loss and misallocation rates; correlation between transparency level and allocation accuracy',
    'If alternative structures equally efficient: current market selection is tangled rope with extractive overlay (suppression policy-driven). If alternative structures less efficient: current selection contains genuine coordination value beyond extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_allocation_efficiency, empirical, 'Whether alternative allocation mechanisms achieve comparable efficiency').

omega_variable(
    information_access_threshold,
    'What level of information access threshold distinguishes effective market selection from insider trading?',
    'Analysis of enforcement data; correlation between information access delay and trading advantage; examination of legal boundaries between arbitrage and manipulation',
    'If threshold low (<100ms): most information arbitrage is legally sanctioned extraction. If threshold high (>10s): most advantage is legitimate faster execution. Threshold location determines whether suppression is inherent or constructed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_access_threshold, conceptual, 'Information access threshold between arbitrage and manipulation').

omega_variable(
    market_selection_counterfactual,
    'In the absence of information-based selection, would alternative coordination mechanisms (planning, matching, randomization) produce superior capital allocation outcomes?',
    'Theoretical analysis of allocation efficiency under different institutional arrangements; comparison with planned economies, mutual funds, and controlled matching experiments',
    'If alternatives superior: market selection is snare naturalizing contingent choice. If market selection superior: constraint is genuine rope or tangled rope with necessary extraction component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_selection_counterfactual, conceptual, 'Whether alternative allocation mechanisms would outperform market selection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_selection_efficiency, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mse_tr_t0, market_selection_efficiency, theater_ratio, 0, 0.18).
narrative_ontology:measurement(mse_tr_t10, market_selection_efficiency, theater_ratio, 10, 0.28).
narrative_ontology:measurement(mse_tr_t20, market_selection_efficiency, theater_ratio, 20, 0.35).
narrative_ontology:measurement(mse_tr_t30, market_selection_efficiency, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(mse_be_t0, market_selection_efficiency, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mse_be_t10, market_selection_efficiency, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(mse_be_t20, market_selection_efficiency, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(mse_be_t30, market_selection_efficiency, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_selection_efficiency, resource_allocation).
narrative_ontology:affects_constraint(market_selection_efficiency, information_asymmetry_in_financial_markets).
narrative_ontology:affects_constraint(market_selection_efficiency, regulatory_capture_finance_sector).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_selection_efficiency, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
