% ============================================================================
% CONSTRAINT STORY: terms_of_trade_volatility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_terms_of_trade_volatility, []).

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
 *   constraint_id: terms_of_trade_volatility
 *   human_readable: Terms of Trade Volatility and Structural Dependency
 *   domain: international_economics/trade
 *
 * SUMMARY:
 *   Terms of trade volatility describes the structural phenomenon whereby
 *   commodity-exporting economies face unpredictable price fluctuations in
 *   their primary export goods, while commodity-importing economies and
 *   financial traders benefit from price variability and information
 *   asymmetries. This constraint operates simultaneously as coordination
 *   mechanism (stabilizing global commodity supply), extraction apparatus
 *   (transferring income from exporters to importers and traders), and
 *   degraded institutional system (financial speculation obscuring price
 *   discovery). The constraint exhibits all six DR types from different
 *   structural positions, revealing how a single economic phenomenon can be
 *   classified radically differently depending on the observer's power, exit
 *   capacity, and temporal horizon. The extractiveness measurement trajectory
 *   (0.35→0.62 over 15 years) reflects increasing financialization of
 *   commodity markets: as futures trading volume has expanded relative to
 *   physical commodity flows, the volatility mechanism has shifted from a
 *   byproduct of supply-demand coordination to an increasingly artificial
 *   extraction mechanism driven by macroeconomic portfolio flows and
 *   speculation.
 *
 * KEY AGENTS:
 *   - Commodity Exporters: Primary victims (powerless/trapped) — economies structurally dependent on commodity sales, bearing full cost of price volatility through income instability and fiscal crises
 *   - Developing Economy Governments: Secondary victims (moderate/constrained) — fiscally dependent on commodity revenue, attempting collective price stabilization with limited enforcement power
 *   - Commodity Importers: Primary beneficiaries (institutional/arbitrage) — wealthy economies and corporations that can time purchases, build reserves, and substitute inputs when prices spike
 *   - Multinational Commodity Traders: Secondary beneficiaries (powerful/mobile) — capture margin through information asymmetries, geographic arbitrage, and intermediation across supply chains
 *   - Producer Cartels (OPEC, cocoa syndicates): Organized actors (organized/constrained) — attempting coordination through supply discipline, but facing defection pressure and substitution threats
 *   - International Commodity Exchanges: Institutional actors (institutional/constrained) — venues for price discovery that have degraded into speculation platforms, maintaining function through regulatory entrenchment
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional architecture (financialization, information asymmetries, cartel structure) as immutable market law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(terms_of_trade_volatility, 0.58).
domain_priors:suppression_score(terms_of_trade_volatility, 0.65).
domain_priors:theater_ratio(terms_of_trade_volatility, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(terms_of_trade_volatility, extractiveness, 0.58).
narrative_ontology:constraint_metric(terms_of_trade_volatility, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(terms_of_trade_volatility, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(terms_of_trade_volatility, tangled_rope).
narrative_ontology:human_readable(terms_of_trade_volatility, "Terms of Trade Volatility and Structural Dependency").
narrative_ontology:topic_domain(terms_of_trade_volatility, "international_economics/trade").

domain_priors:requires_active_enforcement(terms_of_trade_volatility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(terms_of_trade_volatility, commodity_importers).
narrative_ontology:constraint_beneficiary(terms_of_trade_volatility, multinational_traders).
narrative_ontology:constraint_beneficiary(terms_of_trade_volatility, price_setters).
narrative_ontology:constraint_victim(terms_of_trade_volatility, commodity_exporters).
narrative_ontology:constraint_victim(terms_of_trade_volatility, developing_economies).
narrative_ontology:constraint_victim(terms_of_trade_volatility, subsistence_agriculture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMODITY EXPORTER (SNARE) — Trapped in export dependency with no exit. Price volatility in global commodity markets extracts income through uncontrolled fluctuations. Suppression is structural: geographic specialization, infrastructure investment in extraction, international debt obligations, and limited diversification options prevent exit. Maximum extraction experienced by powerless agents with zero alternatives.
constraint_indexing:constraint_classification(terms_of_trade_volatility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING ECONOMY GOVERNMENT (TANGLED ROPE) — Constrained by fiscal dependency on commodity export revenue. Faces genuine coordination need: stabilizing export income requires collective agreement on supply management or price floors (OPEC model). But the coordination mechanism is asymmetrically enforced: price-setting capacity concentrates among exporters with spare capacity or reserve funds. Suppression from both external volatility and internal inability to enforce producer discipline. Moderate extraction: some coordination benefit (mutual stabilization attempts) alongside real costs (foregone revenue during production restraint).
constraint_indexing:constraint_classification(terms_of_trade_volatility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMODITY IMPORTER (ROPE) — Benefits from low, volatile commodity prices. Can arbitrage: delay purchases when prices spike, build reserves when prices crash. Institutional power enables buffer stocks and portfolio diversification. Experiences the constraint as coordination: maintaining global commodity markets requires accepting supplier participation and stable demand signals. Net beneficiary with high exit optionality — can access alternatives, substitute inputs, or shift geographic sourcing.
constraint_indexing:constraint_classification(terms_of_trade_volatility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTINATIONAL TRADER (TANGLED ROPE) — Powerful actor with mobile options (vertical integration, geographic arbitrage, futures hedging). Benefits from volatility through trading spreads and arbitrage opportunities. Coordinates supply chains across multiple sourcing regions while extracting margin through price differentials. Suppression of alternatives is active: traders cultivate information asymmetries, restrict market access through capital requirements and credit terms, and maintain opaque pricing. Moderate-high extraction relative to power: genuine supply-chain coordination function combined with asymmetric information capture.
constraint_indexing:constraint_classification(terms_of_trade_volatility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PRODUCER CARTEL (ROPE) — Organized agents (OPEC, cocoa syndicates) see the constraint as a coordination problem: maintaining price floors requires collective supply discipline. Suppression is internal (enforcing discipline against defection) and external (preventing non-cartel entry or substitute development). But the classification remains Rope because the cartel's primary function IS coordination — stabilizing supply and price to enable joint welfare. Extraction occurs but serves a coordination goal. High suppression (barriers to defection, exclusion of non-members) but low base extractiveness from the cartel's own structural position.
constraint_indexing:constraint_classification(terms_of_trade_volatility, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL COMMODITY EXCHANGE (PITON) — Institutional venue (CME, LME, ICE) whose primary function was price discovery now operates substantially for speculation and hedging. The market mechanism persists through institutional inertia: exchanges maintain themselves as essential infrastructure, but actual price formation increasingly reflects financial flows rather than supply-demand fundamentals. Theater ratio high: trading volume far exceeds physical commodity movement; price movements are driven by macroeconomic sentiment and portfolio rebalancing rather than crop reports or production changes. Degraded coordination function maintained through regulatory entrenchment.
constraint_indexing:constraint_classification(terms_of_trade_volatility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalized perspective, terms of trade volatility appears as an irreducible mathematical property of multi-agent commodity markets: with imperfect information, heterogeneous production cycles, and large numbers of geographically dispersed agents, price volatility is an inherent feature of equilibrium discovery. Weather-driven supply shocks, demand elasticity mismatches, and information asymmetries about future production are structural to agriculture and mining. This perspective risks naturalizing what is actually a contingent institutional arrangement: volatility magnitude and its distribution across agents depend on market structure, speculation intensity, and hedging infrastructure — all human-chosen parameters. Engine will classify this as a false summit.
constraint_indexing:constraint_classification(terms_of_trade_volatility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(terms_of_trade_volatility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(terms_of_trade_volatility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(terms_of_trade_volatility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(terms_of_trade_volatility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(terms_of_trade_volatility, TR),
    TR >= 0.70.

:- end_tests(terms_of_trade_volatility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The base constraint reflects genuine structural asymmetry: commodity exporters are price-takers in global markets, while importers and traders are price-makers with hedging and arbitrage capacity. The 23-point increase over the measurement interval (0.35→0.62) reflects rising financialization: commodity futures have become decoupled from physical supply-demand ratios, driven increasingly by macro sentiment and leverage dynamics. Suppression (0.65): High. Barriers to exit include: geographic specialization in production, infrastructure and capital sunk in extraction, international debt obligations denominated in commodity revenues, limited alternative employment in commodity-exporting regions, and information asymmetries preventing exporters from understanding or hedging futures markets. Theater ratio (0.48, rising to 0.55): Moderate and increasing. Financial trading volume in commodity futures now exceeds physical commodity flows by 10-20x for crude oil, copper, and grain. This indicates that price formation is increasingly decoupled from fundamentals, driven by financial flows. However, the theater is not yet dominant (would need >0.70) because commodity exchanges still maintain some price-discovery function — fundamentals still move prices, they are just overwhelmed by financial noise. The rising trajectory reflects increasing theatricalization as speculation intensity grows.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival gap is among the largest in the corpus because the directionality flow is completely asymmetric: a single price movement creates opposing effects. When prices spike, commodity exporters gain nominal revenue but cannot invest it (prices may crash tomorrow, creating fiscal uncertainty). Commodity importers lose nominal cost but gain strategic advantage (they can defer purchases until prices fall). Traders gain in both directions (they profit from volatility itself, not direction). Financial markets gain (speculation becomes more profitable as volatility increases). The gap between Snare (exporter view) and Rope (importer view) is not about different measurements of the same constraint — it is about the same constraint having literally opposite extractive direction for different agents. This is the canonical case where 'effectiveness' and 'extraction' must be computed separately for each agent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary and victim declarations plus exit capacity. Commodity exporters are declared victims with trapped exit (high d ~0.90). Commodity importers and traders are declared beneficiaries with arbitrage/mobile exit (low d ~0.15-0.25). The sigmoid f(d) produces chi asymmetry: f(0.90) ≈ 1.42, f(0.15) ≈ -0.01. Scope modifier σ(S) = 1.2 (global scope) amplifies the asymmetry. The result: chi_exporter ≈ 0.58 × 1.42 × 1.2 ≈ 0.99 (near-maximum experienced extraction), chi_importer ≈ 0.58 × (-0.01) × 1.2 ≈ -0.007 (negative extraction / subsidy). This directionality reversal is the structural signature of an extractive constraint: the same mechanism produces opposite results for different agents. Overrides are not needed because the derivation chain correctly captures the structural relationships from beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint is classified as Tangled Rope at the analytical level, not as pure Snare (0.66+ χ) or pure Rope. This requires justification: why does genuine coordination function exist alongside asymmetric extraction? The coordination function is real: commodity markets DO solve the problem of matching geographically dispersed producers with global consumers. Prices DO emerge from this coordination. Supply chains DO form around commodity flows. The constraint exists because this coordination cannot exist without some agents bearing volatility risk. Commodity importers and traders (the beneficiaries) will not participate unless they can hedge and arbitrage — hedging requires liquid futures markets, which requires speculation, which amplifies price volatility. The exporter victims cannot exit the system because their economies are structured around commodity production. Thus the system reaches a tangled equilibrium: coordination proceeds (Rope function) but only by extracting stability from the powerless (Snare effect). The asymmetric extraction is not separable from the coordination — it is the mechanism by which coordination is accomplished. The mandatrophy is resolved by recognizing that Tangled Rope is the correct classification: high χ (0.58-0.99 for exporters), mixed coordination and extraction mechanisms, active enforcement (cartels, regulatory frameworks, financial infrastructure), and asymmetric distribution across agents. The constraint should NOT be misclassified as pure Rope (which would suggest benign coordination) or pure Snare (which would ignore the real coordination function). Tangled Rope is diagnostically precise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speculation_vs_hedging_ratio,
    'What proportion of commodity futures trading volume represents genuine hedging of real exposure versus financial speculation? Does this ratio change the classification from Tangled Rope to Snare?',
    'Analysis of open interest positions relative to physical commodity flows; tracking of non-commercial trader participation; correlation between speculation intensity and price volatility magnitude',
    'If speculation dominates (>70% non-commercial): constraint reclassifies as Snare for commodity exporters — volatility is artificially amplified extraction. If hedging dominates (>50% commercial): constraint remains Tangled Rope — volatility is a coordination byproduct, not pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculation_vs_hedging_ratio, empirical, 'Ratio of financial speculation to genuine hedging in commodity futures').

omega_variable(
    cartel_stability_threshold,
    'What is the minimum price premium required to maintain producer cartel discipline against defection? Does this threshold define the boundary between coordination function and extraction mechanism?',
    'Historical analysis of cartel collapses and price-floor violations; modeling of defection incentives as function of price differential; comparison of actual enforcement costs to revenue capture',
    'If threshold is low (<5% premium): cartel extraction is minimal, classification as Rope is robust. If threshold is high (>20%): cartel is primarily an extraction mechanism (Snare), with coordination as the cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartel_stability_threshold, empirical, 'Minimum price premium needed for cartel discipline').

omega_variable(
    real_vs_financial_volatility,
    'What fraction of observed commodity price volatility is driven by real supply-demand shocks versus financial market dynamics? Does the real component alone constitute an unavoidable Mountain, while the financial component is a human-constructed Snare?',
    'Decomposition of price variance into fundamentals (weather, production, inventory) and financialization (macro sentiment, portfolio flows, leverage cycles); comparison of volatility across commodities with different trading volumes; historical volatility before and after financialization of commodity markets',
    'If real component dominates (>60%): constraint is partially a Mountain (some volatility is unavoidable). If financial component dominates (>60%): the constraint is mostly humanly constructed (Tangled Rope to Snare depending on beneficiary intent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(real_vs_financial_volatility, empirical, 'Decomposition of price volatility into real fundamentals versus financial speculation').

omega_variable(
    information_asymmetry_quantification,
    'How much of commodity traders'' profit margin derives from information advantages over commodity exporters? Is this extractive arbitrage or legitimate coordination service?',
    'Analysis of trader profit distribution (bid-ask spreads, storage margins, forward premiums) relative to information lag; comparison of exporter revenues when trading through direct contracts versus through intermediaries; measurement of information asymmetry through price discovery timing',
    'If trader margins exceed transaction costs by >2x: extraction mechanism dominates, classification shifts toward Snare. If margins approximate transaction costs: coordination service classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_quantification, empirical, 'Quantification of trader profit from information asymmetry').

omega_variable(
    substitution_feasibility,
    'For commodity exporters (primary victims), how feasible is economic diversification or domestic value addition? Does true infeasibility make the trap a Mountain, or is the trap contingent on policy choices and institutions?',
    'Case studies of successful diversification (Malaysia, Indonesia, Botswana partial success); analysis of institutional barriers to processing/manufacturing in commodity exporters; measurement of capital and skill requirements relative to available resources',
    'If diversification is materially infeasible for most exporters: trap is structural (Mountain). If many exporters have succeeded through specific policies: trap is institutional (Snare/Tangled Rope), not natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_feasibility, empirical, 'Feasibility of economic diversification away from commodity export dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(terms_of_trade_volatility, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tot_tr_t0, terms_of_trade_volatility, theater_ratio, 0, 0.3).
narrative_ontology:measurement(tot_tr_t5, terms_of_trade_volatility, theater_ratio, 5, 0.42).
narrative_ontology:measurement(tot_tr_t10, terms_of_trade_volatility, theater_ratio, 10, 0.48).
narrative_ontology:measurement(tot_tr_t15, terms_of_trade_volatility, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(tot_be_t0, terms_of_trade_volatility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tot_be_t5, terms_of_trade_volatility, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(tot_be_t10, terms_of_trade_volatility, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(tot_be_t15, terms_of_trade_volatility, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(terms_of_trade_volatility, resource_allocation).
narrative_ontology:affects_constraint(terms_of_trade_volatility, commodity_export_fiscal_dependency).
narrative_ontology:affects_constraint(terms_of_trade_volatility, financial_speculation_feedback).
narrative_ontology:affects_constraint(terms_of_trade_volatility, cartel_enforcement_mechanisms).

% DUAL FORMULATION NOTE:
% Terms of trade volatility decomposes into three structurally distinct constraints: (1) commodity_export_fiscal_dependency (extractiveness 0.45) — structural dependency of commodity economies on revenue; (2) financial_speculation_feedback (extractiveness 0.62) — financialization amplifying volatility; (3) cartel_enforcement_mechanisms (extractiveness 0.30) — coordination attempts with internal defection pressure. Each has different ε. The present story models the integrated constraint. Upstream stories address causes; downstream stories address consequences (sovereign debt cycles, currency crashes, political instability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
