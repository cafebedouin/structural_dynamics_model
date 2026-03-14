% ============================================================================
% CONSTRAINT STORY: pokemon_card_market_volatility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pokemon_card_market_volatility, []).

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
 *   constraint_id: pokemon_card_market_volatility
 *   human_readable: Pokemon Card Market Volatility and Collector Extraction
 *   domain: collectibles/financial_markets/consumer_behavior
 *
 * SUMMARY:
 *   The Pokemon trading card market has transformed from a childhood hobby
 *   into a speculative financial asset class, creating a structural
 *   constraint that combines genuine coordination functions (price discovery,
 *   authentication, liquidity provision) with significant extraction
 *   mechanisms (volatility rents, information asymmetry, psychological
 *   manipulation). The market exhibits classic tangled-rope characteristics:
 *   secondary market dealers and grading services provide real coordination
 *   that enables efficient matching of buyers and sellers, while
 *   simultaneously extracting value through spreads, fees, and artificially
 *   reinforced scarcity. Retail collectors face suppression through multiple
 *   channels: limited information on supply decisions, FOMO-driven
 *   psychological manipulation, high transaction costs (grading fees 1-3%,
 *   dealer spreads 5-15%), and identity fusion with the collecting hobby that
 *   raises perceived exit costs beyond material barriers. The constraint's
 *   evolution from low extractiveness (0.35, primarily hobby coordination) to
 *   high extractiveness (0.58, speculative volatility) maps to increasing
 *   theater ratio (0.42 to 0.72), indicating that performative speculation
 *   has crowded out actual hobby coordination. The Pokemon Company maintains
 *   scarcity through print-run decisions that create artificial volatility,
 *   enabling secondary market extraction while generating licensing fees from
 *   grading services and dealer margins. The analytical observer sees a
 *   tangled rope where the coordination function (efficient market clearing)
 *   is genuine but has become secondary to the extraction mechanism
 *   (volatility rents and information-asymmetric pricing).
 *
 * KEY AGENTS:
 *   - Retail Collectors: Primary victims (powerless/trapped) — bear full volatility burden, sunk costs, identity fusion with hobby
 *   - Secondary Market Dealers: Primary beneficiary (institutional/arbitrage) — capture bid-ask spreads, profit from volatility, low transaction costs
 *   - Card Grading Services (PSA/Beckett): Secondary beneficiary (institutional/arbitrage) — capture 1-3% of transaction value through authentication gatekeeping
 *   - Small Investors: Secondary victim (moderate/constrained) — face 10-15% annual fee burden, constrained liquidity, information disadvantage vs institutional traders
 *   - Pokemon Company: Organized actor (organized/mobile) — controls supply lever, captures licensing fees, maintains scarcity through print-run decisions
 *   - Secondary Market Speculation Culture: Institutional piton (institutional/arbitrage) — performative layer of price-watching and 'investing' rhetoric that has largely replaced hobby function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees full structure of coordination + extraction hybrid, reveals suppression mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pokemon_card_market_volatility, 0.58).
domain_priors:suppression_score(pokemon_card_market_volatility, 0.65).
domain_priors:theater_ratio(pokemon_card_market_volatility, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pokemon_card_market_volatility, extractiveness, 0.58).
narrative_ontology:constraint_metric(pokemon_card_market_volatility, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(pokemon_card_market_volatility, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pokemon_card_market_volatility, tangled_rope).
narrative_ontology:human_readable(pokemon_card_market_volatility, "Pokemon Card Market Volatility and Collector Extraction").
narrative_ontology:topic_domain(pokemon_card_market_volatility, "collectibles/financial_markets/consumer_behavior").

domain_priors:requires_active_enforcement(pokemon_card_market_volatility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pokemon_card_market_volatility, secondary_market_dealers).
narrative_ontology:constraint_beneficiary(pokemon_card_market_volatility, card_grading_services).
narrative_ontology:constraint_beneficiary(pokemon_card_market_volatility, pokemon_company_licensing).
narrative_ontology:constraint_victim(pokemon_card_market_volatility, retail_collectors).
narrative_ontology:constraint_victim(pokemon_card_market_volatility, small_investors).
narrative_ontology:constraint_victim(pokemon_card_market_volatility, market_price_discovery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL COLLECTOR (SNARE) — Powerless agents with no exit options face extraction via artificial scarcity, speculative price manipulation, and psychological manipulation through grading tiers and rarity signals. Trapped by sunk costs and identity fusion with collecting hobby. Bears full burden of volatility without ability to exit or hedge. Suppression includes information asymmetry, authentication gatekeeping, and FOMO-driven purchasing psychology.
constraint_indexing:constraint_classification(pokemon_card_market_volatility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL INVESTOR (TANGLED ROPE) — Constrained by modest capital and technical barriers to exit positions quickly. Experiences both coordination function (card market enables alternative investment exposure) and significant extraction (volatility, bid-ask spreads, grading service fees accumulate to 10-15% of position value annually). Organized institutional investors operate at different scale, paying lower effective fees.
constraint_indexing:constraint_classification(pokemon_card_market_volatility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SECONDARY MARKET DEALER (ROPE) — Institutional/arbitrage position enables low-friction exit and profitable coordination through price discovery. Dealers benefit from volatility as source of trading spreads. Genuine coordination function: aggregating buyer/seller information and enabling efficient matching. Net beneficiary through arbitrage capacity — extraction flows toward this agent.
constraint_indexing:constraint_classification(pokemon_card_market_volatility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CARD GRADING SERVICE (ROPE) — Institutional beneficiary with arbitrage exit options. Pure coordination function: standardizes authentication and condition assessment, enabling market price discovery. Captures 1-3% of transaction value as grading fees. Low suppression because service is optional (though practically necessary for high-value cards). Market-clearing mechanism justifies fee structure.
constraint_indexing:constraint_classification(pokemon_card_market_volatility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: POKEMON COMPANY (SCAFFOLD) — Organized actor with longer time horizon. Controls card supply as primary lever on market volatility. Print-run decisions create artificial scarcity that enables secondary market extraction. However, Pokemon Company has incentive structure toward sunset: excessive volatility damages brand equity and customer trust, limiting lifetime value extraction. Licensing restrictions and release rate controls represent active enforcement with declining effectiveness as secondary markets mature.
constraint_indexing:constraint_classification(pokemon_card_market_volatility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: SECONDARY MARKET SPECULATION CULTURE (PITON) — Institutional layer of performative activity (card flipping, YouTube speculation, 'investing' rhetoric) that has largely divorced from genuine hobby coordination. Theater ratio extremely high: most activity is price-watching and social performance rather than collection, play, or use. Mechanism persists through cultural inertia and FOMO feedback loops despite degraded actual function (hobby enjoyment). The constraint maintains itself through theatrical maintenance.
constraint_indexing:constraint_classification(pokemon_card_market_volatility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Full structural view reveals the constraint combines genuine coordination (price discovery, authentication, market liquidity) with significant extraction (volatility rents, information asymmetry, psychological manipulation). The 'investment' framing naturalizes extraction as market mechanism, obscuring suppression mechanisms. Suppression is high because exit costs (liquidity barriers, sunk costs, identity lock) create cognitive traps even when institutional structure permits exit.
constraint_indexing:constraint_classification(pokemon_card_market_volatility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pokemon_card_market_volatility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pokemon_card_market_volatility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pokemon_card_market_volatility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pokemon_card_market_volatility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pokemon_card_market_volatility, TR),
    TR >= 0.70.

:- end_tests(pokemon_card_market_volatility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, representing the effective value captured by beneficiaries relative to the cost borne by victims. The measurement trajectory (0.35 → 0.48 → 0.58) shows extraction accumulating as the market shifted from hobby to speculation. The baseline of 0.35 reflects legitimate coordination costs (price discovery, authentication, liquidity). The increase to 0.58 reflects the addition of extraction mechanisms: dealer spreads widening, grading fee escalation, volatility rents, and FOMO-driven psychological extraction. Not as high as a pure snare (0.66+) because some agents (small investors, dealers) can exit and some coordination function remains. Suppression (0.65): High. Multiple suppression channels: (1) Information asymmetry — retail collectors lack real-time supply/demand data, price prediction models, or access to wholesale channels. (2) Authentication gatekeeping — grading services control legitimacy signals, creating practical necessity of expensive grading. (3) Liquidity barriers — retail collectors face weeks/months to liquidate positions while institutional dealers clear in minutes. (4) Psychological suppression — FOMO loops, social comparison via YouTube card-opening, status signals embedded in grading tiers. (5) Exit cost illusion — identity fusion makes exit feel like identity death even when material barriers are surmountable. Theater ratio (0.68): High and increasing. The constraint has shifted toward performative activity: YouTube unboxing videos ($100K+ budgets), social media card displays, 'investment strategy' rhetoric, price-watching communities. The performative content (0.68) exceeds the actual coordination content (hobby play, collection building) among retail participants. Most collected cards are not played, displayed, or used — they are held as speculative assets. This is the signature of piton degradation within a tangled-rope structure.
 *
 * PERSPECTIVAL GAP:
 *   The gap between institutional perspectives and retail victim perspectives is the primary diagnostic signal. Secondary market dealers see rope (smooth price discovery, legitimate spreads for coordination service). Grading services see rope (authentication as market infrastructure). But retail collectors experience snare (trapped by information asymmetry, liquidity barriers, identity fusion). Small investors experience tangled rope (they get some coordination benefit — market access — but bear disproportionate extraction — fees, spreads, volatility). The Pokemon Company sees scaffold (temporary scarcity model eventually replaced by mature market mechanisms), but this is aspirational — the scarcity model is actively maintained and generates profits. The piton perspective reveals that speculation culture itself has become inertial: the performative layer persists not because it enables hobby coordination but because the social performance itself has become the hobby. The analytical observer sees the full structure: genuine coordination at core, extraction mechanisms layered on top, theatrical performance dominating the surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Retail collectors are victims with trapped exit (high d ~0.92), experiencing high experienced extraction chi. Secondary market dealers are beneficiaries with arbitrage exit (low d ~0.08), experiencing extraction that flows toward them (negative chi). Small investors are victims with constrained exit (moderate d ~0.72), experiencing significant but not maximal extraction. Pokemon Company is beneficiary with mobile exit (low-moderate d ~0.25), maintaining the system that generates their licensing extraction. The grading services are institutional beneficiaries with arbitrage (d ~0.10), capturing their fee through legitimate coordination provision. The piton layer (speculation culture) is institutional beneficiary with arbitrage (d ~0.08) — professional speculators and content creators profit from volatility. The perspectival gaps emerge because the same constraint produces opposite directionality values for different agents: retail collectors pay what institutional dealers collect.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint combines genuine coordination (price discovery, authentication, liquidity provision) with systematic extraction. It is not a false positive (tangled rope is accurate). The mandatrophy is resolved by recognizing that each institutional actor's perspective captures part of the actual structure. The retail collector's snare perspective is not wrong — they genuinely experience maximum extraction and suppression. The dealer's rope perspective is not wrong — they genuinely experience coordination benefits. The analytical observer's tangled rope is the meta-perspective that includes both: the constraint is snare from the retail position and rope from the institutional position simultaneously. The false summit risk is the Pokemon Company's scaffold framing — their claim that volatility will naturally sunset as markets mature is aspirational, not structural. They actively maintain scarcity constraints and have incentive structure supporting continued volatility. The scaffold sunset would require them to sacrifice licensing revenue, which the current economic structure prevents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    artificial_scarcity_causality,
    'Is market volatility primarily caused by print-run constraints (supply-side) or by speculative demand cycles (demand-side), or are these mechanistically entangled?',
    'Correlation analysis of print announcements with price spikes; comparison of volatility in heavily-reprinted sets vs limited-run sets; measure price elasticity relative to supply announcements',
    'If supply-driven: Pokemon Company bears responsibility for volatility structure and extraction. If demand-driven: retail collectors'' psychological susceptibility is the primary lever. If entangled: intervention point unclear.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(artificial_scarcity_causality, empirical, 'Whether volatility is supply-driven or demand-driven').

omega_variable(
    grading_service_gatekeeping,
    'Do grading services (PSA, Beckett) provide authentic coordination or do they extract through artificial legitimacy gatekeeping?',
    'Measure authentication accuracy rates; analyze price premiums for graded vs ungraded identical cards; examine grading service fee trajectory relative to market growth',
    'If genuine: grading services are coordination infrastructure. If gatekeeping: their 1-3% fee is extraction masked as quality assurance. Affects classification of grading perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grading_service_gatekeeping, empirical, 'Whether grading services provide coordination or gatekeeping').

omega_variable(
    identity_lock_hypothesis,
    'Are retail collectors trapped by material barriers (liquidity, information costs) or by identity fusion with the collecting hobby?',
    'Qualitative analysis of exit narratives; survey of collector motivations; measure proportion of psychological vs material barriers to exit; track re-entry rates post-exit',
    'If primarily material: ''trapped'' classification appropriate. If primarily identity-based: ''identity_locked'' classification more accurate. Affects exit mechanism for mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_hypothesis, empirical, 'Whether collector exit barriers are material or identity-based').

omega_variable(
    pokemon_company_incentive_misalignment,
    'Does Pokemon Company profit from secondary-market volatility despite public statements about market health?',
    'Measure licensing fee structure relative to secondary market capitalization; track Pokemon Company''s public statements on volatility vs licensing rate changes; analyze whether scarcity-creating decisions correlate with licensing revenue peaks',
    'If Company profits from volatility: they benefit from extraction and suppression increases. If indifferent: volatility is emergent property of licensing model. Determines whether scaffold sunset is real or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pokemon_company_incentive_misalignment, empirical, 'Whether Pokemon Company profit structure incentivizes secondary market volatility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pokemon_card_market_volatility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pkm_tr_t0, pokemon_card_market_volatility, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pkm_tr_t3, pokemon_card_market_volatility, theater_ratio, 3, 0.55).
narrative_ontology:measurement(pkm_tr_t6, pokemon_card_market_volatility, theater_ratio, 6, 0.68).
narrative_ontology:measurement(pkm_tr_t9, pokemon_card_market_volatility, theater_ratio, 9, 0.72).

% Extraction over time
narrative_ontology:measurement(pkm_be_t0, pokemon_card_market_volatility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pkm_be_t3, pokemon_card_market_volatility, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(pkm_be_t6, pokemon_card_market_volatility, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(pkm_be_t9, pokemon_card_market_volatility, base_extractiveness, 9, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pokemon_card_market_volatility, resource_allocation).
narrative_ontology:boltzmann_floor_override(pokemon_card_market_volatility, 0.12).
narrative_ontology:affects_constraint(pokemon_card_market_volatility, speculative_bubble_dynamics).
narrative_ontology:affects_constraint(pokemon_card_market_volatility, authentication_gatekeeping).
narrative_ontology:affects_constraint(pokemon_card_market_volatility, licensing_dependency).

% DUAL FORMULATION NOTE:
% Pokemon card market volatility decomposes into three distinct constraints: (1) market_volatility_mechanism (ε=0.58, tangled rope) — the coordination-extraction hybrid examined here; (2) grading_service_gatekeeping (ε=0.45, tangled rope) — authentication as legitimacy barrier; (3) pokemon_company_supply_control (ε=0.40, tangled rope) — print-run decisions as scarcity lever. All three feed the observed volatility. The primary constraint story addresses the market-level integration; the downstream constraints address specific institutional mechanisms that enable extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pokemon_card_market_volatility, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
