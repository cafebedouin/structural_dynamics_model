% ============================================================================
% CONSTRAINT STORY: market_selection_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_selection_mechanism, []).

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
 *   constraint_id: market_selection_mechanism
 *   human_readable: Market Selection Mechanism: Coordination and Extraction in Competitive Allocation
 *   domain: economic/institutional
 *
 * SUMMARY:
 *   The market selection mechanism describes the constraint structure through
 *   which competitive allocation occurs: agents compete for resources,
 *   capital, and customer attention; differential success concentrates
 *   resources toward high performers; entry and exit determine the
 *   competitive landscape. This constraint operates across all sectors —
 *   consumer goods, technology, finance, energy — and exhibits the full range
 *   of DR types depending on observer position. From the incumbent's
 *   perspective, market selection is genuine coordination: price signals,
 *   quality feedback, and competitive pressure drive efficient allocation.
 *   From the new entrant's perspective, the same mechanism is extraction:
 *   information asymmetries, scale economies, network effects, and switching
 *   costs create insurmountable barriers backed by incumbent defensive
 *   strategies and regulatory theater. The constraint's extractiveness (0.52)
 *   reflects the increasing concentration of capital and strategic advantage
 *   toward incumbents; suppression (0.45) reflects the high barriers to exit
 *   and limited alternatives for capital seekers; theater ratio (0.38)
 *   reflects regulatory frameworks (antitrust law, securities regulation)
 *   that perform market-access commitments while structural barriers persist.
 *   Over the 10-year interval, extractiveness increases (0.38 → 0.52) as
 *   winner-take-most dynamics strengthen and regulatory enforcement degrades
 *   relative to incumbent sophistication.
 *
 * KEY AGENTS:
 *   - New Market Entrant: Primary victim (powerless/trapped) — bears extraction through information asymmetries, capital barriers, and incumbent defensive strategies; cannot exit without abandoning entrepreneurial venture
 *   - Mid-Market Competitor: Secondary victim (moderate/constrained) — faces mixed coordination (market feedback, price discovery) and extraction (switching costs, network effects, scale disadvantages)
 *   - Market Incumbent: Primary beneficiary (institutional/arbitrage) — benefits from network effects, customer lock-in, scale economies, and capital advantage; experiences the mechanism as pure coordination
 *   - Venture Capital Allocator: Secondary beneficiary (powerful/mobile) — coordinates capital deployment to promising ventures while extracting returns through dilution, control, and timing; portfolio diversification provides exit mobility
 *   - Regulatory Market Architecture: Institutional actor (institutional/arbitrage) — maintains performative enforcement of competition law; actual effectiveness at preventing incumbent concentration is degraded relative to stated commitments
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the market selection outcome distribution (Pareto power laws, preferential attachment) as an immutable law rather than recognizing it as an emergent property of extractive institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_selection_mechanism, 0.52).
domain_priors:suppression_score(market_selection_mechanism, 0.45).
domain_priors:theater_ratio(market_selection_mechanism, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_selection_mechanism, extractiveness, 0.52).
narrative_ontology:constraint_metric(market_selection_mechanism, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(market_selection_mechanism, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_selection_mechanism, tangled_rope).
narrative_ontology:human_readable(market_selection_mechanism, "Market Selection Mechanism: Coordination and Extraction in Competitive Allocation").
narrative_ontology:topic_domain(market_selection_mechanism, "economic/institutional").

domain_priors:requires_active_enforcement(market_selection_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_selection_mechanism, incumbent_firms).
narrative_ontology:constraint_beneficiary(market_selection_mechanism, capital_allocators).
narrative_ontology:constraint_victim(market_selection_mechanism, entry_stage_competitors).
narrative_ontology:constraint_victim(market_selection_mechanism, consumer_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW MARKET ENTRANT (SNARE) — Faces maximum extraction through information asymmetries, scale disadvantages, and incumbent defensive strategies. Cannot exit without abandoning the venture; suppressed alternatives force acceptance of extractive terms. Zero degrees of freedom in negotiating access to distribution, capital, or market information.
constraint_indexing:constraint_classification(market_selection_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-MARKET COMPETITOR (TANGLED ROPE) — Experiences genuine coordination benefit (price discovery, customer matching) alongside asymmetric extraction (incumbents use network effects and switching costs to maintain position). Can exit at substantial cost; some coordination functions are real but remain embedded in extractive framework.
constraint_indexing:constraint_classification(market_selection_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MARKET INCUMBENT (ROPE) — Perceives the market selection mechanism as coordination: price signals, quality matching, customer feedback enable efficient allocation. Net beneficiary from the constraint; can maintain position through legitimate competitive advantage or exit if competitive advantage erodes. Extraction is invisible from inside beneficiary position.
constraint_indexing:constraint_classification(market_selection_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: VENTURE CAPITAL ALLOCATOR (TANGLED ROPE) — Coordinates capital allocation to high-potential entrants while extracting returns through dilution, board control, and exit timing. Mobility via diversification across portfolios creates moderate exit option. Both coordination (funding enables innovation) and extraction (asymmetric returns distribution) are structural features.
constraint_indexing:constraint_classification(market_selection_mechanism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY MARKET ARCHITECTURE (PITON) — Antitrust law, securities regulation, and competition policy maintain a performative appearance of enforcing market openness. Actual enforcement is degraded: large incumbents navigate regulation through compliance theater (compliance departments, regulatory filings, public statements of competition commitment) while maintaining structural advantages. The regulatory framework persists through institutional inertia despite declining functional effectiveness at preventing extraction. Theater ratio reflects the gap between stated market-access commitments and actual structural barriers.
constraint_indexing:constraint_classification(market_selection_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, market selection is an immutable consequence of decentralized competition: agents must choose between options, some succeed and some fail, resources concentrate to high-performing incumbents. This appears as an irreducible feature of competitive systems. However, the high extractiveness (0.52), substantial suppression (0.45), and enforced active participation contradict true mountain characteristics. The engine will flag this as a false summit — what appears natural is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(market_selection_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_selection_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_selection_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_selection_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_selection_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_selection_mechanism, TR),
    TR >= 0.70.

:- end_tests(market_selection_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Market selection extracts from entrants through information asymmetries (incumbents know customer preferences, supplier relationships, regulatory pathways), scale advantages (fixed cost distribution, purchasing power), network effects (customer lock-in, platform economics), and switching costs (customer switching costs, switching costs for suppliers and partners). The extraction is moderate rather than severe because some entrants succeed and some markets remain contestable. Suppression (0.45): Barriers to exit include sunk capital costs in market development, customer relationships that are tied to the specific market, and limited alternative uses for specialized knowledge. However, suppression is not total — entrepreneurs can pivot, VC allocators can diversify, competitors can exit for other markets. Theater ratio (0.38): Regulatory frameworks (antitrust law, securities regulation, competition policy) perform commitments to market access and competitive openness while structural barriers persist. The theater is moderate because some regulatory enforcement is effective, but most occurs after concentration has already occurred. Over the interval, extractiveness increases as network effects strengthen (social media, digital platforms), VC concentration increases (megafunds dominating allocation), and winner-take-most dynamics intensify across sectors.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows the maximum possible perspectival divergence. The incumbent sees Rope (pure coordination — customers choose, prices signal, competition drives efficiency). The new entrant sees Snare (pure extraction — barriers prevent entry, incumbent advantage is insurmountable, alternatives are suppressed). The VC allocator sees Tangled Rope (coordination of capital to promising ventures mixed with extraction of outsized returns). The mid-market competitor sees Tangled Rope (both market feedback and incumbent defensive strategies shape their position). The regulatory system sees Piton (antitrust law performs enforcement but structural outcomes show degraded effectiveness). The analytical observer sees Mountain (market selection is an inevitable consequence of decentralized competition and resource scarcity) — but the structural data (high extractiveness, substantial suppression, enforced active participation) contradicts this naturalization. The perspectival gap reveals that 'market efficiency' is not an immutable law but a contested institutional outcome whose classification depends entirely on whose position you occupy.
 *
 * DIRECTIONALITY LOGIC:
 *   The market selection mechanism derives directionality from the structural flow of value. Incumbents are beneficiaries: they have arbitrage exit options (can exit by liquidating, reinvesting in other markets, or maintaining position indefinitely). New entrants are victims: they have trapped exit options (cannot exit without losing invested capital and entrepreneurial effort). Mid-market competitors have constrained exit (can exit at high cost — losing market position, customer relationships, specialized knowledge). VC allocators have mobile exit (portfolio diversification across multiple investments). The engine derives d for each agent from (power, exit_options, beneficiary/victim) tuples: beneficiaries with arbitrage → low d (negative chi experienced as subsidy); victims with trapped → high d (high chi experienced as extraction); moderate agents with constrained → moderate d (mixed chi). The piton classification for the regulatory system derives from theater_ratio (0.38) exceeding the extraction ratio: the regulatory mechanism performs market access more than it delivers market access.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The market selection mechanism resolves the coordination/extraction mandatrophy by showing that both are structurally correct descriptions of the same mechanism from different positions. For incumbents, it is genuine coordination (value flows toward high performers, information gets incorporated into prices, feedback loops drive improvement). For entrants, it is genuine extraction (value concentrates toward incumbents through mechanisms that limit alternatives and lock in disadvantage). The mechanism is BOTH simultaneously because it coordinates resources AND it extracts from those without the resources to compete on equal terms. The regulatory framework's response (antitrust law, securities regulation) attempts to suppress the extractive aspects while maintaining the coordination function — this is why the claimed_type is Tangled Rope, not pure Rope or pure Snare. The Piton classification for the regulatory system indicates that this enforcement has degraded: the theater has increased (compliance departments, regulatory filings) while the actual barrier to extraction has remained stable. The mandatrophy is not resolved by choosing between coordination and extraction but by accepting that markets are hybrid constraints: they coordinate resources AND extract from less-powerful participants, and the balance between these functions is an institutional parameter that can be shifted through regulatory design, information transparency, and access mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'What proportion of market selection extractiveness reflects genuine information asymmetry costs versus deliberate incumbent strategy to suppress competition?',
    'Comparison of entry costs and barriers across markets with different regulatory regimes, incumbent concentration levels, and information transparency; analysis of defensive patent clustering, switching cost engineering, and access restriction patterns',
    'If asymmetry-driven (>60%): classification shifts toward Tangled Rope with lower suppression reading. If strategy-driven (>60%): classification confirms Snare for entrants, Piton for regulatory framework. Affects intervention pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Proportion of extraction due to information asymmetry vs deliberate incumbent strategy').

omega_variable(
    market_selection_efficiency_verification,
    'Does market selection mechanism allocate resources to highest-value uses (true coordination function) or does it concentrate resources to incumbents regardless of relative value creation?',
    'Longitudinal analysis of entry success rates correlated with product quality metrics, customer satisfaction, and innovation output; comparison to counterfactual allocation scenarios; measurement of survivor bias in performance attribution',
    'If truly efficient: classification as Rope or Tangled Rope with lower extractiveness. If rent-based: classification confirms Snare with extraction as primary function. Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_selection_efficiency_verification, empirical, 'Whether market selection allocates to highest-value uses or concentrates to incumbents').

omega_variable(
    exit_cost_trajectory,
    'Are exit costs for trapped entrants declining (market selection improving) or stable/increasing (extraction mechanism deepening)?',
    'Time-series analysis of new entrant failure rates, capital requirements, time-to-profitability, and strategic acquisition patterns; comparison of exit rates pre- and post-regulatory intervention',
    'If declining: constraints may transition toward Scaffold (temporary problem being solved). If stable/increasing: confirms Snare or Piton (extraction or degradation mechanism persisting). Informs sunset clause possibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_trajectory, empirical, 'Trajectory of exit costs for trapped entrants').

omega_variable(
    regulatory_enforcement_effectiveness,
    'Does antitrust enforcement actually constrain incumbent extraction or is it performative theater that leaves structural barriers intact?',
    'Analysis of actual market concentration trends post-enforcement action; measurement of entry rates and competitor survival in concentrated vs competitive markets; assessment of remedies'' effectiveness at reducing barriers',
    'If effective: classification shifts toward Rope (coordination with enforcement). If performative: confirms Piton (degraded regulatory mechanism). Affects interpretation of natural law perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_effectiveness, empirical, 'Whether antitrust enforcement is effective or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_selection_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mktsel_tr_t0, market_selection_mechanism, theater_ratio, 0, 0.28).
narrative_ontology:measurement(mktsel_tr_t5, market_selection_mechanism, theater_ratio, 5, 0.33).
narrative_ontology:measurement(mktsel_tr_t10, market_selection_mechanism, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(mktsel_be_t0, market_selection_mechanism, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mktsel_be_t5, market_selection_mechanism, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(mktsel_be_t10, market_selection_mechanism, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_selection_mechanism, resource_allocation).
narrative_ontology:affects_constraint(market_selection_mechanism, information_asymmetry_in_capital_markets).
narrative_ontology:affects_constraint(market_selection_mechanism, network_effects_and_switching_costs).
narrative_ontology:affects_constraint(market_selection_mechanism, regulatory_capture_in_antitrust).
narrative_ontology:affects_constraint(market_selection_mechanism, venture_capital_concentration).

% DUAL FORMULATION NOTE:
% The market selection mechanism is a constraint family decomposable into several structurally distinct claims: (1) Price discovery and efficient matching (Rope, low ε) — the coordination function; (2) Incumbent advantage and barrier maintenance (Snare/Tangled Rope, moderate-high ε) — the extraction mechanism; (3) Regulatory enforcement effectiveness (Piton, depends on theater_ratio trajectory); (4) VC return concentration (Tangled Rope, moderate ε). Each can be analyzed as a separate constraint with its own perspectives and measurements. This story presents them as an integrated system because they are empirically coupled — market selection cannot be understood without understanding all four components simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_selection_mechanism, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
