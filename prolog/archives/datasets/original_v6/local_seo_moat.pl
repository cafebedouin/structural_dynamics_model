% ============================================================================
% CONSTRAINT STORY: local_seo_moat
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_local_seo_moat, []).

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
 *   constraint_id: local_seo_moat
 *   human_readable: Local SEO Moat: Information Asymmetry and Ranking Extraction
 *   domain: digital_markets/search_engine_optimization
 *
 * SUMMARY:
 *   The local SEO moat describes the structural extraction mechanism created
 *   by opaque search ranking algorithms that govern customer discovery in
 *   local markets. Established businesses accumulate ranking signals
 *   (reviews, citations, link authority) that create compounding advantage,
 *   while new entrants face high barriers to acquiring visibility without
 *   years of optimization effort or paid advertising spend. The constraint
 *   exhibits hybrid characteristics: it provides genuine coordination
 *   (customers find relevant local businesses through search) alongside pure
 *   extraction (ranking opacity and information asymmetry concentrate
 *   discovery power). The moat has strengthened over the measurement interval
 *   (extractiveness rising from 0.35 to 0.58) as SEO has matured from a novel
 *   advantage to a mandatory operational expense, and as algorithm complexity
 *   has increased suppression of alternative discovery mechanisms. The
 *   theater ratio has also risen (0.28 to 0.48), reflecting increasing
 *   prevalence of artificial signal creation (citation farms, schema markup
 *   gaming, review harvesting) that mimics genuine business quality without
 *   substance.
 *
 * KEY AGENTS:
 *   - New Local Business Entrants: Primary victim (powerless/trapped) — face years-long barrier to acquiring search visibility without substantial financial investment or insider SEO knowledge
 *   - Established Local Businesses: Secondary beneficiary (moderate/constrained) — benefit from accumulated ranking signals but must continuously optimize to maintain competitive position against algorithm changes
 *   - SEO Service Providers: Primary beneficiary (institutional/arbitrage) — capture rents from information asymmetry between businesses and search algorithms; provide genuine optimization services while maintaining expertise moat
 *   - Search Engine Operator: Structural coordinator (institutional/mobile) — maintains ranking algorithm as customer-discovery mechanism while monetizing through advertising concentration; has mobile exit to competing platforms but network effects create friction
 *   - Consumers: Secondary victim (powerless/trapped) — reduced access to complete local business information; discovery filtered through opaque ranking criteria; suppressed alternative discovery channels
 *   - Regulatory/Transparency Advocates: Organized actor (organized/mobile) — push for algorithmic transparency and alternative discovery mechanisms; see moat as contingent institutional arrangement rather than natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(local_seo_moat, 0.58).
domain_priors:suppression_score(local_seo_moat, 0.65).
domain_priors:theater_ratio(local_seo_moat, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(local_seo_moat, extractiveness, 0.58).
narrative_ontology:constraint_metric(local_seo_moat, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(local_seo_moat, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(local_seo_moat, tangled_rope).
narrative_ontology:human_readable(local_seo_moat, "Local SEO Moat: Information Asymmetry and Ranking Extraction").
narrative_ontology:topic_domain(local_seo_moat, "digital_markets/search_engine_optimization").

domain_priors:requires_active_enforcement(local_seo_moat).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(local_seo_moat, established_local_businesses).
narrative_ontology:constraint_beneficiary(local_seo_moat, seo_service_providers).
narrative_ontology:constraint_victim(local_seo_moat, new_market_entrants).
narrative_ontology:constraint_victim(local_seo_moat, local_market_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW LOCAL BUSINESS ENTRANT (SNARE) — Trapped by opaque ranking algorithms and high barriers to acquiring local search visibility. No exit from the search monopoly without years of accumulated signals (reviews, citations, link building). Zero alternatives for customer discovery in local markets. Maximum experienced extraction.
constraint_indexing:constraint_classification(local_seo_moat, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ESTABLISHED LOCAL BUSINESS (TANGLED ROPE) — Benefits from accumulated ranking signals and network effects (reviews, citations, brand recognition), but also constrained by ongoing algorithm changes and the need to maintain competitive positioning. Experiences both coordination (customer search is efficient) and extraction (must continuously optimize to retain visibility).
constraint_indexing:constraint_classification(local_seo_moat, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: SEO SERVICE INDUSTRY (ROPE) — Benefits from information asymmetry between businesses and search algorithms. Provides genuine coordination service (helping businesses reach customers) while capturing rents through expertise and access. Primary beneficiary with maximum arbitrage — can exit and serve other markets or pivot to new algorithmic requirements.
constraint_indexing:constraint_classification(local_seo_moat, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SEARCH ENGINE OPERATOR (ROPE) — Maintains ranking algorithm as coordination mechanism connecting searchers to relevant local businesses. Algorithm opacity serves platform business model (advertising concentration) but enables genuine discovery coordination. Mobile exit exists through competing search platforms, but network effects create substantial friction.
constraint_indexing:constraint_classification(local_seo_moat, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSUMER ADVOCATES (TANGLED ROPE) — Organized actors (consumer protection agencies, transparency advocates) see the local SEO moat as both coordination failure (information asymmetry prevents efficient markets) and extraction mechanism (ranking opacity concentrates discovery power). Mobile exit through advocacy for algorithmic transparency and alternative discovery platforms. Mixed classification reflects genuine coordination need alongside real extraction.
constraint_indexing:constraint_classification(local_seo_moat, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL LOCAL DISCOVERY (PITON) — Yellow Pages, local print directories, and traditional business listings were once the primary local discovery mechanism. SEO and search ranking replaced these institutions but maintain the same information asymmetry logic. Theater ratio (0.48) reflects that local SEO optimization is increasingly performative — meta-tag games, citation harvesting, and schema markup are ritual activities mimicking genuine business quality signals. Original function (connecting searchers to relevant businesses) has degraded into ranking theater.
constraint_indexing:constraint_classification(local_seo_moat, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, information asymmetry between search operator and market participants is an irreducible feature of centralized discovery platforms. Any algorithm producing a ranked list must use hidden criteria; perfect transparency would eliminate the differentiation mechanism. This perspective risks naturalizing a contingent institutional choice (centralized search ranking) as an immutable law. Engine false summit detection applies.
constraint_indexing:constraint_classification(local_seo_moat, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(local_seo_moat_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(local_seo_moat, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(local_seo_moat, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(local_seo_moat, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(local_seo_moat, TR),
    TR >= 0.70.

:- end_tests(local_seo_moat_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The moat extracts from new entrants and consumers but provides genuine customer discovery coordination. The extraction is not maximal (0.70+) because established businesses and SEO providers also bear costs (continuous algorithm adaptation, platform dependency, paid advertising necessity). The trajectory from 0.35 to 0.58 reflects SEO maturation: what was once a differentiating advantage has become mandatory operational cost. Suppression (0.65): High. Multiple barriers prevent exit: search platform monopoly in customer discovery, algorithm opacity, high switching costs for businesses with accumulated ranking signals, network effects favoring established results, and suppression of alternative discovery platforms (reduced visibility for business directories, local listings, community recommendations). However, suppression is not total (0.85+) because some exit options exist: local advertising, direct customer relationships, alternative platforms (review sites, social media, SMS, email), and geographic arbitrage. Theater ratio (0.48): Moderate, rising over time. Initial local SEO work (site optimization, business listing completion, genuine review generation) produced real ranking signals. Current SEO practice increasingly involves artificial signal creation: citation harvesting from automated aggregators, schema markup inflation, review incentivization programs, link purchasing, and content factories. The theater has increased because algorithmic complexity has outpaced the legitimate signals available from small businesses.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals that no single type accurately captures the constraint's structural nature. The snare perspective (new entrants) shows maximum extraction without benefit. The rope perspectives (operators, providers) show coordination with arbitrage capture. The tangled rope perspectives (businesses, advocates) show genuine coordination compromised by asymmetric extraction. The piton perspective (traditional discovery) shows degraded function maintained through inertia. The mountain perspective (naturalizing opacity) is a false summit: algorithm opacity is a policy choice, not a law of nature. The gap between these readings reveals the moat's structural nature: it is a tangled rope (mixed coordination and extraction) maintained by institutional choices (platform control, algorithm opacity, suppression of alternatives) that could be changed through regulation or competitive disruption, but are naturalized as inevitable features of search technology.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from their structural position relative to the extraction flow. New entrants (victim + trapped) experience maximum d ≈ 0.95, producing high effective extraction chi. Established businesses (mixed beneficiary/victim + constrained) experience moderate d ≈ 0.55, balancing coordination benefit against extraction constraint. SEO providers (beneficiary + arbitrage) experience low d ≈ 0.15, producing negative chi — they experience the constraint as pure coordination opportunity. The search operator (beneficiary + mobile) experiences d ≈ 0.20, producing slightly negative chi — they solve a real problem while monetizing. Transparency advocates (organized + mobile) experience moderate d ≈ 0.65, seeing both genuine coordination and extractive mechanism. The piton perspective derives d from institutional/arbitrage baseline (≈0.20) but theater_ratio dominance triggers piton classification. The mountain perspective risks d ≈ 0.73 (analytical) but false summit detection identifies naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy by distinguishing between the genuine coordination function (customer discovery in fragmented local markets requires some aggregation and ranking) and the extractive mechanism (algorithm opacity, ranking signal gaming, suppression of alternative discovery channels). The genuine function produces ε_coordination ≈ 0.20–0.30 (customers find relevant businesses efficiently). The extraction mechanism adds ε_extraction ≈ 0.30–0.35 (ranking opacity and gaming impose costs on market participants). Total ε ≈ 0.58 reflects this hybrid. A purely extracted version (snare, ε ≈ 0.75+) would require algorithm opacity and ranking barriers to provide zero coordination benefit. A purely coordinated version (rope, ε ≈ 0.15–0.35) would require algorithmic transparency and minimal barrier to new entrant visibility. The measured constraint is genuinely between these extremes: it coordinates customer discovery (benefit to consumers and established businesses) while extracting rents (cost to new entrants and SEO service dependence). Mandatrophy is resolved by accepting that both characterizations are correct from their respective structural positions — the constraint is simultaneously rope (from beneficiary view), snare (from victim view), tangled rope (from moderate mixed position), and piton (when theater dominates function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithm_transparency_threshold,
    'What level of algorithmic transparency would eliminate the moat without destroying search platform viability?',
    'Comparative analysis of transparency levels (no disclosure vs. factor weighting vs. full algorithm publication) against search quality metrics and platform business model sustainability',
    'If threshold exists below full disclosure: moat is contingent and could be reduced through regulation. If no viable threshold: moat is inherent to platform economics and approaches mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_transparency_threshold, empirical, 'Algorithmic transparency threshold for moat elimination').

omega_variable(
    alternative_discovery_platform_viability,
    'Can decentralized or distributed search discovery platforms (blockchain-based, federated, community-maintained) achieve comparable discovery efficiency to centralized ranking?',
    'Pilot systems testing: comparison of search result relevance, user satisfaction, and network effects between centralized and distributed models',
    'If viable alternatives exist: moat is contingent on platform dominance and could be eliminated through market entry. If alternatives structurally inferior: moat approaches mountain classification as an inherent feature of discovery coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_discovery_platform_viability, empirical, 'Whether decentralized discovery platforms can replace centralized search').

omega_variable(
    citation_harvesting_gaming_measurement,
    'What proportion of local SEO optimization effort produces artificial ranking signals versus genuine business quality signals?',
    'Correlation analysis between SEO optimization activities (citation volume, schema markup, link building) and actual customer satisfaction, business quality, and market outcomes',
    'If high proportion artificial: theater ratio understated and piton classification strengthens. If high proportion genuine: tangled rope classification correct and coordination function substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citation_harvesting_gaming_measurement, empirical, 'Proportion of SEO effort that is gaming versus genuine signal').

omega_variable(
    regulatory_intervention_feasibility,
    'Can regulation mandating algorithmic transparency or alternative discovery mechanisms achieve enforcement without migration to unregulated platforms?',
    'Cross-jurisdiction policy analysis; tracking of search platform behavior under transparency requirements (GDPR, proposed DSA provisions) and user/business migration patterns',
    'If enforcement successful: moat is regulatory contingency and can be eliminated. If platforms migrate: moat persists through jurisdictional arbitrage and cannot be eliminated through single-jurisdiction intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_intervention_feasibility, empirical, 'Feasibility of regulatory elimination of the moat').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(local_seo_moat, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(localseo_tr_t0, local_seo_moat, theater_ratio, 0, 0.28).
narrative_ontology:measurement(localseo_tr_t5, local_seo_moat, theater_ratio, 5, 0.38).
narrative_ontology:measurement(localseo_tr_t10, local_seo_moat, theater_ratio, 10, 0.48).
narrative_ontology:measurement(localseo_tr_t15, local_seo_moat, theater_ratio, 15, 0.52).

% Extraction over time
narrative_ontology:measurement(localseo_be_t0, local_seo_moat, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(localseo_be_t5, local_seo_moat, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(localseo_be_t10, local_seo_moat, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(localseo_be_t15, local_seo_moat, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(local_seo_moat, information_standard).
narrative_ontology:affects_constraint(local_seo_moat, search_platform_advertising_concentration).
narrative_ontology:affects_constraint(local_seo_moat, review_platform_authenticity_moat).

% DUAL FORMULATION NOTE:
% The local SEO moat is downstream of search platform monopoly in customer discovery and upstream of specific business category extraction constraints (e.g., restaurant review gaming, medical practice ranking). The moat creates enabling conditions for secondary extraction mechanisms in high-value local categories (legal services, healthcare, finance) where review authenticity and ranking visibility directly affect consumer choice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(local_seo_moat, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
