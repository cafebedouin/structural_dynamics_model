% ============================================================================
% CONSTRAINT STORY: rare_earth_supply_security
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_supply_security, []).

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
 *   constraint_id: rare_earth_supply_security
 *   human_readable: Rare Earth Supply Security and Strategic Extraction
 *   domain: geopolitical_economics/industrial_policy
 *
 * SUMMARY:
 *   Rare earth elements (REEs) — 17 elements crucial for semiconductors,
 *   permanent magnets, catalysts, and clean energy — are concentrated in both
 *   geological distribution and geopolitical control. China produces ~70% of
 *   global rare earth output and dominates processing, creating a structural
 *   supply bottleneck that enables extraction through both monopoly pricing
 *   and geopolitical leverage. Downstream manufacturers, technology-dependent
 *   economies, and supply-chain resilience bear costs; primary producers and
 *   integrated technology firms capture rents. The constraint exhibits
 *   genuine coordination function (supply-chain planning, long-term contracts
 *   stabilize volatile markets) alongside asymmetric extraction (monopoly
 *   pricing, forced partnerships, geopolitical hostage risk). Diversification
 *   and substitution efforts (alternative mining, recycling infrastructure,
 *   substitute materials) represent a structural sunset mechanism operating
 *   on 15-25 year timescales. Strategic stockpiles and supply-chain
 *   redundancy initiatives persist through institutional inertia despite low
 *   functional use (piton perspective). The extractiveness value (0.62)
 *   reflects that extraction is substantial but mitigated by partial exit
 *   options for powerful actors (technology firms can diversify suppliers at
 *   high cost) and organized responses (coalition-driven alternatives). The
 *   suppression value (0.58) reflects significant barriers (geological
 *   distribution, processing complexity, geopolitical control) that are real
 *   but not absolute — alternatives exist at higher cost/longer timescale.
 *   Theater ratio (0.48) is moderate: some supply-chain activities are
 *   genuinely functional (long-term contracts, inventory management), but
 *   significant performative content exists (strategic announcements of
 *   supply diversification, stockpile buildup with limited drawdown).
 *
 * KEY AGENTS:
 *   - Primary Producing Nation (institutional/arbitrage): Dominant position — China with ~70% global rare earth production and ~95% processing capacity; captures monopoly rents and geopolitical leverage
 *   - Downstream Electronics Manufacturer (powerless/trapped): Small and medium-sized manufacturers dependent on rare earth supply; cannot diversify sources or absorb supply shocks; bears full cost of geopolitical extraction
 *   - Technology-Dependent Economy (moderate/constrained): Developed nations (US, EU, Japan) reliant on rare-earth-dependent technologies but unable to achieve self-sufficiency at acceptable cost; constrained by geopolitical risk and domestic capacity limits
 *   - Strategic Technology Firm (powerful/mobile): Multinational corporations with supply contracts, vertical integration, and supply-chain alternatives; can shift sourcing but absorbs costs; benefits from supply stability through market power
 *   - Diversification and Substitution Coalition (organized/constrained): Mining companies, materials scientists, environmental groups, and policy coalitions driving alternative supply sources and recycling infrastructure; face barriers but see structural exit path
 *   - Strategic Stockpile Infrastructure (institutional/arbitrage): Government-held rare earth reserves maintained through policy inertia; see minimal drawdown and serve primarily as geopolitical signal
 *   - Analytical Observer (analytical/analytical): Recognizes tangled rope structure — genuine coordination function in supply planning alongside geopolitical extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_supply_security, 0.62).
domain_priors:suppression_score(rare_earth_supply_security, 0.58).
domain_priors:theater_ratio(rare_earth_supply_security, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_supply_security, extractiveness, 0.62).
narrative_ontology:constraint_metric(rare_earth_supply_security, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rare_earth_supply_security, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_supply_security, tangled_rope).
narrative_ontology:human_readable(rare_earth_supply_security, "Rare Earth Supply Security and Strategic Extraction").
narrative_ontology:topic_domain(rare_earth_supply_security, "geopolitical_economics/industrial_policy").

domain_priors:requires_active_enforcement(rare_earth_supply_security).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_supply_security, primary_producing_nations).
narrative_ontology:constraint_beneficiary(rare_earth_supply_security, strategic_technology_firms).
narrative_ontology:constraint_victim(rare_earth_supply_security, downstream_manufacturers).
narrative_ontology:constraint_victim(rare_earth_supply_security, technology_dependent_economies).
narrative_ontology:constraint_victim(rare_earth_supply_security, supply_chain_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM MANUFACTURER (SNARE) — Trapped by physical supply concentration and processing bottlenecks. Cannot diversify sources or develop alternatives at cost-competitive scales. Bears full extraction: supply security premiums, long lead times, geopolitical hostage risk, forced technology transfer, or margin compression from supply shocks.
constraint_indexing:constraint_classification(rare_earth_supply_security, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TECHNOLOGY-DEPENDENT ECONOMY (TANGLED ROPE) — Benefits from access to rare earth dependent technologies (semiconductors, renewable energy, defense systems) but is constrained by geopolitical supply risk and domestic capacity bottlenecks. Extraction takes form of technology licensing restrictions, forced partnerships with supply-chain-controlling entities, and macroeconomic vulnerability to supply-driven price shocks. Genuine coordination function exists (supply allocation) alongside asymmetric extraction.
constraint_indexing:constraint_classification(rare_earth_supply_security, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STRATEGIC TECHNOLOGY FIRM (TANGLED ROPE) — Powerful multinational with mobile options (alternative suppliers, supply contracts, stockpiling) but also benefits from stable rare earth access for competitive advantage. Experiences extraction as supply-price volatility and geopolitical leverage over technology choices. Coordination function: supply-chain planning and long-term contracts reduce volatility for all parties. Asymmetric extraction: firms with supply contracts extract value from those without.
constraint_indexing:constraint_classification(rare_earth_supply_security, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIMARY PRODUCING NATION (ROPE) — Dominant rare earth producer (e.g., China with ~70% global production) experiences this constraint as pure coordination with embedded economic rent. No extraction experienced — instead, captures scarcity rent from having monopoly over supply. Benefits from supply security premium, technology partnership leverage, and geopolitical influence. Exit option is arbitrage: can shift production/pricing freely.
constraint_indexing:constraint_classification(rare_earth_supply_security, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DIVERSIFICATION COALITION (SCAFFOLD) — Organized effort (mining companies, materials scientists, policy coalitions) to develop alternative supply sources, substitute materials, and recycling infrastructure. Sees rare earth constraint as temporary coordination failure with sunset clause. Suppression (geopolitical barriers, processing complexity) remains high but declining as alternatives mature. Estimated sunset: 15-25 years for mature recycling and alternative supply infrastructure.
constraint_indexing:constraint_classification(rare_earth_supply_security, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: STRATEGIC STOCKPILE INFRASTRUCTURE (PITON) — Maintains national rare earth reserves (US, Japan, EU) through policy inertia despite low functional use. Theater ratio high: stockpiles exist primarily as geopolitical signal and backup mechanism but see minimal actual drawdown. Institutional maintenance persists because alternatives haven't fully replaced the backup function, not because the stockpiles actively solve the supply problem. Supports piton classification.
constraint_indexing:constraint_classification(rare_earth_supply_security, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Views the constraint as a hybrid: genuine coordination problem (supply concentration enables economies of scale in processing) combined with geopolitical extraction mechanism (monopoly power capturing rent and enabling leverage). Extractiveness reflects both the physical bottleneck and the institutional choices that maintain concentration.
constraint_indexing:constraint_classification(rare_earth_supply_security, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_supply_security_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_supply_security, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_supply_security, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_earth_supply_security, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rare_earth_supply_security, TR),
    TR >= 0.70.

:- end_tests(rare_earth_supply_security_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High but not severe. The constraint extracts through monopoly pricing, geopolitical leverage, and supply-chain control. However, extractiveness is mitigated by: (1) partial exit options for powerful actors (technology firms can diversify suppliers at cost), (2) organized coalition efforts reducing barriers, (3) long-term contracts stabilizing some of the volatility. If China were able to impose absolute chokehold (100% control, no alternatives, no diversification), extractiveness would approach 0.80+. Current 0.62 reflects that the extraction mechanism has real constraints and facing organized resistance. Suppression (0.58): Moderate-high. Physical barriers (geological concentration, processing complexity, rare earth element chemistry) are real and substantial. Geopolitical barriers (supply control, technology partnerships, export restrictions) add significant suppression. However, suppression is not total — alternative suppliers are developing (US, Myanmar, Vietnam), recycling infrastructure exists (though at small scale), and material substitutes are being researched. Theater ratio (0.48): Low-moderate. Unlike strategic stockpiles (which are mostly performative), the actual supply-chain coordination activities in rare earth markets are substantially functional. Long-term contracts genuinely reduce volatility. Inventory management has real effects. Processing investments create real bottleneck relief. But significant theater exists: announcements of supply diversification are often less than claimed, stockpile buildups signal capability more than utility, and recycling investments proceed slowly despite policy support.
 *
 * PERSPECTIVAL GAP:
 *   Primary producer sees rope (coordination + rent capture). Downstream manufacturer sees snare (pure extraction, no exit). Moderate economy sees tangled rope (mixed coordination/extraction). Coalition sees scaffold (temporary with sunset). Stockpile sees piton (degraded). Technology firm sees tangled rope (mixed). Analytical observer sees tangled rope (genuine coordination + extraction). The perspectival spread demonstrates that the constraint cannot be reduced to a single type — it IS a presheaf, and the different positions are all accurate. The false summit risk is in the stockpile piton classification and the rope classification of the producer — both may naturalize what is actually a contingent institutional arrangement (monopoly control, processing concentration). The mountain risk is naturalizing the supply bottleneck as an inevitable feature of rare earth chemistry rather than recognizing it as a product of extraction-driven consolidation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Primary producer (institutional/arbitrage) → beneficiary status + arbitrage exit → d ≈ 0.15 (low). Downstream manufacturer (powerless/trapped) → victim status + trapped exit → d ≈ 0.95 (high). Technology firm (powerful/mobile) → mixed beneficiary/victim + mobile exit → d ≈ 0.55 (moderate). Coalition (organized/constrained) → victim of supply problem + constrained exit → d ≈ 0.45 (moderate). Stockpile (institutional/arbitrage) → beneficiary in policy space + institutional arbitrage → d ≈ 0.18 (low). Analytical (analytical/analytical) → observer position, canonical d ≈ 0.72. Beneficiary declarations (primary_producing_nations, strategic_technology_firms) receive low d consistent with arbitrage/powerful positions. Victim declarations (downstream_manufacturers, technology_dependent_economies, supply_chain_resilience) receive high d consistent with trapped/constrained positions. The engine derives f(d) from these values and applies scope modifier σ(S) — global scope uses σ = 1.2, amplifying χ for all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by disambiguating through institutional decomposition: (1) Supply concentration is real (geological/economic fact), (2) Geopolitical control is contingent (institutional choice to maintain monopoly), (3) Coordination function is real (contracts do reduce volatility), (4) Extraction is real (monopoly pricing is not inevitable). The tangled rope classification holds because BOTH the coordination function AND the asymmetric extraction are structural — neither dominates the other. The rope classification from the producer's perspective is accurate (they genuinely solve a coordination problem) but risks naturalizing the monopoly position. The snare classification from the trapped manufacturer is accurate (they experience pure extraction at their point of contact) but partial — the global structure is tangled rope, not pure snare. The scaffold classification is structural and predictive — the sunset mechanism is real (diversification, recycling, substitution) and operating, though slow. The piton classification of stockpiles correctly identifies degraded function maintained through inertia. The analytical observer sees the tangled rope structure and avoids the naturalizing traps.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    processing_bottleneck_vs_mining_bottleneck,
    'Is the binding constraint mining capacity or processing/refining capacity?',
    'Supply-chain decomposition: separate mining and processing metrics; track which segment has excess capacity and which has shortages; correlate with price spikes and allocation decisions',
    'If mining bottleneck: extractiveness driven by geology; constraint may be mountain-like with looser supply path. If processing bottleneck: extractiveness driven by infrastructure investment choices; supply concentration is contingent, and substitution/recycling become more viable alternatives. Dramatically changes sunset timeline and extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(processing_bottleneck_vs_mining_bottleneck, empirical, 'Whether binding constraint is mining or processing capacity').

omega_variable(
    geopolitical_extraction_mechanism,
    'How much of the measured extractiveness reflects deliberate geopolitical leverage versus passive market concentration?',
    'Historical analysis of supply behavior during periods of high vs low geopolitical tension; identification of allocation decisions that favor political allies; comparison of terms offered to different buyers',
    'If primarily geopolitical: extractiveness is contingent and can be reduced by diversification. If primarily passive market concentration: extractiveness persists even without active leverage — classic monopolistic extraction. Classification remains tangled_rope either way, but the sunset mechanisms differ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_extraction_mechanism, empirical, 'Whether extraction mechanism is deliberate geopolitical leverage or passive monopoly').

omega_variable(
    substitution_feasibility,
    'For critical applications (semiconductors, permanent magnets, green energy), do technical substitutes actually exist, or does ''substitution'' mean accepting significant performance/cost degradation?',
    'Technical review of alternative materials for each rare-earth application; performance gaps vs rare-earth baselines; cost competitiveness at scale; timeline to mature substitutes',
    'If true substitutes exist: exit option for trapped agents improves; constraint duration shortens; extraction mechanisms weaken. If substitutes degrade performance: trapped agents remain trapped; constraint persists despite diversification efforts. Shifts scaffold sunset timeline from 15-25 years to 30+ years or renders it aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_feasibility, empirical, 'Whether viable technical substitutes exist for critical rare earth applications').

omega_variable(
    recycling_scale_asymptote,
    'Can closed-loop recycling scale to meet 50%+ of demand, or does thermodynamic/economic dispersion prevent recovery above 20-30%?',
    'Lifecycle analysis of rare earth recovery from discarded electronics; economic modeling of recycling infrastructure at scale; empirical tracking of recycling rates by rare earth element',
    'If recycling scales to 50%+: supply constraint is substantially weakened; manufacturing-side responsibility becomes viable. If asymptotes at 20-30%: mining diversification remains critical; extraction persists longer; scaffold sunset is later than 25 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recycling_scale_asymptote, empirical, 'Maximum realistic proportion of demand that can be met by recycling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_supply_security, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ares_tr_t0, rare_earth_supply_security, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ares_tr_t10, rare_earth_supply_security, theater_ratio, 10, 0.42).
narrative_ontology:measurement(ares_tr_t20, rare_earth_supply_security, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(ares_be_t0, rare_earth_supply_security, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ares_be_t10, rare_earth_supply_security, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(ares_be_t20, rare_earth_supply_security, base_extractiveness, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_supply_security, resource_allocation).
narrative_ontology:affects_constraint(rare_earth_supply_security, semiconductor_supply_resilience).
narrative_ontology:affects_constraint(rare_earth_supply_security, renewable_energy_technology_access).
narrative_ontology:affects_constraint(rare_earth_supply_security, permanent_magnet_manufacturing).
narrative_ontology:affects_constraint(rare_earth_supply_security, defense_technology_supply_chain).

% DUAL FORMULATION NOTE:
% Rare earth supply security operates at the intersection of geology (resource distribution), economics (processing infrastructure), and geopolitics (control and leverage). This story models the integrated constraint. Downstream constraints (semiconductor supply, renewable energy access, defense supply chains) each depend on rare earth security but have their own distinct ε values reflecting their specific substitution options, supply diversification, and geopolitical vulnerability. Rare earth supply constraint is upstream to all; upstream constraints propagate downward but do not fully determine their classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rare_earth_supply_security, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
