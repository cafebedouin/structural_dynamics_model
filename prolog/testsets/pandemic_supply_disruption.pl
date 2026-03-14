% ============================================================================
% CONSTRAINT STORY: pandemic_supply_disruption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pandemic_supply_disruption, []).

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
 *   constraint_id: pandemic_supply_disruption
 *   human_readable: Pandemic Supply Chain Disruption and Extraction
 *   domain: economic/logistics/public_health
 *
 * SUMMARY:
 *   The pandemic supply disruption represents a structural clash between
 *   just-in-time manufacturing efficiency and distributed resilience,
 *   activated by crisis. The constraint exhibits a classic Tangled Rope
 *   signature: legitimate coordination function (consolidating fragmented
 *   suppliers reduces transaction costs and enables rapid crisis
 *   mobilization) layered with asymmetric extraction (consolidators capture
 *   disproportionate benefits through price leverage, priority allocation,
 *   and market consolidation). The same institutional mechanism — supply
 *   chain consolidation — appears beneficial from the consolidator's
 *   perspective (reducing friction, enabling mobilization) and extractive
 *   from the trapped supplier's perspective (eliminated alternatives, forced
 *   price acceptance, allocation subordination). Extractiveness peaks at 2-4
 *   years into the crisis as consolidation solidifies; theater increases as
 *   firms perform compliance with efficiency metrics while building secret
 *   buffer inventory. The constraint exhibits path-dependent dynamics:
 *   initial disruption is temporary (crisis-driven), but consolidation of
 *   supplier base into fewer players creates durable extraction mechanism
 *   that persists even as supply normalizes. Suppression is high (0.68)
 *   because barriers include geography, capital constraints, regulatory
 *   restrictions, and reputational damage from non-delivery — these are
 *   structural rather than temporary. Small manufacturers face forced choice
 *   between accepting unfavorable consolidator terms or exiting market
 *   entirely.
 *
 * KEY AGENTS:
 *   - Small Manufacturers: Primary victims (powerless/trapped) — dependent on consolidated supply chains, face allocation subordination and forced price acceptance
 *   - Developing Economy Suppliers: Primary victims (powerless/trapped) — geographically dependent, lack alternative export markets, forced into price cuts despite input costs
 *   - Emergency Sector Workers: Secondary victims (powerless/trapped) — trapped by duty, forced to work with inadequate supplies, no exit option
 *   - Large Integrated Manufacturers: Primary beneficiary (powerful/mobile) — benefits from consolidation, captures market share from failed competitors, gains strategic supplier control
 *   - Supply Chain Consolidators: Primary beneficiary (institutional/arbitrage) — profit from intermediation spreads, control allocation, exit via switching to post-pandemic logistics
 *   - Supply Resilience Initiative: Organized actor (organized/constrained) — government and cooperative responses building alternative supply structures with sunset trajectory
 *   - Public Health Authority: Secondary beneficiary-victim (institutional/constrained) — gains coordination authority but loses autonomy, forced to accept consolidator pricing and allocation
 *   - Just-in-Time Doctrine: Institutional narrative (institutional/arbitrage) — maintained through consultant capture and inertia despite demonstrated failure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy equilibrium as law of global economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pandemic_supply_disruption, 0.52).
domain_priors:suppression_score(pandemic_supply_disruption, 0.68).
domain_priors:theater_ratio(pandemic_supply_disruption, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pandemic_supply_disruption, extractiveness, 0.52).
narrative_ontology:constraint_metric(pandemic_supply_disruption, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(pandemic_supply_disruption, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pandemic_supply_disruption, tangled_rope).
narrative_ontology:human_readable(pandemic_supply_disruption, "Pandemic Supply Chain Disruption and Extraction").
narrative_ontology:topic_domain(pandemic_supply_disruption, "economic/logistics/public_health").

domain_priors:requires_active_enforcement(pandemic_supply_disruption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pandemic_supply_disruption, large_integrated_manufacturers).
narrative_ontology:constraint_beneficiary(pandemic_supply_disruption, supply_chain_consolidators).
narrative_ontology:constraint_beneficiary(pandemic_supply_disruption, logistics_intermediaries).
narrative_ontology:constraint_victim(pandemic_supply_disruption, small_manufacturers).
narrative_ontology:constraint_victim(pandemic_supply_disruption, developing_economy_suppliers).
narrative_ontology:constraint_victim(pandemic_supply_disruption, end_consumers).
narrative_ontology:constraint_victim(pandemic_supply_disruption, emergency_sector_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT SMALL MANUFACTURER (SNARE) — Trapped in supply dependency with no alternative sourcing. Bears full extraction cost through forced price increases, allocation priority to larger buyers, and inability to secure inventory. Cannot exit manufacturing sector or secure alternative supply without destroying production capacity. Maximum experienced extraction.
constraint_indexing:constraint_classification(pandemic_supply_disruption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING ECONOMY SUPPLIER (SNARE) — Geographically trapped by lack of alternative export markets and capital to diversify. Faces forced price cuts from logistics consolidators despite input cost increases. Cannot exit the supply chain structure or renegotiate terms without losing only reliable revenue source. Suppression includes currency devaluation, border closure, and reputational damage from non-delivery.
constraint_indexing:constraint_classification(pandemic_supply_disruption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: EMERGENCY SECTOR WORKER (SNARE) — Healthcare and essential workers trapped in supply shortage. Forced to work with inadequate PPE, equipment, and medication supplies during peak demand. No exit option: cannot abandon sector during crisis without abandoning duty. Suppression includes job security threat, regulatory pressure, and moral coercion.
constraint_indexing:constraint_classification(pandemic_supply_disruption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE INTEGRATED MANUFACTURER (TANGLED ROPE) — Genuine coordination function: consolidating fragmented suppliers reduces transaction costs and enables faster mobilization during crisis. But also captures extraction benefit through priority allocation, price leverage, and market consolidation. Benefits from crisis shock that accelerates supplier consolidation. Mobile exit option but benefits from constraint persistence.
constraint_indexing:constraint_classification(pandemic_supply_disruption, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SUPPLY CHAIN CONSOLIDATOR (ROPE) — Net beneficiary with arbitrage exit. Coordination function: acts as broker reducing information asymmetry between suppliers and manufacturers. Captures value spread through inventory control and timing arbitrage. Can exit constraint by switching to non-pandemic logistics or long-term contracts. Experiences constraint as profitable coordination.
constraint_indexing:constraint_classification(pandemic_supply_disruption, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SUPPLY RESILIENCE INITIATIVE (SCAFFOLD) — Organized response (government procurement standards, supplier diversity mandates, regional stockpiling) creates temporary coordination with sunset logic. Crisis drives adoption of redundancy, localization, and decentralization. As these mature, the constraint's extraction mechanism weakens — distributed inventory and backup suppliers reduce consolidator leverage. Estimated sunset: 5-10 years for alternative supply architecture to mature.
constraint_indexing:constraint_classification(pandemic_supply_disruption, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PUBLIC HEALTH AUTHORITY (TANGLED ROPE) — Constrained by reliance on private supply chains for emergency goods. Genuine coordination need: mobilizing distributed supply requires institutional coordination. But authority also bears extraction cost: must accept inflated prices, accept allocation triage, lose autonomy over supply decisions. Requires active enforcement of allocation directives against consolidator preference for profit-maximizing distribution.
constraint_indexing:constraint_classification(pandemic_supply_disruption, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: JUST-IN-TIME DOCTRINE (PITON) — Supply-chain ideology persists despite demonstrated failure. Theater: firms continue JIT performance metrics and consultant advocacy despite knowing fragility. The constraint is maintained through institutional inertia and consultant capture, not because JIT works during disruption. Theater_ratio high: firms perform compliance with efficiency metrics even as they secretly build emergency buffer stock.
constraint_indexing:constraint_classification(pandemic_supply_disruption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of misclassifying the disruption as immutable: 'global supply chains are inherently fragile, pandemics necessarily cause shortage.' Risks naturalizing what is actually a contingent choice between JIT efficiency and distributed resilience. The constraint appears as a natural law of globalization rather than as a policy equilibrium that could be restructured.
constraint_indexing:constraint_classification(pandemic_supply_disruption, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pandemic_supply_disruption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pandemic_supply_disruption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pandemic_supply_disruption, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pandemic_supply_disruption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pandemic_supply_disruption, TR),
    TR >= 0.70.

:- end_tests(pandemic_supply_disruption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The pandemic initially creates legitimate scarcity rents that consolidators capture. But measurements reveal that extractiveness peaks at 2-4 years (0.52) and declines toward 0.48 by year 6, indicating that acute disruption (legitimate coordination problem) moderates as supply normalizes. However, consolidation persists — the constraint shifts from acute crisis to structural lock-in. The 0.52 value reflects that extractiveness is genuine but not maximal: large manufacturers also bear supply costs and visibility losses; consolidators face reputational exposure and regulatory scrutiny. Suppression (0.68): Moderate-high and structural. Small suppliers face geographic lock-in, capital constraints preventing alternative sourcing, regulatory barriers to import diversification, and reputational/financial penalty for non-delivery. Developing suppliers face currency devaluation and market concentration. These barriers are not temporary crisis effects but structural features of the global supply architecture. Theater ratio (0.55): Moderate. Supply-chain management exhibits significant performative content: firms maintain JIT efficiency metrics and performance bonuses while secretly building emergency buffer stock. Consultants advocate efficiency despite knowing fragility. But theater is not dominant — actual supply decisions do reflect efficiency-resilience tradeoff, not pure performance for appearances. Measurements show theater increasing from 0.35 to 0.58 as crisis becomes normalized and firms internalize conflicting signals.
 *
 * PERSPECTIVAL GAP:
 *   The most diagnostic gap is between consolidators (Rope/Tangled Rope) and trapped suppliers (Snare). Consolidators perceive the constraint as beneficial coordination with manageable extraction. Trapped suppliers perceive it as pure extraction with forced submission. Both are measuring the same structural constraint, but their position within it (beneficiary with exit vs victim without exit) produces incompatible classifications. The organized actors (Scaffold) perspective provides the key diagnostic: they see a sunset path to restructured supply (decentralization, redundancy, regionalization). The piton perspective reveals the constraint's persistence mechanism: just-in-time ideology maintains JIT metrics performatively even as they prove false — the doctrine persists through consultant capture, not empirical success. The analytical mountain perspective risks false summit: naturalizing the efficiency-resilience tradeoff as law of economics rather than as policy choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Consolidators have high exit mobility (arbitrage option) and benefit from the constraint — their directionality d is low (approximately 0.15-0.25), producing negative or very low f(d) → low experienced extraction χ. They experience the constraint as coordination opportunity (Rope). Small manufacturers have zero effective exit (trapped) and bear all extraction costs — their d is high (approximately 0.90-0.95), producing maximum f(d) → maximum χ. They experience constraint as Snare. Developing suppliers have geographic/capital traps limiting exit (d ≈ 0.85) but some mobility through alternative industries — they experience Snare with slightly less total extraction than domestic small manufacturers. Organized actors (resilience initiatives) have constrained exit (0.40-0.50 barrier costs to building alternatives) but perceive an exit path — their d is moderate (0.50-0.60), producing moderate f(d) and Scaffold classification. The directionality spread (0.15 to 0.95) is extreme, indicating the constraint produces stark perspectival divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through temporal decomposition. At the immediate/crisis time horizon (year 0-2), the constraint is genuinely coordinative — consolidation solves real mobilization problems and benefits emergency response. Extractiveness is moderate (0.38) and suppression reflects crisis panic rather than structural lock-in. The consolidators' Rope/Tangled Rope classification is accurate: they solve real problems. At the biographical/medium-term time horizon (year 2-6), the constraint transitions to durable extraction. Consolidation persists even as supply normalizes; small suppliers exit market or become subsidiary units of larger firms; barriers to re-entry solidify. Extractiveness peaks (0.52) and suppression becomes structural. The Snare classification for trapped suppliers becomes the primary perspectival reality. At the generational time horizon, the scaffold perspective (resilience initiatives) becomes observable: alternative structures (government procurement standards, supplier diversity mandates, regional stockpiling, cooperative pools) mature and begin to erode consolidator leverage. The constraint's classification drifts toward Scaffold as sunset approaches. The mandatrophy is resolved by recognizing that all six types are temporally valid at different horizons. The constraint is not a single type — it is a dynamical process transitioning from coordination (immediate) through extraction (biographical) toward alternative restructuring (generational), with Piton (performative JIT ideology) and Mountain-risk (false naturalization) as persistent diagnostic hazards.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    crisis_opportunism_vs_coordination,
    'To what extent is consolidator price-gouging opportunistic rent-seeking versus legitimate coordination fee for managing crisis complexity?',
    'Price analysis comparing pandemic-period markups to pre-pandemic and post-pandemic margins; correlation between actual supply restoration speed and prices charged; comparison to alternative supply coordination mechanisms (government procurement, cooperative pooling)',
    'If primarily opportunistic: constraint is pure Snare from consolidator perspective. If legitimate: constraint is Tangled Rope. The classification hinges on whether the price premium reflects genuine scarcity rent or extractive markup.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_opportunism_vs_coordination, empirical, 'Whether consolidator extraction is opportunistic or legitimate scarcity coordination').

omega_variable(
    alternative_supply_feasibility,
    'Were alternative supply pathways (government stockpiles, regionalized manufacturing, cooperative supplier pools) structurally available during the disruption or only available with sufficient advance preparation?',
    'Retrospective analysis of government capacity to mobilize domestic production; assessment of feasibility of emergency alternative sourcing; comparison of countries with advance preparation versus reactive mobilization',
    'If alternatives were available: constraint represents policy choice (structure can be redesigned). If alternatives required years of preparation: constraint approaches immutability during crisis period, and classification becomes temporal-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_supply_feasibility, empirical, 'Whether alternative supply structures were available or required advance preparation').

omega_variable(
    suppression_mechanism_durability,
    'Is the measured suppression (0.68) structural (geography, capital barriers, regulatory restrictions) or temporary (crisis-specific panic, hoarding, allocation controls)?',
    'Post-pandemic suppression trajectory: measurement of actual supplier barriers after crisis abates; assessment of whether barriers persist or dissipate; correlation between initial suppression and lasting competitive damage',
    'If structural: small suppliers face enduring extraction. If temporary: constraint''s suppression overestimated and actual extractiveness lower than measured. Informs whether victims experience lasting harm or recover capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_durability, empirical, 'Whether suppression is structural or crisis-temporary').

omega_variable(
    consolidation_irreversibility,
    'Is the pandemic-driven consolidation of supply chains into fewer, larger players reversible or does it create path-dependent lock-in?',
    'Analysis of supplier exit rates post-pandemic; measurement of market concentration reversal or persistence; assessment of re-entry barriers for exited suppliers',
    'If reversible: scaffold sunset is real and constraint classification converges toward Rope over time. If irreversible: consolidation becomes permanent structural feature and constraint classification becomes durable Tangled Rope or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consolidation_irreversibility, empirical, 'Whether pandemic consolidation is reversible or creates lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pandemic_supply_disruption, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pand_tr_t0, pandemic_supply_disruption, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pand_tr_t2, pandemic_supply_disruption, theater_ratio, 2, 0.48).
narrative_ontology:measurement(pand_tr_t4, pandemic_supply_disruption, theater_ratio, 4, 0.55).
narrative_ontology:measurement(pand_tr_t6, pandemic_supply_disruption, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(pand_be_t0, pandemic_supply_disruption, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(pand_be_t2, pandemic_supply_disruption, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(pand_be_t4, pandemic_supply_disruption, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(pand_be_t6, pandemic_supply_disruption, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pandemic_supply_disruption, resource_allocation).
narrative_ontology:affects_constraint(pandemic_supply_disruption, just_in_time_manufacturing).
narrative_ontology:affects_constraint(pandemic_supply_disruption, supply_chain_resilience).
narrative_ontology:affects_constraint(pandemic_supply_disruption, commodity_price_volatility).

% DUAL FORMULATION NOTE:
% The pandemic supply disruption decomposes into multiple constraints: (1) immediate scarcity coordination (legitimate crisis response), (2) consolidation lock-in (structural extraction), (3) just-in-time degradation (piton), (4) resilience alternatives (scaffold). This story tracks the hybrid evolution from coordination to extraction. Upstream constraints (just-in-time manufacturing fragility, commodity markets) create vulnerability; downstream constraints (supply-chain resilience initiatives, regulatory restructuring) emerge as response. Network linkage reflects causal propagation: JIT fragility enables consolidator extraction; resilience initiatives provide sunset mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
