% ============================================================================
% CONSTRAINT STORY: jevons_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jevons_paradox, []).

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
 *   constraint_id: jevons_paradox
 *   human_readable: Jevons Paradox (The Rebound Effect)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Jevons Paradox describes the structural tension between technological
 *   efficiency gains and aggregate resource consumption. When a technology
 *   becomes more efficient at delivering a service (e.g., lighting,
 *   transportation, heating), the per-unit cost of that service declines,
 *   triggering demand expansion that partially or wholly offsets the
 *   efficiency gain. This creates a paradox: efficiency improvements, the
 *   primary policy lever for sustainability, fail to reduce total resource
 *   consumption and may increase it. The constraint exhibits a classic
 *   tangled rope structure: there is a genuine coordination benefit
 *   (efficiency makes services affordable to more people; energy-poor
 *   households gain access), but this benefit is systematically captured by
 *   incumbent extractors who profit from the demand rebound. The mechanism is
 *   not coercive in the snare sense (no one is forced to consume more), but
 *   the structural incentives ensure that efficiency translates into
 *   consumption expansion rather than conservation. The theater ratio (0.58)
 *   reflects that efficiency improvements are often presented as solutions to
 *   resource constraints when they actually enable consumption growth — the
 *   narrative of 'greening through technology' masks the continuation of
 *   extraction-driven growth. Over the 100-year measurement interval
 *   (industrial efficiency improvements from the 19th century to present),
 *   base extractiveness has risen from 0.18 to 0.38 as efficiency gains have
 *   been systematically converted into demand growth, and theater has risen
 *   from 0.42 to 0.58 as the efficiency-as-solution framing has become more
 *   dominant despite mounting evidence of persistent or increasing resource
 *   extraction.
 *
 * KEY AGENTS:
 *   - Environmental Commons: Primary victim (powerless/trapped) — bears total cost of demand rebound; cannot exit or organize; faces cumulative extraction despite per-unit efficiency improvements
 *   - Climate Policy Advocates: Secondary victim (moderate/constrained) — policy tools (efficiency standards, building codes) are systematically undermined by rebound effects; constrained by political inability to regulate demand directly
 *   - Incumbent Resource Extractors (oil, coal, minerals, water): Primary beneficiary (powerful/arbitrage) — profit from demand rebound; efficiency improvements lower per-unit costs, lowering prices, triggering consumption surge that maintains or expands total extraction revenue
 *   - Efficiency Technology Producers: Secondary beneficiary (institutional/arbitrage) — benefit from regulatory demand for efficient technologies; also benefit from rebound-driven expansion of their addressable market
 *   - Consumers: Aggregate market demand (powerful/mobile) — experience both coordination benefit (lower cost of service) and hidden extraction (consumption expansion maintains their resource footprint despite efficiency); powerful but individually unaware of aggregate effect
 *   - Decoupling Advocates: Organized agents (organized/mobile) — propose structural alternatives (circular economy, sufficiency norms, absolute decoupling); see rebound as temporary coordination problem with sunset
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing efficiency paradox as inevitable law of markets when it is contingent on growth capitalism and externality-unpriced markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jevons_paradox, 0.38).
domain_priors:suppression_score(jevons_paradox, 0.48).
domain_priors:theater_ratio(jevons_paradox, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jevons_paradox, extractiveness, 0.38).
narrative_ontology:constraint_metric(jevons_paradox, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(jevons_paradox, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jevons_paradox, tangled_rope).
narrative_ontology:human_readable(jevons_paradox, "Jevons Paradox (The Rebound Effect)").
narrative_ontology:topic_domain(jevons_paradox, "economic/technological").

domain_priors:requires_active_enforcement(jevons_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jevons_paradox, incumbent_resource_extractors).
narrative_ontology:constraint_beneficiary(jevons_paradox, efficiency_technology_producers).
narrative_ontology:constraint_victim(jevons_paradox, environmental_commons).
narrative_ontology:constraint_victim(jevons_paradox, climate_stabilization_goals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENVIRONMENTAL COMMONS (SNARE) — Cannot exit or organize. Faces total resource depletion via demand rebound even as per-unit efficiency improves. Efficiency gains are systematically redirected into increased consumption rather than conservation. d≈0.93, f(d)≈1.39, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(jevons_paradox, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE POLICY ADVOCATES (SNARE) — Constrained by inability to regulate consumer behavior without severe political resistance. Efficiency mandates (building codes, vehicle standards) produce rebound effects that partially or wholly offset policy intent. Trapped in a cycle where policy success (widespread adoption of efficient technologies) enables policy failure (aggregate consumption remains high or rises). d≈0.82, f(d)≈1.18, σ=1.0 → χ≈0.45.
constraint_indexing:constraint_classification(jevons_paradox, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSUMERS (TANGLED ROPE) — Powerful due to aggregated market demand; mobile exit options (can adopt or reject efficient technologies). Experience constraint as both coordination benefit (efficiency gains lower cost of service) AND extraction (price declines trigger demand expansion, which maintains or increases total resource consumption). Enabled by rebound effects to increase consumption without apparent guilt. d≈0.52, f(d)≈0.68, σ=1.2 → χ≈0.31.
constraint_indexing:constraint_classification(jevons_paradox, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: EFFICIENCY TECHNOLOGY PRODUCERS (ROPE) — Benefit from efficiency mandates (regulatory-driven demand) and from rebound effects (increased consumption of efficient services expands their addressable market). See constraint as pure coordination: efficiency standards expand their business. d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.04.
constraint_indexing:constraint_classification(jevons_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RESOURCE EXTRACTION INDUSTRIES (ROPE) — Benefit from demand rebound. Efficiency improvements reduce per-unit extraction costs, lowering prices, which triggers demand surge. Net effect: profitability sustained or enhanced despite per-unit cost reduction. See constraint as coordination mechanism: efficiency creates new markets they serve. d≈0.10, f(d)≈-0.06, σ=1.2 → χ≈-0.02.
constraint_indexing:constraint_classification(jevons_paradox, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DECOUPLING ADVOCATES (SCAFFOLD) — Organized agents (NGOs, policy researchers, some nation-states) propose structural alternatives: absolute decoupling (efficiency + reduced consumption), circular economy (loop recycling reduces raw material demand), sufficiency norms (cultural shift away from consumption growth). See rebound effect as a temporary coordination problem with a sunset: if decoupling and circular systems mature, rebound effects will be neutralized. χ ≤ 0.30 because organized agents have agency. d≈0.35, f(d)≈0.34, σ=1.0 → χ≈0.20.
constraint_indexing:constraint_classification(jevons_paradox, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (PITON) — From civilizational view, rebound effects can be framed as beneficial: efficiency gains make services affordable to previously excluded populations (welfare expansion). The 'problem' of rebound framing is performative — it naturalizes scarcity when what has changed is abundance. This perspective sees the constraint as degraded: the rebound-effect narrative persists as a ghost of pre-abundance resource anxiety. Theater_ratio ≥0.70 satisfied by the persistence of 'efficiency paradox' framing despite radically changed material conditions. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.44 (only classification matters; χ not binding for piton).
constraint_indexing:constraint_classification(jevons_paradox, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jevons_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jevons_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jevons_paradox, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(jevons_paradox, TR),
    TR >= 0.70.

:- end_tests(jevons_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The rebound effect is not pure coercion — consumers respond to price signals rationally by increasing consumption of now-cheaper services. But the mechanism systematically benefits extractors: resource prices remain accessible (demand-driven extraction), per-unit extraction costs decline (efficiency), and total extraction volume remains high or grows. The 0.38 value reflects that extraction is enabled by efficiency through price mechanisms rather than direct coercion, but is structurally guaranteed by market incentives. Suppression (0.48): Moderate. Alternatives exist (conservation, sufficiency, decoupling) but are actively suppressed through: (a) growth-oriented economic ideology that frames consumption expansion as welfare improvement; (b) incumbent extractor lobbying against consumption-limiting policies; (c) collective action problems (individual consumers cannot coordinate on consumption reduction); (d) externality-unpriced markets (carbon/environmental costs not reflected in price signals). Suppression is not total — decoupling movements, circular economy pilots, and carbon pricing exist — but mainstream policy remains locked into efficiency-without-consumption-reduction. Theater ratio (0.58): Moderate-high. The narrative of 'efficiency solutions' to resource constraints is substantially performative. Efficiency improvements that lower prices are presented as sustainability victories when they enable consumption expansion that maintains extraction levels. The theater has grown over time (from 0.42 to 0.58) as green technology branding has proliferated despite aggregate resource consumption remaining high or rising. The theatrical element is not that efficiency improvements are fake — they are real — but that their presentation as 'solutions' masks the persistence of extraction-driven growth.
 *
 * PERSPECTIVAL GAP:
 *   The Jevons Paradox exhibits a critical perspectival gap between extractors/technology producers and climate advocates. From the extractor perspective, the constraint is pure coordination (Rope) — efficiency mandates expand their market, rebound effects maintain their extraction volumes, and growth continues uninterrupted. From the climate advocate perspective, the constraint is a snare (Snare) — efficiency policies fail to reduce aggregate consumption, and the rebound effect prevents absolute decoupling. The consumer perspective is subtly distinct (Tangled Rope) — they experience real cost reductions (coordination benefit) but their aggregate behavior maintains or increases resource extraction (hidden extraction). The decoupling advocate perspective (Scaffold) sees the rebound effect as a temporary coordination failure that can be overcome through structural alternatives (circular systems, sufficiency norms, absolute decoupling policies). The analytical observer risks the piton perspective — naturalizing the rebound effect as an inevitable law of markets when it is actually contingent on how prices are set, how externalities are priced, and how growth is culturally valued.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent resource extractors: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary. Efficiency improvements lower their per-unit extraction costs; rebound effects maintain their total extraction volume and revenue. Efficiency technology producers: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.06. Net beneficiary. Efficiency mandates drive demand for their products; rebound effects expand their addressable market. Consumers: Powerful + mobile, mixed beneficiary/victim → d≈0.52, f(d)≈0.68. Low-to-moderate extraction. They benefit from cost reductions but their aggregate consumption maintains extraction levels. Their mobility allows them to adopt or reject efficient technologies, but the market mechanism (price declines triggering demand expansion) operates independently of individual choice. Climate policy advocates: Victim + constrained → d≈0.82, f(d)≈1.18. Moderate-high extraction. Their policy tools (efficiency standards) are systematically undermined by rebound effects; constrained by political inability to enforce consumption limits. Environmental commons: Victim + trapped → d≈0.93, f(d)≈1.39. Maximum extraction. Cannot exit; bears total cost of demand rebound; systematic conversion of efficiency gains into continued or increased extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The Jevons Paradox exemplifies the mandatrophy between 'efficiency solves extraction' and 'extraction is coordination.' The constraint's resolution lies in recognizing that rebound effects are NOT an inherent law of technology (piton naturalization) but a contingent outcome of three structural conditions: (1) externality-unpriced markets (carbon, pollution, resource depletion costs not reflected in prices); (2) growth-oriented incentive structures (extractors profit from volume expansion, consumers benefit from cost reductions); (3) decoupling failure (circular economy and sufficiency systems not yet mature enough to replace virgin extraction). The mandatrophy is resolved by examining which structural conditions are binding. If externalities are priced (carbon tax, resource royalties), rebound effects are capped by cost signals — efficiency maintains its conservation benefits. If growth incentives are decoupled from consumption (e.g., through universal basic services rather than commodity markets), rebound effects cannot trigger demand expansion. If circular economy systems mature to closure, virgin resource demand can decline despite per-unit efficiency improvements. The Tangled Rope classification captures this: the constraint has a genuine coordination function (efficiency is real, welfare gains are real) but is being systematically captured by extractors through demand rebound because the underlying economic system (unpriced externalities, growth imperative) guarantees that efficiency gains will be converted into consumption expansion. The constraint is not inevitable — it is contingent on specific market structures, pricing regimes, and cultural values around growth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rebound_magnitude_empirical,
    'What is the true empirical magnitude of aggregate rebound effects across energy, transport, and water sectors? Is it 10-30% (weak, partial offset) or 60-100% (strong, full offset) or >100% (backfire)?',
    'Cross-country longitudinal analysis of efficiency improvements vs. aggregate consumption; econometric isolation of rebound from confounding demand trends; sector-specific decomposition (direct rebound vs. indirect vs. economy-wide spillovers)',
    'If weak (10-30%): efficiency policies work; rebound is manageable. Classification: Rope or Scaffold from multiple perspectives. If strong (60-100%): efficiency paradox is real; rebound prevents absolute decoupling. Classification: Snare or Tangled Rope dominates. If backfire (>100%): efficiency makes consumption worse. Classification: Snare from all perspectives except extractors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rebound_magnitude_empirical, empirical, 'Empirical magnitude of rebound effects across sectors').

omega_variable(
    absolute_decoupling_feasibility,
    'Can sufficiency norms, circular economy systems, and structural constraints (e.g., carbon pricing) achieve absolute decoupling (efficiency + consumption reduction) or is rebound mathematically inevitable under growth capitalism?',
    'Historical case studies of decoupling claims (EU carbon decoupling); modeling of circular economy closure rates; analysis of price-elasticity interaction with sufficiency norms; examination of whether ''decoupling'' measures consumption of virgin materials or total material throughput',
    'If feasible: scaffold perspective validated — sunset is structural. Constraint is temporary. If infeasible: decoupling is piton theater; constraint is permanent. Snare classification becomes inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_feasibility, conceptual, 'Whether absolute decoupling is structurally achievable').

omega_variable(
    extraction_vs_coordination_framing,
    'Is the Jevons Paradox primarily an extraction mechanism (incumbent extractors profit from demand rebound despite efficiency) or a coordination problem (markets fail to price externalities)?',
    'Comparative analysis of profit trajectories in resource industries during efficiency-driven demand booms; examination of whether rebound persists under carbon pricing or cap-and-trade; modeling of externality-internalizing price scenarios vs. actual markets',
    'If extraction dominant: snare and tangled rope classifications correct. Policy response must target extractors'' incentives or constrain their access (regulatory prohibition, carbon caps). If coordination dominant: rope and scaffold classifications correct. Policy response is price/tax reform or decoupling infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_framing, conceptual, 'Whether rebound is extraction mechanism or coordination failure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jevons_paradox, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jevons_tr_t0, jevons_paradox, theater_ratio, 0, 0.42).
narrative_ontology:measurement(jevons_tr_t50, jevons_paradox, theater_ratio, 50, 0.5).
narrative_ontology:measurement(jevons_tr_t100, jevons_paradox, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(jevons_be_t0, jevons_paradox, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(jevons_be_t50, jevons_paradox, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(jevons_be_t100, jevons_paradox, base_extractiveness, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jevons_paradox, resource_allocation).
narrative_ontology:affects_constraint(jevons_paradox, carbon_emissions_rebound).
narrative_ontology:affects_constraint(jevons_paradox, water_stress_demand_growth).
narrative_ontology:affects_constraint(jevons_paradox, mineral_extraction_scale).

% DUAL FORMULATION NOTE:
% Jevons Paradox is downstream of technological efficiency improvements but represents a distinct structural constraint operating at the market level. The upstream constraints (specific efficiency improvements: LED lighting, vehicle fuel economy, heating system efficiency) each have their own ε values reflecting the technical potential for conservation; Jevons Paradox has ε=0.38 reflecting the market mechanism that converts efficiency into consumption growth. The three downstream constraints (carbon rebound, water demand, mineral extraction) are empirical manifestations of the Jevons mechanism in specific resource domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
