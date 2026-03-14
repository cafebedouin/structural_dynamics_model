% ============================================================================
% CONSTRAINT STORY: fossil_fuel_economic_transition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fossil_fuel_economic_transition, []).

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
 *   constraint_id: fossil_fuel_economic_transition
 *   human_readable: Fossil Fuel Economic Transition as Tangled Coordination-Extraction
 *   domain: economic_policy/energy_transition/labor
 *
 * SUMMARY:
 *   The fossil fuel economic transition presents a structural constraint
 *   where legitimate coordination problems (decarbonizing energy
 *   infrastructure, managing stranded assets, retraining workers) are coupled
 *   with systematic extraction from the least-protected populations (coal
 *   workers, fossil-fuel-dependent communities, global south energy-poor).
 *   The constraint exhibits genuine coordination function — renewable energy
 *   deployment does require collective infrastructure investment, policy
 *   certainty, and labor market coordination — but this coordination is
 *   implemented through mechanisms that concentrate extraction downward to
 *   the powerless while protecting investor capital and concentrating gains
 *   upward. The theater ratio (0.68) reflects that much transition policy is
 *   performative: carbon markets, ESG compliance, and voluntary corporate
 *   commitments allow continued fossil fuel use through offset purchase;
 *   transition financing to the global south comes with conditionality that
 *   reproduces dependency; renewable energy supply chains replicate labor
 *   extraction and environmental destruction from previous resource
 *   extraction. The constraint's extractiveness has increased dramatically
 *   (0.28→0.58) as policy has shifted from coal-worker-focused regional
 *   development to investor-focused renewable deployment with minimal worker
 *   protections.
 *
 * KEY AGENTS:
 *   - Coal Workers and Fossil Fuel Employees: Primary victims (powerless/trapped) — geographically dependent, skill-specific, trapped by pension structure and lack of alternatives
 *   - Coal-Dependent Communities: Primary victims (moderate/constrained) — face infrastructure collapse, out-migration, and reduced tax base during transition; constrained through limited retraining capacity
 *   - Global South Populations: Secondary victims (powerless/constrained) — face energy access reduction, debt-financed renewable projects with foreign ownership, and supply chain extraction in mining for transition minerals
 *   - Renewable Energy Investors: Primary beneficiaries (institutional/arbitrage) — capture subsidies, mandates, and market growth with minimal adjustment cost; arbitrage across jurisdictions
 *   - Climate Policy Advocates: Beneficiaries (institutional/arbitrage) — secure policy wins and funding, but may not bear adjustment costs; arbitrage through consulting, NGO funding, and policy influence
 *   - Carbon Market Operators: Institutional actors (institutional/arbitrage) — benefit from offset trading and financial instruments; maintain arbitrage through regulatory capture
 *   - Climate Justice Coalition: Organized agents (organized/constrained) — advocate for just transition and worker protections; constrained by limited enforcement power but organized for advocacy
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy choice as economic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fossil_fuel_economic_transition, 0.58).
domain_priors:suppression_score(fossil_fuel_economic_transition, 0.65).
domain_priors:theater_ratio(fossil_fuel_economic_transition, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fossil_fuel_economic_transition, extractiveness, 0.58).
narrative_ontology:constraint_metric(fossil_fuel_economic_transition, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fossil_fuel_economic_transition, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fossil_fuel_economic_transition, tangled_rope).
narrative_ontology:human_readable(fossil_fuel_economic_transition, "Fossil Fuel Economic Transition as Tangled Coordination-Extraction").
narrative_ontology:topic_domain(fossil_fuel_economic_transition, "economic_policy/energy_transition/labor").

domain_priors:requires_active_enforcement(fossil_fuel_economic_transition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fossil_fuel_economic_transition, renewable_energy_investors).
narrative_ontology:constraint_beneficiary(fossil_fuel_economic_transition, climate_policy_advocates).
narrative_ontology:constraint_beneficiary(fossil_fuel_economic_transition, technology_manufacturers).
narrative_ontology:constraint_victim(fossil_fuel_economic_transition, fossil_fuel_workers).
narrative_ontology:constraint_victim(fossil_fuel_economic_transition, coal_dependent_communities).
narrative_ontology:constraint_victim(fossil_fuel_economic_transition, global_south_energy_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COAL WORKER (SNARE) — Trapped by geographic dependence, skills specificity, and pension structure. Cannot exit without devastating career loss, relocating family, or retraining with uncertain outcome. Experiences transition policy as pure extraction: loses income security, community, and identity with minimal alternative provided. Maximum suppression — no genuine exit option exists within biographical timeframe.
constraint_indexing:constraint_classification(fossil_fuel_economic_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TRANSITION COMMUNITY (TANGLED ROPE) — Communities with diverse energy infrastructure face both coordination problems (infrastructure investment requires collective action) and asymmetric extraction (some regions transition smoothly while others collapse). Constrained exit through retraining programs and regional development, but high cost. Benefits from some transition funding but bears disproportionate transition burden. Experiences genuine coordination function (shared grid infrastructure, energy security) alongside extraction.
constraint_indexing:constraint_classification(fossil_fuel_economic_transition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RENEWABLE ENERGY INDUSTRY (ROPE) — Net beneficiary through subsidies, mandates, and market capture. Experiences the constraint as coordination: building renewable capacity requires policy certainty and grid infrastructure investment. Arbitrage option through technology export and arbitrage across jurisdictions. Extraction runs toward this actor — they benefit from transition policy while bearing minimal adjustment cost.
constraint_indexing:constraint_classification(fossil_fuel_economic_transition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE JUSTICE COALITION (SCAFFOLD) — Organized actors (labor unions, environmental justice groups, development advocates) frame the transition as temporary with sunset: just transition policies, worker retraining, and green jobs are intended as time-bound coordination to manage the shift to clean energy. Constrained but organized — can mobilize to enforce sunset conditions. Sees the constraint as a necessary-but-time-limited coordination mechanism, not a permanent extraction.
constraint_indexing:constraint_classification(fossil_fuel_economic_transition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CARBON MARKET APPARATUS (PITON) — Cap-and-trade, carbon offsets, and voluntary ESG compliance are substantially performative: they allow continued fossil fuel use through offset purchase, create financial instruments divorced from actual emissions reduction, and maintain the fiction of market-based transition without forcing structural change. Theater ratio high (0.68) — the apparatus persists through institutional inertia and financial incentives despite low functional emissions reduction. Arbitrage actors benefit from carbon market operations regardless of climate outcome.
constraint_indexing:constraint_classification(fossil_fuel_economic_transition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the energy transition is framed as an immutable physical/economic necessity: fossil fuels are thermodynamically limited, climate change is inevitable, renewable energy is cheaper and will naturally replace coal. This perspective naturalizes the transition as a law of economics rather than a contingent policy choice involving winners and losers. However, the structural data contradicts the mountain classification — the constraint's extractiveness, suppression, and theater reveal that much of what appears 'natural' is actually enforced policy choice and institutional arrangement.
constraint_indexing:constraint_classification(fossil_fuel_economic_transition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fossil_fuel_economic_transition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fossil_fuel_economic_transition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fossil_fuel_economic_transition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fossil_fuel_economic_transition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fossil_fuel_economic_transition, TR),
    TR >= 0.70.

:- end_tests(fossil_fuel_economic_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The transition creates genuine wealth destruction for fossil fuel workers (pension loss, skill obsolescence, geographic immobility) that is only partially compensated through transition programs. The extraction is not total (some regions transition successfully, some workers retrain effectively) but systematic — it affects the least-protected populations. The value increased from 0.28 to 0.58 as policy shifted from coal-worker protection to investor-focused renewable deployment. Suppression (0.65): High. Workers face multiple barriers: geographic dependence (coal mines and plants are location-specific), skill specificity (coal mining expertise has limited transferability), pension structure (relocating costs pension benefits), and psychological/cultural attachment (coal work is identity). Labor market suppression is substantial — workers cannot easily exit without devastating loss. Suppression is also informational — policy rhetoric frames transition as natural/inevitable rather than acknowledging worker costs as policy choice. Theater ratio (0.68): High-moderate. Carbon markets are substantially performative (allow continued fossil fuel use through offset purchase). Transition financing to global south often comes with minimal climate impact relative to finance flow. ESG compliance by energy companies allows greenwashing while maintaining fossil fuel portfolios. Renewable energy deployment statistics count installed capacity without measuring actual emissions reduction or worker welfare. The theater increased from 0.35 to 0.68 as performative mechanisms (carbon markets, ESG) expanded.
 *
 * PERSPECTIVAL GAP:
 *   The gap between Snare (coal worker) and Rope (renewable investor) perspectives reveals the constraint's asymmetry. The same policy (coal phase-out) appears as pure extraction from the worker's view (no exit, no benefit, no control) and pure coordination from the investor's view (solves deployment problem, provides market opportunity, requires no personal adjustment). This gap is not an artifact of measurement — it reflects real structural difference in power, exit options, and benefit flow. The gap is WIDENED by policy design: if transition funding prioritized worker retraining and community economic development, the worker perspective might shift from Snare to Tangled Rope. The fact that it remains Snare indicates that policy has chosen investor protection over worker protection — a choice, not an inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality derives from their structural position. Coal workers: victims (d→1.0) + trapped exit (d→0.95) + regional scope (σ=0.9) → experienced extraction is severe. Renewable investors: beneficiaries (d→0.0) + arbitrage exit (d→0.05) + global scope (σ=1.2) → experienced extraction is negative (they gain). Transition communities: mixed (partially victimized, partially benefited) + constrained exit (d→0.65) + national scope (σ=1.0) → moderate extraction. The pipeline computes χ from ε, f(d), and σ(S) for each perspective, producing the perspectival gap. No overrides needed — the structural derivation correctly captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATH: The constraint resolves mandatrophy by showing that tangled_rope is the accurate classification when measured at analytical scope with full beneficiary/victim data. The coal worker's snare perspective is NOT a contradiction — it is the correct experienced classification from a trapped position. The renewable investor's rope perspective is also correct — they genuinely experience coordination, not extraction. The mandatrophy dissolves when we recognize that the SAME CONSTRAINT produces different classifications for different agents because their structural positions are genuinely different (different power, different exit options, different benefit flow). The constraint is NOT a mountain (natural law) — the perspectival gap proves policy choice, not inevitability. It IS a tangled rope when measured at the institutional/analytical level: genuine coordination function (energy infrastructure decarbonization) coupled with asymmetric extraction (concentrated costs on workers, concentrated benefits on investors). The theater (0.68) indicates that much rhetoric naturalizes policy choice ('fossil fuels are obsolete') when actually describing how policy design concentrates extraction. Resolving mandatrophy requires acknowledging both the genuine coordination problem AND the policy choice about who bears transition costs. Neither Rope (pure coordination) nor Snare (pure extraction) describes the full structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    just_transition_feasibility,
    'Can rapid fossil fuel phase-out provide genuine economic alternatives for workers in coal-dependent regions within a biographical timeframe?',
    'Empirical tracking of retraining outcomes, wage replacement rates, and community economic recovery in early transition zones (Germany, Poland, Australia); comparison of promised vs realized job creation in clean energy sectors',
    'If feasible: transition is genuine scaffold with real sunset and worker agency. If infeasible: constraint is structural snare masked by transition rhetoric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_feasibility, empirical, 'Whether just transition programs provide genuine economic alternatives').

omega_variable(
    global_south_energy_justice,
    'Does transition financing and technology transfer enable energy access in global south or does it reproduce dependency and extraction under green labels?',
    'Analysis of conditionality in transition finance, ownership structure of renewable projects in developing nations, and real energy cost/access for poor populations post-transition',
    'If enabling: constraint is primarily regional (confined to wealthy nations). If extractive: constraint extends globally and snare classification dominates for global majority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_energy_justice, empirical, 'Whether transition policies enable or reproduce global energy injustice').

omega_variable(
    renewable_intermittency_cost,
    'What is the true system cost of managing renewable intermittency and grid flexibility? Is this cost currently being borne by trapped agents or is it being explicitly socialized?',
    'Life-cycle cost analysis of full renewable deployment including storage, grid hardening, and backup capacity; identification of who bears costs vs receives benefits of grid transition',
    'If costs socialized to fossil fuel workers and coal communities: reinforces snare classification. If costs explicitly funded through carbon tax or equitable mechanism: reduces snare severity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_intermittency_cost, empirical, 'True system cost of renewable intermittency management and cost allocation').

omega_variable(
    stranded_asset_compensation,
    'Do stranded asset compensation schemes adequately compensate workers and communities or do they primarily protect investor capital?',
    'Comparison of compensation paid to fossil fuel companies vs workers and communities; analysis of pension fund impacts and wealth destruction in coal-dependent regions',
    'If primarily protecting investor capital: extraction becomes visible as policy choice, not market necessity. Theater ratio may be overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stranded_asset_compensation, empirical, 'Whether stranded asset compensation serves workers and communities or investors').

omega_variable(
    renewable_supply_chain_extraction,
    'Do renewable energy supply chains (lithium, cobalt, rare earths) reproduce colonial extraction patterns and labor exploitation under green labels?',
    'Labor standards analysis in renewable supply chains; comparison of working conditions and environmental impact in mining/manufacturing for renewables vs fossil fuels; wealth distribution across supply chain',
    'If true: the transition constraint is remaking extraction rather than eliminating it. Classification shifts from tangled_rope to snare with redistributed victim group. Theater ratio increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_supply_chain_extraction, empirical, 'Whether renewable supply chains reproduce extractive patterns').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fossil_fuel_economic_transition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ffet_tr_t0, fossil_fuel_economic_transition, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ffet_tr_t5, fossil_fuel_economic_transition, theater_ratio, 5, 0.52).
narrative_ontology:measurement(ffet_tr_t10, fossil_fuel_economic_transition, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(ffet_be_t0, fossil_fuel_economic_transition, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ffet_be_t5, fossil_fuel_economic_transition, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ffet_be_t10, fossil_fuel_economic_transition, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fossil_fuel_economic_transition, resource_allocation).
narrative_ontology:boltzmann_floor_override(fossil_fuel_economic_transition, 0.18).
narrative_ontology:affects_constraint(fossil_fuel_economic_transition, carbon_market_performativity).
narrative_ontology:affects_constraint(fossil_fuel_economic_transition, renewable_supply_chain_labor).
narrative_ontology:affects_constraint(fossil_fuel_economic_transition, stranded_asset_compensation).
narrative_ontology:affects_constraint(fossil_fuel_economic_transition, global_south_energy_access).

% DUAL FORMULATION NOTE:
% The fossil fuel transition constraint family decomposes into four related stories: (1) fossil_fuel_economic_transition (ε=0.58, Tangled Rope) — the primary coordination-extraction hybrid of managing energy infrastructure decarbonization with asymmetric worker costs; (2) carbon_market_performativity (ε=0.72, Snare) — the degraded offset mechanism allowing continued emissions while appearing to transition; (3) renewable_supply_chain_labor (ε=0.65, Snare) — the reproduction of extractive mining and labor conditions under green labels; (4) stranded_asset_compensation (ε=0.55, Tangled Rope) — the genuine coordination of managing asset transition coupled with asymmetric wealth protection. These constraints are upstream/downstream related: carbon markets and supply chain extraction are consequences of transition policy design choices; stranded asset compensation is a parallel mechanism operating in the same policy space. Each has its own ε value reflecting different structural mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fossil_fuel_economic_transition, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
