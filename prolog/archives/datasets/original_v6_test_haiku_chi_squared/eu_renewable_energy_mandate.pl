% ============================================================================
% CONSTRAINT STORY: eu_renewable_energy_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_renewable_energy_mandate, []).

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
 *   constraint_id: eu_renewable_energy_mandate
 *   human_readable: EU Renewable Energy Directive and Support Schemes
 *   domain: economic/political
 *
 * SUMMARY:
 *   The EU Renewable Energy Directive and its supporting mechanisms (feed-in
 *   tariffs, contracts for difference, investment guarantees) represent a
 *   hybrid constraint that combines genuine coordination toward
 *   decarbonization with asymmetric extraction from fossil fuel incumbents,
 *   grid operators, and electricity consumers. The constraint solves the
 *   collective action problem of energy system transformation — no individual
 *   member state has incentive to invest heavily in renewables if neighbors
 *   free-ride on technology learning curves and grid infrastructure
 *   investments. However, the directive simultaneously transfers wealth from
 *   locked-in fossil fuel assets to renewable energy producers, concentrates
 *   costs on dispersed and politically weak populations (household consumers,
 *   fossil fuel workers), and creates operational stress for grid systems
 *   designed for baseload generation. The theater ratio (0.55) reflects the
 *   substantial performative element in compliance: complex accounting
 *   schemes (green bonds, renewable energy credits), political declarations
 *   of progress that mask continued gas infrastructure investment, and the
 *   diversion of support toward 'transition fuel' natural gas rather than
 *   aggressive decarbonization. The extractiveness (0.52) and suppression
 *   (0.48) indicate a constraint that is neither pure coordination nor pure
 *   predation, but genuinely hybrid — all eight perspectives produce
 *   different classifications, signaling that the constraint's true nature
 *   depends entirely on the observer's structural position and exit capacity.
 *
 * KEY AGENTS:
 *   - Renewable Energy Producers and Equipment Manufacturers: Primary beneficiaries (institutional/arbitrage) — capture guaranteed market access, pricing floors, and technology export opportunities
 *   - EU Climate Policy Institutions: Organized beneficiaries (organized/mobile) — benefit from coordinated investment and progress toward climate targets; see directive as temporary with sunset
 *   - Household Electricity Consumers: Primary victims (powerless/trapped) — bear cost of subsidized renewable energy through higher tariffs and grid stabilization charges; cannot exit national markets
 *   - Fossil Fuel Incumbent Workers: Primary victims (powerless/trapped) — face accelerated phase-out and stranded skills with limited compensation or transition support
 *   - Grid Operators and Transmission System Operators: Mixed victims (organized/constrained) — benefit from coordination and infrastructure investment but bear operational costs of managing intermittent supply
 *   - Industrial Electricity Consumers: Secondary victims (moderate/constrained) — face energy cost increases but can shift to cheaper grids and benefit from EU partnerships
 *   - Member State Energy Ministers: Institutional actors (institutional/constrained) — constrained by EU obligations; maintain theater through compliance reporting while protecting domestic incumbents
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both the genuine coordination function and the regressive distributional effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_renewable_energy_mandate, 0.52).
domain_priors:suppression_score(eu_renewable_energy_mandate, 0.48).
domain_priors:theater_ratio(eu_renewable_energy_mandate, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_renewable_energy_mandate, extractiveness, 0.52).
narrative_ontology:constraint_metric(eu_renewable_energy_mandate, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(eu_renewable_energy_mandate, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_renewable_energy_mandate, tangled_rope).
narrative_ontology:human_readable(eu_renewable_energy_mandate, "EU Renewable Energy Directive and Support Schemes").
narrative_ontology:topic_domain(eu_renewable_energy_mandate, "economic/political").

domain_priors:requires_active_enforcement(eu_renewable_energy_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_renewable_energy_mandate, renewable_energy_producers).
narrative_ontology:constraint_beneficiary(eu_renewable_energy_mandate, equipment_manufacturers).
narrative_ontology:constraint_beneficiary(eu_renewable_energy_mandate, eu_climate_goals).
narrative_ontology:constraint_victim(eu_renewable_energy_mandate, fossil_fuel_incumbents).
narrative_ontology:constraint_victim(eu_renewable_energy_mandate, electricity_consumers).
narrative_ontology:constraint_victim(eu_renewable_energy_mandate, grid_flexibility_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOUSEHOLD CONSUMER (SNARE) — Trapped within national electricity markets with no exit option. Bears the cost of subsidized renewable energy through higher tariffs and grid stabilization costs. Cannot switch to cheaper suppliers or self-generate without major capital expenditure. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FOSSIL FUEL INCUMBENT WORKERS (SNARE) — Trapped in coal, gas, and oil industries facing accelerated phase-out timelines. Regional economies dependent on fossil fuel infrastructure have limited mobility. No explicit compensation mechanism for stranded skills. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INDUSTRIAL ELECTRICITY CONSUMERS (TANGLED ROPE) — Constrained by energy costs and grid reliability requirements, but also benefit from EU decarbonization partnerships and access to renewable energy procurement. Can shift to cheaper grids within EU but face switching costs and regulatory restrictions. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RENEWABLE ENERGY PRODUCERS AND EQUIPMENT MANUFACTURERS (ROPE) — Primary beneficiaries. Guaranteed market access and pricing floors through feed-in tariffs and contracts for difference. Can arbitrage between EU member states and export technologies. EU directives solve the collective action problem of coordinating transition investment. d≈0.08, f(d)≈-0.10, σ=1.1 → χ≈-0.06. Negative effective extraction = net beneficiary through coordination.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GRID OPERATORS AND TRANSMISSION SYSTEM OPERATORS (TANGLED ROPE) — Organized institutional actors. Coordination benefit: EU directives enable cross-border grid integration and investment in transmission infrastructure. Extraction: forced to manage intermittency costs, grid balancing expenses, and weather-dependent supply with limited cost recovery mechanisms. Constrained by regulatory price caps. d≈0.55, f(d)≈0.73, σ=1.1 → χ≈0.42.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: EU CLIMATE POLICY INSTITUTIONS AND ENVIRONMENTAL ADVOCATES (SCAFFOLD) — Organized actors see the directive as a temporary coordination tool with a sunset clause: as renewable energy costs decline and grid storage technology matures, the support schemes (feed-in tariffs, CfD) will become less necessary. Climate targets will transition from mandatory support to market-competitive economics. High mobility: these actors can withdraw support if targets are met. d≈0.35, f(d)≈0.25, σ=1.1 → χ≈0.15. Low effective extraction because agents perceive declining necessity and have policy agency to terminate.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: MEMBER STATE ENERGY MINISTERS (PITON) — Constrained by EU treaty obligations and political pressure to meet binding renewables targets. The directive persists through institutional inertia even as market conditions change. Many support schemes are theater: maintaining the appearance of progress on climate targets while protecting incumbent energy interests. Theater ratio reflects complex compliance reporting (accounting rules, renewable accounting, green bonds) that substitutes for actual transformation. d≈0.45, f(d)≈0.48, σ=1.1 → χ≈0.29.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / ENERGY TRANSITION VIEW (TANGLED ROPE) — From a civilizational economic perspective, the directive solves the coordination failure problem of decarbonization (pure rope function) but simultaneously extracts from locked-in incumbent industries and consumers through stranded assets and tariff mechanisms (snare function). Extractiveness reflects the asymmetric burden on fossil fuel workers and consumers. The support schemes are efficient from climate economics perspective but regressive from distribution perspective. d≈0.62, f(d)≈0.88, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_renewable_energy_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_renewable_energy_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_renewable_energy_mandate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_renewable_energy_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_renewable_energy_mandate, TR),
    TR >= 0.70.

:- end_tests(eu_renewable_energy_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The directive extracts significantly from fossil fuel incumbents (stranded assets) and consumers (tariff burden), but this extraction is partially justified by internalization of environmental externalities. The base value reflects that the asymmetry is real and substantial — renewable producers and manufacturers capture massive wealth transfers through support schemes — but the extraction is not pure predation; it serves the articulated public good of decarbonization. The measurement progression (0.38 → 0.46 → 0.52) shows increasing extraction as the directive has matured: initial support schemes were more modest, but recent iterations (especially post-2020) have concentrated support toward larger-scale renewable projects while shifting costs to consumers. Suppression (0.48): Moderate. Significant barriers to exit and voice exist — consumers cannot leave national markets, fossil fuel workers have limited geographic mobility, and member states cannot easily opt out of EU climate targets. However, suppression is not total: renewable producers have high exit options (arbitrage between member states), grid operators are organized and can negotiate, and some industrial consumers can relocate. Theater ratio (0.55): Moderate. The directive exhibits substantial theater: complex green finance accounting rules, renewable energy credits and certificates that can be traded or banked without corresponding generation, political announcements of progress that mask continued gas infrastructure, and 'just transition' rhetoric not matched by fund allocation. However, it's not pure theater — actual renewable capacity is being deployed at scale, grid infrastructure is being upgraded, and technology learning curves are genuine. The theater reflects the political need to maintain coalition support across ideologically diverse member states while executing systemic change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a dramatic perspectival spectrum. At one extreme, renewable energy producers and equipment manufacturers see the directive as pure coordination (Rope) — it solves the problem of coordinating massive infrastructure investment and learning-curve development that no single actor has incentive to undertake alone. The support schemes enable them to finance projects that would be unprofitable in an undirected market. At the other extreme, household consumers and fossil fuel workers see the directive as pure extraction (Snare) — they bear costs without benefits and have no exit option. In the middle, grid operators see tangled rope (genuine coordination benefit in cross-border integration and infrastructure development, but extraction through unpaid balancing services and regulatory price caps). The analytical observer sees all perspectives simultaneously: the directive is genuinely solving a coordination problem (decarbonization requires coordinated investment) while genuinely extracting from those with the least power to exit (consumers, workers). The pivotal disagreement is whether the extraction is *justified* by the public good produced (climate stabilization) or *illegitimate* rent-seeking disguised as climate policy. This mirrors the mandatrophy tension: are we looking at a rope mechanism that has extractive side effects, or a snare mechanism that claims coordination as legitimation?
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable Energy Producers: Beneficiary + arbitrage → d≈0.06, f(d)≈-0.09. Net beneficiaries with high exit options (can invest in other EU member states or globally). Household Consumers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction — cannot exit national electricity markets, cannot avoid tariff increases. Fossil Fuel Workers: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — stranded skills, regional lock-in, limited compensation mechanisms. Industrial Consumers: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but with some exit options (grid switching, relocation). Grid Operators: Mixed (organized + constrained) → d≈0.55, f(d)≈0.73. Intermediate extraction reflecting both coordination benefits and uncompensated operational costs. EU Climate Institutions: Beneficiary + mobile → d≈0.35, f(d)≈0.25. Low extraction; these actors see the constraint as temporary (sunset logic) and have policy agency. Member State Ministers: Constrained institution → d≈0.45, f(d)≈0.48. Intermediate; trapped by EU obligations but able to negotiate implementation details and maintain theater. Analytical Observer: analytical → d≈0.62, f(d)≈0.88. High directionality reflecting that the observer sees the constraint as genuine extraction despite its coordination rationale.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATHWAY: This constraint resolves the mandatrophy by disaggregating the directive into its genuine coordination function and its extractive side effects. The coordination function (rope core) is: solving the problem of decentralized energy system transformation that creates free-rider incentives for individual actors. The extractive overlay (snare shell) is: concentrating wealth transfers to renewable producers while distributing costs to politically weak populations. The high theater ratio (0.55) indicates that the institutional apparatus is partially performing legitimation theater — justifying the wealth transfers through climate rhetoric and 'just transition' language that is not matched by resource allocation. The tangled_rope classification at the analytical level reflects that this is genuinely BOTH mechanisms simultaneously, not a rope with side effects or a snare disguised as coordination. The mandatrophy resolves at extractiveness > 0.70 because the constraint has clear directionality: it benefits concentrated interests (renewable producers, climate-conscious EU institutions) at expense of diffuse interests (consumers, workers). This asymmetry is the signature of extraction, even though the extraction serves an articulated public good. The scaffold perspective (temporary, with sunset via storage technology maturation) provides the escape hatch: as renewable energy becomes cheaper than fossil fuels and storage technology matures, the support schemes will be made unnecessary by market economics, and the extractive element can be unwound. Until that point, the constraint is structurally extractive — it transfers wealth from the politically weak to the politically organized, and it is only justified (not exonerated) by the climate goals it serves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_internalization_threshold,
    'What support scheme subsidy level internalizes true environmental costs without creating pure extraction from grid operators and consumers?',
    'Life cycle assessment of externalities; comparison of environmental damage avoided vs consumer tariff burden across EU member states',
    'If threshold is achievable at <20% cost adder: directive transitions from tangled_rope to rope (coordination-dominant). If threshold requires >40% adders: directive is classified as snare for consumers (extraction-dominant). Current evidence suggests 15-35% range creates ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_internalization_threshold, empirical, 'Support scheme subsidy level that internalizes environmental costs without pure extraction').

omega_variable(
    stranded_asset_inevitability,
    'Are coal and gas plant stranding inevitable from decarbonization physics, or contingent on policy design choices that could phase transition more gradually?',
    'Scenario analysis: techno-economic models comparing abrupt vs gradual phase-out costs; historical analysis of prior energy transitions (coal to gas, nuclear adoption)',
    'If inevitable: worker impacts are feature of thermodynamic/economic constraint (mountain-like). If contingent: stranding is extractive choice embedded in the directive (snare feature for workers). Current policy trajectory suggests contingent design: faster phase-out than techno-economics requires.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stranded_asset_inevitability, conceptual, 'Whether stranded asset externalities are inevitable or contingent on policy design').

omega_variable(
    grid_intermittency_storage_capability,
    'Will battery storage, green hydrogen, and demand flexibility technologies mature fast enough to make support schemes obsolete (proving scaffold hypothesis) or will they hit capacity ceilings (proving permanent tangled_rope)?',
    'Tracking battery cost curves, electrolyzer deployment rates, and grid storage capacity additions; comparison against projected renewable variability; historical rates of technology adoption',
    'If storage scales fast: scaffold perspective is structural — support schemes are temporary bridge. If storage hits ceilings: support schemes persist indefinitely (tangled_rope hardens into snare). Current trajectory suggests 2035-2045 inflection point.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_intermittency_storage_capability, empirical, 'Whether storage technology will enable phase-out of support schemes (scaffold sunset)').

omega_variable(
    cross_border_arbitrage_regulatory_response,
    'Will harmonized EU regulation prevent member states from using support schemes to create regulatory arbitrage and attract multinational renewable producers at others'' expense?',
    'Analysis of support scheme differentiation across member states; investigation of renewable investment flows and cherry-picking of high-subsidy jurisdictions; modeling of zero-sum competition',
    'If harmonization succeeds: directive functions as pure coordination for EU-wide energy transition (rope). If arbitrage persists: directive becomes extraction tool for capital-rich member states at expense of fiscally constrained states (snare at international level).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_border_arbitrage_regulatory_response, empirical, 'Whether EU harmonization prevents member-state-level regulatory arbitrage').

omega_variable(
    incumbent_political_capture_durability,
    'To what extent do fossil fuel incumbents capture renewable energy support schemes (biasing toward natural gas as ''transition fuel'' vs solar/wind), and how durable is that capture against policy reform?',
    'Forensic policy analysis: budget allocations to gas support vs renewables; gas infrastructure investment vs renewable grid modernization; political economy of gas industry influence on member state regulators',
    'If capture is durable: directive functions as snare for climate goals (locks in gas as permanent infrastructure, not transition). If capture is reversible: piton classification is correct (theater can be exposed and reform attempted).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_political_capture_durability, empirical, 'Political durability of fossil fuel incumbent capture of renewable support schemes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_renewable_energy_mandate, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_ren_tr_t0, eu_renewable_energy_mandate, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eu_ren_tr_t7, eu_renewable_energy_mandate, theater_ratio, 7, 0.48).
narrative_ontology:measurement(eu_ren_tr_t14, eu_renewable_energy_mandate, theater_ratio, 14, 0.55).

% Extraction over time
narrative_ontology:measurement(eu_ren_be_t0, eu_renewable_energy_mandate, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(eu_ren_be_t7, eu_renewable_energy_mandate, base_extractiveness, 7, 0.46).
narrative_ontology:measurement(eu_ren_be_t14, eu_renewable_energy_mandate, base_extractiveness, 14, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_renewable_energy_mandate, resource_allocation).
narrative_ontology:affects_constraint(eu_renewable_energy_mandate, fossil_fuel_stranded_assets).
narrative_ontology:affects_constraint(eu_renewable_energy_mandate, electricity_grid_intermittency).
narrative_ontology:affects_constraint(eu_renewable_energy_mandate, eu_carbon_price_mechanism).
narrative_ontology:affects_constraint(eu_renewable_energy_mandate, natural_gas_infrastructure_lock_in).

% DUAL FORMULATION NOTE:
% The EU Renewable Energy Directive decomposes into three structurally distinct constraints: (1) the coordination function of decarbonized energy system investment (rope-dominant, ε≈0.15); (2) the extractive wealth transfer to renewable producers and away from fossil fuel incumbents (snare-dominant, ε≈0.65); (3) the consumer tariff burden mechanism (snare-dominant, ε≈0.58). The directive as a unified policy object creates a single effective constraint with ε≈0.52 that reflects the mixture. Downstream constraints like grid intermittency and stranded asset management inherit this mixed nature and are affected by the tangled rope's internal tensions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_renewable_energy_mandate, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
