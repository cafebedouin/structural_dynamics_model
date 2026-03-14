% ============================================================================
% CONSTRAINT STORY: carbon_externality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_carbon_externality, []).

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
 *   constraint_id: carbon_externality
 *   human_readable: Carbon Externality: Atmospheric Cost Transfer
 *   domain: environmental/economic/political
 *
 * SUMMARY:
 *   The carbon externality is a structural mechanism of cost transfer wherein
 *   the global economy systematically externalizes the atmospheric carbon
 *   cost of energy production, manufacturing, and consumption onto three
 *   voiceless victims: future generations, climate-vulnerable populations,
 *   and ecosystems. This constraint exhibits high extractiveness (0.68),
 *   extreme suppression (0.72), and moderate theater (0.58). From the
 *   perspective of those bearing the costs, it classifies as pure snare — no
 *   coordination benefit, no exit pathway, no alternative. From the
 *   perspective of emitters and beneficiaries, it appears as tangled rope
 *   (real coordination in energy systems, supply chains) or rope (renewable
 *   sectors experience it as pure coordination problem). The constraint is
 *   strengthened by attribution opacity (who is the emitter?), temporal
 *   distance (damage is future), and political fragmentation (no global
 *   governance of atmospheric commons). Theater has increased substantially
 *   over the past 50 years as emission accounting, corporate climate
 *   disclosures, and net-zero commitments create institutional appearance of
 *   constraint without corresponding reduction in cumulative emissions. The
 *   fundamental structure: those who bear costs cannot opt out, those who
 *   create costs can arbitrage to lower-cost jurisdictions or simply lobby to
 *   prevent internalization, and the atmospheric boundary — the actual
 *   constraint from physics — is not enforced by any institution.
 *
 * KEY AGENTS:
 *   - Climate Vulnerable Populations: Primary victims (powerless/trapped) — bear disproportionate climate impacts with minimal emission contribution; geographically locked and economically dependent on carbon-intensive systems
 *   - Future Generations: Primary victims (powerless/trapped) — temporally locked; have no participation in current decisions; bear accumulated warming and ecosystem loss
 *   - Ecosystem Integrity: Victim (non-agent) — species, coral reefs, permafrost systems bear costs of warming with zero voice in allocation of atmospheric carbon budget
 *   - Carbon-Intensive Industries: Primary beneficiary (powerful/arbitrage) — externalize production costs; capture economic rents during transition period; can relocate capital and influence policy
 *   - High-Consumption Populations: Secondary beneficiary (moderate/arbitrage or constrained) — benefit from low-cost energy and goods; distributed across high-income nations; face constrained exit from consumption patterns but have choice and political voice
 *   - Renewable Energy Sector: Secondary beneficiary (institutional/arbitrage) — experiences constraint as market opportunity and coordination problem; captures subsidies and growth; no structural extraction
 *   - Climate Advocates Coalition: Organized victims (organized/constrained) — have recognized extraction mechanism; attempt to enforce constraint through litigation, divestment, policy pressure; trapped by moral necessity despite recognition of structural asymmetry
 *   - Formal Accounting Systems: Institutional theater (institutional/arbitrage) — IPCC inventories, corporate carbon reporting, net-zero commitments create appearance of governance without reducing cumulative emissions; Goodhart drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(carbon_externality, 0.68).
domain_priors:suppression_score(carbon_externality, 0.72).
domain_priors:theater_ratio(carbon_externality, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(carbon_externality, extractiveness, 0.68).
narrative_ontology:constraint_metric(carbon_externality, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(carbon_externality, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(carbon_externality, snare).
narrative_ontology:human_readable(carbon_externality, "Carbon Externality: Atmospheric Cost Transfer").
narrative_ontology:topic_domain(carbon_externality, "environmental/economic/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(carbon_externality, carbon_emitting_industries).
narrative_ontology:constraint_beneficiary(carbon_externality, high_consumption_populations).
narrative_ontology:constraint_victim(carbon_externality, future_generations).
narrative_ontology:constraint_victim(carbon_externality, climate_vulnerable_populations).
narrative_ontology:constraint_victim(carbon_externality, ecosystem_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE VULNERABLE POPULATIONS (SNARE) — Cannot exit the constraint. Low-income populations in climate-vulnerable regions (small island states, sub-Saharan Africa, South Asia) bear disproportionate costs of carbon externality (flooding, drought, crop failure, displacement) while having minimal capacity to emit or reduce emissions. Trapped by geography and economic dependence on carbon-intensive supply chains. Maximum experienced extraction.
constraint_indexing:constraint_classification(carbon_externality, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Absolutely trapped by temporal structure. Cannot participate in current decisions; bear accumulated costs of atmospheric carbon accumulation (1.5°C to 3°C+ warming pathways, sea-level rise, ecosystem collapse). Entirely dependent on current agents' voluntary constraint of emissions. Zero degrees of freedom.
constraint_indexing:constraint_classification(carbon_externality, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ECOSYSTEM INTEGRITY (SNARE) — Non-agent victim. Cannot negotiate, organize, or exit. Bears extraction through species extinction, ecosystem disruption, ocean acidification, coral bleaching. Suppression is absolute — no voice in allocation of atmospheric carbon budget. Pure extraction from a voiceless system.
constraint_indexing:constraint_classification(carbon_externality, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: MIDDLE-INCOME POPULATIONS (TANGLED ROPE) — Constrained but not trapped. Face real costs of decarbonization (energy price increases, job transitions) but also benefit from some coordination: renewable energy, public transit, circular economy create efficiency gains. Coordination function exists (economies of scale in renewable deployment, shared grid infrastructure) alongside asymmetric extraction (those with assets ride the transition; workers in fossil fuel sectors bear transition costs). Moderate agent power through political organization and consumer choice.
constraint_indexing:constraint_classification(carbon_externality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RENEWABLE ENERGY SECTOR (ROPE) — Institutional beneficiary with arbitrage options. Experiences the constraint as coordination: carbon price signals, renewable subsidies, grid integration requirements coordinate multi-stakeholder transition. Benefits from market growth and policy support. Can exit via arbitrage (relocate capital, shift to new markets). Sees extraction only in transition costs, not structural extraction.
constraint_indexing:constraint_classification(carbon_externality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CARBON-INTENSIVE INDUSTRY (TANGLED ROPE) — Powerful beneficiary with arbitrage options. Extracts by externalizing atmospheric cost; coordinates through supply chains, regulatory capture, and lobbying influence. Has both coordination function (global energy supply) and asymmetric extraction (captures economic rents while distributing costs). Can arbitrage to lower-cost jurisdictions or adjust business models. High power position enables selective exit.
constraint_indexing:constraint_classification(carbon_externality, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: COALITION OF CLIMATE ADVOCATES (SNARE) — Organized but structurally outmatched. Have recognized the extraction mechanism and attempted to enforce constraint (climate litigation, divestment, policy pressure) but remain trapped in an architecture where externalizing costs is cheaper than internalizing them. Exit is theoretically available (advocacy fatigue) but constrained by moral and practical necessity to continue — cannot simply leave the field. Snare from organized perspective reflects structural asymmetry despite organizational capacity.
constraint_indexing:constraint_classification(carbon_externality, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: EMISSION ACCOUNTING (PITON) — Sophisticated measurement and reporting systems (IPCC methodologies, national inventories, corporate carbon accounting, Scope 1/2/3 frameworks) create theater of accountability without fully solving the externality. These systems perform precision and governance while the core externality persists — atmospheric carbon accumulation continues regardless of how well it is counted. Theater ratio increases as companies report ever-detailed climate disclosure while absolute emissions remain high (Goodhart drift: the metric substitutes for the outcome). Piton classification reflects degradation of the original coordination function (transparent accounting) into theater (disclosure without reduction).
constraint_indexing:constraint_classification(carbon_externality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a reductionist thermodynamic perspective, carbon externality appears as an immutable law: global carbon dioxide concentration increases with industrial energy use as a consequence of thermodynamics and atmospheric circulation. The cost-transfer appears natural (inevitable consequence of energy production) rather than institutional (choice to externalize rather than internalize). This false summit naturalizes a contingent institutional arrangement (choice not to price carbon) as a physical law. Perspective serves as diagnostic control.
constraint_indexing:constraint_classification(carbon_externality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(carbon_externality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(carbon_externality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(carbon_externality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(carbon_externality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(carbon_externality, TR),
    TR >= 0.70.

:- end_tests(carbon_externality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Very high. The constraint transfers enormous cost (estimated $900B-$3T annually in climate damages, depending on valuation methodology and discount rate) from those who create emissions to those who bear climate impacts. The cost transfer is structural and persistent — absent explicit pricing or regulation, externalization is the economically rational choice for any single actor. The extractiveness has increased over the 170-year interval as cumulative atmospheric carbon has risen from ~280 ppm (1850) to ~420 ppm (2020), increasing the implicit cost being externalized. Suppression (0.72): Extreme. Multiple layers: (1) Attribution opacity — supply chain fragmentation makes causality difficult to trace; (2) Temporal distance — damage is future, making present sacrifice politically difficult; (3) Institutional lock-in — carbon-intensive infrastructure (energy, transport, agriculture) creates path dependence; (4) Political influence — fossil fuel industries lobby against carbon pricing and regulation; (5) Collective action failure — no single actor can exit; requires coordinated global constraint. Theater ratio (0.58): Moderate-high. Over the 170-year interval, theater has increased dramatically. Early industrial period (1850-1900) had minimal acknowledgment of the externality; theater was ~0.15 (silent extraction). Post-IPCC (1995-present) period has sophisticated measurement, reporting, and voluntary commitment systems that create institutional appearance of constraint. However, these systems are substantially performative: corporate net-zero targets rely on offsetting and future reductions; national climate pledges lack enforcement mechanisms; emission accounting systems measure without reducing. Theater rose from 0.15 to 0.58 as the constraint shifted from silent extraction to managed extraction with governance theater. The theater is not false in the sense of being deceptive — the measurement and reporting are technically accurate — but it substitutes metric achievement (emissions reported, targets set) for outcome achievement (atmospheric carbon reduction).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range from pure snare to pure rope depending on observation point. For climate-vulnerable populations and future generations: absolute snare, maximum extraction, zero exit. For ecosystems: pure victim, no agency. For carbon-intensive industries: tangled rope or rope — genuine coordination function (energy supply) with real but constrained extraction (facing policy pressure and transition costs but capturing rents during transition). For renewable energy: rope — coordination mechanism without extraction. For the global analytical observer: risk of naturalizing the externality as a natural law (thermodynamic inevitability) rather than recognizing it as an institutional choice (failure to price or regulate carbon). The perspectival gap is extreme — from 'pure natural law' (mountain frame) to 'pure coercive extraction' (snare frame) depending on whether one focuses on physics or institutional choices. The critical perspective gap: those bearing costs see snare; those creating costs see rope or tangled rope; those profiting see rope with opportunity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the externality. Climate-vulnerable populations: d ≈ 0.95 (full targets, trapped exit, victim status) → f(d) ≈ 1.40 → high experienced extractiveness. Carbon-intensive industries: d ≈ 0.15 (beneficiary status, arbitrage exit, powerful position) → f(d) ≈ -0.01 → negative or minimal experienced extraction (they experience coordination, not extraction). Renewable energy: d ≈ 0.25 (beneficiary of transition, institutional exit) → f(d) ≈ 0.05 → minimal extraction. Scope modifier σ(S) = 1.2 (global) amplifies χ for all perspectives — global atmospheric commons means nowhere to exit. The scope scaling reveals that no actor can fully arbitrage away from the carbon externality; relocation only shifts exposure, not eliminates it.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is the FALSE MOUNTAIN threat. The constraint naturally generates pressure to classify it as mountain (natural law: thermodynamics requires carbon emission for energy production; physics is immutable). However, the schema gates prevent this classification: the extraction is contingent on institutional choice (not pricing carbon), not on physical law. The physical law is: burning fossil fuel releases CO2. The institutional choice is: do not require emitters to internalize the cost. These are distinct. The constraint becomes snare because of the institutional choice, not because of thermodynamics. The analytical mountain perspective serves as a diagnostic control, revealing the naturalization fallacy. The mandatrophy resolves by recognizing that (1) the physics is natural law (mountain), but (2) the externality is institutional (snare), and these are structurally different constraints that should be decomposed into two stories: one on thermodynamic limits of energy (mountain), one on carbon pricing architecture (snare). The snare story decouples physics from institutions, clarifying that the extraction is not inevitable but rather the result of specific policy choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_price_sufficiency,
    'What carbon price would fully internalize the externality and eliminate the snare structure?',
    'Social cost of carbon estimation (climate damages, ecosystem loss, health impacts); comparison with current market carbon prices and policy-set prices; cross-national variation in implied valuations',
    'If true SCC >> current prices: externality remains large snare. If true SCC ≈ current prices: policy has substantially closed extraction. If true SCC is unstable/contested: ambiguity enables continued extraction through price-setting authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_price_sufficiency, empirical, 'Whether carbon pricing mechanisms are sufficient to internalize atmospheric costs').

omega_variable(
    temporal_discount_rate,
    'What discount rate should apply to future climate damages when computing present-day extraction?',
    'Ethical and economic literature on intergenerational justice; empirical climate impact trajectories; comparison of different discount schemes (pure time preference, declining discount rates, equal weight to future generations)',
    'High discount rate (3-5%): future damage discounted heavily, current extraction appears smaller. Low discount rate (0-1%): future damage valued nearly equally, current extraction appears massive. This is not an empirical fact but a value choice that determines classification severity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_discount_rate, preference, 'Discount rate applied to future climate damages').

omega_variable(
    attribution_granularity,
    'Who is the ''beneficiary'' of carbon emissions — the emitter, the consumer, the shareholder, or the entire high-consumption lifestyle?',
    'Supply-chain attribution modeling; Scope 1/2/3 accounting verification; consumer goods lifecycle analysis; geographic tracing of who captures economic rents vs who bears costs',
    'If beneficiary = emitter: snare with clear extractor. If beneficiary = consumer: snare distributes across middle/high-income populations globally. If beneficiary = shareholder: extraction concentrates in capital-owning class. Different framings change who is ''powerful'' and who is ''victim'' in the perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_granularity, conceptual, 'Who captures benefits of carbon-intensive production and consumption').

omega_variable(
    voluntary_constraint_credibility,
    'Can the snare be converted to rope/scaffold through voluntary corporate or state commitment to carbon neutrality?',
    'Tracking corporate net-zero commitments vs actual emissions reductions; offsetting quality verification; policy lock-in analysis (whether climate commitments survive political turnover)',
    'If commitments are credible: snare transitions to scaffold with sunset clause (zero by 2050). If commitments are theater: classification remains snare, theater_ratio increases. If commitments break: snare becomes MORE severe (betrayal increases suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_constraint_credibility, empirical, 'Whether voluntary carbon commitments constitute genuine constraint evolution').

omega_variable(
    technological_decoupling,
    'Can economic growth decouple from carbon emissions at the global scale required (>5% annual reduction while maintaining growth)?',
    'Historical decoupling data (Denmark, UK, France renewable success); absolute vs relative decoupling analysis; global material throughput trends; rebound effect modeling (efficiency gains offset by increased consumption)',
    'If decoupling is feasible: snare has an engineered exit path, perspectives shift toward scaffold/rope. If decoupling is limited to high-income nations: global snare persists, with vulnerability concentration in low-income regions. If decoupling requires degrowth: conflicts with organized political economy of growth-dependent states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_decoupling, empirical, 'Whether technological decoupling can eliminate carbon externality at required scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(carbon_externality, 0, 170).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carb_theater_1850, carbon_externality, theater_ratio, 0, 0.15).
narrative_ontology:measurement(carb_theater_1950, carbon_externality, theater_ratio, 100, 0.25).
narrative_ontology:measurement(carb_theater_2000, carbon_externality, theater_ratio, 150, 0.48).
narrative_ontology:measurement(carb_theater_2020, carbon_externality, theater_ratio, 170, 0.58).

% Extraction over time
narrative_ontology:measurement(carb_extract_1850, carbon_externality, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(carb_extract_1950, carbon_externality, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(carb_extract_2000, carbon_externality, base_extractiveness, 150, 0.68).
narrative_ontology:measurement(carb_extract_2020, carbon_externality, base_extractiveness, 170, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(carbon_externality, resource_allocation).
narrative_ontology:affects_constraint(carbon_externality, fossil_fuel_subsidy_lock_in).
narrative_ontology:affects_constraint(carbon_externality, climate_adaptation_financing).
narrative_ontology:affects_constraint(carbon_externality, emissions_trading_scheme_design).
narrative_ontology:affects_constraint(carbon_externality, renewable_energy_grid_integration).

% DUAL FORMULATION NOTE:
% The carbon externality constraint family decomposes into: (1) thermodynamic_constraint_energy_production (ε=0.05, Mountain) — physics of carbon release from fossil fuels; (2) carbon_externality_cost_transfer (ε=0.68, Snare) — institutional choice not to internalize costs; (3) carbon_pricing_architecture (ε=0.55, Tangled Rope) — mechanisms to internalize externality. These are distinct constraints with different ε values. The snare story focuses on the institutional extraction mechanism, not the physics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(carbon_externality, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
