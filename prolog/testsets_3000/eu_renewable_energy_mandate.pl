% ============================================================================
% CONSTRAINT STORY: eu_renewable_energy_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: eu_renewable_energy_mandate
 *   human_readable: EU Renewable Energy Directive and Support Schemes
 *   domain: economic/political
 *
 * SUMMARY:
 *   The EU Renewable Energy Directive and national support schemes (feed-in
 *   tariffs, contracts for difference, investment grants) represent a complex
 *   hybrid constraint combining genuine coordination function (solving
 *   renewable investment certainty) with asymmetric extraction (passing
 *   integration costs to consumers and fossil fuel industries). The
 *   constraint exhibits all primary classification types from different
 *   structural positions. From the residential consumer perspective, it
 *   functions as a snare: mandatory grid connection and inability to exit
 *   creates a trapped population bearing electricity bill increases without
 *   negotiation power. From the renewable producer perspective, it functions
 *   as rope: access to guaranteed revenues solves the investment-certainty
 *   problem and enables rapid scaling. From grid operators, it functions as
 *   tangled rope: mandatory integration obligations plus balancing cost
 *   burdens, but also fee-based revenue streams. From fossil fuel industries,
 *   it functions as tangled rope: extraction via declining market share and
 *   stranded assets, but also benefit via capacity payments and grid
 *   stability contracts. From EU climate policy coalitions, it functions as
 *   scaffold: temporary support structures with explicit sunset logic
 *   (subsidy phase-outs scheduled for 2030+) and declining theater as market
 *   economics improve. From legacy coal authorities, it functions as piton:
 *   political maintenance through 'just transition' rhetoric despite
 *   functional degradation. The constraint's extractiveness has increased
 *   from 0.28 (2008, policy inception) to 0.52 (2023) as support schemes have
 *   expanded and consumer bill impacts have accumulated. Theater ratio has
 *   also increased from 0.42 to 0.58 as performative 'just transition'
 *   narrative has grown relative to actual grid stability function.
 *
 * KEY AGENTS:
 *   - Residential Electricity Consumers: Primary victims (powerless/trapped) — bear escalating electricity bills through support scheme cost pass-through; lack exit options from grid connection
 *   - Renewable Energy Producers: Primary beneficiaries (institutional/arbitrage) — capture feed-in tariffs, contracts for difference, and investment subsidies; high exit mobility through subsidy arbitrage
 *   - Regional Grid Operators: Secondary beneficiary-victim hybrid (moderate/constrained) — forced to integrate variable renewables while capturing service fees; constrained by infrastructure ownership
 *   - Fossil Fuel Industries: Secondary victims with partial benefits (organized/constrained) — face market share extraction and stranded asset risk, but capture capacity payments and grid stability revenues
 *   - EU Climate Policy Coalition: Organized actors (organized/constrained) — see renewable mandates as temporary support structures declining as market economics improve
 *   - Legacy Coal Power Authorities: Institutional actors (institutional/arbitrage) — maintain coal generation through political protection despite functional redundancy; high theater ratio
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy apparatus as inherent energy economics rather than contingent political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_renewable_energy_mandate, 0.52).
domain_priors:suppression_score(eu_renewable_energy_mandate, 0.48).
domain_priors:theater_ratio(eu_renewable_energy_mandate, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_renewable_energy_mandate, extractiveness, 0.52).
narrative_ontology:constraint_metric(eu_renewable_energy_mandate, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(eu_renewable_energy_mandate, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_renewable_energy_mandate, tangled_rope).
narrative_ontology:human_readable(eu_renewable_energy_mandate, "EU Renewable Energy Directive and Support Schemes").
narrative_ontology:topic_domain(eu_renewable_energy_mandate, "economic/political").

domain_priors:requires_active_enforcement(eu_renewable_energy_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_renewable_energy_mandate, renewable_energy_producers).
narrative_ontology:constraint_beneficiary(eu_renewable_energy_mandate, grid_infrastructure_operators).
narrative_ontology:constraint_beneficiary(eu_renewable_energy_mandate, eu_climate_objectives).
narrative_ontology:constraint_victim(eu_renewable_energy_mandate, fossil_fuel_industries).
narrative_ontology:constraint_victim(eu_renewable_energy_mandate, end_consumers_electricity_bills).
narrative_ontology:constraint_victim(eu_renewable_energy_mandate, grid_stability_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESIDENTIAL ELECTRICITY CONSUMER (SNARE) — Trapped by mandatory connection to grid and inability to opt out of the subsidy system. Experiences rising electricity bills to fund renewable support without meaningful exit option or negotiating power. Bears extraction through feed-in tariff costs and grid stabilization fees passed through to consumers.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL GRID OPERATOR (TANGLED ROPE) — Constrained by mandatory grid integration obligations and variable renewable intermittency. Benefits from infrastructure investment funding and operational fee revenues. Mixed extraction: forced to absorb balancing costs (victim) while capturing guaranteed service revenue (beneficiary). Exit options severely constrained by infrastructure ownership.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RENEWABLE ENERGY PRODUCER (ROPE) — Primary beneficiary with access to feed-in tariffs, contracts for difference, and investment subsidies. Experiences the constraint as coordination mechanism solving the investment certainty problem. High exit mobility through location arbitrage (building in highest-subsidy jurisdictions). Net beneficiary with organized agency.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FOSSIL FUEL INDUSTRY (TANGLED ROPE) — Constrained by mandatory renewable penetration targets and carbon pricing. Suffers extraction through reduced market share and stranded asset risk. Also benefits from infrastructure coordination (grid stability payments, capacity mechanism revenues). Organized agent with constrained but real exit options (geographic relocation, portfolio diversification into renewables).
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: EU CLIMATE POLICY COALITION (SCAFFOLD) — Organized agents (EU Commission, member state climate authorities, climate-aligned NGOs) see renewable mandates as temporary support structures with explicit sunset logic: as renewable economics improve and grid technology matures, subsidies should decline (legally scheduled phase-out of feed-in tariffs in 2030+). Theatre exists but is declining as market mechanisms replace mandates.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: LEGACY COAL POWER AUTHORITY (PITON) — Institutional actor maintaining coal generation through political advocacy and protection despite structural redundancy. Theater ratio high: compliance messaging about 'just transition' and 'stranded worker protection' persists while the actual economic function (baseload power provision) is being replaced by gas and renewables. Extraction mechanism degraded but inertially maintained through political coalition.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, renewable energy transitions are inherent to physics and thermodynamics: fossil fuels are finite, solar/wind are inexhaustible, and economics inevitably favor renewables at scale. This perspective risks naturalizing the EU policy apparatus as an immutable law of energy economics. However, base properties contradict mountain classification — the supportiveness, theater, and beneficiary/victim structure reveal contingent political choices, not natural law.
constraint_indexing:constraint_classification(eu_renewable_energy_mandate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   Extractiveness (0.52): Moderate-high, increasing over interval. The directive extracts from consumers and fossil industries but enables renewable producer benefits. Starting value (0.28 in 2008) reflected policy in early adoption phase with modest support levels. Current value (0.52) reflects accumulated effects: feed-in tariff costs have concentrated (highest in Germany, Denmark), consumer bill impacts now visible (€10-20/month average), and fossil fuel stranded assets are materializing. Suppression (0.48): Moderate. Significant barriers to consumer exit include regulatory grid monopolies, startup costs for alternative systems (storage, microgrids), and political resistance to prosumer markets. But suppression is not absolute — Germany's prosumer penetration (15%+) and emerging peer-to-peer energy trading show exit options are not completely closed. Theater ratio (0.58): Moderate-high and increasing. Feed-in tariff design involves genuine technical coordination (grid stability) but also performative elements (environmental messaging divorced from actual carbon accounting, 'just transition' rhetoric for coal regions that receive minimal retraining support). Theater increased from 0.42 to 0.58 as political justification narrative expanded while underlying cost-benefit analysis remained contested.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is maximal here: Renewable producers experience chi ≈ -0.12 × 0.52 × 1.1 ≈ -0.069 (negative extraction, pure coordination benefit). Residential consumers experience chi ≈ 1.42 × 0.52 × 1.1 ≈ 0.814 (severe extraction, maximum snare signature). This gap — from strong beneficiary to maximal victim — is precisely what triggers Tangled Rope classification at the meso-level (grid operators, fossil industries, policy coalitions). The constraint is neither pure rope (would require all perspectives to see coordination benefit) nor pure snare (would require all perspectives to see extraction). It is hybrid: real coordination function (solving renewable investment certainty) combined with asymmetric extraction (passing integration costs to consumers and carbon-dependent industries).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from structural position: power level, exit options, and relationship to extraction flow. Residential consumers (powerless/trapped) derive d ≈ 0.95, experiencing near-maximal extraction via f(d) ≈ 1.42. Renewable producers (institutional/arbitrage) derive d ≈ 0.05, experiencing negative extraction (subsidy flow toward them) via f(d) ≈ -0.12. Grid operators (moderate/constrained) derive d ≈ 0.55-0.60, experiencing moderate extraction with partial benefit via f(d) ≈ 0.75-0.85. Fossil fuel industries (organized/constrained) derive d ≈ 0.65, experiencing moderate-to-high extraction via f(d) ≈ 1.00-1.15 from market share loss, partially offset by capacity payment benefits. EU climate coalition (organized/constrained with organized agency in policy design) derives d ≈ 0.40-0.50, experiencing moderate extraction mediated by policy control. Coal authorities (institutional/arbitrage in policy, but constrained in market) derive d ≈ 0.45-0.55 depending on whether political protection is viewed as arbitrage (regulatory capture) or constraint (inevitable decline). Each agent's d feeds into chi = ε × f(d) × σ(S), where scope modifiers (σ) reflect that EU-scale coordination (σ=1.1) amplifies extractiveness relative to local mechanisms (σ=0.8).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (risk of mislabeling extraction as coordination or vice versa) is partially but not fully resolved here. The RESOLVED claim (mandatrophy_resolved: false) indicates ongoing ambiguity. Core tension: Is the directive a coordination mechanism solving a market failure (renewable investment certainty, grid integration externalities) or an extraction mechanism benefiting renewable producers and grid operators at consumer expense? Resolution requires empirical answer to omega variables. If subsidy sufficiency threshold < 40% penetration, the mandate becomes pure extraction post-threshold. If grid stability costs are properly internalized, the coordination function is visible and tangled rope is correct. If political capture is deep, sunset mechanisms degrade and piton classification emerges. If consumer exit options remain infeasible, snare classification dominates. The JSON declares mandatrophy_resolved: false because the empirical status of these omega variables is contested, and classification could shift with resolution. Current classification (Tangled Rope, claimed_type) reflects that BOTH coordination function AND asymmetric extraction are structurally present, but their relative weights are uncertain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_sufficiency_threshold,
    'What level of renewable penetration (%) removes the need for active subsidy enforcement?',
    'Cost-benefit analysis: levelized cost of renewable energy (LCOE) vs fossil fuels; grid stability cost curves; economic modeling of merchant power market viability',
    'If threshold < 40% penetration: mandates become pure extraction mechanism (Snare dominates). If threshold > 60%: mandates are legitimately temporary (Scaffold dominates). Current EU target is 42.5% by 2030.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_sufficiency_threshold, empirical, 'Renewable penetration threshold for subsidy-free viability').

omega_variable(
    grid_stability_cost_internalization,
    'Are the true costs of grid balancing and storage infrastructure properly allocated to renewable integration, or hidden in general transmission costs?',
    'System-level cost accounting: comparison of grid balancing costs in high-renewable vs low-renewable jurisdictions; attribution analysis of infrastructure investment drivers',
    'If properly allocated: supports tangled rope classification (both extraction and coordination visible). If hidden: renewable producers capture externalized costs → snare for consumers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grid_stability_cost_internalization, empirical, 'Whether grid stability costs are properly internalized in renewable subsidy calculations').

omega_variable(
    political_capture_depth,
    'To what extent have renewable energy subsidies become rent-seeking mechanisms captured by incumbent renewable producers and grid operators?',
    'Policy analysis: subsidy design stability vs market evolution; comparison of support intensity vs actual investment barriers; revolving-door analysis of EU/member state energy officials',
    'If capture is shallow: scaffold sunset mechanisms remain functional. If capture is deep: subsidies persist indefinitely despite improving economics → Piton degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_capture_depth, empirical, 'Degree of political capture in renewable subsidy design').

omega_variable(
    consumer_exit_option_feasibility,
    'Can residential consumers meaningfully exit the electricity grid or organize collective alternatives (prosumers, microgrids, storage)?',
    'Technological analysis: storage cost curves, microgrid economics, distributed generation feasibility; policy analysis: regulatory barriers to consumer exit',
    'If exit is infeasible: consumer perspective remains Snare (trapped). If exit becomes feasible: consumers transition to Tangled Rope or escape the constraint entirely → classification shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consumer_exit_option_feasibility, empirical, 'Feasibility of consumer exit options from centralized grid subsidy system').

omega_variable(
    fossil_fuel_stranded_asset_timeline,
    'What is the actual economic lifespan of existing fossil fuel infrastructure before stranded asset losses exceed recoverable returns?',
    'Financial analysis: NPV modeling of coal/gas plants vs renewable alternatives; regulatory timeline for plant decommissioning; break-even analysis under EU carbon pricing',
    'If timeline < 10 years: fossil fuel industry faces maximal extraction (Snare dominates). If timeline > 20 years: transition period allows portfolio diversification → Tangled Rope more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_fuel_stranded_asset_timeline, empirical, 'Economic stranded asset timeline for fossil fuel infrastructure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_renewable_energy_mandate, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eurem_tr_t0, eu_renewable_energy_mandate, theater_ratio, 0, 0.42).
narrative_ontology:measurement(eurem_tr_t7, eu_renewable_energy_mandate, theater_ratio, 7, 0.52).
narrative_ontology:measurement(eurem_tr_t15, eu_renewable_energy_mandate, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(eurem_be_t0, eu_renewable_energy_mandate, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(eurem_be_t7, eu_renewable_energy_mandate, base_extractiveness, 7, 0.4).
narrative_ontology:measurement(eurem_be_t15, eu_renewable_energy_mandate, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_renewable_energy_mandate, resource_allocation).
narrative_ontology:boltzmann_floor_override(eu_renewable_energy_mandate, 0.35).
narrative_ontology:affects_constraint(eu_renewable_energy_mandate, eu_carbon_pricing_mechanism).
narrative_ontology:affects_constraint(eu_renewable_energy_mandate, fossil_fuel_subsidy_phase_out).
narrative_ontology:affects_constraint(eu_renewable_energy_mandate, grid_infrastructure_investment_coordination).

% DUAL FORMULATION NOTE:
% The EU renewable mandate comprises two structurally distinct claims: (1) renewable penetration targets as coordination mechanism for technology deployment (ε ≈ 0.25, Rope-dominant), and (2) support scheme design as extraction mechanism for producer benefit (ε ≈ 0.65, Snare-dominant). These could be decomposed into separate stories, but current analysis treats them as unified because policy design intentionally combines both functions. Upstream constraints (carbon pricing, fossil fuel subsidies) constrain the support scheme design space. Downstream constraints (grid infrastructure investment) are driven by renewable penetration targets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_renewable_energy_mandate, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
