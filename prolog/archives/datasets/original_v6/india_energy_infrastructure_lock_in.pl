% ============================================================================
% CONSTRAINT STORY: india_energy_infrastructure_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_india_energy_infrastructure_lock_in, []).

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
 *   constraint_id: india_energy_infrastructure_lock_in
 *   human_readable: India Energy Infrastructure Lock-In: Coal Dependency and Transition Friction
 *   domain: economic_political/energy_infrastructure
 *
 * SUMMARY:
 *   India's energy infrastructure lock-in represents a tangled
 *   coordination-extraction hybrid spanning decades of thermal power plant
 *   deployment, coal mining expansion, and regulatory design around coal
 *   supply. The constraint coordinates legitimate energy access and
 *   industrial capacity (rope function) while simultaneously extracting from
 *   renewables sector development, rural electrification equity, and climate
 *   adaptation capacity. Coal mining interests and thermal operators benefit
 *   from long-term power purchase agreements, capacity utilization
 *   guarantees, and system-wide dispatch priority. Renewable energy
 *   developers, rural communities without reliable grid access, and future
 *   climate adaptation capacity bear asymmetric costs. The constraint
 *   exhibits all hall-marks of institutional lock-in: sunk capital in thermal
 *   plants and coal mines, contractual obligations extending 15-20 years,
 *   workforce dependencies, political economy of coal-producing regions, and
 *   regulatory architecture designed for coal-centric dispatch.
 *   Extractiveness has increased from 0.42 (2010s) to 0.58 (present) as
 *   renewable deployment has accelerated, generating conflicts with coal
 *   plant utilization and creating visible stranded asset problems. Theater
 *   ratio has risen as policy apparatus intensifies performative compliance
 *   with renewable targets (capacity auction mandates, green energy
 *   certification) while maintaining coal plant dispatch priority. The
 *   constraint contains a genuine scaffold component: India's 2030 renewable
 *   energy targets and Paris Agreement commitments create explicit sunset
 *   logic, with capital flows and supply chains increasingly conditional on
 *   transition progress.
 *
 * KEY AGENTS:
 *   - Coal Mining Companies & Thermal Power Operators: Primary beneficiary (institutional/arbitrage) — capture stable long-term contracts, preferential dispatch, portfolio concentration benefits during lock-in period
 *   - Rural Communities Without Reliable Grid Access: Primary victim (powerless/trapped) — geographic isolation, income constraints, and grid design biased toward thermal baseload make alternatives inaccessible; trapped in intermittent/absent access
 *   - Renewable Energy Developers: Secondary victim (moderate/constrained) — face grid interconnection barriers, dispatch priority disadvantages, and financing constraints; benefit from renewable mandates but constrained by institutional barriers
 *   - State Distribution Companies (Discoms): Institutional victim-beneficiary (institutional/constrained) — benefit from stable coal supply costs but trapped by cross-subsidization politics, stranded thermal assets, and capital constraints for grid modernization
 *   - Coal Ministry & Mining Ministry: Institutional beneficiary (institutional/arbitrage) — maintain regulatory dominance and coal supply coordination; can exit through portfolio diversification but benefit from status quo
 *   - International Climate Actors & Green Capital: Organized beneficiary (organized/constrained) — organized pressure for transition with explicit sunset (2030 targets), benefits from renewable market growth but constrained by Indian institutional resistance
 *   - Grid Flexibility & Future Climate Capacity: Victim abstraction (powerless/trapped) — abstract goods bearing cost of locked-in thermal dependency; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(india_energy_infrastructure_lock_in, 0.58).
domain_priors:suppression_score(india_energy_infrastructure_lock_in, 0.62).
domain_priors:theater_ratio(india_energy_infrastructure_lock_in, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(india_energy_infrastructure_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(india_energy_infrastructure_lock_in, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(india_energy_infrastructure_lock_in, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(india_energy_infrastructure_lock_in, tangled_rope).
narrative_ontology:human_readable(india_energy_infrastructure_lock_in, "India Energy Infrastructure Lock-In: Coal Dependency and Transition Friction").
narrative_ontology:topic_domain(india_energy_infrastructure_lock_in, "economic_political/energy_infrastructure").

domain_priors:requires_active_enforcement(india_energy_infrastructure_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(india_energy_infrastructure_lock_in, coal_mining_interests).
narrative_ontology:constraint_beneficiary(india_energy_infrastructure_lock_in, thermal_power_operators).
narrative_ontology:constraint_beneficiary(india_energy_infrastructure_lock_in, incumbent_utilities).
narrative_ontology:constraint_victim(india_energy_infrastructure_lock_in, renewable_energy_sector).
narrative_ontology:constraint_victim(india_energy_infrastructure_lock_in, future_climate_adaptation_capacity).
narrative_ontology:constraint_victim(india_energy_infrastructure_lock_in, low_income_rural_communities).
narrative_ontology:constraint_victim(india_energy_infrastructure_lock_in, grid_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL COMMUNITIES (SNARE) — Trapped by geographic isolation and income constraints. Coal-dependent grid infrastructure provides only intermittent, unreliable access. Cannot exit the constraint through market mechanisms; cannot afford distributed renewable alternatives at current capital costs. Maximum experienced extraction with no alternatives within biographical timeframe.
constraint_indexing:constraint_classification(india_energy_infrastructure_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RENEWABLE ENERGY DEVELOPERS (TANGLED ROPE) — Structurally mobile but constrained by grid integration barriers, interconnection delays, and dispatch priority rules favoring coal plants. Benefits from renewable energy mandates and declining equipment costs, but faces real barriers to market entry. Mixed coordination (grid access) with asymmetric extraction (priority dispatch rules).
constraint_indexing:constraint_classification(india_energy_infrastructure_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COAL MINING COMPANIES & THERMAL OPERATORS (ROPE) — Experience the constraint as coordination mechanism enabling their business model. Benefit from long-term power purchase agreements, capacity utilization guarantees, and preferential dispatch. Can exit through portfolio diversification but choose not to; coordination benefits exceed exit costs. Net beneficiary position.
constraint_indexing:constraint_classification(india_energy_infrastructure_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE DISCOMS (TANGLED ROPE) — Institutional actors structurally captured by cross-subsidization and cost-recovery politics. Benefit from stable coal-based supply (low price volatility, predictable costs); constrained by political pressure on tariffs, stranded assets in coal plants, and lack of capital for grid modernization. Mixed experience: genuine coordination benefits alongside asymmetric extraction from stranded cost obligations.
constraint_indexing:constraint_classification(india_energy_infrastructure_lock_in, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CLIMATE COMMITMENTS & GREEN MANDATE (SCAFFOLD) — India's 2030 renewable capacity target (500 GW) and Paris Agreement commitments create organized pressure for transition. High suppression currently (coal lobby, sunk costs in thermal plants), but sunset logic is explicit: renewable targets have fixed dates, international capital flows depend on progress, global supply chains for solar/wind equipment create competitive advantage for early adopters. Temporary constraint with declining suppression over generational timescale.
constraint_indexing:constraint_classification(india_energy_infrastructure_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COAL MINISTRY APPARATUS (PITON) — Institutional structure that has atrophied in functional capacity relative to energy transition needs. Maintains regulatory theater (energy audits, efficiency standards, clean coal technology mandates) while primary function (coal supply coordination) persists through inertia. Theater ratio high (0.58) — substantial performative compliance without commensurate functional change. Institutional identity fused with coal logistics; exists because alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(india_energy_infrastructure_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: PHYSICAL ENERGY DENSITY (FALSE SUMMIT) — Risk of naturalizing a contingent infrastructure choice as physical law. Coal energy density is real, but the constraint is not the physics; it's the institutional lock-in that treats coal as inevitable because infrastructure was designed for it decades ago. This perspective will be flagged as a false summit — the mountain classification naturalizes what is actually a tangled_rope at all inhabited perspectives.
constraint_indexing:constraint_classification(india_energy_infrastructure_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_energy_infrastructure_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(india_energy_infrastructure_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(india_energy_infrastructure_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(india_energy_infrastructure_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(india_energy_infrastructure_lock_in, TR),
    TR >= 0.70.

:- end_tests(india_energy_infrastructure_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts from renewable deployment (delay of 5-10 years), rural electrification (substandard service quality), and climate adaptation (sunk carbon from extended thermal plant lifespans). The rise from 0.42 to 0.58 reflects that as renewables become cheaper and more deployable, the extraction mechanism intensifies — incumbent actors increase regulatory pressure and dispatch priority rules to protect thermal utilization. This is not declining with technological change (as pure coordination would), but increasing. The metric captures the widening gap between economically rational renewable deployment and politically enforced coal utilization. Suppression (0.62): Moderate-high. Barriers include: sunk capital (150+ GW thermal capacity), long-term power purchase agreements (enforceable contracts extending 15-20 years), coal worker employment (concentrated in coal-producing regions with limited alternative employment), and regulatory architecture (dispatch rules, grid codes, environmental standards) designed around coal-centric operation. Exit for thermal operators requires capital write-downs and political permission; exit for renewable developers requires navigating interconnection queues and dispatch rules; exit for rural communities requires alternative electricity sources at accessible costs. Theater ratio (0.58): Increasing. Performance indicators include renewable capacity targets (met through auctions), efficiency standards (applied to coal plants), clean energy mandates (met through green certificates), and energy audits — substantial regulatory activity without commensurate functional change in coal dispatch priority. Theater has risen as political accountability for renewable targets has increased while institutional capacity to enforce coal exit has remained negligible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Coal operators see rope — coordination of energy access and industrial power with beneficial long-term contracts. Renewable developers see tangled rope — real market opportunities blocked by institutional barriers and preferential dispatch. Rural communities see snare — trapped without viable exit options. Discoms see tangled rope — stable coal costs (benefit) but stranded assets and political tariff pressure (extraction). International climate actors see scaffold — explicit sunset dates and capital flows dependent on progress, with suppression declining toward 2030. Coal ministry sees piton — institutional ritual persists through inertia despite loss of functional primacy. Analytical observer risks mountain classification (treating coal as inevitable for grid stability) that will be flagged as naturalization of contingent infrastructure design. No single perspective is 'wrong' — each captures the constraint as experienced from that structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's relationship to the extraction flow. Coal companies and thermal operators benefit (d ≈ 0.10-0.15, low directionality toward extraction); renewable developers are victims (d ≈ 0.65, moderate-high toward extraction); rural communities are trapped victims (d ≈ 0.92, maximum toward extraction); discoms are mixed (d ≈ 0.55, moderate toward extraction due to constrained position and stranded costs); organized climate actors are beneficiaries with power (d ≈ 0.35, moderate toward but constrained by institutional resistance). The sigmoid f(d) function then modulates these into effective extractiveness chi. The piton perspective derives from theater ratio (0.58) exceeding functional necessity, not from high chi. The false summit perspective is an analytical risk — grid stability constraints are real, but the constraint is not physical; it is the institutional lock-in treating coal as essential.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION VIA PERSPECTIVAL DECOMPOSITION: The tangled rope classification is validated by the presence of both genuine coordination (energy access, industrial power supply) and asymmetric extraction (coal lock-in, renewable delay, rural service quality). Beneficiaries and victims are clearly distinguished: coal sector benefits, renewables and rural access bear costs. Active enforcement is present (dispatch rules, grid codes, interconnection barriers). The constraint resolves mandatrophy by showing that neither pure coordination (rope) nor pure extraction (snare) captures the structure — the lock-in simultaneously enables energy access (would be snare without this) and extracts through artificial preservation of high-cost thermal capacity (would be rope without this). The measurement trajectory (extractiveness rising, theater increasing) confirms that the coordination function is weakening relative to the extraction mechanism as renewable alternatives mature. The scaffold perspective provides the sunset mechanism: explicit 2030 renewable targets with capital flows conditional on progress suggest that suppression is expected to decline over 10-15 year timescale, potentially transitioning the constraint from tangled rope toward rope (as coal becomes residual) or toward snare (if stranded costs cascade into new extraction layers). The piton perspective is a warning signal — institutional inertia may prevent the intended transition even after suppression declines, creating a degraded snare rather than a clean rope handoff.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grid_stability_requirement_scope,
    'What fraction of current grid stability difficulties are genuinely caused by coal-free operation vs. caused by grid technical design for thermal baseload generation?',
    'Comparative grid stability analysis from high-renewable-penetration grids (Denmark 80% wind, South Australia 60% variable renewables); technical audits of required vs. claimed stability margins in Indian grids',
    'If genuine requirement: grid modernization costs are real and substantial, raising exit costs for renewable transition. If mostly design artifact: perceived stability requirement is performative theater, and the suppression metric is partly illusory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_stability_requirement_scope, empirical, 'Actual vs. perceived grid stability requirements for coal-free operation').

omega_variable(
    capital_cost_trajectory_inflection,
    'At what renewable capacity deployment level do solar/wind supply chains achieve cost dominance making thermal retirement economically rational without policy intervention?',
    'Long-term cost modeling with learning curve parameters from manufacturing scale; threshold analysis for economic retirement of thermal plants',
    'If inflection point < 300 GW: market forces will drive transition within 10-15 years, scaffold perspective dominates. If inflection > 500 GW: transition requires active policy extraction (maintaining suppression), snare perspective dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_cost_trajectory_inflection, empirical, 'Economic inflection point for thermal plant retirement').

omega_variable(
    stranded_asset_political_spillover,
    'Will thermal power asset write-downs (2-3 trillion INR cumulative) trigger fiscal strain on state discoms that forces renationalization or bailout, creating new extraction layers?',
    'Fiscal impact modeling of stranded asset recognition; comparative case study of other energy transitions (Germany Energiewende coal closures, Australia coal region transition)',
    'If yes: stranded costs become externalized through bailout, creating new snare constraint on general taxpayers. If no: extraction remains localized to coal sector. Either way, suppression increases during transition phase.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stranded_asset_political_spillover, empirical, 'Political spillover from thermal plant asset write-downs').

omega_variable(
    distributed_renewable_autonomy_capture,
    'Do distributed rooftop solar and microgrids provide genuine exit option for rural communities, or do interconnection requirements and state discom revenue protection create new extraction layers?',
    'Longitudinal tracking of net metering policy erosion; comparison of net metering tariff structures across states; analysis of interconnection queue wait times vs. renewable deployment',
    'If genuine exit option: rural communities move from trapped to mobile, classification changes. If captured by new extraction: rural constraint persists with different mechanism, theater ratio increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_renewable_autonomy_capture, empirical, 'Whether distributed renewables provide genuine autonomy or new extraction layer').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(india_energy_infrastructure_lock_in, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ieili_tr_t0, india_energy_infrastructure_lock_in, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ieili_tr_t5, india_energy_infrastructure_lock_in, theater_ratio, 5, 0.52).
narrative_ontology:measurement(ieili_tr_t10, india_energy_infrastructure_lock_in, theater_ratio, 10, 0.58).
narrative_ontology:measurement(ieili_tr_t15, india_energy_infrastructure_lock_in, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(ieili_be_t0, india_energy_infrastructure_lock_in, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ieili_be_t5, india_energy_infrastructure_lock_in, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(ieili_be_t10, india_energy_infrastructure_lock_in, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ieili_be_t15, india_energy_infrastructure_lock_in, base_extractiveness, 15, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(india_energy_infrastructure_lock_in, resource_allocation).
narrative_ontology:affects_constraint(india_energy_infrastructure_lock_in, indian_renewable_manufacturing_capacity).
narrative_ontology:affects_constraint(india_energy_infrastructure_lock_in, coal_worker_transition_livelihoods).
narrative_ontology:affects_constraint(india_energy_infrastructure_lock_in, grid_interconnection_bottleneck).
narrative_ontology:affects_constraint(india_energy_infrastructure_lock_in, discoms_financial_sustainability).
narrative_ontology:affects_constraint(india_energy_infrastructure_lock_in, rural_electrification_equity).

% DUAL FORMULATION NOTE:
% India's energy infrastructure lock-in is decomposable into multiple structurally distinct constraints with different ε values. The present story captures the macroeconomic lock-in (coal vs. renewables system-level competition, ε=0.58). Downstream constraints include grid technical architecture (renewable integration barriers, ε=0.40-0.50), coal worker transition (livelihood extraction, ε=0.60-0.70), and discom fiscal strain (financial extraction, ε=0.55-0.65). Each has distinct perspectives and resolution pathways but all are influenced by the macroeconomic lock-in. This story models the system-level constraint; linked stories address specific mechanism layers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(india_energy_infrastructure_lock_in, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
