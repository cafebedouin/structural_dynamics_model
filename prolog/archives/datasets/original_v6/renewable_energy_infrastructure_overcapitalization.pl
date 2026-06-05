% ============================================================================
% CONSTRAINT STORY: renewable_energy_infrastructure_overcapitalization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_renewable_energy_infrastructure_overcapitalization, []).

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
 *   constraint_id: renewable_energy_infrastructure_overcapitalization
 *   human_readable: Renewable Energy Infrastructure Overcapitalization
 *   domain: energy/economic_policy/climate_finance
 *
 * SUMMARY:
 *   Renewable energy infrastructure overcapitalization arises from the
 *   combination of binding policy targets for installed capacity, financial
 *   incentive structures that reward deployment volume over optimization, and
 *   the absence of clear performance metrics tied to actual carbon reduction
 *   or grid reliability. The constraint exhibits extraction masquerading as
 *   climate coordination. Policymakers set binding renewable energy targets
 *   (e.g., '50% renewable energy by 2035') measured in installed capacity
 *   rather than actual energy production or system efficiency. Equipment
 *   manufacturers, project developers, and financial intermediaries benefit
 *   from this measurement choice because capacity targets decouple from real
 *   operational requirements — grids are incentivized to install more than is
 *   optimal given actual demand, storage capacity, and grid characteristics.
 *   Electricity ratepayers bear the cost through inflated rates. Grid
 *   operators face extraction (forced overcapitalization) alongside genuine
 *   coordination challenges. The constraint's theater ratio has increased as
 *   compliance becomes increasingly about visible construction projects
 *   rather than measured climate outcomes. Open challenges are whether this
 *   is rational portfolio insurance against uncertain future demand, or
 *   whether it is pure rent extraction. The scaffold perspective recognizes
 *   that advanced grid technologies (storage, demand-response, forecasting)
 *   are creating alternative mechanisms that will make overcapitalization
 *   economically irrational within 10-15 years.
 *
 * KEY AGENTS:
 *   - Equipment Manufacturers: Primary beneficiary (institutional/arbitrage) — directly benefit from capacity installation targets; can exit through market arbitrage between jurisdictions
 *   - Project Developers: Primary beneficiary (institutional/arbitrage) — benefit from deployment volume incentives; can arbitrage across regulatory jurisdictions
 *   - Financial Intermediaries: Secondary beneficiary (institutional/constrained) — extract through project financing; constrained by regulatory framework
 *   - Grid Operators: Mixed role (organized/constrained) — face genuine coordination challenges (variable renewable management) alongside extraction (forced overcapitalization); constrained by grid stability obligations and regulatory requirements
 *   - Electricity Ratepayers: Primary victim (powerless/trapped) — bear cost of overcapitalized infrastructure through rates; trapped in jurisdictional monopoly electricity markets with no exit option
 *   - Systems Optimization Future: Emerging actor (analytical/analytical) — grid modeling and technology advancement creating alternative coordination mechanisms with lower cost structures
 *   - Green Finance Coalition: Beneficiary-victim hybrid (organized/constrained) — benefits from volume targets and deployment incentives; constrained by dependence on policy mandates continuing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(renewable_energy_infrastructure_overcapitalization, 0.58).
domain_priors:suppression_score(renewable_energy_infrastructure_overcapitalization, 0.52).
domain_priors:theater_ratio(renewable_energy_infrastructure_overcapitalization, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(renewable_energy_infrastructure_overcapitalization, extractiveness, 0.58).
narrative_ontology:constraint_metric(renewable_energy_infrastructure_overcapitalization, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(renewable_energy_infrastructure_overcapitalization, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(renewable_energy_infrastructure_overcapitalization, tangled_rope).
narrative_ontology:human_readable(renewable_energy_infrastructure_overcapitalization, "Renewable Energy Infrastructure Overcapitalization").
narrative_ontology:topic_domain(renewable_energy_infrastructure_overcapitalization, "energy/economic_policy/climate_finance").

domain_priors:requires_active_enforcement(renewable_energy_infrastructure_overcapitalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(renewable_energy_infrastructure_overcapitalization, equipment_manufacturers).
narrative_ontology:constraint_beneficiary(renewable_energy_infrastructure_overcapitalization, project_developers).
narrative_ontology:constraint_beneficiary(renewable_energy_infrastructure_overcapitalization, financial_intermediaries).
narrative_ontology:constraint_beneficiary(renewable_energy_infrastructure_overcapitalization, grid_operators).
narrative_ontology:constraint_victim(renewable_energy_infrastructure_overcapitalization, electricity_ratepayers).
narrative_ontology:constraint_victim(renewable_energy_infrastructure_overcapitalization, grid_reliability).
narrative_ontology:constraint_victim(renewable_energy_infrastructure_overcapitalization, systems_optimization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELECTRICITY RATEPAYER (SNARE) — Trapped in jurisdictional electricity market with no exit option. Bears the cost of overcapitalized infrastructure through rates that exceed actual operational need. Cannot opt out or switch to competitor systems. Maximum extraction from powerless agent with no alternatives.
constraint_indexing:constraint_classification(renewable_energy_infrastructure_overcapitalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GRID OPERATOR (TANGLED ROPE) — Experiences genuine coordination function: managing variable renewable sources requires operational flexibility, reserve capacity, and investment in grid modernization. Also experiences extraction: policy mandates for capacity installation exceed optimization requirements, forcing costly over-deployment. Constrained exit due to regulatory obligations and infrastructure lock-in.
constraint_indexing:constraint_classification(renewable_energy_infrastructure_overcapitalization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EQUIPMENT MANUFACTURER (ROPE) — Primary beneficiary (institutional/arbitrage). Experiences the constraint as pure coordination: installation targets align perfectly with profitable deployment. Can arbitrage between markets with different overcapitalization thresholds. Net beneficiary with maximum exit agency.
constraint_indexing:constraint_classification(renewable_energy_infrastructure_overcapitalization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GREEN FINANCE COALITION (TANGLED ROPE) — Organized institutional actors (development banks, climate funds, ESG investors) see genuine coordination benefit: mobilizing private capital for climate transitions requires clear investment signals and volume targets. Also extract: overcapitalization drives inflated returns on deployed capital and extends project timelines, creating fee opportunities. Constrained because coalition members depend on policy mandates continuing.
constraint_indexing:constraint_classification(renewable_energy_infrastructure_overcapitalization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RENEWABLE ENERGY MANDATE (PITON) — Original function was carbon reduction coordination. Current operation is substantially performative: numerical targets (e.g., 50% renewable by 2035) are measured by installed capacity, not by actual energy production or grid effectiveness. Mandate persists through regulatory inertia. Theater ratio high because compliance is staged through visible construction rather than measured by efficiency outcomes. Actual carbon reduction per installed watt has declined as capacity additions have saturated.
constraint_indexing:constraint_classification(renewable_energy_infrastructure_overcapitalization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SYSTEMS OPTIMIZATION APPROACH (SCAFFOLD) — Analytical perspective recognizing overcapitalization as a temporary problem with a sunset. Grid modeling, demand-response, energy storage, and advanced forecasting are creating alternative mechanisms for managing variable renewable sources without excess capacity. As these technologies mature, overcapitalization will become economically irrational. Sunset estimated at 10-15 years as storage costs decline and smart grid infrastructure deploys.
constraint_indexing:constraint_classification(renewable_energy_infrastructure_overcapitalization, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(renewable_energy_infrastructure_overcapitalization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(renewable_energy_infrastructure_overcapitalization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(renewable_energy_infrastructure_overcapitalization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(renewable_energy_infrastructure_overcapitalization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(renewable_energy_infrastructure_overcapitalization, TR),
    TR >= 0.70.

:- end_tests(renewable_energy_infrastructure_overcapitalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting accumulating overcapitalization costs. The initial 0.32 reflects a genuine climate coordination need in 2010s when policy targets were first set. As installed capacity has grown beyond optimization requirements (measured 0.58 in current period), extraction has increased — ratepayers pay for capacity they do not use, while beneficiaries profit from excess deployment. Suppression (0.52): Moderate. Ratepayers have structural barriers to exit (monopoly electricity markets, geographic immobility, essential service), but some capacity exists for demand-side response and rooftop solar adoption. Grid operators face regulatory suppression (mandates) but retain operational flexibility. Theater ratio (0.64): High and increasing. Renewable energy targets are measured by installed megawatts, not by actual energy production, emissions reduction, or grid reliability outcomes. This creates systematic misalignment between measured compliance and climate/grid outcomes. A grid that installs 100 GW but generates only 30% of annual energy from renewables (due to capacity factor, curtailment, and variable production patterns) appears compliant while producing minimal incremental carbon reduction. Theater has grown as policy targets have approached saturation and distinction between capacity and productivity has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits strong perspectival divergence reflecting asymmetric extraction. Equipment manufacturers see Rope (pure coordination: capacity targets create predictable demand for equipment, enabling rational capacity planning). Project developers see Rope (capacity targets create deployable investment opportunities). Ratepayers see Snare (forced cost burden with no exit). Grid operators see Tangled Rope (genuine coordination function — managing variable renewable sources — alongside forced overcapitalization costs). Financial intermediaries see Rope (volume targets guarantee project pipeline and financing opportunities). The green finance coalition sees Tangled Rope (genuine climate coordination alongside rent extraction). The mandate itself appears as Piton to the civilizational analytical observer — its original function (carbon reduction) has atrophied while it persists through regulatory inertia, measured by performative capacity targets. The systems optimization perspective sees a Scaffold (temporary problem with a sunset as technology advances). The perspectival gap reveals the core extraction mechanism: the constraint's beneficiaries experience it as pure coordination (Rope from their perspectives), while its victims experience it as forced cost (Snare from powerless perspectives) or mixed coordination-extraction (Tangled Rope from moderate positions).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation maps each agent's structural relationship to the constraint. Beneficiaries (equipment manufacturers, project developers) have institutional power and arbitrage exit options — they derive low d values (~0.10-0.20), experiencing negative effective extraction (the constraint subsidizes them). Ratepayers have powerless position and trapped exit — they derive high d values (~0.95), experiencing maximum effective extraction from the constraint. Grid operators occupy a hybrid position: they are both coordinators (managing variable renewable integration is a genuine coordination function) and victims (forced overcapitalization is extraction). Their moderate power level and constrained exit produce mid-range d values (~0.55-0.65), yielding tangled rope classification at the agent's structural position. Financial intermediaries are organized beneficiaries with constrained rather than arbitrage exit — they derive moderate d values (~0.30-0.40), producing rope classification. The perspectival gap emerges because beneficiaries see coordination (Rope) while victims and the analytical observer see mixed coordination-extraction (Tangled Rope) or pure extraction (Snare).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by identifying the specific measurement asymmetry that creates extraction. The tension is not between 'is this coordination or extraction?' but 'at what spatial and temporal scale should we measure success?' At the policy level, success is measured as 'percentage of electricity from renewable sources' or 'installed renewable capacity.' At the grid operations level, success should be measured as 'total emissions reduction per dollar invested' or 'grid reliability and resilience metrics.' These measurements frequently diverge: a jurisdiction can reach 50% installed renewable capacity while actually producing 30-35% of annual electricity from renewables (due to capacity factors, curtailment, and variable production patterns). The mandatrophy resolution requires measuring whether the policy's stated goal (carbon reduction, climate stabilization) is actually achieved by its claimed mechanism (capacity installation targets). Comparative analysis across grids with different policies would show that some jurisdictions achieve equivalent or superior climate outcomes with lower capacity installation levels through demand-side management, storage deployment, and grid optimization. This reveals that overcapitalization is not necessary for the policy's stated goal — it is profitable for beneficiaries but not optimal for the climate objective. The constraint is therefore Tangled Rope (not pure Rope): it coordinates capital deployment for renewable energy while simultaneously extracting through enforced overcapitalization relative to actual climate needs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overcapitalization_threshold_ambiguity,
    'What level of installed renewable capacity constitutes optimization vs. overcapitalization for a given grid?',
    'Grid simulation modeling: compare actual energy production, demand coverage, and reliability outcomes across grids with different capacity-to-demand ratios',
    'If threshold lower than policy assumes: constraint is snare (extraction confirmed). If threshold higher: constraint is rope (apparent overcapitalization is necessary coordination buffer).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(overcapitalization_threshold_ambiguity, empirical, 'Threshold distinguishing necessary grid capacity from overcapitalization').

omega_variable(
    policy_mandate_intent_divergence,
    'Was the original renewable capacity mandate designed to achieve carbon reduction or to establish a growing market for equipment manufacturers?',
    'Historical policy analysis: comparison of stated carbon reduction goals vs actual deployment patterns; examination of whether alternative mechanisms (carbon pricing, demand-reduction) were considered and rejected',
    'If carbon-reduction intent: overcapitalization is unintended rent-seeking failure (tangled rope). If market-creation intent: overcapitalization is coordination mechanism (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_mandate_intent_divergence, conceptual, 'Whether policy mandate aimed at carbon reduction or market creation').

omega_variable(
    storage_technology_timeline,
    'Will grid-scale energy storage (battery, thermal, mechanical) reach cost parity with overcapitalized renewable capacity within the estimated 10-15 year sunset window?',
    'Technology cost curve analysis: battery price trends, competing storage modalities, learning curve slopes; comparison against baseline renewable capacity cost curves',
    'If yes: scaffold sunset is real and overcapitalization will become economically irrational. If no: overcapitalization persists longer, extending extraction period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_technology_timeline, empirical, 'Feasibility of storage-based grid optimization within sunset timeline').

omega_variable(
    financial_intermediary_extraction_vs_coordination,
    'Do financial intermediaries extract economic rents through overcapitalized project financing, or do they coordinate capital mobilization that would not occur without overcapitalization incentives?',
    'Comparative project analysis: returns-on-capital for optimally-sized vs overcapitalized projects; identification of whether financing mechanisms could incentivize optimal deployment at lower capacity levels',
    'If rents: intermediaries are extractive beneficiaries (increases chi). If coordination: intermediaries are necessary coordinating agents (decreases chi, increases rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_intermediary_extraction_vs_coordination, empirical, 'Role of financial intermediaries in overcapitalization mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(renewable_energy_infrastructure_overcapitalization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(renew_overcap_tr_t0, renewable_energy_infrastructure_overcapitalization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(renew_overcap_tr_t5, renewable_energy_infrastructure_overcapitalization, theater_ratio, 5, 0.53).
narrative_ontology:measurement(renew_overcap_tr_t10, renewable_energy_infrastructure_overcapitalization, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(renew_overcap_be_t0, renewable_energy_infrastructure_overcapitalization, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(renew_overcap_be_t5, renewable_energy_infrastructure_overcapitalization, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(renew_overcap_be_t10, renewable_energy_infrastructure_overcapitalization, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(renewable_energy_infrastructure_overcapitalization, resource_allocation).
narrative_ontology:affects_constraint(renewable_energy_infrastructure_overcapitalization, grid_stability_reserve_margin).
narrative_ontology:affects_constraint(renewable_energy_infrastructure_overcapitalization, electricity_rate_structure).
narrative_ontology:affects_constraint(renewable_energy_infrastructure_overcapitalization, renewable_energy_subsidy_capture).

% DUAL FORMULATION NOTE:
% Renewable energy infrastructure overcapitalization is downstream of policy target-setting mechanisms but represents a distinct structural constraint. The upstream constraints involve policy design and subsidy allocation; this constraint addresses the capital deployment mechanism those policies trigger. Decomposed from broader 'renewable energy incentive architecture' constraint because the ε values differ significantly (0.58 here vs. 0.35-0.42 for pure policy design).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(renewable_energy_infrastructure_overcapitalization, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
