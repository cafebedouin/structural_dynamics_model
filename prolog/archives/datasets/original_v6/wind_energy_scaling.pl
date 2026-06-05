% ============================================================================
% CONSTRAINT STORY: wind_energy_scaling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wind_energy_scaling, []).

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
 *   constraint_id: wind_energy_scaling
 *   human_readable: Wind Energy Scaling: Coordination and Extraction in Infrastructure Transition
 *   domain: energy/infrastructure/political_economy
 *
 * SUMMARY:
 *   Wind energy scaling presents a dual structure: genuine coordination
 *   challenge (integrating variable generation into stable grids requires
 *   massive infrastructure investment, demand management, and storage) paired
 *   with asymmetric extraction (costs pass to ratepayers and disadvantaged
 *   regions; benefits concentrate among equipment manufacturers, utilities,
 *   and large developers). Over the past 15 years, extractiveness has risen
 *   as capacity deployment has accelerated without proportional grid
 *   modernization investment, forcing ratepayers to bear integration costs.
 *   Simultaneously, theater has declined as engineering solutions (smart
 *   grids, battery storage, demand management) have matured, reducing the
 *   credibility of incumbent utility claims about 'baseload necessity.' This
 *   constraint exemplifies how a transition mechanism (necessary
 *   infrastructure coordination) can embed extractive power asymmetries.
 *   Rural communities hosting turbines experience snare conditions; utilities
 *   navigate tangled coordination-extraction hybrids; manufacturers benefit
 *   from global arbitrage; and organized actors are building the technical
 *   scaffolding (storage, smart grids) that will sunset the integration
 *   extraction mechanism within 10-20 years.
 *
 * KEY AGENTS:
 *   - Rural Communities: Primary victims (powerless/trapped) — host wind installations, bear externalities, receive minimal financial benefit; trapped by land proximity and regulatory frameworks prioritizing project completion
 *   - Equipment Manufacturers: Primary beneficiaries (institutional/arbitrage) — global supply chains, standardization, expanding markets; genuine coordination participation with arbitrage mobility
 *   - Utility Operators and Grid Operators: Institutional actors (powerful/arbitrage and moderate/constrained) — face genuine coordination challenge (grid integration) while extracting through cost-pass-through and capacity payments; different perspectives depending on regional market structure
 *   - Utility-Scale Developers: Secondary beneficiaries (powerful/arbitrage) — extract through asymmetric land negotiations and favorable regulatory terms; coordinate technical challenges
 *   - Ratepayers: Victims (powerless/trapped to moderate/constrained) — bear integration costs through bill increases; trapped by regulated utility dependency
 *   - Grid Modernization Coalition: Organized actors (organized/constrained) — building storage and smart-grid infrastructure with sunset logic; constrained by current grid architecture but building exit pathway
 *   - Incumbent Coal-Based Utilities: Institutional defenders (institutional/arbitrage) — maintain extraction through regulatory theater (piton perspective); have arbitrage options but prefer incumbent advantage
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes genuine coordination function embedded with asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wind_energy_scaling, 0.52).
domain_priors:suppression_score(wind_energy_scaling, 0.48).
domain_priors:theater_ratio(wind_energy_scaling, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wind_energy_scaling, extractiveness, 0.52).
narrative_ontology:constraint_metric(wind_energy_scaling, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(wind_energy_scaling, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wind_energy_scaling, tangled_rope).
narrative_ontology:human_readable(wind_energy_scaling, "Wind Energy Scaling: Coordination and Extraction in Infrastructure Transition").
narrative_ontology:topic_domain(wind_energy_scaling, "energy/infrastructure/political_economy").

domain_priors:requires_active_enforcement(wind_energy_scaling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wind_energy_scaling, utility_operators).
narrative_ontology:constraint_beneficiary(wind_energy_scaling, equipment_manufacturers).
narrative_ontology:constraint_beneficiary(wind_energy_scaling, grid_operators_in_favorable_regions).
narrative_ontology:constraint_victim(wind_energy_scaling, grid_stability_reliability).
narrative_ontology:constraint_victim(wind_energy_scaling, renewable_transition_lagging_regions).
narrative_ontology:constraint_victim(wind_energy_scaling, ratepayers_bearing_integration_costs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL COMMUNITIES (SNARE) — Trapped by proximity to land resources and resource ownership regulations. Bear externalities (noise, wildlife impact, visual impact) while financial benefits accrue to external operators. Inability to exit or renegotiate terms; minimal local control over installation or operations. High suppression through legal frameworks that prioritize project completion over community consent.
constraint_indexing:constraint_classification(wind_energy_scaling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: GRID OPERATORS AND UTILITIES (TANGLED ROPE) — Face genuine coordination problem: integrating variable renewable generation requires grid modernization, demand management, storage, and transmission infrastructure. Simultaneously extract through cost-pass-through to ratepayers and capacity payments. Constrained by regulatory frameworks and interconnection standards but retain significant agency in implementation choices.
constraint_indexing:constraint_classification(wind_energy_scaling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EQUIPMENT MANUFACTURERS (ROPE) — Primary beneficiary with global arbitrage options. Participate in genuine coordination of supply chains, standardization, and technical innovation. Experience the constraint as beneficial coordination mechanism — expanding market, establishing standards, creating network effects. Net beneficiary with ability to relocate operations or shift markets.
constraint_indexing:constraint_classification(wind_energy_scaling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UTILITY-SCALE DEVELOPERS (TANGLED ROPE) — Benefit from power asymmetry in land negotiations, tax incentives, and power purchase agreements. Simultaneously coordinate genuine technical challenges: site selection, transmission access, environmental permitting, financing. Mobile actors with arbitrage capacity but extract during regional scaling phases through favorable regulatory and land acquisition terms.
constraint_indexing:constraint_classification(wind_energy_scaling, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GRID MODERNIZATION COALITION (SCAFFOLD) — Organized actors (Tesla, Ørsted, NextEra, grid authorities, research institutions) are building storage and smart-grid infrastructure with explicit sunset logic for the scaling constraint. As battery capacity and distributed energy resource management mature, the integration extraction mechanism loses force — variable generation becomes manageable without cost-pass-through. Estimated sunset: 10-20 years as storage economics reach grid parity.
constraint_indexing:constraint_classification(wind_energy_scaling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: COAL-BASED GRID ARCHITECTURE (PITON) — Persists through regulatory capture and incumbent utility advantage rather than functional necessity. Many grid operators maintain high reserve margins and baseload capacity requirements justified by 'grid stability' while wind integration methods now exist that eliminate the necessity for these holdover requirements. Theater ratio high: regulatory testimony about stability risks that engineering solutions have addressed. Maintained through institutional inertia, not structural function.
constraint_indexing:constraint_classification(wind_energy_scaling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, wind energy scaling is a genuine coordination challenge with embedded asymmetric extraction. Grid integration requires massive capital investment in transmission and storage (coordination function). Simultaneously, the financing and cost-allocation mechanisms extract from ratepayers and disadvantaged regions (extraction function). The constraint is neither natural law (mountain) nor pure coordination (rope) — it is a hybrid where institutional power asymmetries shape which costs are socialized and which are privatized.
constraint_indexing:constraint_classification(wind_energy_scaling, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wind_energy_scaling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wind_energy_scaling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wind_energy_scaling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wind_energy_scaling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wind_energy_scaling, TR),
    TR >= 0.70.

:- end_tests(wind_energy_scaling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising over interval. Initial value (0.28) reflects lower penetration and less acute integration challenges. Middle value (0.40) reflects acceleration of deployment without proportional infrastructure investment — extraction rises as ratepayers bear integration costs without corresponding benefit sharing. Final value (0.52) reflects peak extraction as wind penetration approaches integration limits without sufficient storage/smart-grid deployment. Suppression (0.48): Moderate. Significant barriers to alternative energy models and community control mechanisms exist through regulatory frameworks, financing structures, and incumbent utility advantage. But suppression is not total — Denmark, Germany, and cooperative models demonstrate alternative arrangements. Theater ratio (0.68 declining to 0.32): Declining over interval. Initial high theater (0.62) reflects incumbent utility rhetoric about baseload necessity, grid instability risks, and coal plant necessity. As engineering solutions mature (smart grids, battery storage, demand response), the same claims become demonstrably false — theater declines as the underlying technical justification collapses. This declining theater trajectory is diagnostic of scaffold sunset logic: the constraint persists functionally even as its theatrical justification deteriorates.
 *
 * PERSPECTIVAL GAP:
 *   Maximal gap between beneficiaries and victims. Manufacturers experience coordination (low chi, rope); rural communities experience extraction (high chi, snare). Utilities experience mixed dynamics (moderate chi, tangled_rope). Developers experience arbitrage benefits (rope) while imposing snare conditions on communities. This gap is not perspectival disagreement — it is structural asymmetry. Rural communities are not 'seeing' extraction differently; they are structurally positioned to bear costs that manufacturers and developers are structurally positioned to capture. The gap measures inequality, not disagreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Rural communities: trapped + victim status → high d (0.90+) → experienced extractiveness multiplied. Utilities in constrained exit (regulated regional monopolies): mixed beneficiary/victim status, constrained options → moderate d (0.45-0.55) → moderate experienced extraction. Manufacturers with global arbitrage + beneficiary status: low d (0.15-0.25) → low/negative experienced extraction. Developers with mobile options + beneficiary status in regional extraction: moderate-to-high d (0.35-0.65) depending on regional regulatory environment. Grid modernization coalition with organized power + constrained exit + beneficiary status: lower d (0.30-0.40) → institutional scaffolding can proceed with institutional benefits. Incumbent utilities with institutional power + beneficiary status + arbitrage options: very low canonical d (0.00) but omega uncertainty on whether arbitrage is real (coal phase-out forcing exit) or theoretical. Analytical observer at universal scope: d~0.72 (standard analytical).d derivation accounts for beneficiary status (manufacturers, utilities capture benefits) vs victim status (rural communities, ratepayers bear costs). The power asymmetry in land negotiations directly feeds victim status for rural communities → high d → high chi experienced by that agent, even though base extractiveness is moderate.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: Wind energy scaling resolves mandatrophy by showing that the constraint serves a genuine coordination function (integrating variable renewable generation requires infrastructure investment, demand management, storage) while simultaneously permitting asymmetric extraction (costs socialized to ratepayers and rural communities; benefits concentrated among manufacturers and large operators). The constraint is NOT pure coordination (rope) — the cost-allocation mechanism is not inevitable; alternative financing, community ownership models, and risk-sharing structures exist and are used in some jurisdictions. It is NOT pure extraction (snare) — the underlying grid integration coordination problem is real and requires capital investment. The tangled_rope classification captures this hybrid: both functions are structurally present. Theater decline is diagnostic: as engineering solutions mature, the extraction mechanism becomes less defended by technical necessity claims and more visibly extractive institutional choice. The scaffold sunset (storage/smart grids eliminating integration extraction within 10-20 years) is real — not aspirational — because the underlying technical problem is being solved. The piton perspective (coal-based grid architecture) confirms that incumbent utilities maintain extraction mechanisms through regulatory theater after technical solutions exist, demonstrating how tangled ropes can persist as theatrical pitons.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    storage_cost_trajectory,
    'At what battery cost per kWh does the grid integration extraction mechanism become economically unnecessary?',
    'Historical cost tracking; modeling of grid stability requirements as function of storage capacity; empirical correlation between storage availability and capacity payments to fossil baseload',
    'If storage reaches <$50/kWh: scaffold sunset accelerates — integration extraction disappears. If storage stalls at >$150/kWh: extraction persists as structural necessity. Determines whether current constraint is temporary (scaffold) or permanent (tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(storage_cost_trajectory, empirical, 'Storage cost threshold for elimination of grid integration extraction').

omega_variable(
    land_acquisition_power_asymmetry,
    'Is the power asymmetry in land acquisition (developers vs rural communities) inherent to wind resource distribution or contingent on regulatory frameworks and negotiating capacity?',
    'Comparative analysis: Denmark (strong community ownership, low asymmetry) vs US (developer control, high asymmetry) vs Germany (cooperative models); correlation between community participation mechanisms and distributional outcomes',
    'If contingent on frameworks: rural communities are victims of snare through institutional design, not natural constraint. If inherent: resource geography drives unavoidable extraction. Determines whether rural snare perspective is inevitable or engineered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_acquisition_power_asymmetry, empirical, 'Whether land acquisition power asymmetry is structural or institutional').

omega_variable(
    grid_stability_necessity_claim,
    'Do grid stability concerns (reserve margins, baseload requirements) constitute genuine technical necessity or are they regulatory theater maintained by incumbent utilities?',
    'Engineering analysis of actual stability requirements vs regulatory mandates; correlation with grid operating experience in high-renewable-penetration systems (Denmark, South Australia, Ireland); evaluation of reserve margin adequacy studies',
    'If genuine necessity: coal-based architecture (piton) reflects actual technical constraint. If theater: piton classification confirmed — incumbent utilities maintain extraction mechanism through obsolete regulatory requirements. Determines coal architecture''s functional vs theatrical nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grid_stability_necessity_claim, empirical, 'Whether grid stability mandates reflect technical necessity or regulatory theater').

omega_variable(
    transmission_bottleneck_allocation,
    'Are transmission expansion costs and interconnection delays (affecting wind farms'' revenue) genuinely coordination problems or strategic extraction by incumbent grid operators?',
    'Historical analysis of interconnection queue times, cost allocation disputes, and revenue impacts; comparison between regions with open-access transmission vs monopoly control; correlation between incumbent utility capacity and project developer delays',
    'If coordination problems: tangled_rope classification holds. If strategic extraction: snare classification more accurate for wind developers. Determines whether grid operator perspective accurately captures mixed coordination/extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_bottleneck_allocation, empirical, 'Whether transmission bottlenecks reflect coordination challenges or strategic extraction').

omega_variable(
    ratepayer_cost_distribution_mechanisms,
    'Are ratepayer cost burdens for grid integration genuine socialization of public transition benefits or redistributive extraction from low-income households to equipment manufacturers and developers?',
    'Distributional analysis: who bears integration costs vs who captures benefits; correlation between bill impacts and household income; comparison with alternative financing mechanisms (public investment, risk-sharing pools, developer retention of costs)',
    'If genuine socialization: tangled_rope reflects real coordination need. If redistributive extraction: ratepayers are victims of snare, not beneficiaries of transition. Determines social justice dimension of constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratepayer_cost_distribution_mechanisms, preference, 'Whether cost distribution reflects public transition benefits or regressive extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wind_energy_scaling, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wind_tr_t0, wind_energy_scaling, theater_ratio, 0, 0.62).
narrative_ontology:measurement(wind_tr_t5, wind_energy_scaling, theater_ratio, 5, 0.5).
narrative_ontology:measurement(wind_tr_t10, wind_energy_scaling, theater_ratio, 10, 0.38).
narrative_ontology:measurement(wind_tr_t15, wind_energy_scaling, theater_ratio, 15, 0.32).

% Extraction over time
narrative_ontology:measurement(wind_be_t0, wind_energy_scaling, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(wind_be_t5, wind_energy_scaling, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(wind_be_t10, wind_energy_scaling, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(wind_be_t15, wind_energy_scaling, base_extractiveness, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wind_energy_scaling, resource_allocation).
narrative_ontology:boltzmann_floor_override(wind_energy_scaling, 0.18).
narrative_ontology:affects_constraint(wind_energy_scaling, electricity_grid_stability).
narrative_ontology:affects_constraint(wind_energy_scaling, land_use_environmental_externalities).
narrative_ontology:affects_constraint(wind_energy_scaling, battery_storage_supply_chains).
narrative_ontology:affects_constraint(wind_energy_scaling, utility_regulatory_capture).

% DUAL FORMULATION NOTE:
% Wind energy scaling decomposes into three constraint stories: (1) grid_integration_coordination (ε~0.15, rope) — technical problem of managing variable generation; (2) cost_allocation_extraction (ε~0.52, tangled_rope) — institutional choice of how to distribute integration costs; (3) incumbent_utility_defense (ε~0.40, piton) — regulatory theater maintaining coal infrastructure. This story addresses the hybrid (2), with network effects to (1) and (3). The constraint family resolves ε-invariance by clarifying that the 'scaling problem' is not a single constraint but three structurally distinct mechanisms with different measurability and different solutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wind_energy_scaling, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
