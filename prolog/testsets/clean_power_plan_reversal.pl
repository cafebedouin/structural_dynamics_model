% ============================================================================
% CONSTRAINT STORY: clean_power_plan_reversal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_clean_power_plan_reversal, []).

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
 *   constraint_id: clean_power_plan_reversal
 *   human_readable: Clean Power Plan Reversal: Extraction via Regulatory Arbitrage
 *   domain: environmental_policy/energy_regulation
 *
 * SUMMARY:
 *   The Clean Power Plan reversal represents a constraint where regulatory
 *   deregulation becomes an extraction mechanism. The original Clean Power
 *   Plan (2015) imposed federal carbon emission standards on power plants,
 *   creating a coordination floor across states. Its reversal (2019-2020)
 *   removed this floor, enabling fossil fuel incumbents to extract economic
 *   rent from renewable energy developers, coal-dependent communities, and
 *   climate stabilization capacity through regulatory arbitrage. The
 *   constraint exhibits mixed coordination and extraction: power grids
 *   genuinely require interstate coordination, but the reversal weaponizes
 *   this coordination function to protect incumbent fossil capacity and delay
 *   renewable transition. The constraint's extractiveness increased over the
 *   measurement interval (0.35 → 0.58) as the reversal matured from policy
 *   reversal to implementation in interconnection delays, state regulatory
 *   capture, and renewable project deferrals. Theater ratio (0.65) reflects
 *   the performative regulatory process: EPA maintains climate authority
 *   rhetoric while enforcement mechanisms atrophy; states conduct lengthy
 *   grid studies and interconnection reviews as delay tactics rather than
 *   technical necessity; utilities maintain rate-base recovery arguments
 *   while blocking distributed renewable competition. Different agents
 *   perceive fundamentally different constraint types depending on their
 *   structural position and exit options.
 *
 * KEY AGENTS:
 *   - Fossil Fuel Incumbents: Primary beneficiary (institutional/arbitrage) — capture regulatory protection and extended profitable coal/gas generation window
 *   - Coal Corridor Communities: Primary victim (powerless/trapped) — economically dependent on coal with no alternative development pathways; bear health costs from extended coal operation
 *   - Renewable Energy Developers: Secondary victim (moderate/constrained) — face interconnection delays, regulatory uncertainty, and extended compliance costs; can exit via relocation but at capital cost
 *   - State Utilities and Grid Operators: Institutional actor (institutional/constrained) — maintain incumbent generation assets; constrained by federal deregulation but benefit from rate-base recovery of fossil infrastructure
 *   - Environmental Coalitions: Organized agent (organized/mobile) — have legal, political, and investor tools for opposition but face funding suppression and state-level regulatory capture
 *   - State Environmental Agencies: Institutional actor (institutional/constrained) — lose federal coordination floor; constrained by state-level capture and federal deregulation
 *   - EPA: Institutional actor (institutional/arbitrage) — maintains legal authority but experiences extraction of enforcement capacity through litigation delays and state regulatory arbitrage
 *   - Analytical Observer: Observes thermodynamic limits and institutional arrangements (analytical/analytical) — risks naturalizing regulatory choices as inevitable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(clean_power_plan_reversal, 0.58).
domain_priors:suppression_score(clean_power_plan_reversal, 0.62).
domain_priors:theater_ratio(clean_power_plan_reversal, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(clean_power_plan_reversal, extractiveness, 0.58).
narrative_ontology:constraint_metric(clean_power_plan_reversal, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(clean_power_plan_reversal, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(clean_power_plan_reversal, tangled_rope).
narrative_ontology:human_readable(clean_power_plan_reversal, "Clean Power Plan Reversal: Extraction via Regulatory Arbitrage").
narrative_ontology:topic_domain(clean_power_plan_reversal, "environmental_policy/energy_regulation").

domain_priors:requires_active_enforcement(clean_power_plan_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(clean_power_plan_reversal, fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(clean_power_plan_reversal, coal_sector_incumbents).
narrative_ontology:constraint_beneficiary(clean_power_plan_reversal, state_regulatory_capture).
narrative_ontology:constraint_victim(clean_power_plan_reversal, renewable_energy_developers).
narrative_ontology:constraint_victim(clean_power_plan_reversal, public_health_communities).
narrative_ontology:constraint_victim(clean_power_plan_reversal, climate_stabilization_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COAL CORRIDOR COMMUNITIES (SNARE) — Trapped by economic dependency on coal infrastructure with minimal exit options. Regulatory reversal locks in extraction: dependent on coal employment while bearing health costs (particulate matter, mercury, climate impacts). No alternative economic development pathways available at local scale. Suppression is structural: geographic isolation, educational capture, and institutional dependency create immobility.
constraint_indexing:constraint_classification(clean_power_plan_reversal, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE GRID OPERATORS (TANGLED ROPE) — Constrained by regulation but also benefit from incumbent generation assets and rate-base recovery mechanisms. The reversal enables coordination of existing coal/gas fleets while extracting from renewable developers through prolonged interconnection delays. Mixed coordination function (grid stability) with asymmetric extraction (incumbent protection).
constraint_indexing:constraint_classification(clean_power_plan_reversal, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RENEWABLE ENERGY DEVELOPERS (SNARE) — Constrained by regulatory uncertainty and interconnection barriers. Reversal extends grid study timelines, increases compliance costs, and delays revenue generation. Exit requires relocating to states with continued clean energy standards, but this carries reputational and capital costs. High extraction: costs accumulate through regulatory time-stalling.
constraint_indexing:constraint_classification(clean_power_plan_reversal, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FOSSIL FUEL INCUMBENTS (ROPE) — Primary beneficiary (institutional/arbitrage). Experiences reversal as coordination of existing assets: maintains market share and extends profitable generation window through regulatory protection. Can arbitrage across state jurisdictions—those with continued clean standards, and those with reversal. Net low extraction from their perspective; gains from coordination.
constraint_indexing:constraint_classification(clean_power_plan_reversal, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CLIMATE COALITIONS (TANGLED ROPE) — Organized agents (environmental NGOs, climate advocates) see reversal as coordination failure with extraction function. Genuine coordination problem exists: interstate power markets need regional standards. But reversal extracts by locking in fossil generation pathways while environmental costs are externalized. Coalitions have some mobility (litigation, subnational action, international advocacy) but face suppression through funding capture and political pressure.
constraint_indexing:constraint_classification(clean_power_plan_reversal, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: EPA REGULATORY FRAMEWORK (PITON) — The reversal reveals the degradation of EPA's Clean Air Act enforcement capacity. The Clean Power Plan was designed to enforce existing statutory authority; reversal shows the framework persists through institutional inertia despite lost practical function. Theater ratio high (0.65): agency maintains authority rhetoric while enforcement capacity declines through regulatory arbitrage—states can challenge rules through litigation delays, creating theater of process without substantive environmental protection.
constraint_indexing:constraint_classification(clean_power_plan_reversal, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: STATE ENVIRONMENTAL AGENCIES (TANGLED ROPE) — Constrained by federal deregulation but maintain some coordination function through state-level standards. Reversal simultaneously removes coordination floor (federal baseline) and creates extraction opportunity for states to capture local regulatory authority. Constrained exit: Federal regulation provides defense against state-level capture; reversal removes that defense.
constraint_indexing:constraint_classification(clean_power_plan_reversal, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER—PHYSICS VIEW (MOUNTAIN) — From civilizational scale, carbon accumulation in atmosphere creates thermodynamic constraint: CO2 concentration determines radiative forcing, which determines climate response. Reversal does not change this physics. However, this perspective risks false summit: naturalizing the regulatory reversal as inevitable given energy demand. The analytical view must distinguish between thermodynamic limits and institutional arrangements.
constraint_indexing:constraint_classification(clean_power_plan_reversal, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(clean_power_plan_reversal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(clean_power_plan_reversal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(clean_power_plan_reversal, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(clean_power_plan_reversal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(clean_power_plan_reversal, TR),
    TR >= 0.70.

:- end_tests(clean_power_plan_reversal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The reversal creates asymmetric benefits for fossil incumbents and costs for renewable developers and health-exposed communities. However, extraction is not total (chi ≤ 1.0) because: (1) some coordination function is genuine (grid stability requires interstate coordination); (2) renewable developers retain some exit options (relocation, subnational standards); (3) environmental coalitions retain organizing capacity; (4) health costs will eventually drive litigation. Extractiveness increased over interval as reversal moved from policy to implementation. Suppression (0.62): Moderate-high. Significant barriers include: regulatory capture of state agencies, litigation delays (EPA challenges take 3-5 years), community economic dependency on coal (geographic lock-in), funding suppression of environmental organizations. But suppression is not total: renewable developers can relocate, coalitions can mobilize subnational action and investor pressure, health litigation eventually forces reckoning. Theater ratio (0.65): High-moderate. Regulatory processes (grid studies, interconnection reviews, EPA legal challenges) appear technical but function as delay tactics. State utility commissions conduct extended analyses that inflate perceived complexity. EPA maintains climate authority rhetoric while enforcement capacity declines. This theater serves the extraction mechanism: creating appearance of rational deliberation while blocking renewable deployment.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap opens between beneficiaries and victims. Fossil incumbents perceive coordination (Rope): 'We are solving the legitimate problem of managing energy transitions.' Coal communities perceive extraction (Snare): 'Our livelihoods are locked in coal while we bear health costs.' The constraint's theater (performative regulation) allows these perspectives to coexist without friction from the beneficiary view—the regulatory apparatus appears to be neutral, technical, deliberative. The analytics view that naturalizes the reversal as inevitable (Mountain: 'physics requires energy coordination') provides post-hoc rationalization for the extraction. Environmental coalitions see through this theater (Tangled Rope: 'Genuine grid coordination exists but the reversal misuses it for incumbent protection'), but coalitions face suppression and funding capture that limit their perspective's influence on policy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's relationship to the extraction flow. Fossil incumbents: d ≈ 0.05 (full beneficiary + arbitrage exit → negative effective extraction f(d) ≈ -0.12). They experience the constraint as enabling rather than constraining. Coal communities: d ≈ 0.98 (trapped exit with victim status → f(d) ≈ 1.41). They bear maximum extraction; their trapped status means no escape path through arbitrage or relocation. Renewable developers: d ≈ 0.75 (constrained exit + victim status → f(d) ≈ 1.15). They experience extraction but retain some exit capacity (relocation, subnational markets). Environmental coalitions: d ≈ 0.65 (mobile exit + victim status → f(d) ≈ 1.00). Their organizing capacity moderates experienced extraction. The Institutional observer with arbitrage exit (EPA, utilities): d ≈ 0.35 (partly beneficiary, partly constrained → f(d) ≈ 0.42). They experience mixed extraction/protection depending on their position (captured vs not captured). These d values map to experienced extractiveness (chi) through the formula χ = ε × f(d) × σ(S), with scope modifier σ(national) = 1.0.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The reversal resolves mandatrophy by showing that a constraint can genuinely coordinate (interstate power markets) while simultaneously extracting (protecting incumbents). The Tangled Rope classification is not a hedge between coordination and extraction—it is the accurate structural description. The mandatrophy was: 'Is this coordination or extraction?' The answer is: it coordinates for beneficiaries (incumbents gain functioning markets) while extracting from victims (renewables are delayed, communities are locked in). The tension is not in measurement but in structure. The fossil incumbents' Rope perspective and the renewable developers' Snare perspective are both correct descriptions of their structural positions. The analytical observer's mountain perspective is a false summit—it naturalizes an institutional choice as inevitable. The reversal cannot be accurately described as pure coordination or pure extraction; it is an institutional arrangement that provides coordination services to incumbent beneficiaries while extracting costs from renewable developers and health-exposed communities. This is the definition of Tangled Rope, and its presence confirms that the constraint structure is correctly identified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grid_stability_genuine_coordination,
    'Does the reversal''s stated grid stability justification represent genuine coordination need or post-hoc rationalization for extraction protection?',
    'Comparative grid reliability analysis: regions with high renewable penetration (California, Denmark) vs reversal-protected regions (coal-heavy grids). If renewables show comparable or better reliability metrics, coordination argument is weakened.',
    'If genuine coordination: constraint reclassifies from Snare toward Tangled Rope for more perspectives. If rationalization: confirms Snare classification and extraction character of reversal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grid_stability_genuine_coordination, empirical, 'Whether grid stability justification is genuine coordination need or extraction rationalization').

omega_variable(
    renewable_cost_trajectory_suppression,
    'What portion of renewable energy delay is due to technical grid integration constraints vs regulatory delay tactics?',
    'Comparison of interconnection timeline data across jurisdictions; multivariate regression on approval duration controlling for grid complexity, project scale, and regulatory regime.',
    'If >60% regulatory: suppression is primarily institutional (Snare confirmed). If <40% regulatory: grid constraints are genuine (Tangled Rope gains credibility).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_cost_trajectory_suppression, empirical, 'Proportion of renewable delay attributable to regulatory vs technical constraints').

omega_variable(
    state_capture_mechanism_reversibility,
    'Is regulatory reversal primarily federal deregulation or active state-level regulatory capture by fossil incumbents?',
    'Analysis of state regulatory commission composition, campaign finance to state officials, and post-reversal regulatory decisions. If states independently reverse (low incumbent influence) vs states captured (high incumbent involvement), mechanism differs.',
    'If independent: constraint is hierarchical federal-state extraction (institutional power imbalance). If captured: constraint is peer-level asymmetric extraction (lateral extraction amplified by deregulation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capture_mechanism_reversibility, empirical, 'Whether reversal is federal deregulation or state-level regulatory capture').

omega_variable(
    renewable_relocation_viability,
    'Do renewable developers have genuine exit options (relocate to clean-standard states) or is exit suppressed by capital illiquidity and supply chain lock-in?',
    'Tracking of renewable project cancellations vs relocations; cost analysis of project pivot/relocation vs compliance with reversal.',
    'If exit viable: exit_options upgrade from ''constrained'' toward ''mobile,'' reducing experienced extraction. If exit suppressed: confirms trap classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_relocation_viability, empirical, 'Viability of renewable developer exit via relocation').

omega_variable(
    climate_coalition_organizing_capacity,
    'Can environmental coalitions overcome funding suppression and political pressure to mount sustained opposition (interstate litigation, subnational standards, investor divestment)?',
    'Longitudinal tracking of coalition funding, legal victories, state-level adoption of clean standards, institutional investor divestment commitments.',
    'If coalition capacity grows: Tangled Rope classification confirmed (organized agents with real exit). If capacity declines: reclassify toward Snare (suppression overcomes organizing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_coalition_organizing_capacity, empirical, 'Environmental coalition capacity to overcome suppression').

omega_variable(
    health_cost_externalization_duration,
    'What is the time horizon over which health costs (respiratory disease, excess mortality) from extended coal operation accumulate before health-cost-driven litigation overcomes suppression?',
    'Health impact accounting: attributable mortality and morbidity from reversal-extended coal operation; timing of health litigation bringing suppressed agents (communities) into economic recovery proceedings.',
    'If < 5 years: victims gain material leverage through health damages. If > 10 years: extraction persists through suppression of victim bargaining power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(health_cost_externalization_duration, empirical, 'Timeline for health costs to overcome suppression via litigation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(clean_power_plan_reversal, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cpp_tr_t0, clean_power_plan_reversal, theater_ratio, 0, 0.5).
narrative_ontology:measurement(cpp_tr_t3, clean_power_plan_reversal, theater_ratio, 3, 0.58).
narrative_ontology:measurement(cpp_tr_t6, clean_power_plan_reversal, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(cpp_be_t0, clean_power_plan_reversal, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cpp_be_t3, clean_power_plan_reversal, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(cpp_be_t6, clean_power_plan_reversal, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(clean_power_plan_reversal, enforcement_mechanism).
narrative_ontology:affects_constraint(clean_power_plan_reversal, renewable_energy_interconnection_delay).
narrative_ontology:affects_constraint(clean_power_plan_reversal, state_regulatory_capture_utility_nexus).
narrative_ontology:affects_constraint(clean_power_plan_reversal, coal_community_economic_lock_in).

% DUAL FORMULATION NOTE:
% The Clean Power Plan reversal is a family of related constraints: federal regulatory reversal (this story, ε=0.58) affects state-level enforcement capacity and renewable interconnection rules (separate stories with lower ε). The federal reversal upstream enables downstream state-level regulatory capture and renewable project delays. Each story in the family has its own extractiveness value reflecting the granularity of observation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(clean_power_plan_reversal, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
