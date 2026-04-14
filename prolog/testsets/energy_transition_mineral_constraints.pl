% ============================================================================
% CONSTRAINT STORY: energy_transition_mineral_constraints
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_energy_transition_mineral_constraints, []).

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
 *   constraint_id: energy_transition_mineral_constraints
 *   human_readable: Energy Transition Mineral Supply Constraints
 *   domain: energy_policy/resource_management/geopolitics
 *
 * SUMMARY:
 *   The energy transition requires deployment of renewable generation,
 *   storage, and electrified transport at scale — all of which depend on
 *   minerals with concentrated geographic supply: lithium (batteries), cobalt
 *   (battery cathodes), rare earths (wind turbines, permanent magnets),
 *   copper (grid infrastructure), and others. This creates a structural
 *   constraint where energy-transition-dependent nations must secure supplies
 *   from geographically concentrated sources, often controlled by state
 *   monopolies or oligopolies. The constraint exhibits a genuine coordination
 *   function (matching scarce mineral supply to global demand for energy
 *   transition) alongside asymmetric extraction (suppliers capture scarcity
 *   rents, lock in price premiums, and leverage geopolitical dependence).
 *   Unlike pure coordination, the constraint cannot be solved by information
 *   sharing or better matching algorithms — it requires actual mineral
 *   expansion, recycling infrastructure, or demand-side substitution. The
 *   theater ratio is moderate and rising: governments increasingly represent
 *   mineral constraints as a temporary logistics problem requiring
 *   coordination mechanisms (strategic stockpiles, procurement standards,
 *   circular economy mandates) while the underlying extraction mechanisms
 *   (supply concentration, long-term contracts, monopoly pricing) remain
 *   structurally intact.
 *
 * KEY AGENTS:
 *   - Energy-Transition-Dependent Importing Nations: Primary victims (powerless/trapped) — locked into mineral dependency by technological path and climate urgency; face price extraction and supply disruption risk
 *   - Mining Companies: Primary beneficiaries (institutional/arbitrage) — capture scarcity premiums and long-term contract negotiation power; coordinate supply matching to demand
 *   - Renewable Energy Developers: Secondary victims/mixed (moderate/constrained) — compete for limited minerals and face price volatility; also benefit from renewable energy ecosystem growth
 *   - Climate and Materials Policy Coalition: Organized actors (organized/constrained) — perceive constraint as temporary problem with sunset via circular economy, alternative materials, and efficiency standards
 *   - Resource-Exporting Nations: Secondary beneficiaries with embedded extraction (institutional/constrained) — coordinate domestic production and global supply; benefit from scarcity premiums; face long-term demand uncertainty
 *   - Extractive Institutional Legacy: Institutional persistence mechanism (institutional/arbitrage) — colonial-era concessions and opaque contracts maintain scarcity-based extraction through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy-contingent scarcity as geological immutability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(energy_transition_mineral_constraints, 0.58).
domain_priors:suppression_score(energy_transition_mineral_constraints, 0.65).
domain_priors:theater_ratio(energy_transition_mineral_constraints, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(energy_transition_mineral_constraints, extractiveness, 0.58).
narrative_ontology:constraint_metric(energy_transition_mineral_constraints, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(energy_transition_mineral_constraints, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(energy_transition_mineral_constraints, tangled_rope).
narrative_ontology:human_readable(energy_transition_mineral_constraints, "Energy Transition Mineral Supply Constraints").
narrative_ontology:topic_domain(energy_transition_mineral_constraints, "energy_policy/resource_management/geopolitics").

domain_priors:requires_active_enforcement(energy_transition_mineral_constraints).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(energy_transition_mineral_constraints, mining_companies).
narrative_ontology:constraint_beneficiary(energy_transition_mineral_constraints, resource_exporting_nations).
narrative_ontology:constraint_victim(energy_transition_mineral_constraints, energy_transition_dependent_economies).
narrative_ontology:constraint_victim(energy_transition_mineral_constraints, renewable_energy_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENERGY-DEPENDENT IMPORTING NATION (SNARE) — Structurally locked into mineral dependency for renewable energy deployment. No alternatives exist at scale; supply chains are geographically concentrated (lithium, cobalt, rare earths). Faces price extraction, supply disruption risk, and long-term contractual lock-in. Cannot negotiate meaningfully given climate urgency and technological path dependence. Maximum experienced extraction.
constraint_indexing:constraint_classification(energy_transition_mineral_constraints, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MINING COMPANY (ROPE) — Experiences the constraint as a coordination mechanism solving a market failure: demand for minerals far exceeds current supply, and production is risky and capital-intensive. The constraint (supply bottleneck) creates value for mining companies through scarcity premiums and long-term contract negotiation power. Genuine coordination function (matching supply to demand) alongside asymmetric benefit extraction. Net beneficiary with arbitrage optionality.
constraint_indexing:constraint_classification(energy_transition_mineral_constraints, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: RENEWABLE ENERGY DEVELOPER (TANGLED ROPE) — Faces mineral supply constraints (must compete for limited lithium, copper, rare earths) but also benefits from the renewable ecosystem being built to address climate urgency. Constrained by mineral availability and price volatility; can substitute between technologies or delay projects but at significant cost. Experiences mixed coordination (supply matching) and extraction (price markup, supply security premium).
constraint_indexing:constraint_classification(energy_transition_mineral_constraints, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CLIMATE AND MATERIALS POLICY COALITION (SCAFFOLD) — Organized actors (governments, environmental NGOs, clean tech associations) perceive mineral constraints as a temporary coordination problem with a sunset: circular economy policies, mining efficiency standards, alternative materials research, and strategic reserves are building exit pathways. The constraint has a genuine coordination function (allocating scarce minerals to highest-value uses) with enforcement (export restrictions, procurement standards) that is explicitly designed to sunset as alternatives mature and circular supply chains mature. Sunset horizon: 15-25 years as recycling infrastructure and alternative battery chemistries mature.
constraint_indexing:constraint_classification(energy_transition_mineral_constraints, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RESOURCE-EXPORTING NATION (TANGLED ROPE) — Coordinates domestic mineral production and global supply (genuine function) while extracting rent through monopoly/cartel power and supply leverage. Benefits from scarcity-driven price premiums but also faces coordination challenges (managing production, environmental costs, revenue stability). Experiences the constraint as both enabling (scarcity = high prices) and binding (climate pressure reduces long-term demand predictability, creates stranded asset risk). Active enforcement required to maintain supply agreements and price controls.
constraint_indexing:constraint_classification(energy_transition_mineral_constraints, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: EXTRACTIVE INSTITUTIONAL LEGACY (PITON) — Colonial-era resource concession agreements, multinational mining contracts, and opaque procurement processes persist despite their degraded function. These institutional arrangements maintain scarcity-based extraction while their original coordination rationale (capital mobilization for mining infrastructure) has largely been superseded by alternative financing. Theater ratio rises as states rhetorically commit to sustainable mining while preserving extraction-enabling contracts. Institutional inertia sustains arrangements that no longer serve stated coordination goals.
constraint_indexing:constraint_classification(energy_transition_mineral_constraints, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL SCARCITY VIEW (MOUNTAIN) — From a geological/physical perspective, certain minerals (lithium, cobalt, rare earths) exist in limited crustal abundance with concentrated geographic distribution as a brute fact of Earth's geochemistry. This perspective sees the constraint as emerging naturally from planetary mineralogy — an immutable resource limit. However, this misses the structurally contingent elements: recycling rates, mining efficiency, alternative chemistries, and demand management are all policy-dependent variables. The mountain classification is a false summit naturalizing what are partly institutional and technological choices.
constraint_indexing:constraint_classification(energy_transition_mineral_constraints, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(energy_transition_mineral_constraints_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(energy_transition_mineral_constraints, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(energy_transition_mineral_constraints, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(energy_transition_mineral_constraints, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(energy_transition_mineral_constraints, TR),
    TR >= 0.70.

:- end_tests(energy_transition_mineral_constraints_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Importing nations face genuine price extraction through monopoly/oligopoly control of supply and long-term contracting that locks in asymmetric terms. The extractiveness is not absolute (alternatives exist but require investment or time) and is partly justified as compensation for mining capital risk. The trajectory shows extractiveness rising from 0.42 to 0.58 over the interval as renewable deployment accelerates and supply expansion lags demand growth. Suppression (0.65): High. Barriers to alternatives are substantial: recycling infrastructure doesn't yet exist at scale; alternative battery chemistries are 5-15 years from commercial viability; demand-side substitution (smaller vehicles, slower transition) conflicts with climate goals; geographic supply concentration creates inelastic demand. Policy options exist (circular economy mandates, alternative chemistry R&D) but implementation is constrained by capital requirements and institutional inertia. Theater ratio (0.48, rising): Moderate. Initial theater is low because the constraint is still genuinely based on supply-demand mismatch (real scarcity). As recycling infrastructure and alternative chemistries mature, theater will rise — policy focus on 'sustainable mining' and 'responsible sourcing' will increase while actual material constraints decline. The rising trajectory reflects increasing performative content as the constraint's functional core weakens.
 *
 * PERSPECTIVAL GAP:
 *   The gap between importing nations (snare) and mining companies (rope) is maximal because they occupy opposite structural positions relative to the same constraint. Importing nation sees: supply lock-in, price extraction, leverage asymmetry (mining company can withhold supply, importing nation cannot withhold demand). Mining company sees: market failure (high-risk capital investment cannot be funded at competitive rates given price volatility), coordination value (long-term contracts reduce both parties' uncertainty), asymmetric benefit (premium for bearing supply risk). Neither perspective is false — they describe real, opposite experiences of the same constraint. The scaffold perspective (policy coalition) projects beyond both: as recycling scales and alternatives mature, the constraint's extraction mechanism weakens and coordination dominates. The piton perspective (institutional legacy) sees the constraint as already degraded — the colonial-era contracts and opaque procurement are performing governance functions they no longer exist to serve, sustained by institutional momentum rather than active necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across agents. Trapped importing nations (powerless/trapped) have maximum d → maximum f(d) → maximum experienced extraction. Mining companies (institutional/arbitrage) have minimum d (beneficiaries with exit) → minimum f(d) → negative experienced extraction (they benefit). Policy coalitions (organized/constrained) have moderate d (they can exit through policy change but at cost) → moderate f(d) → moderate experienced extraction. Resource exporters have mixed directionality: they are beneficiaries (d ≈ 0.20) via pricing power but also structurally locked (trapped exporting nation reclassified as constrained due to long-term demand risk) into mineral-dependent economy (d ≈ 0.65). The average across all positions produces d ≈ 0.48, which feeds χ computation. The sigmoid f(d) scales this to intermediate extracted value — not maximum snare extraction, not pure rope coordination, but mixed tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by clearly separating genuine coordination (supply-demand matching, risk allocation, capital mobilization) from extractive layering (monopoly pricing, supply lock-in, geopolitical leverage). The tangled_rope type correctly identifies that both are present: minerals do need to be produced and distributed (genuine coordination), and producers do extract monopoly rents above competitive return (genuine extraction). The constraint's extractiveness (0.58) is moderately high but not extreme (snare-level 0.66+) because alternatives do exist in principle — recycling can substitute for mining, alternative chemistries can reduce lithium dependency, efficiency can reduce total material requirements. The suppression is high (0.65) because barriers to these alternatives are substantial in the near term. The scaffold perspective's sunset clause is the mandatrophy resolution: the constraint is explicitly designed with an exit path (circular economy + alternative materials + efficiency) that should reduce extraction if realized. If recycling achieves >90% efficiency and alternative chemistries reach cost parity within the 15-year interval, the constraint reclassifies from tangled_rope toward rope; if recycling stalls and alternatives are delayed, it drifts toward snare. The omega variables address the key uncertainties that determine which trajectory occurs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recycling_viability_threshold,
    'At what recycling efficiency threshold do mineral constraints shift from structural scarcity to logistics-coordination problem?',
    'Empirical measurement of battery recycling recovery rates and economics at scale; tracking of actual vs theoretical recycling potential for each critical mineral over 5-year interval',
    'If recycling achieves > 90% recovery: constraint reclassifies from tangled_rope to rope (coordination dominates extraction). If recycling stalls below 60%: constraint becomes snare for importers (extraction dominates). Currently estimated 40-60% for lithium, 80-90% for cobalt — mixed signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recycling_viability_threshold, empirical, 'Recycling efficiency threshold for constraint reclassification').

omega_variable(
    alternative_chemistry_breakthrough_probability,
    'What is the realistic probability and timeline for sodium-ion, solid-state, or other alternative battery chemistries to achieve cost and performance parity with lithium-ion?',
    'Patent analysis, lab-to-market timelines, venture capital deployment, manufacturing pilot projects; tracking of cost curves and energy density improvements for leading alternative candidates',
    'If breakthrough occurs within 5 years: scaffold sunset is real and imminent, constraint reclassifies to temporary coordination. If timeline extends beyond 15 years: scaffold classification becomes aspirational rather than structural — constraint remains extractive for longer horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_chemistry_breakthrough_probability, empirical, 'Timeline for alternative battery chemistry viability').

omega_variable(
    supply_cartel_durability,
    'Can resource-exporting nations maintain production coordination and price leverage as renewable energy deployment accelerates and recycling increases, or does growing supply create competitive pressure that breaks coordination?',
    'Analysis of cartel behavior under demand growth vs saturation; comparison to historical commodity cartel collapse patterns; tracking of mining production levels, price volatility, and export agreement stability',
    'If cartel sustains: constraint remains tangled_rope with high extraction for 15+ years. If competition emerges: constraint weakens, reclassifies toward rope, and scaffold sunset accelerates as supply abundance reduces scarcity premium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_cartel_durability, empirical, 'Durability of resource-exporting nation supply coordination').

omega_variable(
    environmental_extraction_cost_internalization,
    'Do environmental and social costs of mining (water depletion, habitat destruction, labor exploitation, geopolitical leverage from resource dependence) get structurally internalized into mineral pricing, or do they remain externalized across borders and time horizons?',
    'Tracking of environmental remediation costs, water scarcity premiums, labor standards compliance, and true social cost accounting; comparison of mineral prices under externality scenarios',
    'If externalities internalized: effective cost of minerals increases substantially, reducing snare extraction severity for importing nations through economic mechanisms. If externalities remain externalized: extraction perpetuates across supply chain, constraint remains snare despite official ''fair trade'' narratives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_extraction_cost_internalization, conceptual, 'Whether environmental and social costs of mining are internalized in pricing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(energy_transition_mineral_constraints, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(etmc_tr_t0, energy_transition_mineral_constraints, theater_ratio, 0, 0.32).
narrative_ontology:measurement(etmc_tr_t5, energy_transition_mineral_constraints, theater_ratio, 5, 0.38).
narrative_ontology:measurement(etmc_tr_t10, energy_transition_mineral_constraints, theater_ratio, 10, 0.48).
narrative_ontology:measurement(etmc_tr_t15, energy_transition_mineral_constraints, theater_ratio, 15, 0.52).

% Extraction over time
narrative_ontology:measurement(etmc_be_t0, energy_transition_mineral_constraints, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(etmc_be_t5, energy_transition_mineral_constraints, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(etmc_be_t10, energy_transition_mineral_constraints, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(etmc_be_t15, energy_transition_mineral_constraints, base_extractiveness, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(energy_transition_mineral_constraints, resource_allocation).
narrative_ontology:boltzmann_floor_override(energy_transition_mineral_constraints, 0.18).
narrative_ontology:affects_constraint(energy_transition_mineral_constraints, grid_scale_energy_storage_bottleneck).
narrative_ontology:affects_constraint(energy_transition_mineral_constraints, renewable_geopolitical_supply_chain).
narrative_ontology:affects_constraint(energy_transition_mineral_constraints, circular_economy_maturity_lag).

% DUAL FORMULATION NOTE:
% Energy transition mineral constraints decompose into three structurally distinct constraints: (1) supply-demand coordination in mining (resource_allocation, ε≈0.30, rope), (2) geopolitical leverage in supply concentration (ε≈0.65, snare for importers), (3) circular economy infrastructure maturity (ε≈0.40, scaffold with sunset). This story integrates all three; the decomposition enables per-constraint policy analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(energy_transition_mineral_constraints, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
