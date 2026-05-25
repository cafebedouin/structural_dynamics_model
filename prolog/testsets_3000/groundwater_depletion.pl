% ============================================================================
% CONSTRAINT STORY: groundwater_depletion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_groundwater_depletion, []).

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
 *   constraint_id: groundwater_depletion
 *   human_readable: Groundwater Depletion and Aquifer Extraction
 *   domain: environmental/resource_management/agriculture
 *
 * SUMMARY:
 *   Groundwater depletion represents one of the world's largest and
 *   slowest-moving resource extraction crises, affecting over 2 billion
 *   people across major agricultural regions (Indo-Gangetic Plain, North
 *   China Plain, Ogallala Aquifer, Middle East, North Africa). The constraint
 *   operates across multiple timescales: immediate (daily water access for
 *   farmers), biographical (livelihood viability over a working lifetime),
 *   and civilizational (aquifer recovery timescales of centuries to
 *   millennia). The structural mechanism is asymmetric extraction: industrial
 *   and intensive agricultural users benefit from rapid groundwater pumping
 *   while subsistence farmers, future generations, and ecosystems bearing
 *   base flow dependence bear the costs of depletion. The constraint exhibits
 *   high suppression (72%) through three mechanisms: (1) institutional
 *   (regulatory permitting systems that permit unsustainable extraction), (2)
 *   economic (groundwater is cheaper than alternatives, creating lock-in),
 *   and (3) informational (aquifer depletion is spatially and temporally
 *   diffuse, making costs invisible until collapse is imminent). Theater
 *   ratio (58%) reflects the performative character of many water management
 *   systems — regulations exist but are not enforced, sustainability plans
 *   are published but extraction rates remain constant, and water
 *   conservation is promoted while agricultural subsidies incentivize
 *   intensive irrigation. Extractiveness (68%) reflects that beneficiaries
 *   (large-scale irrigators and industrial users) capture substantial value
 *   from unsustainable extraction while victims (subsistence farmers, future
 *   generations, aquifer-dependent ecosystems) bear costs they cannot
 *   distribute or escape.
 *
 * KEY AGENTS:
 *   - Industrial Agricultural Producers: Primary beneficiaries (institutional/arbitrage) — benefit from productivity gains and can shift to alternative water sources or relocate if aquifers deplete
 *   - Subsistence Farmers: Primary victims (powerless/trapped) — geographically and economically locked into groundwater dependence with no exit options
 *   - Future Generations: Victims (powerless/trapped) — bear costs of irreversible aquifer depletion across civilizational timescales
 *   - Aquifer-Dependent Ecosystems: Victims (powerless/trapped) — lose base flow and riparian water sources with no alternative
 *   - Small-Scale Agricultural Cooperatives: Secondary agents (moderate/constrained) — benefit from water access but also bear costs as depletion deepens and pumping costs rise
 *   - Water Management Authorities: Institutional actors (institutional/constrained) — nominally regulate but lack political will or capacity to enforce sustainable extraction limits
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional/policy choices (subsidies, permitting systems) as physical laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(groundwater_depletion, 0.68).
domain_priors:suppression_score(groundwater_depletion, 0.72).
domain_priors:theater_ratio(groundwater_depletion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(groundwater_depletion, extractiveness, 0.68).
narrative_ontology:constraint_metric(groundwater_depletion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(groundwater_depletion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(groundwater_depletion, snare).
narrative_ontology:human_readable(groundwater_depletion, "Groundwater Depletion and Aquifer Extraction").
narrative_ontology:topic_domain(groundwater_depletion, "environmental/resource_management/agriculture").

domain_priors:requires_active_enforcement(groundwater_depletion).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(groundwater_depletion, agricultural_irrigators).
narrative_ontology:constraint_beneficiary(groundwater_depletion, industrial_water_users).
narrative_ontology:constraint_beneficiary(groundwater_depletion, fossil_fuel_extractors).
narrative_ontology:constraint_victim(groundwater_depletion, future_generations).
narrative_ontology:constraint_victim(groundwater_depletion, subsistence_farmers).
narrative_ontology:constraint_victim(groundwater_depletion, ecosystems_dependent_on_baseflow).
narrative_ontology:constraint_victim(groundwater_depletion, groundwater_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSISTENCE FARMER (SNARE) — Trapped by geography and livelihood dependence on groundwater. As water tables decline, extraction costs rise exponentially; drilling deeper requires capital they cannot access. No alternative water source; no exit option. Bears full cost of depletion with zero agency. Maximum experienced extraction.
constraint_indexing:constraint_classification(groundwater_depletion, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALL-SCALE AGRICULTURAL COOPERATIVE (TANGLED ROPE) — Benefits from groundwater access for irrigation that coordinates food production; also bears costs as depletion deepens. Constrained by capital requirements for deeper wells and energy costs for pumping. Some agency through collective organization, but exit to alternative water sources is expensive. Mixed coordination and extraction.
constraint_indexing:constraint_classification(groundwater_depletion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INDUSTRIAL AGRICULTURAL PRODUCER (ROPE) — Net beneficiary with arbitrage options. Captures productivity gains from intensive irrigation; can shift to alternative water sources (desalination, recycled wastewater) or relocate operations. Experiences the constraint as coordination of water access for efficiency gains. Extraction runs toward this agent.
constraint_indexing:constraint_classification(groundwater_depletion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FUTURE GENERATIONS (SNARE) — Trapped by temporal location and irreversible depletion. Cannot exit or negotiate. Aquifer recovery timescales (centuries to millennia) ensure that exhaustion in the present becomes permanent constraint in the future. No compensation mechanism; no agency.
constraint_indexing:constraint_classification(groundwater_depletion, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: WATER MANAGEMENT AUTHORITY (PITON) — Nominally regulates groundwater extraction but enforcement is theatrical: regulations permit extraction rates that exceed recharge by 5-10x in many regions. The authority's primary function (sustainable aquifer management) has atrophied; it now maintains the ritual of permitting while aquifers deplete uncontested. Persists through institutional inertia — farmers would resist enforcement; politicians prefer permitting to conflict.
constraint_indexing:constraint_classification(groundwater_depletion, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a physical/hydrological perspective, extraction exceeding recharge is a mathematical impossibility that resolves only through exhaustion. The constraint is invariant: aquifer depletion is a function of extraction rate and recharge rate. However, the structural data suggests this is a false summit — the constraint is not a natural law but an institutional arrangement (permitting, subsidies, irrigation incentives) that could be reformed. The mountain classification masks contingent policy choices as physical necessities.
constraint_indexing:constraint_classification(groundwater_depletion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(groundwater_depletion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(groundwater_depletion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(groundwater_depletion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(groundwater_depletion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(groundwater_depletion, TR),
    TR >= 0.70.

:- end_tests(groundwater_depletion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Beneficiaries (large-scale irrigators) capture substantial value through productivity gains during the extraction window; victims (subsistence farmers, future generations) bear costs of depletion with no compensation. The constraint is not extraction by force (coercion is low) but extraction through control of a shared resource combined with regulatory systems that permit unsustainable use. The value is asymmetric: agricultural output is concentrated and countable; depletion costs are diffuse and deferred. Suppression (0.72): High. Multiple mechanisms prevent exit: (1) institutional — regulations permit extraction rates far exceeding recharge, creating a legal/moral barrier to resistance; (2) economic — groundwater is cheap relative to alternatives (desalination, wastewater recycling), creating path dependence on groundwater; (3) informational — depletion is spatially distributed (wells spread across regions) and temporally diffuse (collapse takes decades), making costs invisible until systemic failure; (4) social — livelihood dependence on groundwater creates immobility for subsistence farmers. Theater ratio (0.58): Moderate-high. Water management systems create the appearance of regulation and sustainability without enforcing sustainable extraction. Permitting systems, water conservation campaigns, and sustainability plans persist while extraction rates remain constant or increase. The theater serves both institutional (water authorities maintain legitimacy) and beneficiary (regulators avoid conflict with agricultural constituencies) interests. Extractiveness trend (0.45 → 0.68 over 60 years): Rising. As aquifers deplete and extraction costs increase (deeper wells, longer pumping distances), the asymmetry between beneficiaries (who can shift to alternatives or relocate) and victims (who cannot) deepens. The constraint becomes more purely extractive as coordination functions (allocation of a common resource) atrophy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the divergence between beneficiary and victim perspectives at institutional and individual power levels. Industrial producers see coordination (Rope) — groundwater enables efficient food production. Subsistence farmers see extraction (Snare) — they bear costs with no exit. The water management authority sees its own degradation (Piton) — the regulatory system persists through inertia but lacks enforcement. The small-scale cooperative sees mixed coordination and extraction (Tangled Rope) — water access benefits their livelihood but rising extraction costs constrain their competitiveness. Future generations and ecosystems see pure extraction (Snare) — they inherit a depleted resource with no agency. The analytical observer risks naturalizing the constraint as a physical law (Mountain — extraction inevitably exceeds recharge in high-demand regions) when the structural data reveals it as a policy choice (subsidy systems, permitting frameworks, irrigation incentives) that could be reformed. The perspectival gap widens over time: as aquifers deplete, the beneficiary perspective becomes increasingly unstable (arbitrage options become more expensive), while victim perspectives become increasingly severe (costs rise, escape becomes harder).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position relative to the extraction flow. Industrial producers with arbitrage options (desalination, relocation, alternative water sources) experience low or negative effective extraction (d ≈ 0.15) — the constraint benefits them. Subsistence farmers trapped by geography and livelihood dependence experience high effective extraction (d ≈ 0.90) — they bear the full cost with no exit. Small-scale cooperatives with some collective organization capacity but limited capital experience moderate extraction (d ≈ 0.65) — they benefit from water access but cannot escape rising costs. Future generations, trapped by temporal location, experience maximum extraction (d ≈ 0.98) — they inherit depleted aquifers with no opportunity to negotiate terms. Water management authorities face a constraint on their regulatory capacity: they appear as institutional beneficiaries (legitimacy through permitting) but are actually constrained by political pressure from agricultural constituencies and resource limitations. The piton classification reflects that their primary function (sustainable aquifer management) has degraded while the ritual (permitting, conservation campaigns) persists.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The snare classification is the correct canonical type. The constraint could be framed as rope (coordination of water allocation) or mountain (physical limits to extraction) from false perspectives, but the structural data reveals it as pure extraction. Coordination function: Water allocation does occur (small-scale cooperatives benefit from coordinated pumping), but the primary institutional function (regulatory frameworks, permitting systems) has shifted from coordination to asymmetric extraction. The presence of beneficiaries with arbitrage options (industrial producers) confirms the snare mechanism — they benefit from extraction because others cannot exit. False summits: (1) The mountain perspective (aquifer depletion is a physical law) naturalizes policy choices (subsidies, permitting thresholds, irrigation incentives) as inevitabilities. Removing these policies would shift extraction toward sustainable levels. (2) The rope perspective (groundwater access coordinates food production) is genuine but incomplete — it ignores that beneficiaries could coordinate at lower extraction rates while others could not, making the coordination function serve as a cover story for asymmetric extraction. The snare classification persists because: (1) suppression is high and structural (regulatory, economic, informational mechanisms all prevent alternatives); (2) victims lack exit options at multiple scales (subsistence farmers cannot relocate, future generations cannot negotiate, ecosystems cannot migrate); (3) beneficiaries benefit precisely because others cannot exit (arbitrage value requires trapped populations). The mandatrophy is resolved by recognizing that aquifer depletion is not fundamentally a coordination problem or a physical law but an institutional extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recharge_rate_uncertainty,
    'What is the true recharge rate for contested aquifers, and how much does it vary with climate variability?',
    'Isotopic analysis of groundwater age; paleoclimate reconstruction; long-term precipitation and infiltration modeling',
    'If recharge is higher than current estimates: extraction is less extractive (transitions toward tangled rope). If recharge is lower: extraction is more severe (snare becomes more dominant for all victim perspectives). Climate variability uncertainty: recharge rates may decline 10-30% due to shifting precipitation patterns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recharge_rate_uncertainty, empirical, 'True aquifer recharge rates and climate sensitivity').

omega_variable(
    substitution_cost_feasibility,
    'Can alternative water sources (desalination, wastewater recycling, atmospheric capture) economically replace groundwater extraction at scales needed to maintain current agricultural productivity?',
    'Cost trajectory analysis for alternative technologies; life-cycle assessment of energy inputs and carbon footprint; regional feasibility studies',
    'If alternatives are economically viable: constraint transitions from snare to tangled rope (victims gain exit options). If alternatives remain expensive: snare persists as fundamental structural constraint. Current evidence suggests alternatives cost 2-10x more than groundwater extraction in most regions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_cost_feasibility, empirical, 'Economic viability of water source alternatives').

omega_variable(
    institutional_enforcement_capacity,
    'Do water management authorities have the political and technical capacity to reduce extraction to sustainable levels, or is enforcement capacity fundamentally constrained by agricultural constituency power?',
    'Comparative analysis of regulatory success in jurisdictions with enforcement; cost-benefit analysis of agricultural transition support; political feasibility assessment',
    'If enforcement is feasible: suppression may be reducible (snare may transition to tangled rope). If enforcement is blocked by agricultural constituencies: suppression remains structural and snare persists. Evidence suggests enforcement is politically infeasible in most agricultural regions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_enforcement_capacity, preference, 'Political feasibility of aquifer protection enforcement').

omega_variable(
    collapse_timeline_precision,
    'What is the precise timeline for aquifer exhaustion in major depletion hotspots (Ogallala, Indo-Gangetic, North China Plain), and how does uncertainty affect current policy choices?',
    'High-resolution groundwater modeling; satellite gravity measurement data; well decline rate analysis',
    'If collapse is imminent (< 20 years): current extractors face structural collapse; may trigger rapid policy transition. If collapse is gradual (50+ years): institutional inertia can persist; depletion appears abstract rather than urgent. Current estimates range 20-80 years depending on region and extraction assumptions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapse_timeline_precision, empirical, 'Timeline for major aquifer exhaustion').

omega_variable(
    subsistence_farmer_coalitional_capacity,
    'Can powerless subsistence farmers organize collective action to resist depletion or negotiate alternative arrangements, or are they structurally isolated from coalition formation?',
    'Case studies of farmer organizing in aquifer-stressed regions; analysis of barriers to coordination (geographic dispersion, information asymmetry, resource constraints); comparison with successful water rights movements',
    'If coalition formation is possible: powerless agents may upgrade to organized power level; snare may transition to tangled rope. If isolation persists: coalition analysis cannot apply; snare classification is robust. Evidence suggests farmer organizing is difficult but not impossible — success in some South Asian and North African contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsistence_farmer_coalitional_capacity, empirical, 'Capacity of subsistence farmers to organize coalition resistance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(groundwater_depletion, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gwd_tr_t0, groundwater_depletion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gwd_tr_t20, groundwater_depletion, theater_ratio, 20, 0.5).
narrative_ontology:measurement(gwd_tr_t40, groundwater_depletion, theater_ratio, 40, 0.58).
narrative_ontology:measurement(gwd_tr_t60, groundwater_depletion, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(gwd_be_t0, groundwater_depletion, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gwd_be_t20, groundwater_depletion, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(gwd_be_t40, groundwater_depletion, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(gwd_be_t60, groundwater_depletion, base_extractiveness, 60, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(groundwater_depletion, resource_allocation).
narrative_ontology:boltzmann_floor_override(groundwater_depletion, 0.12).
narrative_ontology:affects_constraint(groundwater_depletion, agricultural_subsidy_lock_in).
narrative_ontology:affects_constraint(groundwater_depletion, food_security_depletion).
narrative_ontology:affects_constraint(groundwater_depletion, water_pricing_asymmetry).
narrative_ontology:affects_constraint(groundwater_depletion, climate_driven_groundwater_stress).

% DUAL FORMULATION NOTE:
% Groundwater depletion is downstream of several policy constraints (agricultural subsidies, irrigation incentives, groundwater pricing regimes) but represents a distinct structural constraint with its own extractiveness and suppression. The upstream policy constraints enable the extraction mechanism; this constraint models the resource depletion directly. Network links identify constraints that amplify or depend on groundwater depletion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(groundwater_depletion, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
