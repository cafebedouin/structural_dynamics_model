% ============================================================================
% CONSTRAINT STORY: freshwater_depletion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_freshwater_depletion, []).

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
 *   constraint_id: freshwater_depletion
 *   human_readable: Freshwater Depletion as Structural Extraction
 *   domain: environmental/hydrological/economic
 *
 * SUMMARY:
 *   Freshwater depletion creates a structural constraint where users with
 *   high water consumption and low sensitivity to depletion costs extract
 *   from common aquifer resources, transferring costs to populations without
 *   exit options and to ecosystems. The constraint exhibits properties of a
 *   pure snare for powerless trapped agents and a tangled rope for organized
 *   beneficiaries. Extractiveness has risen from 0.35 to 0.68 over the
 *   20-year interval as depletion accelerates and policy responses remain
 *   performative. The theater ratio (0.55) reflects that water sustainability
 *   discourse has grown substantially while actual extraction restrictions
 *   remain weak — conservation rhetoric performs commitment to sustainability
 *   without requiring sacrifice from major beneficiaries. The constraint is
 *   maintained through legal/institutional structures (prior appropriation
 *   doctrine, extraction rights allocation, subsidy architecture) that are
 *   enforcement mechanisms disguised as natural resource management.
 *
 * KEY AGENTS:
 *   - Rural subsistence farmers: Primary victims (powerless/trapped) — depend on groundwater for survival, cannot relocate, bear extraction through livelihood collapse
 *   - Indigenous communities: Primary victims (powerless/trapped) — identity-locked to watersheds, suffer cultural reproduction failure, possess legal sovereignty that is systematically violated
 *   - Agricultural producers: Secondary beneficiaries (moderate/constrained) — benefit from access to cheap water through infrastructure and extraction rights, face exit costs from infrastructure switching
 *   - Commodity traders: Primary beneficiaries (institutional/arbitrage) — profit from low-cost water-intensive commodities without bearing depletion costs, can arbitrage to alternative sourcing
 *   - Municipal water authorities: Secondary beneficiaries (organized/mobile) — coordinate urban supply but maintain extraction model; possess exit pathways (recycling, desalination) that are suppressed by cost and political resistance
 *   - Groundwater-dependent ecosystems: Structural victims (powerless/trapped) — no agency, zero exit options, systematic extraction triggers cascade failures
 *   - Water policy reform coalition: Organized challengers (organized/mobile) — perceive policy sunset clause and exit pathway; lack enforcement power but represent institutional pressure point
 *   - Agricultural extension: Institutional supporter (institutional/constrained) — performs compatibility maintenance between extraction and sustainability rhetoric; enforces through knowledge transfer that legitimates water-intensive production
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(freshwater_depletion, 0.68).
domain_priors:suppression_score(freshwater_depletion, 0.72).
domain_priors:theater_ratio(freshwater_depletion, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(freshwater_depletion, extractiveness, 0.68).
narrative_ontology:constraint_metric(freshwater_depletion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(freshwater_depletion, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(freshwater_depletion, snare).
narrative_ontology:human_readable(freshwater_depletion, "Freshwater Depletion as Structural Extraction").
narrative_ontology:topic_domain(freshwater_depletion, "environmental/hydrological/economic").

domain_priors:requires_active_enforcement(freshwater_depletion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(freshwater_depletion, industrial_agriculture).
narrative_ontology:constraint_beneficiary(freshwater_depletion, fossil_fuel_extraction).
narrative_ontology:constraint_beneficiary(freshwater_depletion, urban_development_interests).
narrative_ontology:constraint_beneficiary(freshwater_depletion, irrigation_intensive_industries).
narrative_ontology:constraint_victim(freshwater_depletion, rural_subsistence_farmers).
narrative_ontology:constraint_victim(freshwater_depletion, indigenous_communities).
narrative_ontology:constraint_victim(freshwater_depletion, groundwater_dependent_ecosystems).
narrative_ontology:constraint_victim(freshwater_depletion, future_generations).
narrative_ontology:constraint_victim(freshwater_depletion, downstream_riparian_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL SUBSISTENCE FARMER (SNARE) — Trapped by geographic location and livelihood dependence on groundwater. Aquifer depletion removes the material basis of survival with no exit option. Zero degrees of freedom: relocation costs exceed income capacity, alternative livelihoods unavailable, legal protections absent. Bears extraction of water rights and land value with no compensation mechanism. Maximum perceived extraction.
constraint_indexing:constraint_classification(freshwater_depletion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDIGENOUS COMMUNITY — GENERATIONAL (SNARE) — Structurally trapped by territorial rights that are systematically violated through upstream water extraction. Identity locked into place-based lifeways dependent on watershed integrity. Suppression operates through legal frameworks that privilege prior appropriation rights over indigenous sovereignty. The constraint destroys not just water availability but cultural reproduction across generations.
constraint_indexing:constraint_classification(freshwater_depletion, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 3: IRRIGATION-DEPENDENT PRODUCER (TANGLED ROPE) — Faces significant exit costs (capital invested in irrigation infrastructure, market lock-in, production contracts) but benefits from access to subsidized water through extraction of common resource. Genuine coordination function exists (water distribution networks enable aggregate production) but is asymmetrically coupled with extraction from common pool. Can exit by shifting to rainfed crops, but cost is substantial relative to income.
constraint_indexing:constraint_classification(freshwater_depletion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: AGRICULTURAL COMMODITY TRADER (ROPE) — Benefits from low-cost water-intensive crop production without bearing depletion costs (externalizes to source regions). Experiences constraint as pure coordination mechanism: trade networks require stable commodity flows. Can easily arbitrage to alternative sourcing regions. Extraction flows away from this actor toward water-stressed source communities.
constraint_indexing:constraint_classification(freshwater_depletion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: MUNICIPAL WATER AUTHORITY (TANGLED ROPE) — Organized actor with genuine coordination function (urban water supply distribution) but operates through extraction from rural/upstream communities. Exit option exists (water recycling, desalination, demand reduction) but capital transition is expensive and politically resisted. Demonstrates inter-institutional dynamic: municipal authority benefits while being organized, but has exit pathway that is currently suppressed by cost and political inertia.
constraint_indexing:constraint_classification(freshwater_depletion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: AQUIFER ECOSYSTEMS (SNARE) — Cannot perceive or exit constraints. Systematic extraction of groundwater triggers cascade failures: springs dry, riparian zones collapse, biodiversity crashes. Suppression is absolute — no mechanism for ecosystem adaptation or negotiation. Abstract collective victim with zero agency.
constraint_indexing:constraint_classification(freshwater_depletion, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: WATER POLICY REFORM COALITION (SCAFFOLD) — Organized agents (conservation NGOs, regional governance bodies, indigenous sovereignty movements) see depletion as a temporary institutional failure with exit pathway: water pricing reform, extraction limits, alternative crop selection, infrastructure investment. Coalition has agency and perceives sunset clause: adaptive governance frameworks with hard caps on extraction are structurally feasible. Effective extraction is low because coalition can exit via policy reform. Theater ratio is moderate — some enforcement is real, some is performative.
constraint_indexing:constraint_classification(freshwater_depletion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: AGRICULTURAL EXTENSION (PITON) — Institutional actor tasked with promoting productivity, now performing compatibility maintenance between increasing extraction and sustainability rhetoric. Extension agencies continue recommending water-intensive crops while simultaneously promoting 'water-smart' practices — performative greenwashing. Theater ratio is high; original coordination function (knowledge transfer for yield improvement) has been subordinated to extraction maintenance.
constraint_indexing:constraint_classification(freshwater_depletion, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilizational-scale analysis reveals freshwater depletion as a hybrid constraint: genuine coordination function (water distribution networks, agricultural intensification solving nutrition security) exists alongside systematic extraction from powerless agents and ecosystems. Exit pathways are technically feasible (renewable water management, crop diversification, alternative livelihoods) but require redistributing costs currently externalized to trapped populations. High suppression reflects institutional lock-in and cost avoidance by beneficiaries.
constraint_indexing:constraint_classification(freshwater_depletion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(freshwater_depletion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(freshwater_depletion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(freshwater_depletion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(freshwater_depletion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(freshwater_depletion, TR),
    TR >= 0.70.

:- end_tests(freshwater_depletion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint transfers water wealth from trapped populations and ecosystems to organized beneficiaries through institutional mechanisms (prior appropriation law, infrastructure monopoly, subsidy structure). The 20-year trajectory shows accelerating extraction as depletion deepens and consequences intensify. Suppression (0.72): High. Material barriers include capital costs of alternative irrigation systems, market lock-in to water-intensive crop contracts, and geographic immobility of affected populations. Institutional barriers include legal structures that subordinate riparian rights to prior appropriation, exclusion of indigenous sovereignty from water governance, and difficulty organizing powerless scattered farmers. Barriers to policy reform include concentrated benefits among organized agriculture and diffuse costs to powerless victims. Theater ratio (0.55): Moderate-high. Water conservation discourse (Smart Agriculture, Efficient Irrigation, Blue Water Footprinting) performs commitment to sustainability while actual extraction constraints remain minimal for major users. Government reporting emphasizes efficiency gains rather than absolute reduction. Internationally-brokered water-sharing agreements (Jordan River, Indus Waters Treaty) have maintained formal coordination while extraction has continued rising.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The rural farmer perceives snare: no exit, extraction is total, suppression is absolute. The commodity trader perceives rope: coordination mechanism enables their profitability. The municipal authority perceives tangled rope: genuine coordination function (urban supply) alongside extraction (from upstream communities). The organized water coalition perceives scaffold: policy sunset is technically feasible via extraction limits and crop diversification. The agricultural extension perceives routine coordination: knowledge transfer for productivity improvement. The analytical observer perceives snare underlying the facade: the coordination functions are real but subordinated to extraction logic; exit pathways are theoretically available but suppressed by cost and institutional lock-in. The piton perspective observes that the water system has become increasingly performative as genuine coordination (connecting supply to demand) is replaced by extraction maintenance (maximizing water removal regardless of consequence). The ecosystem perspective is unmeasurable but structurally decisive: cascade failure at depletion thresholds eliminates all other perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position relative to the extraction flow. Rural subsistence farmers are structurally victimized (d ≈ 0.98: full targets) with zero exit options; the sigmoid f(d) produces maximum experienced extraction. Commodity traders are beneficiaries with arbitrage mobility (d ≈ 0.10); they experience negative effective extraction (the constraint subsidizes them). Municipal authorities occupy a middle position: they coordinate urban supply (beneficiary function) but extract from rural groundwater (victim role), giving a split directionality reflected in the tangled rope classification. Indigenous communities have d ≈ 0.95 (full targets) despite potential governance authority because their sovereignty is systematically violated by upstream extraction — structural position dominates nominal legal status. Agricultural producers have constrained exit (d ≈ 0.65), reflecting that they face real switching costs but can exit (unlike subsistence farmers) if extraction becomes unprofitable. The water reform coalition has mobile exit (d ≈ 0.55) because they can influence policy architecture but cannot unilaterally end extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Freshwater depletion resolves mandatrophy through the snare/tangled rope distinction. The constraint is NOT pure coordination (rope) because withdrawal does not benefit powerless victims — it transfers their water wealth to organized beneficiaries. The constraint is NOT a transient policy problem (scaffold) because aquifer depletion is thermodynamically irreversible at timescales relevant to trapped populations — policy reform would need to be immediate and severe, which suppression mechanisms prevent. The constraint IS a snare for powerless agents: extraction is coercive (you cannot refuse to lose access to groundwater when upstream users deplete aquifers), operates through suppression (geographic and economic barriers prevent relocation or alternative livelihoods), and provides no coordination benefit to victims. For organized beneficiaries, it is tangled rope: water-intensive agriculture does serve coordination function (feeding populations, maintaining rural livelihoods during transition), but this function is systematically subordinated to extraction maximization — water is extracted beyond what nutrition security requires, for commodity export profits. The constraint cannot be decomposed into a pure coordination function with minimal extraction cost. The theater ratio rising from 0.32 to 0.55 shows that as physical depletion accelerates, institutional discourse increasingly emphasizes efficiency and sustainability while maintaining extraction rates — the constraint is degrading into piton (performative sustainability rhetoric without functional change).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aquifer_recovery_feasibility,
    'Can depleted aquifers be restored within a human lifetime (biographical horizon) through managed recharge and extraction reduction, or are they functionally exhausted at timescales that matter to current users?',
    'Hydrological modeling of recharge capacity vs historical extraction rates; comparison of depletion trajectories to recharge potential across major aquifer systems (Ogallala, Indus, North China Plain)',
    'If recoverable: constraint is Scaffold (temporary with sunset) from more perspectives. If irreversible at biographical timescale: constraint is Snare (permanent extraction) for all powerless perspectives. Determines whether exit pathways are genuine or illusory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aquifer_recovery_feasibility, empirical, 'Whether aquifer depletion is reversible within human timescales').

omega_variable(
    alternative_production_viability,
    'Do agronomically and economically viable alternatives to water-intensive crops exist for regions currently dependent on such production, or is high-extraction agriculture the only economically rational option given current markets and infrastructure?',
    'Comparative analysis of crop profitability, market stability, and labor requirements across water-intensive vs alternative production systems; analysis of transition costs and regional feasibility',
    'If alternatives viable: constrained exit is real; moderate/organized agents can shift production. If alternatives limited: exit suppression is structural and severe; constraint is closer to snare for agricultural producers. Affects classification of irrigation-dependent perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_production_viability, empirical, 'Whether economically viable alternatives to water-intensive agriculture exist').

omega_variable(
    subsidy_architecture_necessity,
    'Does water-intensive agricultural production depend on explicit subsidies (water price caps, extraction rights allocation) or would it collapse without them, revealing the constraint''s extractive mechanism?',
    'Cost-benefit analysis of water-intensive agriculture with full-price water; comparison of profitability when extraction costs include actual depletion damage and ecosystem service loss',
    'If subsidy-dependent: the constraint is an enforcement mechanism disguised as coordination. Removal of subsidies would reveal extraction as the primary function, not ancillary. If economically self-sustaining: extraction may be embedded in resource quality/location rather than policy distortion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidy_architecture_necessity, empirical, 'Whether water-intensive agriculture depends on subsidies or genuine comparative advantage').

omega_variable(
    groundwater_commons_governance,
    'Can common-pool groundwater resources be governed through nested polycentric institutions without state-level extraction-maximization incentives, or does aquifer depletion require centralized control that systematically favors organized beneficiaries?',
    'Comparative study of successful and failed groundwater governance regimes (Spanish huertas, Nepalese aquifer management, Rwandan water councils); analysis of institutional design requirements for sustainable commons management',
    'If decentralized governance viable: constraint''s suppression is contingent on institutional choice; policy reform pathway is real. If centralization necessary: constraint is more structural and harder to exit; suppression persists across institutional designs. Affects whether scaffold perspective is realistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(groundwater_commons_governance, conceptual, 'Whether sustainable groundwater governance requires centralized state control').

omega_variable(
    ecosystem_collapse_feedback,
    'At what depletion threshold does groundwater extraction trigger irreversible ecosystem cascades (spring dry-out → riparian collapse → groundwater recharge failure → permanent aquifer loss), and how near is that threshold in major depletion zones?',
    'Hydro-ecological modeling of threshold dynamics; paleoclimate analysis of past aquifer failures and recovery timescales; monitoring of early warning indicators (spring emergence decline, aquifer pressure head reduction)',
    'If thresholds are imminent: suppression is amplified by ecological lock-in; constraint approaches mountain status (irreversible). If thresholds are distant: policy windows remain open; scaffold perspective retains viability. Affects how victims perceive their exit options (trapped vs constrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_collapse_feedback, empirical, 'Proximity to irreversible ecosystem collapse thresholds in major aquifer zones').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(freshwater_depletion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fresh_tr_t0, freshwater_depletion, theater_ratio, 0, 0.32).
narrative_ontology:measurement(fresh_tr_t10, freshwater_depletion, theater_ratio, 10, 0.45).
narrative_ontology:measurement(fresh_tr_t20, freshwater_depletion, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(fresh_be_t0, freshwater_depletion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fresh_be_t10, freshwater_depletion, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(fresh_be_t20, freshwater_depletion, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(freshwater_depletion, resource_allocation).
narrative_ontology:affects_constraint(freshwater_depletion, agricultural_productivity_dependence).
narrative_ontology:affects_constraint(freshwater_depletion, climate_migration_pressure).
narrative_ontology:affects_constraint(freshwater_depletion, groundwater_infrastructure_lock_in).
narrative_ontology:affects_constraint(freshwater_depletion, riparian_ecosystem_collapse).

% DUAL FORMULATION NOTE:
% Freshwater depletion is decomposable into distinct constraints: (1) aquifer extraction as an enforcement mechanism (this story); (2) water pricing architecture that enables below-cost withdrawal; (3) agricultural commodity markets that create demand for water-intensive exports; (4) irrigation infrastructure as capital lock-in. Each has different ε and structural properties. This story focuses on the extraction mechanism itself; downstream constraints capture how extraction effects propagate through agricultural systems, migration pressure, and ecosystem collapse. The upstream constraint is water pricing architecture (constrains prices below cost of groundwater depletion damage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(freshwater_depletion, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
