% ============================================================================
% CONSTRAINT STORY: french_ag_land_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_french_ag_land_concentration, []).

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
 *   constraint_id: french_ag_land_concentration
 *   human_readable: French Agricultural Land Concentration
 *   domain: economic/agricultural_policy
 *
 * SUMMARY:
 *   French agricultural land concentration represents a multi-decade
 *   structural transformation driven by mechanization, subsidy incentives,
 *   and credit allocation patterns. The constraint exhibits the hallmark
 *   Tangled Rope structure: it solves a genuine coordination problem (input
 *   logistics, equipment efficiency, labor markets) while simultaneously
 *   extracting from a substantial class of actors (small farmers, rural
 *   communities). The extractiveness has increased from 0.32 to 0.58 over
 *   four decades as consolidation has become self-reinforcing through land
 *   value inflation and subsidy structure. The low theater ratio (0.45)
 *   indicates that the underlying consolidation is real and functional—this
 *   is not a performative ritual but an actual economic mechanism. However,
 *   the constraint now faces pressure from agro-ecological movements and
 *   climate-focused agricultural reform (carbon markets, regenerative
 *   premiums), which may provide a genuine sunset pathway via alternative
 *   economic models. The piton perspective captures the collapse of rural
 *   village economies: the institutions remain (town halls, cooperatives) but
 *   their primary function (supporting multi-family farming) has atrophied.
 *   The mountain perspective risks naturalizing a contingent policy choice as
 *   an iron law of agricultural economics.
 *
 * KEY AGENTS:
 *   - Small Farmers: Primary victims (powerless/trapped) — face mechanization costs, land value inflation, subsidy structures that disadvantage smaller operations
 *   - Large Farm Operators: Primary beneficiaries (institutional/arbitrage) — capture economies of scale, favorable subsidy treatment, operational flexibility
 *   - Agricultural Ministry & CAP Structure: Institutional enforcer (organized/constrained) — maintains consolidation-favoring subsidy systems while constrained by EU requirements and rural political backlash
 *   - Agro-Ecological Movement: Organized reformers (organized/mobile) — environmental NGOs, young farmers, climate advocates building alternative economic models with sunset potential
 *   - Rural Village Communities: Victims through degradation (institutional/constrained) — experienced primary function collapse; now maintained through heritage subsidies (piton state)
 *   - Input Consolidators (Equipment, Seed, Fertilizer): Secondary beneficiaries (institutional/arbitrage) — larger farms create simplified distribution networks and standardized input requirements
 *   - Credit and Land Finance Systems: Institutional participants (institutional/arbitrage) — land value appreciation and consolidation financing create extraction through debt and asset concentration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_ag_land_concentration, 0.58).
domain_priors:suppression_score(french_ag_land_concentration, 0.62).
domain_priors:theater_ratio(french_ag_land_concentration, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_ag_land_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(french_ag_land_concentration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(french_ag_land_concentration, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(french_ag_land_concentration, tangled_rope).
narrative_ontology:human_readable(french_ag_land_concentration, "French Agricultural Land Concentration").
narrative_ontology:topic_domain(french_ag_land_concentration, "economic/agricultural_policy").

domain_priors:requires_active_enforcement(french_ag_land_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(french_ag_land_concentration, large_farm_operators).
narrative_ontology:constraint_beneficiary(french_ag_land_concentration, agricultural_equipment_manufacturers).
narrative_ontology:constraint_beneficiary(french_ag_land_concentration, input_consolidators).
narrative_ontology:constraint_victim(french_ag_land_concentration, small_holders).
narrative_ontology:constraint_victim(french_ag_land_concentration, rural_communities).
narrative_ontology:constraint_victim(french_ag_land_concentration, agricultural_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL FARMER (SNARE) — Trapped by mechanization costs, credit requirements, and land value inflation. Cannot exit without abandoning livelihood. Bears full cost of consolidation pressure through land value escalation and inability to compete with economies of scale. No coordination benefit; pure extraction.
constraint_indexing:constraint_classification(french_ag_land_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-SIZED COOPERATIVE (TANGLED ROPE) — Constrained by subsidy structures and input pricing but benefits from cooperative pooling of resources and shared equipment. Experiences both extraction (pressure to consolidate for subsidy efficiency) and coordination (collective bargaining for input prices). Moderate agency through regional networks.
constraint_indexing:constraint_classification(french_ag_land_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LARGE AGRICULTURAL CORPORATION (ROPE) — Benefits from consolidation through economies of scale and simplified logistics. Experiences constraint as coordination mechanism: land consolidation enables efficient input distribution and standardized production protocols. Net beneficiary with high exit options (can exit French market or lease arrangements at will).
constraint_indexing:constraint_classification(french_ag_land_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AGRICULTURAL MINISTRY & CAP STRUCTURE (TANGLED ROPE) — Enforces consolidation through subsidy structures that reward farm size and mechanization. Possesses institutional power and organizational capacity but constrained by EU CAP requirements and rural political backlash. Enforcement mechanism is active: acreage-based payments and equipment subsidies create strong incentive gradients toward consolidation. Secondary beneficiary through simplified administration.
constraint_indexing:constraint_classification(french_ag_land_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AGRO-ECOLOGICAL REFORM MOVEMENT (SCAFFOLD) — Organized advocates (environmental ministries, NGOs, young farmer collectives) see consolidation as a temporary institutional arrangement with a sunset. Regenerative agriculture, polyculture farms, and carbon credit markets are building alternative economic models that reward smaller-scale, biodiverse operations. High agency and visible exit pathway. Theater is minimal — actual alternative production systems are being deployed.
constraint_indexing:constraint_classification(french_ag_land_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: RURAL VILLAGE ECONOMY (PITON) — The traditional multi-family farm village was the primary unit. That function has atrophied — consolidation has eliminated most of the small holdings that sustained rural communities. The village remains institutionally, with town halls, schools, and cultural events, but the economic base has collapsed. Maintenance is now performative: subsidized cultural festivals, heritage preservation grants. Theater ratio is high because the institutions persist without primary function.
constraint_indexing:constraint_classification(french_ag_land_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ECONOMIC NATURALIZATION (MOUNTAIN) — From a civilizational view, consolidation appears as an immutable law of agricultural economics: mechanization and capital intensity necessarily drive consolidation; small farms cannot compete with economies of scale. This perspective naturalizes what is actually a contingent policy choice (subsidy structures, credit allocation, land-value taxation). The engine's false summit detector will identify this as naturalization of institutional arrangements.
constraint_indexing:constraint_classification(french_ag_land_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_ag_land_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(french_ag_land_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(french_ag_land_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(french_ag_land_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(french_ag_land_concentration, TR),
    TR >= 0.70.

:- end_tests(french_ag_land_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not extreme. The constraint demonstrates clear extraction (small farmers exit while their land passes to larger operations at inflated values). However, the underlying coordination function is real—consolidation does enable efficient input distribution and mechanized operations. The value reflects that extraction is structural and self-reinforcing but not total: some small farmers survive through niche markets, cooperatives, or agro-ecological premiums. Suppression (0.62): Moderate-high. Multiple barriers prevent small farmers from competing: mechanization capital requirements, credit gatekeeping, subsidy structures that reward acreage, and land value inflation that makes entry prohibitive. However, suppression is not absolute—exit options exist through cooperative structures, agro-ecological pivots, or part-time farming. Theater ratio (0.45): Moderate-low. The consolidation is functionally real, not performative. Mechanization actually reduces labor needs; scale actually reduces per-unit input costs. This is not a ritual maintained by institutional inertia but an active economic mechanism. The theater that exists (heritage preservation grants, rural development subsidies) is supplementary to the core consolidation dynamic.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximal perspectival divergence. The small farmer sees a snare (trapped, no alternatives, pure extraction). The large operator sees a rope (coordination mechanism enabling efficient operations). The ministry sees tangled rope (they both solve a coordination problem AND extract value). The agro-ecological movement sees a scaffold (temporary arrangement with a visible sunset via carbon markets and regenerative economics). The rural village sees a piton (institutional forms persist, but primary economic function has collapsed). The civilizational analytical observer risks seeing a mountain (natural law of agricultural economics), but this naturalizes what is actually a policy choice about subsidy design and credit allocation. The engine's false summit detector should flag this.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position and exit options. Small farmers (trapped exit, victim status) experience maximum d → high f(d) → high effective extraction chi. Large operators (arbitrage exit, beneficiary status) experience low d → low/negative f(d) → they experience the constraint as beneficial coordination. The ministry has organized power and constrained exit (locked into CAP structure by EU), placing them in institutional beneficiary position with moderate d. The agro-ecological movement has organized power and mobile exit (can deploy alternative systems), giving them lower d despite partial victim status. Rural communities have constrained exit and victim status historically, but their institutions now lack primary function—they are maintained through separate subsidy mechanisms (piton). This decomposition requires separate perspectives for each institutional actor because their exit options and structural relationships differ substantially.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through structural differentiation of actors and their exit options. The potential error is naturalizing consolidation as inevitable economic law (mountain perspective). The actual structure is: a genuine coordination problem (mechanization requires scale) is being solved through institutional mechanisms (subsidy structures, credit allocation, land-value taxation policies) that extract from one class (small farmers) while benefiting another (large operators). If the subsidy and credit structures were redesigned, the same mechanization coordination could be achieved with lower extraction—proving that the extractiveness is not an immutable feature of agricultural economics but a contingent feature of French policy design. The scaffold perspective (agro-ecological alternatives) tests this: if regenerative and polyculture systems can become economically competitive through carbon credits and organic premiums, the constraint may reverse. This would conclusively show that consolidation was tangled rope (coordination + extraction), not mountain (natural law).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_elasticity_threshold,
    'What level of CAP subsidy reform would shift the consolidation dynamic from snare to rope for small holders?',
    'Comparative policy analysis: subsidy-per-hectare vs subsidy-per-farm-entity models; pilot regions with different payment structures; historical data from subsidy reforms (2003, 2015 CAP revisions)',
    'If threshold exists at current funding levels: structural reform is possible. If consolidation is driven primarily by mechanization cost (not subsidies): subsidy reform alone cannot reverse trend.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_elasticity_threshold, empirical, 'CAP subsidy elasticity for consolidation reversal').

omega_variable(
    agro_ecological_market_viability,
    'Can carbon markets, organic premiums, and regenerative agriculture create a sustainable economic alternative to consolidation before rural communities collapse entirely?',
    'Market analysis of carbon credit pricing and volume; organic crop price differentials vs conventional; profitability modeling for small-scale regenerative operations; timeline comparison with rural depopulation rates',
    'If viable at scale within 10 years: scaffold sunset is real and the constraint may reverse. If not viable: agro-ecological movement is aspirational theater, and consolidation is structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(agro_ecological_market_viability, empirical, 'Market viability of regenerative agriculture as consolidation alternative').

omega_variable(
    land_value_spiral_reversibility,
    'Is land value inflation (driven by consolidation itself) reversible by policy, or has it created a structural lock-in where small-scale farming is no longer mathematically viable?',
    'Land-value time series analysis; correlation with consolidation rates; modeling of land-tax or speculation-control policies and their historical effectiveness in other countries; assessment of whether current valuations can sustain small-farm economics even with optimal subsidies',
    'If reversible: policy intervention (land tax, speculation controls, tenant protections) could restore viability. If locked-in: consolidation is now semi-autonomous and resistant to subsidy-based remedies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(land_value_spiral_reversibility, empirical, 'Whether land value inflation creates irreversible consolidation lock-in').

omega_variable(
    intergenerational_succession_mechanism,
    'What fraction of small-farm abandonment is driven by active extraction (pricing out young farmers) vs passive succession failure (aging owners with no heirs willing to stay)?',
    'Demographic analysis of farm succession patterns; interviews with exiting farmers; comparison of abandonment rates with land purchase prices for new farmers; assessment of family farm culture shift',
    'If active extraction dominates: policy can address barriers (credit access, land pricing). If succession failure dominates: the constraint is demographic and cultural, not purely economic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_succession_mechanism, empirical, 'Relative contribution of active extraction vs cultural succession failure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(french_ag_land_concentration, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fagland_tr_t0, french_ag_land_concentration, theater_ratio, 0, 0.28).
narrative_ontology:measurement(fagland_tr_t20, french_ag_land_concentration, theater_ratio, 20, 0.38).
narrative_ontology:measurement(fagland_tr_t40, french_ag_land_concentration, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(fagland_be_t0, french_ag_land_concentration, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fagland_be_t20, french_ag_land_concentration, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(fagland_be_t40, french_ag_land_concentration, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(french_ag_land_concentration, resource_allocation).
narrative_ontology:affects_constraint(french_ag_land_concentration, european_cap_subsidy_structure).
narrative_ontology:affects_constraint(french_ag_land_concentration, rural_property_speculation).

% DUAL FORMULATION NOTE:
% French agricultural land concentration is downstream of EU CAP subsidy design (acreage-based payments favor larger operations) and upstream of rural community collapse. The constraint family includes the CAP structure itself (institutional-level mechanism) and individual regional land-concentration stories with different ε values based on local geography and inheritance patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(french_ag_land_concentration, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
