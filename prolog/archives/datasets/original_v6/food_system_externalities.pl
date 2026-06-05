% ============================================================================
% CONSTRAINT STORY: food_system_externalities
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_food_system_externalities, []).

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
 *   constraint_id: food_system_externalities
 *   human_readable: Food System Externalities and Agricultural Extraction
 *   domain: environmental/economic/social
 *
 * SUMMARY:
 *   The global food system exhibits a structural constraint where the
 *   coordination benefits of industrialized supply chains, logistics
 *   networks, and price stabilization mechanisms are genuine but become a
 *   vehicle for systematic extraction. Industrial food producers, retailers,
 *   and commodity traders capture efficiency gains from coordination while
 *   externalizing costs onto agricultural workers, small-scale farmers, and
 *   ecosystems. The constraint demonstrates how a coordination mechanism
 *   (supply chain standardization, price information systems, logistics
 *   networks) can simultaneously function as an extraction system
 *   (cost-shifting, labor suppression, resource depletion). The
 *   extractiveness has increased from 0.35 to 0.58 over the measurement
 *   interval as industrial consolidation has intensified, while theater_ratio
 *   has risen from 0.32 to 0.55 as subsidy and certification mechanisms have
 *   become increasingly performative without addressing underlying structural
 *   asymmetries. This pattern — rising extractiveness alongside rising
 *   theater — is diagnostic of a constraint degrading from tangled rope
 *   toward snare as the coordination function declines and extraction becomes
 *   the dominant mechanism.
 *
 * KEY AGENTS:
 *   - Industrial Food Producers: Primary beneficiary (institutional/arbitrage) — capture supply chain efficiency gains, price-setting power, subsidy allocation
 *   - Agricultural Workers and Small Farmers: Primary victim (powerless/trapped) — bear commodity price suppression, labor exploitation, forced externality absorption
 *   - Retail Corporations and Commodity Traders: Secondary beneficiary (institutional/arbitrage) — control distribution, pricing, and information asymmetry
 *   - Downstream Communities: Secondary victim (moderate/trapped) — experience environmental contamination, health impacts from agricultural chemical runoff
 *   - Ecosystem Services: Tertiary victim (analytical/trapped) — soil degradation, water depletion, biodiversity loss treated as free externality
 *   - Agricultural Subsidy Systems: Institutional actor (institutional/constrained) — designed for stabilization; captured to concentrate benefits on large producers
 *   - Labor Justice and Environmental Coalitions: Organized agents (organized/constrained) — building countervailing power; constrained by institutional capture
 *   - Regenerative and Local Food Initiatives: Organized agents (organized/constrained) — building alternative pathways with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(food_system_externalities, 0.58).
domain_priors:suppression_score(food_system_externalities, 0.68).
domain_priors:theater_ratio(food_system_externalities, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(food_system_externalities, extractiveness, 0.58).
narrative_ontology:constraint_metric(food_system_externalities, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(food_system_externalities, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(food_system_externalities, tangled_rope).
narrative_ontology:human_readable(food_system_externalities, "Food System Externalities and Agricultural Extraction").
narrative_ontology:topic_domain(food_system_externalities, "environmental/economic/social").

domain_priors:requires_active_enforcement(food_system_externalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(food_system_externalities, industrial_food_producers).
narrative_ontology:constraint_beneficiary(food_system_externalities, retail_food_corporations).
narrative_ontology:constraint_beneficiary(food_system_externalities, commodity_traders).
narrative_ontology:constraint_victim(food_system_externalities, agricultural_workers).
narrative_ontology:constraint_victim(food_system_externalities, small_scale_farmers).
narrative_ontology:constraint_victim(food_system_externalities, downstream_communities).
narrative_ontology:constraint_victim(food_system_externalities, ecosystem_services).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AGRICULTURAL WORKERS AND SMALL FARMERS (SNARE) — Trapped by economic dependency, lack of alternative livelihood, and geographic immobility. Bear full extraction: below-cost commodity prices, unsustainable labor conditions, resource depletion on their land. No coordination benefit. Maximum suppression — cannot negotiate prices, organize labor, or exit the system without catastrophic loss.
constraint_indexing:constraint_classification(food_system_externalities, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-SCALE REGIONAL PRODUCERS (TANGLED ROPE) — Experience both coordination and extraction. Genuine coordination benefit from logistics networks, price information systems, and market access. But also significant extraction: price-setting by processors, cost externalization onto their land and labor, pressure to intensify production. Constrained by capital and infrastructure dependencies; moderate exit cost but alternatives are difficult.
constraint_indexing:constraint_classification(food_system_externalities, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INDUSTRIAL FOOD PRODUCERS AND RETAILERS (ROPE) — Primary beneficiaries experiencing the constraint as pure coordination: supply chain logistics, price stabilization, market access standardization. Arbitrage options available (vertical integration, geographic sourcing diversification, product switching). Net extraction flows toward this agent — they capture coordination efficiency gains. Theater low relative to beneficiary group, indicating functional coordination mechanism from their position.
constraint_indexing:constraint_classification(food_system_externalities, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AGRICULTURAL SUBSIDY AND TRADE POLICY (PITON) — Institutional structure that persists through political inertia despite degraded function. Originally coordinated agricultural stabilization; now largely allocates subsidies to large producers and commodity traders, not small farmers. Theater ratio high (0.55+) — performative policy language about 'supporting farmers' masks extraction toward industrial producers. Low functional verification that policies achieve stated aims. Maintained through institutional sunk costs and political veto points, not because it works.
constraint_indexing:constraint_classification(food_system_externalities, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ENVIRONMENTAL AND LABOR COALITIONS (TANGLED ROPE) — Organized agents with agency and ability to demand structural change. Genuine coordination benefit from collective action frameworks (certification schemes, supply chain auditing, transparency standards). But also experience extraction: regulatory capture limits enforcement, corporate greenwashing absorbs critique energy, costs of compliance monitoring often passed to coalitions. Sunset dynamics present as regulatory windows open and close.
constraint_indexing:constraint_classification(food_system_externalities, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: REGENERATIVE AND LOCAL FOOD SYSTEMS (SCAFFOLD) — Organized agents building alternative coordination mechanisms with explicit sunset clauses: farmer cooperatives, direct-to-consumer networks, regional agroecology hubs. Low effective extraction because the mechanism explicitly declines the industrial system's terms and builds parallel pathways. Suppression remains high (market barriers, infrastructure gaps) but agents have exit agency. Sunset implicit: as local systems scale and internalize transaction costs, the temporary coordination theater declines.
constraint_indexing:constraint_classification(food_system_externalities, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / THERMODYNAMIC INEVITABILITY (MOUNTAIN) — Risk of false summit: framing food system externalities as inevitable thermodynamic costs of nutrient extraction, population scale, or biological complexity. This naturalizes the constraint, treating cost-shifting as inherent rather than contingent. However, structural data contradicts mountain classification — the extraction is maintained through enforceable property rights, subsidy allocation, and price-setting power, not through immutable physical limits. The engine flags this as a false summit, revealing the 'inevitable cost' framing as ideology rather than physics.
constraint_indexing:constraint_classification(food_system_externalities, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(food_system_externalities_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(food_system_externalities, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(food_system_externalities, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(food_system_externalities, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(food_system_externalities, TR),
    TR >= 0.70.

:- end_tests(food_system_externalities_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The food system's coordination benefits are real — supply chain logistics, price information, logistics standardization enable feeding a global population. But the magnitude of cost-shifting is substantial: agricultural commodity prices have declined in real terms for 40 years despite productivity gains, labor conditions remain severely suppressed in much of the value chain, and environmental costs are fully externalized. The 0.58 value reflects that the extraction is not maximum (some coordination function persists, some producer benefit exists) but is substantial and has intensified over time. Suppression (0.68): High. Trapped producers face: commodity price-setting by processors/retailers they cannot negotiate, lack of alternative crops/markets, geographic immobility, capital barriers to switching systems, and political inability to organize against consolidated buyers. Suppression is not total (some producers have exited to alternative systems) but is severe enough that most remain locked in. Theater ratio (0.55): Moderate-high. Subsidy programs, certification schemes (Fair Trade, organic, sustainability certifications), and corporate sustainability commitments are substantially performative — they signal commitment to change while maintaining underlying extraction structures. The theater has increased as legitimacy pressure has mounted but structural change has not materialized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The industrial food producer sees rope — genuine coordination solving complex logistics and price discovery problems. The small farmer sees snare — price-setting power, labor suppression, and forced externality absorption with no exit. The coalition sees tangled rope with sunset — genuine coordination mechanisms that are now captured, but alternative pathways (regenerative systems, regional food hubs) are reducing their dominance. The subsidy framework sees piton — designed for stabilization but captured to concentrate benefits, maintained through institutional sunk costs. The regenerative systems see scaffold — temporary coordination challenges being solved by building parallel infrastructure. The civilizational observer risks seeing mountain — natural limits to feeding a global population — but the structural data reveals this as naturalization: the actual constraint is extractive pricing power and institutional capture, not physics.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derives from structural relationships to the extraction flow. Trapped small farmers with no exit options experience maximum d (0.90+) — they bear full structural cost and have no way to shift it. Industrial producers with arbitrage options (geographic sourcing, vertical integration, product switching) experience low d (0.10-0.20) — they can exit individual supplier relationships and have multiple revenue options. Organized coalitions with constrained exit (organizing capacity but capital and infrastructure constraints) experience moderate d (0.50-0.65) — they can exert countervailing pressure but cannot fully escape the system. The shift from beneficiary to victim status is stark: those extracting (producers/retailers) are institutional/arbitrage; those bearing costs (workers/farmers/ecosystems) are powerless/trapped or moderate/constrained. This asymmetry is the core of the tangled rope classification — real coordination function exists, but asymmetrically captured.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The food system externalities constraint resolves mandatrophy by showing that tangled rope is the correct analytical classification because the system performs BOTH coordination AND extraction simultaneously and these are not separable. The coordination function (supply chain efficiency, price discovery, logistics standardization) is real and valuable — it enables feeding 8+ billion people. But this coordination function is asymmetrically captured by industrial producers and retailers, who use control of distribution and pricing to extract from farmers and workers, and who externalize environmental and social costs. No reformulation of the constraint eliminates either the coordination or the extraction — they are structurally interdependent. Attempts to 'fix' the system by emphasizing coordination (Rope framing) ignore the extraction magnitude. Attempts to 'fix' the system by emphasizing pure extraction (Snare framing) ignore that the coordination function is genuinely solving complex collective action problems. The tangled rope classification is the analytical truth — this system coordinates AND extracts, and that combination is why it persists despite persistent ethical and environmental failure. Regulatory capture (subsidy allocation, certification gaming) and institutional inertia (sunk costs in industrial infrastructure) maintain the hybrid form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_measurement_threshold,
    'At what extraction magnitude does cost-shifting become a deliberate policy rather than an unintended consequence?',
    'Historical analysis of subsidy design, price-setting transparency, and environmental impact documentation; comparison of stated policy aims vs measurable outcomes; regulatory capture analysis',
    'If threshold low: current system is primarily extractive (snare-dominant). If threshold high: system retains coordination intent despite degradation (tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_measurement_threshold, empirical, 'Threshold distinguishing policy consequence from deliberate extraction').

omega_variable(
    regenerative_system_scalability,
    'Can regenerative and local food systems scale to feed global population without reintroducing industrial-scale externality pressures?',
    'Long-term monitoring of system growth: energy return on investment, land productivity, labor scalability, capital requirements; comparison of per-capita environmental footprint at different scales',
    'If scalable without reversion: scaffold sunset is real and structural. If scalability requires industrial inputs: scaffold is aspirational theater, and underlying snare persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regenerative_system_scalability, empirical, 'Whether regenerative systems can scale without reintroducing externalities').

omega_variable(
    subsidy_effectiveness_ambiguity,
    'Do agricultural subsidies stabilize farmer income or primarily flow to commodity traders and processors, masking extraction as support?',
    'Subsidy flow mapping: tracing payments from government through intermediaries to actual farming households; longitudinal income analysis for subsidy recipients vs non-recipients; cross-national comparison of subsidy design and outcomes',
    'If stabilizing farm income: policy is genuinely coordinating (rope from farmer perspective). If flowing to processors: subsidy structure is active extraction mechanism (snare becomes visible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidy_effectiveness_ambiguity, empirical, 'Whether subsidies stabilize farmers or flow to extractive intermediaries').

omega_variable(
    internalized_cost_perception,
    'Do trapped agricultural workers and small farmers perceive low commodity prices as market signal or as systemic extraction?',
    'Qualitative research on farmer framing of pricing; analysis of organizing attempts and framing narratives; comparison of exit rates when alternative pathways become visible vs when trapped in commodity production',
    'If market signal perceived: extraction mechanism relies on cognitive capture. If systemic extraction perceived: organizing potential increases, indicating coalition power possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_cost_perception, conceptual, 'Whether low prices are perceived as market signals or systemic extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(food_system_externalities, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(food_ext_tr_t0, food_system_externalities, theater_ratio, 0, 0.32).
narrative_ontology:measurement(food_ext_tr_t20, food_system_externalities, theater_ratio, 20, 0.45).
narrative_ontology:measurement(food_ext_tr_t40, food_system_externalities, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(food_ext_be_t0, food_system_externalities, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(food_ext_be_t20, food_system_externalities, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(food_ext_be_t40, food_system_externalities, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(food_system_externalities, resource_allocation).
narrative_ontology:boltzmann_floor_override(food_system_externalities, 0.2).
narrative_ontology:affects_constraint(food_system_externalities, agricultural_labor_exploitation).
narrative_ontology:affects_constraint(food_system_externalities, environmental_cost_externalization).
narrative_ontology:affects_constraint(food_system_externalities, commodity_price_suppression).
narrative_ontology:affects_constraint(food_system_externalities, agricultural_subsidy_capture).

% DUAL FORMULATION NOTE:
% Food system externalities decomposes into structurally distinct constraints: (1) commodity_price_suppression (ε≈0.65, Snare) — cost-shifting via price-setting power; (2) agricultural_labor_exploitation (ε≈0.72, Snare) — labor suppression via immobility and consolidation; (3) environmental_cost_externalization (ε≈0.55, Tangled Rope) — coordination of production alongside cost-shifting to commons. This story models the unified constraint; upstream stories address specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(food_system_externalities, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
