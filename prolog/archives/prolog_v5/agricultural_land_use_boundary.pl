% ============================================================================
% CONSTRAINT STORY: agricultural_land_use_boundary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agricultural_land_use_boundary, []).

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
 *   constraint_id: agricultural_land_use_boundary
 *   human_readable: Agricultural Land Use Boundary Constraint
 *   domain: environmental_policy/resource_allocation
 *
 * SUMMARY:
 *   The agricultural land use boundary constraint structures the division
 *   between designated agricultural zones and protected/non-agricultural
 *   land, creating asymmetric extraction between industrial agricultural
 *   operators (beneficiaries) and smallhold farmers, ecosystem services, and
 *   water-dependent communities (victims). This constraint exhibits the core
 *   Tangled Rope signature: a genuine coordination function (reducing
 *   transaction costs, enabling infrastructure concentration, standardizing
 *   supply chains) overlaps with asymmetric extraction (externalized water
 *   pollution, absorbed ecosystem costs, smallhold displacement). The
 *   extractiveness has increased from 0.35 to 0.52 over a 40-year interval as
 *   industrial intensification has deepened and climate variability has
 *   increased water stress, shifting from a more balanced coordination
 *   arrangement toward higher asymmetric extraction.
 *
 * KEY AGENTS:
 *   - Smallhold Farmers: Primary victims (powerless/trapped) — cannot exit agricultural dependency; face legal restrictions on land conversion and economic pressure to intensify existing plots
 *   - Industrial Agriculture Operators: Primary beneficiaries (institutional/arbitrage) — capture coordination gains from standardized zones, infrastructure concentration, and labor market pooling; can relocate or arbitrage across jurisdictions
 *   - Ecosystem Services (Abstract Collective): Victim (powerless/trapped) — watershed services, soil carbon, pollinator habitat absorbed as externality with no compensation or voice
 *   - Water Resource Users (Downstream Communities): Secondary victims (moderate/constrained) — face water quality degradation and flow unpredictability; can organize but at high coordination cost
 *   - Commodity Traders and Input Suppliers: Beneficiaries (institutional/arbitrage) — profit from standardized systems and predictable output concentration
 *   - Environmental and Food Sovereignty Movements: Organized challengers (organized/constrained) — have policy voice but face structural barriers from land ownership concentration and trade agreements
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing scale economies as immutable law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agricultural_land_use_boundary, 0.52).
domain_priors:suppression_score(agricultural_land_use_boundary, 0.58).
domain_priors:theater_ratio(agricultural_land_use_boundary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agricultural_land_use_boundary, extractiveness, 0.52).
narrative_ontology:constraint_metric(agricultural_land_use_boundary, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(agricultural_land_use_boundary, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agricultural_land_use_boundary, tangled_rope).
narrative_ontology:human_readable(agricultural_land_use_boundary, "Agricultural Land Use Boundary Constraint").
narrative_ontology:topic_domain(agricultural_land_use_boundary, "environmental_policy/resource_allocation").

domain_priors:requires_active_enforcement(agricultural_land_use_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agricultural_land_use_boundary, industrial_agriculture_operators).
narrative_ontology:constraint_beneficiary(agricultural_land_use_boundary, commodity_traders).
narrative_ontology:constraint_victim(agricultural_land_use_boundary, smallhold_farmers).
narrative_ontology:constraint_victim(agricultural_land_use_boundary, ecosystem_services).
narrative_ontology:constraint_victim(agricultural_land_use_boundary, water_resource_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLHOLD FARMER (SNARE) — Faces legal prohibitions on land conversion, economic pressure to intensify existing plots, and inability to access capital for alternative livelihoods. No meaningful exit from agricultural dependency; extraction is maximal and coercive. Suppression is structural: land rights frameworks prevent expansion, credit systems lock them into commodity chains.
constraint_indexing:constraint_classification(agricultural_land_use_boundary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ECOSYSTEM SERVICES AS POWERLESS AGENT (SNARE) — Watershed services, soil carbon, pollinator habitat, and climate regulation are external to market valuation. The constraint extracts these services at no compensation. Abstract collective good with no exit, no voice, no standing in enforcement mechanisms. Pure extraction target.
constraint_indexing:constraint_classification(agricultural_land_use_boundary, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: WATER RESOURCE USERS (TANGLED ROPE) — Agricultural intensification in upland zones degrades downstream water quality and reliability. These users have some agency (they can organize, lobby, invest in filtration) but face high costs to exit dependency on agricultural-zone water sources. Genuine coordination problem (shared watershed management) overlaps with extraction (water cost externalization).
constraint_indexing:constraint_classification(agricultural_land_use_boundary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INDUSTRIAL AGRICULTURE OPERATORS (ROPE) — Experience the land boundary as coordination mechanism: zoning rules enable predictable input supply and labor markets, create property certainty for investment, and allow standardization of production systems. Net beneficiaries with exit options (can relocate operations, arbitrage across jurisdictions). The constraint subsidizes them through externality absorption.
constraint_indexing:constraint_classification(agricultural_land_use_boundary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COMMODITY TRADERS AND INPUT SUPPLIERS (ROPE) — Benefit from standardized agricultural zones with predictable output volumes and geographic concentration. Lower transaction costs than dispersed smallhold systems. Exit options through arbitrage (can trade alternative commodities, relocate supply chains).
constraint_indexing:constraint_classification(agricultural_land_use_boundary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL AND FOOD SOVEREIGNTY MOVEMENTS (TANGLED ROPE) — Organized agents advocating for boundary reform (agroecology, conservation agriculture, land redistribution). They have agency (coalitions, policy influence) but face structural constraints (agricultural lobby power, land ownership concentration, trade agreements locking in industrial models). The constraint both coordinates a value system they oppose AND extracts from them by blocking policy exits.
constraint_indexing:constraint_classification(agricultural_land_use_boundary, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, some land use concentration may appear inevitable: agriculture has increasing returns to scale in mechanization, infrastructure (roads, storage, processing plants), and knowledge diffusion. Productivity per hectare is highest in consolidated operations. This perspective risks naturalizing what is actually a contingent institutional arrangement (property law, trade policy, credit systems, commodity markets) as an immutable law of resource efficiency. Engine will flag as false summit.
constraint_indexing:constraint_classification(agricultural_land_use_boundary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agricultural_land_use_boundary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agricultural_land_use_boundary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agricultural_land_use_boundary, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agricultural_land_use_boundary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(agricultural_land_use_boundary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The boundary transfers ecosystem service values to industrial operators without compensation. Smallhold farmers are forced into commodity chains or landlessness. Water pollution costs are externalized. However, the extractiveness is not maximal (0.70+) because: (1) genuine coordination gains exist (standardized supply chains, infrastructure economies, knowledge diffusion); (2) enforcement is partly market-based rather than purely coercive; (3) some smallholders benefit through access to commodity markets and technology spillovers. Suppression (0.58): Significant structural barriers include property law frameworks that favor consolidation, credit systems that require collateral and scale, trade agreements that privilege industrial production, and knowledge systems that promote chemical-mechanical agriculture. But suppression is not total — smallholders do persist, agroecological alternatives exist, and policy change is possible. Theater ratio (0.48): Moderate-low. The constraint operates partly through material market forces (genuine scale economies) and partly through institutional theater (land policy, agricultural subsidies, extension systems promoting industrial models). The theater has increased from 0.32 as ecological contradictions have accumulated and required increasingly elaborate justifications.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the danger of the mountain perspective in policy contexts. The naturalizing framing ('agriculture requires consolidation for efficiency') obscures the extractive redistributive choice embedded in the constraint. The gap between the beneficiary's rope (genuine coordination gains) and the smallholder's snare (structural entrapment) reveals that the same constraint can be functionally coordination for agents with exit options and functionally extraction for agents without them. The water users' tangled rope perspective correctly identifies the mixed nature — the constraint does solve genuine coordination problems but does so in a way that extracts from some groups and concentrates benefits in others. The organized movements' perspective shows that the constraint is not immutable — alternative arrangements (agroecological, land redistribution, water cost internalization) are technically feasible but blocked by power asymmetries, not physical limits.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position. Smallhold farmers: victim status + trapped exit → high d (0.92) → high f(d) ≈ 1.35 → high experienced extraction. Industrial operators: beneficiary status + arbitrage exit → low d (0.08) → f(d) ≈ -0.10 → negative/subsidizing extraction. Ecosystem services: victim status + trapped exit → high d (0.95) → f(d) ≈ 1.42 → maximum experienced extraction. Water users: victim-adjacent + constrained exit → moderate-high d (0.68) → f(d) ≈ 1.00 → proportional extraction. Organized movements: mixed beneficiary/victim + constrained exit → moderate d (0.52) → f(d) ≈ 0.68 → moderate extraction below aggregate. Scope modifier σ(national) = 1.0 preserves χ without dampening or amplification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in the agricultural boundary is that 'efficiency' (the mountain justification) and 'extraction' (the snare reality) are not contradictory — they are the same institutional choice. Industrial consolidation does generate genuine coordination gains and productivity increases in commodity yield per hectare. It simultaneously extracts ecosystem services, displaces smallholders, and externalizes water pollution. The constraint resolves the mandatrophy by showing that Tangled Rope is the correct classification: the coordination gains are real AND the extraction is real, not a trade-off between them but a single institutional arrangement that produces both. The false summit (mountain perspective) mistakes one component (coordination gain) for the whole, ignoring the asymmetric distribution. Policy debate should not be 'efficiency vs sustainability' but 'how should coordination gains be distributed, and should externalities be internalized?' — reframing that shifts from mountain naturalization to rope/snare choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    smallhold_productivity_assumption,
    'Are smallhold systems genuinely less productive per hectare than industrial systems, or does standard productivity measurement (commodity yield) exclude nutrient cycling, biodiversity, and resilience outputs?',
    'Full-cost analysis comparing commodity yield vs total ecosystem service provision; longitudinal comparison of smallhold vs industrial systems under equivalent input conditions; measurement of non-commodity outputs (soil organic matter, water infiltration, pest control).',
    'If smallhold = productive: the boundary constraint is pure extraction (Snare for smallholders). If smallhold < productive: the boundary reflects genuine coordination gains (Rope for society, justified asymmetry). Current evidence suggests partial smallhold advantage when ecosystem services are costed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smallhold_productivity_assumption, empirical, 'Productivity comparison when ecosystem services are costed').

omega_variable(
    land_tenure_collapse_mechanism,
    'Is the constraint structurally enforced through property law and credit exclusion, or primarily through market-scale asymmetries and technology access?',
    'Legal analysis of tenure systems; statistical comparison of constraint persistence in jurisdictions with communal vs individual property rights; measurement of smallhold access to credit, inputs, and technology under equivalent legal frameworks.',
    'If legal enforcement is primary: the constraint is a snare relying on state coercion (suppression gate high). If market-scale is primary: it is a snare relying on capital barriers (suppression gate medium). This affects classification stability across political transitions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(land_tenure_collapse_mechanism, empirical, 'Whether constraint is enforced through property law or market-scale mechanisms').

omega_variable(
    agroecology_scale_feasibility,
    'Can agroecological systems (intercropping, crop-livestock integration, natural pest management) scale to feed current global population without consolidation?',
    'Modeling studies comparing total caloric output of agroecological vs industrial systems at global scale; cost analysis of labor inputs and knowledge coordination; comparison with dietary shift scenarios (reduced meat consumption, reduced food waste).',
    'If yes: the boundary is unnecessary coordination overhead (Snare classification stable). If no: boundary reflects genuine coordination constraint for population scale (Rope from civilizational perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(agroecology_scale_feasibility, empirical, 'Whether agroecological systems can scale globally').

omega_variable(
    water_externality_internalization,
    'If water pollution from agricultural runoff were fully costed and charged to agricultural operators, would the boundary remain economically viable for industrial operations?',
    'Cost accounting: water treatment, agricultural runoff cleanup, water-borne disease, ecosystem restoration; market simulation with full externality pricing; comparison with agroecological system costs.',
    'If industrial becomes uneconomic: the boundary is an extraction mechanism hiding behind externalities (Snare confirmed). If industrial remains viable: costs are distributed but the system persists rationally (Tangled Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(water_externality_internalization, preference, 'Whether industrial agriculture is viable with internalized externalities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agricultural_land_use_boundary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agland_tr_t0, agricultural_land_use_boundary, theater_ratio, 0, 0.32).
narrative_ontology:measurement(agland_tr_t20, agricultural_land_use_boundary, theater_ratio, 20, 0.4).
narrative_ontology:measurement(agland_tr_t40, agricultural_land_use_boundary, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(agland_be_t0, agricultural_land_use_boundary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(agland_be_t20, agricultural_land_use_boundary, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(agland_be_t40, agricultural_land_use_boundary, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agricultural_land_use_boundary, resource_allocation).
narrative_ontology:affects_constraint(agricultural_land_use_boundary, water_resource_allocation).
narrative_ontology:affects_constraint(agricultural_land_use_boundary, soil_carbon_pricing).
narrative_ontology:affects_constraint(agricultural_land_use_boundary, smallhold_credit_access).
narrative_ontology:affects_constraint(agricultural_land_use_boundary, trade_commodity_pricing).

% DUAL FORMULATION NOTE:
% The agricultural boundary decomposes into multiple structurally distinct constraints when measured by different observables. Commodity productivity analysis yields one ε (high efficiency, low extraction). Ecosystem service analysis yields different ε (high extraction, coordination secondary). Water impact analysis yields third ε (asymmetric externality). These are linked by affecting_constraints network — the boundary constraint influences all three downstream claims and would require negotiating all three to reform.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(agricultural_land_use_boundary, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
