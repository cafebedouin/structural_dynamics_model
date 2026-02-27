% ============================================================================
% CONSTRAINT STORY: condiment_tyranny
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_condiment_tyranny, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: condiment_tyranny
 *   human_readable: The Tyranny of the Default Condiment Offering
 *   domain: social/economic
 *
 * SUMMARY:
 *   The tyranny of the default condiment offering is a structural constraint
 *   emerging from the economics of large-scale food service operations. QSR
 *   chains, casual dining, institutional cafeterias, and airline catering
 *   have converged on a standardized set of condiments (ketchup, mustard,
 *   mayonnaise, hot sauce, ranch dressing) that minimize sourcing complexity,
 *   inventory management, and customer decision time. This standardization
 *   creates genuine efficiencies: reduced SKU count, simplified procurement,
 *   predictable supply chains, and consistent customer experience across
 *   locations. However, it simultaneously constrains the condiment
 *   preferences of consumers in many regions, locks out small artisanal
 *   producers, and gradually homogenizes food culture. The constraint
 *   exhibits hybrid character — it solves real coordination problems (Rope
 *   from the chain and manufacturer perspectives) while simultaneously
 *   extracting diversity from consumer surplus and market access from niche
 *   producers (Snare from their perspectives). The theater_ratio (0.58)
 *   reflects that while the efficiency justification is substantive, it
 *   increasingly serves as post-hoc rationalization for convenience — supply
 *   chain technology has matured to the point where diversity is economically
 *   feasible but not yet operationally normalized. The craft food movement
 *   and regionalization trends suggest the constraint may have a temporal
 *   sunset as consumer demand for culinary diversity increases market value
 *   for sourcing complexity.
 *
 * KEY AGENTS:
 *   - Consumers with Niche Preferences: Primary victim (powerless/trapped) — trapped by global standardization; cannot access preferred condiments within mainstream food service
 *   - Small Artisanal Condiment Producers: Primary victim (powerless/trapped) — locked out of national distribution by volume requirements and supply chain standardization
 *   - Large Food Service Chains: Primary beneficiary (institutional/arbitrage) — capture efficiency gains and network coordination benefits from standardization
 *   - Dominant Condiment Manufacturers: Secondary beneficiary (organized/arbitrage) — benefit from volume concentration and switching costs
 *   - Independent Restaurant Owners: Mixed position (moderate/constrained) — constrained by supply chain efficiency pressures but also benefit from coordination
 *   - Craft Food Movement: Organized counter-force (organized/mobile) — creating alternative distribution pathways with potential sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(condiment_tyranny, 0.38).
domain_priors:suppression_score(condiment_tyranny, 0.52).
domain_priors:theater_ratio(condiment_tyranny, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(condiment_tyranny, extractiveness, 0.38).
narrative_ontology:constraint_metric(condiment_tyranny, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(condiment_tyranny, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(condiment_tyranny, tangled_rope).
narrative_ontology:human_readable(condiment_tyranny, "The Tyranny of the Default Condiment Offering").
narrative_ontology:topic_domain(condiment_tyranny, "social/economic").

domain_priors:requires_active_enforcement(condiment_tyranny).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(condiment_tyranny, large_food_service_operators).
narrative_ontology:constraint_beneficiary(condiment_tyranny, condiment_manufacturers).
narrative_ontology:constraint_victim(condiment_tyranny, culinary_preference_diversity).
narrative_ontology:constraint_victim(condiment_tyranny, small_condiment_producers).
narrative_ontology:constraint_victim(condiment_tyranny, consumers_with_niche_preferences).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NICHE PALATE CONSUMER (SNARE) — Trapped within global food service system where regional, spicy, fermented, or unconventional condiments are unavailable. Individual cannot exit without bearing high cost (seeking specialty shops, meal preparation overhead). d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(condiment_tyranny, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL ARTISANAL PRODUCER (SNARE) — Locked out of major distribution channels by supply chain standardization and volume requirements. Replicating access to national restaurant chains is prohibitively expensive. d≈0.90, f(d)≈1.36, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(condiment_tyranny, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INDEPENDENT RESTAURANT (TANGLED ROPE) — Constrained by supply chain efficiency pressures and customer expectations set by chains, yet benefits from coordination function: standardized condiments reduce sourcing complexity and customer confusion. d≈0.62, f(d)≈0.85, σ=0.9 → χ≈0.31.
constraint_indexing:constraint_classification(condiment_tyranny, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: LARGE FOOD SERVICE CHAIN (ROPE) — Primary beneficiary. Standardized condiment offering solves genuine coordination problem: reduces inventory complexity, enables supply chain efficiency, and creates consistent customer experience across locations. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(condiment_tyranny, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DOMINANT CONDIMENT MANUFACTURER (ROPE) — Solves coordination for food service operators: single supplier managing consistent quality, volume, and pricing. Benefits from network effects and switching costs. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.04. Coordination function outweighs extraction.
constraint_indexing:constraint_classification(condiment_tyranny, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSUMER EXPECTATION INERTIA (PITON) — The constraint persists partly through habituation: customers expect certain condiments because they've always been offered. Theater_ratio=0.58 suggests moderate performative content — the justification (efficiency, consistency) is real but increasingly theatrical as supply chains mature. The constraint is maintained by inertia as much as genuine functional necessity. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.03.
constraint_indexing:constraint_classification(condiment_tyranny, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: CRAFT FOOD MOVEMENT (SCAFFOLD) — Organized counter-movement (artisanal restaurants, farmers markets, food tourism, regional cuisine revival) is creating parallel distribution pathways for diverse condiments. This perspective sees the tyranny as a temporary coordination failure with a sunset: as localism and culinary diversity become market values, supply chains will fragment and revert to regional/niche offerings. d≈0.35, f(d)≈0.32, σ=0.9 → χ≈0.10. Low effective extraction because exit pathways exist and are expanding.
constraint_indexing:constraint_classification(condiment_tyranny, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(condiment_tyranny_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(condiment_tyranny, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(condiment_tyranny, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(condiment_tyranny, TR),
    TR >= 0.70.

:- end_tests(condiment_tyranny_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts consumer surplus (niche preferences unavailable), producer surplus (market access barriers), and diversity from the food system. However, the extraction is not severe because (a) consumers can still access preferred condiments outside mainstream food service, (b) alternative distribution channels exist (specialty shops, farmers markets, international markets), and (c) the constraint does solve genuine coordination problems. The value (0.38 vs initial overestimate of 0.50+) reflects that the extraction is real but bounded. Suppression (0.52): Moderate-high. Significant barriers exist: supply chain economies of scale, infrastructure designed for standardized inputs, consumer expectations trained by chain consistency, and the high cost for independent operators to maintain diverse inventory. But suppression is not total — regions with strong food cultures maintain alternatives, and emerging logistics technologies are reducing barriers. Theater ratio (0.58): Moderate. The efficiency justification for standardization has real content (supply chain complexity reduction, inventory management simplification), but increasingly serves as post-hoc rationalization. As logistics mature, the performative content rises relative to functional necessity. The rising trajectory (0.35→0.58 over 50 years) reflects that the constraint persists increasingly through habituation and expectation management rather than hard technical necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how structural position determines classification. Large chains and manufacturers experience it as coordination (Rope) — solving genuine problems of complexity and consistency. Consumers with diverse preferences and artisanal producers experience it as pure extraction (Snare) — locked out with no viable alternative. Independent restaurants occupy the middle (Tangled Rope) — constrained by system pressures but also benefiting from coordination. The craft food movement sees a temporary state with a sunset (Scaffold) — emerging alternative pathways (localism, culinary tourism, farm-to-table) are building ways to escape the tyranny. Inertia perspective (Piton) reveals that the constraint increasingly persists through habituation rather than necessity. The perspectival gap reflects genuine structural differences in exit options and beneficiary/victim status, not measurement ambiguity.
 *
 * DIRECTIONALITY LOGIC:
 *   Large chain operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Strong net beneficiaries. Condiment manufacturers: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiaries. Niche consumers: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction — no viable exit. Small producers: Victim + trapped → d≈0.90, f(d)≈1.36. High extraction — market access blocked. Independent restaurants: Mixed (moderate/constrained) → d≈0.62, f(d)≈0.85. Constrained by system but also benefiting from coordination. Craft movement: Organized + mobile → d≈0.35, f(d)≈0.32. Low effective extraction because coalition has agency and visible exit paths. The divergence between beneficiary and victim directionalities drives the perspectival gap and mandates the Tangled Rope classification at the moderately-powered level.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION: The 'condiment tyranny' conflates two structurally distinct constraints that should be analyzed separately: (1) the supply chain standardization constraint (ε≈0.28, primarily Rope + Piton), which solves genuine coordination problems but is increasingly theatrical, and (2) the market access barrier constraint (ε≈0.50, primarily Snare), which locks small producers out of mainstream distribution regardless of whether standardization is functionally necessary. The unified analysis treats the constraint at ε=0.38 (tangled rope) to capture both effects. Mandatrophy is resolved by showing that the constraint IS genuinely hybrid: both coordination function (supply chain efficiency) and extraction (consumer/producer surplus loss) are real structural features. No single perspective can claim 'the true type' — all six are legitimate readings from different structural positions. The Rope perspectives (chain, manufacturer) see the genuine coordination benefit. The Snare perspectives (consumers, producers) experience the genuine extraction. The Scaffold perspective (craft movement) sees the temporal bound. The Piton perspective (consumer habituation) sees the degraded functional necessity. The constraint maintains its Tangled Rope classification because beneficiaries + victims + active enforcement are all present in the structural data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_necessity_threshold,
    'What fraction of supply chain cost reduction is genuine efficiency necessity versus cost externalization/convenience for operators?',
    'Comparative analysis of supply chain costs with regional/diverse condiment sourcing vs standardized supply; cost accounting including customer satisfaction and churn rates across diverse vs standard menus',
    'If necessity > 70%: constraint is primarily Rope (coordination). If necessity < 40%: constraint is primarily Snare (extraction via convenience tax). This determines whether the constraint is reclassified as pure coordination or pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_necessity_threshold, empirical, 'Fraction of standardization cost reduction that is genuine necessity vs convenience').

omega_variable(
    consumer_preference_revelation,
    'What is the actual distribution of consumer condiment preferences when choices are not constrained by supply availability?',
    'Comparative study of condiment usage in regions with diverse offerings (markets, international communities) vs standardized-only markets; willingness-to-pay studies for diverse vs standard condiments',
    'If diversity preference is high (>60% seek alternatives): constraint extracts significant consumer surplus. If diversity preference is low (<30%): constraint reflects genuine consumer demand. This determines victim classification and χ computation for consumer perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_preference_revelation, empirical, 'Actual distribution of consumer condiment preferences under choice abundance').

omega_variable(
    supply_chain_technology_trajectory,
    'Are advances in logistics, automation, and micromanufacturing making diverse condiment sourcing economically feasible at scale?',
    'Technology trend analysis: cold chain efficiency, precision portion packaging, automated inventory management, 3D flavor printing, modular condiment dispensing. Comparison with adoption rates in tech-forward food service (QSR innovation labs, meal kit services)',
    'If technology trajectory enables feasibility: scaffold perspective is structural (sunset is real). If technology lags: scaffold is aspirational. This determines whether the constraint has genuine temporal decline or persists indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_technology_trajectory, empirical, 'Whether emerging logistics and automation make diverse sourcing economically viable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(condiment_tyranny, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cond_tr_t0, condiment_tyranny, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cond_tr_t25, condiment_tyranny, theater_ratio, 25, 0.5).
narrative_ontology:measurement(cond_tr_t50, condiment_tyranny, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(cond_be_t0, condiment_tyranny, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cond_be_t25, condiment_tyranny, base_extractiveness, 25, 0.34).
narrative_ontology:measurement(cond_be_t50, condiment_tyranny, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(condiment_tyranny, resource_allocation).
narrative_ontology:affects_constraint(condiment_tyranny, food_cultural_homogenization).
narrative_ontology:affects_constraint(condiment_tyranny, small_producer_market_access).
narrative_ontology:affects_constraint(condiment_tyranny, consumer_preference_suppression).

% DUAL FORMULATION NOTE:
% The condiment tyranny is upstream of food cultural homogenization (which it enables) and downstream of supply chain standardization economics (which it executes). The constraint represents the point where logistical efficiency becomes social/economic extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
