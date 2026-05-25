% ============================================================================
% CONSTRAINT STORY: processed_food_sodium_targeting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_processed_food_sodium_targeting, []).

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
 *   constraint_id: processed_food_sodium_targeting
 *   human_readable: Processed Food Sodium Targeting Coordination and Extraction
 *   domain: public_health/food_industry/regulation
 *
 * SUMMARY:
 *   Processed food sodium targeting exemplifies how a purported public health
 *   coordination mechanism operates as embedded extraction disguised by
 *   voluntary industry compliance frameworks. The constraint mixes genuine
 *   coordination (cheap processed foods enable food security for low-income
 *   populations; sodium functions as preservative and flavor enhancer
 *   enabling efficient supply chains) with asymmetric extraction
 *   (sodium-related hypertension, stroke, and kidney disease concentrate
 *   health costs on low-income and hypertensive populations who cannot afford
 *   alternatives). The extractiveness has increased over the interval (0.35 →
 *   0.58) as disease burden accumulates while voluntary sodium reduction
 *   targets remain performative (theater_ratio rising from 0.45 → 0.65). The
 *   constraint presents as six distinct types depending on perspective: a
 *   snare from the powerless consumer's view, tangled rope from public health
 *   and hypertensive populations, rope from manufacturers and retailers, a
 *   degraded piton for voluntary targets, a solvable scaffold with mandatory
 *   mandates, and a false natural law from biochemical determinism.
 *
 * KEY AGENTS:
 *   - Low-income consumers: Primary victim (powerless/trapped) — economically locked into cheap processed foods; bears health extraction with no exit
 *   - Hypertensive populations: Secondary victim (moderate/constrained) — constrained by cost and availability; experience mixed coordination (access) and extraction (health risk)
 *   - Processed food manufacturers: Primary beneficiary (institutional/arbitrage) — extract margin and cost advantages from high-sodium formulations; arbitrage regulatory targets through minimal compliance
 *   - Retail distribution networks: Beneficiary (institutional/arbitrage) — leverage sodium for supply chain efficiency; benefit from long shelf-life and reduced spoilage
 *   - Public health systems: Victim and coordinator (powerful/constrained) — bear disease burden but also depend on cheap food system for mass nutrition; generational perspective
 *   - Voluntary targets system: Degraded mechanism (institutional/arbitrage) — performative compliance with minimal enforcement; piton classification reveals institutional inertia
 *   - Mandatory reformulation mandates: Reform pathway (organized/constrained) — scaffold perspective showing sunset potential through policy change
 *   - Analytical observer: Risks naturalization (analytical/analytical) — may mistake evolved salt preference for biological constraint, masking social and economic construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(processed_food_sodium_targeting, 0.58).
domain_priors:suppression_score(processed_food_sodium_targeting, 0.68).
domain_priors:theater_ratio(processed_food_sodium_targeting, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(processed_food_sodium_targeting, extractiveness, 0.58).
narrative_ontology:constraint_metric(processed_food_sodium_targeting, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(processed_food_sodium_targeting, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(processed_food_sodium_targeting, tangled_rope).
narrative_ontology:human_readable(processed_food_sodium_targeting, "Processed Food Sodium Targeting Coordination and Extraction").
narrative_ontology:topic_domain(processed_food_sodium_targeting, "public_health/food_industry/regulation").

domain_priors:requires_active_enforcement(processed_food_sodium_targeting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(processed_food_sodium_targeting, processed_food_manufacturers).
narrative_ontology:constraint_beneficiary(processed_food_sodium_targeting, retail_distribution_networks).
narrative_ontology:constraint_victim(processed_food_sodium_targeting, low_income_consumers).
narrative_ontology:constraint_victim(processed_food_sodium_targeting, hypertensive_populations).
narrative_ontology:constraint_victim(processed_food_sodium_targeting, public_health_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME CONSUMER (SNARE) — Structurally trapped by economic dependency on cheap processed foods; limited retail access in food deserts; cannot exit without bearing significant cost (higher food prices, relocation, time investment). Maximum extraction and suppression. Consumer experiences the constraint as insurmountable — sodium intake is an unavoidable feature of affordable food access, not a choice.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HYPERTENSIVE POPULATION (TANGLED ROPE) — Bears extraction through elevated health risks and restricted food choice, but also benefits from the low-cost food system and the coordination benefit of standardized, shelf-stable products. Exit is costly but possible (dietary modification, medication, higher food expenses). Mixed extraction and coordination: the constraint both enables and constrains their health management.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROCESSED FOOD MANUFACTURERS (ROPE) — Experience the sodium constraint as coordination: sodium functions as preservative, flavor enhancer, and cost-reduction lever. High-sodium formulations minimize production costs and extend shelf life. Manufacturers see compliance with voluntary sodium reduction targets as a coordination problem they can arbitrage — marginal reformulation for regulatory compliance while maintaining affordability and palatability. Net beneficiary position.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RETAIL DISTRIBUTION (ROPE) — Benefits from high-sodium, shelf-stable, low-spoilage products. The sodium constraint enables efficient supply chain coordination: reduced refrigeration requirements, longer shelf life, predictable product performance. Retail networks view sodium as a coordination feature, not an extractive mechanism. Arbitrage position with immediate exit options.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH SYSTEMS (TANGLED ROPE) — Bear extraction through disease burden (hypertension, stroke, kidney disease), healthcare costs, and population mortality. But also benefit from the cheap food system's coordination: processed foods enable mass food security and prevent malnutrition in low-income populations. The constraint mixes genuine coordination (food access for vulnerable populations) with extraction (health costs concentrated on those populations). Constrained by regulatory jurisdictions and industry political power; generational time horizon because sodium reduction requires sustained dietary and reformulation changes.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: VOLUNTARY TARGETS SYSTEM (PITON) — The voluntary sodium reduction framework (industry self-regulation, non-binding guidelines) is substantially theatrical. Manufacturers announce compliance while reformulating minimally; surveillance mechanisms are weak; enforcement is absent. The targets persist through institutional inertia and regulatory theater rather than functional effectiveness. Theater ratio high because the public health narrative (industry is reducing sodium) diverges from actual reformulation pace. The piton perspective recognizes that voluntary targets are performative while alternative mandatory mechanisms have not replaced them.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: REFORMULATION MANDATES (SCAFFOLD) — Some jurisdictions (Argentina, Chile, Canada) have implemented mandatory sodium targets with labeling requirements and enforcement. This perspective sees the constraint as solvable through regulatory architecture with a sunset: as low-sodium formulations become cost-competitive and consumer demand shifts, the regulatory constraint can relax. Represents organized actors (regulators, advocacy groups) with agency and exit paths through policy reform. Theater is lower because mandatory targets have measurable compliance metrics.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a biochemical/evolutionary perspective, human taste preferences for salt are hardwired (electrolyte homeostasis, survival in ancestral environments with salt scarcity). The constraint appears as an immutable feature of human palatability: food systems must use salt to be acceptable; reducing sodium reduces palatability and consumption; reducing consumption creates malnutrition risk in calorie-restricted populations. This perspective risks naturalizing what is a contingent industrial and economic arrangement (ultra-processed foods + poverty + food deserts) as a biological law.
constraint_indexing:constraint_classification(processed_food_sodium_targeting, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(processed_food_sodium_targeting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(processed_food_sodium_targeting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(processed_food_sodium_targeting, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(processed_food_sodium_targeting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(processed_food_sodium_targeting, TR),
    TR >= 0.70.

:- end_tests(processed_food_sodium_targeting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts health burden (hypertension, cardiovascular disease, kidney disease) disproportionately from low-income and vulnerable populations. The extraction is embedded in the cost structure of the food system: low-sodium formulations require R&D, higher-cost ingredients (herbs, spices, flavor compounds), or accepted palatability loss. Manufacturers pass cost to consumers or absorb margin reduction, but low-income consumers cannot afford premium low-sodium alternatives. The interval trajectory shows increasing extractiveness (0.35 → 0.58) because disease burden accumulates while reformulation remains slow. Suppression (0.68): High. Multiple barriers prevent exit: food-desert geographies limit retail options; poverty constrains purchasing power for higher-cost foods; food company marketing targets price-sensitive consumers; regulatory frameworks remain voluntary and weak in most jurisdictions; knowledge barriers (nutritional literacy) vary by income. The suppression is not absolute (exit is possible through diet modification or relocation) but sufficiently high that most trapped agents don't exercise it. Theater ratio (0.65): Moderate-high. Voluntary sodium reduction targets create theatrical compliance: industry announces reduction targets, implements marginal reformulation, maintains affordability through other ingredients (sugar, additives), and communicates progress while actual sodium intake remains elevated. The theater has increased over the interval as gap between announced targets and actual consumption widens. Mandatory regimes (Canada, Chile, Argentina) show lower theater because they have measurable enforcement and labeling requirements.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates dramatic perspectival divergence. Manufacturers see coordination (Rope): sodium is a functional input enabling supply chain efficiency, cost reduction, and shelf stability. They view voluntary targets as a coordination mechanism they can satisfy through marginal compliance. Public health sees extraction (Snare/Tangled Rope): the same sodium properties that enable manufacturers to profit create disease burden that concentrates on powerless populations. Voluntary targets are theater. Low-income consumers see a trap (Snare): the constraint is experienced as structural inevitability because exiting requires both higher income (to purchase low-sodium alternatives) and geographic mobility (to escape food deserts). Regulators with mandatory approaches see a solvable problem (Scaffold): reformulation targets with enforcement and labeling can decouple affordability from sodium without requiring consumer behavior change. The piton perspective reveals that the voluntary system has become inert ritual: announced without enforcement, complied minimally, perpetuated through institutional continuity rather than functional success.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position: beneficiary or victim status, power level, and exit options. Low-income trapped consumers: d ≈ 0.95 (full target, no exit) → high f(d) ≈ 1.42 → high experienced extraction. Manufacturers and retailers: d ≈ 0.05 (beneficiary with arbitrage) → low f(d) ≈ -0.12 → negative experienced extraction (they subsidize the system). Public health systems: d ≈ 0.60 (victim but powerful and constrained) → f(d) ≈ 0.85 → moderate extraction. The scope modifier σ(S) amplifies extractiveness at national/global scope: σ(national) = 1.0, so χ = 0.58 × f(d) × 1.0. The constraint's extractiveness is concentrated at local and regional scales (food deserts are hyper-local) but the coordination function (cheap food supply) is national/global, creating a geographic mismatch: global beneficiaries, local victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through recognition that coordination and extraction coexist structurally, not perspectivally. The coordination function is real: processed foods with high sodium do provide affordable nutrition, cost-effective preservation, and supply chain reliability. But this coordination is asymmetric — benefits flow to manufacturers and retailers; costs flow to consumers and health systems. The tangled rope classification captures this: the constraint both enables coordination (food access for poor populations) and extracts (health burden concentrated on those populations). The false natural law (mountain) perspective — 'salt preference is biological, reformulation is impossible' — represents a naturalization of industrial and economic structures (food desert economics, agricultural commodity subsidies, marketing intensity) as immutable. The mandatrophy is resolved by maintaining the tangled rope classification for moderate power levels and public health while recognizing that the snare perspective from powerless consumers is the control case: the constraint is extraction when exit is impossible, regardless of coordination benefits elsewhere.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_mandatory_effectiveness,
    'Do voluntary industry targets achieve meaningful sodium reduction compared to mandatory regulatory approaches?',
    'Comparative analysis: countries with voluntary targets (US) vs mandatory targets (Canada, Chile) tracking reformulation pace, health outcomes, and actual sodium intake over 10-15 years',
    'If voluntary achieves comparable results: constraint is coordination problem (Rope). If mandatory required: constraint is extraction mechanism requiring enforcement (Tangled Rope or Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_vs_mandatory_effectiveness, empirical, 'Effectiveness comparison: voluntary vs mandatory sodium reduction').

omega_variable(
    reformulation_cost_allocation,
    'Who bears the real cost of low-sodium reformulation: manufacturers through R&D and margin reduction, or consumers through higher food prices?',
    'Price tracking analysis for reformulated vs non-reformulated products; margin data from manufacturer financial statements; consumer price indices in low-sodium vs high-sodium product categories',
    'If manufacturers bear cost: extraction flows away from consumers, snare perspective weakens. If consumers bear cost: extraction flows toward low-income populations, snare and tangled rope perspectives strengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformulation_cost_allocation, empirical, 'Cost allocation for reformulation: manufacturers vs consumers').

omega_variable(
    taste_adaptation_mechanism,
    'Can human taste preferences adapt to lower-sodium foods within a generation, or is the salt preference truly fixed?',
    'Longitudinal studies of taste preference shifts in populations transitioning to low-sodium diets; cross-cultural comparison of sodium intake and taste acceptability; pediatric taste development studies in low-sodium vs high-sodium environments',
    'If adaptation occurs: mountain perspective is false naturalization; constraint is socially constructed. If preference is fixed: barrier to low-sodium transition is real and deep.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taste_adaptation_mechanism, empirical, 'Human taste adaptation to lower-sodium foods').

omega_variable(
    food_access_trade_off,
    'Does mandatory sodium reduction for cheap processed foods reduce availability and affordability for low-income populations, forcing a choice between sodium-reduced but expensive products and sodium-high affordable products?',
    'Market analysis post-mandate: price elasticity, product availability, consumer purchasing patterns in low-income vs high-income markets; health outcome data (sodium intake vs malnutrition vs hypertension rates)',
    'If trade-off is severe: mandatory approach harms the most vulnerable; scaffold perspective requires sunset clauses and income support. If trade-off is minimal: mandatory approach successfully decouples health from poverty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(food_access_trade_off, empirical, 'Food affordability and access trade-off from sodium reduction').

omega_variable(
    industry_lobbying_suppression,
    'How much of the slow voluntary compliance reflects genuine technical/palatability barriers vs political suppression from industry advocacy?',
    'Comparative regulatory velocity: sodium targets in countries with strong vs weak industry lobbying influence; analysis of industry campaign spending vs regulatory tightening; technical feasibility assessments from independent food scientists',
    'If genuine barriers dominate: snare classification weakens; problem is technical/nutritional. If suppression dominates: snare and tangled rope classifications strengthen; extraction is deliberate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_lobbying_suppression, empirical, 'Suppression role: technical barriers vs industry lobbying').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(processed_food_sodium_targeting, 0, 21).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pfst_tr_t0, processed_food_sodium_targeting, theater_ratio, 0, 0.45).
narrative_ontology:measurement(pfst_tr_t7, processed_food_sodium_targeting, theater_ratio, 7, 0.58).
narrative_ontology:measurement(pfst_tr_t14, processed_food_sodium_targeting, theater_ratio, 14, 0.65).
narrative_ontology:measurement(pfst_tr_t21, processed_food_sodium_targeting, theater_ratio, 21, 0.7).

% Extraction over time
narrative_ontology:measurement(pfst_be_t0, processed_food_sodium_targeting, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pfst_be_t7, processed_food_sodium_targeting, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(pfst_be_t14, processed_food_sodium_targeting, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(pfst_be_t21, processed_food_sodium_targeting, base_extractiveness, 21, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(processed_food_sodium_targeting, resource_allocation).
narrative_ontology:boltzmann_floor_override(processed_food_sodium_targeting, 0.12).
narrative_ontology:affects_constraint(processed_food_sodium_targeting, food_desert_geographic_isolation).
narrative_ontology:affects_constraint(processed_food_sodium_targeting, agricultural_commodity_subsidies).
narrative_ontology:affects_constraint(processed_food_sodium_targeting, processed_food_marketing_targeting).

% DUAL FORMULATION NOTE:
% Processed food sodium targeting is downstream of agricultural subsidy structures and upstream of food access geography. The sodium constraint itself has ε ≈ 0.58 (tangled rope); but sodium is a lever for extracting value that flows from upstream agricultural policy (subsidies that make commodity inputs cheap) and enables downstream geographic extraction (food deserts where cheap high-sodium foods dominate). Decomposition: the constraint family includes upstream subsidy structures (affecting input costs) and downstream geography constraints (affecting exit options). Each has different ε; all three are linked via manufacturing cost structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(processed_food_sodium_targeting, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
