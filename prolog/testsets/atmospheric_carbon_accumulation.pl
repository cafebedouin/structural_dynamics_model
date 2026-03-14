% ============================================================================
% CONSTRAINT STORY: atmospheric_carbon_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_atmospheric_carbon_accumulation, []).

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
 *   constraint_id: atmospheric_carbon_accumulation
 *   human_readable: Atmospheric Carbon Accumulation and Climate Forcing
 *   domain: planetary_climate/biogeochemical_constraints
 *
 * SUMMARY:
 *   Atmospheric carbon accumulation represents a global constraint mechanism
 *   of historically unprecedented scale. The accumulation of anthropogenic
 *   CO2 and other greenhouse gases creates radiative forcing that drives
 *   climate state change. However, the constraint is not merely physical — it
 *   is fundamentally a mechanism of asymmetric extraction, where the costs of
 *   carbon-intensive development are externalized to vulnerable populations
 *   and future generations while benefits concentrate among high-consumption
 *   economies and fossil fuel extractors. The constraint exhibits all six DR
 *   types from different positions, making it a diagnostic exemplar for how
 *   indexical classification captures structural reality. From the
 *   perspective of fossil fuel industry and developed-economy middle classes,
 *   the constraint appears as a coordination problem (rope) or manageable
 *   risk (scaffold). From the perspective of island nations, subsistence
 *   farmers, and future generations, it appears as a pure extraction
 *   mechanism with no escape route (snare). The measured extractiveness
 *   (0.72) reflects the scale of asymmetric harm — the constraint directly
 *   extracts habitability, agricultural productivity, and livelihood security
 *   from vulnerable populations to sustain carbon-intensive consumption
 *   elsewhere. Theater ratio (0.58) reflects the mixed nature of climate
 *   governance: some genuine decarbonization infrastructure exists (renewable
 *   energy deployment, efficiency improvements), but much of the
 *   institutional response (carbon markets, net-zero pledges, offset
 *   programs) is performative or fraudulent. The constraint's evolution from
 *   minimal extractiveness (0.08 in 1850) to maximum (0.72 by 2024) tracks
 *   the accumulation of atmospheric carbon and the increasing visibility of
 *   climate impacts, creating a measurable drift toward snare classification
 *   as the institutional response has shifted from denial to managed
 *   adaptation rather than genuine mitigation.
 *
 * KEY AGENTS:
 *   - Climate-Vulnerable Populations: Primary victims (powerless/trapped) — island nations, subsistence farmers, low-latitude regions bearing climate impacts with no exit option
 *   - Future Generations: Ultimate victims (powerless/trapped) — cannot negotiate, organize, or consent; bearing full civilizational legacy of atmospheric carbon
 *   - Fossil Fuel Extractors: Primary beneficiaries (institutional/arbitrage) — capture economic rents from carbon-intensive development; manage externalities through regulatory arbitrage
 *   - High-Consumption Developed Economies: Secondary beneficiaries and moderate victims (moderate/constrained) — benefit from carbon-intensive infrastructure but also bear some climate costs; hybrid relationship to extraction
 *   - Renewable Energy and Climate Justice Coalition: Organized actors (organized/constrained) — see constraint as temporary institutional failure with achievable sunset through decarbonization
 *   - Carbon Markets and Offset Institutions: Institutional performers (institutional/arbitrage) — maintain ritual compliance while allowing continued emissions; piton classification reflects degraded function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional extraction as physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(atmospheric_carbon_accumulation, 0.72).
domain_priors:suppression_score(atmospheric_carbon_accumulation, 0.68).
domain_priors:theater_ratio(atmospheric_carbon_accumulation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(atmospheric_carbon_accumulation, extractiveness, 0.72).
narrative_ontology:constraint_metric(atmospheric_carbon_accumulation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(atmospheric_carbon_accumulation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(atmospheric_carbon_accumulation, snare).
narrative_ontology:human_readable(atmospheric_carbon_accumulation, "Atmospheric Carbon Accumulation and Climate Forcing").
narrative_ontology:topic_domain(atmospheric_carbon_accumulation, "planetary_climate/biogeochemical_constraints").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(atmospheric_carbon_accumulation, fossil_fuel_extractors).
narrative_ontology:constraint_beneficiary(atmospheric_carbon_accumulation, carbon_intensive_industry).
narrative_ontology:constraint_beneficiary(atmospheric_carbon_accumulation, high_consumption_populations).
narrative_ontology:constraint_victim(atmospheric_carbon_accumulation, climate_vulnerable_populations).
narrative_ontology:constraint_victim(atmospheric_carbon_accumulation, future_generations).
narrative_ontology:constraint_victim(atmospheric_carbon_accumulation, ecosystems).
narrative_ontology:constraint_victim(atmospheric_carbon_accumulation, agricultural_productivity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-VULNERABLE POPULATIONS (SNARE) — Island nations, subsistence farmers, coastal communities, and low-latitude populations face cascading climate impacts (sea level rise, drought, flooding, ecosystem collapse) with no credible exit option. Trapped by geography and economics. Extraction mechanism: their labor, resources, and habitability subsidize carbon-intensive development elsewhere. High suppression — no meaningful alternative to bearing costs. Effective extraction approaches maximum.
constraint_indexing:constraint_classification(atmospheric_carbon_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Cannot negotiate, organize, or exit. Bearing full cost of atmospheric legacy carbon and climate state shift. No ability to consent or resist. Maximum suppression and extraction — pure victimhood with zero degrees of freedom. The constraint's most extreme manifestation of asymmetric harm.
constraint_indexing:constraint_classification(atmospheric_carbon_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVELOPED-ECONOMY MIDDLE-INCOME POPULATIONS (TANGLED ROPE) — Moderate power, constrained exit. Beneficiaries of carbon-intensive development (housing, transportation, consumption) while bearing some costs (air pollution, climate impacts, resource scarcity). Exit options exist (relocate, consume differently) but carry high costs. Significant suppression through infrastructure lock-in and consumption-identity fusion. Neither pure extractors nor pure victims — hybrid relationship to the constraint.
constraint_indexing:constraint_classification(atmospheric_carbon_accumulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FOSSIL FUEL INDUSTRY (ROPE) — Institutional beneficiary with arbitrage options. Primary extractor and primary beneficiary. Experiences the constraint as coordination mechanism — managing carbon externalities, navigating regulatory arbitrage between jurisdictions, maintaining market position. From their perspective, the constraint (atmospheric carbon as externality) is a coordination problem they can solve through lobbying, standard-setting, and market positioning. High extractiveness but perceived as legitimate business coordination.
constraint_indexing:constraint_classification(atmospheric_carbon_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE JUSTICE AND DECARBONIZATION COALITION (SCAFFOLD) — Organized actors (climate scientists, justice movements, renewable industries, some governments) see the constraint as a temporary institutional failure with a clear sunset: renewable energy deployment, carbon pricing, and electrification are building alternative pathways that make fossil fuel extraction economically suboptimal. Suppression high (carbon lock-in infrastructure, political barriers) but coalition believes sunset is achievable within 20-50 years. Theater low relative to extraction — the coalition focuses on material infrastructure change, not performative carbon accounting.
constraint_indexing:constraint_classification(atmospheric_carbon_accumulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CARBON MARKETS AND OFFSET SYSTEMS (PITON) — Institutional arrangements (voluntary carbon markets, cap-and-trade, carbon credits) designed to solve the climate externality problem but largely performative. Theater ratio high (additionality questions, permanence issues, land-use credit fabrication reduce functional efficiency). Extraction mechanism persists through inertia — corporations and governments maintain offset programs despite low environmental integrity, because the ritual provides cover for continued emissions. The constraint persists because the alternative (genuine emissions reduction) carries higher political and economic cost.
constraint_indexing:constraint_classification(atmospheric_carbon_accumulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURAL LAW / THERMODYNAMIC VIEW (FALSE SUMMIT) — The atmospheric carbon accumulation could be misframed as a natural law: carbon emissions from combustion are inherent to industrial development; the radiative forcing of CO2 is a law of physics. From this view, climate change is an inevitable consequence of thermodynamic constraints, not a solvable coordination problem. However, the false summit detector reveals this as naturalization of a contingent institutional arrangement — the empirical ε (0.72) is too high, suppression (0.68) shows structure not inherent necessity, and the constraint exhibits all six types across perspectives rather than invariant mountain classification. The mountain reading naturalizes what is actually a contingent extraction mechanism.
constraint_indexing:constraint_classification(atmospheric_carbon_accumulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(atmospheric_carbon_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(atmospheric_carbon_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(atmospheric_carbon_accumulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(atmospheric_carbon_accumulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(atmospheric_carbon_accumulation, TR),
    TR >= 0.70.

:- end_tests(atmospheric_carbon_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72): Very high. The constraint extracts habitability, resources, and livelihood security from vulnerable populations to sustain carbon-intensive consumption in developed economies. The extraction mechanism is asymmetric: high-consumption populations and fossil fuel industries capture benefits (energy, mobility, consumption) while costs (climate impacts, resource scarcity, ecosystem collapse) concentrate on those with minimal responsibility for emissions. The measurement trajectory (0.08 → 0.72 over 174 years) reflects acceleration of both emissions and visible climate damages. Suppression (0.68): Very high. Multiple reinforcing barriers prevent exit or resistance: infrastructure lock-in (energy systems, transportation, agriculture designed for carbon-intensive inputs), consumption-identity fusion (high-consumption lifestyle experienced as normal/necessary), political capture of climate policy (fossil fuel industry influence), and geographic/economic trapping (vulnerable populations cannot relocate). However, suppression is not total — renewable energy deployment, policy shifts, and climate movements are creating alternative pathways, justifying the scaffold perspective. Theater ratio (0.58): Moderate-high. Carbon markets, net-zero pledges, and many climate policies are performative or minimally effective (additionality fraud, scope 3 exclusions, net-zero targets without near-term emissions reduction). However, genuine decarbonization infrastructure is being deployed (renewable capacity, building efficiency, grid electrification), reducing theater ratio below pure piton threshold. The measurement trajectory (0.15 → 0.58) reflects increasing institutional engagement (both genuine and performative) over the interval. Claimed type: Snare. The constraint exhibits the core snare signature: high extractiveness (0.72), high suppression (0.68), minimal coordination function (this is not a coordination mechanism solving a collective action problem — it is an extraction mechanism), and victims with zero exit options. The constraint persists despite catastrophic long-term harm because the extraction mechanism is embedded in the normal operation of economic systems and reinforced by political capture.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across all seven perspectives. The fossil fuel industry and developed-economy middle classes experience the constraint as coordination (rope) — managing externalities, optimizing energy systems. The renewable energy coalition sees the constraint as temporary with a sunset (scaffold) — decarbonization will eventually make fossil fuels uneconomical. The carbon markets see their own ritual as degraded but persistent (piton) — offset programs provide compliance theater while fossil fuels continue. The moderate-income populations experience the constraint as mixed coordination and extraction (tangled_rope) — benefiting from development while bearing climate costs. The climate-vulnerable and future generations experience the constraint as pure extraction (snare) — bearing all costs with no benefits and no exit options. The civilizational analytical observer risks seeing the constraint as a natural law (false summit) — CO2 radiative forcing is physics, climate change is inevitable. However, the false summit detector reveals this naturalization as illegitimate: the extractiveness is too high, suppression shows institutional structure not inherent necessity, and the perspectival multiplicity demonstrates that the constraint is contingent on institutional arrangements, not inevitable from physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by structural position: agent power level, exit options, and relationship to the extraction flow. Powerless/trapped agents (climate-vulnerable, future generations) experience maximum d and maximum f(d) — they are full targets of extraction with zero escape options. Institutional/arbitrage agents (fossil fuel industry) experience minimal d — they are primary beneficiaries with exit capacity (can shift investments to renewables if policy changes). Moderate/constrained agents (developed-economy middle classes) experience moderate d — constrained exit (switching to low-carbon living carries significant costs) and mixed beneficiary/victim status (benefit from development, bear some climate impacts). Organized/constrained agents (climate justice coalition) experience moderate d — they have agency and see exit pathways but face institutional suppression. The piton perspective experiences low d derived from arbitrage exit — the carbon markets benefit from maintaining the constraint but could reposition if political winds shifted. The mountain perspective experiences analytical d (0.73) — the analytical observer has the most distance from the extraction flow but risks naturalizing institutional structures as laws of nature.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE DIAGNOSIS VALIDATED: Atmospheric carbon accumulation classifies as Snare from the powerless perspective (the view that bears maximum extraction) and shows snare signatures across most perspectives except those positioned as primary beneficiaries or organized actors with exit pathways. The mandatrophy is resolved by recognizing that the snare classification is the structural reality — the constraint exists because of asymmetric extraction, not because of coordination necessity. The coordination framing ('we all benefit from carbon-intensive development') is the cover story. The snare mechanism is the reality: vulnerable populations bear costs with no exit options while consumption elsewhere depends on externalizing those costs. The false summit detector flags the mountain perspective (naturalizing CO2 radiative forcing as law of nature) as illegitimate — the constraint is contingent on institutional arrangements (fossil fuel subsidies, regulatory arbitrage, consumption norms) that could be rapidly changed. The scaffold perspective (sunset through decarbonization) is structurally sound but faces snare dynamics that resist transition: those benefiting from the current arrangement use political and economic power to slow decarbonization, deepening the extraction for vulnerable populations during the transition period. The mandatrophy analysis confirms that all six types are perspectivally legitimate, but the constraint's primary structural function is extraction (snare), not coordination (rope) or temporary support (scaffold).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_budget_temporal_binding,
    'Is the remaining carbon budget a hard physical limit or a contingent policy target derived from climate sensitivity uncertainty?',
    'Climate model ensemble studies varying radiative forcing sensitivity; empirical paleoclimate data on climate state transitions; assessment of reversibility and tipping-point thresholds',
    'If hard limit: atmospheric accumulation becomes partially mountain-like (irreversible dynamics). If contingent: remains squarely in snare/tangled_rope territory — the constraint is institutional lock-in, not physics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_budget_temporal_binding, empirical, 'Whether carbon budget is physical limit or policy target').

omega_variable(
    mitigation_cost_asymmetry,
    'Who bears the cost of decarbonization: the extractors (fossil industry, high-consumption populations) or the vulnerable populations through adaptation burden?',
    'Comparative cost accounting: mitigation capex vs adaptation spending across income quintiles and regions; longitudinal tracking of climate finance flows vs actual climate damage costs',
    'If extractors bear cost: constraint transitions toward rope/scaffold at developed-economy power levels. If vulnerable populations bear cost: snare deepens and extraction mechanism strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_cost_asymmetry, empirical, 'Distributional burden of decarbonization costs').

omega_variable(
    renewable_substitution_completeness,
    'Can renewable energy and electrification pathways fully replace fossil fuels across all sectors (aviation, shipping, high-temperature heat, fertilizer production) within the carbon budget?',
    'Technology roadmap analysis; thermodynamic feasibility studies for hard-to-decarbonize sectors; empirical deployment rates vs required acceleration rates',
    'If substitution incomplete: atmospheric accumulation becomes partially mountain-like (irreducible emissions). If complete: scaffold perspective confirmed — sunset is structurally achievable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_substitution_completeness, empirical, 'Whether renewable energy can replace all fossil fuel functions').

omega_variable(
    political_economy_of_fossil_lock_in,
    'Are suppression mechanisms (0.68) structural (infrastructure, capital stocks) or primarily institutional (subsidy capture, regulatory capture, geopolitical power)?',
    'Analysis of capital turnover rates in energy infrastructure; comparative governance studies of rapid energy transitions; assessment of fossil fuel industry political influence vs objective economic competitiveness',
    'If structural: suppression persists decades regardless of policy. If institutional: suppression can be rapidly reversed through political realignment and regulatory change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_economy_of_fossil_lock_in, empirical, 'Whether fossil lock-in is structural or institutional').

omega_variable(
    climate_tipping_point_irreversibility,
    'At what CO2 concentration level do climate tipping points (Amazon dieback, permafrost methane, Atlantic circulation collapse) become irreversible on multi-generational timescales?',
    'Paleoclimate proxy analysis; climate model ensemble projections with high-resolution coupling; empirical observation of tipping-point precursor signals (bipolar seesaw, regime shifts)',
    'If tipping points occur within remaining carbon budget and are irreversible: constraint becomes partially mountain-like with respect to future generations (immutable legacy damage). If thresholds are distant or reversible: snare mechanism could be interrupted by midcentury action.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_tipping_point_irreversibility, empirical, 'Irreversibility threshold for climate system tipping points').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(atmospheric_carbon_accumulation, 1850, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(atm_carbon_theater_1990, atmospheric_carbon_accumulation, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(atm_carbon_theater_2005, atmospheric_carbon_accumulation, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(atm_carbon_theater_2015, atmospheric_carbon_accumulation, theater_ratio, 2015, 0.5).
narrative_ontology:measurement(atm_carbon_theater_2024, atmospheric_carbon_accumulation, theater_ratio, 2024, 0.58).

% Extraction over time
narrative_ontology:measurement(atm_carbon_extractiveness_1850, atmospheric_carbon_accumulation, base_extractiveness, 1850, 0.08).
narrative_ontology:measurement(atm_carbon_extractiveness_1950, atmospheric_carbon_accumulation, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement(atm_carbon_extractiveness_2000, atmospheric_carbon_accumulation, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(atm_carbon_extractiveness_2024, atmospheric_carbon_accumulation, base_extractiveness, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(atmospheric_carbon_accumulation, resource_allocation).
narrative_ontology:affects_constraint(atmospheric_carbon_accumulation, fossil_fuel_subsidy_lock_in).
narrative_ontology:affects_constraint(atmospheric_carbon_accumulation, climate_refugee_vulnerability).
narrative_ontology:affects_constraint(atmospheric_carbon_accumulation, agricultural_collapse_risk).
narrative_ontology:affects_constraint(atmospheric_carbon_accumulation, coastal_ecosystem_loss).

% DUAL FORMULATION NOTE:
% Atmospheric carbon accumulation is the upstream constraint affecting multiple downstream constraints in climate, migration, agriculture, and ecosystem domains. Decomposition into separate constraint stories is justified when analyzing specific sectoral impacts (e.g., coastal sea-level rise as distinct from aggregate climate forcing, or agricultural drought as distinct from overall radiative forcing). Each decomposed story would have distinct ε values reflecting different measurement observables: the physical radiative forcing (high ε), the geopolitical migration crisis (very high ε), the agricultural productivity loss (very high ε). This story treats the fundamental extraction mechanism (unequal distribution of climate impacts and development benefits) as the unified constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(atmospheric_carbon_accumulation, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
