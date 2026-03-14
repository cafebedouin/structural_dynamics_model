% ============================================================================
% CONSTRAINT STORY: geopolitical_mineral_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geopolitical_mineral_dependency, []).

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
 *   constraint_id: geopolitical_mineral_dependency
 *   human_readable: Geopolitical Mineral Dependency Constraint
 *   domain: geopolitical_economic
 *
 * SUMMARY:
 *   Geopolitical mineral dependency creates a structural constraint operating
 *   at the intersection of physical resource scarcity, technological demand,
 *   and state power. The constraint exhibits asymmetric extraction: states
 *   and actors controlling supply chains benefit from artificial scarcity
 *   (whether naturally occurring or politically maintained), while
 *   import-dependent states bear vulnerability to supply disruption, price
 *   volatility, and coercive leverage. The constraint is not purely
 *   extractive (mineral supply chains do enable genuine coordination in
 *   manufacturing and energy systems) nor purely coordinating (beneficiaries
 *   use their structural position to extract rents and geopolitical
 *   leverage). The constraint's architecture involves multiple institutional
 *   actors: mineral-producing states (primary beneficiaries), integrated
 *   multinational supply chain firms (secondary beneficiaries),
 *   import-dependent manufacturing states (victims), and energy transition
 *   coalitions (organizing to build exit mechanisms). The measurement
 *   trajectory shows extractiveness increasing from 0.42 to 0.58 over 22
 *   years, reflecting intensifying demand for battery minerals and rare-earth
 *   elements in clean energy and electronics. Theater ratio remained
 *   relatively low (0.35-0.52) because actual supply constraints and material
 *   dependencies are real, not performative — the constraint operates through
 *   genuine scarcity rather than narrative.
 *
 * KEY AGENTS:
 *   - Mineral-Producing States: Primary beneficiary (institutional/arbitrage) — benefit from export revenue, geopolitical leverage, and rents from supply scarcity
 *   - Integrated Supply Chain Actors: Secondary beneficiary (institutional/constrained) — multinational firms controlling processing and downstream manufacturing capture geographic rents and reduced competition
 *   - Import-Dependent Manufacturing States: Primary victim (powerless/trapped) — manufacturing base depends on minerals with no meaningful domestic alternative; vulnerable to disruption
 *   - Downstream Industrial Sectors: Secondary victim (moderate/constrained) — electronics, automotive, renewable energy sectors face rising input costs and supply uncertainty
 *   - Energy Transition Coalition: Organized agent (organized/mobile) — states, firms, and coalitions pursuing renewable energy are building substitution pathways, recycling infrastructure, and supply diversification
 *   - Cold War Strategic Minerals Institutions: Degraded actor (institutional/arbitrage) — government stockpiles and supply security rhetoric persist but with declining functional leverage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geopolitical_mineral_dependency, 0.58).
domain_priors:suppression_score(geopolitical_mineral_dependency, 0.65).
domain_priors:theater_ratio(geopolitical_mineral_dependency, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geopolitical_mineral_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(geopolitical_mineral_dependency, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(geopolitical_mineral_dependency, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geopolitical_mineral_dependency, tangled_rope).
narrative_ontology:human_readable(geopolitical_mineral_dependency, "Geopolitical Mineral Dependency Constraint").
narrative_ontology:topic_domain(geopolitical_mineral_dependency, "geopolitical_economic").

domain_priors:requires_active_enforcement(geopolitical_mineral_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geopolitical_mineral_dependency, mineral_producing_states).
narrative_ontology:constraint_beneficiary(geopolitical_mineral_dependency, integrated_supply_chain_actors).
narrative_ontology:constraint_victim(geopolitical_mineral_dependency, mineral_importing_states).
narrative_ontology:constraint_victim(geopolitical_mineral_dependency, downstream_industrial_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPORT-DEPENDENT MANUFACTURING SECTOR (SNARE) — Nations dependent on imported critical minerals for domestic manufacturing have no exit option within biographical timescales. Supply chain disruption causes immediate economic damage. Suppression is total: no stockpile lasts indefinitely, substitution is technologically limited, and alternative suppliers are similarly constrained. This agent experiences maximum extraction during geopolitical crises.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STRATEGIC IMPORTER NATION (TANGLED ROPE) — States with moderate power and constrained exit options experience both genuine coordination (mineral supply chains do enable advanced manufacturing and energy transition) and asymmetric extraction (dependency creates vulnerability to sanctions, price volatility, and geopolitical coercion). At generational timescale, alternatives emerge: domestic mining revival, substitution research, diversification of suppliers. Extraction is real but not total.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MINERAL PRODUCING STATE (ROPE) — Primary beneficiary with arbitrage exit options (can redirect exports, integrate downstream, or negotiate with competing buyers). Experiences the constraint as beneficial coordination: mineral exports create foreign exchange, enable infrastructure investment, and provide leverage in international negotiations. This perspective sees the constraint as enabling their agency rather than constraining it.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTEGRATED SUPPLY CHAIN ACTOR (TANGLED ROPE) — Multinational firms that control processing, refinement, and downstream manufacturing benefit from supply constraints (reduced competition, geographic rents) while also coordinating actual supply chain logistics. These actors have constrained exit (reversing vertically-integrated operations requires decades) but also have arbitrage capacity (can shift operations between jurisdictions). Effective extraction is moderate — they coordinate real functions while capturing rents.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ENERGY TRANSITION COALITION (SCAFFOLD) — Organized states and private actors pursuing renewable energy and electrification are building alternative supply chains and substitution pathways. Lithium mining expansion, recycling infrastructure, and rare-earth substitution research create exit mechanisms within generational timescales. This constraint is experienced as temporary: the coalition sees a sunset where mineral dependency declines as circular economy practices mature. Theater is low because the coalition is driving real substitution, not performative compliance.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR RESOURCE CONTROL NARRATIVE (PITON) — Post-Cold War institutions (strategic mineral stockpiles, trade agreements, supply security rhetoric) persist largely as theater. The actual geopolitical leverage of minerals has shifted: rare-earth substitution is accelerating, electric vehicle mineral demand is volatile, and recycling reduces primary supply dependency. The institutional narrative of 'critical minerals as strategic weapons' maintains its form through inertia despite functional degradation. Theater ratio 0.48 reflects that much strategic mineral policy is now performative — countries maintain stockpiles and security rhetoric even as the actual leverage declines.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Civilizational analysis reveals the constraint as a genuine hybrid: mineral supply chains do enable advanced manufacturing and energy transition (coordination function), AND geopolitical control over mineral supplies enables asymmetric extraction (states use export restrictions as coercion, supply chain actors capture rents). The classification is not falsely naturalized as mountain — this is a contingent institutional arrangement with both real coordination and real extraction mixed together.
constraint_indexing:constraint_classification(geopolitical_mineral_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geopolitical_mineral_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geopolitical_mineral_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geopolitical_mineral_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geopolitical_mineral_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geopolitical_mineral_dependency, TR),
    TR >= 0.70.

:- end_tests(geopolitical_mineral_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting real supply-side constraints combined with beneficiary rent capture. The constraint is not at maximum extraction (0.70+) because substitution pathways exist and supply diversification is occurring. The measurement trajectory shows a rise from 0.42 to 0.58, indicating intensifying extraction pressure driven by energy transition mineral demand (lithium, cobalt, rare earths for batteries) without matching supply growth. At time=0 (early 2000s), mineral dependency was less acute; by time=15 (mid-2010s), clean energy demand and supply concentration in China created sharp extraction increases; by time=22 (early 2020s), the constraint stabilized as recycling and substitution began scaling. Suppression (0.65): High. Barriers to exit include long lead times for mining development (10-15 years), geological concentration of deposits, capital intensity of processing, and geopolitical control of key chokepoints. Import-dependent states cannot substitute quickly within biographical timescales. Theater ratio (0.48): Moderate-low. The constraint operates through real material scarcity, not narrative — mineral supply chains are functionally critical for manufacturing and energy systems. However, some theater exists: strategic mineral stockpiles are maintained partly for actual security and partly for political signaling; trade policy rhetoric exaggerates leverage (export controls are expensive for producers and often fail); Cold War resource control narratives persist despite declining functional relevance. The theater is increasing slightly (0.35→0.48) as policy responses outpace actual supply constraints.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across institutional actors. The mineral-producing state sees coordination and beneficial exchange (Rope) — mineral exports enable development and geopolitical agency. The integrated supply chain firm sees profitable coordination (Tangled Rope) — genuine supply chain functions with captured rents. The import-dependent manufacturing state sees extraction with no exit (Snare) — supply vulnerability creates coercive leverage without offsetting benefits. The energy transition coalition sees a temporary problem with solutions (Scaffold) — substitution and recycling pathways are building exits. The Cold War resource control narrative sees persistent strategic imperative (Piton) — institutional rhetoric of 'critical minerals as weapons' persists despite declining leverage. The analytical observer at civilizational scope sees a durable hybrid (Tangled Rope) — real coordination functions mixed with real extraction mechanisms that will persist even as specific supply constraints resolve. No perspective sees a false summit (Mountain); the constraint is genuinely contingent and structurally mixed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d values) for each institutional actor reflects their structural position relative to the extraction flow. Mineral-producing states are beneficiaries with arbitrage exit options (can redirect exports, negotiate with competing buyers, or integrate downstream) — low d (≈0.15), resulting in low/negative effective extraction. Import-dependent manufacturing states are victims with trapped exit options (cannot substitute minerals quickly; no alternative supply chains) — high d (≈0.85), resulting in high effective extraction. Integrated supply chain firms are secondary beneficiaries with constrained exit (reversing vertical integration is expensive) — moderate d (≈0.35), resulting in moderate effective extraction capturing rents without pure extraction. The energy transition coalition is organized with mobile exit options (substitution research, recycling, alternative materials) — lower d (≈0.40), resulting in moderate extraction pressure but with declining impact over time as alternatives mature. The analytical observer at civilizational scope applies no beneficiary/victim asymmetry — neutral d (≈0.50), producing a balanced tangled rope classification that reflects genuine mixed coordination and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by decomposing geopolitical mineral dependency into its actual structural components: (1) Real coordination function — mineral supply chains do enable manufacturing, electronics, renewable energy, and modern industrial systems. (2) Asymmetric extraction — beneficiaries use control over supply to capture rents and geopolitical leverage. (3) Temporal structure — substitution and recycling are building exit mechanisms within generational timescales, making the constraint a Scaffold from the coalition perspective rather than permanent Snare from the importer perspective. The classification is Tangled Rope at the civilizational/analytical level because both functions are structurally durable: even as specific mineral constraints resolve (battery recycling matures, substitutes are found), the general pattern of geopolitical leverage over critical resources will persist — resources will shift from rare earths and cobalt to other materials, but the asymmetry of control over concentration will remain. The constraint does not collapse to pure Rope (mere coordination) because beneficiaries will continue to exercise extraction leverage. It does not collapse to pure Snare because the coordination function is genuine and victims are not completely powerless — they are organizing substitution pathways, diversifying suppliers, and building alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_velocity,
    'How quickly can technological substitution reduce dependency on specific critical minerals?',
    'Historical velocity of substitution (rare-earth elements in magnets, cobalt in batteries). Time-to-replacement for critical technologies.',
    'If substitution is fast (5-10 years): constraint degrades to scaffold. If slow (30+ years): constraint hardens as snare for importers. If blocked by physics/economics: constraint is structurally durable tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_velocity, empirical, 'Technological substitution velocity for critical minerals').

omega_variable(
    recycling_loop_closure,
    'Can circular economy recycling close the supply loop fast enough to meaningfully reduce primary dependency?',
    'Recycling infrastructure deployment rates, recovery rates by mineral, economic viability of secondary processing, time-to-steady-state circulation.',
    'If loop closes < 15 years: supply constraint shifts to resource availability within importers (reduces extraction). If > 30 years: importers remain dependent on primary extraction and geopolitical supply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recycling_loop_closure, empirical, 'Speed of circular economy loop closure for mineral recycling').

omega_variable(
    geopolitical_chokepoint_fungibility,
    'Are specific geographic chokepoints (DRC cobalt, China rare earths) truly irreplaceable, or does supply diversification eliminate the extraction leverage?',
    'Comparison of production capacity when secondary producers scale up; identification of true bottlenecks vs contingent concentration.',
    'If truly irreplaceable: extraction is structural and durable (snare for importers). If fungible: leverage is temporary and vulnerable to supply competition (weaker tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geopolitical_chokepoint_fungibility, empirical, 'Whether geographic mineral chokepoints are truly irreplaceable').

omega_variable(
    energy_transition_mineral_coupling,
    'As energy transitions away from fossil fuels, does mineral dependency shift or merely relocate (battery minerals replacing oil)?',
    'Comparative analysis of supply chain complexity: oil geopolitics vs. battery mineral geopolitics. Concentration metrics for producing regions.',
    'If dependency shifts structurally: constraint persists but with different beneficiaries. If dependency is reduced: energy transition truly breaks the constraint. If dependency relocates equally: the constraint''s beneficiary shifts but extraction magnitude stays constant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_transition_mineral_coupling, empirical, 'Whether energy transition reduces or relocates mineral dependency').

omega_variable(
    export_control_enforcement_cost,
    'What is the true enforcement cost for mineral-producing states to weaponize supply constraints?',
    'Historical analysis of export restrictions (China rare earths 2010, DRC cobalt embargoes): revenue losses, retaliation, supply chain repositioning costs.',
    'If enforcement is cheap: extraction leverage is durable. If enforcement is expensive: the threat is credible but exercising it damages the beneficiary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(export_control_enforcement_cost, empirical, 'Cost of enforcing mineral supply weaponization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geopolitical_mineral_dependency, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geomin_tr_t0, geopolitical_mineral_dependency, theater_ratio, 0, 0.35).
narrative_ontology:measurement(geomin_tr_t7, geopolitical_mineral_dependency, theater_ratio, 7, 0.42).
narrative_ontology:measurement(geomin_tr_t15, geopolitical_mineral_dependency, theater_ratio, 15, 0.48).
narrative_ontology:measurement(geomin_tr_t22, geopolitical_mineral_dependency, theater_ratio, 22, 0.52).

% Extraction over time
narrative_ontology:measurement(geomin_be_t0, geopolitical_mineral_dependency, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(geomin_be_t7, geopolitical_mineral_dependency, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(geomin_be_t15, geopolitical_mineral_dependency, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(geomin_be_t22, geopolitical_mineral_dependency, base_extractiveness, 22, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geopolitical_mineral_dependency, resource_allocation).
narrative_ontology:affects_constraint(geopolitical_mineral_dependency, energy_transition_supply_chain).
narrative_ontology:affects_constraint(geopolitical_mineral_dependency, semiconductor_manufacturing_bottleneck).
narrative_ontology:affects_constraint(geopolitical_mineral_dependency, rare_earth_export_restriction).

% DUAL FORMULATION NOTE:
% Geopolitical mineral dependency is an upstream structural constraint affecting downstream specific-material and technology-specific constraints. The general constraint operates through supply concentration and geopolitical control; specific constraints (rare earths, lithium, cobalt) have higher extraction values because they represent acute chokepoints. All specific-material constraints link to this general story as affected constraints, showing how systemwide resource allocation asymmetries cascade into technology-specific vulnerabilities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geopolitical_mineral_dependency, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
