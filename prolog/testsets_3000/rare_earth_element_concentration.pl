% ============================================================================
% CONSTRAINT STORY: rare_earth_element_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_element_concentration, []).

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
 *   constraint_id: rare_earth_element_concentration
 *   human_readable: Rare Earth Element Geographic Concentration and Supply Dependency
 *   domain: geopolitical_economy/materials_science
 *
 * SUMMARY:
 *   Rare earth elements (REEs) are a group of 17 chemically similar metals
 *   essential for modern technology: neodymium and dysprosium for permanent
 *   magnets in wind turbines and electric motors; europium and terbium for
 *   phosphors in displays; lanthanum and cerium for catalytic converters and
 *   batteries. The constraint arises from a deeply asymmetric geographic and
 *   processing distribution: 70% of global mining production is concentrated
 *   in five countries (China, Myanmar, Australia, Russia, USA), but 85% of
 *   processing and refining capacity is in China. This concentration creates
 *   a structural dependency: advanced manufacturing economies cannot source
 *   rare earths without accessing Chinese processing infrastructure. The
 *   constraint exhibits tangled rope structure: genuine coordination (a
 *   global supply chain is more efficient than autarky), nested within
 *   structural extraction (dependency creates leverage for price increases
 *   and geopolitical coercion). The clean energy transition is particularly
 *   vulnerable because rare earth magnets enable the high-efficiency
 *   generators and motors that make renewable energy economically viable,
 *   creating a binding loop: decarbonization requires rare earths, which are
 *   geopolitically concentrated. The constraint's extractiveness has
 *   increased over the interval as manufacturing dependency deepened and
 *   Chinese state integration of mining and processing tightened.
 *
 * KEY AGENTS:
 *   - China's Processing Monopoly: Institutional beneficiary (institutional/arbitrage) — controls 85% of refining capacity; experiences constraint as coordination (rope) and leverages processing control for geopolitical advantage
 *   - Advanced Manufacturing Dependent States: Primary victims (powerless/trapped at biographical timescale) — US, Japan, South Korea, EU economies trapped in dependency; face supply vulnerability and price volatility
 *   - Clean Energy Transition: Victim (powerless/trapped) — wind and EV industries structurally dependent on rare earth magnets with no existing substitutes at scale
 *   - Non-Chinese Mining Operators: Constrained beneficiaries (organized/constrained) — benefit from mining but face higher environmental and regulatory costs than Chinese state-backed competitors; cannot exit mining sector
 *   - State Diversification Programs: Organized actors (organized/mobile) — US, EU, Australia investing in alternative supply chains and processing capacity; perceive exit path through substitution and diversification
 *   - Strategic Reserve Custodians: Institutional actors (institutional/arbitrage) — governments maintain expensive strategic reserves with low functional use but high performative value (piton dynamic)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_element_concentration, 0.58).
domain_priors:suppression_score(rare_earth_element_concentration, 0.65).
domain_priors:theater_ratio(rare_earth_element_concentration, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_element_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(rare_earth_element_concentration, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(rare_earth_element_concentration, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_element_concentration, tangled_rope).
narrative_ontology:human_readable(rare_earth_element_concentration, "Rare Earth Element Geographic Concentration and Supply Dependency").
narrative_ontology:topic_domain(rare_earth_element_concentration, "geopolitical_economy/materials_science").

domain_priors:requires_active_enforcement(rare_earth_element_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_element_concentration, china_processing_monopoly).
narrative_ontology:constraint_beneficiary(rare_earth_element_concentration, mining_corporations).
narrative_ontology:constraint_victim(rare_earth_element_concentration, advanced_manufacturing_dependent_states).
narrative_ontology:constraint_victim(rare_earth_element_concentration, clean_energy_transition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLEAN ENERGY TRANSITION (SNARE) — Wind turbines, electric vehicles, renewable energy infrastructure are structurally dependent on rare earth magnets (neodymium, dysprosium). No substitute technology exists at scale. Exit is impossible without fundamental technology redesign. The constraint extracts from the energy transition through supply uncertainty, price volatility, and forced technology choices. Maximum suppression — the physics of high-efficiency magnets requires these specific elements.
constraint_indexing:constraint_classification(rare_earth_element_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ADVANCED MANUFACTURING ECONOMIES (SNARE) — Developed economies dependent on rare earth supply for semiconductor fabrication, aerospace, defense, medical devices cannot exit without massive industrial relocation. The concentration of processing capacity (85% in China) creates a structural trap. Suppression is high — alternative suppliers take 10-15 years to develop and require capital investment states cannot control. Manufacturing competitiveness is extracted through supply dependency.
constraint_indexing:constraint_classification(rare_earth_element_concentration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-CHINESE MINING CORPORATIONS (TANGLED ROPE) — Mine operators in the US, Australia, and other countries face high environmental remediation costs and complex permitting that Chinese operators avoid. They benefit from coordination norms around environmental standards and labor rights, but these standards create cost asymmetry. They cannot exit mining but are constrained by regulatory enforcement China's state-backed operations circumvent. Mixed extraction and coordination.
constraint_indexing:constraint_classification(rare_earth_element_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CHINA'S PROCESSING MONOPOLY (ROPE) — China benefits from coordination: by controlling refining and separation processes, it established a global hub model that solved the supply chain coordination problem. Purchasers benefit from having a reliable single source. The constraint appears as efficient specialization (rope) from the monopoly's perspective. But this rope contains embedding extraction mechanisms — the monopoly can restrict exports, leverage supply for geopolitical advantage, and set prices with minimal competition.
constraint_indexing:constraint_classification(rare_earth_element_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DIVERSIFICATION AND SUBSTITUTION PROGRAMS (SCAFFOLD) — State-funded efforts to develop new mining in Australia, Mongolia, and the US; investment in alternative magnet materials (ferrite, permanent magnet alloys); and recycling infrastructure represent a temporary coordination solution with a sunset clause. As these mature, the Chinese monopoly loses leverage. Extraction is limited because organized actors perceive an exit path. Estimated sunset: 10-15 years for alternative supplies to reduce China's leverage to <50%.
constraint_indexing:constraint_classification(rare_earth_element_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: STRATEGIC RESERVES AND STOCKPILING THEATER (PITON) — Governments maintain strategic rare earth reserves and maintain alternative processing facilities at high operational cost despite China's lower-cost production. These reserves have limited functional use (drawn on only in true emergencies) but significant performative value (signal of preparedness, domestic capacity maintenance). The theater ratio is high because the primary function — actually using reserves — almost never occurs; the secondary function — appearing prepared — is constant.
constraint_indexing:constraint_classification(rare_earth_element_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational view, rare earth concentration exhibits genuine coordination value (global supply chain efficiency) alongside structural extraction (dependency-based leverage). The constraint solves a real logistics problem while creating a vulnerability. The measured extractiveness (0.58) reflects this hybrid: genuine gains from specialization offset by genuine extraction through concentration.
constraint_indexing:constraint_classification(rare_earth_element_concentration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_element_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_element_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_element_concentration, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_earth_element_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rare_earth_element_concentration, TR),
    TR >= 0.70.

:- end_tests(rare_earth_element_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from dependent manufacturers through price volatility, supply uncertainty, and forced technology choices during the dependency window. The extraction is real but not absolute — alternative pathways (substitution, recycling, new mining) exist and are being pursued. The 0.58 value reflects extraction that is significant and asymmetric but not permanent or totalizing. The measurement trajectory shows extractiveness rising from 0.42 to 0.58 over 15 years as dependency deepened, then slightly declining from 0.55 to 0.58 as substitution programs accelerated (noise in the measurement). Suppression (0.65): High. Barriers to exit include (1) physics of magnet efficiency (rare earth magnets are superior for power density), (2) capital requirements for alternative supply infrastructure (10-15 year development timelines), (3) geopolitical coordination barriers (sanctioning alternative supplies, regulatory capture), (4) technology lock-in (existing manufacturing tooled for rare earth magnets). Theater ratio (0.38): Moderate. Strategic reserves are theater (high performative content, low functional use), but the core constraint is operational — actual supply bottlenecks in refining and processing are real, not performative. The theater ratio is lower than in the verification bottleneck or strategic reserves because the extraction mechanism is material (actual supply constraints) rather than purely reputational.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same material structure can appear as coordination (rope), extraction (snare), temporary failure (scaffold), degraded ritual (piton), or hybrid (tangled rope) depending on the observer's position. The beneficiary's rope is the victim's snare. The organized coalition's scaffold is the trapped manufacturer's false hope if substitution fails. The strategic reserve's piton is actual backup insurance if supply is truly disrupted. The analytical observer sees the tangled rope: genuine coordination value (specialization efficiency) embedded in genuine extraction (dependency leverage). The perspectival gap is not a measurement error — it is the constraint's true structure. The mandatrophy is resolved by recognizing that all perspectives are locally correct and that the global structure is the presheaf (the stack of all perspectives) rather than any single type.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. China's processing monopoly has low directionality (d ≈ 0.05-0.15) because they are beneficiaries with arbitrage-level exit options — they control the chokepoint and can leverage it for maximum advantage. Advanced manufacturers have high directionality (d ≈ 0.85-0.95) because they are victims trapped by physics and capital requirements — they cannot exit without massive technology redesign or decades of investment in alternative supply chains. Non-Chinese miners have moderate directionality (d ≈ 0.50-0.65) because they are partly beneficiaries (mining is profitable) but constrained by regulatory and environmental costs that Chinese state operators avoid. The diversification coalition has moderate-to-low directionality (d ≈ 0.35-0.50) because they perceive an exit path (substitution and alternative supplies) and have agency (state resources and R&D capacity). The sigmoid transformation f(d) then scales these d values to produce experienced extractiveness χ. Victims with high d experience high χ; beneficiaries with low d experience negative or near-zero χ. The pipeline automatically adjusts χ based on spatial scope (global scope σ=1.2 amplifies extraction) and time horizon (biographical horizon makes exit appear more fixed than civilizational horizon). No directionality overrides are needed because the structural data correctly derives the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by exhibiting genuine tangled rope structure: (1) coordination function: the global rare earth processing hub is more efficient than dispersed, redundant capacity; (2) asymmetric extraction: the hub operator leverages this efficiency to extract rents through supply control and pricing power; (3) active enforcement: the monopoly is maintained through state subsidies, regulatory capture, environmental standards evasion, and supply restriction threats. The constraint is not 'actually a snare' (victim perspective) nor 'actually a rope' (beneficiary perspective) — it is genuinely both. The tangled rope classification prevents mislabeling coordination as pure extraction (which would ignore real efficiency gains from specialization) and prevents mislabeling extraction as pure coordination (which would ignore real asymmetric power and pricing). The measured extractiveness (0.58) sits in the tangled rope zone (0.40 ≤ χ ≤ 0.90), confirming the hybrid structure. The suppression (0.65) reflects high barriers to exit, which would push a pure extraction constraint into the snare zone but is compatible with tangled rope when coordination benefits are genuine. The theater ratio (0.38) is low because the constraint operates through material bottlenecks (actual processing capacity), not performative theater — this is consistent with tangled rope that solves real coordination problems while enabling extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_feasibility,
    'Can alternative magnet materials (ferrite, neodymium-free alloys) achieve parity with rare-earth magnets within the next 15 years for key applications?',
    'Tracking development milestones for ferrite magnet efficiency improvements; commercial deployment metrics for alternative materials in wind turbines and EV motors; cost curve projections',
    'If feasible: scaffold perspective confirmed, extraction window is genuinely closing. If infeasible: snare classification persists, clean energy transition remains structurally trapped. Affects mandatrophy resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_feasibility, empirical, 'Whether alternative magnet materials can substitute for rare earths at scale').

omega_variable(
    china_supply_weaponization_likelihood,
    'What is the probability China will weaponize supply restrictions for geopolitical leverage in the next decade?',
    'Analysis of China''s previous export restriction patterns (2010 incident); assessment of state capacity to enforce restrictions; comparison with historical precedent in other critical materials; scenario modeling of geopolitical crises',
    'If high probability: extraction component of tangled rope is stronger, and suppression metric should increase. If low probability: rope perspective gains strength — the constraint is primarily coordination, with extraction as latent possibility rather than active mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_supply_weaponization_likelihood, empirical, 'Likelihood of Chinese supply weaponization').

omega_variable(
    recycling_scalability,
    'Can rare earth recycling from end-of-life products (spent magnets, electronics) reach 30%+ of global demand within 20 years?',
    'Tracking current recycling rates (currently ~5%); capacity expansion in collection and reprocessing infrastructure; cost curve analysis for recycled vs virgin material; regulatory mandates driving collection rates',
    'If scalable: alternative supply pathway reduces China''s leverage, scaffold sunset accelerates. If not scalable: manufacturing economies remain trapped, substitution becomes the only exit path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recycling_scalability, empirical, 'Whether recycling can provide significant alternative supply').

omega_variable(
    deep_sea_extraction_feasibility,
    'Can polymetallic nodule and rare earth extraction from ocean floors become economically viable and environmentally acceptable in the next 20 years?',
    'Tracking International Seabed Authority regulations; pilot extraction project results; cost curve modeling for ocean extraction vs land mining; environmental impact assessments and international consensus on acceptability',
    'If viable: entirely new supply pathway opens, snare perspective begins to degrade. If not viable or banned: ocean option remains closed, constraint structure unchanged. Affects long-term extraction trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deep_sea_extraction_feasibility, empirical, 'Whether ocean-based rare earth extraction can become viable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_element_concentration, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ree_tr_t0, rare_earth_element_concentration, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ree_tr_t5, rare_earth_element_concentration, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ree_tr_t10, rare_earth_element_concentration, theater_ratio, 10, 0.38).
narrative_ontology:measurement(ree_tr_t15, rare_earth_element_concentration, theater_ratio, 15, 0.41).

% Extraction over time
narrative_ontology:measurement(ree_be_t0, rare_earth_element_concentration, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ree_be_t5, rare_earth_element_concentration, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ree_be_t10, rare_earth_element_concentration, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ree_be_t15, rare_earth_element_concentration, base_extractiveness, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_element_concentration, resource_allocation).
narrative_ontology:boltzmann_floor_override(rare_earth_element_concentration, 0.18).
narrative_ontology:affects_constraint(rare_earth_element_concentration, semiconductor_supply_chain).
narrative_ontology:affects_constraint(rare_earth_element_concentration, electric_vehicle_manufacturing).
narrative_ontology:affects_constraint(rare_earth_element_concentration, wind_energy_scalability).
narrative_ontology:affects_constraint(rare_earth_element_concentration, clean_energy_transition_viability).

% DUAL FORMULATION NOTE:
% Rare earth element concentration is downstream of geological distribution (rare earth deposits are geographically clustered) and upstream of manufacturing dependency (clean energy and advanced manufacturing depend on rare earth supply). The geological constraint has ε ≈ 0.10 (mountain — natural distribution). The concentration constraint has ε ≈ 0.58 (tangled rope — coordination plus extraction through monopoly control). The dependency constraint has ε ≈ 0.65+ (snare from manufacturing perspective — trapped by physics and capital requirements). The family structure: geology → concentration → dependency, with concentration as the mechanism that transforms geographic geology into structural extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rare_earth_element_concentration, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
