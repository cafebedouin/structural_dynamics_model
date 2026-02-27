% ============================================================================
% CONSTRAINT STORY: planetary_diet_constraint_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_planetary_diet_constraint_2026, []).

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
 *   constraint_id: planetary_diet_constraint_2026
 *   human_readable: Planetary Boundary Dietary Alignment
 *   domain: ecological/economic/social
 *
 * SUMMARY:
 *   The planetary boundary dietary alignment constraint represents a
 *   fundamental tension between ecological survival and economic
 *   distribution. The framing of Mediterranean diet as 'planetary health gold
 *   standard' creates a seemingly coordinated response to climate and
 *   biodiversity crises — individual consumption choices aligned with
 *   biophysical limits. However, the enforcement mechanism and distribution
 *   of compliance costs reveal an asymmetric extraction structure: wealthy
 *   consuming regions define what counts as planetary health, smallholder
 *   farmers in the global south bear production and livelihood costs,
 *   price-vulnerable populations face nutritional trade-offs, and
 *   agribusiness consolidates market power through sustainability
 *   certification. The constraint exhibits genuine coordination function
 *   (aligning human diet to planetary carrying capacity is not optional)
 *   simultaneously with extractive distribution (the alignment occurs via
 *   mechanisms that concentrate power and externalize costs). This makes it a
 *   canonical tangled_rope: hybrid coordination-extraction where both
 *   components are structural. The theater ratio has increased from 0.42
 *   (2020) to 0.65 (2050) as the regulatory machinery of food standards,
 *   certification regimes, and sustainability metrics has proliferated
 *   without corresponding reform of land tenure, smallholder economics, or
 *   food system power consolidation. Agroecological alternatives
 *   (agroforestry, intercropping, regenerative agriculture) offer genuine
 *   technical pathways to satisfy both planetary boundaries AND smallholder
 *   livelihoods, creating a scaffold structure with a realistic 15-25 year
 *   sunset — but only if the extraction mechanism is deliberately dismantled
 *   rather than naturalized as inevitable.
 *
 * KEY AGENTS:
 *   - Smallholder Farmers (Global South): Primary victim (powerless/trapped) — forced to meet planetary boundary specifications while bearing production risk and subsistence insecurity
 *   - Price-Vulnerable Urban Consumers: Primary victim (powerless/trapped) — face caloric/nutritional trade-offs when planetary diet exceeds affordable food baskets
 *   - Planetary Biosphere: Nominal beneficiary (yet cannot organize or claim its benefit) — subject of the constraint; ecological system to be preserved
 *   - Agribusiness Consolidation: Primary beneficiary (institutional/arbitrage) — benefits from planetary boundary framing enabling premium product lines, certification rents, and scale advantages
 *   - Industrial Food Retailers: Secondary beneficiary (moderate/constrained) — benefit from sustainability marketing and differentiated pricing; constrained by supply diversification requirements
 *   - Climate and Food Justice Coalition: Organized actors (organized/constrained) — NGOs, indigenous movements, agroecology networks building alternative pathways with sunset logic
 *   - Nutritional Science Establishment: Institutional actor (institutional/arbitrage) — maintains Mediterranean diet consensus through citation clustering and regional preference validation; sees own guidance as performative (piton perspective)
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — views constraint as hybrid coordination-extraction requiring simultaneous protection of biophysical limits AND redistribution of power in food system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(planetary_diet_constraint_2026, 0.52).
domain_priors:suppression_score(planetary_diet_constraint_2026, 0.68).
domain_priors:theater_ratio(planetary_diet_constraint_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(planetary_diet_constraint_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(planetary_diet_constraint_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(planetary_diet_constraint_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(planetary_diet_constraint_2026, tangled_rope).
narrative_ontology:human_readable(planetary_diet_constraint_2026, "Planetary Boundary Dietary Alignment").
narrative_ontology:topic_domain(planetary_diet_constraint_2026, "ecological/economic/social").

domain_priors:requires_active_enforcement(planetary_diet_constraint_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(planetary_diet_constraint_2026, planetary_biosphere).
narrative_ontology:constraint_beneficiary(planetary_diet_constraint_2026, agribusiness_consolidation).
narrative_ontology:constraint_beneficiary(planetary_diet_constraint_2026, wealthy_consuming_populations).
narrative_ontology:constraint_victim(planetary_diet_constraint_2026, smallholder_farmers).
narrative_ontology:constraint_victim(planetary_diet_constraint_2026, rural_agricultural_workers).
narrative_ontology:constraint_victim(planetary_diet_constraint_2026, price_vulnerable_populations).
narrative_ontology:constraint_victim(planetary_diet_constraint_2026, traditional_food_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLHOLDER FARMER (SNARE) — Trapped by land tenure insecurity, commodity price controls, and dietary imperatives imposed externally. Cannot exit commodity crop systems without loss of livelihood. Bears full extraction: forced to produce according to planetary boundary specifications while bearing subsistence risk. Suppression through credit dependence, seed patents, and market concentration.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRICE-VULNERABLE CONSUMER (SNARE) — Trapped by income constraints. Planetary boundary diet (Mediterranean-style, plant-forward, seasonal) requires access to expensive fresh produce, diverse legumes, and premium olive oil. Low-income households face choice between ecological compliance and caloric adequacy. Maximum extraction through food insecurity and nutritional trade-offs.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: MID-MARKET RETAILER (TANGLED ROPE) — Constrained by both supply chain consolidation and consumer demand differentiation. Benefits from planetary boundary marketing (premium pricing on sustainable/Mediterranean products) but also bears cost of supply diversification and inventory complexity. Mixed extraction: benefits from coordination premium but constrained by logistics.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: AGRIBUSINESS CONSOLIDATION (ROPE) — Net beneficiary. Planetary boundary framing enables vertical consolidation: large firms can invest in certification, premium Mediterranean product lines, and market differentiation while smaller competitors cannot. Low extraction experienced — the constraint itself is a coordination mechanism that benefits scale and concentration. Arbitrage available through premium product development.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE AND FOOD JUSTICE COALITION (SCAFFOLD) — Organized agents (NGOs, indigenous food sovereignty movements, agroecology networks) see the planetary boundary constraint as temporary: regenerative agriculture, local food systems, and agroforestry are building alternative pathways that satisfy both ecological limits AND smallholder livelihoods. Exit path visible through agroecological transition (15-25 year sunset).
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NUTRITIONAL SCIENCE ESTABLISHMENT (PITON) — The 'Mediterranean diet is planetary health gold standard' narrative persists through institutional inertia and citation clustering despite contradictory evidence: Mediterranean diet requires temperate climate conditions not globally replicable, depends on olive oil monocultures with their own ecological costs, and prescribes protein sources (fish, dairy) with high embodied resource requirements. The scientific consensus persists because it validates wealthy, consuming-region preferences. Theater ratio high — the ritual of diet guideline-setting performs planetary concern without requiring systemic food system change.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational/universal view, the constraint exhibits genuine coordination function (aligning diet to biophysical carrying capacity is not optional) AND asymmetric extraction (the alignment mechanism concentrates power toward wealthy consuming regions and away from smallholders). The constraint is neither pure law nor pure extraction — it is a hybrid where the coordination content is real but the distribution mechanism is extractive. This is the canonical DR classification that captures the full structural tension.
constraint_indexing:constraint_classification(planetary_diet_constraint_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planetary_diet_constraint_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(planetary_diet_constraint_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planetary_diet_constraint_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(planetary_diet_constraint_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(planetary_diet_constraint_2026, TR),
    TR >= 0.70.

:- end_tests(planetary_diet_constraint_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The planetary diet constraint extracts from smallholders and poor consumers through production mandates, price volatility, and nutritional adequacy trade-offs. However, this is not maximal extraction (not 0.70+) because: (1) legitimate coordination content exists — aligning diet to planetary boundaries is not arbitrary, (2) alternative pathways (agroecology) are technologically feasible, not structurally impossible, and (3) the constraint is recent (2020 consensus) and not yet fully entrenched. Suppression (0.68): Moderately high. Smallholders face significant barriers: land tenure insecurity, credit dependence on commodity crops, seed patents that prevent saving/replanting, commodity price controls, and lack of market access for diverse agroecological products. These barriers are substantial but not total — some countries have functioning smallholder sectors and active agroecological movements. Theater ratio (0.58): Moderate-high. Dietary guidelines, sustainability certifications, and food labeling perform planetary concern visibly; the performative content has increased as regulatory machinery has proliferated. However, theater is not dominant (not 0.70+) because genuine ecological and nutritional content remains — the performance does not wholly replace function.
 *
 * PERSPECTIVAL GAP:
 *   Maximal perspectival divergence between victim (Snare) and beneficiary (Rope) perspectives. Smallholders and poor consumers experience maximum extraction; agribusiness experiences minimum extraction and benefits from consolidation. The scaffold perspective (organized agents) is crucial: it reveals that the extraction is NOT inevitable but depends on current institutional arrangements (land tenure, commodity pricing, power consolidation). If agroecological pathways succeeded and smallholder economics were protected, the constraint could transition from tangled_rope (mixed) to primarily rope (coordination). The piton perspective reveals that nutritional science has naturalized regional preferences (Mediterranean diet) as universal planetary health, performing concern while avoiding systemic food system redistribution. The analytical perspective captures why the constraint is tangled_rope at the foundational level: both coordination and extraction are real, and mandatrophy resolution requires naming both rather than collapsing one into the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options per the schema. Victims with trapped exit (smallholders, price-vulnerable consumers) derive high d (0.90+), producing f(d) ≈ 1.40, multiplying their experienced χ substantially. Beneficiaries with arbitrage (agribusiness, nutrition establishment) derive low d (0.05-0.15), producing f(d) ≈ -0.12, dampening their experienced extraction and potentially reversing it (they extract benefit from the constraint, not burden). The climate coalition occupies a middle position: they are victims of food system inertia (bear cost of delay) but also partial architects of solutions (beneficiary of agroecological transition momentum), with constrained exit (cannot fully exit current system, cannot fully transition to alternatives). This produces d ≈ 0.50-0.55, f(d) ≈ 0.65-0.75, moderate experienced extraction reflecting their hybrid position. Scope modifier σ(global)=1.2 amplifies all χ values because the constraint operates at global scale — verification and enforcement are harder, hidden extraction is easier to hide.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint stems from confusion between coordination content and extraction mechanism. The COORDINATION CLAIM: Aligning human diet to planetary biophysical boundaries is necessary and good. This is true and non-negotiable. The EXTRACTION MECHANISM: The alignment is being achieved via mechanisms (commodity crop mandates, nutritional science consensus defining Mediterranean as universal standard, agricultural consolidation) that concentrate power toward wealthy consuming regions and away from smallholders. THIS IS NOT NECESSARY and represents distributional choice, not biophysical law. Resolving mandatrophy requires: (1) affirming the coordination content — planetary boundaries are real constraints on sustainable food systems, (2) rejecting the claim that current extraction mechanisms are inevitable — agroecological systems can satisfy boundaries while protecting smallholder livelihoods, (3) recognizing that the tangled_rope classification depends on CURRENT power structures, not on physics, and (4) naming the decision point: if agroecological alternatives are pursued and land tenure/smallholder economics are protected, the constraint can transition toward pure coordination (Rope). If current extraction mechanisms are locked in through institutional inertia, the constraint degradates toward pure extraction (Snare) for powerless agents. The analytical observer's perspective prevents false naturalization: the constraint is NOT a Mountain (not inherent to human nutrition or planetary limits). It is tangled_rope because we have chosen enforcement mechanisms that mix coordination and extraction. Different mechanisms would produce different classifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mediterranean_scalability_threshold,
    'Can a Mediterranean diet pattern (olive oil, seasonal produce, legumes, fish, dairy) be globally scaled to 8+ billion people without exceeding planetary boundaries for land, water, and fisheries?',
    'Biophysical modeling of global land-use, freshwater, and marine-stock requirements if 50%, 75%, and 100% of global population adopted Mediterranean dietary pattern; comparison to current carrying capacity estimates',
    'If globally scalable: planetary diet constraint is coordination mechanism (Rope from more perspectives). If not scalable: constraint masks regional resource capture and becomes pure extraction for non-Mediterranean regions (Snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mediterranean_scalability_threshold, empirical, 'Whether Mediterranean diet pattern is globally scalable').

omega_variable(
    smallholder_agroecology_parity,
    'Can smallholder-scale agroecological systems (intercropping, agroforestry, rotational grazing) achieve yield-per-hectare parity with commodity monocultures while simultaneously meeting planetary boundary criteria?',
    'Longitudinal yield and nutritional output comparison; meta-analysis of agroecological transition studies; carbon and biodiversity metrics for agroecological vs industrial systems at equivalent land areas',
    'If parity achieved: smallholders have genuine exit path (Scaffold true, sunset realistic). If not: smallholders remain trapped (Snare persists, extractive mechanism is structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smallholder_agroecology_parity, empirical, 'Whether agroecology can match commodity productivity').

omega_variable(
    price_elasticity_compliance_deadweight,
    'What fraction of global population experiences caloric or micronutrient deficiency if forced to adopt a planetary-boundary-aligned diet at current food prices and income distribution?',
    'Nutritional modeling of dietary compliance rates across income quintiles; estimation of malnutrition deadweight; comparison to baseline (unrestricted) nutrition rates',
    'If deficiency rate > 20%: constraint creates structural harm for powerless agents (Snare extraction confirmed). If < 5%: harm is marginal and extraction claim weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(price_elasticity_compliance_deadweight, empirical, 'Nutritional compliance deadweight under planetary diet adoption').

omega_variable(
    wealthy_region_dietary_substitutability,
    'Can wealthy consuming regions substitute meat and dairy with alternative proteins (cultivated meat, legume-based products, insect protein) at cost parity within 15-20 years, reducing extraction pressure on smallholders?',
    'Techno-economic modeling of alternative protein production; tracking of price convergence trajectories; patent and R&D investment trends in substitute technologies',
    'If substitutability achieved: scaffold timeline validated, sunset realistic, extraction pressure released. If not: wealthy regions remain dependent on externalized smallholder production, extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wealthy_region_dietary_substitutability, empirical, 'Technological feasibility of alternative protein cost parity').

omega_variable(
    planetary_boundary_enforcement_mechanism,
    'Which agent(s) have the structural capacity and incentive to enforce dietary alignment with planetary boundaries? Who decides what counts as ''planetary health diet'' and via what authority?',
    'Institutional mapping of food system governance; analysis of standard-setting authority (WHO, FAO, national governments, corporate); investigation of whose preferences and regions'' diets are embedded in the ''planetary health'' definition',
    'If enforcement is technocratic/distant: constraint operates as external imposition (Snare from victim perspective). If enforcement is participatory/local: constraint can operate as coordination (Rope potential).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(planetary_boundary_enforcement_mechanism, conceptual, 'Who enforces planetary diet standards and via what authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(planetary_diet_constraint_2026, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(planetdiet_theater_t0, planetary_diet_constraint_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(planetdiet_theater_t15, planetary_diet_constraint_2026, theater_ratio, 15, 0.58).
narrative_ontology:measurement(planetdiet_theater_t30, planetary_diet_constraint_2026, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(planetdiet_extract_t0, planetary_diet_constraint_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(planetdiet_extract_t15, planetary_diet_constraint_2026, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(planetdiet_extract_t30, planetary_diet_constraint_2026, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(planetary_diet_constraint_2026, resource_allocation).
narrative_ontology:boltzmann_floor_override(planetary_diet_constraint_2026, 0.35).
narrative_ontology:affects_constraint(planetary_diet_constraint_2026, agricultural_land_use_boundary).
narrative_ontology:affects_constraint(planetary_diet_constraint_2026, freshwater_availability_constraint).
narrative_ontology:affects_constraint(planetary_diet_constraint_2026, nitrogen_cycle_limit).
narrative_ontology:affects_constraint(planetary_diet_constraint_2026, smallholder_tenure_insecurity).

% DUAL FORMULATION NOTE:
% The planetary diet constraint is downstream of physical planetary boundaries (land, water, nutrients) but represents a distinct structural constraint on the MECHANISM of alignment. The upstream biophysical constraints have ε ≈ 0.05-0.15 (Mountain-Rope hybrid: boundaries are fixed facts but some policy latitude exists). The planetary diet constraint has ε ≈ 0.52 (Tangled Rope: the enforcement mechanism is contingent and extractive). The family structure: biophysical limits are primary (scientific constraint), dietary alignment is secondary (policy constraint), and food system power distribution is tertiary (distributional constraint). All three must be addressed for genuine resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(planetary_diet_constraint_2026, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
