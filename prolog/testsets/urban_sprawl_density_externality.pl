% ============================================================================
% CONSTRAINT STORY: urban_sprawl_density_externality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_urban_sprawl_density_externality, []).

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
 *   constraint_id: urban_sprawl_density_externality
 *   human_readable: Urban Sprawl Density Externality
 *   domain: urban_planning/environmental_economics
 *
 * SUMMARY:
 *   Urban sprawl density externality creates a structural tension between
 *   individual household preferences for low-density housing, automotive
 *   freedom, and private space on the one hand, and the system-level
 *   requirement for density to sustain transit, ecosystems, walkable
 *   neighborhoods, and efficient public infrastructure on the other. The
 *   constraint exhibits mixed coordination and extraction functions: sprawl
 *   coordinates housing distribution across a growing population (genuine
 *   coordination benefit) while simultaneously extracting from those left in
 *   depopulating urban cores and imposing infrastructure costs that exceed
 *   revenues. The extractiveness value (0.58) reflects that the constraint's
 *   primary mechanism is not pure coordination but rather the capture of
 *   benefits by developers and suburban homeowners while costs are socialized
 *   (road maintenance, ecosystem loss, transit system insolvency). The
 *   theater ratio (0.48) indicates moderate performative content: zoning
 *   claims to rationally separate land uses but primarily locks in sprawl
 *   patterns through regulatory inertia; real estate marketing emphasizes
 *   'natural' housing preferences while suppressing visibility of
 *   infrastructure externalities.
 *
 * KEY AGENTS:
 *   - Urban Core Residents: Primary victims (powerless/trapped) — sunk costs in declining neighborhoods; bear cost of tax base erosion and service degradation
 *   - Peripheral Developers: Primary beneficiaries (institutional/arbitrage) — capture land appreciation and development profits on cheap periphery; benefit from infrastructure subsidies
 *   - Suburban Homeowners: Mixed beneficiary-victim (moderate/constrained) — benefit from low-cost housing and space; locked into car dependency and eventual infrastructure maintenance collapse
 *   - Transit Authority Coalition: Organized agent (organized/mobile) — see sprawl as correctable through density mandates and transit investment; building alternative coordination mechanisms
 *   - Zoning Establishment: Institutional gatekeeper (institutional/arbitrage) — maintains regulatory structure through path dependence; sees own system as degraded but enforces it anyway
 *   - Transportation-Automotive Complex: Powerful extractor (powerful/arbitrage) — captures vehicle dependency extraction; coordinates market expansion while enforcing car-required infrastructure
 *   - Ecosystem Services: Victim (powerless/trapped) — abstract collective good bearing full cost of habitat loss, watershed fragmentation, and pollution; no exit or voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(urban_sprawl_density_externality, 0.58).
domain_priors:suppression_score(urban_sprawl_density_externality, 0.65).
domain_priors:theater_ratio(urban_sprawl_density_externality, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(urban_sprawl_density_externality, extractiveness, 0.58).
narrative_ontology:constraint_metric(urban_sprawl_density_externality, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(urban_sprawl_density_externality, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(urban_sprawl_density_externality, tangled_rope).
narrative_ontology:human_readable(urban_sprawl_density_externality, "Urban Sprawl Density Externality").
narrative_ontology:topic_domain(urban_sprawl_density_externality, "urban_planning/environmental_economics").

domain_priors:requires_active_enforcement(urban_sprawl_density_externality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(urban_sprawl_density_externality, peripheral_developers).
narrative_ontology:constraint_beneficiary(urban_sprawl_density_externality, suburban_homeowners).
narrative_ontology:constraint_beneficiary(urban_sprawl_density_externality, automotive_industry).
narrative_ontology:constraint_victim(urban_sprawl_density_externality, urban_core_residents).
narrative_ontology:constraint_victim(urban_sprawl_density_externality, transit_system_viability).
narrative_ontology:constraint_victim(urban_sprawl_density_externality, ecosystem_services).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: URBAN CORE RESIDENT (SNARE) — Trapped by sunk housing costs, social networks, and employment concentration in declining urban centers. Bears full cost of density loss (reduced tax base, deteriorating transit, service degradation) with no exit. Maximum experienced extraction from perspective of those left behind.
constraint_indexing:constraint_classification(urban_sprawl_density_externality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PERIPHERAL DEVELOPER (ROPE) — Captures coordination benefit: sprawl coordination solves the problem of housing demand distribution across space. Also captures extraction: profitable lot development on cheap periphery land, subsidized by infrastructure costs borne elsewhere. Net beneficiary experiencing pure coordination gain during development phase.
constraint_indexing:constraint_classification(urban_sprawl_density_externality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: SUBURBAN HOMEOWNER (TANGLED ROPE) — Benefits from low-cost housing, automotive freedom, and private space (coordination function: distributed housing allocation). Also bears costs of car dependency, community fragmentation, and eventual infrastructure maintenance collapse. Constrained by mortgage lock-in and lack of alternative suburban systems.
constraint_indexing:constraint_classification(urban_sprawl_density_externality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: TRANSIT AUTHORITY COALITION (SCAFFOLD) — Sees sprawl as temporary failure correctable through density mandates, mixed-use zoning, and transit investment. Organized agents (planning departments, transit agencies, environmental groups) have agency and exit paths through land-use reform. Sunset logic: New Urbanism, 15-minute cities, and infill development are building alternative coordination mechanisms.
constraint_indexing:constraint_classification(urban_sprawl_density_externality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ZONING ESTABLISHMENT (PITON) — Euclidean zoning (residential/commercial/industrial separation) is largely theatrical: it claims to organize land use rationally but primarily preserves property values for incumbent owners while locking in sprawl patterns through regulatory inertia. The zoning ritual persists despite documented dysfunction (traffic congestion, environmental harm, housing unaffordability). Maintained through path dependence, not because it solves the stated coordination problem.
constraint_indexing:constraint_classification(urban_sprawl_density_externality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TRANSPORTATION-AUTOMOTIVE COMPLEX (TANGLED ROPE) — Beneficiary capturing enormous extraction: sprawl creates car dependency, fuel consumption, maintenance, insurance, and parking costs. Coordinates vehicle market expansion (genuine coordination function: distributes vehicles across population growth). Also enforces extraction: political capture of transportation budgets, highway subsidy, parking mandates, and resistance to transit funding. Powerful with exit options, so moderate experienced chi despite high structural extraction.
constraint_indexing:constraint_classification(urban_sprawl_density_externality, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From an economic physics perspective, sprawl may appear as an immutable consequence of transport cost reduction and land supply elasticity: given cheap transport and abundant land, dispersed settlement is the 'natural' equilibrium. This perspective treats sprawl density externality as a law of settlement economics. However, the structural data (beneficiaries, victims, regulatory enforcement) contradicts the mountain classification — the engine's false summit detector reveals that 'inevitable market outcome' naturalizes contingent institutional choices (zoning, infrastructure subsidy, parking mandates).
constraint_indexing:constraint_classification(urban_sprawl_density_externality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(urban_sprawl_density_externality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(urban_sprawl_density_externality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(urban_sprawl_density_externality, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(urban_sprawl_density_externality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(urban_sprawl_density_externality, TR),
    TR >= 0.70.

:- end_tests(urban_sprawl_density_externality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantially through: (1) private developers capturing land appreciation while infrastructure costs are public, (2) suburban homeowners capturing low-cost housing while urban cores bear density loss costs, (3) automotive industry capturing vehicle dependency while road maintenance and pollution costs are socialized. The value reflects that extraction is significant but not absolute — suburban homeowners do gain genuine housing and space benefits, suggesting genuine coordination function alongside the extraction. The value has increased from 0.35 to 0.58 over the interval as sprawl accumulates and infrastructure maintenance costs come due (cities face declining tax bases, aging road and transit systems, ecosystem service loss). Suppression (0.65): Moderate-high. Significant barriers to exit or reform include: mortgage lock-in for both suburban homeowners and urban residents, zoning regulatory barriers to density and mixed-use development, infrastructure subsidy structures that make sprawl artificially cheap relative to dense infill, car dependency making car-free alternatives difficult, and political capture by development and real estate interests. Theater ratio (0.48): Moderate-low. Zoning regulations are substantially performative (rationalizing what is actually property-value preservation), real estate marketing emphasizes 'natural' preferences while suppressing visibility of infrastructure costs, and density development restrictions claim environmental protection while actually enforcing sprawl. However, the constraint has genuine functional content: sprawl does distribute housing across land, does provide space efficiency trade-offs, and does reflect some authentic homeowner preferences. Theater increases slightly over the interval as zoning becomes more visibly disconnected from stated goals (environmental protection, affordability, walkability) as sprawl's costs become apparent.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence reflects the constraint's core function: private actors capture benefits while costs are socialized. The beneficiary (developer, suburban homeowner) sees pure coordination (housing distribution) or mixed coordination-extraction (Tangled Rope). The victim (urban core resident) sees pure extraction (Snare) with no escape. The organized reformer sees a temporary failure with a sunset (Scaffold). The institutional gatekeeper sees its own system as degraded but continues enforcing it (Piton). The extractor (automotive complex) sees coordination of vehicle dependency (Tangled Rope) while capturing extraction. These gaps are not ambiguity — they are structural: different positions in the extraction flow legitimately experience the same constraint as different types.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) track how structural position determines experienced extraction. Urban core residents as trapped powerless victims derive d ≈ 0.95 → high f(d) → high experienced χ. Suburban homeowners as constrained moderate beneficiary-victims derive d ≈ 0.55 → moderate f(d) → moderate experienced χ. Peripheral developers as institutional arbitrage beneficiaries derive d ≈ 0.15 → low f(d) → low or negative experienced χ. The transportation-automotive complex as powerful arbitrage beneficiary derives d ≈ 0.35 → moderate f(d) → moderate experienced χ despite high structural extraction, because power and exit options reduce experienced pressure. The analytical observer as analytical observer derives d ≈ 0.72 → high f(d) → high visible extraction chi, consistent with the false summit detection (the mountain classification is a naturalization of institutional choices, not a law of nature).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES THROUGH PERSPECTIVAL DIVERGENCE: The mandatrophy — 'is sprawl coordination or extraction?' — is resolved by noting that it is genuinely both, experienced differently from different structural positions. The classification is not wrong when it varies; the variation IS the diagnostic signal. The beneficiary's Rope classification is accurate: sprawl does solve the coordination problem of distributing housing across growing population. The victim's Snare classification is accurate: sprawl does extract by concentrating costs on those left in declining urban cores. The reformer's Scaffold classification is accurate: sprawl patterns are correctable through zoning reform and transit investment, with a real sunset from New Urbanism policies. No single classification is 'the' answer; the presheaf over the structural positions IS the answer. The false summit (Mountain from analytical context) is specifically flagged by the engine's natural law detector — the 'inevitable economic outcome' framing naturalizes institutional choices around subsidy structure, zoning, and infrastructure investment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_attribution_boundary,
    'What portion of sprawl results from genuine transport-cost reduction vs. hidden infrastructure subsidies (highway funding, road maintenance, free parking, externality non-pricing)?',
    'Lifecycle cost accounting: compare sprawl patterns in jurisdictions with full cost-recovery road pricing vs. subsidized road regimes; cross-national comparison of sprawl density vs. transportation cost structures',
    'If subsidies > 40% of transport cost: sprawl is largely extraction mechanism disguised as market efficiency. If subsidies < 20%: sprawl is genuine equilibrium response to real technology and land availability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_attribution_boundary, empirical, 'Attribution of sprawl to subsidy vs. genuine transport cost reduction').

omega_variable(
    density_critical_mass_threshold,
    'What minimum urban core density is required for transit viability, ecosystem service provision, and social infrastructure efficiency?',
    'Cross-city analysis of density vs. transit operating cost per capita, ecosystem service provision, municipal service cost per capita, and walkability indices',
    'If threshold is high (> 8000 per sq km): sprawl creates permanent structural underperformance in transit and services. If threshold is moderate (3000-6000): sprawl is recoverable through infill; snare classification is too strong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(density_critical_mass_threshold, empirical, 'Minimum density thresholds for service viability').

omega_variable(
    zoning_entrenchment_mechanism,
    'Is suburban zoning maintained through deliberate political capture, or through genuine homeowner preference for low-density living and property protection?',
    'Policy experiment: compare zoning reform adoption rates with homeowner opposition; analyze correlation between zoning restriction strength and property-value protection vs. genuine preference measures',
    'If deliberate capture: Piton classification is accurate — zoning is theatrical enforcement of extraction. If genuine preference: Rope classification is more accurate — sprawl coordination reflects authentic demand distribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(zoning_entrenchment_mechanism, conceptual, 'Whether zoning entrenchment is institutional capture or genuine preference').

omega_variable(
    automotive_dependency_lock_in,
    'Is car dependency in sprawling systems a result of sprawl-induced infrastructure design, or does sprawl result from pre-existing automobile-centric culture?',
    'Historical analysis of sprawl timing relative to car adoption; comparison of sprawl patterns in car-centric vs. transit-first cultural contexts; analysis of sprawl reversal when transit infrastructure is added',
    'If sprawl induces car dependency: infrastructure lock-in makes Snare classification more accurate for urban core residents. If culture induces sprawl: homeowner agency is higher; Tangled Rope is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automotive_dependency_lock_in, empirical, 'Causal direction between sprawl and automotive dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(urban_sprawl_density_externality, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sprawl_tr_t0, urban_sprawl_density_externality, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sprawl_tr_t20, urban_sprawl_density_externality, theater_ratio, 20, 0.42).
narrative_ontology:measurement(sprawl_tr_t40, urban_sprawl_density_externality, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(sprawl_be_t0, urban_sprawl_density_externality, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sprawl_be_t20, urban_sprawl_density_externality, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(sprawl_be_t40, urban_sprawl_density_externality, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(urban_sprawl_density_externality, resource_allocation).
narrative_ontology:affects_constraint(urban_sprawl_density_externality, automotive_infrastructure_subsidy).
narrative_ontology:affects_constraint(urban_sprawl_density_externality, real_estate_speculation_extraction).
narrative_ontology:affects_constraint(urban_sprawl_density_externality, transit_system_viability).
narrative_ontology:affects_constraint(urban_sprawl_density_externality, ecosystem_fragmentation).

% DUAL FORMULATION NOTE:
% Urban sprawl density externality is the parent constraint affecting downstream infrastructure subsidies (which have their own extraction dynamics), real estate speculation (which has its own extractiveness value), transit system non-viability (which is a structural consequence), and ecosystem fragmentation (which has its own victimology). Each downstream constraint has independent ε values reflecting their specific structural mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
