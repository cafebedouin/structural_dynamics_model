% ============================================================================
% CONSTRAINT STORY: housing_affordability_crisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_housing_affordability_crisis, []).

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
 *   constraint_id: housing_affordability_crisis
 *   human_readable: Housing Affordability Crisis: Coordination and Extraction in Residential Real Estate
 *   domain: economic/urban_planning/policy
 *
 * SUMMARY:
 *   The housing affordability crisis represents a structural constraint where
 *   genuine coordination problems (matching housing supply to demand,
 *   spreading settlement risk, maintaining neighborhoods) coexist with
 *   extractive mechanisms (speculative capture, zoning-induced scarcity,
 *   mortgage financialization, property-tax-dependent municipal governance).
 *   No single classification captures the full structure — the constraint
 *   appears as pure extraction (snare) to trapped renters, as legitimate
 *   market coordination (rope) to property investors, as removable policy
 *   failure (scaffold) to reform advocates, as degraded institutional ritual
 *   (piton) to underfunded public housing sectors, and as immutable natural
 *   law (mountain) to those who naturalize land scarcity. The extractiveness
 *   score has risen monotonically from 0.35 (1990s, when homeownership rates
 *   were higher and debt-to-income ratios lower) to 0.58 (2020s, with
 *   widespread unaffordability and renter precarity). The theater ratio has
 *   remained moderate (0.32-0.48) because policy discourse acknowledges the
 *   problem while action remains performative — zoning reform, affordable
 *   housing mandates, and rent control policies generate political cover
 *   without producing supply. The constraint is fundamentally a tangled_rope
 *   because removing either the coordination function (housing markets) or
 *   the extraction mechanism (speculative capture, zoning restriction) would
 *   break the other. Institutional decomposition suggests separating this
 *   into distinct constraints: housing_supply_coordination (lower ε,
 *   primarily rope), zoning_capture_mechanism (higher ε, tangled_rope),
 *   mortgage_financialization (high ε, snare), and incumbent_voter_lock
 *   (medium ε, tangled_rope with identity_locked component).
 *
 * KEY AGENTS:
 *   - Renters (powerless/trapped): Primary victims; structurally locked out of ownership by income insufficiency and supply constraints; bear extraction through perpetual wealth transfer
 *   - First-time homebuyers (powerless/identity_locked): Victims whose identity is fused with homeownership aspiration; structurally mobile but identity-trapped by internalized 'homeownership = adulthood' narrative
 *   - Middle-income households (moderate/constrained): Mixed position — constrained by rising prices and debt service, but benefit from housing as leverage and inflation hedge; experience asymmetric extraction relative to established owners
 *   - Property investors and developers (institutional/arbitrage): Primary beneficiaries; perceive constraint as enabling capital allocation; high exit options enable arbitrage across markets
 *   - Existing homeowners (institutional/arbitrage): Secondary beneficiaries; benefit from asset appreciation and leveraged wealth accumulation; form electoral majority supporting zoning restrictions
 *   - Municipal governments (institutional/constrained): Constrained by property-tax dependence and incumbent voter preference; provide coordination (infrastructure, zoning) alongside extraction (restricting supply to maintain values)
 *   - Tenant unions and advocacy coalitions (organized/mobile): Recognize hybrid nature; perceive both coordination function and extraction mechanism; have exit options through organizing and policy influence
 *   - Regional policy innovators (organized/mobile): See constraint as temporary failure addressable through institutional redesign; Vienna, Singapore, Portland models demonstrate feasible alternatives
 *   - Public housing sector (institutional/constrained): Formerly provided coordination function; degraded to symbolic gesture (piton) due to political defunding and stigmatization despite operational viability elsewhere
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(housing_affordability_crisis, 0.58).
domain_priors:suppression_score(housing_affordability_crisis, 0.68).
domain_priors:theater_ratio(housing_affordability_crisis, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(housing_affordability_crisis, extractiveness, 0.58).
narrative_ontology:constraint_metric(housing_affordability_crisis, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(housing_affordability_crisis, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(housing_affordability_crisis, tangled_rope).
narrative_ontology:human_readable(housing_affordability_crisis, "Housing Affordability Crisis: Coordination and Extraction in Residential Real Estate").
narrative_ontology:topic_domain(housing_affordability_crisis, "economic/urban_planning/policy").

domain_priors:requires_active_enforcement(housing_affordability_crisis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(housing_affordability_crisis, property_investors).
narrative_ontology:constraint_beneficiary(housing_affordability_crisis, existing_homeowners).
narrative_ontology:constraint_beneficiary(housing_affordability_crisis, real_estate_financiers).
narrative_ontology:constraint_victim(housing_affordability_crisis, first_time_homebuyers).
narrative_ontology:constraint_victim(housing_affordability_crisis, renters).
narrative_ontology:constraint_victim(housing_affordability_crisis, low_income_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RENTER LOCKED IN INSECURITY — Trapped by income insufficient to afford ownership; limited exit options due to geographic job concentration and rental supply constraints. Bears full extraction cost: wages insufficient to build equity, perpetually transfers wealth to property owners. No material pathway to escape the constraint within biographical timeframe.
constraint_indexing:constraint_classification(housing_affordability_crisis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIRST-TIME HOMEBUYER (IDENTITY-LOCKED SNARE) — Structurally mobile (could immigrate to lower-cost regions, delay family formation, accept extended renting) but identity-fused with homeownership as path to adulthood, family stability, and middle-class membership. The constraint's binding mechanism is cognitive: the agent has internalized the narrative that 'becoming a homeowner' is identity achievement, not optional purchase. Exit would require abandoning not just a financial transaction but a constituted social identity. At biographical time, perceived as mountain (unchangeable); at generational time, could perceive as rope if identity frame shifts, but currently trapped by internalized framing.
constraint_indexing:constraint_classification(housing_affordability_crisis, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: MIDDLE-INCOME HOUSEHOLD (TANGLED ROPE) — Constrained by rising prices and debt service costs, but also benefits from housing as inflation hedge and collateral for consumption credit. Experiences mixed extraction: loses savings rate and family formation agency, gains leveraged asset appreciation. High costs to exit (relocation, career sacrifice) but not material impossibility. Bears asymmetric extraction relative to existing homeowners while participating in the coordination function (housing supply maintains social stability).
constraint_indexing:constraint_classification(housing_affordability_crisis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PROPERTY INVESTORS AND DEVELOPERS (ROPE) — Primary beneficiaries. Experience the constraint as coordination: allocating capital to housing supply, managing portfolio risk, enabling transactions. Net extraction runs toward them. High arbitrage options (reallocate capital across markets, geographies, asset classes) provide exit capacity. Perceive the constraint as enabling their market function with minimal coercive overhead — extraction appears as earned return on capital.
constraint_indexing:constraint_classification(housing_affordability_crisis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EXISTING HOMEOWNERS (ROPE) — Secondary beneficiary class. Coordinated by mortgage institutions and property tax systems; benefits from asset appreciation and leverage availability. Perceives constraint as enabling wealth accumulation and stability. Exit options available (downsize, relocate, sell) but highly exercised arbitrage (tax-deferred exchanges, HELOC debt). Net extraction toward this group from constrained newcomers.
constraint_indexing:constraint_classification(housing_affordability_crisis, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MUNICIPAL GOVERNMENTS (TANGLED ROPE) — Institutionally constrained by property tax dependence and incumbent voter preference for low-density zoning (existing homeowners are dominant electoral bloc). Provides genuine coordination (zoning, infrastructure, public goods) alongside extraction (restrictive supply maintains property values; affordable housing mandates are performative without enforcement). Active enforcement required: zoning boards block dense development, rent control policies oscillate, inclusionary zoning generates minimal supply. Exit options exist (regional coordination, state override) but politically costly due to incumbent homeowner opposition.
constraint_indexing:constraint_classification(housing_affordability_crisis, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: SOCIAL HOUSING SECTOR (PITON) — Historically provided coordination function (municipal housing, housing associations). Theater ratio elevated (0.62+): public programs operate at loss, require subsidy, generate political backlash from incumbent owners. Primary function (provide affordable supply) has atrophied in most markets; constraints persist through institutional inertia and rhetorical commitment without resource allocation. Sunset mechanism never triggered: programs persist as symbolic gesture rather than functional alternative, blocking the genuine market-based or large-scale coordinated solutions that might actually compete with private extraction.
constraint_indexing:constraint_classification(housing_affordability_crisis, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ORGANIZED RENTERS (TANGLED ROPE) — Organized agents (tenant unions, housing justice movements, advocacy coalitions) perceive the constraint as hybrid: genuine coordination problems (matching renters to units, maintaining housing stock, spreading risk) coexist with extraction mechanisms (rent capture, displacement, speculation). Mobile exit options through coalition building, political organizing, and geographic mobility across regions with different regulatory regimes. Classify as tangled rope because the constraint both enables and extracts: without housing markets, no matching function; with current extraction mechanisms, matching fails for low-income agents.
constraint_indexing:constraint_classification(housing_affordability_crisis, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 9: POLICY INNOVATORS (SCAFFOLD) — Organized communities (Vienna housing model, Singapore public housing, Portland zoning reform movements) perceive temporary coordination failure with structural sunset. See extraction mechanism as contingent on specific regulatory choices (restrictive zoning, mortgage-centric finance, property tax dependence) that can be reformed. Mobile exit options through policy redesign, regional demonstration effects, and institutional diffusion. Theater is moderate (policy discourse is partly performative) but functional alternatives exist with measurable lower extraction. Sunset mechanism: as alternative models scale and legitimize, incumbent extraction mechanisms lose political cover.
constraint_indexing:constraint_classification(housing_affordability_crisis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal scope, housing affordability crisis appears inherent to the human condition: land is scarce, population concentrates, demand exceeds supply, some form of allocation mechanism becomes necessary, and any allocation creates stratification. This perspective risks naturalizing contingent institutional arrangements (zoning restrictions, mortgage financialization, property-tax-dependent governance) as immutable laws of land scarcity. The engine's false summit detector will classify this as incorrect: the structural data shows high suppression (0.68) and measurable extraction (0.58) driven by policy choices, not physical limits.
constraint_indexing:constraint_classification(housing_affordability_crisis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(housing_affordability_crisis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(housing_affordability_crisis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(housing_affordability_crisis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(housing_affordability_crisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(housing_affordability_crisis, TR),
    TR >= 0.70.

:- end_tests(housing_affordability_crisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The affordability crisis is clearly extractive — trapped renters experience maximal extraction, and systematic pricing above development cost suggests speculative capture. However, the value is not extreme (snare would be ≥0.66) because some legitimate coordination function remains: housing markets do allocate units, maintain stock, and provide settlement mechanism. The extraction mechanism is contingent on policy choices (zoning, tax structure, mortgage leverage limits, speculation regulation), not purely material. Measured over 30 years, extractiveness has drifted upward from 0.35 to 0.58, indicating rent-seeking layering onto coordination. Suppression (0.68): Very high. Multiple barriers lock victims in place: income-supply gap (material), zoning restrictions (regulatory), mortgage financing structure (institutional), incumbent voter lock (political), and identity fusion with homeownership (cognitive). Renters cannot exit through market mechanisms; first-time buyers cannot exit through identity reframing within biographical horizon; middle-income households cannot exit without catastrophic cost. High suppression is appropriate — victims have minimal exit capacity across multiple dimensions. Theater ratio (0.45): Moderate-low. Policy discourse is substantial (zoning reform proposals, affordable housing mandates, rent control debate) but disconnected from functional outcomes — most policy produces symbolic gestures without supply. Theater could rise if policy substitutes for actual mechanism. The value reflects that some real transactions occur (not pure theater like credentials) but policy layer is largely performative.
 *
 * PERSPECTIVAL GAP:
 *   The renter-beneficiary gap is maximal: renters experience snare (chi ≥ 0.66, maximum extraction) while investors experience rope (chi ≤ 0.35, coordination). This gap is not a measurement error — it reflects genuine structural asymmetry. At global scope, the gap widens (σ(S) = 1.2 amplifies chi for high-d renter perspective, dampens it for low-d investor perspective). The middle-income perspective (moderate/constrained) produces tangled_rope because their exit cost is high but not prohibitive; they experience extraction but retain some leverage. The municipal government perspective is critical: despite institutional status, they are structurally constrained by property-tax dependence and incumbent voter lock, producing d ≈ 0.55 (symmetric) rather than low-d beneficiary. This explains why municipal governments cannot simply solve affordability — to do so would require defying their dominant electoral bloc, which would be career-ending for politicians. The constraint locks even powerful institutional actors in place. The piton perspective (public housing) reveals institutional degradation: a formerly functional coordination mechanism (public housing provision) has become theater because it cannot compete with private extraction under current policy. The sunset mechanism is blocked by lack of political will and resource allocation. The scaffold perspective (policy innovators) sees the constraint as temporary because the extraction mechanism depends on specific policy choices — zoning reform, mortgage regulation, speculation taxes, public housing investment — that are technically implementable. The mountain perspective is false: the analytical observer naturalizes land scarcity, but cross-national data proves affordability is achievable at scale under alternative institutions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from each agent's structural relationship to the extraction flow. Renters (powerless/trapped) have d ≈ 0.95 (full targets): income insufficient for ownership, cannot escape constraint, extraction runs entirely toward them. First-time buyers (powerless/identity_locked) have d ≈ 0.89 (high target): structurally mobile but identity-locked; the identity frame prevents exercising the structural mobility, functionally equivalent to trapped. Property investors (institutional/arbitrage) have d ≈ 0.15 (near beneficiary): capital mobility gives arbitrage options, extraction flows toward them. Existing homeowners (institutional/arbitrage) have d ≈ 0.12 (beneficiary): same power level as investors, similar arbitrage options, but deeper entrenchment in single-market leverage. Municipal governments (institutional/constrained) have d ≈ 0.55 (roughly symmetric): provide genuine coordination but also extract through zoning restrictions; high exit cost (political backlash from incumbent voters) makes them constrained despite institutional status. Tenant unions (organized/mobile) have d ≈ 0.60 (moderate target): organized agents bear extraction but have exit capacity through coalition building and policy influence. The chi formula χ = ε × f(d) × σ(S) amplifies extraction for high-d agents (renters, trapped buyers) and dampens it for low-d beneficiaries (investors, existing owners), producing the observed perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: The constraint resolves the mandatrophy by decomposing into its constituent mechanisms. At base, housing affordability appears to be a pure extraction snare (prices exceed construction cost, trapped renters, no exit). But structural analysis reveals genuine coordination functions: matching supply to demand, spreading settlement risk, maintaining neighborhoods, enabling permanent settlement. These functions are not mere cover stories — housing markets do solve coordination problems that would be harder to solve otherwise. The tangled_rope classification captures this: the constraint both coordinates and extracts. The extractiveness (0.58) reflects that extraction is substantial but not total; the suppression (0.68) reflects that victims are locked in place; the theater ratio (0.45) reflects that policy discourse acknowledges problems but solutions remain performative. The mandatrophy resolves by recognizing that the constraint cannot be eliminated without destroying the coordination function. The solution is institutional redesign that preserves coordination while removing extraction: zoning reform (expand supply), mortgage regulation (reduce leverage), speculation taxes (reduce rent capture), public housing investment (provide alternatives), property tax reform (reduce incumbent bias). These reforms target the extraction mechanism specifically, not the coordination function. Cross-national examples (Vienna, Singapore, Tokyo) demonstrate this is achievable — affordability and coordination are compatible, current extraction mechanisms are not inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_scarcity_vs_policy_choice,
    'How much of the affordability crisis is driven by absolute land scarcity vs. contingent regulatory choices (zoning, financialization, tax policy)?',
    'Cross-national comparison of housing affordability vs. housing density, zoning density regulations, mortgage leverage ratios, and property tax structures. Vienna, Singapore, and Tokyo data show dense, affordable housing is achievable under different policy regimes.',
    'If primarily scarcity-driven: constraint is more mountain-like, extraction less addressable. If primarily policy-driven: constraint is tangled_rope, extraction is remediable through institutional redesign.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(land_scarcity_vs_policy_choice, empirical, 'Attribution of affordability crisis to land scarcity vs. regulatory constraints').

omega_variable(
    incumbent_homeowner_electoral_lock,
    'Is municipal zoning restriction maintained by genuine electoral preference for low-density or by information asymmetry and incumbent bias?',
    'Randomized preference surveys; comparison of stated vs. revealed preferences for zoning reform; analysis of political opposition timing relative to property value expectations.',
    'If genuine preference: regulatory constraint becomes democratically legitimate (identity_locked coalition). If information-dependent: constraint is maintained by suppression of alternatives and asymmetric information; exit potential exists if framing changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_homeowner_electoral_lock, empirical, 'Whether incumbent homeowner zoning preference is authentic or information-dependent').

omega_variable(
    rental_suppression_mechanism_origin,
    'Is suppression of renter exit capacity primarily structural (income insufficient for ownership) or institutional (rental market governance creating artificial scarcity)?',
    'Income-to-rent ratio analysis controlling for housing supply elasticity; comparison of rental markets with varying regulation (rent control, eviction protection, speculation limits); longitudinal tracking of renter wealth accumulation vs. historical periods with lower ownership barriers.',
    'If primarily income-structural: constraint approaches snare (structural trap). If institutional: constraint is tangled_rope with addressable mechanisms. If both: suppression is internalized (renters believe ownership is impossible) even where structural barriers are lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rental_suppression_mechanism_origin, empirical, 'Whether renter suppression is income-structural or institutionally maintained').

omega_variable(
    speculative_extraction_vs_legitimate_return,
    'What portion of housing cost inflation is speculative capture vs. legitimate return on development and maintenance?',
    'Price decomposition analysis: construction cost + land value + financing cost + investor profit margin; comparison of markets with varying speculation tax rates; historical pricing data for speculation booms vs. regulation periods.',
    'High speculative component confirms snare component (pure extraction). Low speculative component suggests rope (legitimate coordination). Ambiguity between them drives classification uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speculative_extraction_vs_legitimate_return, empirical, 'Decomposition of housing cost into legitimate return vs. speculative extraction').

omega_variable(
    public_housing_functionality_collapse,
    'Is public housing sector degradation (piton classification) due to genuine inability to operate affordably or political defunding and stigmatization?',
    'Cost accounting for public housing vs. private comparable; analysis of rent-setting policies (below-market mandates); comparison of well-funded public systems (Vienna, Singapore) vs. defunded US public housing.',
    'If genuine cost problem: public housing cannot compete (constraint is rope or snare). If political defunding: sunset mechanism could be triggered by policy commitment; constraint is scaffold, not piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_housing_functionality_collapse, preference, 'Whether public housing sector collapse is functional or political').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(housing_affordability_crisis, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hac_tr_t0, housing_affordability_crisis, theater_ratio, 0, 0.32).
narrative_ontology:measurement(hac_tr_t10, housing_affordability_crisis, theater_ratio, 10, 0.4).
narrative_ontology:measurement(hac_tr_t20, housing_affordability_crisis, theater_ratio, 20, 0.45).
narrative_ontology:measurement(hac_tr_t30, housing_affordability_crisis, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(hac_be_t0, housing_affordability_crisis, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hac_be_t10, housing_affordability_crisis, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hac_be_t20, housing_affordability_crisis, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(hac_be_t30, housing_affordability_crisis, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(housing_affordability_crisis, resource_allocation).
narrative_ontology:boltzmann_floor_override(housing_affordability_crisis, 0.18).
narrative_ontology:affects_constraint(housing_affordability_crisis, zoning_capture_mechanism).
narrative_ontology:affects_constraint(housing_affordability_crisis, mortgage_financialization).
narrative_ontology:affects_constraint(housing_affordability_crisis, incumbent_voter_lock).
narrative_ontology:affects_constraint(housing_affordability_crisis, land_speculation_extraction).

% DUAL FORMULATION NOTE:
% Housing affordability crisis decomposes into four structurally distinct constraints: (1) zoning_capture_mechanism (ε=0.52, tangled_rope) — zoning boards restrict supply while coordinating neighborhood stability; (2) mortgage_financialization (ε=0.64, snare) — leverage amplifies asset capture; (3) incumbent_voter_lock (ε=0.45, tangled_rope with identity_locked component) — existing homeowners form electoral majority supporting restrictions; (4) land_speculation_extraction (ε=0.68, snare) — pure extraction layer above legitimate property returns. Each story has its own perspectives, beneficiary/victim declarations, and measurements. Housing_affordability_crisis links all four as upstream constraint shared by distinct extractive mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(housing_affordability_crisis, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
