% ============================================================================
% CONSTRAINT STORY: real_estate_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_real_estate_extraction, []).

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
 *   constraint_id: real_estate_extraction
 *   human_readable: Real Estate Extraction Through Financialization and Rent Capture
 *   domain: economic/political/housing
 *
 * SUMMARY:
 *   Real estate extraction operates through a hybrid coordination-extraction
 *   mechanism that has intensified over the past two decades through
 *   financialization. The constraint solves a genuine coordination problem —
 *   aggregating dispersed capital to finance housing — while simultaneously
 *   extracting wealth through rent capture, mortgage origination fees,
 *   speculative gains, and tax arbitrage. The mechanism's enforcement relies
 *   on property law, debt collection infrastructure, and zoning restrictions.
 *   Theater ratio has risen over the interval as speculative capital flows
 *   (rather than housing needs) increasingly drive allocation decisions. The
 *   constraint exhibits a perspectival range from pure extraction (trapped
 *   tenants), through hybrid mechanisms (first-time buyers, institutional
 *   landlords), to policy scaffolds (rent control, eviction protection), to
 *   false naturalization (property rights abstraction). The extractiveness
 *   trajectory shows systematic increase from 0.35 (1990s deregulation
 *   baseline) to 0.58 (post-2008 institutional concentration), driven by
 *   mortgage securitization, institutional investor scale, zoning-constrained
 *   supply, and tax code asymmetries favoring property owners.
 *
 * KEY AGENTS:
 *   - Residential Tenants: Primary victim (powerless/trapped) — rent-burdened, geographically immobile, high suppression from eviction law and market concentration
 *   - First-Time Homebuyers: Secondary victim (moderate/constrained) — high transaction costs, debt-service risk, but benefit from equity building in appreciating markets
 *   - Institutional Landlords (REITs, large property companies): Primary beneficiary (institutional/arbitrage) — capture rent income, appreciation, and tax arbitrage; low exit costs due to portfolio depth
 *   - Real Estate Finance Sector (banks, mortgage originators, securitization platforms): Primary beneficiary (institutional/constrained) — extract through origination fees, securitization spreads, prepayment penalties; constrained by regulatory lock-in and portfolio concentration
 *   - Working-Class Coalition (tenant unions, labor organizations): Secondary actor (organized/mobile) — organized power but face enforcement asymmetry and displacement risk
 *   - Housing Policy Coalitions (municipal governments, housing advocates): Scaffolding actor (organized/constrained) — provide temporary extraction suppression through rent control, eviction protection, community land trusts
 *   - Property Rights Abstraction (legal/conceptual): Institutional actor (institutional/arbitrage) — maintains performative enforcement of absolute property rights; theaters coordination benefits that increasingly accrue to speculation rather than housing
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing financialized extraction as inherent scarcity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(real_estate_extraction, 0.58).
domain_priors:suppression_score(real_estate_extraction, 0.62).
domain_priors:theater_ratio(real_estate_extraction, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(real_estate_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(real_estate_extraction, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(real_estate_extraction, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(real_estate_extraction, tangled_rope).
narrative_ontology:human_readable(real_estate_extraction, "Real Estate Extraction Through Financialization and Rent Capture").
narrative_ontology:topic_domain(real_estate_extraction, "economic/political/housing").

domain_priors:requires_active_enforcement(real_estate_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(real_estate_extraction, institutional_landlords).
narrative_ontology:constraint_beneficiary(real_estate_extraction, real_estate_financiers).
narrative_ontology:constraint_beneficiary(real_estate_extraction, property_developers).
narrative_ontology:constraint_victim(real_estate_extraction, residential_tenants).
narrative_ontology:constraint_victim(real_estate_extraction, first_time_homebuyers).
narrative_ontology:constraint_victim(real_estate_extraction, working_class_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED TENANT (SNARE) — Rent-burdened households with no exit option. High housing costs constrain other spending, limit geographic mobility for employment, and create debt-dependent cycles. Suppression is extreme: relocation cost, credit barriers, geographic job market concentration, and gentrification of affordable areas. Zero agency.
constraint_indexing:constraint_classification(real_estate_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FIRST-TIME HOMEBUYER (TANGLED ROPE) — Constrained by down payment requirements, debt-to-income limits, and geographic wage differentials. The mortgage system coordinates housing access (genuine function) with asymmetric extraction through origination fees, closing costs, prepayment penalties, and interest rate discrimination. Buyers benefit from wealth building through equity but bear disproportionate transaction costs relative to institutional buyers.
constraint_indexing:constraint_classification(real_estate_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL LANDLORD (ROPE) — Benefits from rent income, property appreciation, and tax arbitrage (depreciation deductions, 1031 exchanges). Experiences the constraint as coordination: rental markets aggregate dispersed capital into consolidated property management. Extraction runs toward this agent; the system's enforcement (eviction law, debt collection, tax code) subsidizes their position.
constraint_indexing:constraint_classification(real_estate_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WORKING-CLASS COALITION (TANGLED ROPE) — Organized labor and tenant unions have genuine mobility (strike capacity, political organization) but face enforcement asymmetry: landlord litigation is subsidized by law, while tenant organizing risks retaliation and displacement. The constraint coordinates labor access to housing with asymmetric extraction through rent-seeking. Organized power moderates chi below snare threshold.
constraint_indexing:constraint_classification(real_estate_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PROPERTY RIGHTS ABSTRACTION (PITON) — The legal fiction of absolute property rights (ownership, alienability, securitization) is largely performative at civilizational scale. The constraint persists through institutional inertia: property law was designed for agrarian societies and persists in financialized real estate despite no longer serving its original coordination function. Theater ratio rises as actual housing allocation is driven by speculative capital flows, not property law's stated rationale. The abstraction has atrophied.
constraint_indexing:constraint_classification(real_estate_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HOUSING POLICY COALITION (SCAFFOLD) — Municipal rent control, community land trusts, social housing programs, and eviction moratoriums are temporary mechanisms with explicit sunset or contingency clauses. They coordinate housing access while suppressing speculation-driven extraction. Policy exists precisely because the market constraint extracts; policy has an inherent sunset (either replaced by structural reform or degrading back to market extraction). Theater ratio is low — these mechanisms attempt genuine function, not ritual.
constraint_indexing:constraint_classification(real_estate_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: REAL ESTATE FINANCE SECTOR (TANGLED ROPE) — Institutional actor (banks, REITs, investment funds) coordinating capital allocation to housing (genuine function) while extracting through securitization, mortgage origination margins, and maturity transformation. Constrained exit because mortgage portfolios create regulatory and reputational lock-in. Beneficiary with asymmetric power over extraction terms.
constraint_indexing:constraint_classification(real_estate_extraction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — Risk of naturalizing the constraint as an immutable law: 'Land is scarce, housing is a commodity, markets allocate efficiently.' This perspective sees extraction as inherent to supply-demand mechanics. However, the structural data reveals this as false naturalization: extraction mechanisms (financing, zoning, speculation) are contingent institutional arrangements, not physical laws. The engine flags this as a false summit.
constraint_indexing:constraint_classification(real_estate_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(real_estate_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(real_estate_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(real_estate_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(real_estate_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(real_estate_extraction, TR),
    TR >= 0.70.

:- end_tests(real_estate_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The real estate system extracts through multiple channels: (1) Rent extraction — landlords capture value from property appreciation and scarcity rents, with suppression preventing tenant exit; (2) Mortgage origination — lenders extract through fees, interest rate spreads, and securitization margins; (3) Speculative gains — institutional investors extract through buy-and-hold and price appreciation. The value is high but not extreme (snare level ≥0.66) because some genuine coordination function persists: mortgages do finance housing construction, property management does aggregate maintenance, and capital markets do allocate resources to construction. The extraction is not the constraint's only function. Suppression (0.62): High. Barriers to exit include: relocation costs (typically 8-10% of property value), geographic employment concentration, credit barriers (speculative foreclosures reduce access), zoning rents (legal restrictions reduce supply), and eviction law asymmetry (landlord litigation is subsidized, tenant litigation is expensive). Suppression has increased over the interval as zoning has become more restrictive and institutional landlords have scaled eviction operations. Theater ratio (0.48): Moderate. Property rights rhetoric frames the constraint as natural law and fair exchange, but actual allocation is increasingly driven by speculative capital flows rather than genuine coordination. The rise in theater ratio from 0.32 to 0.48 reflects growing disconnect between property law's stated function (secure housing access) and actual outcome (speculative asset vehicles). Policy interventions (rent control, eviction protection) have lower theater — they explicitly acknowledge extraction and attempt to suppress it, rather than pretending it doesn't exist.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how the same structural mechanism (property financialization) appears as coordinated housing access from beneficiary perspectives and pure extraction from victim perspectives. The trapped tenant sees maximum extraction with no coordination benefit. The first-time buyer sees genuine coordination (mortgage enables access) hybrid with extraction (fees, rate discrimination). The institutional landlord sees pure coordination — the system's enforcement mechanisms subsidize their position. The policy coalition sees a temporary problem (scaffold) solvable through intervention. The property rights abstraction sees naturalized law (piton, degraded ritual). The analytical observer risks seeing immutable scarcity (false mountain). The perspectival gap is not ambiguity — it is a diagnostic signal that the constraint's extraction function is active and unevenly distributed. If all perspectives produced the same type, extraction would be symmetric (rope). The gap reveals extraction asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's effective extraction (χ) is computed from: beneficiary/victim status × exit options × power level. Trapped tenants have no exit and bear all costs (high d, high f(d)). Institutional landlords have multiple exits and capture benefits (low d, negative f(d)). First-time buyers have moderate exit costs and mixed benefit (medium d, medium f(d)). Organized workers have strike power and can negotiate (medium-high d but reduced by organized power). The key insight: the same base extractiveness (ε=0.58) scales differently for each agent because their exit options and power levels differ. Scope scaling then applies: institutional landlords with global portfolio arbitrage experience amplified effective extraction (σ=1.2 for global scope); tenants with local housing markets experience compressed extraction (σ=0.8 for local scope, reduced verification difficulty). The directionality overrides are not needed — the base derivation from beneficiary/victim + exit + power captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The real estate extraction constraint exhibits classical mandatrophy — it is classified as tangled_rope (hybrid coordination-extraction) in the primary perspective (institutional landlord) but appears as snare (pure extraction) from trapped tenant perspective. The resolution hinges on three omega variables: (1) Does institutional landlordism provide genuine coordination function, or is scale only benefiting landlords? (2) Do mortgages genuinely build wealth for borrowers, or are fees and interest extraction-heavy? (3) Are housing policy scaffolds genuinely temporary with sunset clauses, or do they become permanent suppression mechanisms that mask underlying extraction? If omegas resolve in favor of coordination, the tangled_rope classification holds and mandatrophy is resolved by showing that extraction is real but partial. If omegas resolve in favor of pure extraction, multiple perspectives should reclassify as snare, and the constraint becomes dominated by extraction rather than coordination. Current status: ACTIVE MANDATROPHY. The constraint cannot be finalized as tangled_rope until the omega variables about institutional landlord coordination and mortgage benefit are resolved through empirical analysis. The false mountain perspective (analytical observer naturalizing scarcity) should be flagged by the engine's false summit detector.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    land_scarcity_vs_financial_scarcity,
    'Is elevated housing costs driven by land scarcity or by financialization-induced artificial scarcity (speculation, underutilization, zoning rents)?',
    'Comparative analysis: vacancy rates, land use efficiency, speculative holding periods, and correlations between construction constraints (zoning) and price growth across jurisdictions',
    'If land scarcity: extraction is moderate (mountain-adjacent). If financial scarcity: extraction is severe (snare confirmed). This distinction changes whether the constraint is immutable or policy-contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(land_scarcity_vs_financial_scarcity, empirical, 'Whether housing costs reflect land scarcity or financialization').

omega_variable(
    debt_servitude_vs_wealth_building,
    'For mortgaged homeowners, does the wealth-building function of home equity ownership outweigh or undercompensate for extraction through interest, origination fees, and prepayment penalties?',
    'Longitudinal household data: net wealth accumulation from primary residence ownership vs total interest and fees paid; comparison with equivalent renting + index fund investing over same time horizon',
    'If wealth building dominates: tangled rope classification for first-time buyers is correct. If extraction dominates: reclassify as snare. This affects whether the constraint coordinates or purely extracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_servitude_vs_wealth_building, empirical, 'Whether mortgage extraction is compensated by wealth building').

omega_variable(
    policy_sunset_realism,
    'Are housing policy interventions (rent control, eviction moratoriums, community land trusts) genuinely temporary scaffolds or permanent forms of market suppression that mask underlying extraction?',
    'Historical analysis: policy duration, renewal rates, repeal triggers, policy effectiveness data (does intervention reduce extraction or just delay it?), and structural analysis of whether policy addresses root causes or symptoms',
    'If genuinely temporary: scaffold classification holds. If permanent: policies become a second constraint layer (regulatory extraction on top of market extraction), requiring decomposition into separate constraint stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_sunset_realism, empirical, 'Whether housing policy is temporary scaffold or permanent intervention').

omega_variable(
    institutional_landlord_coordination_function,
    'What genuine coordination function, if any, does institutional landlordism provide? Is the benefit real coordination or theatrical justification for rent extraction?',
    'Comparative analysis: maintenance costs, vacancy rates, and service quality for institutional vs independent landlords; whether scale economics benefit tenants or only landlords; whether coordination role could be performed by alternative models (co-ops, municipal housing, land trusts)',
    'If genuine coordination: institutional landlord perspective (rope) is correct. If theatrical: reclassify as snare, not rope. This determines whether the constraint is hybrid or pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_landlord_coordination_function, empirical, 'Whether institutional landlordism provides genuine coordination function').

omega_variable(
    mortgage_securitization_necessity,
    'Does mortgage securitization provide genuine liquidity benefits to borrowers (lower interest rates, broader access) or is it primarily an extraction mechanism that benefits financiers while shifting risk to borrowers?',
    'Historical comparison: mortgage interest rates and lending standards before/after securitization; analysis of whether securitization increased total lending volume or just transferred origination profits to finance sector; modeling of interest rate elasticity with respect to securitization market depth',
    'If genuinely beneficial: securitization is coordination (rope). If primarily extractive: it is snare, and the real estate finance sector perspective should reclassify. This affects whether mortgages are tangled rope or pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mortgage_securitization_necessity, empirical, 'Whether mortgage securitization benefits borrowers or financiers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(real_estate_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reex_tr_t0, real_estate_extraction, theater_ratio, 0, 0.32).
narrative_ontology:measurement(reex_tr_t10, real_estate_extraction, theater_ratio, 10, 0.4).
narrative_ontology:measurement(reex_tr_t20, real_estate_extraction, theater_ratio, 20, 0.48).
narrative_ontology:measurement(reex_tr_t5, real_estate_extraction, theater_ratio, 5, 0.36).

% Extraction over time
narrative_ontology:measurement(reex_be_t0, real_estate_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(reex_be_t10, real_estate_extraction, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(reex_be_t20, real_estate_extraction, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(reex_be_t5, real_estate_extraction, base_extractiveness, 5, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(real_estate_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(real_estate_extraction, 0.18).
narrative_ontology:affects_constraint(real_estate_extraction, mortgage_securitization).
narrative_ontology:affects_constraint(real_estate_extraction, zoning_extraction).
narrative_ontology:affects_constraint(real_estate_extraction, tax_code_property_asymmetry).
narrative_ontology:affects_constraint(real_estate_extraction, eviction_law_enforcement).

% DUAL FORMULATION NOTE:
% Real estate extraction is a parent constraint that affects multiple downstream mechanisms. Mortgage securitization operates at the finance layer; zoning extraction operates at the regulatory layer; tax code asymmetry operates at the fiscal layer; eviction law operates at the enforcement layer. Each downstream constraint has its own ε value reflecting its specific extraction mechanism. Real estate extraction represents their structural coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(real_estate_extraction, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
