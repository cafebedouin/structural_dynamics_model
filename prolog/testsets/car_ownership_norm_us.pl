% ============================================================================
% CONSTRAINT STORY: car_ownership_norm_us
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_car_ownership_norm_us, []).

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
 *   constraint_id: car_ownership_norm_us
 *   human_readable: The Norm of Individual Car Ownership in the US
 *   domain: economic/social_infrastructure
 *
 * SUMMARY:
 *   Dean Baker's analysis identifies car ownership as a constructed norm in
 *   the United States that functions as an extractive constraint on
 *   low-income and transit-dependent populations. The constraint emerges from
 *   a system of reinforcing institutional choices: zoning laws that mandate
 *   sprawl, highway-centric infrastructure investment, parking minimums,
 *   gasoline tax insufficiency, and cultural messaging that conflates car
 *   ownership with freedom and autonomy. This system extracts wealth from
 *   those who cannot afford to live near employment (forcing long commutes),
 *   subsidizes auto manufacturers and fossil fuel industries through tax
 *   policy and infrastructure spending, and naturalizes this arrangement as
 *   inevitable given American geography. From the perspective of a
 *   transit-dependent worker or low-income suburban parent, car ownership is
 *   not a choice but a trapped necessity. From the perspective of auto
 *   manufacturers and oil companies, the constraint is a coordination
 *   mechanism that stabilizes demand. The theater ratio has increased over
 *   the interval as the performance of 'freedom of choice' has intensified
 *   even as real alternatives have diminished.
 *
 * KEY AGENTS:
 *   - Transit-Dependent Workers: Primary victims (powerless/trapped) — forced into car ownership despite high cost; no viable alternatives
 *   - Suburban Parents: Secondary victims (moderate/constrained) — normalized car dependency for family logistics; constrained alternatives for activity scheduling
 *   - Automotive Manufacturers: Primary beneficiaries (institutional/arbitrage) — stable demand stream for vehicles, maintenance, replacement cycles; experience constraint as coordination benefit
 *   - Fossil Fuel Industry: Primary beneficiaries (institutional/arbitrage) — normalized gasoline consumption; benefit from car-centric zoning and highway spending
 *   - Highway Construction Contractors: Secondary beneficiaries (institutional/arbitrage) — recurring infrastructure projects; benefit from underfunded transit alternatives
 *   - Auto Finance Lenders: Secondary beneficiaries (institutional/arbitrage) — debt-financed vehicle purchases; profit from high-cost ownership structure
 *   - Urban Transit Advocates: Mixed actors (organized/constrained) — benefit from coordination frameworks; victimized by extraction of transit funding and developable land
 *   - Suburban Planning System: Institutional persistence (institutional/arbitrage) — maintains zoning and parking mandate architecture through inertia; sees degraded original function
 *   - Environmental Commons: Victim (analytical/analytical) — bears costs of emissions, sprawl, habitat loss, soil sealing; diffuse and unorganized; no negotiating capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(car_ownership_norm_us, 0.58).
domain_priors:suppression_score(car_ownership_norm_us, 0.68).
domain_priors:theater_ratio(car_ownership_norm_us, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(car_ownership_norm_us, extractiveness, 0.58).
narrative_ontology:constraint_metric(car_ownership_norm_us, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(car_ownership_norm_us, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(car_ownership_norm_us, snare).
narrative_ontology:human_readable(car_ownership_norm_us, "The Norm of Individual Car Ownership in the US").
narrative_ontology:topic_domain(car_ownership_norm_us, "economic/social_infrastructure").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(car_ownership_norm_us, automotive_manufacturers).
narrative_ontology:constraint_beneficiary(car_ownership_norm_us, fossil_fuel_industry).
narrative_ontology:constraint_beneficiary(car_ownership_norm_us, highway_construction_contractors).
narrative_ontology:constraint_beneficiary(car_ownership_norm_us, auto_finance_lenders).
narrative_ontology:constraint_victim(car_ownership_norm_us, transit_dependent_populations).
narrative_ontology:constraint_victim(car_ownership_norm_us, low_income_workers).
narrative_ontology:constraint_victim(car_ownership_norm_us, urban_renters).
narrative_ontology:constraint_victim(car_ownership_norm_us, environmental_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANSIT-DEPENDENT WORKER (SNARE) — Structurally locked into car ownership despite high financial cost. Public transit is systematically underfunded, zoning disperses employment, and social stigma attaches to non-car transportation. The worker has no exit: car ownership is functionally mandatory, subsidizes auto industry profits, and extracts disposable income and debt obligation. Experiences maximum suppression (trapped exit) with zero alternatives.
constraint_indexing:constraint_classification(car_ownership_norm_us, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUBURBAN PARENT (SNARE) — Constrained but not fully trapped. Car ownership is normalized as essential for family logistics (school runs, grocery access, children's activities). Transit-accessible housing is expensive; car-dependent zoning is the norm. The parent experiences high extraction (vehicle purchase, fuel, maintenance, insurance) with constrained alternatives — one child's activity schedule requires vehicle coordination that public transit cannot support.
constraint_indexing:constraint_classification(car_ownership_norm_us, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AUTOMOTIVE MANUFACTURER (ROPE) — Benefits from the constraint as coordination: the norm ensures stable demand for vehicles, financing, maintenance, fuel, and replacement cycles. The manufacturer experiences this as the coordination function that justifies capital investment in production. The constraint subsidizes profitability. Net beneficiary — experiences the constraint as coordination benefit, not extraction.
constraint_indexing:constraint_classification(car_ownership_norm_us, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: URBAN TRANSIT ADVOCATES (TANGLED ROPE) — Organized actors (transit agencies, urban planning bodies, climate coalitions) see mixed structure: the norm creates genuine coordination problems (congestion, land-use inefficiency) that require collective action, but the constraint also extracts from them (underfunded transit budgets, land devoted to parking and highways instead of housing or community space). They benefit from coordination frameworks but are victimized by extraction toward auto industry.
constraint_indexing:constraint_classification(car_ownership_norm_us, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SUBURBAN PLANNING SYSTEM (PITON) — Zoning laws, parking minimums, and highway-centric infrastructure planning originally served a coordination function (efficient movement of goods and people post-WWII). These institutional forms now persist through inertia despite degraded function: they create sprawl, increase housing costs, reduce walkability, and reinforce car dependency. The theater ratio is high — planning meetings perform responsiveness to growth while infrastructure decisions remain predetermined by car-centric assumptions. The original functional purpose (rapid suburbanization) has atrophied; institutional forms persist.
constraint_indexing:constraint_classification(car_ownership_norm_us, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GEOGRAPHIC REALISM (FALSE SUMMIT) — From a civilizational perspective, this perspective risks naturalizing car dependency as inherent to American geography and scale: vast distances, dispersed settlement patterns, and low population density supposedly make individual car ownership inevitable. However, this naturalizes what is actually contingent institutional choice. Countries with equivalent or greater land areas (Australia, Canada) have major transit-accessible urban cores; US policy choices drove sprawl and highway-centric zoning rather than inevitable geography.
constraint_indexing:constraint_classification(car_ownership_norm_us, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(car_ownership_norm_us_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(car_ownership_norm_us, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(car_ownership_norm_us, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(car_ownership_norm_us, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(car_ownership_norm_us, TR),
    TR >= 0.70.

:- end_tests(car_ownership_norm_us_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint extracts significant wealth from trapped agents (vehicle payments, fuel, maintenance, insurance costs) while subsidizing beneficiaries through tax policy (highway spending exceeds gas tax revenue by ~$25B annually), zoning-protected monopoly rents for auto manufacturers, and externalized costs (congestion, pollution, accidents). The extraction is less than a pure monopoly snare (0.75+) because some agents retain agency through geographic choice or alternative transit in select cities. Suppression (0.68): High. Structural barriers to alternatives include: zoning that disperses employment, parking mandates that make transit-accessible housing expensive, underfunded transit (order of magnitude lower per capita investment than car infrastructure), social stigma, and gasoline pricing that does not include full infrastructure/environmental costs. Agents cannot easily relocate to transit-accessible areas (cost barrier), cannot reliably use transit (frequency/coverage gaps), and face social pressure to conform. Theater ratio (0.55): Moderate-high. The constraint maintains significant performative element: political messaging around 'freedom of choice' and 'American mobility' masks the systematic underfunding of alternatives; suburban planning processes perform responsiveness while zoning and parking mandates are predetermined; car culture mythology ('the open road') obscures the spatial and financial lock-in. However, the extractive mechanism (forced consumption) is structural and real, not purely theatrical.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows a clear perspectival fissure between beneficiaries and victims. The automotive manufacturer and oil company see Rope — a stable, legitimate coordination function that solves the transportation problem and justifies capital investment. The transit-dependent worker sees Snare — trapped in costly, mandatory consumption with no exit. The organized transit advocates see Tangled Rope — the system creates both coordination problems (congestion, inefficiency) and extraction (underfunded transit). The suburban planning system sees Piton — the original postwar function (rapid suburban expansion) has atrophied, but zoning and parking mandates persist through institutional inertia. The geographic naturalization view (analytical) risks a false Mountain — the constraint appears inevitable given American scale and dispersal, but structural evidence shows it is policy-contingent. The perspectival range (Snare → Rope → Tangled Rope → Piton → false Mountain) demonstrates how a single institutional arrangement produces radically different experienced constraints depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from agent power, exit options, and beneficiary/victim status. Transit-dependent workers: powerless + trapped + victim status → d ≈ 0.95 → high f(d) → high experienced extraction chi. Automotive manufacturers: institutional + arbitrage + beneficiary status → d ≈ 0.05 → low/negative f(d) → negative/neutral experienced extraction (benefits). Urban transit advocates: organized + constrained + both beneficiary (coordination) and victim (extraction) status → d ≈ 0.50 → moderate f(d) → moderate experienced extraction (mixed). Suburban planning system: institutional + arbitrage + beneficiary status (for legacy zoning/highway interests) → d ≈ 0.10 → low f(d). The scope modifier (national, σ=1.0) scales all chi values uniformly across perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare classification resolves the mandatrophy by demonstrating that the constraint is not a natural coordination mechanism but a hybrid with dominant extraction. The beneficiary perspective (Rope) is empirically false when contextualized: the 'coordination' function (transportation) could be achieved more efficiently through multimodal alternatives that distribute benefits more widely. The snare classification reveals that the coordination framing masks extraction — the beneficiaries (auto industry, oil companies) have economic interest in maintaining the norm, while costs are borne by those least able to negotiate (low-income, transit-dependent, urban renters). The theater ratio (0.55) confirms degradation: the constraint now maintains itself partly through cultural mythology ('freedom') rather than functional necessity. The analytical mountain perspective is a false summit — geographic and scale arguments for inevitability do not withstand scrutiny (transit-capable peer countries exist). The mandatrophy is resolved by showing that the snare classification is structurally robust: removal of the constraint (shift to multimodal, transit-investment, zoning reform) would reduce extraction and improve overall welfare, but the beneficiaries have sufficient institutional power to prevent this reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transit_feasibility_threshold,
    'What population density and urban form thresholds make transit genuinely infeasible versus merely underfunded by policy choice?',
    'Comparative analysis of US regions with transit investment (Portland MAX, DC Metro, NYC subway) vs peer geographies with equivalent density; identification of density floors for economically viable transit',
    'If threshold is low (<5000/sq mi): most US car dependency is policy-driven, not inherent. If threshold is high (>15000/sq mi): significant US regions genuinely require car dependency, reducing snare classification confidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transit_feasibility_threshold, empirical, 'Population density threshold for economically viable public transit').

omega_variable(
    car_cost_externalization_completeness,
    'How much of the true cost of car ownership (congestion, pollution, accidents, road maintenance, parking subsidy) is externalized versus captured in vehicle prices and fuel costs?',
    'Comprehensive transport cost accounting: comparison of full system costs (user-paid + subsidized + externalized) vs modal alternatives; analysis of gas tax adequacy for road maintenance',
    'If >60% externalized: snare classification is robust (extraction obscured by artificial affordability). If <30% externalized: car ownership may be economically rational, reducing snare confidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(car_cost_externalization_completeness, empirical, 'Proportion of car ownership costs externalized via subsidies and unpriced externalities').

omega_variable(
    zoning_entrenchment_reversibility,
    'Can car-centric zoning and parking mandates be reversed through policy reform, or are they locked in by property rights, capital stock, and political economy?',
    'Case study analysis of zoning reform attempts (Minneapolis YIMBY movement, California SB-9 implementation, parking mandate rollback in major cities); assessment of successful vs failed transitions',
    'If readily reversible: the snare is contingent and has political exit pathways, potentially downgrading to tangled_rope. If locked in: snare classification is robust; structural change requires multi-generational capital turnover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zoning_entrenchment_reversibility, empirical, 'Whether car-centric zoning can be reversed through policy reform').

omega_variable(
    norm_entrenchment_mechanism,
    'Is car ownership norm maintenance primarily driven by infrastructure lock-in (physical capital) or by cultural/social belief propagation?',
    'Analysis of US regions with both transit infrastructure and cultural car preference (San Francisco) vs regions with neither (declining Rust Belt areas); social survey data on car necessity beliefs vs actual availability of alternatives',
    'If primarily infrastructure: reform requires capital replacement (slow, expensive, but possible). If primarily cultural: reform requires values shift (potentially harder). Hybrid suggests different intervention levers for different regions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrenchment_mechanism, conceptual, 'Whether car norm maintenance is driven by infrastructure or cultural entrenchment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(car_ownership_norm_us, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carownorm_tr_t0, car_ownership_norm_us, theater_ratio, 0, 0.3).
narrative_ontology:measurement(carownorm_tr_t25, car_ownership_norm_us, theater_ratio, 25, 0.42).
narrative_ontology:measurement(carownorm_tr_t50, car_ownership_norm_us, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(carownorm_be_t0, car_ownership_norm_us, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(carownorm_be_t25, car_ownership_norm_us, base_extractiveness, 25, 0.47).
narrative_ontology:measurement(carownorm_be_t50, car_ownership_norm_us, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(car_ownership_norm_us, resource_allocation).
narrative_ontology:affects_constraint(car_ownership_norm_us, suburban_zoning_lock_in).
narrative_ontology:affects_constraint(car_ownership_norm_us, gasoline_tax_insufficiency).
narrative_ontology:affects_constraint(car_ownership_norm_us, highway_funding_bias).

% DUAL FORMULATION NOTE:
% Car ownership norm is downstream of three structurally distinct constraints: zoning (controls land-use patterns), gas tax (prices fuel below true cost), and highway funding bias (concentrates infrastructure investment). Each has its own extractiveness value. Car ownership norm integrates these three into a unified extractive system with high suppression and theater. Decomposition into separate stories enables analysis of specific policy levers (zoning reform, carbon pricing, transit investment) while car_ownership_norm_us captures the whole-system constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(car_ownership_norm_us, powerless, 0.95).
constraint_indexing:directionality_override(car_ownership_norm_us, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
