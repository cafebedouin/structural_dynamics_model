% ============================================================================
% CONSTRAINT STORY: device_manufacturer_distribution_models
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_device_manufacturer_distribution_models, []).

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
 *   constraint_id: device_manufacturer_distribution_models
 *   human_readable: Device Manufacturer Distribution Models
 *   domain: economic/industrial/technology
 *
 * SUMMARY:
 *   Device manufacturer distribution models create a structural constraint
 *   that coordinates genuine supply-chain functions (inventory management,
 *   geographic coverage, technical support standardization) alongside
 *   asymmetric extraction (margin capture, price control, market access
 *   gatekeeping). The constraint exhibits perspectival divergence across six
 *   DR types: manufacturers and large retail chains experience it as
 *   coordination, independent retailers as extraction, consumers as
 *   information opacity, and organized platform aggregators as a temporary
 *   problem with visible sunset mechanisms. The extractiveness metric (0.58)
 *   reflects moderate-to-high economic rents captured by manufacturers
 *   through distribution lock-in, while suppression (0.65) captures both
 *   material barriers (exclusive agreements, resale restrictions, warranty
 *   penalties) and institutional inertia preventing alternative channels. The
 *   theater ratio (0.48) indicates that roughly half the enforcement activity
 *   is performative — maintaining dealer standards and geographic exclusivity
 *   through ritual rather than functional necessity, as direct-to-consumer
 *   logistics have matured sufficiently to reduce the genuine coordination
 *   costs that originally justified exclusive distribution. The constraint is
 *   decomposable into separate stories by device category (smartphones show
 *   near-complete direct-to-consumer dominance; industrial equipment remains
 *   locked in exclusive distribution; consumer electronics exist in
 *   transition), but at the level of the general model, it functions as a
 *   Tangled Rope with significant snare characteristics for excluded
 *   retailers.
 *
 * KEY AGENTS:
 *   - Device Manufacturers: Primary beneficiary (institutional/arbitrage) — control brand positioning, margin structures, inventory allocation; can exit to direct-to-consumer without loss of function
 *   - Authorized Distributors/Dealers: Secondary beneficiary (moderate/constrained) — enjoy exclusive territorial rights but bear inventory risk and margin constraints; significant switching costs
 *   - Large Retail Chains: Mixed actor (moderate/constrained) — benefit from exclusive product bundles and pricing guarantees; constrained by promotional restrictions and inventory sourcing limits
 *   - Independent Retailers: Primary victim (powerless/trapped) — locked out of manufacturer channels; face warranty denial and account termination for secondary-market sourcing; cannot exit without abandoning retail operation
 *   - Consumers (price discovery): Abstract victim (powerless/trapped) — lack transparent pricing across channels; cannot identify optimal purchasing location; no organized mechanism for cross-channel price comparison
 *   - Platform Aggregators: Organized agents (organized/constrained) — Amazon, online marketplaces, manufacturer direct channels creating alternative distribution pathways; constrained by remaining geographic restrictions but building sunset mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent logistical requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(device_manufacturer_distribution_models, 0.58).
domain_priors:suppression_score(device_manufacturer_distribution_models, 0.65).
domain_priors:theater_ratio(device_manufacturer_distribution_models, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(device_manufacturer_distribution_models, extractiveness, 0.58).
narrative_ontology:constraint_metric(device_manufacturer_distribution_models, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(device_manufacturer_distribution_models, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(device_manufacturer_distribution_models, tangled_rope).
narrative_ontology:human_readable(device_manufacturer_distribution_models, "Device Manufacturer Distribution Models").
narrative_ontology:topic_domain(device_manufacturer_distribution_models, "economic/industrial/technology").

domain_priors:requires_active_enforcement(device_manufacturer_distribution_models).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(device_manufacturer_distribution_models, manufacturer_ecosystem).
narrative_ontology:constraint_beneficiary(device_manufacturer_distribution_models, authorized_distributors).
narrative_ontology:constraint_victim(device_manufacturer_distribution_models, independent_retailers).
narrative_ontology:constraint_victim(device_manufacturer_distribution_models, consumer_price_discovery).
narrative_ontology:constraint_victim(device_manufacturer_distribution_models, supply_chain_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT RETAILERS (SNARE) — Face severe barriers to obtaining devices through manufacturer channels. Exclusive distribution agreements lock them out; purchasing from secondary markets incurs penalties (warranty denial, account termination). Cannot exit without abandoning their retail operation entirely. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(device_manufacturer_distribution_models, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL DISTRIBUTORS (TANGLED ROPE) — Participate in genuine distribution coordination (inventory management, logistics, regional demand forecasting) alongside asymmetric extraction. Manufacturers capture margin through exclusive territorial agreements; distributors bear market risk while manufacturers set prices and allocate inventory. High switching costs but some agency in regional operations.
constraint_indexing:constraint_classification(device_manufacturer_distribution_models, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEVICE MANUFACTURERS (ROPE) — Primary beneficiary. Control brand positioning, pricing, inventory allocation. Experiences the constraint as coordination: managing geographic coverage, maintaining distribution standards, ensuring retailer compliance. Can exit distribution model entirely by transitioning to direct-to-consumer. Net beneficiary — extraction flows toward this actor.
constraint_indexing:constraint_classification(device_manufacturer_distribution_models, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE RETAIL CHAINS (TANGLED ROPE) — Benefit from manufacturer price guarantees and exclusive product bundles (coordination function) while bearing constraints on promotional autonomy and margin structures (extraction). Significant scale gives negotiating power but exclusivity agreements limit inventory sourcing alternatives. Mixed extraction and coordination at organizational timescale.
constraint_indexing:constraint_classification(device_manufacturer_distribution_models, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSUMER PRICE DISCOVERY (SNARE) — Abstract victim. Geographic price fragmentation, opacity around margin structures, and resale restrictions prevent transparent cross-channel pricing. Consumers cannot identify where devices are cheapest or why prices vary. No advocate, no exit mechanism, no organized response capacity. Extraction through information asymmetry maintained by distribution model enforcement.
constraint_indexing:constraint_classification(device_manufacturer_distribution_models, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: VERTICAL INTEGRATION THEATER (PITON) — The formal justification for exclusive distribution (supply chain efficiency, quality control, customer support) has degraded as direct-to-consumer logistics have matured. Manufacturers maintain exclusive distribution frameworks through institutional inertia despite logistics improvements that render the theatrical justifications obsolete. Theater ratio reflects that much of the enforcement (authorized dealer verification, territorial exclusivity) is performative — maintained because the alternative hasn't fully displaced it, not because it functions optimally.
constraint_indexing:constraint_classification(device_manufacturer_distribution_models, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: PLATFORM AGGREGATORS (SCAFFOLD) — Organized actors (Amazon, online marketplaces, manufacturer direct channels) are building alternative distribution pathways that bypass exclusive dealer agreements. Sunset mechanism: as direct-to-consumer logistics mature and platform aggregators establish supply relationships, the traditional exclusive distribution model loses enforcement capacity. Constrained because some manufacturers still enforce geographic restrictions, but visible exit path creating lower chi even with moderate extraction.
constraint_indexing:constraint_classification(device_manufacturer_distribution_models, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN) — Risk of naturalizing contingent institutional arrangements as inherent logistical limits. Some analytical frames treat exclusive distribution as an immutable feature of hardware supply chains — framing the problem as 'devices are inherently complex and require specialized dealer expertise.' However, the structural data reveals this as false summit: digital devices now ship through commodity logistics (same-day delivery, automated warehouses, remote technical support), and the specialized dealer infrastructure persists through organizational inertia rather than technical necessity.
constraint_indexing:constraint_classification(device_manufacturer_distribution_models, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(device_manufacturer_distribution_models_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(device_manufacturer_distribution_models, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(device_manufacturer_distribution_models, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(device_manufacturer_distribution_models, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(device_manufacturer_distribution_models, TR),
    TR >= 0.70.

:- end_tests(device_manufacturer_distribution_models_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the significant economic rents manufacturers capture through exclusive distribution control. The value increased from 0.42 to 0.58 over the measurement interval, indicating strengthening enforcement and margin capture as digital devices became commodity-like and manufacturers could no longer justify lock-in through technical necessity — yet continued enforcement anyway. Suppression (0.65): High. Multiple interlocking barriers prevent independent retailers from accessing devices: exclusive distribution agreements (legal enforcement), warranty penalties (economic punishment), account termination threats (business viability threat), and restricted resale (contract enforcement). These barriers are material and institutional, not merely coordinated preferences. Theater ratio (0.48): Moderate. Roughly half the enforcement infrastructure — authorized dealer verification, territorial exclusivity maintenance, promotional restriction policing — serves genuine coordination (ensuring geographic coverage, maintaining service standards). The other half is purely extractive (preventing margin competition, protecting manufacturer pricing power). The theater ratio has increased slightly over time as logistics matured (reducing genuine coordination needs) while extraction enforcement intensified.
 *
 * PERSPECTIVAL GAP:
 *   The largest perspectival divergence is between manufacturers (who see pure coordination benefit) and independent retailers (who see pure extraction with no benefit). This 180-degree divergence indicates asymmetric extraction is the constraint's core function. The secondary gap is between regional distributors (who see mixed coordination and extraction) and independent retailers (who see extraction only). This divergence reveals that the distribution model's coordination benefits are concentrated among authorized participants — the 'coordination' exists only among parties the manufacturer has granted access. For excluded retailers, the constraint is extraction without coordination. The Piton perspective (institutional inertia) and Mountain perspective (false summit) both identify risk of naturalizing this extractive arrangement. The analytical observer must explicitly flag that 'supply chain efficiency through exclusive distribution' is the theatrical justification masking organizational lock-in and margin protection.
 *
 * DIRECTIONALITY LOGIC:
 *   Manufacturers occupy the low-d beneficiary position (d ≈ 0.15): their arbitrage exit options (direct-to-consumer, platform relationships, regional variations) and beneficiary status (distribution lock-in extracts toward them) produce low f(d) and potentially negative χ from their perspective — they experience the constraint as enabling power, not constraint. Independent retailers occupy the high-d victim position (d ≈ 0.95): trapped exit options and victim status (extracted from) produce high f(d) and positive χ — maximum experienced extraction. Regional distributors occupy the middle position (d ≈ 0.60): constrained exit and mixed beneficiary/victim status produce moderate f(d) and mixed χ. The piton perspective derives not from high χ but from the theater gate (theater ratio 0.48, approaching 0.70 threshold) — the enforcement infrastructure is increasingly performative as logistics alternatives emerge. Platform aggregators see low χ because their exit options are mobile/arbitrage (they can build alternative channels) and their victim status is temporary (sunset visible) — classification as Scaffold rather than Snare despite suppression ≥ 0.40.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that 'distribution coordination' and 'distribution extraction' are not mutually exclusive — the same institutional arrangement serves both functions simultaneously for different agents. Manufacturers genuinely need distributed inventory and geographic coverage (coordination function = Rope). But they capture this coordination as a vehicle for price control and market access gatekeeping (extraction function = Snare from retailer perspective). The Tangled Rope classification is not a compromise between Rope and Snare; it is the accurate type that captures both functions operating simultaneously for different participants. The mandatrophy dissolves when we recognize that coordination and extraction can be perfectly coupled in the same constraint — the extraction mechanism IS the coordination mechanism (exclusive distribution coordinates supply while extracting retailer margins). The key diagnostic: if the coordination function could be preserved while removing the extraction, the constraint would split into two types; but attempt to preserve manufacturer coordination (geographic coverage, inventory management) without exclusive distribution reveals that the coordination genuinely does require asymmetric enforcement — agents will free-ride on inventory availability while refusing to pay margin. Thus the Tangled Rope is stable, not decomposable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorized_dealer_necessity,
    'Does the ''authorized dealer'' requirement genuinely serve coordination functions (quality control, customer support, inventory management) or is it primarily an enforcement mechanism for margin protection?',
    'Comparative analysis: authorized dealer service quality vs. manufacturer direct-to-consumer support satisfaction scores; warranty claim resolution rates and costs between channels; inventory turnover and waste metrics.',
    'If coordination-dominant: classification shifts toward pure Rope; suppression score decreases. If enforcement-dominant: confirms Tangled Rope with high extraction component; validates snare perspective for excluded retailers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authorized_dealer_necessity, empirical, 'Whether authorized dealer structure serves genuine coordination vs enforcement').

omega_variable(
    platform_displacement_timeline,
    'At what adoption rate does direct-to-consumer and platform aggregator distribution displace exclusive dealer channels as the dominant model?',
    'Market share tracking across device categories; time-series analysis of manufacturer price consistency across channels; measurement of exclusive distribution enforcement intensity over time.',
    'If timeline < 5 years: scaffold sunset is imminent, extractiveness decreasing. If timeline > 15 years: scaffold is aspirational rather than structural, and constraint remains snare-like for years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_displacement_timeline, empirical, 'Timeline for direct-to-consumer dominance over exclusive distribution').

omega_variable(
    geographic_market_divergence,
    'Does the distribution constraint operate uniformly across developed, emerging, and least-developed markets, or does it bifurcate based on logistics infrastructure maturity?',
    'Mapping of distribution model enforcement by geography; analysis of manufacturer policy consistency across regions; identification of markets where exclusive distribution has already collapsed.',
    'If uniform: constraint affects all spatial scopes equally. If divergent: regional scope constraints are weaker (direct-to-consumer options available) while developing-market scopes remain trapped (logistics unavailable). Requires constraint decomposition by geographic region.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_market_divergence, empirical, 'Whether distribution constraint operates uniformly across geographic markets').

omega_variable(
    resale_price_maintenance_enforcement,
    'What enforcement mechanisms actually prevent retailers from pricing below manufacturer-recommended retail price (MRRP), and how durable are they against organized discount channels?',
    'Case studies of manufacturers pursuing retailer litigation or account termination for pricing violations; analysis of discount retailer survival rates and their workarounds (bundling, territory exclusions, online restrictions).',
    'If enforcement is durable: extraction mechanism is stable, snare classification for independent retailers is robust. If enforcement is eroding: suppression score decreases, classification shifts toward constrained rather than trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resale_price_maintenance_enforcement, empirical, 'Enforceability of resale price maintenance in distribution models').

omega_variable(
    supply_chain_transparency_coordination,
    'Could a publicly transparent inventory-allocation system achieve the same coordination benefits as exclusive distribution while reducing extraction?',
    'Pilot projects in tech and consumer electronics where manufacturers share inventory forecasts publicly; analysis of whether transparent allocation reduces need for exclusive dealer lock-in while maintaining supply chain efficiency.',
    'If transparency achieves coordination: entire constraint shifts toward pure Rope or Scaffold (temporary enforcement during transition); extractiveness decreases significantly. If transparency fails: confirms that extraction component is necessary to maintain coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_chain_transparency_coordination, empirical, 'Whether transparent allocation could replace exclusive distribution coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(device_manufacturer_distribution_models, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(devdist_tr_t0, device_manufacturer_distribution_models, theater_ratio, 0, 0.35).
narrative_ontology:measurement(devdist_tr_t8, device_manufacturer_distribution_models, theater_ratio, 8, 0.42).
narrative_ontology:measurement(devdist_tr_t16, device_manufacturer_distribution_models, theater_ratio, 16, 0.48).
narrative_ontology:measurement(devdist_tr_t4, device_manufacturer_distribution_models, theater_ratio, 4, 0.39).

% Extraction over time
narrative_ontology:measurement(devdist_be_t0, device_manufacturer_distribution_models, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(devdist_be_t8, device_manufacturer_distribution_models, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(devdist_be_t16, device_manufacturer_distribution_models, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(devdist_be_t4, device_manufacturer_distribution_models, base_extractiveness, 4, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(device_manufacturer_distribution_models, resource_allocation).
narrative_ontology:affects_constraint(device_manufacturer_distribution_models, platform_aggregator_gatekeeper_power).
narrative_ontology:affects_constraint(device_manufacturer_distribution_models, consumer_electronics_pricing_opacity).

% DUAL FORMULATION NOTE:
% Device manufacturer distribution models decompose by device category (smartphone distribution differs structurally from industrial equipment distribution) and by geographic market (developed-market direct-to-consumer dominance vs. emerging-market exclusive dealer lock-in). Each decomposition maintains separate ε values reflecting market maturity. This story aggregates across categories and geographies; constraint families by market-segment would provide higher precision.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(device_manufacturer_distribution_models, moderate, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
