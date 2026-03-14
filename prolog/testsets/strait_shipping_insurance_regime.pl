% ============================================================================
% CONSTRAINT STORY: strait_shipping_insurance_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strait_shipping_insurance_regime, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: strait_shipping_insurance_regime
 *   human_readable: Strait Shipping Insurance Regime and Geopolitical Risk Extraction
 *   domain: maritime_commerce/geopolitical_extraction/insurance
 *
 * SUMMARY:
 *   The strait shipping insurance regime operates as a hybrid mechanism
 *   combining genuine risk coordination (pooling geopolitical uncertainty
 *   across global shipping) with asymmetric extraction (concentrating premium
 *   escalation risk on smaller enterprises without arbitrage capacity).
 *   Narrow straits — Hormuz, Malacca, Suez — create geographic chokepoints
 *   where 30-35% of global maritime commerce flows through a few hundred
 *   kilometers of vulnerable waterway. This geographic vulnerability is then
 *   institutionalized through insurance mechanisms that price geopolitical
 *   risk, naval presence, and political escalation into premium structures.
 *   The regime creates a structural incentive cascade: dominant naval powers
 *   maintain threat credibility by demonstrating capability to disrupt
 *   shipping; insurers use threat intelligence to price premiums; smaller
 *   shippers trapped by geography cannot exit without massive cost;
 *   extraction flows to insurers and dominant powers. The theater ratio
 *   (0.48) reflects that actual incident rates remain low while perceived
 *   risk (and premiums) escalate sharply during diplomatic crises — much of
 *   the premium movement is performative escalation rather than response to
 *   real hazard increase. Alternative routes exist but carry fuel costs that
 *   make them uneconomical for most cargo types, trapping shippers in the
 *   strait corridor and into the insurance regime.
 *
 * KEY AGENTS:
 *   - Small-Medium Shippers: Primary victims (powerless/trapped) — must pay escalated premiums with no exit option; bear full cost of geopolitical tension
 *   - Major Shipping Consortiums: Secondary beneficiaries (organized/arbitrage) — can arbitrage between strait and alternative routes; have scale to negotiate preferential insurance terms
 *   - Maritime Insurers: Primary beneficiary (institutional/constrained) — capture information asymmetry on geopolitical escalation; pool risk across global fleet; concentrate underwriting and rebates at major carriers
 *   - Dominant Naval Powers: Primary beneficiary (powerful/arbitrage) — benefit from premium extraction reflecting their threat credibility; provide genuine naval protection that justifies some extraction; can also affect insurance pricing through political signals
 *   - Regional Autonomous Shipping Initiatives: Organized challengers (organized/constrained) — building alternative verification systems and autonomous vessels that bypass geopolitical insurance extraction; represent sunset mechanism for current regime
 *   - Cold War-Era Governance Bodies: Degraded institutional actors (institutional/arbitrage) — UNCLOS, Strait Convention signatories maintain frameworks for legitimacy but lack enforcement power; regime operates around rather than through them (Piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional extraction as geographic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strait_shipping_insurance_regime, 0.58).
domain_priors:suppression_score(strait_shipping_insurance_regime, 0.62).
domain_priors:theater_ratio(strait_shipping_insurance_regime, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strait_shipping_insurance_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(strait_shipping_insurance_regime, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(strait_shipping_insurance_regime, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strait_shipping_insurance_regime, tangled_rope).
narrative_ontology:human_readable(strait_shipping_insurance_regime, "Strait Shipping Insurance Regime and Geopolitical Risk Extraction").
narrative_ontology:topic_domain(strait_shipping_insurance_regime, "maritime_commerce/geopolitical_extraction/insurance").

domain_priors:requires_active_enforcement(strait_shipping_insurance_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(strait_shipping_insurance_regime, maritime_insurers).
narrative_ontology:constraint_beneficiary(strait_shipping_insurance_regime, geopolitically_dominant_naval_powers).
narrative_ontology:constraint_victim(strait_shipping_insurance_regime, cargo_shippers_small_medium_enterprises).
narrative_ontology:constraint_victim(strait_shipping_insurance_regime, global_trade_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL-MEDIUM SHIPPERS (SNARE) — Trapped by the requirement to insure cargo through major carriers with steeply escalated premiums during geopolitical tension. No alternative shipping routes practical due to strait geography and fuel economics. Cannot exit insurance markets; must absorb premium increases or lose market access entirely. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(strait_shipping_insurance_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MAJOR SHIPPING CONSORTIUMS (ROPE) — Large carriers with scale, diversified routes, and state backing can arbitrage between strait insurance and alternative routes (longer but fuel-costlier). Experience the regime as coordination: risk pooling and premium differentiation by vessel class and owner reputation. Net beneficiary through arbitrage capacity and regulatory influence on insurance standard-setting.
constraint_indexing:constraint_classification(strait_shipping_insurance_regime, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: MARITIME INSURERS (TANGLED ROPE) — Genuine coordination function: pooling geopolitical risk and calibrating premiums by vessel, cargo, and route. But also extraction mechanism: insurers capture information asymmetry (intelligence on geopolitical escalation before markets price it), concentrate underwriting at major carriers (rebates for volume), and maintain premium floors that prevent rate competition during low-risk windows. Constrained by regulatory capital requirements and competitive pressures but benefit from the regime's information monopoly.
constraint_indexing:constraint_classification(strait_shipping_insurance_regime, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: DOMINANT NAVAL POWERS (TANGLED ROPE) — Genuine coordination: naval protection of shipping lanes provides public good (enables commerce for all flag states). But also asymmetric extraction: insurance premiums effectively tax non-aligned shipping; dominant powers' vessels receive preferential insurance terms; threat credibility of minor powers is priced at higher premiums, creating de facto toll on non-aligned commerce. Arbitrage through control over security narrative and naval presence.
constraint_indexing:constraint_classification(strait_shipping_insurance_regime, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: AUTONOMOUS SHIPPING INITIATIVES (SCAFFOLD) — Organized actors (regional shipping blocs, cooperative insurance pools, autonomous vessel consortiums) are building alternative verification and security models: real-time vessel tracking, decentralized insurance pooling, and autonomous unmanned cargo systems that reduce geopolitical risk exposure. Theater ratio low because alternatives bypass the political theater of naval escort and intelligence-based risk pricing. Sunset clause: as autonomous systems mature (5-10 years), geopolitical risk pricing loses leverage — uncrewed vessels cannot be captured or held hostage.
constraint_indexing:constraint_classification(strait_shipping_insurance_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR GOVERNANCE FRAMEWORKS (PITON) — Straits governance (Suez, Hormuz, Malacca) based on 1970s-80s frameworks: guaranteed passage rights, fixed toll structures, international regulatory bodies. These institutional structures persist through inertia despite changing geopolitical realities. The regime is largely performative: insurance pricing now overrides legal passage rights; actual security is provided by private naval contractors and national military posturing, not international law. The international framework (UNCLOS, bilateral strait agreements) is degraded — maintained for legitimacy but circumvented by insurance-based extraction.
constraint_indexing:constraint_classification(strait_shipping_insurance_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOGRAPHIC INEVITABILITY (MOUNTAIN) — From a civilizational perspective, narrow straits create inherent chokepoints: geography alone creates structural vulnerability that any power controlling the strait can exploit. This perspective naturalizes geopolitical extraction as inevitable consequence of physical geography. However, the structural data reveals this as a false summit: insurance-based extraction mechanisms, information asymmetry, and political theater are contingent institutional arrangements, not geographic laws.
constraint_indexing:constraint_classification(strait_shipping_insurance_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strait_shipping_insurance_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(strait_shipping_insurance_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strait_shipping_insurance_regime, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(strait_shipping_insurance_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(strait_shipping_insurance_regime, TR),
    TR >= 0.70.

:- end_tests(strait_shipping_insurance_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The base rate reflects genuine extraction through information asymmetry and premium escalation during crises, but not as severe as a pure snare (which would be ~0.75). Insurance pools coordinate real risk; the extraction layer (geographic entrapment + political signaling) sits on top. The measurement trajectory shows escalation from 0.32 to 0.58 over the interval, reflecting increasing geopolitical tension and rising premiums without corresponding increase in actual incident rates. Suppression (0.62): High. Small shippers face multiple suppression mechanisms: (1) geographic chokepoint (no alternative routes practical for most cargo), (2) capital barriers to autonomous vessel conversion, (3) information asymmetry (insurers know geopolitical intelligence before shipping markets), (4) regulatory compliance (maritime insurance requirements are non-negotiable for legal commerce), (5) coordination barriers (small shippers cannot pool risk independently). Some suppression mechanisms are structural (geography); others are institutional (insurance regulation) and could change. Theater ratio (0.48): Moderate. The regime exhibits significant theater: naval shows-of-force, diplomatic crisis escalation, and political signaling drive premium changes that precede actual risk increase. However, the theater is not dominant — the regime also coordinates real risk pooling and provides genuine naval security. The theater ratio reflects the gap between perceived and actual risk rather than pure performative activity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals that small shippers and major consortiums experience the same constraint mechanism at approximately a 100× difference in effective extraction intensity. This gap is purely structural (their exit options and power levels) rather than from different underlying metrics (ε is constant across all perspectives). The gap is the primary diagnostic signal that the regime is indeed extractive toward powerless agents — if the constraint were pure coordination, small and large shippers would experience similar proportional costs.
 *
 * DIRECTIONALITY LOGIC:
 *   See above — d-values derived from beneficiary/victim status + exit options + power level. No overrides required; structural derivation captures the key dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint exhibits genuine coordination (naval protection of shipping lanes, risk pooling) alongside asymmetric extraction (premium escalation targeting trapped agents, information asymmetry favoring insurers). The Tangled Rope classification is mandatrophy-resolving because it acknowledges both functions simultaneously: the regime would collapse if naval protection were withdrawn (coordination function is real), but the regime also concentrates extraction on powerless agents (asymmetric extraction is real). The false summit risk is that geographic chokepoints feel inevitable ('straits are always vulnerable') and therefore extraction feels inevitable ('premiums must reflect geopolitical risk'). But insurance pricing is institutional, not geographic. The regime could be replaced by autonomous systems, decentralized insurance pooling, or political agreements that reduce naval signaling. The mandatrophy prevents collapsing the regime into pure extraction (Snare) because the coordination function is genuine; it prevents collapsing it into pure coordination (Rope) because the extraction is severe and asymmetric. The Tangled Rope classification holds both truths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    insurance_premium_correlation_mechanism,
    'Do insurance premium escalations during geopolitical tension reflect genuine risk increase or political pricing by insurers anticipating escalation?',
    'Historical correlation analysis: compare actual incident rates (captures, vessel losses, attacks) in periods of high vs low tension against corresponding insurance premium changes. Lag analysis to identify whether premiums precede or follow actual incidents.',
    'If premiums reflect actual risk: extraction is moderate, insurance regime is coordination-heavy (Rope from insurer perspective). If premiums precede incidents: extraction is severe, regime is information-based price discrimination (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_premium_correlation_mechanism, empirical, 'Whether insurance premiums track actual vs anticipated geopolitical risk').

omega_variable(
    alternative_route_viability_threshold,
    'At what premium level do alternative routes (Cape of Good Hope, Suez bypass) become economically viable for various cargo types and vessel classes?',
    'Cost modeling: fuel, time, port charges, crew rotation for each alternative route by vessel size and cargo type. Comparison against strait insurance premium ceiling that triggers route switching.',
    'If threshold < 30% premium: small shippers have real exit option (constrains, not traps). If threshold > 80% premium: most shippers are trapped regardless of premium level (snare confirmed). Thresholds vary by cargo type and ship age, creating tiered exit options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_route_viability_threshold, empirical, 'Economic viability threshold for alternative shipping routes').

omega_variable(
    reinsurance_chain_extraction_cascades,
    'Do reinsurance chains (primary insurer → reinsurer → lloyds syndicates → capital markets) amplify geopolitical premium extraction through information asymmetry at each tier?',
    'Tracing premium markup flow through reinsurance chain; identifying information bottlenecks where upstream actors price intelligence not available to downstream purchasers; comparison of final premium to underlying risk metrics.',
    'If asymmetry is high: extraction is amplified through institutional layering (Tangled Rope with higher chi than base metrics suggest). If asymmetry is low: premiums reflect genuine risk pooling coordination (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reinsurance_chain_extraction_cascades, empirical, 'Whether reinsurance chains amplify geopolitical extraction').

omega_variable(
    autonomous_vessel_risk_profile_discontinuity,
    'Do autonomous unmanned cargo vessels face structurally lower geopolitical insurance premiums than crewed vessels, and does this create a discontinuity that collapses the current regime?',
    'Comparative insurance underwriting for autonomous vs crewed systems for identical cargo and route; assessment of whether autonomous vessels eliminate the hostage-taking and crew-detention risks that drive premiums.',
    'If autonomous vessels achieve 40%+ premium reduction: regime is genuinely temporary (Scaffold confirmed). If no premium reduction: current extractive mechanisms persist regardless of technology (Snare/Tangled Rope persist).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomous_vessel_risk_profile_discontinuity, empirical, 'Whether autonomous vessels disrupt geopolitical insurance extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strait_shipping_insurance_regime, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(strait_tr_t0, strait_shipping_insurance_regime, theater_ratio, 0, 0.35).
narrative_ontology:measurement(strait_tr_t5, strait_shipping_insurance_regime, theater_ratio, 5, 0.42).
narrative_ontology:measurement(strait_tr_t10, strait_shipping_insurance_regime, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(strait_be_t0, strait_shipping_insurance_regime, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(strait_be_t5, strait_shipping_insurance_regime, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(strait_be_t10, strait_shipping_insurance_regime, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strait_shipping_insurance_regime, resource_allocation).
narrative_ontology:boltzmann_floor_override(strait_shipping_insurance_regime, 0.18).
narrative_ontology:affects_constraint(strait_shipping_insurance_regime, suez_canal_governance).
narrative_ontology:affects_constraint(strait_shipping_insurance_regime, hormuz_chokepoint_control).
narrative_ontology:affects_constraint(strait_shipping_insurance_regime, autonomous_maritime_vessels).
narrative_ontology:affects_constraint(strait_shipping_insurance_regime, decentralized_insurance_pooling).

% DUAL FORMULATION NOTE:
% The strait shipping regime decomposes into three structurally distinct constraints: (1) geographic chokepoint vulnerability (ε ≈ 0.05, Mountain — this is immutable); (2) naval control mechanisms (ε ≈ 0.45, Tangled Rope — coordination + power projection); (3) insurance premium extraction (ε ≈ 0.58, Tangled Rope — coordination + information asymmetry). This story focuses on the regime as whole (insurance + naval + geographic), with the understanding that decomposition is possible along these structural lines. The geographic chokepoint alone would be Mountain; the regime transforms it into Tangled Rope through institutional layering.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
