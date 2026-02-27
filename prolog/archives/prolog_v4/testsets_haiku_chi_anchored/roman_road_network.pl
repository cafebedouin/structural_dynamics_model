% ============================================================================
% CONSTRAINT STORY: roman_road_network
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roman_road_network, []).

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
 *   constraint_id: roman_road_network
 *   human_readable: The Roman Road Network as a Mechanism of Imperial Control and Economic Integration
 *   domain: technological/political/economic
 *
 * SUMMARY:
 *   The Roman road network (approximately 250,000 miles constructed from 312
 *   BCE to 476 CE) represents a hybrid constraint combining genuine
 *   coordination benefits with significant imperial extraction. The network
 *   enabled unprecedented military mobility, economic integration, and
 *   administrative control across territories spanning three continents.
 *   However, the same infrastructure that facilitated trade also enabled tax
 *   collection, military occupation, and suppression of local autonomy.
 *   Different structural actors experienced this network through radically
 *   different lenses: the imperial military saw it as solving a collective
 *   action problem (rope); conquered populations saw it as a forced labor
 *   extraction mechanism with suppressed alternatives (snare); provincial
 *   merchants experienced mixed benefits and costs (tangled rope); emerging
 *   merchant networks saw it as temporary infrastructure destined for
 *   replacement (scaffold); and post-imperial successor states maintained it
 *   performatively without function (piton). The constraint's theater ratio
 *   rises over time (0.32 to 0.72) as the functional purpose transitions from
 *   active imperial logistics to performative toll collection, indicating
 *   Goodhart drift and eventual institutional degradation.
 *
 * KEY AGENTS:
 *   - Imperial Military: Primary beneficiary (institutional/arbitrage) — roads enable rapid legion deployment and military coordination across continental scale
 *   - Roman Administration: Primary beneficiary (institutional/arbitrage) — roads facilitate tax collection, census operations, and centralized control
 *   - Conquered Populations: Primary victim (powerless/trapped) — subjected to forced labor for road construction, lack exit options, lose local trade autonomy
 *   - Provincial Merchants: Secondary victim/partial beneficiary (moderate/constrained) — benefit from reduced transportation costs but pay tolls and tariffs, constrained by imperial checkpoints
 *   - Merchant Guilds: Organized actors (organized/constrained) — view roads as temporary coordination infrastructure; build parallel merchant networks to escape toll extraction
 *   - Local Elites: Mixed actors (organized/arbitrage) — capture some toll revenues locally but lack control over road policy or military use
 *   - Post-Imperial Regional Powers: Degraded beneficiaries (institutional/arbitrage) — maintain roads as performative toll infrastructure without functional military/administrative purpose
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent imperial design as inevitable physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_road_network, 0.52).
domain_priors:suppression_score(roman_road_network, 0.65).
domain_priors:theater_ratio(roman_road_network, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_road_network, extractiveness, 0.52).
narrative_ontology:constraint_metric(roman_road_network, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(roman_road_network, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roman_road_network, tangled_rope).
narrative_ontology:human_readable(roman_road_network, "The Roman Road Network as a Mechanism of Imperial Control and Economic Integration").
narrative_ontology:topic_domain(roman_road_network, "technological/political/economic").

domain_priors:requires_active_enforcement(roman_road_network).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roman_road_network, roman_military).
narrative_ontology:constraint_beneficiary(roman_road_network, imperial_administration).
narrative_ontology:constraint_beneficiary(roman_road_network, merchant_elites).
narrative_ontology:constraint_victim(roman_road_network, peripheral_populations).
narrative_ontology:constraint_victim(roman_road_network, conquered_territories).
narrative_ontology:constraint_victim(roman_road_network, local_trade_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONQUERED PROVINCE (SNARE) — Forced labor requisitions for road construction and maintenance, no exit from imperial jurisdiction. Local trade routes replaced by imperial highways designed for military mobility and tax collection. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.68. High extraction with suppressed alternatives.
constraint_indexing:constraint_classification(roman_road_network, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PROVINCIAL MERCHANT (TANGLED ROPE) — Roads enable long-distance trade and reduced transportation costs, but require passage tolls, military escort fees, and tariffs at imperial checkpoints. Constrained exit: cannot abandon roads without losing market access. Benefits from infrastructure coordination but extraction via tax/fee asymmetry. d≈0.70, f(d)≈1.05, σ=0.9 → χ≈0.49.
constraint_indexing:constraint_classification(roman_road_network, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IMPERIAL MILITARY LOGISTICS (ROPE) — Roads solve the collective action problem of moving legions, supplies, and communication across 3+ million square kilometers. Coordination function is primary: standardized widths, maintenance protocols, way-stations. d≈0.15, f(d)≈0.02, σ=1.1 → χ≈0.01. Near-zero effective extraction; roads are coordination dividend for the imperial center.
constraint_indexing:constraint_classification(roman_road_network, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: EMERGING MERCHANT NETWORKS (SCAFFOLD) — Organized merchant guilds in the 1st-2nd centuries view the road network as temporary coordination infrastructure with an implicit sunset. As trade volumes grow and merchant power increases, guilds become independent route-setters and eventually bypass imperial checkpoints. The road network enables merchant coordination but is destined to be superseded by merchant-controlled routes and shipping. d≈0.35, f(d)≈0.33, σ=1.1 → χ≈0.19. Theater ratio low (0.48) because the primary function—moving goods efficiently—is real and functional.
constraint_indexing:constraint_classification(roman_road_network, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: POST-IMPERIAL ROAD SYSTEM (PITON) — After the Western Empire's administrative collapse (5th century), Roman roads persist through institutional inertia. Local lords claim toll collection rights without maintaining infrastructure; bandits exploit roads knowing travelers use them. Theater ratio rises to 0.72 as the functional purpose (imperial logistics) is lost but the road system persists as a performative institutional artifact—tolls collected without maintenance or security. Roads are maintained by local communities because alternatives haven't been built, not because they serve their original function.
constraint_indexing:constraint_classification(roman_road_network, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL GEOGRAPHY (MOUNTAIN) — From a civilizational/universal perspective, some large-scale transport networks are inherent to organizing settled populations above a certain density. The topological constraint—that connecting dispersed settlements requires infrastructure—appears as a natural law of scale. However, the structural data (ε=0.52, suppression=0.65, theater=0.48) contradicts the mountain classification. This reveals the false summit: network topology is immutable, but the Roman road system's SPECIFIC design (width, toll structure, military priority, imperial checkpoint placement) is contingent political choice, not natural law. The constraint is the design, not the existence of roads.
constraint_indexing:constraint_classification(roman_road_network, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_road_network_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roman_road_network, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_road_network, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roman_road_network, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(roman_road_network, TR),
    TR >= 0.70.

:- end_tests(roman_road_network_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The road network enabled genuine trade and military benefits, but the design incorporated systematic extraction mechanisms: tolls at imperial checkpoints, forced labor requisitions for construction, and tariff collection concentrated in imperial treasuries. Unlike a purely extractive system, roads generated real value for merchants and improved overall trade efficiency. The 0.52 value reflects that extraction is significant but not dominant—merchants could still profit, trade volumes expanded, and the network served coordination functions. Suppression (0.65): High. Peripheral populations had no real alternatives to using imperial roads for long-distance travel or trade; building parallel routes was forbidden or economically infeasible. Forced labor conscription for road maintenance eliminated any genuine choice about participation. However, suppression is not absolute (0.85+) because local alternatives (river routes, sea routes, local trails) remained viable in some regions. Theater ratio (0.48): Moderate-low. The functional purpose of the roads—moving goods and military units efficiently—was real and achieved. The theatrical component emerges primarily post-collapse (perspective 5) when maintenance ceases but toll collection continues. The moderate value reflects that during the imperial period, the functionality-to-performance ratio was healthy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same infrastructure generates radically different classifications depending on structural position. The imperial military (perspective 3) sees a Rope—roads solve the logistics coordination problem with real benefits. The conquered province (perspective 1) sees a Snare—roads are imposed without consent and extract labor with suppressed alternatives. The provincial merchant (perspective 2) sees a Tangled Rope—roads enable profitable trade but require payment and restrict routing freedom. The emerging merchant networks (perspective 4) see a Scaffold—they recognize the roads as temporary, destined to be superseded as merchant power increases. The post-imperial road system (perspective 5) sees itself as a Piton—maintaining roads for toll collection without maintenance or security, a degraded institutional artifact. The analytical observer (perspective 6) risks seeing a Mountain—that large-scale transport networks are inherent to civilization—but the data reveals this as a false summit naturalizing what is actually a contingent imperial design choice. The perspectival gap arises from fundamental differences in exit options (trapped → snare; arbitrage → rope; constrained → tangled rope) and beneficiary/victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial military: Beneficiary + arbitrage → d≈0.15, f(d)≈0.02. Net beneficiary; roads are the primary instrument of their power projection. Roman administration: Beneficiary + arbitrage → d≈0.15, f(d)≈0.02. Net beneficiary; roads enable tax collection and control. Conquered populations: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit from imperial jurisdiction, forced participation in road maintenance. Provincial merchants: Victim + constrained → d≈0.70, f(d)≈1.05. Significant extraction but not total; can still profit from roads, but with toll and tariff burden. Merchant guilds: Organized + constrained → d≈0.35, f(d)≈0.33. Low effective extraction; organized actors with agency can build alternatives. Local elites: Mixed (partial beneficiary) + arbitrage → d≈0.30, f(d)≈0.24. Capture some local toll revenue but lack control over policy. Post-imperial powers: Institutional + arbitrage → d≈0.05, f(d)≈-0.12 (piton classification driven by theater gate, not directionality). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival; the engine's false summit detector flags this.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint satisfies the tangled rope gate through three structural features: (1) genuine coordination function (roads enable military and merchant coordination across continental scale); (2) asymmetric extraction (tolls, tariffs, and forced labor extracted predominantly from peripheral populations and merchants); (3) active enforcement (imperial military and administration maintain the system through coercive power). The mandatrophy is resolved by showing that the network is neither pure coordination (rope) nor pure extraction (snare). It is hybrid: roads genuinely solve logistics and trade coordination problems, generating real efficiency gains that benefit merchants and the empire alike. Simultaneously, the design embeds extraction mechanisms—toll placement at imperial checkpoints, revenue concentration in imperial treasuries, forced labor conscription—that capture a disproportionate share of the value. The tangled rope classification prevents either the beneficiaries (military, merchants) from claiming the roads are pure coordination or the victims (conquered populations) from claiming they are pure extraction. The constraint is both, indexed to position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    military_vs_trade_primacy,
    'Was the Roman road network designed primarily for military mobility (extraction/suppression) or trade facilitation (coordination), and does this distinction change the classification?',
    'Historical analysis of road placement patterns: correlation with military bases vs trade hubs; examination of imperial decrees on road use priorities; archaeological evidence of toll infrastructure vs military way-stations',
    'If primarily military: Snare from provincial perspective is confirmed (control with suppression). If primarily trade: Rope from merchant perspective is more accurate. If hybrid: Tangled Rope classification holds across both.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(military_vs_trade_primacy, empirical, 'Whether roads prioritized military control or trade coordination').

omega_variable(
    local_trade_route_displacement,
    'Did Roman imperial roads actually displace pre-existing local trade networks, or did they supplement and integrate them?',
    'Archaeological survey of pre-Roman trade routes and settlements; analysis of artifact distribution patterns in peripheral regions before/after road construction; examination of whether local routes persisted alongside imperial roads',
    'If displaced: local trade autonomy was suppressed (supports high suppression=0.65). If supplemented: roads reduced extraction burden (supports lower suppression, reframe as Rope). If coexistence: distinguishes which regions experienced snare vs rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_trade_route_displacement, empirical, 'Whether Roman roads displaced or supplemented pre-existing trade routes').

omega_variable(
    toll_revenue_distribution,
    'How were toll and tariff revenues from Roman roads distributed? Did provincial elites capture local revenue or did Rome extract it centrally?',
    'Analysis of provincial financial records (inscriptions, papyri); comparison of road toll receipts vs local administrative budgets; examination of whether local elites had financial incentive to maintain roads',
    'If centrally extracted: peripheral populations were victims of double extraction (road labor + revenue loss). If distributed locally: provincial elites were partial beneficiaries (reframes moderate power perspective toward rope). If mixed: different provinces had different classification outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(toll_revenue_distribution, empirical, 'Distribution of toll revenues between Rome and provinces').

omega_variable(
    alternative_transport_viability,
    'What was the cost and feasibility of maintaining alternative transport (river routes, sea routes, local trails) relative to using Roman roads?',
    'Comparative cost analysis of transport modes; examination of merchant route preferences in periods of road deterioration vs maintenance; evidence of smuggling routes or toll avoidance',
    'If alternatives were viable: merchant suppression=0.65 is overstated, exit_options should be ''mobile'' rather than ''constrained''. If alternatives were expensive/dangerous: suppression confirmed. If suppression decreased post-collapse: piton classification is supported (theater without function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_transport_viability, empirical, 'Viability of transport alternatives to Roman roads').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roman_road_network, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rrn_tr_t0, roman_road_network, theater_ratio, 0, 0.32).
narrative_ontology:measurement(rrn_tr_t150, roman_road_network, theater_ratio, 150, 0.48).
narrative_ontology:measurement(rrn_tr_t300, roman_road_network, theater_ratio, 300, 0.72).

% Extraction over time
narrative_ontology:measurement(rrn_be_t0, roman_road_network, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rrn_be_t150, roman_road_network, base_extractiveness, 150, 0.52).
narrative_ontology:measurement(rrn_be_t300, roman_road_network, base_extractiveness, 300, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roman_road_network, global_infrastructure).
narrative_ontology:affects_constraint(roman_road_network, roman_tax_system).
narrative_ontology:affects_constraint(roman_road_network, legionary_deployment_logistics).
narrative_ontology:affects_constraint(roman_road_network, provincial_trade_integration).
narrative_ontology:affects_constraint(roman_road_network, post_roman_infrastructure_decay).

% DUAL FORMULATION NOTE:
% The Roman road network is upstream of specific economic and military constraints in the imperial system. The network's design choices (toll structure, military priority, checkpoint placement) affect how conquest, taxation, and trade integration operate. The post-imperial degradation (piton perspective) is a separate downstream constraint (post_roman_infrastructure_decay) that emerges when the functional purpose is lost but institutional inertia persists.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(roman_road_network, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
