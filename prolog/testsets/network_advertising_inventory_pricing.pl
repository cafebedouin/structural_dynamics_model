% ============================================================================
% CONSTRAINT STORY: network_advertising_inventory_pricing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_network_advertising_inventory_pricing, []).

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
 *   constraint_id: network_advertising_inventory_pricing
 *   human_readable: Network Advertising Inventory Pricing
 *   domain: digital_advertising/platform_economics
 *
 * SUMMARY:
 *   Network advertising inventory pricing operates through real-time bidding
 *   (RTB) auctions that ostensibly match advertiser demand to publisher
 *   supply through algorithmic market mechanisms. However, this constraint
 *   exhibits characteristics of both coordination and extraction: genuine
 *   coordination of attention-targeting benefits exists alongside systematic
 *   extraction of publisher and advertiser margins through information
 *   asymmetry and algorithmic opacity. The extractiveness metric (0.58)
 *   reflects moderate-to-high rent-seeking, while the theater ratio (0.48)
 *   indicates that RTB pricing, though performative, retains some functional
 *   market dynamics. The constraint is maintained through suppression (0.65)
 *   of pricing transparency, algorithmic opacity, and switching costs that
 *   lock participants into the exchange ecosystem. Publishers cannot directly
 *   value their inventory; advertisers cannot trust pricing signals;
 *   exchanges control the information required for accurate valuation. This
 *   creates asymmetric extraction despite the coordinating function of
 *   attention matching.
 *
 * KEY AGENTS:
 *   - Publisher Networks: Primary victims (powerless/trapped) — cannot exit ad exchange participation; face opaque pricing and zero visibility into valuation drivers
 *   - Advertiser Ecosystems: Secondary victims (moderate/constrained) — benefit from reach and targeting but bear cost of bid complexity, fraud, and information asymmetry; constrained by lock-in to major platforms
 *   - Ad Exchange Operators: Primary beneficiaries (institutional/arbitrage) — control pricing mechanism and information; arbitrage asymmetries between supply and demand sides
 *   - Large Tech Platforms: Powerful beneficiaries (powerful/mobile) — own both supply (user attention) and demand (advertiser relationships); integrate vertically; maintain mobile exit but prefer extraction through platform lock-in
 *   - Programmatic System: Institutional actor (institutional/arbitrage) — maintains performative auction ritual; delegates extraction mechanism to algorithmic rules
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing information asymmetry as inherent market feature rather than maintained institutional structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(network_advertising_inventory_pricing, 0.58).
domain_priors:suppression_score(network_advertising_inventory_pricing, 0.65).
domain_priors:theater_ratio(network_advertising_inventory_pricing, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(network_advertising_inventory_pricing, extractiveness, 0.58).
narrative_ontology:constraint_metric(network_advertising_inventory_pricing, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(network_advertising_inventory_pricing, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(network_advertising_inventory_pricing, tangled_rope).
narrative_ontology:human_readable(network_advertising_inventory_pricing, "Network Advertising Inventory Pricing").
narrative_ontology:topic_domain(network_advertising_inventory_pricing, "digital_advertising/platform_economics").

domain_priors:requires_active_enforcement(network_advertising_inventory_pricing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(network_advertising_inventory_pricing, ad_exchange_operators).
narrative_ontology:constraint_beneficiary(network_advertising_inventory_pricing, programmatic_platforms).
narrative_ontology:constraint_victim(network_advertising_inventory_pricing, publisher_networks).
narrative_ontology:constraint_victim(network_advertising_inventory_pricing, advertiser_ecosystems).
narrative_ontology:constraint_victim(network_advertising_inventory_pricing, consumer_attention_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLISHER NETWORK (SNARE) — Publishers are locked into ad exchange participation. They cannot reach advertisers directly without intermediaries; forced to accept opaque pricing determined by real-time bidding algorithms. Suppressed by: algorithm opacity (cannot understand valuation), information asymmetry (exchange knows aggregate data, publishers see only their share), and switching costs (audience reach requires exchange participation). No meaningful exit option.
constraint_indexing:constraint_classification(network_advertising_inventory_pricing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ADVERTISER ECOSYSTEM (TANGLED ROPE) — Advertisers benefit from targeting precision and scaled reach via programmatic systems (coordination function) but also experience extraction through opaque pricing, bid complexity, fraud risk, and data dependency. High switching costs constrained by platform lock-in, but some agencies maintain multi-platform strategies. Mixed experience: genuine coordination value alongside asymmetric extraction.
constraint_indexing:constraint_classification(network_advertising_inventory_pricing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AD EXCHANGE OPERATOR (ROPE) — Net beneficiary. Operates the pricing mechanism itself and arbitrages information asymmetries. Experiences the constraint as coordination: matching supply and demand through transparent (to operator) pricing. High exit optionality — can expand to new exchanges or modify algorithms without material constraint. Extraction flows toward this agent.
constraint_indexing:constraint_classification(network_advertising_inventory_pricing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE TECH PLATFORM (TANGLED ROPE) — Powerful actors (Google, Meta, Amazon) both operate exchanges and control supply (user attention). They coordinate supply and demand internally (genuine coordination) but extract rents through algorithmic opacity and preferential treatment of owned inventory. High mobile exit option but strong incentive to remain (vertical integration advantage). Moderate extraction experienced because of power parity with exchange operators.
constraint_indexing:constraint_classification(network_advertising_inventory_pricing, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PROGRAMMATIC RITUAL (PITON) — Real-time bidding auctions are largely performative verification of value. The majority of pricing is predetermined by algorithmic rules, bid shading, and data monopolies — true auction dynamics account for minority of final price formation. Theater persists as legitimacy mechanism ('market efficiency') masking pre-coordinated extraction. Institutional inertia maintains the ritual despite degraded actual function.
constraint_indexing:constraint_classification(network_advertising_inventory_pricing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, advertising inventory pricing reflects an inherent asymmetry: publishers control supply but cannot value it; advertisers need targeting but cannot trust valuation; exchanges control information and allocation. This perspective naturalizes information asymmetry as immutable feature of market structure. However, structural evidence contradicts mountain classification — the asymmetry is actively maintained through algorithmic opacity, data silos, and regulatory extraction rather than emerging naturally. False summit indicator.
constraint_indexing:constraint_classification(network_advertising_inventory_pricing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(network_advertising_inventory_pricing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(network_advertising_inventory_pricing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(network_advertising_inventory_pricing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(network_advertising_inventory_pricing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(network_advertising_inventory_pricing, TR),
    TR >= 0.70.

:- end_tests(network_advertising_inventory_pricing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The pricing constraint extracts value from both supply (publishers receive depressed prices) and demand (advertisers pay inflated prices with uncertain returns). The extraction is not total because (1) real matching coordination occurs, (2) large advertisers and publishers maintain some negotiating power, and (3) alternative channels (direct deals, private marketplaces) exist. The trajectory from 0.32 to 0.58 over the interval reflects increased algorithmic sophistication layering additional extraction onto genuine coordination. Suppression (0.65): Moderate-high. Participants face high barriers to transparent pricing information, algorithmic rules are proprietary and obscured, switching costs are substantial (audience reach requires exchange participation), and the system is protected by technical complexity. Publishers face especially high suppression — they cannot exit without losing advertiser reach. Advertisers can exit but face audience loss. Theater ratio (0.48): Moderate. RTB auctions maintain appearance of market efficiency and price discovery, but actual price formation is substantially predetermined by bid shading, data monopolies, and algorithmic rules. The theater has remained relatively stable (0.38 to 0.48) because the performative verification mechanism serves legitimacy for all parties even as underlying extraction mechanisms evolve.
 *
 * PERSPECTIVAL GAP:
 *   Publishers and advertisers perceive snare/tangled_rope classification — they see extraction and suppression with limited exit. Exchange operators perceive rope — they experience coordination benefits and high exit optionality. Large platforms perceive tangled rope — they benefit from coordination while also experiencing extraction from exchange operator competition (on demand side) and from their own data monopoly maintenance (on supply side). The piton classification recognizes that RTB auctions are substantially performative — they legitimate through market appearance what is actually predetermined extraction. The mountain classification (natural law view) is a false summit: it naturalizes information asymmetry as immutable rather than recognizing it as actively maintained through algorithmic opacity and data silos.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each actor is derived from their structural position: publishers are trapped beneficiaries (low initial power, extraction victims, no exit) — high d toward victimhood; advertisers are moderate players with some exit options but constrained by reach dependency — moderate d; exchanges are institutional beneficiaries with full arbitrage options — low d toward victimhood. The sigmoid f(d) converts these structural relationships into experienced extractiveness chi. Publishers experience chi near maximum (high f(d)); exchange operators experience negative chi (low f(d) benefits them). Advertisers experience moderate chi. The large tech platforms occupy an intermediate position — powerful enough to negotiate but integrated into the system sufficiently that exit is costly. Their d is moderate despite high power because their structural role as both supply and demand controllers creates internal tension in extraction direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through recognizing that all six classifications are legitimate perspectival readings of the same structural constraint. The constraint is NOT ambiguously a single type — it genuinely IS coordination (rope) for exchange operators, genuinely IS extraction (snare) for publishers, genuinely IS mixed (tangled rope) for advertisers and large platforms, genuinely IS performative ritual (piton) in its verification mechanism, and genuinely IS a contingent institutional arrangement (not mountain) when viewed civilizationally. The analytical observer risks false naturalization by treating information asymmetry as a law of markets rather than as a maintained institutional feature. The resolution: the presheaf over observation positions (who measures, from what power/time/exit context) IS the structure. No single type 'solves' the mandatrophy; the perspectival gap itself is diagnostic of extraction embedded in coordination mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_opacity_mechanism,
    'Is the pricing extraction driven by technical complexity of real-time bidding or by deliberate opacity design?',
    'Transparency audit: require exchanges to publish full bid-to-price mappings and algorithmic rules; measure publisher price recovery after transparency intervention',
    'If technical complexity: suppression (0.65) overstated; constraint may reclassify toward rope. If deliberate design: suppression is intentional extraction mechanism; constraint classifies as snare from publisher perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity_mechanism, empirical, 'Whether pricing opacity is technical complexity or deliberate extraction design').

omega_variable(
    bid_shading_prevalence,
    'What fraction of actual pricing variance is explained by pre-coordinated bid shading algorithms versus genuine auction competition?',
    'Counterfactual analysis: simulate exchange behavior with bid shading disabled; measure price distribution change; compare to theoretical auction equilibrium',
    'If bid shading < 20% of variance: auction coordination is genuine (rope classification stronger). If bid shading > 60% of variance: auction is performative (piton classification confirmed; underlying constraint is pre-coordinated extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bid_shading_prevalence, empirical, 'Proportion of pricing explained by bid shading versus auction competition').

omega_variable(
    first_party_data_value_capture,
    'Which actors capture the value of first-party data signals in pricing: publishers, advertisers, or exchanges?',
    'Comparative pricing analysis: identical ad slots with/without first-party data signals; trace value flow through intermediaries; measure willingness-to-pay spread',
    'If publishers capture: coordination benefit is genuine (rope-ish). If advertisers capture: coordination benefit is real. If exchanges capture: extraction mechanism is rent-seeking on data asymmetry (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_party_data_value_capture, empirical, 'Which actor captures value from first-party data in pricing').

omega_variable(
    alternative_pricing_mechanism_viability,
    'Could alternative pricing mechanisms (fixed pricing, direct deals, auction-free inventory) capture equivalent advertiser-publisher matching quality at lower extraction cost?',
    'Pilot programs: test fixed-price and direct-deal models; measure matching quality and price efficiency compared to programmatic baseline',
    'If alternatives viable: current constraint is contingent institutional arrangement (not mountain); extraction can be reduced through structural redesign. If alternatives fail: current system is coordination solution with unavoidable extraction overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_pricing_mechanism_viability, empirical, 'Whether alternative pricing mechanisms could reduce extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(network_advertising_inventory_pricing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(netadv_tr_t0, network_advertising_inventory_pricing, theater_ratio, 0, 0.38).
narrative_ontology:measurement(netadv_tr_t3, network_advertising_inventory_pricing, theater_ratio, 3, 0.42).
narrative_ontology:measurement(netadv_tr_t6, network_advertising_inventory_pricing, theater_ratio, 6, 0.46).
narrative_ontology:measurement(netadv_tr_t10, network_advertising_inventory_pricing, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(netadv_be_t0, network_advertising_inventory_pricing, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(netadv_be_t3, network_advertising_inventory_pricing, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(netadv_be_t6, network_advertising_inventory_pricing, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(netadv_be_t10, network_advertising_inventory_pricing, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(network_advertising_inventory_pricing, resource_allocation).
narrative_ontology:affects_constraint(network_advertising_inventory_pricing, platform_data_monopoly).
narrative_ontology:affects_constraint(network_advertising_inventory_pricing, advertiser_attribution_opacity).
narrative_ontology:affects_constraint(network_advertising_inventory_pricing, publisher_revenue_concentration).

% DUAL FORMULATION NOTE:
% Network advertising inventory pricing is downstream of platform data monopolies (which determine targeting precision and valuation certainty) and feeds into advertiser attribution opacity (inability to verify ROI). These constraints form an extractive family: data monopoly enables pricing extraction enables attribution opacity. Decompose as separate stories if measuring different observables (data monopoly by data availability, pricing by auction mechanics, attribution by measurement infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(network_advertising_inventory_pricing, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
