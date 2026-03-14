% ============================================================================
% CONSTRAINT STORY: platform_intermediation_rent_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_intermediation_rent_extraction, []).

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
 *   constraint_id: platform_intermediation_rent_extraction
 *   human_readable: Platform Intermediation Rent Extraction
 *   domain: economic/digital_infrastructure
 *
 * SUMMARY:
 *   Platform intermediation has become the dominant distribution
 *   infrastructure for digital and physical goods globally. The constraint
 *   operates at the intersection of network effects, data asymmetry,
 *   algorithmic opacity, and contractual lock-in. Small merchants and
 *   logistics partners face immediate extraction (snare perspective); SMEs
 *   and consumers experience mixed coordination-extraction dynamics (tangled
 *   rope); the platform operator captures asymmetric rents through control of
 *   matching algorithms and payment systems (rope from their perspective);
 *   and regulatory bodies are attempting to impose temporary constraints on
 *   platform dominance through unbundling and commission caps (scaffold). The
 *   extractiveness has increased substantially over the interval (0.35 →
 *   0.68) as platforms have consolidated market power and developed
 *   increasingly sophisticated rent-extraction mechanisms. Theater ratio has
 *   also increased (0.38 → 0.58), reflecting the growing performative content
 *   of platform 'services' — recommendation algorithms optimized for platform
 *   profit rather than user utility, review systems that serve discovery
 *   theater rather than reliability, and data-driven pricing that obscures
 *   cost structures.
 *
 * KEY AGENTS:
 *   - Platform Operator (Amazon, Shopify, Alibaba): Primary beneficiary (institutional/arbitrage) — captures commission spreads, develops proprietary logistics, exploits data advantages, has unilateral control over terms
 *   - Merchant Sellers (SMEs): Primary victims (powerless/trapped and moderate/constrained) — locked into platform ecosystem by network effects, face unilateral commission increases, algorithmic suppression, data access restrictions
 *   - Logistics Partners (3PL, delivery networks): Secondary victims (powerless/trapped) — contracted with take-it-or-leave-it terms, face rate compression, infrastructure lock-in, information asymmetry in routing
 *   - Consumers: Secondary victims (moderate/mobile) — benefit from platform convenience but subject to algorithmic steering toward high-margin items, reduced merchant competition, opacity in pricing and recommendations
 *   - Larger Merchants/Brand Owners: Mixed role (powerful/mobile) — sufficient scale to negotiate favorable terms and maintain alternative distribution, but still subject to platform extraction; experience tangled rope rather than snare
 *   - Regulatory Coalitions: Organized intervention agents (organized/constrained) — attempting to scaffold extraction reduction through data portability mandates, commission caps, algorithmic transparency requirements; effectiveness partially unproven
 *   - Incumbent Retail Distribution: Institutional degradation (institutional/arbitrage) — traditional wholesale/retail persists through inertia; piton classification reflects hollowing out of physical distribution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_intermediation_rent_extraction, 0.68).
domain_priors:suppression_score(platform_intermediation_rent_extraction, 0.72).
domain_priors:theater_ratio(platform_intermediation_rent_extraction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_intermediation_rent_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(platform_intermediation_rent_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(platform_intermediation_rent_extraction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_intermediation_rent_extraction, tangled_rope).
narrative_ontology:human_readable(platform_intermediation_rent_extraction, "Platform Intermediation Rent Extraction").
narrative_ontology:topic_domain(platform_intermediation_rent_extraction, "economic/digital_infrastructure").

domain_priors:requires_active_enforcement(platform_intermediation_rent_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_intermediation_rent_extraction, platform_operator).
narrative_ontology:constraint_victim(platform_intermediation_rent_extraction, merchant_sellers).
narrative_ontology:constraint_victim(platform_intermediation_rent_extraction, consumer_surplus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MERCHANT SELLER (SNARE) — Small merchants have no practical exit from major platforms (Amazon, Alibaba, Shopify). Delisting means loss of customer access; alternative platforms have negligible reach. Platform controls terms unilaterally: commission rates, algorithmic visibility, payment terms, dispute resolution. Trapped by network effects and customer concentration. Experiences maximum extraction with minimal coordination benefit beyond access to the network itself.
constraint_indexing:constraint_classification(platform_intermediation_rent_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOGISTICS PARTNER (SNARE) — Third-party logistics providers (fulfilled-by-merchant couriers, warehouse operators) face take-it-or-leave-it contracts with Amazon's Logistics Network. Limited alternatives due to scale requirements. Rate compression and operational demands increase annually with minimal negotiation capacity. Suppression mechanisms include contractual lock-in, platform-specific infrastructure investment, and information asymmetry in routing algorithms.
constraint_indexing:constraint_classification(platform_intermediation_rent_extraction, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SMALL BUSINESS ECOSYSTEM (TANGLED ROPE) — SMEs benefit from platform access to customers, payments infrastructure, and fulfillment services (genuine coordination). Simultaneously extract through opaque ranking algorithms, sudden policy changes, commission fee increases, and data access restrictions. Exit costs are substantial but not absolute — some SMEs successfully diversify to owned websites or multiple platforms, but at significant operational cost. Exhibits both real coordination value and asymmetric extraction.
constraint_indexing:constraint_classification(platform_intermediation_rent_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Operates the intermediation network and captures commission spreads. Experiences the constraint as pure coordination: matching buyers to sellers, enabling transactions, collecting payments, managing disputes. From the operator's perspective, extraction rates reflect fair compensation for infrastructure and risk. Has maximal exit optionality (can pivot business model, adjust terms, enter adjacent markets) and gains persistent network effects from lock-in.
constraint_indexing:constraint_classification(platform_intermediation_rent_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSUMER (TANGLED ROPE) — Consumers benefit from platform convenience, price discovery, and selection (genuine coordination). Simultaneously subject to extraction through recommendation algorithms optimized for platform profit (not consumer utility), monopolistic pricing enabled by reduced merchant competition, and informational asymmetry in product reviews/vendor ratings. Exit options exist (alternative platforms, direct merchant sites) but switching costs create constrained mobility. Moderate time horizon because adaptation (learning new platform norms) happens at biographical scale.
constraint_indexing:constraint_classification(platform_intermediation_rent_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY COALITION (SCAFFOLD) — Antitrust authorities, labor regulators, and consumer protection agencies increasingly enforce constraints on platform dominance (EU Digital Markets Act, FTC enforcement, Amazon unionization). These interventions are structured as temporary scaffolding: unbundling requirements, data portability mandates, commission caps, or disclosure obligations designed to reduce extraction mechanisms. High suppression in enforcement timeline, but agents believe the sunset is real — compliance frameworks will eventually mature and reduce platform power asymmetry. Currently mid-implementation; effectiveness partially unproven.
constraint_indexing:constraint_classification(platform_intermediation_rent_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: INCUMBENT RETAIL DISTRIBUTION (PITON) — Traditional retail and wholesale distribution networks (brick-and-mortar, distributor hierarchies) persist despite platform dominance. These institutions perform substantially performative functions (physical store presence, retail ritual) while ceding market share to platforms. They maintain pricing power through brand control and physical locations but are gradually hollowed out. Theater ratio high because much of retail infrastructure (excessive store counts, high commercial rent capture) persists through institutional inertia rather than functional necessity.
constraint_indexing:constraint_classification(platform_intermediation_rent_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: LARGER MERCHANT/BRAND OWNER (TANGLED ROPE) — Established brands (Nike, Apple, Levi's) have sufficient scale to negotiate favorable terms, maintain owned distribution channels, and threaten platform exit. Experience tangled rope: platforms provide essential reach and logistics coordination, while extraction through commission rates and exclusive deal demands remains real. High exit optionality (can build own e-commerce, leverage other platforms) moderates experienced extraction. Power asymmetry favors the brand but platform still captures meaningful rent.
constraint_indexing:constraint_classification(platform_intermediation_rent_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / MARKET STRUCTURE VIEW (MOUNTAIN) — From a sufficiently abstract civilizational view, some intermediation fee is inherent to matching supply and demand: network effects and information asymmetry between buyers and sellers create natural monopoly conditions in digital markets. This perspective sees platform dominance as an inevitable structural outcome of scale economics. However, the falsifiability of this claim depends on whether alternative models (cooperative platforms, public utilities, open-source marketplaces) can achieve comparable scale without extraction. The mountain classification itself is contestable — it naturalizes what may be a contingent institutional design choice.
constraint_indexing:constraint_classification(platform_intermediation_rent_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_intermediation_rent_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_intermediation_rent_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_intermediation_rent_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_intermediation_rent_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_intermediation_rent_extraction, TR),
    TR >= 0.70.

:- end_tests(platform_intermediation_rent_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. Platform commissions (15-45% depending on category) have increased over the 15-year interval as platforms consolidated dominance. Additional extraction mechanisms include payment processing fees, advertising costs to gain algorithmic visibility, and data monetization. The measurement shows clear accumulation pattern (0.35 → 0.68). Core extraction derives from control of the matching algorithm — merchants and consumers cannot access each other without platform intermediation, and the platform sets terms unilaterally. Suppression (0.72): High. Multiple barriers prevent exit: network effects (merchant loses customer access outside platform), switching costs (infrastructure investment in SKU formatting, fulfillment setup), information asymmetry (algorithmic ranking factors opaque), and contractual restrictions (exclusivity clauses, non-compete clauses in some jurisdictions). Merchants cannot organize collectively due to platform's ability to delist coordinators. Suppression has remained stable even as extractiveness increased — barriers persist regardless of extraction rate. Theater ratio (0.58, rising): Moderate-high. Platform 'services' include increasingly performative components: recommendation algorithms presented as personalization but optimized for platform profit; review systems presented as quality signals but subject to platform moderation incentives; sponsored listings presented as algorithmic ranking but actually pay-to-play advertising. Theater has increased over the interval as platforms shift from pure intermediation to active profit-maximization through opaque mechanisms. True intermediation cost (matching, payment processing, dispute resolution) is substantially lower than captured commission, but this true cost is obscured by theater.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap exists between the platform operator's rope perspective and the merchant seller's snare perspective. Both experience the same commission rates and terms, but experienced extractiveness differs by an order of magnitude. This gap reveals that χ = ε × f(d) correctly captures that the same nominal extraction mechanism (commission rates) operates asymmetrically depending on one's structural position. The platform operator's low d (beneficiary + exit options) produces low or negative χ, making the constraint feel like pure coordination (rope). The merchant's high d (victim + trapped) produces high χ, making the same constraint feel like pure predation (snare). The regulatory coalition's scaffold perspective introduces a critical temporal dimension: they see the constraint as temporary because they have agency to change the underlying institutional structure. This contrasts with the merchant's immediate timeframe (biographical survival within the platform) and the analytical observer's civilizational timeframe (network economics appear inevitable). The perspectival gap itself is diagnostic — it reveals that 'inevitability' framing requires erasing the agent who experiences the constraint as changeable through political action.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: The platform operator benefits from the constraint and has exit options (arbitrage) — derives low d (beneficiary + mobile/arbitrage → d ≈ 0.10-0.20), producing negative or near-zero χ. Merchant sellers are victims and trapped — derives high d (victim + trapped → d ≈ 0.90), producing maximum χ ≈ 1.40 after sigmoid conversion. SMEs are victims but constrained (not trapped) — derives moderate-high d (victim + constrained → d ≈ 0.65-0.75), producing χ ≈ 1.00. Larger brands are victims but mobile — derives moderate d (victim + mobile + powerful → d ≈ 0.45-0.55), producing χ ≈ 0.65. Regulatory coalitions are organized agents with constrained exit (regulatory work is costly and uncertain) — derives moderate d (d ≈ 0.55-0.65), producing χ ≈ 0.85. The scaffolding perspective reflects that regulatory agents have agency and see an exit path (regulatory implementation reduces extraction), which is structurally different from merchant entrapment. The high variance in d values across perspectives (0.10 to 0.90) explains why different agents perceive fundamentally different constraint types from the same base extractiveness (0.68).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CONFIRMATION: The constraint satisfies all three tangled rope gates. (1) Genuine coordination function: platforms solve the matching problem and reduce transaction costs for merchants and consumers. Without the platform, finding customers requires expensive advertising or physical retail presence. (2) Asymmetric extraction: platforms capture commission spreads and rents from algorithmic steering that exceed the cost of intermediation. (3) Active enforcement: platforms actively enforce extraction through algorithmic suppression of merchant visibility (demotion for vendors who don't use fulfillment-by-platform), data access restrictions, and contractual lock-in. The tangled rope classification is not challenged by false natural law (mountain) because regulatory coalitions demonstrate the constraints are changeable — EU Digital Markets Act enforcement, FTC antitrust actions, and merchant unionization show the extraction mechanisms are politically malleable, not laws of nature. The mandatrophy is resolved by recognizing that platform dominance benefits from coordination function (genuine rope elements) while layering extraction mechanisms (snare elements) that were not inevitable. The combination of genuine coordination + asymmetric enforcement + high suppression defines tangled rope precisely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_inevitability,
    'Are platform monopolies an inevitable outcome of network effects, or are they contingent on specific institutional choices (algorithm design, data access, commission structures)?',
    'Comparative analysis of alternative platform models (cooperative platforms like Stocksy, Etsy''s decentralization experiments, open-source marketplaces). Historical counterfactuals: would different initial design choices have prevented monopoly formation?',
    'If inevitable: mountain classification justified; regulation treats symptoms not causes. If contingent: extraction is policy-dependent; regulatory intervention can redistribute rents or eliminate extraction mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_inevitability, conceptual, 'Whether platform monopolies are inevitable structural outcomes or contingent institutional choices').

omega_variable(
    commission_rate_reference_point,
    'What commission rate or fee structure represents fair compensation for platform services versus extractive overhead?',
    'Benchmarking against alternative intermediation models (traditional wholesale/retail margins, open-source platform operating costs, non-profit cooperative overhead). Analysis of platform profit margins and their drivers (actual service cost vs rent capture).',
    'If current rates (15-45%) reflect true service costs: tangled rope classification validated. If rates exceed service costs by >50%: snare classification should extend to more perspectives, indicating pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commission_rate_reference_point, empirical, 'Reference point for distinguishing fair intermediation fees from extractive overhead').

omega_variable(
    algorithmic_opacity_suppression_mechanism,
    'Is algorithmic invisibility (search ranking, recommendation prioritization, fraud detection) a necessary technical feature or a deliberate suppression mechanism preventing merchant exit?',
    'Algorithmic transparency analysis: can merchants understand ranking factors? Do competing platforms use different algorithms with transparent logic? Can open-source platforms achieve comparable ranking quality with auditable algorithms?',
    'If necessary: opacity is coordination cost (Boltzmann floor elevation). If deliberate: opacity is suppression mechanism, increasing χ and extending snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_opacity_suppression_mechanism, empirical, 'Whether algorithmic opacity is necessary or a designed suppression mechanism').

omega_variable(
    regulatory_intervention_sunset_realism,
    'Will regulatory interventions (data portability, commission caps, unbundling requirements) actually reduce platform extraction, or will platforms adapt their rent-extraction mechanisms faster than regulation responds?',
    'Longitudinal tracking of EU Digital Markets Act enforcement outcomes; measurement of merchant exit rates and profitability pre/post regulation; platform business model adaptations (shift from commission to advertising, ancillary fees, data monetization).',
    'If effective: scaffold sunset is real; extraction mechanisms decline over regulatory implementation timeframe. If ineffective: scaffold classification was aspirational; extraction persists through adapted mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_intervention_sunset_realism, empirical, 'Whether regulatory interventions will achieve sustainable extraction reduction').

omega_variable(
    consumer_surplus_attribution,
    'What portion of platform value creation benefits consumers (lower search costs, better selection, faster delivery) versus what portion is captured as platform rent through algorithmic steering and price elevation?',
    'Comparative consumer price analysis: identical products on platform vs independent sites; counterfactual modeling of consumer welfare under alternative platform commission structures; measurement of algorithmic steering toward high-margin items.',
    'If consumer surplus >> platform rent: platforms provide substantial coordination value (rope classification valid). If consumer surplus ≈ platform rent: extraction is balanced against genuine service provision (tangled rope confirmed). If platform rent >> consumer surplus: pure extraction with minimal coordination (snare classification extends).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_surplus_attribution, empirical, 'Attribution of platform value creation between consumer welfare and platform rent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_intermediation_rent_extraction, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plat_inter_tr_t0, platform_intermediation_rent_extraction, theater_ratio, 0, 0.38).
narrative_ontology:measurement(plat_inter_tr_t5, platform_intermediation_rent_extraction, theater_ratio, 5, 0.48).
narrative_ontology:measurement(plat_inter_tr_t10, platform_intermediation_rent_extraction, theater_ratio, 10, 0.58).
narrative_ontology:measurement(plat_inter_tr_t15, platform_intermediation_rent_extraction, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(plat_inter_be_t0, platform_intermediation_rent_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(plat_inter_be_t5, platform_intermediation_rent_extraction, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(plat_inter_be_t10, platform_intermediation_rent_extraction, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(plat_inter_be_t15, platform_intermediation_rent_extraction, base_extractiveness, 15, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_intermediation_rent_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(platform_intermediation_rent_extraction, 0.12).
narrative_ontology:affects_constraint(platform_intermediation_rent_extraction, algorithmic_opacity_regulatory_arbitrage).
narrative_ontology:affects_constraint(platform_intermediation_rent_extraction, data_portability_technical_incompatibility).
narrative_ontology:affects_constraint(platform_intermediation_rent_extraction, merchant_unionization_collective_action).

% DUAL FORMULATION NOTE:
% Platform intermediation can be decomposed into two structurally distinct constraints: (1) matching/coordination (ε ≈ 0.15, rope) — the core function of connecting buyers and sellers; (2) rent extraction through opacity and lock-in (ε ≈ 0.60, snare) — algorithmic steering, commission escalation, data monetization. This story models the combined constraint. Downstream stories track specific extraction mechanisms (algorithmic opacity, data access restrictions, payment term manipulation) with their own ε values and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_intermediation_rent_extraction, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
