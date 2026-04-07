% ============================================================================
% CONSTRAINT STORY: decentralized_preservation_scalability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decentralized_preservation_scalability, []).

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
 *   constraint_id: decentralized_preservation_scalability
 *   human_readable: Decentralized Preservation Scalability Constraint
 *   domain: digital_infrastructure/cultural_preservation/distributed_systems
 *
 * SUMMARY:
 *   Decentralized preservation systems promise to solve the
 *   single-point-of-failure problem in cultural heritage archiving by
 *   distributing content across networks of participating institutions and
 *   communities. The constraint emerges from the tension between the promise
 *   of equitable, resilient preservation and the structural reality that
 *   participation requires hardware, bandwidth, and technical expertise costs
 *   that are not uniformly distributed. Large institutional archives can
 *   amortize these costs across massive collections and aggregate resources
 *   across multiple access patterns. Small archives, indigenous communities,
 *   and local preservation initiatives face per-unit costs that are
 *   prohibitive without subsidy. The decentralized architecture thus creates
 *   a hybrid coordination-extraction mechanism: genuine coordination benefits
 *   (redundancy, resilience, network effects) coexist with asymmetric cost
 *   distribution. Major institutions coordinate through the network while
 *   marginal participants subsidize its stability.
 *
 * KEY AGENTS:
 *   - Major Archive Institutions (institutional/arbitrage): Library of Congress, national libraries, Wikimedia Foundation. Primary beneficiaries. Can exit to alternative infrastructure without significant loss. Experience pure coordination.
 *   - Edge Preservationists (powerless/trapped): Small archives, community collections, indigenous heritage initiatives. Primary victims. Cannot exit without losing visibility and access. Bear asymmetric costs.
 *   - Regional Archive Collectives (moderate/constrained): Mid-sized regional archives that coordinate through networks. Secondary victims. Experience mixed coordination and extraction. Exit is costly but possible.
 *   - Protocol Developers (organized/mobile): IPFS, Arweave, blockchain-based preservation projects. Organized actors building alternative incentive structures. See constraint as temporary problem with sunset.
 *   - Archive Governance Structures (institutional/arbitrage): Curation standards bodies, best-practice committees. Maintain appearance of distributed control while actual decisions concentrate among major institutions.
 *   - Cultural Commons (powerless/trapped): Abstract collective good of cultural diversity and accessibility. Benefits from preservation but cannot directly participate or negotiate. Extracted from implicitly.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decentralized_preservation_scalability, 0.52).
domain_priors:suppression_score(decentralized_preservation_scalability, 0.58).
domain_priors:theater_ratio(decentralized_preservation_scalability, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decentralized_preservation_scalability, extractiveness, 0.52).
narrative_ontology:constraint_metric(decentralized_preservation_scalability, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(decentralized_preservation_scalability, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decentralized_preservation_scalability, tangled_rope).
narrative_ontology:human_readable(decentralized_preservation_scalability, "Decentralized Preservation Scalability Constraint").
narrative_ontology:topic_domain(decentralized_preservation_scalability, "digital_infrastructure/cultural_preservation/distributed_systems").

domain_priors:requires_active_enforcement(decentralized_preservation_scalability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decentralized_preservation_scalability, resource_aggregators).
narrative_ontology:constraint_beneficiary(decentralized_preservation_scalability, institutional_archives).
narrative_ontology:constraint_beneficiary(decentralized_preservation_scalability, protocol_developers).
narrative_ontology:constraint_victim(decentralized_preservation_scalability, edge_preservationists).
narrative_ontology:constraint_victim(decentralized_preservation_scalability, resource_scarce_communities).
narrative_ontology:constraint_victim(decentralized_preservation_scalability, cultural_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINAL PRESERVATIONIST (SNARE) — Small archives and community-based preservation initiatives face insurmountable hardware and bandwidth costs to participate in decentralized preservation networks. Cannot exit: losing access to the network means losing visibility for their collections. Bears full extraction: must maintain redundant copies and connectivity costs but receives minimal direct benefit. No negotiating power.
constraint_indexing:constraint_classification(decentralized_preservation_scalability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL ARCHIVE COLLECTIVE (TANGLED ROPE) — Mid-sized archives coordinate preservation labor through the decentralized network, reducing duplication and enabling redundancy across regions. Genuine coordination function. But they also face extraction: network participation requires technical standardization, metadata compliance, and continuous infrastructure investment. Exit is costly (loss of collaborative partners, reduced collection accessibility) but possible. Mixed experience: benefits from network effects but subsidizes network stability for larger participants.
constraint_indexing:constraint_classification(decentralized_preservation_scalability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR ARCHIVE INSTITUTION (ROPE) — Large institutional archives (Library of Congress, national libraries) use the decentralized network to offload redundancy and access costs while maintaining control over their primary collections. Experience the constraint as pure coordination: the network solves the distribution problem that would otherwise require proprietary infrastructure. Can arbitrage: could exit to commercial CDNs or closed systems without significant loss. Net beneficiary.
constraint_indexing:constraint_classification(decentralized_preservation_scalability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROTOCOL COALITION (SCAFFOLD) — Standards bodies, content-addressing initiatives (IPFS, Arweave), and open-source preservation projects frame decentralized preservation as a temporary solution with an explicit sunset: cryptographic proof systems, energy-efficient consensus, and cost amortization across scale should eventually make preservation participation cost-neutral for all actors. Theater is declining (theatrical proof-of-work ceremonies giving way to genuine cost reduction). The sunset is structural: if cost curves follow predicted trends, extraction mechanisms lose force in 15-25 years.
constraint_indexing:constraint_classification(decentralized_preservation_scalability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ARCHIVE GOVERNANCE RITUAL (PITON) — Archive governance (preservation best practices, curation standards, collection policies) persists in decentralized systems largely as theater: governance decisions are made through consensus mechanisms that appear decentralized but are often dominated by major institutions. The governance process has high theater ratio because it performs legitimacy while actual coordination decisions remain concentrated. The ritual persists through institutional inertia and the need to maintain appearance of community voice.
constraint_indexing:constraint_classification(decentralized_preservation_scalability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, decentralized preservation scalability appears as an immutable technical law: bandwidth and storage costs are physical constants; redundancy requires resource multiplication; coordination at scale requires overhead. The constraint appears natural and irreducible. However, this perspective risks naturalizing contingent economic choices (centralized profit incentives, specific hardware architectures, energy-intensive consensus) as physical limits. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(decentralized_preservation_scalability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decentralized_preservation_scalability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(decentralized_preservation_scalability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(decentralized_preservation_scalability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(decentralized_preservation_scalability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(decentralized_preservation_scalability, TR),
    TR >= 0.70.

:- end_tests(decentralized_preservation_scalability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from marginal participants through cost asymmetry: they must maintain hardware and connectivity to participate, but receive proportionally fewer benefits (smaller collections reach smaller audiences). However, extraction is not total — decentralized networks do create genuine coordination benefits that reduce total system costs compared to isolated archives. Measurement shows extractiveness increasing over the interval (0.35 → 0.52) as network effects amplify value for major institutions while per-unit costs for edge participants remain high or increase. Suppression (0.58): Moderate-high. Multiple barriers prevent exit: loss of network visibility, dependency on collaborative redundancy, technical switching costs, and reputational cost of abandoning 'decentralized' participation. But suppression is not total — some communities do maintain isolated archives successfully, accepting lower accessibility. Theater ratio (0.64): Moderate-high. Governance processes exhibit substantial theater: consensus mechanisms create legitimacy narratives that mask concentrated decision-making power. However, theater is declining (measurement shows 0.58 → 0.64, but growth is slowing) as protocol improvements reduce need for consensus performance and cost reduction mechanisms replace governance theater as legitimation strategy.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. Major institutional archives (institutional/arbitrage) classify the same structural phenomenon as Rope (pure coordination); edge preservationists (powerless/trapped) classify it as Snare (pure extraction); regional collectives (moderate/constrained) experience Tangled Rope (mixed). The same network architecture, governance system, and cost distribution mean completely different things from different structural positions. This gap is the diagnostic signature that the constraint embeds asymmetric extraction within a coordination framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: major institutions are declared beneficiaries with arbitrage exit options, producing low d → low/negative χ, enabling Rope classification despite non-zero extractiveness. Edge preservationists are victims with trapped exit, producing high d → high χ, enabling Snare classification. Regional archives are both partial beneficiaries and partial victims with constrained exit, producing moderate d → moderate χ, enabling Tangled Rope. Protocol developers are organized actors with mobile exit, producing d ≈ 0.45 → moderate χ, enabling Scaffold classification. The governance system is beneficiary (institutional legitimacy is its payoff) with arbitrage exit (could maintain alternative governance), producing low d → Rope or Piton depending on theater gate. Edge preservationists' high d combined with their trapped exit and powerless status creates maximum experienced extraction χ despite moderate base extractiveness ε. This is the mechanism through which a moderate ε (0.52) produces a Snare classification at the powerless perspective: the f(d) function amplifies ε through power asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STRUCTURE: The constraint resolves the mandatrophy through network decomposition and cost analysis. The core claim is that decentralized preservation creates equitable access. The contradicting evidence is that participation costs create access barriers — the constraint solves one kind of centralization (single point of failure) while creating another (concentration of benefit in resource-wealthy institutions). The analytical resolution: decompose decentralized_preservation_scalability into upstream technical claims (content-addressing achieves cost efficiency for redundancy, cryptographic proofs reduce consensus theater) and downstream institutional claims (equitable participation is possible under current cost structures). The upstream technical claims approach Mountain status as cost curves improve. The downstream institutional claims remain Tangled Rope because institutional incentives concentrate benefits unless explicitly structured otherwise. The mandatrophy is resolved by showing that 'decentralized preservation' is a single label covering two structurally distinct constraints: technical redundancy coordination (approaching Mountain as costs decline) and equitable access provision (Tangled Rope unless incentive structures change). No single classification is correct until these are separated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_curve_trajectory,
    'Will hardware and bandwidth costs follow exponential decline sufficient to make edge participation cost-neutral within the scaffold timeline?',
    'Historical analysis of storage/bandwidth cost curves; prediction models from semiconductor roadmaps and fiber deployment; comparison to past infrastructure transitions (electricity, telephony, internet access)',
    'If costs decline sufficiently: scaffold classification confirmed, sunset is real, constraint evolves toward rope by 2050. If costs plateau: extraction mechanism persists indefinitely, constraint remains tangled rope for institutional actors and snare for marginal actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_curve_trajectory, empirical, 'Whether hardware cost curves support cost-neutral participation timeline').

omega_variable(
    institutional_incentive_alignment,
    'Do major institutional archives have structural incentives to subsidize edge participation costs, or does the network consolidate benefits to resource-wealthy nodes?',
    'Economic analysis of preservation network incentives; measurement of cost distribution and benefit concentration; comparison of bandwidth/storage spending by node size; analysis of governance power concentration in protocol decisions',
    'If incentives align toward subsidy: extraction reduces over time, constraint evolves toward tangled rope. If incentives concentrate: network topology may ossify with major institutions as hubs, creating structural extraction that persists indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_alignment, empirical, 'Whether institutional incentives support or undermine equitable cost distribution').

omega_variable(
    governance_decentralization_authenticity,
    'Is decentralized governance of archive protocols genuine distributed decision-making or performative legitimacy masking concentrated control?',
    'Analysis of governance participation rates by node size; correlation between voting power and economic interest; historical tracking of governance decisions and whose interests they served; comparison of stated governance models to actual decision-making authority',
    'If genuine: governance theater is low, piton classification is premature, multiple perspectives experience rope characteristics. If performative: governance theater remains high, piton classification is accurate, marginal actors'' sense of exclusion is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_decentralization_authenticity, empirical, 'Authenticity of decentralized governance versus performance').

omega_variable(
    cultural_commons_externality,
    'What is the value of decentralized preservation to the cultural commons (diversity, resilience, accessibility) compared to the cost borne by marginal participants?',
    'Measurement of collection accessibility improvements; comparison of preservation outcomes in decentralized vs centralized regimes; valuation of cultural resilience and diversity benefits; analysis of distribution of these benefits across economic classes',
    'If commons value exceeds marginal costs: extraction mechanism is partially justified as subsidy for public goods. If commons value concentrates in wealthy institutions: extraction is pure rent-seeking disguised as collective benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_commons_externality, preference, 'Whether cultural commons benefits justify costs to marginal preservationists').

omega_variable(
    alternative_preservation_models,
    'Are there structurally distinct preservation models (federated, centralized-subsidized, hybrid) that would reduce extraction without losing coordination benefits?',
    'Comparative analysis of preservation models; simulation of cost and coordination outcomes under alternative incentive structures; historical case studies of successful preservation transitions',
    'If alternatives exist with lower extraction: current model is contingent choice, not natural necessity. Mandatrophy dissolves; constraint becomes policy decision, not immutable law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_preservation_models, conceptual, 'Whether alternative preservation models reduce extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decentralized_preservation_scalability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dpres_tr_t0, decentralized_preservation_scalability, theater_ratio, 0, 0.58).
narrative_ontology:measurement(dpres_tr_t3, decentralized_preservation_scalability, theater_ratio, 3, 0.61).
narrative_ontology:measurement(dpres_tr_t6, decentralized_preservation_scalability, theater_ratio, 6, 0.64).
narrative_ontology:measurement(dpres_tr_t9, decentralized_preservation_scalability, theater_ratio, 9, 0.64).

% Extraction over time
narrative_ontology:measurement(dpres_be_t0, decentralized_preservation_scalability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dpres_be_t3, decentralized_preservation_scalability, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(dpres_be_t6, decentralized_preservation_scalability, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(dpres_be_t9, decentralized_preservation_scalability, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decentralized_preservation_scalability, global_infrastructure).
narrative_ontology:boltzmann_floor_override(decentralized_preservation_scalability, 0.18).
narrative_ontology:affects_constraint(decentralized_preservation_scalability, knowledge_commons_extractiveness).
narrative_ontology:affects_constraint(decentralized_preservation_scalability, infrastructure_cost_asymmetry).
narrative_ontology:affects_constraint(decentralized_preservation_scalability, institutional_cultural_power).

% DUAL FORMULATION NOTE:
% Decentralized preservation scalability decomposes into technical redundancy coordination (approaching Mountain as cryptographic and cost improvements mature) and institutional access equity (Tangled Rope as long as cost asymmetry persists). The scaffold sunset applies to technical redundancy costs; institutional access equity remains unresolved by protocol improvements alone. Network edges link this constraint to upstream claims about cost curves (infrastructure_cost_asymmetry) and downstream effects on knowledge commons extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decentralized_preservation_scalability, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
