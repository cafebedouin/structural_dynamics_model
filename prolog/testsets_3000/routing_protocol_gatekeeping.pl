% ============================================================================
% CONSTRAINT STORY: routing_protocol_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_routing_protocol_gatekeeping, []).

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
 *   constraint_id: routing_protocol_gatekeeping
 *   human_readable: Routing Protocol Gatekeeping in Internet Infrastructure
 *   domain: telecommunications/internet_governance
 *
 * SUMMARY:
 *   Routing protocol gatekeeping represents a structural constraint in
 *   internet infrastructure where the approval and standardization of routing
 *   algorithms is controlled by formal standards bodies (primarily IETF and
 *   IEEE) whose governance and incentive structures favor incumbent router
 *   manufacturers and large ISP operators. This creates a hybrid
 *   coordination-extraction dynamic: genuine need for standardized protocols
 *   to ensure global interoperability coexists with asymmetric extraction
 *   where emerging developers and edge providers cannot deploy superior
 *   protocols without standards approval. The constraint demonstrates all
 *   eight perspectives, revealing how the same structural phenomenon appears
 *   as coordination (standards body view), pure extraction (trapped emerging
 *   developer), mixed experience (edge provider), temporary problem
 *   (open-source coalition), and false natural law (analytical view).
 *   Extractiveness has increased from 0.42 to 0.58 over the measurement
 *   interval as standards approval timelines have lengthened and incumbent
 *   interests have consolidated. Theater ratio remains moderate (0.48)
 *   because the standards process, while politically influenced, maintains
 *   genuine technical evaluation rather than pure performative theater.
 *
 * KEY AGENTS:
 *   - Emerging Protocol Developer: Primary victim (powerless/trapped) — cannot deploy new routing protocols without standards approval; no alternative deployment pathway available
 *   - Edge Network Provider: Secondary victim (moderate/constrained) — derives coordination benefit from standards but faces extraction through regional protocol suppression; constrained exit (can self-deploy but loses peering)
 *   - Standards Body (IETF/IEEE): Primary beneficiary (institutional/arbitrage) — controls gatekeeping mechanism; maintains legitimate coordination function but benefits from incumbent influence
 *   - Incumbent Router Manufacturer: Primary beneficiary (institutional/arbitrage) — locks in hardware designs through standards; can delay competing protocols
 *   - Large ISP Operator: Ambiguous (powerful/mobile) — both benefits from coordination (backbone stability) and profits from gatekeeping (suppresses regional competitors); chooses to maintain extraction
 *   - Open-Source Routing Coalition: Organized exit-builder (organized/mobile) — building parallel validation and deployment pathways with sunset logic; visible exit for organized agents
 *   - Network Innovation Capacity: Structural victim (powerless/trapped) — abstract collective good of routing innovation is suppressed; has no voice or exit mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(routing_protocol_gatekeeping, 0.58).
domain_priors:suppression_score(routing_protocol_gatekeeping, 0.65).
domain_priors:theater_ratio(routing_protocol_gatekeeping, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(routing_protocol_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(routing_protocol_gatekeeping, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(routing_protocol_gatekeeping, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(routing_protocol_gatekeeping, tangled_rope).
narrative_ontology:human_readable(routing_protocol_gatekeeping, "Routing Protocol Gatekeeping in Internet Infrastructure").
narrative_ontology:topic_domain(routing_protocol_gatekeeping, "telecommunications/internet_governance").

domain_priors:requires_active_enforcement(routing_protocol_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(routing_protocol_gatekeeping, protocol_standardization_bodies).
narrative_ontology:constraint_beneficiary(routing_protocol_gatekeeping, incumbent_router_manufacturers).
narrative_ontology:constraint_beneficiary(routing_protocol_gatekeeping, large_isp_operators).
narrative_ontology:constraint_victim(routing_protocol_gatekeeping, emerging_protocol_developers).
narrative_ontology:constraint_victim(routing_protocol_gatekeeping, edge_providers).
narrative_ontology:constraint_victim(routing_protocol_gatekeeping, network_innovation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING PROTOCOL DEVELOPER (SNARE) — New routing protocols cannot gain deployment without approval from standardization bodies (IETF, IEEE) controlled by incumbent firms. Developer has no exit: must either conform to gatekeeping or remain undeployed. Trapped by both structural (deployment infrastructure is monopolized) and suppressive (standards process favors incumbent interests) mechanisms. Zero degrees of freedom.
constraint_indexing:constraint_classification(routing_protocol_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EDGE NETWORK PROVIDER (TANGLED ROPE) — Small ISP or regional carrier derives genuine coordination benefit from standardized routing (ensures interoperability with backbone infrastructure) but faces extraction through gatekeeping: cannot deploy optimized protocols for their region without standards approval. Constrained exit — can self-deploy but loses peering relationships and global reach. Mixed experience: real coordination need + asymmetric extraction.
constraint_indexing:constraint_classification(routing_protocol_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STANDARDS BODY (ROPE) — Genuine coordination function: standardized routing protocols solve collective action problem (interoperability, scale, reliability). From the standards body's view, gatekeeping is necessary and legitimate oversight. Net beneficiary position: controls the approval process, benefits from the legitimacy of standards role. Arbitrage exit — can threaten to fragment standards if pressured, so maintains independent power.
constraint_indexing:constraint_classification(routing_protocol_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INCUMBENT ROUTER MANUFACTURER (ROPE) — Captures coordination benefits of standardized protocols (guaranteed market demand, interoperability requirements increase hardware sales). Gatekeeping benefits them: can delay competing protocol adoption, lock in existing hardware designs. Arbitrage exit — can switch protocol support if standards shift, so maintains strategic mobility despite gatekeeping role.
constraint_indexing:constraint_classification(routing_protocol_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN-SOURCE ROUTING COALITION (SCAFFOLD) — Organized developers (Linux kernel, OpenDaylight, ONAP communities) are building parallel routing validation pathways: open-source routers can test and deploy protocols independently of standards approval. This creates a sunset path: as open hardware routing matures, the standards gatekeeping mechanism loses force. Temporary constraint with exit visible to organized agents.
constraint_indexing:constraint_classification(routing_protocol_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LARGE ISP OPERATOR (TANGLED ROPE) — Major carriers (AT&T, Verizon, China Telecom) both benefit from and perpetuate gatekeeping. Genuine coordination need: standardized routing ensures backbone stability and vendor interoperability. But they also benefit from gatekeeping — it prevents competitors from deploying optimized regional protocols. Mobile exit (can lobby standards bodies, build proprietary overlay networks) but strategically chooses to maintain gatekeeping. Mixed beneficiary-target position: benefits from coordination, profits from extraction.
constraint_indexing:constraint_classification(routing_protocol_gatekeeping, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: NETWORK INNOVATION CAPACITY (SNARE) — Abstract collective good (the rate and diversity of routing protocol innovation) is trapped and bears maximum extraction. Gatekeeping suppresses alternative protocols even when they have superior properties (latency, energy efficiency, convergence speed). The innovation capacity has no voice, no exit, no organized representation. Structural victim of the constraint.
constraint_indexing:constraint_classification(routing_protocol_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal scope, standardization is inherent to networked systems: any global-scale coordination problem requires some gatekeeper function to prevent chaos and ensure universal interoperability. This perspective risks naturalizing what is actually a contingent institutional arrangement (which body controls the gate, whose interests it serves). The engine's false summit detector will flag this — network standardization could operate under different governance models.
constraint_indexing:constraint_classification(routing_protocol_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(routing_protocol_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(routing_protocol_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(routing_protocol_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(routing_protocol_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(routing_protocol_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The standards gatekeeping mechanism does perform genuine coordination (ensures interoperability, prevents chaos), but the extraction component is substantial. Emerging developers face 2-4 year standardization timelines even for technically superior protocols. Alternative deployments without standards approval lose 40-60% of potential adoption due to peering restrictions. The extractiveness has increased over the interval as standards timelines have lengthened and incumbent consolidation has strengthened. Suppression (0.65): Moderate-high. Suppression includes both structural barriers (deployment infrastructure controlled by standard-compliant vendors) and procedural barriers (RFC approval process, RFC editor selection biases, voting structures that favor incumbent interests). Emerging developers have limited technical barriers to protocol design but face insurmountable institutional barriers to deployment. Theater ratio (0.48): Moderate-low. The standards process does include genuine technical evaluation of routing protocol correctness, security, and performance. However, political evaluation (alignment with incumbent interests) is also significant. Theater is lower than peer review in academic publishing or regulatory review in finance, suggesting that technical merit is genuinely contested alongside political interests.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates fundamental perspectival divergence. The standards body and incumbent manufacturers see Rope — legitimate coordination mechanism ensuring global interoperability. The emerging developer sees Snare — trapped by gatekeeping with no exit. The edge provider sees Tangled Rope — genuine coordination need mixed with asymmetric extraction. The open-source coalition sees Scaffold — temporary problem being solved by alternative deployment pathways. The large ISP sees Tangled Rope but with beneficiary weighting (benefits from coordination, profits from gatekeeping, maintains strategic control over which protocols compete). The civilizational analytical observer risks seeing Mountain (standardization is inherent to networked systems) but structural data reveals this as false summit — alternative governance models exist (federated standards, guild-based approval, competitive protocol ecosystems with graceful degradation). The perspectival gaps reveal that all six DR types are simultaneously true from different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) reflect each agent's structural position relative to the extraction flow. Emerging developers are pure targets (d=0.95): no power, no exit, bear full institutional suppression cost. Edge providers are moderate targets (d=0.65): constrained exit (self-deployment costs them peering), moderate power (regional leverage), but genuine coordination need. Standards body and incumbent manufacturers are beneficiaries (d=0.05-0.15): institutional power, arbitrage exit (can lobby standards changes), capture coordination legitimacy. Large ISPs are ambiguous beneficiaries (d=0.30-0.50): powerful (can influence standards), mobile exit (can build overlays), but strategically choose extraction. Open-source coalition is organized but has mobile exit (d=0.45), reducing experienced extraction through visible alternative pathway. The network innovation capacity is abstract victim (d=0.98): structurally trapped, no representation, maximum suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves mandatrophy by demonstrating why Tangled Rope (not pure Rope) is the correct classification. Pure Rope would require minimal asymmetric extraction (victims absent or minimal). But emerging protocol developers and network innovation capacity are genuine victims bearing substantial costs. The coordination function is also genuine — standards do solve interoperability problems. The constraint exhibits both: (1) active enforcement (standards bodies police deployment of non-compliant routing), (2) beneficiaries (incumbent manufacturers, large ISPs), (3) victims (emerging developers, edge providers, innovation capacity), and (4) significant suppression (institutional barriers, timeline delays, peering restrictions). Mandate loop prevented: false classification as pure Rope (merely coordination) would hide the extraction component; false classification as Snare (merely extraction) would hide the genuine coordination benefit. Tangled Rope captures the hybrid structure accurately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_merit_vs_gatekeeping_incentive,
    'Do standards bodies reject protocols primarily on technical merit or on alignment with incumbent manufacturer interests?',
    'Comparative analysis: protocol rejection rates by technical category; correlation between rejection reason (performance, compatibility, security) and incumbent manufacturer impact; post-deployment security/performance data on rejected vs approved protocols',
    'If rejection rate highly correlates with incumbent harm: gatekeeping is primarily extractive. If rejection aligns with genuine technical risk: gatekeeping is primarily coordination. Currently shows mixed signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_merit_vs_gatekeeping_incentive, empirical, 'Whether rejections reflect technical merit or incumbent interests').

omega_variable(
    open_source_viability_as_alternative,
    'Can open-source routers and software-defined networking genuinely displace proprietary standards gatekeeping?',
    'Market adoption analysis: deployment share of open-source routing (Linux kernel, ONAP, OpenDaylight) vs standards-approved proprietary protocols; cost trajectories and feature parity; peering agreements that accept open-source routing',
    'If open-source achieves 30%+ deployment: scaffold sunset is real and extractiveness ceiling is 0.45. If open-source remains niche: gatekeeping persists and extractiveness remains 0.58+. Current estimate: 15-20% adoption in data center, 5% in ISP backbone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_source_viability_as_alternative, empirical, 'Viability of open-source routing as escape from standards gatekeeping').

omega_variable(
    network_effects_unavoidability,
    'Is the gatekeeping-like centralization of routing standards inevitable due to network effects, or is it a contingent institutional choice?',
    'Comparative case study: internet routing vs other complex multi-vendor domains (electricity grids, telecommunications standards) with different governance models; simulation of alternative standards governance structures',
    'If inevitable: mountain classification correct (gatekeeping is inherent to networked systems). If contingent: false summit — institution is treating a policy choice as natural law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(network_effects_unavoidability, conceptual, 'Whether gatekeeping is inevitable network effect or contingent institutional choice').

omega_variable(
    incumbent_manufacturer_dependency,
    'Could large ISPs and edge providers deploy alternative protocols at scale if they chose to, or are they actually dependent on incumbent manufacturer compliance?',
    'Technical capability audit: which ISPs have in-house routing expertise and hardware sourcing alternatives; case studies of past ISP attempts to deploy non-standard protocols; network simulation showing protocol diversity without manufacturer support',
    'If ISPs have technical capacity: victim status is choice rather than structural (exit_options upgrade from trapped to constrained or mobile for sophisticated operators). If dependent: gatekeeping is truly suppressive and extractiveness increases to 0.65+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_manufacturer_dependency, empirical, 'Whether ISPs are dependent on incumbent manufacturers or have exit capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(routing_protocol_gatekeeping, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rpg_tr_t0, routing_protocol_gatekeeping, theater_ratio, 0, 0.38).
narrative_ontology:measurement(rpg_tr_t5, routing_protocol_gatekeeping, theater_ratio, 5, 0.45).
narrative_ontology:measurement(rpg_tr_t10, routing_protocol_gatekeeping, theater_ratio, 10, 0.48).
narrative_ontology:measurement(rpg_tr_t15, routing_protocol_gatekeeping, theater_ratio, 15, 0.52).

% Extraction over time
narrative_ontology:measurement(rpg_be_t0, routing_protocol_gatekeeping, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rpg_be_t5, routing_protocol_gatekeeping, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(rpg_be_t10, routing_protocol_gatekeeping, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(rpg_be_t15, routing_protocol_gatekeeping, base_extractiveness, 15, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(routing_protocol_gatekeeping, information_standard).
narrative_ontology:boltzmann_floor_override(routing_protocol_gatekeeping, 0.12).
narrative_ontology:affects_constraint(routing_protocol_gatekeeping, bgp_hijacking_vulnerability).
narrative_ontology:affects_constraint(routing_protocol_gatekeeping, internet_routing_centralization).
narrative_ontology:affects_constraint(routing_protocol_gatekeeping, protocol_innovation_bottleneck).

% DUAL FORMULATION NOTE:
% Routing protocol gatekeeping is upstream of specific vulnerabilities (BGP hijacking, routing centralization) and the broader innovation bottleneck in networking. Each downstream constraint has its own ε value reflecting its specific empirical status; the gatekeeping constraint represents the institutional mechanism that produces the bottleneck.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(routing_protocol_gatekeeping, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
