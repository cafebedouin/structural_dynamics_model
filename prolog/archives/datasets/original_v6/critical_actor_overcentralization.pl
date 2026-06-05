% ============================================================================
% CONSTRAINT STORY: critical_actor_overcentralization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_critical_actor_overcentralization, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: critical_actor_overcentralization
 *   human_readable: The Single Point of Failure: Critical Actor Overcentralization
 *   domain: logistical/technological/economic
 *
 * SUMMARY:
 *   Critical actor overcentralization creates a structural paradox: the
 *   concentration that enables efficient coordination also creates
 *   catastrophic fragility. A single node—a clearinghouse bank processing
 *   $1.5 trillion daily, a cloud provider hosting 40% of internet
 *   infrastructure, a charismatic leader controlling a political
 *   party—becomes essential precisely because it solves a coordination
 *   problem that decentralized alternatives cannot (yet) solve. But the
 *   solution embeds a Snare: dependent actors have no exit. From the
 *   operator's perspective, the concentration is a Rope—they have solved a
 *   real coordination problem and capture legitimate rents. From the
 *   resilience perspective, it is a Tangled Rope: the network gains
 *   efficiency at the cost of tail-risk concentration. From the decentralized
 *   coalition's view, it is a Scaffold with a sunset: distributed
 *   alternatives (blockchain settlement, edge computing, peer-to-peer
 *   leadership) are maturing and will eventually make the central node
 *   redundant. The constraint's theater ratio (0.55) reflects that the
 *   operator and regulators maintain narratives about the necessity of
 *   concentration ('too complex to decentralize,' 'security requires
 *   centralization') that may or may not be technically justified. The
 *   extractiveness trend (0.35 → 0.58 over the interval) shows rent-seeking
 *   layered onto coordination as the network's dependency deepens and
 *   alternatives remain suppressed.
 *
 * KEY AGENTS:
 *   - Central Node Operator: Primary beneficiary (institutional/arbitrage) — captures coordination rents and control premium; minimum-cost operator because all traffic must route through them
 *   - Dependent Network Actors: Primary victims (powerless/trapped) — no alternative routing; forced reliance on single chokepoint with no exit
 *   - Resilience-Seeking Participants: Secondary victim (moderate/constrained) — benefit from network during stability but bear catastrophic tail-risk costs; constrained by high switching costs
 *   - Decentralization Coalition: Organized agents (organized/constrained) — blockchain developers, mesh network researchers, regulatory bodies mandating redundancy; building alternative infrastructure with sunset logic
 *   - Systemic Resilience: Implicit victim (powerless/trapped) — abstract collective good; fragility is invisible until the critical node fails
 *   - Regulatory Authorities: Institutional actor (institutional/constrained) — aware of concentration risk but constrained by coordination lock-in and switching costs to mandate alternatives; maintain theater that concentration is 'monitored' or 'manageable'
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(critical_actor_overcentralization, 0.58).
domain_priors:suppression_score(critical_actor_overcentralization, 0.68).
domain_priors:theater_ratio(critical_actor_overcentralization, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(critical_actor_overcentralization, extractiveness, 0.58).
narrative_ontology:constraint_metric(critical_actor_overcentralization, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(critical_actor_overcentralization, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(critical_actor_overcentralization, tangled_rope).
narrative_ontology:human_readable(critical_actor_overcentralization, "The Single Point of Failure: Critical Actor Overcentralization").
narrative_ontology:topic_domain(critical_actor_overcentralization, "logistical/technological/economic").

domain_priors:requires_active_enforcement(critical_actor_overcentralization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(critical_actor_overcentralization, central_node_operator).
narrative_ontology:constraint_beneficiary(critical_actor_overcentralization, network_participants_during_stability).
narrative_ontology:constraint_victim(critical_actor_overcentralization, dependent_network_actors).
narrative_ontology:constraint_victim(critical_actor_overcentralization, systemic_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT NETWORK ACTOR (SNARE) — No alternative routing exists. Failure of the central node means immediate operational collapse. The actor cannot diversify, cannot exit, and cannot organize alternatives without breaking the network itself. Maximum extraction — forced reliance on a single chokepoint.
constraint_indexing:constraint_classification(critical_actor_overcentralization, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESILIENCE-SEEKING PARTICIPANT (TANGLED ROPE) — Benefits from the network's coordination function during stability but bears catastrophic tail-risk costs. Constrained exit: building alternative infrastructure requires collective action and capital investment. Both coordination (network efficiency) and extraction (forced concentration risk) present.
constraint_indexing:constraint_classification(critical_actor_overcentralization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL NODE OPERATOR (ROPE) — Experiences the constraint as pure coordination. The operator solves a collective action problem by consolidating routing, settlement, or decision-making. Benefits flow directly to the operator (rent extraction, control), but the operator's perspective is that they are providing a valuable service that coordinates the network. This is not experienced as a constraint but as a solution.
constraint_indexing:constraint_classification(critical_actor_overcentralization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZATION COALITION (SCAFFOLD) — Organized efforts (blockchain developers, mesh network protocols, regulatory mandates for redundancy) aim to distribute the critical function across multiple nodes. Sees the single-point-of-failure architecture as a temporary problem with a known sunset: as alternative infrastructure matures, the dependency dissolves. Suppression is declining as technical barriers fall. Low effective extraction from this perspective because the coalition sees a path to exit.
constraint_indexing:constraint_classification(critical_actor_overcentralization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY HUB ARCHITECTURE (PITON) — The single-node design persists through path dependency and switching costs, not because it is functionally optimal. Modern network theory shows distributed topologies are more resilient. Yet the hub persists: regulations are written around it, legacy systems depend on it, institutional inertia maintains it. Theater ratio (0.55) reflects the gap between the nominal function (efficient routing) and actual function (regulatory compliance theater, risk concentration that is officially invisible).
constraint_indexing:constraint_classification(critical_actor_overcentralization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NETWORK THEORY VIEW (MOUNTAIN) — From a civilizational perspective, some degree of hierarchical structure is inherent to large-scale coordination systems: networks with N nodes require at least log(N) layers of aggregation, and bottlenecks are an unavoidable feature of hierarchical architectures. This perspective sees single points of failure as a natural law of centralized systems. However, the structural data contradicts this: distributed consensus protocols, mesh networks, and redundant routing all achieve scale without single points of failure. The 'natural law' framing naturalizes a design choice, not a physical limit.
constraint_indexing:constraint_classification(critical_actor_overcentralization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(critical_actor_overcentralization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(critical_actor_overcentralization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(critical_actor_overcentralization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(critical_actor_overcentralization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(critical_actor_overcentralization, TR),
    TR >= 0.70.

:- end_tests(critical_actor_overcentralization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The central operator extracts coordination rent (they are the only option), but the extraction is not maximal (0.70+) because the network genuinely benefits from the coordination function during stability. The operator captures margin from transaction fees, data access, or control premium, but the underlying network service is real. The trend toward 0.58 reflects both the operator's increasing bargaining power and the layering of surveillance/data-extraction features as the dependency deepens. Suppression (0.68): High. Multiple mechanisms suppress alternatives: switching costs lock in dependent actors, technical knowledge is concentrated in the operator (tacit knowledge barrier), regulatory approval processes favor the incumbent (regulatory capture), and competitive dynamics are suppressed by the network effect (a decentralized system is valuable only if everyone switches simultaneously). Dependent actors cannot feasibly exit. Theater ratio (0.55): Moderate. The operator and regulators maintain narratives about the optimality of concentration ('security requires centralization,' 'redundancy is inefficient,' 'trust networks must have a hub') that are partially performative. Technical analysis shows that distributed alternatives can achieve similar security and efficiency with different trade-offs. The theater ratio is not as high as a Piton (0.70+) because the coordination function is genuinely valuable — it is not purely ritual. But the theater reflects suppressed alternatives and invisible risk concentration.
 *
 * PERSPECTIVAL GAP:
 *   The operator sees coordination (Rope) because they are solving a genuine problem. The dependent actor sees extraction (Snare) because they have no exit. The decentralization coalition sees a temporary problem with a sunset (Scaffold) because they are building distributed alternatives that will mature in 5-15 years. The regulatory authority sees degraded theater (Piton) because they maintain narratives about managing concentration that are only partially effective. The analytical observer sees a natural law of hierarchies (Mountain) but this is a false summit—distributed architectures prove that scale does not require central nodes.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality (d) from each agent's structural position. The central operator is a beneficiary with arbitrage exit options (d ≈ 0.05, low extraction experienced). The dependent actor is a victim with trapped exit options (d ≈ 0.95, maximum extraction experienced). The resilience-seeker is a victim with constrained exit options (d ≈ 0.70, high extraction experienced). The regulatory authority is an institutional actor with constrained exit—they would prefer to mandate decentralization but are themselves locked into the coordination system (d ≈ 0.55, moderate-high extraction experienced). The directionality chain reflects that concentrated topology creates asymmetric exit: the operator can leave (or threaten to leave) and the network collapses; dependent actors cannot leave without coordinating a simultaneous migration. This asymmetry is the mechanism of extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL CASE: The mandatrophy is resolved by showing that the single-point-of-failure constraint can be EITHER a Rope (pure coordination) OR a Snare (pure extraction) depending on whether decentralized alternatives are technically feasible. If decentralization is infeasible, the operator's rent is justified coordination cost, and the constraint is a Rope from all perspectives—the operator should be rewarded for solving an unsolvable problem. If decentralization is feasible, the operator is actively maintaining centralization to preserve extraction, and the constraint is a Snare from the dependent actor's perspective and a Tangled Rope from the resilience perspective—extraction is being layered onto a coordination problem that could be solved with lower friction cost via distribution. The omega variables operationalize this distinction: ALTERNATIVE_ARCHITECTURE_FEASIBILITY determines whether the operator has a genuine monopoly or is choosing to extract. The measured extractiveness trend (0.35 → 0.58) suggests that extraction is increasing faster than the coordination function is maturing, which implies the operator is gradually shifting from a Rope (solving a problem) to a Snare-adjacent (exploiting a dependency). Resolution requires empirical evidence on feasibility and operator investment patterns.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_mass_threshold_disruption,
    'What percentage of network participants must defect or be unable to access the central node before the constraint transitions from extraction to systemic collapse?',
    'Empirical observation of historical failures (2008 bank runs, 2011 DNS attacks, AWS regional outages); threshold identification from network simulation models',
    'If threshold < 5%: single-node system is fragile even under normal variation. If threshold > 25%: operator has significant buffer and can exploit near-monopoly pricing. Affects classification from participant perspective: lower threshold → higher experienced extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_threshold_disruption, empirical, 'Percentage of participants at which central node failure causes systemic collapse').

omega_variable(
    alternative_architecture_feasibility,
    'Is decentralization technically and economically feasible for this specific network function, or are the operator''s claims about necessity justified?',
    'Comparison of operational costs: hub-and-spoke vs mesh/distributed; security analysis of alternative topologies; historical case studies (SWIFT vs blockchain settlement, centralized DNS vs distributed naming)',
    'If alternative is feasible: constraint is institutional choice, not technical necessity. Scaffold perspective is correct — sunset is real. If technically infeasible: operator claims are valid, and what appears as extraction is legitimate coordination rent. Affects classification from resilience-seeker perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_architecture_feasibility, empirical, 'Whether decentralized alternatives are technically feasible for this network function').

omega_variable(
    operator_capture_vs_coordination,
    'Is the central operator actively maintaining the single-point-of-failure architecture because it maximizes their extraction, or because distributed alternatives have genuine technical/economic disadvantages they cannot overcome?',
    'Analysis of operator investment patterns (e.g., do they invest in redundancy/resilience or only in consolidation?); comparison of operator margins in competitive vs monopoly positions; survey of technical expertise distribution',
    'If capture is true: constraint is a Snare from the operator''s perspective (they are choosing extraction). If technical barriers are genuine: constraint is a Rope (they are solving a real coordination problem). Affects whether mandatrophy is resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operator_capture_vs_coordination, empirical, 'Whether operator maintains SPOF architecture for rent extraction or technical necessity').

omega_variable(
    systemic_fragility_visibility,
    'Are the risks of single-point-of-failure architecture visible to participants and regulators, or is risk concentration hidden behind technical opacity and theater?',
    'Audit of transparency in operator disclosures (stress tests, redundancy levels, failure mode documentation); analysis of regulatory filings and stress-test assumptions; participant surveys on risk awareness',
    'If risk is hidden: theater_ratio should be higher, suppression should be higher, and the snare classification is correct. If risk is visible: participants are consciously accepting centralization for coordination benefits, and the rope perspective is more accurate. Affects theater_ratio scoring.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_fragility_visibility, empirical, 'Whether SPOF risks are transparent or hidden from participants and regulators').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(critical_actor_overcentralization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spof_tr_t0, critical_actor_overcentralization, theater_ratio, 0, 0.4).
narrative_ontology:measurement(spof_tr_t5, critical_actor_overcentralization, theater_ratio, 5, 0.48).
narrative_ontology:measurement(spof_tr_t10, critical_actor_overcentralization, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(spof_be_t0, critical_actor_overcentralization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(spof_be_t5, critical_actor_overcentralization, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(spof_be_t10, critical_actor_overcentralization, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(critical_actor_overcentralization, global_infrastructure).
narrative_ontology:affects_constraint(critical_actor_overcentralization, systemic_fragility_cascades).
narrative_ontology:affects_constraint(critical_actor_overcentralization, distributed_alternative_feasibility).
narrative_ontology:affects_constraint(critical_actor_overcentralization, regulatory_capture_in_finance).

% DUAL FORMULATION NOTE:
% Single-point-of-failure is structurally distinct from systemic_fragility_cascades. SPOF addresses the topology (one node, all paths), while systemic_fragility addresses the dynamics (correlation and contagion). SPOF is upstream: if SPOF is resolved via decentralization, systemic_fragility becomes harder to trigger. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
