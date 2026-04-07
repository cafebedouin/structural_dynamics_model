% ============================================================================
% CONSTRAINT STORY: distributed_alternative_feasibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_distributed_alternative_feasibility, []).

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
 *   constraint_id: distributed_alternative_feasibility
 *   human_readable: Distributed Alternative Feasibility Constraint
 *   domain: political_economy/coordination
 *
 * SUMMARY:
 *   The distributed alternative feasibility constraint describes the
 *   structural impossibility (or extreme difficulty) of building and scaling
 *   alternatives to established centralized systems, even when the
 *   alternatives are technically superior or address legitimate grievances
 *   about incumbent behavior. This constraint operates across domains:
 *   monetary systems (fiat vs decentralized currency), communication
 *   platforms (proprietary vs open-source social networks), information
 *   architecture (search monopolies vs peer-to-peer indexing), cloud
 *   infrastructure (AWS dominance vs federated computing), and payment
 *   systems (visa/mastercard duopoly vs blockchain). The constraint exhibits
 *   a puzzling property: it is simultaneously a real coordination problem
 *   (distributed systems genuinely do face technical, governance, and
 *   network-effect challenges) and an extraction mechanism (incumbents
 *   benefit from the fictional impossibility of alternatives and may actively
 *   suppress them). The theater ratio reflects the degree to which
 *   'alternatives are not feasible' functions as a coordination narrative
 *   that justifies incumbent dominance versus genuine technical barriers. As
 *   distributed protocol maturity increases and examples of scaled
 *   alternatives emerge (Bitcoin, Ethereum, Signal, Mastodon), the theater
 *   rises — the coordination story persists even as its empirical foundation
 *   weakens, indicating degradation toward piton status.
 *
 * KEY AGENTS:
 *   - Exit Seekers: Powerless/trapped (individual_moderate/powerless) — attempt to migrate to alternatives, discover barriers (platform lock-in, network effects, regulatory friction) and revert to incumbent systems despite dissatisfaction
 *   - Alternative Advocates: Organized/constrained (organized/constrained) — build and promote decentralized alternatives; face resource disadvantages, coordination costs, and incumbent network advantage
 *   - Incumbent System Operators: Institutional/arbitrage (institutional/arbitrage) — benefit from coordination narrative and network effects; responsible for maintaining critical infrastructure
 *   - Distributed Protocol Pioneers: Powerful/mobile (powerful/mobile) — Bitcoin developers, P2P protocol designers, early adopters who have mobile exit options and can build alternatives
 *   - Regulatory Capture Actors: Institutional/arbitrage (institutional/arbitrage) — lobbying and policy efforts that formalize incumbent advantage through legal/technical standards
 *   - Analytical Observer: Civilizational/analytical (analytical/analytical) — risks naturalizing contingent institutional lock-in as immutable property of large systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(distributed_alternative_feasibility, 0.58).
domain_priors:suppression_score(distributed_alternative_feasibility, 0.62).
domain_priors:theater_ratio(distributed_alternative_feasibility, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(distributed_alternative_feasibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(distributed_alternative_feasibility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(distributed_alternative_feasibility, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(distributed_alternative_feasibility, tangled_rope).
narrative_ontology:human_readable(distributed_alternative_feasibility, "Distributed Alternative Feasibility Constraint").
narrative_ontology:topic_domain(distributed_alternative_feasibility, "political_economy/coordination").

domain_priors:requires_active_enforcement(distributed_alternative_feasibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(distributed_alternative_feasibility, incumbent_system_operators).
narrative_ontology:constraint_victim(distributed_alternative_feasibility, alternative_advocates).
narrative_ontology:constraint_victim(distributed_alternative_feasibility, exit_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXIT SEEKER (SNARE) — Individual or group attempting to leave the incumbent system discovers that viable alternatives do not exist at scale. Trapped by the constraint that no distributed alternative is simultaneously accessible, trustworthy, and functionally equivalent. Extraction operates through forced dependence on incumbent despite awareness of structural problems.
constraint_indexing:constraint_classification(distributed_alternative_feasibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE MOVEMENT ORGANIZERS (TANGLED ROPE) — Coalition building alternatives faces genuine coordination problems (aligning technical standards, building network effects, establishing trust) AND asymmetric extraction (incumbent system's network advantage, regulatory capture, installed base lock-in). Real coordination function plus real asymmetric extraction. Constrained by resource disparity and network effects, not trapped by total barriers.
constraint_indexing:constraint_classification(distributed_alternative_feasibility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INCUMBENT SYSTEM OPERATORS (ROPE) — Experience the constraint as pure coordination. Managing scale, reliability, and interoperability across heterogeneous actors is genuine coordination work. The constraint solves a real problem: maintaining standards and preventing fragmentation. Arbitrage access (can exit coordination by accepting fragmentation or vertical integration) means low experienced extraction.
constraint_indexing:constraint_classification(distributed_alternative_feasibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISTRIBUTED PROTOCOL PIONEERS (SCAFFOLD) — Powerful actors (technology leaders, early adopters, blockchain developers) see the constraint as a temporary coordination failure with built-in sunset. P2P protocols, federated architecture, and mesh networks represent genuinely achievable alternatives that don't require incumbent permission. High agency, mobile exit options, path-dependent sunset as protocols mature. Theater is moderate because some protocol governance remains performative (governance tokens that don't vote), but functionality is genuine.
constraint_indexing:constraint_classification(distributed_alternative_feasibility, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY DECENTRALIZATION RHETORIC (PITON) — Early decentralization movements (peer-to-peer file sharing, mesh networks, early blockchain) produced extensive governance theater without corresponding functional alternatives at scale. The theater persists through institutional inertia even where the underlying coordination problem has been solved by different mechanisms (cloud computing's reliability advantage, smart contracts' trust properties). Degraded through Goodhart substitution: 'decentralization' became a goal in itself rather than a solution to the underlying coordination problem.
constraint_indexing:constraint_classification(distributed_alternative_feasibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, some concentration of coordination function may be unavoidable: large systems require governance, liability allocation, and failure recovery that centralized actors execute more reliably than distributed consensus. The constraint appears to be a law of organizing large-scale systems. However, this naturalizes what is contingent: blockchain, mesh networks, and federated protocols demonstrate that distributed alternatives ARE feasible at scale — the mountain classification reflects observational bias toward existing incumbents rather than structural impossibility.
constraint_indexing:constraint_classification(distributed_alternative_feasibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(distributed_alternative_feasibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(distributed_alternative_feasibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(distributed_alternative_feasibility, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(distributed_alternative_feasibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(distributed_alternative_feasibility, TR),
    TR >= 0.70.

:- end_tests(distributed_alternative_feasibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Incumbents benefit substantially from the real difficulty of building distributed alternatives, but the extraction is not total — alternatives DO exist and operate at meaningful scale in some domains. The value reflects that the barrier is genuine but not insurmountable. The increasing trajectory (0.35→0.58 over interval) suggests that as incumbents consolidated advantage, the extractive premium rose even as alternative technology matured. Suppression (0.62): High. Multiple suppression mechanisms operate: network effects create switching costs; regulatory barriers created through lobbying; installed base lock-in; coordination costs for bootstrapping alternatives; information asymmetry about alternative viability; user habituation and switching friction. Suppression has multiple sources (technical, institutional, psychological) which compounds its strength. Theater ratio (0.55): Moderate-high but rising. 'Alternatives are not feasible' operates as a coordination narrative (genuine explanation for incumbent dominance) and as a cover story for extraction (justifies incumbent rent-taking). Theater rise reflects that newer alternatives (blockchain, federated social networks) demonstrate feasibility that contradicts the narrative, yet the narrative persists unchanged — classic Goodhart degradation where the story becomes maintenance mechanism rather than explanation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Exit seekers perceive a Snare — no way out, forced dependence. Organized alternative advocates see Tangled Rope — genuine coordination problems PLUS extraction advantage. Incumbents see Rope — solving a real coordination problem. Pioneers see Scaffold — temporary problem with achievable sunset via protocol maturity. Legacy decentralization rhetoric sees Piton — the governance story persists even as functional alternatives emerge. Analytical observer risks seeing Mountain — large systems 'need' centralization — but structural data reveals this as false naturalization. The divergence is not observational error but genuine structural difference in experienced constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. Exit seekers are pure victims (high d) — they bear the cost of being locked into systems they want to leave, with no realistic exit option. Alternative advocates have moderate-high d — they benefit from proving alternatives are viable but face resource extraction by incumbents. Incumbent operators have low d — the constraint subsidizes them; they arbitrage away from distributed coordination. Distributed protocol pioneers have the lowest d (near 0) — they have arbitrage mobility and benefit from proving alternatives work. The perspectival gap reflects that the constraint appears as pure extraction to powerless agents, genuine coordination to incumbents, temporary problem to pioneers, and naturalizable immutability to civilizational observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through recognition that distributed alternative feasibility is BOTH a real coordination problem AND an extraction mechanism. The coordination function is genuine — managing scale, reliability, and standards across heterogeneous actors is hard, and incumbents have solved it. The extraction is genuine — the solved coordination problem has become a moat that prevents better solutions from emerging. The resolution: the constraint is not 'is coordination needed?' but 'who benefits from the particular coordination solution we have locked ourselves into?' Mandatrophy prevention requires distinguishing between (a) coordination functions that all perspectives agree are necessary (low chi in Rope-range), (b) hybrid coordination-extraction where some perspectives see necessity and others see imposed dependency (Tangled Rope, chi mid-range), and (c) pure extraction masked as coordination (Snare, high chi). Measurement trajectory shows increasing extractiveness despite rising alternative maturity — this suggests the coordination function is being used as cover for increasing extraction, indicating Tangled Rope classification is appropriate and mandatrophy is NOT resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distributed_scale_feasibility,
    'At what system scale does distributed coordination become functionally inferior to centralized alternatives?',
    'Empirical comparison: throughput, latency, failure recovery, and operational cost for distributed vs centralized implementations at comparable scale. Cross-domain analysis (payment systems, file storage, compute platforms).',
    'If feasible at scale > 1B users: distributed alternative is genuinely viable, constraint is Tangled Rope / Scaffold. If feasible only at scale < 100M users: incumbent has structural advantage, constraint is Snare for most exit-seekers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_scale_feasibility, empirical, 'Threshold scale for distributed system viability').

omega_variable(
    network_effects_versus_technical_advantage,
    'How much of the incumbent''s advantage derives from network effects vs genuine technical/organizational superiority?',
    'Historical analysis of displaced incumbents (telegraph→telephone, myspace→facebook, flash→html5). Comparison of technical metrics vs adoption curves. User migration experiments when barriers to switching are reduced.',
    'If primarily network effects: alternatives ARE feasible but constrained by coordination/bootstrap problem (genuine Tangled Rope). If primarily technical: alternatives may not be feasible at equivalent quality (Snare has structural foundation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effects_versus_technical_advantage, empirical, 'Attribution of incumbent advantage to network effects vs technical superiority').

omega_variable(
    incumbent_capture_of_regulatory_space,
    'To what extent does the incumbent system''s regulatory position CREATE barriers to alternatives, vs merely REFLECT their operational superiority?',
    'Comparative analysis: jurisdictions with different regulatory regimes (EU vs US crypto policy, messaging app interoperability mandates). Documentation of regulatory lobbying efforts. Identification of technical barriers that predate regulatory barriers.',
    'If primarily regulatory capture: removing barriers reveals genuinely viable alternatives (Scaffold perspective). If regulatory barriers protect a genuinely superior system: alternatives remain unviable even if barriers fall (Snare foundations persist).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_capture_of_regulatory_space, empirical, 'Regulatory barriers as creation vs reflection of incumbent advantage').

omega_variable(
    trust_and_verification_in_distributed_systems,
    'Can distributed systems achieve the trust properties users require without reverting to centralized verification components?',
    'Analysis of successful distributed systems (Bitcoin, Ethereum, BitTorrent): what trust properties do they provide? What do users actually verify vs delegate? Where does de facto centralization creep in (mining pools, exchange custody, Infura RPC nodes)?',
    'If true distribution is feasible: alternatives are genuinely viable (Scaffold). If trust requires re-centralization: distributed alternatives have structural limits (Snare for trust-seeking agents).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trust_and_verification_in_distributed_systems, empirical, 'Whether distributed trust is achievable without implicit re-centralization').

omega_variable(
    incumbent_capacity_for_openness,
    'Can incumbent systems transition to genuinely open architectures (interoperability, portability, decentralization) without losing their coordination function?',
    'Case studies of incumbent system transitions (telephone network openness, internet''s TCP/IP adoption, web standards bodies). Economic analysis of whether openness improves or degrades coordination efficiency. Path dependency analysis — can incumbents change trajectory once locked in?',
    'If incumbents can transition: the constraint is Scaffold (sunset via incumbent evolution). If transition is structurally impossible: the constraint is Snare or Tangled Rope with no sunset (path-dependent lock-in).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_capacity_for_openness, empirical, 'Whether incumbents can transition to genuine openness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(distributed_alternative_feasibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(daf_tr_t0, distributed_alternative_feasibility, theater_ratio, 0, 0.42).
narrative_ontology:measurement(daf_tr_t5, distributed_alternative_feasibility, theater_ratio, 5, 0.49).
narrative_ontology:measurement(daf_tr_t10, distributed_alternative_feasibility, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(daf_be_t0, distributed_alternative_feasibility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(daf_be_t5, distributed_alternative_feasibility, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(daf_be_t10, distributed_alternative_feasibility, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(distributed_alternative_feasibility, global_infrastructure).
narrative_ontology:affects_constraint(distributed_alternative_feasibility, platform_network_effects).
narrative_ontology:affects_constraint(distributed_alternative_feasibility, regulatory_capture_in_technology).
narrative_ontology:affects_constraint(distributed_alternative_feasibility, switching_costs_lock_in).

% DUAL FORMULATION NOTE:
% Distributed alternative feasibility is decomposed from more specific domain constraints (payment system alternatives, social media alternatives, cloud infrastructure alternatives) each with their own ε values reflecting domain-specific feasibility. This story represents the cross-domain pattern: where genuine alternatives SHOULD exist (given technical maturity and user dissatisfaction) but don't at scale, indicating extraction mechanism rather than pure coordination barrier.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(distributed_alternative_feasibility, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
