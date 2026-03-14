% ============================================================================
% CONSTRAINT STORY: ergo_oracle_price_discovery
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_oracle_price_discovery, []).

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
 *   constraint_id: ergo_oracle_price_discovery
 *   human_readable: Ergo Oracle Price Discovery Constraint
 *   domain: blockchain/oracle/financial_infrastructure
 *
 * SUMMARY:
 *   The Ergo oracle system enables price discovery for decentralized finance
 *   by aggregating price feeds from multiple sources and injecting them
 *   on-chain. This coordination mechanism solves a genuine problem: smart
 *   contracts need reliable price data to execute trades, collateral
 *   calculations, and liquidations. However, the oracle operator model
 *   creates an extractive layer: users become dependent on oracle operators
 *   for access to price data, operators can influence prices by selecting
 *   source components, and users have no exit mechanism without abandoning
 *   DeFi participation entirely. The constraint exhibits asymmetric
 *   extraction (operators benefit, users trapped) alongside genuine
 *   coordination functionality (price discovery works), making it a canonical
 *   tangled rope. Theater has increased over the measurement interval as the
 *   system's mechanical complexity obscures the underlying extraction
 *   mechanism — oracle operations appear as neutral technical infrastructure
 *   rather than as a profit-capture layer.
 *
 * KEY AGENTS:
 *   - Oracle Operators: Primary beneficiary (institutional/arbitrage) — capture fees, MEV, and data priority. Can exit costlessly to competing systems.
 *   - Smart Contract Users: Primary victim (powerless/trapped) — dependent on oracle price feeds, cannot verify independently, cannot exit without abandoning DeFi, subject to price manipulation.
 *   - Protocol Governance (DeFi DAOs): Secondary beneficiary (organized/constrained) — benefits from oracle functionality but trapped by path-dependency of existing smart contracts depending on oracle data.
 *   - Alternative Oracle Projects: Competing systems (moderate/constrained) — face network effects and switching costs, coordinated by same price discovery problem, experience mixed coordination and extraction.
 *   - Analytical Observer: Systemic view (analytical/analytical) — sees hybrid coordination-extraction structure, risks naturalizing oracle architecture as technological necessity rather than institutional design choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_oracle_price_discovery, 0.52).
domain_priors:suppression_score(ergo_oracle_price_discovery, 0.58).
domain_priors:theater_ratio(ergo_oracle_price_discovery, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_oracle_price_discovery, extractiveness, 0.52).
narrative_ontology:constraint_metric(ergo_oracle_price_discovery, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ergo_oracle_price_discovery, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_oracle_price_discovery, tangled_rope).
narrative_ontology:human_readable(ergo_oracle_price_discovery, "Ergo Oracle Price Discovery Constraint").
narrative_ontology:topic_domain(ergo_oracle_price_discovery, "blockchain/oracle/financial_infrastructure").

domain_priors:requires_active_enforcement(ergo_oracle_price_discovery).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_oracle_price_discovery, oracle_operators).
narrative_ontology:constraint_beneficiary(ergo_oracle_price_discovery, protocol_developers).
narrative_ontology:constraint_victim(ergo_oracle_price_discovery, smart_contract_users).
narrative_ontology:constraint_victim(ergo_oracle_price_discovery, price_discovery_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMART CONTRACT END USERS (SNARE) — Trapped by oracle dependency. Users cannot verify price feeds independently; cannot exit without forgoing DeFi participation. Suppression is structural: no alternative price sources exist at required scale/speed. Experience maximum extraction: dependent on operators' price feeds, subject to manipulation, with no recourse mechanism. Cannot organize exit because the trap is technological, not contractual.
constraint_indexing:constraint_classification(ergo_oracle_price_discovery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE ORACLE PROJECTS (TANGLED ROPE) — Constrained by network effects and switching costs. Coordination function exists: oracle aggregation does solve genuine price-discovery problem. But asymmetric extraction: must match Ergo's liquidity and speed standards, cannot fully exit without losing market relevance. Experience mixed coordination and extraction. Can organize collectively but face high cost of defection.
constraint_indexing:constraint_classification(ergo_oracle_price_discovery, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ERGO ORACLE OPERATORS (ROPE) — Arbitrage-capable institutional actors. Benefit from operator fees, MEV capture, and priority data access. Experience constraint as pure coordination mechanism: pricing mechanism aggregates information efficiently. Can exit costlessly to competing oracle systems. Net beneficiaries — extraction runs toward this agent.
constraint_indexing:constraint_classification(ergo_oracle_price_discovery, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEFI PROTOCOL GOVERNANCE (TANGLED ROPE) — Organized but constrained by oracle lock-in. Coordination function: oracle system enables DeFi markets to function. But extraction: cannot downgrade oracle standards without breaking smart contract assumptions. Constrained by path-dependency of deployed contracts. See benefits (market functionality) and costs (dependency) simultaneously.
constraint_indexing:constraint_classification(ergo_oracle_price_discovery, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL PRICE REFERENCE SYSTEMS (PITON) — Legacy centralized price feeds (Bloomberg, Reuters) maintain institutional presence but declining functional role in DeFi. The oracle mechanism substitutes traditional references without fully replacing them — theater of institutional legacy authority persists despite decentralized alternatives working. Maintained through regulatory familiarity, not superior functionality. High theater_ratio reflects performative role.
constraint_indexing:constraint_classification(ergo_oracle_price_discovery, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine price discovery coordination alongside extractive oracle operator consolidation. Both are structurally real: aggregation solves information problem AND concentration creates rentier mechanism. Classification depends on whether extraction or coordination dominates — current metrics suggest tangled hybrid (0.52 extractiveness, genuine coordination function). Risk of naturalizing oracle consolidation as inevitable technical requirement rather than institutional choice.
constraint_indexing:constraint_classification(ergo_oracle_price_discovery, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_oracle_price_discovery_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergo_oracle_price_discovery, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_oracle_price_discovery, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ergo_oracle_price_discovery, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ergo_oracle_price_discovery, TR),
    TR >= 0.70.

:- end_tests(ergo_oracle_price_discovery_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Oracle operators capture fees for price data provision, but genuine service is provided — aggregation does improve price signal quality relative to single-source feeds. Not maximal extraction because there is real coordination benefit. Theater ratio (0.64): High and rising. Over the measurement interval, the oracle mechanism's complexity (MEV extraction, operator selection, fee mechanics) has become less transparent, creating theater — users see 'oracle prices' as given rather than understanding the operator selection and fee layer. Suppression (0.58): Moderate-high. Users face structural barriers to exit (smart contract dependency, network effects, lack of alternative price sources), but not absolute immobility — migration to alternative oracle systems or on-chain price sources is technically possible at high cost. Claimed type (Tangled Rope): Required by structure — genuine coordination function (price discovery aggregation) + asymmetric extraction (operator fee capture and price influence) + requires active enforcement (oracle operators actively maintain price feed infrastructure).
 *
 * PERSPECTIVAL GAP:
 *   Oracle operators see a coordination mechanism (Rope) — they provide a service that enables DeFi functionality. Smart contract users see a trap (Snare) — they are dependent on operators with no exit. DeFi protocols see mixed coordination and constraint (Tangled Rope) — they need the oracle functionality but cannot change oracle implementations without redeploying contracts. Alternative oracle projects see coordination with asymmetric costs (Tangled Rope) — they solve the same problem but face switching costs. Traditional price reference systems see their role as degraded and performative (Piton) — Bloomberg/Reuters data remains cited in documentation but actual price discovery happens in oracle aggregation. The analytical observer sees the whole structure (Tangled Rope) but risks naturalizing oracle operator consolidation as inevitable, missing that decentralized or more transparent price discovery mechanisms could exist if institutional choices were different.
 *
 * DIRECTIONALITY LOGIC:
 *   Oracle operators derive beneficiary status + arbitrage exit options (can switch to competing oracle systems costlessly) → low d → negative or near-zero χ contribution. They experience the constraint as pure coordination (Rope perspective). Smart contract users derive victim status + trapped exit (cannot exit without forgoing DeFi) → high d → high f(d) → high χ. They experience maximum extraction despite coordination function existing. Alternative oracle projects derive moderate power + constrained exit (can build alternatives but face network effects) → moderate d → moderate χ. Protocol governance derives organized power + constrained exit (locked in by deployed contracts) → moderate-high d → moderate χ. The perspectival gap emerges because operators' arbitrage exit inverts the directionality derivation chain relative to users' trapped exit. Same constraint, opposite structural positions, opposite experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how a genuine coordination mechanism can be weaponized into an extraction mechanism through asymmetric exit options. The oracle system solves a real problem (price discovery in decentralized systems) — that is the rope component. But operator consolidation and user lock-in create extraction — that is the snare component. The mandatrophy is resolved by recognizing both are structurally true. The classification is not 'is this rope or snare?' but 'rope with embedded snare,' where the snare component is enabled by architectural lock-in (smart contracts cannot easily change oracle sources) rather than by lack of coordination benefit. The rising theater ratio over the measurement interval indicates that the extraction mechanism is becoming more opaque — what was visible operator fee capture is increasingly obscured by complexity (MEV extraction, batching, keeper economics). This is a drift toward piton territory if the trend continues: the oracle system maintains formal coordination role while actual function (transparent price discovery) becomes theater for fee capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    price_feed_independence_boundary,
    'At what point does oracle aggregation become extractive consolidation rather than coordination mechanism?',
    'Measurement of price feed correlation; analysis of whether aggregation reduces information diversity or merely pools it; comparison of aggregated vs constituent feed accuracy',
    'If aggregation increases information redundancy: primarily coordination (Rope from more perspectives). If aggregation creates correlation where none existed: primarily extraction (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_feed_independence_boundary, empirical, 'Threshold between aggregation-as-coordination and aggregation-as-consolidation').

omega_variable(
    operator_incentive_alignment,
    'Are oracle operator fees compensating for genuine service or capturing economic rent from locked-in users?',
    'Cost-benefit analysis of oracle infrastructure; comparison of operator revenue to infrastructure costs; measurement of fee elasticity with respect to user lock-in',
    'If fees match infrastructure costs: rope classification justified (coordination benefit exceeds extraction). If fees exceed costs inversely proportional to user exit options: snare classification justified (pure extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operator_incentive_alignment, empirical, 'Whether oracle operator fees reflect service value or rent extraction').

omega_variable(
    decentralized_oracle_viability,
    'Can fully decentralized price discovery mechanisms (fully on-chain, no operator role) achieve required speed and accuracy, or are oracle operators a necessary technological layer?',
    'Technical feasibility study of pure blockchain-based price discovery; empirical latency and accuracy metrics vs oracle-dependent systems; institutional readiness assessment',
    'If viable: oracle consolidation is institutional choice (extraction), classification shifts toward snare. If not viable: oracle operator role is technological necessity, classification toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_oracle_viability, empirical, 'Technological necessity vs institutional choice in oracle operator role').

omega_variable(
    smart_contract_composability_lock,
    'Does composability with existing smart contracts create technical lock-in that exceeds economic exit costs, making the trap structural rather than merely suppressed?',
    'Analysis of migration costs for contracts: redeployment, liquidity fragmentation, governance approval requirements; measurement of users who would exit if technical barriers were zero',
    'If lock-in is primarily technical: trapped classification justified (mountain-like immobility at biographical horizon). If lock-in is primarily economic: trapped classification justified but with possible sunset (scaffold perspective if migration pathways emerge).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(smart_contract_composability_lock, empirical, 'Nature and severity of smart contract composability lock-in').

omega_variable(
    price_oracle_false_summit,
    'Is oracle-based price discovery a natural law (inherent to decentralized systems) or a contingent institutional choice that naturalizes extraction?',
    'Comparative study of alternative DeFi price discovery mechanisms; analysis of whether centralization is technological requirement or solution to incentive design; historical analysis of how oracle consolidation emerged',
    'If natural law: mountain classification justified, extraction is unavoidable. If contingent: false summit detected, classification should shift toward snare/tangled rope, indicating extractive mechanism disguised as necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_oracle_false_summit, conceptual, 'Whether oracle architecture is technical necessity or institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_oracle_price_discovery, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergo_oracle_tr_t0, ergo_oracle_price_discovery, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ergo_oracle_tr_t2, ergo_oracle_price_discovery, theater_ratio, 2, 0.48).
narrative_ontology:measurement(ergo_oracle_tr_t4, ergo_oracle_price_discovery, theater_ratio, 4, 0.58).
narrative_ontology:measurement(ergo_oracle_tr_t6, ergo_oracle_price_discovery, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(ergo_oracle_be_t0, ergo_oracle_price_discovery, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ergo_oracle_be_t2, ergo_oracle_price_discovery, base_extractiveness, 2, 0.39).
narrative_ontology:measurement(ergo_oracle_be_t4, ergo_oracle_price_discovery, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(ergo_oracle_be_t6, ergo_oracle_price_discovery, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_oracle_price_discovery, information_standard).
narrative_ontology:affects_constraint(ergo_oracle_price_discovery, defi_liquidation_cascades).
narrative_ontology:affects_constraint(ergo_oracle_price_discovery, mev_extraction_mechanisms).
narrative_ontology:affects_constraint(ergo_oracle_price_discovery, oracle_manipulation_attacks).

% DUAL FORMULATION NOTE:
% Oracle price discovery decomposes into at least two structurally distinct constraints: (1) price_aggregation_coordination (ε~0.15, Rope) — the technical problem of combining multiple price sources, and (2) operator_fee_extraction (ε~0.65, Snare) — the economic rent captured by oracle operators from locked-in users. This story captures the tangled hybrid. The upstream constraint is the information aggregation problem; the downstream constraint is MEV extraction enabled by oracle control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ergo_oracle_price_discovery, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
