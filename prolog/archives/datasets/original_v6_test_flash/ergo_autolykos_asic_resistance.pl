% ============================================================================
% CONSTRAINT STORY: ergo_autolykos_asic_resistance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_autolykos_asic_resistance, []).

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
 *   constraint_id: ergo_autolykos_asic_resistance
 *   human_readable: Autolykos PoW Algorithm (ASIC Resistance)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Autolykos is Ergo's Proof-of-Work (PoW) algorithm, specifically designed
 *   to be memory-hard and ASIC-resistant. It aims to maintain fair
 *   accessibility for individual miners while ensuring a decentralized and
 *   secure network. The algorithm achieves this by requiring miners to
 *   utilize a large amount of memory, making the development of specialized
 *   ASIC hardware less economically attractive. This constraint attempts to
 *   balance the desires of various stakeholders: individual miners, network
 *   security, and potential ASIC manufacturers.
 *
 * KEY AGENTS:
 *   - individual_miners: Primary beneficiary (moderate/constrained) - Benefits from fair accessibility and potential profit.
 *   - ergo_network_security: Secondary beneficiary (institutional/arbitrage) - Benefits from decentralized mining.
 *   - asic_manufacturers: Primary target (powerless/trapped) - Hindered by the algorithm's design.
 *   - large_scale_mining_farms: Secondary target (powerful/constrained) - Face challenges in scaling operations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_autolykos_asic_resistance, 0.5).
domain_priors:suppression_score(ergo_autolykos_asic_resistance, 0.6).
domain_priors:theater_ratio(ergo_autolykos_asic_resistance, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_autolykos_asic_resistance, extractiveness, 0.5).
narrative_ontology:constraint_metric(ergo_autolykos_asic_resistance, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ergo_autolykos_asic_resistance, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_autolykos_asic_resistance, tangled_rope).
narrative_ontology:human_readable(ergo_autolykos_asic_resistance, "Autolykos PoW Algorithm (ASIC Resistance)").
narrative_ontology:topic_domain(ergo_autolykos_asic_resistance, "technological/economic").

domain_priors:requires_active_enforcement(ergo_autolykos_asic_resistance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_autolykos_asic_resistance, individual_miners).
narrative_ontology:constraint_beneficiary(ergo_autolykos_asic_resistance, ergo_network_security).
narrative_ontology:constraint_victim(ergo_autolykos_asic_resistance, asic_manufacturers).
narrative_ontology:constraint_victim(ergo_autolykos_asic_resistance, large_scale_mining_farms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: ASIC Manufacturers (Snare) - Autolykos actively prevents them from creating specialized hardware, hindering their business model. They are essentially trapped, unable to adapt their existing infrastructure to mine Ergo efficiently.
constraint_indexing:constraint_classification(ergo_autolykos_asic_resistance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Individual Miners (Tangled Rope) - Benefits from the algorithm maintaining fair accessibility, allowing them to mine using readily available hardware (GPUs). However, they are still somewhat constrained by hardware costs and electricity consumption.
constraint_indexing:constraint_classification(ergo_autolykos_asic_resistance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 3: Ergo Network Security (Rope) - Benefits from a more decentralized mining network, enhancing network resilience and security. This is a long-term advantage for the entire Ergo ecosystem. They can arbitrage different consensus mechanisms by hard forking if a better solution emerges.
constraint_indexing:constraint_classification(ergo_autolykos_asic_resistance, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Analytical Observer (Tangled Rope) - Sees the algorithm as a deliberate attempt to balance fairness (accessibility for individual miners) with network security (resistance against ASIC dominance). Recognizes the ongoing game between algorithm developers and hardware manufacturers.
constraint_indexing:constraint_classification(ergo_autolykos_asic_resistance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_autolykos_asic_resistance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ergo_autolykos_asic_resistance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ergo_autolykos_asic_resistance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ergo_autolykos_asic_resistance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ergo_autolykos_asic_resistance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50): Moderate. The algorithm extracts resources from miners in the form of electricity consumption and hardware costs but also provides the potential for profit. ASIC manufacturers are entirely suppressed. Suppression (0.60): Moderate-high. The algorithm makes it difficult for ASIC manufacturers to develop profitable hardware, but it does not completely eliminate the possibility. The design actively suppresses specialized hardware. Theater ratio (0.30): Low. The algorithm is primarily functional, with relatively little 'theatrical' overhead. It directly serves the purpose of securing the network and distributing rewards.
 *
 * PERSPECTIVAL GAP:
 *   ASIC manufacturers see Autolykos as a snare, preventing them from entering the Ergo mining ecosystem. Individual miners experience it as a tangled rope: it allows them to participate but still requires investment and effort. Ergo network security views it as a rope, enhancing network resilience through decentralization. The analytical observer sees the constant hardware/software battle.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the algorithm's effects on different agents. Individual miners benefit from accessibility, while ASIC manufacturers are suppressed. Ergo network security benefits from decentralization. The directionality values reflect these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not simply a case of extraction. It is a design choice to favor individual miners and decentralized network security over the potential efficiency gains of ASICs. The algorithm actively enforces this preference. The perspective taken greatly affects the type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hardware_arms_race,
    'How long can Autolykos maintain ASIC resistance before specialized hardware becomes economically viable despite design constraints?',
    'Monitoring hardware development, analyzing mining profitability, observing hashrate distribution.',
    'If ASIC resistance fails: Centralization of mining power, reduced network security. If ASIC resistance succeeds: Decentralized mining, enhanced network security.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hardware_arms_race, empirical, 'Duration of ASIC resistance before diminishing returns.').

omega_variable(
    algorithm_evolution,
    'Will future algorithmic adjustments be necessary to counteract potential ASIC development, and how will these adjustments affect existing mining hardware?',
    'Algorithm development, simulations of hardware performance, community feedback.',
    'Frequent adjustments: Instability, reduced miner participation. Rare adjustments: Increased vulnerability to ASIC dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_evolution, conceptual, 'Frequency and impact of future algorithm modifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_autolykos_asic_resistance, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ergo_tr_t0, ergo_autolykos_asic_resistance, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ergo_tr_t1, ergo_autolykos_asic_resistance, theater_ratio, 1, 0.25).
narrative_ontology:measurement(ergo_tr_t2, ergo_autolykos_asic_resistance, theater_ratio, 2, 0.3).

% Extraction over time
narrative_ontology:measurement(ergo_be_t0, ergo_autolykos_asic_resistance, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(ergo_be_t1, ergo_autolykos_asic_resistance, base_extractiveness, 1, 0.45).
narrative_ontology:measurement(ergo_be_t2, ergo_autolykos_asic_resistance, base_extractiveness, 2, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_autolykos_asic_resistance, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
