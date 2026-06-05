% ============================================================================
% CONSTRAINT STORY: orbital_data_center_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orbital_data_center_2026, []).

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
 *   constraint_id: orbital_data_center_2026
 *   human_readable: SpaceX Million-Satellite Orbital Compute Network
 *   domain: technological/geopolitical
 *
 * SUMMARY:
 *   SpaceX's proposed network of one million satellites functioning as a
 *   single orbital data center presents a complex constraint with both
 *   coordination and extraction aspects. The network promises to provide
 *   global internet access, low-latency computing, and secure communications,
 *   but also raises concerns about market dominance, regulatory capture, and
 *   potential security vulnerabilities. Different actors experience the
 *   network as a rope, snare, or tangled rope depending on their structural
 *   position.
 *
 * KEY AGENTS:
 *   - SpaceX: Primary beneficiary (institutional/arbitrage) - Benefits through revenue generation and expanded market share.
 *   - DoD: Secondary beneficiary (powerful/mobile) - Benefits from secure, low-latency communications and data processing.
 *   - Terrestrial Internet Providers: Primary victim (powerless/trapped) - Suffers from decreased market share and potential obsolescence.
 *   - Competitor Satellite Constellations: Secondary victim (moderate/constrained) - Faces increased competition and pressure to innovate.
 *   - High Frequency Trading Firms: beneficiary (powerful/mobile) - Benefits from low latency and high speed computations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orbital_data_center_2026, 0.55).
domain_priors:suppression_score(orbital_data_center_2026, 0.45).
domain_priors:theater_ratio(orbital_data_center_2026, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orbital_data_center_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(orbital_data_center_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(orbital_data_center_2026, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orbital_data_center_2026, tangled_rope).
narrative_ontology:human_readable(orbital_data_center_2026, "SpaceX Million-Satellite Orbital Compute Network").
narrative_ontology:topic_domain(orbital_data_center_2026, "technological/geopolitical").

domain_priors:requires_active_enforcement(orbital_data_center_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orbital_data_center_2026, spacex).
narrative_ontology:constraint_beneficiary(orbital_data_center_2026, dod).
narrative_ontology:constraint_beneficiary(orbital_data_center_2026, high_frequency_trading_firms).
narrative_ontology:constraint_victim(orbital_data_center_2026, terrestrial_internet_providers).
narrative_ontology:constraint_victim(orbital_data_center_2026, competitor_satellite_constellations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% SpaceX views the network as a coordination mechanism for global internet access and high-bandwidth computing. They benefit directly through revenue generation and expanded market share, and they have arbitrage options due to their technological lead.
constraint_indexing:constraint_classification(orbital_data_center_2026, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% Terrestrial internet providers are trapped and see the network as a pure extraction mechanism that suppresses their market share. They cannot effectively compete with the network's global coverage and high bandwidth, leading to decreased revenue and potential obsolescence.
constraint_indexing:constraint_classification(orbital_data_center_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% The DoD benefits greatly from the secure, low-latency communications and data processing capabilities provided by the orbital network. However, they are also somewhat constrained by their reliance on a commercial entity and must balance the benefits with potential vulnerabilities and dependencies.
constraint_indexing:constraint_classification(orbital_data_center_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% An analytical observer sees a tangled rope: a mixed coordination and extraction mechanism. The network offers genuine benefits in terms of global connectivity and computing power, but also creates dependencies and potentially suppresses competition, leading to an asymmetric distribution of value.
constraint_indexing:constraint_classification(orbital_data_center_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Competitor satellite constellations are constrained by SpaceX's first-mover advantage and scale. They are both victims (losing market share) and beneficiaries (forced to innovate and compete on price/services). Thus, they experience a Tangled Rope scenario.
constraint_indexing:constraint_classification(orbital_data_center_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orbital_data_center_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(orbital_data_center_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(orbital_data_center_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(orbital_data_center_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orbital_data_center_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The network extracts value from terrestrial internet providers and competitor satellite constellations by suppressing their market share. Suppression (0.45): Moderate. There are significant barriers to entry for competitors due to the high cost of satellite development and launch. Theater ratio (0.20): Low. The network is primarily functional, with minimal performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   SpaceX views the network as a coordination mechanism (rope) for global internet access, while terrestrial internet providers see it as a pure extraction mechanism (snare) that suppresses their market share. The DoD and competitor satellite constellations experience a tangled rope, balancing the benefits of the network with potential risks and dependencies. An analytical observer sees the network as a tangled rope, recognizing both the coordination and extraction aspects.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are determined by the structural position of each agent. SpaceX, as the primary beneficiary, experiences low extraction. Terrestrial internet providers, as the primary victims, experience high extraction. The DoD and competitor satellite constellations experience moderate extraction due to their mixed relationship with the network.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the different types are legitimate perspectival readings of the same structural data. The mandatrophy is not 'which type is correct?' but 'which perspective are you measuring from?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_security_vulnerabilities,
    'What are the potential security vulnerabilities of a large-scale orbital compute network, and how can they be mitigated?',
    'Security audits, penetration testing, and ongoing monitoring of network infrastructure.',
    'If vulnerabilities are significant, the network could be exploited for malicious purposes, leading to widespread disruption and data breaches. If effectively mitigated, the network can provide a secure and reliable platform for global communications and computing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_security_vulnerabilities, empirical, 'Potential security vulnerabilities of the orbital network').

omega_variable(
    regulatory_capture,
    'Will SpaceX be able to exert undue influence over regulatory bodies, potentially hindering competition and innovation?',
    'Monitoring of lobbying activities, regulatory decisions, and enforcement actions.',
    'If SpaceX exerts undue influence, the regulatory landscape could become skewed in their favor, stifling competition and innovation. If regulations are fair and transparent, the network can contribute to a vibrant and competitive space economy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture, preference, 'Risk of regulatory capture by SpaceX').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orbital_data_center_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orbi_tr_t0, orbital_data_center_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(orbi_tr_t5, orbital_data_center_2026, theater_ratio, 5, 0.15).
narrative_ontology:measurement(orbi_tr_t10, orbital_data_center_2026, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(orbi_be_t0, orbital_data_center_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(orbi_be_t5, orbital_data_center_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(orbi_be_t10, orbital_data_center_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orbital_data_center_2026, global_infrastructure).
narrative_ontology:affects_constraint(orbital_data_center_2026, global_internet_access).
narrative_ontology:affects_constraint(orbital_data_center_2026, space_debris_proliferation).
narrative_ontology:affects_constraint(orbital_data_center_2026, spectrum_allocation_regimes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
