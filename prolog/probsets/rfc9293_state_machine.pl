% ============================================================================
% CONSTRAINT STORY: rfc9293_state_machine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_state_machine, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rfc9293_state_machine
 *   human_readable: TCP State Machine Constraints
 *   domain: technological/institutional
 *
 * SUMMARY:
 *   The TCP state machine governs the lifecycle of a connection, from initial
 *   handshake (SYN) to termination (FIN/TIME-WAIT). It ensures reliable,
 *   ordered data transfer between applications. The constraints imposed by
 *   the state machine, as defined in RFC9293, are essential for maintaining
 *   network stability and interoperability.
 *
 * KEY AGENTS:
 *   - Internet Applications: Benefit from reliable data transfer (powerful/mobile).
 *   - Network Operators: Rely on consistent state management for network stability (institutional/analytical).
 *   - Theoretical Computer Scientists: See the state machine as an embodiment of fundamental network principles (analytical/analytical).
 *   - Unpatched Legacy Systems: Forced adherence, potential exploits (powerless/trapped).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_state_machine, 0.1).
domain_priors:suppression_score(rfc9293_state_machine, 0.05).
domain_priors:theater_ratio(rfc9293_state_machine, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_state_machine, extractiveness, 0.1).
narrative_ontology:constraint_metric(rfc9293_state_machine, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(rfc9293_state_machine, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_state_machine, rope).
narrative_ontology:human_readable(rfc9293_state_machine, "TCP State Machine Constraints").
narrative_ontology:topic_domain(rfc9293_state_machine, "technological/institutional").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_state_machine, internet_applications).
narrative_ontology:constraint_beneficiary(rfc9293_state_machine, network_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of a network architect who views the TCP state machine as a fundamental coordination mechanism ensuring reliable data transfer across the internet.
constraint_indexing:constraint_classification(rfc9293_state_machine, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% Perspective of an application developer who benefits from the reliable connection management provided by the TCP state machine, allowing them to focus on application logic.
constraint_indexing:constraint_classification(rfc9293_state_machine, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% Perspective of a theoretical computer scientist who views the TCP state machine as an embodiment of fundamental principles of reliable communication over unreliable channels, akin to a natural law.
constraint_indexing:constraint_classification(rfc9293_state_machine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Perspective of unpatched legacy systems that are forced to adhere to the TCP state machine, even with known vulnerabilities, due to lack of updates and support. They are trapped and bear the cost of potential exploits.
constraint_indexing:constraint_classification(rfc9293_state_machine, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_state_machine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rfc9293_state_machine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rfc9293_state_machine, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(rfc9293_state_machine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the state machine primarily functions as a coordination mechanism. Suppression is low because alternative protocols exist, although TCP remains the dominant choice for many applications. Theater ratio is low because the state machine's operations are largely functional and directly contribute to connection management.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives largely converge on the TCP state machine as a rope, with the theoretical computer scientist considering it a mountain. However, unpatched legacy systems experience it as a snare due to the forced adherence and potential vulnerabilities.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are internet applications and network operators, as they directly benefit from the state machine's reliable connection management. The d values are low for these agents. Unpatched legacy systems are victims, leading to a snare classification from their perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction because the state machine's primary function is to enable reliable communication, not to extract resources from any particular agent. The low extractiveness score reflects this, except for the specific case of unpatched systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_state_machine, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_state_machine, global_infrastructure).
narrative_ontology:affects_constraint(rfc9293_state_machine, osi_model).
narrative_ontology:affects_constraint(rfc9293_state_machine, internet_protocol).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
