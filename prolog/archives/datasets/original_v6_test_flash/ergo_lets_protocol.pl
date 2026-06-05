% ============================================================================
% CONSTRAINT STORY: ergo_lets_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ergo_lets_protocol, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ergo_lets_protocol
 *   human_readable: Ergo Local Exchange Trading System (LETS)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   LETS on the Ergo blockchain is a trustless mutual credit system where the
 *   sum of all participant balances is always zero. It allows participants to
 *   create their own money and trade locally, fostering community resilience.
 *   The system operates with minimal extraction or suppression due to the
 *   decentralized nature of the Ergo blockchain.
 *
 * KEY AGENTS:
 *   - lets_participants: Primary beneficiaries (moderate/mobile) - benefit from facilitated local trade.
 *   - analytical_observer: observes minimal extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ergo_lets_protocol, 0.35).
domain_priors:suppression_score(ergo_lets_protocol, 0.2).
domain_priors:theater_ratio(ergo_lets_protocol, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ergo_lets_protocol, extractiveness, 0.35).
narrative_ontology:constraint_metric(ergo_lets_protocol, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(ergo_lets_protocol, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ergo_lets_protocol, rope).
narrative_ontology:human_readable(ergo_lets_protocol, "Ergo Local Exchange Trading System (LETS)").
narrative_ontology:topic_domain(ergo_lets_protocol, "economic/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ergo_lets_protocol, lets_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% LETS participant views the system as a coordination mechanism, enabling local trade and exchange without reliance on centralized authorities. They can easily exit the system by ceasing to participate.
constraint_indexing:constraint_classification(ergo_lets_protocol, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% Analytical observer sees the LETS as a pure coordination mechanism facilitating trade within a local community.  The Ergo blockchain ensures trustless operation, minimizing potential extraction or suppression.
constraint_indexing:constraint_classification(ergo_lets_protocol, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ergo_lets_protocol_tests).
:- end_tests(ergo_lets_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.35) as the system aims to facilitate trade rather than extract value. Suppression is also low (0.2) as participants are free to join or leave the system. Theater ratio is low (0.1) as the system is primarily functional.
 *
 * PERSPECTIVAL GAP:
 *   Both the participant and the analytical observer see the system as a coordination mechanism. There is no significant perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Participants benefit from the system, therefore 'd' is closer to 0.  Analytical observer's directionality is derived from lack of extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ergo_lets_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ergo_lets_protocol, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
