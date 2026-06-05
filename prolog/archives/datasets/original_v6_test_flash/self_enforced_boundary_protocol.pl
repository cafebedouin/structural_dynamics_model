% ============================================================================
% CONSTRAINT STORY: self_enforced_boundary_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_self_enforced_boundary_protocol, []).

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
 *   constraint_id: self_enforced_boundary_protocol
 *   human_readable: The Self-Enforced Boundary Protocol
 *   domain: psychological/social
 *
 * SUMMARY:
 *   A boundary is defined not as a request for another's change, but as an
 *   internal action protocol: 'If X happens, I will do Y.' It requires the
 *   other person to do nothing, effectively moving the 'exit option' entirely
 *   into the Subject's hands.
 *
 * KEY AGENTS:
 *   - boundary_enforcer: The person setting the boundary (powerless/mobile)
 *   - boundary_tester: The person whose actions are 'testing' the boundary (moderate/mobile)
 *   - analytical_observer: An objective observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(self_enforced_boundary_protocol, 0.2).
domain_priors:suppression_score(self_enforced_boundary_protocol, 0.1).
domain_priors:theater_ratio(self_enforced_boundary_protocol, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(self_enforced_boundary_protocol, extractiveness, 0.2).
narrative_ontology:constraint_metric(self_enforced_boundary_protocol, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(self_enforced_boundary_protocol, theater_ratio, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(self_enforced_boundary_protocol, rope).
narrative_ontology:human_readable(self_enforced_boundary_protocol, "The Self-Enforced Boundary Protocol").
narrative_ontology:topic_domain(self_enforced_boundary_protocol, "psychological/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(self_enforced_boundary_protocol, boundary_enforcer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of someone attempting to establish a boundary. They are now empowered to act, given a clear protocol, and feel as though they have a way out of a bad situation.
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% From the perspective of the person who is being 'tested' by the boundary. They feel they are not being forced to do anything, because they are not. If they do 'X' the other person will do 'Y', but that's on them. Therefore, there is no extraction or suppression of their behavior.
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% From an analytical perspective, this protocol is a clear coordination mechanism. Person A does X, Person B does Y. Nothing is being extracted from Person A.
constraint_indexing:constraint_classification(self_enforced_boundary_protocol, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(self_enforced_boundary_protocol_tests).
:- end_tests(self_enforced_boundary_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.20) because the boundary tester isn't forced to do anything; all actions are internal to the boundary enforcer. The suppression is low (0.10) for the same reason. Theater ratio is very low (0.05) as this approach is quite direct and action oriented.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap; all agents experience this as a low extractiveness, low suppression system.
 *
 * DIRECTIONALITY LOGIC:
 *   The boundary_enforcer benefits from the increased sense of control and agency, making them a beneficiary. The boundary_tester does not experience any imposition, so they are not a victim. The analytical observer simply observes the protocol.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved because the protocol emphasizes internal action and empowerment rather than external coercion. It avoids mislabeling coordination as extraction by ensuring that no extraction occurs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(self_enforced_boundary_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(self_enforced_boundary_protocol, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
