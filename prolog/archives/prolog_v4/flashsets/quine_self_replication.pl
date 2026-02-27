% ============================================================================
% CONSTRAINT STORY: quine_self_replication
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quine_self_replication, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quine_self_replication
 *   human_readable: Quines (Computational Self-Replication)
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   A Quine is a non-empty computer program which takes no input and produces
 *   a copy of its own source code as its only output. This is a fundamental
 *   concept in computer science illustrating self-replication.
 *
 * KEY AGENTS:
 *   - Analytical Observer (analytical/analytical)
 *   - Formal System (powerless/analytical)
 *   - Computer Science Community (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quine_self_replication, 0.01).
domain_priors:suppression_score(quine_self_replication, 0.01).
domain_priors:theater_ratio(quine_self_replication, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quine_self_replication, extractiveness, 0.01).
narrative_ontology:constraint_metric(quine_self_replication, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(quine_self_replication, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quine_self_replication, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(quine_self_replication, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quine_self_replication, mountain).
narrative_ontology:human_readable(quine_self_replication, "Quines (Computational Self-Replication)").
narrative_ontology:topic_domain(quine_self_replication, "technological/mathematical").

domain_priors:emerges_naturally(quine_self_replication).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From an analytical perspective, the existence of quines is a fundamental property of computation, demonstrating the possibility of self-reference and self-description within formal systems. It is a logical necessity given the power of Turing-complete systems.
constraint_indexing:constraint_classification(quine_self_replication, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of the formal system itself, the existence of a quine represents an inherent property of its expressiveness. The ability to self-replicate is not something imposed from the outside, but rather a capability that arises naturally from its structure.
constraint_indexing:constraint_classification(quine_self_replication, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of the computer science community, the existence of quines represents a fundamental concept in the theory of computation, demonstrating the power and limitations of formal systems. It's a basic building block.
constraint_indexing:constraint_classification(quine_self_replication, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quine_self_replication_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(quine_self_replication, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quine_self_replication, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quine_self_replication, ExtMetricName, E),
    domain_priors:suppression_score(quine_self_replication, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quine_self_replication),
    narrative_ontology:constraint_metric(quine_self_replication, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quine_self_replication, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quine_self_replication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression are near zero because the concept is a fundamental property of computation, not a constraint that actively extracts or suppresses. The theater ratio is low, as there is little performative aspect to the core concept.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap, as all perspectives converge on the understanding of quines as a fundamental property of computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not relevant as this constraint is classified as a mountain, representing a fundamental property.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a mountain constraint, not a social structure. Mandatrophy is not applicable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quine_self_replication, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
