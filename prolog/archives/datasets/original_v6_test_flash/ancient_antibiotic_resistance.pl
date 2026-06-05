% ============================================================================
% CONSTRAINT STORY: ancient_antibiotic_resistance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ancient_antibiotic_resistance, []).

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
 *   constraint_id: ancient_antibiotic_resistance
 *   human_readable: The Inherent Evolutionary Potential for Antibiotic Resistance
 *   domain: biological/technological
 *
 * SUMMARY:
 *   The discovery of antibiotic resistance mechanisms in ancient bacteria,
 *   isolated from 5,000-year-old ice, reveals that resistance is not a modern
 *   phenomenon created by human antibiotic use. It demonstrates the inherent
 *   evolutionary potential for antibiotic resistance exists independent of
 *   human influence, classifying it as a natural law or constraint.
 *
 * KEY AGENTS:
 *   - The Bacteria: Inherent carrier of resistance genes (powerless/trapped)
 *   - The Scientific Community: Observer and analyzer of the phenomenon (institutional/analytical)
 *   - The Analytical Observer: Neutral observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ancient_antibiotic_resistance, 0.15).
domain_priors:suppression_score(ancient_antibiotic_resistance, 0.05).
domain_priors:theater_ratio(ancient_antibiotic_resistance, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, extractiveness, 0.15).
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(ancient_antibiotic_resistance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ancient_antibiotic_resistance, mountain).
narrative_ontology:human_readable(ancient_antibiotic_resistance, "The Inherent Evolutionary Potential for Antibiotic Resistance").
narrative_ontology:topic_domain(ancient_antibiotic_resistance, "biological/technological").

domain_priors:emerges_naturally(ancient_antibiotic_resistance).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The bacteria are inherently trapped by their own evolutionary constraints.
constraint_indexing:constraint_classification(ancient_antibiotic_resistance, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% The scientific community understands that antibiotic resistance is a natural phenomenon due to evolutionary pressures.
constraint_indexing:constraint_classification(ancient_antibiotic_resistance, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From an analytical perspective, antibiotic resistance is an inevitable outcome of evolutionary processes.
constraint_indexing:constraint_classification(ancient_antibiotic_resistance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ancient_antibiotic_resistance_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ancient_antibiotic_resistance, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ancient_antibiotic_resistance, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ancient_antibiotic_resistance, ExtMetricName, E),
    domain_priors:suppression_score(ancient_antibiotic_resistance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ancient_antibiotic_resistance),
    narrative_ontology:constraint_metric(ancient_antibiotic_resistance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ancient_antibiotic_resistance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ancient_antibiotic_resistance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because the constraint is a pre-existing condition, not imposed. Suppression is low as the constraint arises from natural processes. Theater ratio is low because it's a fundamental aspect of evolutionary biology.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify this as a Mountain, indicating a uniform view of antibiotic resistance as a natural, pre-existing phenomenon, regardless of agent position or scope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not strongly relevant as this is a naturally occurring constraint. The scientific community and analytical observer are observers, so they are not directly affected. The bacteria are simply acting according to evolutionary pressures.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved because the categorization as mountain reflects that this is a natural law independent of human action.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ancient_antibiotic_resistance, 0, 5000).

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
