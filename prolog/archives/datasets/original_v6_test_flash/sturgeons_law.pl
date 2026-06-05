% ============================================================================
% CONSTRAINT STORY: sturgeons_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sturgeons_law, []).

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
 *   constraint_id: sturgeons_law
 *   human_readable: Sturgeon's Law ('90% of everything is crap')
 *   domain: sociological/artistic
 *
 * SUMMARY:
 *   Sturgeon's Law posits that "ninety percent of everything is crud." It
 *   serves as an observation about the statistical distribution of quality in
 *   any creative or productive field. This constraint story examines
 *   Sturgeon's Law as a potentially immutable property in the distribution of
 *   quality, viewed from multiple perspectives.
 *
 * KEY AGENTS:
 *   - Powerless Consumer (powerless/analytical)
 *   - Archival Institution (institutional/analytical)
 *   - Analytical Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sturgeons_law, 0.15).
domain_priors:suppression_score(sturgeons_law, 0.01).
domain_priors:theater_ratio(sturgeons_law, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sturgeons_law, extractiveness, 0.15).
narrative_ontology:constraint_metric(sturgeons_law, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(sturgeons_law, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sturgeons_law, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sturgeons_law, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sturgeons_law, mountain).
narrative_ontology:human_readable(sturgeons_law, "Sturgeon's Law ('90% of everything is crap')").
narrative_ontology:topic_domain(sturgeons_law, "sociological/artistic").

domain_priors:emerges_naturally(sturgeons_law).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a consumer with limited time, Sturgeon's Law is an immutable truth, a statistical distribution they cannot change.
constraint_indexing:constraint_classification(sturgeons_law, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From the perspective of an archivist or historian, Sturgeon's Law reflects the reality of source material: most documents, artifacts, and artworks are of limited historical value, but the important ones are preserved.
constraint_indexing:constraint_classification(sturgeons_law, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% An analytical observer recognizes that Sturgeon's Law is a broad statistical observation about the distribution of quality, not a normative judgment.
constraint_indexing:constraint_classification(sturgeons_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sturgeons_law_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(sturgeons_law, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sturgeons_law, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sturgeons_law, ExtMetricName, E),
    domain_priors:suppression_score(sturgeons_law, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sturgeons_law),
    narrative_ontology:constraint_metric(sturgeons_law, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sturgeons_law, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sturgeons_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Sturgeon's Law is treated as a mountain, therefore extractiveness and suppression are low and accessibility_collapse and resistance reflect that.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on the mountain type because the law is treated as an immutable feature of the distribution of quality.
 *
 * DIRECTIONALITY LOGIC:
 *   No explicit beneficiaries or victims, directionality derived from power atom. The focus is on documenting the classification as a mountain and validating the natural law profile.
 *
 * MANDATROPHY ANALYSIS:
 *   As Sturgeon's Law has been modeled as a mountain, mandatrophy does not apply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sturgeons_law, 0, 100).

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
