% ============================================================================
% CONSTRAINT STORY: sts86_ascent_checklist
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sts86_ascent_checklist, []).

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
 *   constraint_id: sts86_ascent_checklist
 *   human_readable: Space Shuttle Ascent/Abort Procedural Matrix
 *   domain: technological/institutional
 *
 * SUMMARY:
 *   The Space Shuttle Ascent/Abort Procedural Matrix (STS-86) is a critical
 *   checklist (JSC-48005) during ascent, representing a procedural
 *   constraint. It outlines steps and contingency actions to be taken by the
 *   crew and mission control to ensure a safe ascent to orbit, or a
 *   controlled abort if necessary. The matrix minimizes the risk of human
 *   error during a highly dynamic and unforgiving phase of flight. The
 *   checklist serves as a coordination mechanism.
 *
 * KEY AGENTS:
 *   - Astronaut: bears extraction (trapped/powerless)
 *   - Mission Control: benefits from checklist (analytical/institutional)
 *   - Analytical Observer: understands the need (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sts86_ascent_checklist, 0.01).
domain_priors:suppression_score(sts86_ascent_checklist, 0.01).
domain_priors:theater_ratio(sts86_ascent_checklist, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sts86_ascent_checklist, extractiveness, 0.01).
narrative_ontology:constraint_metric(sts86_ascent_checklist, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(sts86_ascent_checklist, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sts86_ascent_checklist, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sts86_ascent_checklist, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sts86_ascent_checklist, mountain).
narrative_ontology:human_readable(sts86_ascent_checklist, "Space Shuttle Ascent/Abort Procedural Matrix").
narrative_ontology:topic_domain(sts86_ascent_checklist, "technological/institutional").

domain_priors:emerges_naturally(sts86_ascent_checklist).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Astronaut must follow the checklist or face catastrophic failure.
constraint_indexing:constraint_classification(sts86_ascent_checklist, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Mission Control must follow the checklist or face catastrophic failure.
constraint_indexing:constraint_classification(sts86_ascent_checklist, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% Analytical Observer acknowledges the fundamental necessity of a structured checklist for complex, life-or-death procedures.
constraint_indexing:constraint_classification(sts86_ascent_checklist, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sts86_ascent_checklist_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(sts86_ascent_checklist, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sts86_ascent_checklist, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sts86_ascent_checklist, ExtMetricName, E),
    domain_priors:suppression_score(sts86_ascent_checklist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sts86_ascent_checklist),
    narrative_ontology:constraint_metric(sts86_ascent_checklist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sts86_ascent_checklist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sts86_ascent_checklist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The checklist is effectively a mountain constraint because the risk of not using it is catastrophic, leaving nearly zero room for resistance. Suppression is correspondingly high, while extractiveness is low because, if effective, it extracts nothing.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap here; all actors understand the high-stakes nature of spaceflight, therefore are in agreement that proper procedures and a checklist are essential.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sts86_ascent_checklist, 0, 10).

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
