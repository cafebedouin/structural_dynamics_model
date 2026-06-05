% ============================================================================
% CONSTRAINT STORY: ulysses_chp14
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp14, []).

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
 *   constraint_id: ulysses_chp14
 *   human_readable: The Gestation Mountain (Holles Street)
 *   domain: biological/linguistic/medical
 *
 * SUMMARY:
 *   Chapter 14 models the "proliferent continuance" of life at the National
 *   Maternity Hospital. Gestation is presented as a biological and social
 *   process. It exists in a domain bounded by medical practice and linguistic
 *   construction.
 *
 * KEY AGENTS:
 *   - The Expectant Mother: Primary target (powerless/trapped) — bears full costs/risks
 *   - The National Maternity Hospital: Primary beneficiary (institutional/constrained)
 *   - The Analytical Observer: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp14, 0.15).
domain_priors:suppression_score(ulysses_chp14, 0.02).
domain_priors:theater_ratio(ulysses_chp14, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp14, extractiveness, 0.15).
narrative_ontology:constraint_metric(ulysses_chp14, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(ulysses_chp14, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ulysses_chp14, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(ulysses_chp14, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp14, mountain).
narrative_ontology:human_readable(ulysses_chp14, "The Gestation Mountain (Holles Street)").
narrative_ontology:topic_domain(ulysses_chp14, "biological/linguistic/medical").

domain_priors:emerges_naturally(ulysses_chp14).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of the expectant mother, the biological process of gestation is a fixed constraint. While agency exists within its bounds, the underlying process is largely immutable.
constraint_indexing:constraint_classification(ulysses_chp14, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% From the hospital's perspective, the biological constraints are largely fixed, though medical interventions can alter outcomes.
constraint_indexing:constraint_classification(ulysses_chp14, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Viewed from a long timescale, the underlying biology of gestation is a mountain.
constraint_indexing:constraint_classification(ulysses_chp14, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp14_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ulysses_chp14, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp14, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ulysses_chp14, ExtMetricName, E),
    domain_priors:suppression_score(ulysses_chp14, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ulysses_chp14),
    narrative_ontology:constraint_metric(ulysses_chp14, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ulysses_chp14, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ulysses_chp14_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is a mountain as the gestational process is fundamental and largely unchangeable. Extractiveness and suppression are low, given the naturalness of the process. The NL profile aligns with the Mountain characteristics: high accessibility collapse and low resistance.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives align on the 'mountain' classification, though their interaction with it varies according to their position and power.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary target is the expectant mother who must bear the constraint's costs. The hospital benefits through fulfillment of its purpose. The analytical observer objectively assesses the situation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification avoids mislabeling because the process is fundamentally biological and unchangeable, fulfilling the Mountain's fixed status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp14, 0, 100).

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
