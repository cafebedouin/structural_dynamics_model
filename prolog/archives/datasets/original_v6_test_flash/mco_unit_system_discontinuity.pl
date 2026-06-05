% ============================================================================
% CONSTRAINT STORY: mco_unit_system_discontinuity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mco_unit_system_discontinuity, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mco_unit_system_discontinuity
 *   human_readable: Persistence of Imperial Units in a Metric World
 *   domain: technological/institutional
 *
 * SUMMARY:
 *   The continued use of the Imperial unit system in a global scientific
 *   community that has standardized on Metric is a classic Piton. The initial
 *   transition costs were high and now its persistence creates a small but
 *   ongoing extraction and high theater.
 *
 * KEY AGENTS:
 *   - US Consumers: Powerless/Trapped
 *   - International Scientific Community: Institutional/Constrained
 *   - Analytical Observer: Analytical/Analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mco_unit_system_discontinuity, 0.15).
domain_priors:suppression_score(mco_unit_system_discontinuity, 0.4).
domain_priors:theater_ratio(mco_unit_system_discontinuity, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, extractiveness, 0.15).
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(mco_unit_system_discontinuity, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mco_unit_system_discontinuity, piton).
narrative_ontology:human_readable(mco_unit_system_discontinuity, "Persistence of Imperial Units in a Metric World").
narrative_ontology:topic_domain(mco_unit_system_discontinuity, "technological/institutional").

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% US consumers are largely trapped by the prevalence of Imperial units in daily life, despite the scientific advantages of metric. The theater is high due to performative labeling requirements that do little to change usage.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, piton,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The international scientific community largely adheres to metric, but is still constrained by the need to convert and understand Imperial units due to their continued use in some contexts, creating a low level of extraction. The persistence despite a better alternative makes this a piton.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical observer recognizes the inefficiency and potential for error introduced by maintaining two systems. The high theater and low functional value mark this as a piton.
constraint_indexing:constraint_classification(mco_unit_system_discontinuity, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mco_unit_system_discontinuity_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(mco_unit_system_discontinuity, TR),
    TR >= 0.70.

:- end_tests(mco_unit_system_discontinuity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low as it only impacts specific actors. Suppression is moderate, there's no active enforcement but it's difficult for US consumers to adopt metric only. Theater is high as the labeling requirements are performative, not functional.
 *
 * PERSPECTIVAL GAP:
 *   All actors classify it as a Piton.
 *
 * DIRECTIONALITY LOGIC:
 *   The derived d values reflect the relative position to the constraint. US consumers bear the cost, scientific community is constrained but doesn't benefit. The analytical observer sees the global perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not a coordination mechanism nor a case of pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mco_unit_system_discontinuity, 0, 50).

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
