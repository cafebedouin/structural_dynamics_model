% ============================================================================
% CONSTRAINT STORY: lindy_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lindy_effect, []).

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
 *   constraint_id: lindy_effect
 *   human_readable: The Lindy Effect
 *   domain: social/intellectual
 *
 * SUMMARY:
 *   The Lindy Effect posits that the future life expectancy of non-perishable
 *   entities, such as ideas and technologies, is proportional to their
 *   current age. This constraint story explores the Lindy Effect and
 *   categorizes it within the Deferential Realism framework. The constraint
 *   tends to favor established ideas and technologies over newer ones,
 *   assuming older concepts have proven their value over time.
 *
 * KEY AGENTS:
 *   - Short-lived Ideas: Unable to gain traction and are subsequently forgotten (powerless/trapped)
 *   - Institutional Knowledge Systems: Supports the preservation of long-lived ideas through documentation and analysis (institutional/analytical)
 *   - Analytical Observer: Examines the effect as a function of time and value. (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lindy_effect, 0.15).
domain_priors:suppression_score(lindy_effect, 0.02).
domain_priors:theater_ratio(lindy_effect, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lindy_effect, extractiveness, 0.15).
narrative_ontology:constraint_metric(lindy_effect, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(lindy_effect, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lindy_effect, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(lindy_effect, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lindy_effect, mountain).
narrative_ontology:human_readable(lindy_effect, "The Lindy Effect").
narrative_ontology:topic_domain(lindy_effect, "social/intellectual").

domain_priors:emerges_naturally(lindy_effect).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Ideas that fail early are permanently eliminated. This is unavoidable.
constraint_indexing:constraint_classification(lindy_effect, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% Academic institutions, libraries, and other knowledge systems are generally aligned with the effect, preserving long-lived ideas. They are analytical in that they have the capacity to determine which ideas are worth preserving.
constraint_indexing:constraint_classification(lindy_effect, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The analytical observer sees the Lindy Effect as a fundamental property of information and knowledge preservation.
constraint_indexing:constraint_classification(lindy_effect, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lindy_effect_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(lindy_effect, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lindy_effect, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lindy_effect, ExtMetricName, E),
    domain_priors:suppression_score(lindy_effect, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lindy_effect),
    narrative_ontology:constraint_metric(lindy_effect, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lindy_effect, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lindy_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Lindy Effect, in its purest form, is an emergent property where time acts as a filter. Ideas/technologies that have survived longer have inherently demonstrated some level of utility or resilience to environmental changes. Suppression is low because it doesn't actively prevent the development of new ideas but rather favors the persistence of older ones. This story classifies the Lindy Effect as a Mountain, as the effect is presented as an inherent property of information ecosystems. The effect emerges naturally and is not caused by external factors.
 *
 * PERSPECTIVAL GAP:
 *   There is limited perspectival gap as all perspectives observe the effect as a natural consequence of time and value. Short-lived ideas are inevitably forgotten, while institutional knowledge systems act as a passive filter, and the analytical observer simply recognizes the pattern.
 *
 * DIRECTIONALITY LOGIC:
 *   The Lindy Effect does not have strong beneficiaries or victims. Short-lived ideas could be considered 'victims,' but their loss is due to a lack of inherent qualities to persist rather than active suppression. This leads to the classification as a Mountain, as the dominant force is time itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lindy_effect, 0, 100).

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
