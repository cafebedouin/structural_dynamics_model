% ============================================================================
% CONSTRAINT STORY: biological_specification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biological_specification, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: biological_specification
 *   human_readable: Biological Specification as a Natural Limit
 *   domain: biology/epistemology
 *
 * SUMMARY:
 *   This constraint story models the fundamental limit on our ability to
 *   fully specify a biological system. The core tension is between the
 *   'Ideal' specification (a complete, static, perfect description) and the
 *   'Real' (an incomplete, dynamic, and stochastic reality). This gap is not
 *   due to a human-made rule but is an inherent feature of the universe,
 *   arising from the complexity, evolutionary history, and physical
 *   constraints of living systems. It is a natural law.
 *
 * KEY AGENTS:
 *   - Analytical Observer: Sees the irreducible complexity as a fundamental limit (Mountain).
 *   - Synthetic Biologist: Uses known biological rules as a design standard (experiences as Rope).
 *   - Regulatory Agency: Builds simplified, temporary standards to manage the complexity (induces Scaffolds).
 *   - Historian of Science: Studies how past, failed models persist as inertial ideas (observes Piton generation).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biological_specification, 0.05).
domain_priors:suppression_score(biological_specification, 0.02).
domain_priors:theater_ratio(biological_specification, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biological_specification, extractiveness, 0.05).
narrative_ontology:constraint_metric(biological_specification, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(biological_specification, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biological_specification, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(biological_specification, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biological_specification, mountain).
narrative_ontology:human_readable(biological_specification, "Biological Specification as a Natural Limit").
narrative_ontology:topic_domain(biological_specification, "biology/epistemology").

domain_priors:emerges_naturally(biological_specification).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL (MOUNTAIN) — The irreducible complexity, dynamism, and stochasticity of biological systems represent a fundamental limit on our ability to create a complete specification. This is a natural law, an unchangeable feature of the universe.
constraint_indexing:constraint_classification(biological_specification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: SYNTHETIC BIOLOGIST (ROPE-LIKE FUNCTION) — Experiences the *mapped portions* of the natural limit (e.g., codon tables, promoter logic) as a reliable coordination standard (a Rope) for engineering new systems. The formal classification remains Mountain due to near-zero base extraction (ε), but the functional experience is that of a Rope.
constraint_indexing:constraint_classification(biological_specification, mountain,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AGENCY (SCAFFOLD-INDUCING) — Confronts the Mountain of complexity and is forced to construct a simplified, temporary model (a Scaffold) to ensure public safety (e.g., drug specifications). The constraint they face is the Mountain; the Scaffold is their output, a separate, downstream constraint.
constraint_indexing:constraint_classification(biological_specification, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HISTORIAN OF SCIENCE (PITON-GENERATING) — Observes how past, oversimplified attempts to specify biology (e.g., strict genetic determinism) become Pitons—inertial concepts that lose their scientific function but persist theatrically. The Mountain is the underlying reality that inevitably degrades these simplistic models into Pitons.
constraint_indexing:constraint_classification(biological_specification, mountain,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biological_specification_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(biological_specification, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(biological_specification, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(biological_specification, ExtMetricName, E),
    domain_priors:suppression_score(biological_specification, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(biological_specification),
    narrative_ontology:constraint_metric(biological_specification, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(biological_specification, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(biological_specification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is classified as a Mountain because it is an emergent, unchangeable feature of the natural world. Extractiveness (ε=0.05) is near zero; the universe does not 'extract' value, though the effort required for discovery is a cost to humans. Suppression (0.02) is also near zero, as we are free to attempt any method of specification. The key Mountain metrics are met: `emerges_naturally` is true, `accessibility_collapse` (0.95) is high because the closer one looks, the more complexity is revealed, and `resistance` (0.05) is low as one cannot 'push back' against this limit.
 *
 * PERSPECTIVAL GAP:
 *   There is no formal perspectival gap in classification; due to the extremely low base extractiveness (ε), all perspectives resolve to Mountain. However, there is a significant gap in functional interpretation. Different agents interact with the Mountain in ways that resemble other constraint types. The synthetic biologist uses it as a Rope (coordination), the regulator builds a Scaffold on it (temporary support), and the historian sees it as a force that degrades old theories into Pitons (inertial objects). This demonstrates how a single Mountain can induce the creation of a diverse ecosystem of human-made constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain constraint representing a natural law, there are no structural beneficiaries or victims. The constraint is symmetric and applies universally. Directionality (d) is therefore uniform and close to 0.5 for all actors, but since ε is negligible, the effective extraction (χ) is always near zero, leading to a consistent Mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This case clarifies the relationship between natural law (Mountain) and the social systems built to manage it. A common error is to misclassify a human-made regulatory system (e.g., a Tangled Rope) as an inevitable consequence of nature (a Mountain). This analysis avoids that by applying the ε-invariance principle: the natural limit is a Mountain (ε≈0), while the human regulatory system built upon it is a separate constraint with its own, higher ε. Recognizing the foundational constraint as a Mountain prevents the 'false summit' error of naturalizing contingent, human-made rules.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biological_specification, 1859, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(biological_specification, drug_approval_specification_process).

% DUAL FORMULATION NOTE:
% This constraint, the natural limit of biological specification (Mountain), is the upstream cause for many human-made constraints. For example, the 'drug_approval_specification_process' is a downstream Tangled Rope or Scaffold built by regulators to create a workable, simplified model of this underlying complexity. The two are distinct constraints with different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
