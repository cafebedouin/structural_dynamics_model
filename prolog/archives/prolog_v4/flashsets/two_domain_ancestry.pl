% ============================================================================
% CONSTRAINT STORY: two_domain_ancestry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_two_domain_ancestry, []).

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
 *   constraint_id: two_domain_ancestry
 *   human_readable: The Two-Domain Tree of Life
 *   domain: scientific/genomics
 *
 * SUMMARY:
 *   Based on comprehensive genomic analysis of Asgard archaea, this
 *   constraint represents the discovery that eukaryotes (all complex life)
 *   did not emerge as a third, distinct domain of life, but rather evolved
 *   from within the archaeal domain. This finding reshapes our understanding
 *   of the tree of life and the evolutionary relationships between organisms.
 *
 * KEY AGENTS:
 *   - Genomics Research Community: Analytical Observer (institutional/analytical)
 *   - The Tree of Life: Subject to constraint (universal/universal)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(two_domain_ancestry, 0.05).
domain_priors:suppression_score(two_domain_ancestry, 0.02).
domain_priors:theater_ratio(two_domain_ancestry, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(two_domain_ancestry, extractiveness, 0.05).
narrative_ontology:constraint_metric(two_domain_ancestry, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(two_domain_ancestry, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(two_domain_ancestry, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(two_domain_ancestry, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(two_domain_ancestry, mountain).
narrative_ontology:human_readable(two_domain_ancestry, "The Two-Domain Tree of Life").
narrative_ontology:topic_domain(two_domain_ancestry, "scientific/genomics").

domain_priors:emerges_naturally(two_domain_ancestry).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From a civilizational perspective, the two-domain ancestry of eukaryotes represents a fundamental constraint on the possible evolutionary pathways of life. It is a mountain because it reflects an inherent limitation on biological systems.
constraint_indexing:constraint_classification(two_domain_ancestry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The genomics research community views this as a fundamental finding, reflecting a natural constraint on the evolution of life. While new data may refine the understanding of the exact mechanisms, the core concept of eukaryote ancestry within archaea is unlikely to change.
constraint_indexing:constraint_classification(two_domain_ancestry, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(two_domain_ancestry_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(two_domain_ancestry, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(two_domain_ancestry, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(two_domain_ancestry, ExtMetricName, E),
    domain_priors:suppression_score(two_domain_ancestry, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(two_domain_ancestry),
    narrative_ontology:constraint_metric(two_domain_ancestry, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(two_domain_ancestry, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(two_domain_ancestry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Very Low. This constraint represents a fundamental discovery about the nature of life, not an extractive process.  Suppression (0.02): Very Low. While alternative hypotheses about eukaryote origins may exist, they are not actively suppressed by the scientific community; rather, they have been disproven by genomic evidence. Theater ratio (0.10): Low. This constraint is based on objective scientific evidence, not on performative actions or theatrical compliance.
 *
 * PERSPECTIVAL GAP:
 *   Both perspectives classify this as a mountain, reflecting the robust nature of the genomic evidence and the fundamental constraint it represents on the possible evolutionary pathways of life. There is no significant perspectival gap because all relevant observers agree on the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Because this is a mountain constraint representing a fundamental scientific discovery, there are no beneficiaries or victims in the traditional sense. The directionality is neutral, as the constraint reflects the underlying structure of reality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as a mountain, which means there's little possibility for misclassification as a pure extraction. It represents a basic understanding of biology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(two_domain_ancestry, 0, 100).

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
