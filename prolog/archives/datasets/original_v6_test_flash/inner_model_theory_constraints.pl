% ============================================================================
% CONSTRAINT STORY: inner_model_theory_constraints
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_inner_model_theory_constraints, []).

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
 *   constraint_id: inner_model_theory_constraints
 *   human_readable: The Axiom of Constructibility (V=L)
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   Inner Model Theory studies sub-universes of the set-theoretic universe
 *   (V) that satisfy the axioms of ZFC. The Axiom of Constructibility (V=L)
 *   posits that every set is constructible, meaning it can be built up from
 *   the empty set using transfinite recursion and certain definable
 *   operations. This axiom, if true, significantly restricts the possible
 *   models of set theory and has profound consequences for the independence
 *   results in set theory.
 *
 * KEY AGENTS:
 *   - Set Theorists: Analytical observers exploring the consequences of V=L (analytical/analytical)
 *   - Mathematical Community: Institutional body influenced by the consistency and implications of V=L (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(inner_model_theory_constraints, 0.1).
domain_priors:suppression_score(inner_model_theory_constraints, 0.05).
domain_priors:theater_ratio(inner_model_theory_constraints, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(inner_model_theory_constraints, extractiveness, 0.1).
narrative_ontology:constraint_metric(inner_model_theory_constraints, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(inner_model_theory_constraints, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(inner_model_theory_constraints, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(inner_model_theory_constraints, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(inner_model_theory_constraints, mountain).
narrative_ontology:human_readable(inner_model_theory_constraints, "The Axiom of Constructibility (V=L)").
narrative_ontology:topic_domain(inner_model_theory_constraints, "mathematical/logical").

domain_priors:emerges_naturally(inner_model_theory_constraints).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The axiom V=L, if true, fundamentally constrains the possible models of set theory, simplifying the landscape of inner models. From this perspective it is a mountain, albeit a potentially false one.
constraint_indexing:constraint_classification(inner_model_theory_constraints, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The adoption of V=L, or the exploration of its consequences, by the broader mathematical community is constrained by its consistency with ZFC. To the extent this is established, acceptance follows, even if not universal, forming a 'social natural law' similar to other widely accepted axioms or conjectures.
constraint_indexing:constraint_classification(inner_model_theory_constraints, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(inner_model_theory_constraints_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(inner_model_theory_constraints, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(inner_model_theory_constraints, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(inner_model_theory_constraints, ExtMetricName, E),
    domain_priors:suppression_score(inner_model_theory_constraints, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(inner_model_theory_constraints),
    narrative_ontology:constraint_metric(inner_model_theory_constraints, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(inner_model_theory_constraints, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(inner_model_theory_constraints_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because V=L is a foundational axiom, not directly extracting resources or opportunities. Suppression is low as alternative axioms are actively explored. Theater ratio is low because acceptance depends on consistency and logical implications, not performance.
 *
 * PERSPECTIVAL GAP:
 *   Both perspectives classify as mountain because the constraint arises from fundamental logical properties of set theory. The set theorist views it from the standpoint of logical necessity; the mathematical community from the viewpoint of a consistent axiom with significant mathematical consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the logical relationship between axioms and their consequences. V=L constrains the possible models of set theory, but this constraint is seen as a fundamental property, not an act of extraction. The low extractiveness score reflects the natural law characterization.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction because V=L is primarily a statement about the structure of the set-theoretic universe, not an attempt to extract resources or suppress alternatives in the way that snares or tangled ropes do.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(inner_model_theory_constraints, 0, 100).

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
