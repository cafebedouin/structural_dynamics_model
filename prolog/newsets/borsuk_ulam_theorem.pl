% ============================================================================
% CONSTRAINT STORY: borsuk_ulam_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_borsuk_ulam_theorem, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: borsuk_ulam_theorem
 *   human_readable: Borsuk-Ulam Theorem
 *   domain: mathematics/topology
 *
 * SUMMARY:
 *   The Borsuk-Ulam theorem is a fundamental result in algebraic topology. It
 *   states that for any continuous function f from an n-sphere into
 *   n-dimensional Euclidean space (R^n), there exists a pair of antipodal
 *   points (x and -x) on the sphere such that f(x) = f(-x). Informally, it's
 *   impossible to flatten a sphere into a plane of the same dimension without
 *   making at least one pair of opposite points land on top of each other. As
 *   a proven mathematical theorem, it represents a pure logical constraint,
 *   an unchangeable feature of the mathematical universe.
 *
 * KEY AGENTS:
 *   - Research Topologist (analytical/analytical): Views the theorem as a foundational piece of knowledge and a tool for further discovery.
 *   - Applied Mathematics Community (organized/constrained): Uses the theorem as a constraint that must be respected in models and applications, like the Ham Sandwich Theorem.
 *   - Topology Student (powerless/trapped): Encounters the theorem as an immutable fact to be learned and applied, with no room for negotiation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(borsuk_ulam_theorem, 0.01).
domain_priors:suppression_score(borsuk_ulam_theorem, 0.01).
domain_priors:theater_ratio(borsuk_ulam_theorem, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(borsuk_ulam_theorem, extractiveness, 0.01).
narrative_ontology:constraint_metric(borsuk_ulam_theorem, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(borsuk_ulam_theorem, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(borsuk_ulam_theorem, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(borsuk_ulam_theorem, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(borsuk_ulam_theorem, mountain).
narrative_ontology:human_readable(borsuk_ulam_theorem, "Borsuk-Ulam Theorem").
narrative_ontology:topic_domain(borsuk_ulam_theorem, "mathematics/topology").

domain_priors:emerges_naturally(borsuk_ulam_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a professional mathematician, the theorem is a fundamental, unchangeable feature of the logical landscape. It is a tool and a boundary condition for further research. There is no exit from its logical consequences.
constraint_indexing:constraint_classification(borsuk_ulam_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% For fields that use topology (e.g., economics, physics, computer science), the theorem is a given constraint. Systems cannot be designed that violate it. The community is constrained to respect its implications, such as in fair division problems.
constraint_indexing:constraint_classification(borsuk_ulam_theorem, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% A student learning the theorem experiences it as an absolute, objective fact. During an exam or proof, they are trapped by its logic; there is no alternative answer or way to circumvent its truth. The only path is comprehension.
constraint_indexing:constraint_classification(borsuk_ulam_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(borsuk_ulam_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(borsuk_ulam_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(borsuk_ulam_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(borsuk_ulam_theorem, ExtMetricName, E),
    domain_priors:suppression_score(borsuk_ulam_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(borsuk_ulam_theorem),
    narrative_ontology:constraint_metric(borsuk_ulam_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(borsuk_ulam_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(borsuk_ulam_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is a canonical example of a Mountain. Extractiveness (ε=0.01) and Suppression (0.01) are minimal, as a mathematical theorem does not extract value or coerce behavior in a social sense; it simply describes a logical necessity. The NL Profile metrics confirm this: it `emerges_naturally` from axioms, has extremely high `accessibility_collapse` (0.98) as its logic is inescapable once understood, and extremely low `resistance` (0.02) as it cannot be defied. The `theater_ratio` is zero, as a proof is pure function with no performative aspect.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. The defining characteristic of a Mountain constraint, particularly a mathematical one, is its invariance across all observer positions. Whether viewed by a student, a researcher, or an applied scientist, the theorem's classification remains 'mountain'. Its truth is not contingent on power, time horizon, or exit options. This uniformity serves as a baseline against which the perspectival gaps of social and political constraints can be measured.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain constraint, there are no beneficiaries or victims. The theorem is a universal fact of a logical system. The directionality `d` is therefore not derived from beneficiary/victim status. The engine will use canonical fallbacks for each power atom, but with ε being near zero, the effective extraction χ will remain near zero for all perspectives, reinforcing the Mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The Borsuk-Ulam theorem provides a grounding example that prevents mandatrophy. It establishes a clear case of a non-extractive, non-coercive, universal constraint. Any attempt to frame it as a 'snare' of academic elitism or a 'rope' for coordinating research would be a category error, confusing the map (the theorem) with the territory (the social structures of academia). This constraint's pristine Mountain classification helps calibrate the system to correctly identify true natural law constraints versus socially constructed ones that merely claim to be.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(borsuk_ulam_theorem, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(borsuk_ulam_theorem, information_standard).
narrative_ontology:affects_constraint(borsuk_ulam_theorem, brouwer_fixed_point_theorem).
narrative_ontology:affects_constraint(borsuk_ulam_theorem, ham_sandwich_theorem).

% DUAL FORMULATION NOTE:
% The Borsuk-Ulam theorem is considered a fundamental result from which other significant theorems, like the Brouwer fixed-point theorem and the Ham Sandwich theorem, can be derived. It acts as an upstream logical constraint on these downstream results.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
