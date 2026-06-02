% ============================================================================
% CONSTRAINT STORY: ramsey_theorem_foundation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ramsey_theorem_foundation, []).

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
 *   constraint_id: ramsey_theorem_foundation
 *   human_readable: Ramsey Theorem Foundation
 *   domain: combinatorics/logic/mathematics
 *
 * SUMMARY:
 *   Ramsey's theorem is a foundational result in combinatorics stating that
 *   for any finite coloring of the integers, there must exist an infinite
 *   monochromatic subset. This is a pure mathematical constraint — a logical
 *   invariant that admits no exceptions, alternatives, or escape routes. The
 *   theorem establishes an irreducible structural property of discrete
 *   infinite systems: sufficiently large structured sets must contain
 *   homogeneous substructures regardless of how they are partitioned. No
 *   agent, no matter their resources or strategic position, can construct a
 *   finite coloring scheme that violates this constraint. The theorem
 *   generates no beneficiaries or victims — it is indifferent to agents
 *   entirely. It emerges naturally from first-order logic and the infinite
 *   pigeonhole principle.
 *
 * KEY AGENTS:
 *   - Mathematical Systems: Passive subjects of the constraint; all finite colorings must satisfy the homogeneity requirement
 *   - Logical Structure: The constraint inheres in logical necessity, not in any agent's actions or incentives
 *   - Analytical Observer: Recognizes the constraint as a law of mathematical truth, not a social construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ramsey_theorem_foundation, 0.12).
domain_priors:suppression_score(ramsey_theorem_foundation, 0.03).
domain_priors:theater_ratio(ramsey_theorem_foundation, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ramsey_theorem_foundation, extractiveness, 0.12).
narrative_ontology:constraint_metric(ramsey_theorem_foundation, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(ramsey_theorem_foundation, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ramsey_theorem_foundation, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(ramsey_theorem_foundation, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ramsey_theorem_foundation, mountain).
narrative_ontology:human_readable(ramsey_theorem_foundation, "Ramsey Theorem Foundation").
narrative_ontology:topic_domain(ramsey_theorem_foundation, "combinatorics/logic/mathematics").

domain_priors:emerges_naturally(ramsey_theorem_foundation).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FINITE COLORING AGENT (MOUNTAIN) — No agent can escape the theorem's constraint by choosing different colorings or cardinalities. For any finite coloring of the integers, a monochromatic infinite subset must exist. This is not contingent on the agent's power, strategy, or perspective. It holds universally and irreversibly.
constraint_indexing:constraint_classification(ramsey_theorem_foundation, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMBINATORIAL SYSTEM DESIGNER (MOUNTAIN) — Even organized agents with full mobility cannot design a finite coloring that evades the monochromatic infinite subset requirement. The constraint applies uniformly regardless of system design choices. Accessibility to alternative structures is zero.
constraint_indexing:constraint_classification(ramsey_theorem_foundation, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational/universal analytical context, Ramsey's theorem is an invariant logical structure. The constraint emerges from the pigeonhole principle and combinatorial necessity, not from any contingent institutional or strategic arrangement. Zero degrees of freedom for all indices.
constraint_indexing:constraint_classification(ramsey_theorem_foundation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ramsey_theorem_foundation_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ramsey_theorem_foundation, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ramsey_theorem_foundation, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ramsey_theorem_foundation, ExtMetricName, E),
    domain_priors:suppression_score(ramsey_theorem_foundation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ramsey_theorem_foundation),
    narrative_ontology:constraint_metric(ramsey_theorem_foundation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ramsey_theorem_foundation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ramsey_theorem_foundation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The theorem has no extraction mechanism — it neither benefits nor burdens any agent. The low score reflects that all mathematical laws score at the lower bound; 0.12 represents instrumental costs of applying the theorem in computational contexts (searching for monochromatic subsets is computationally expensive), not structural extraction. Suppression (0.03): Negligible. There is no alternative framework that is suppressed. Mathematical alternatives (non-Ramsey colorings) are logically impossible, not suppressed by institutional power. Resistance (0.08): Very low. No resistance to the theorem's truth value exists in any meaningful sense — the theorem is proven and accepted universally across all mathematical communities. Theater ratio (0.05): Negligible. Mathematical proof is entirely functional; there is no performative component to establishing Ramsey's theorem.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify the constraint as Mountain because it is a genuine mathematical law. There is no perspectival gap — the theorem's truth is invariant across all observational positions. The constraint is not contingent on power, time horizon, exit options, or spatial scope. This uniformity is diagnostic of a true natural law in the mathematical domain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for mountain constraints with no beneficiaries or victims. The theorem is not an extraction mechanism and has no targeting logic. All agents experience the constraint equally as an immutable logical boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy arises because the constraint is pure mathematical law. There is no possibility of misclassifying coordination as extraction or vice versa — the constraint is neither. The theorem satisfies the mountain gates: accessibility_collapse = 0.92 (no alternative colorings can escape the constraint), resistance = 0.08 (universal mathematical acceptance), emerges_naturally = true (flows from first principles). The uniform classification across all perspectives confirms the mountain classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ramsey_theorem_foundation, 0, 0).

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
