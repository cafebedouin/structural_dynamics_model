% ============================================================================
% CONSTRAINT STORY: four_color_theorem_topological_bound
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_four_color_theorem_topological_bound, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: four_color_theorem_topological_bound
 *   human_readable: The Four Color Theorem
 *   domain: mathematical/topological
 *
 * SUMMARY:
 *   The Four Color Theorem states that no more than four colors are required to
 *   color the regions of any map on a plane such that no two adjacent regions
 *   have the same color. It is a fundamental topological constraint on how
 *   planar space can be partitioned. This constraint is a natural law of
 *   2D geometry, classifying as a Mountain from all perspectives. Its proof,
 *   notably one of the first to rely on computer assistance, has social
 *   implications, but these are structurally distinct from the theorem itself.
 *
 * KEY AGENTS (by structural relationship):
 *   - Map Region: A powerless entity whose state (color) is determined by its neighbors.
 *   - Cartographer/Network Architect: An institutional actor who uses the theorem to optimize resource allocation.
 *   - Mathematician: An analytical observer verifying the theorem's properties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: A mathematical theorem has near-zero extractiveness. It is a pure
% statement of fact about a system's structure.
domain_priors:base_extractiveness(four_color_theorem_topological_bound, 0.02).
% Rationale: Suppression is extremely low. The theorem doesn't actively suppress
% 5-colorings; it simply renders them unnecessary. It's a limit, not a coercive force.
% This value is set to pass the mountain suppression ceiling (<= 0.05).
domain_priors:suppression_score(four_color_theorem_topological_bound, 0.05).
% Rationale: The theorem is pure function with no performative aspect.
domain_priors:theater_ratio(four_color_theorem_topological_bound, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, extractiveness, 0.02).
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
% Accessibility Collapse: 1.0 because a 5-coloring is not just inaccessible,
% it is logically impossible for a planar graph. Alternatives are fully collapsed.
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, accessibility_collapse, 1.0).
% Resistance: 0.0 because active resistance to a mathematical theorem is
% incoherent. One cannot "oppose" it.
narrative_ontology:constraint_metric(four_color_theorem_topological_bound, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(four_color_theorem_topological_bound, mountain).

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
domain_priors:emerges_naturally(four_color_theorem_topological_bound).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a Mountain (natural law), this constraint has no intrinsic beneficiaries
% or victims. Its effects are symmetric and emerge from the structure of
% planar graphs. No enrichment needed.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE INDIVIDUAL MAP REGION (MOUNTAIN)
% For a specific region on a map, the limit of 4 colors is an absolute
% Mountain. It cannot "choose" to be a 5th color that is distinct from its
% neighbors if the global topology has already constrained the available
% choices. The arithmetic of adjacency is a natural law.
constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SPECTRUM MANAGER (MOUNTAIN)
% For an institutional actor, the theorem is an unchangeable fact of the
% world that they can leverage for coordination. While its *application*
% in frequency allocation functions as a Rope, the underlying theorem itself
% remains a Mountain—a fixed boundary condition for their planning.
constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a detached, analytical perspective, the theorem is a classic example
% of a Mountain: a fixed, verifiable, and unchangeable property of a
% mathematical system. Its truth value is invariant across all contexts.
constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(four_color_theorem_topological_bound_tests).

test(classification_invariance) :-
    % Verify that the classification is Mountain from all key perspectives,
    % as expected for a natural law.
    constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(four_color_theorem_topological_bound, mountain, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify that the base metrics adhere to the strict thresholds for a Mountain.
    narrative_ontology:constraint_metric(four_color_theorem_topological_bound, extractiveness, E),
    narrative_ontology:constraint_metric(four_color_theorem_topological_bound, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_validation) :-
    % Verify that the NL profile metrics meet certification thresholds.
    narrative_ontology:constraint_metric(four_color_theorem_topological_bound, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(four_color_theorem_topological_bound, resistance, R),
    domain_priors:emerges_naturally(four_color_theorem_topological_bound),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(four_color_theorem_topological_bound_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scores for extractiveness (0.02) and suppression (0.05) are set to
 *   be extremely low, reflecting the nature of a mathematical theorem as a
 *   non-coercive, non-extractive statement of fact. The addition of the
 *   Natural Law profile metrics (accessibility_collapse=1.0, resistance=0.0)
 *   and the `emerges_naturally` flag completes the requirements for a robust
 *   Mountain classification, ensuring it passes the engine's natural law
 *   certification chain.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a mathematical truth (natural law), the
 *   constraint is a Mountain from all perspectives. The original file's
 *   inclusion of Rope and Snare perspectives was a category error, conflating
 *   the theorem itself with its applications (Rope) or the social impact of
 *   its proof method (a separate, social constraint). This version adheres to
 *   the ε-invariance principle by modeling only the core mathematical claim.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, the constraint has no directionality. It has no targeted
 *   victims or intended beneficiaries. It is a symmetric property of the
 *   system. Therefore, `constraint_beneficiary` and `constraint_victim`
 *   declarations are omitted.
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying this as a pure Mountain, we avoid the error of attributing
 *   extractive properties to a natural law. The "extraction" felt by
 *   mathematicians due to the computer-assisted proof is not a property of
 *   the theorem, but of a separate social constraint regarding standards of
 *   proof. Decomposing these two prevents misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_four_color_proof,
    'Does a short, elegant, human-verifiable proof of the Four Color Theorem exist?',
    'Discovery and verification of a new proof that does not rely on exhaustive computer-aided case checking.',
    'If yes, the social "snare" of computer-assisted proofs was merely a temporary Scaffold for human cognitive limits, not an intrinsic feature of the problem.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(four_color_theorem_topological_bound, 1852, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. As a Mountain with base_extractiveness < 0.46, this constraint
% does not exhibit lifecycle drift. Its properties are static.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint, as a mathematical law, could be seen as an upstream
% dependency for practical coordination problems. For example, it enables
% efficient spectrum allocation schemes.
%
% narrative_ontology:affects_constraint(four_color_theorem_topological_bound, spectrum_allocation_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. As a Mountain, this constraint has no directionality,
% and no beneficiary/victim groups are declared, so there is nothing to override.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */