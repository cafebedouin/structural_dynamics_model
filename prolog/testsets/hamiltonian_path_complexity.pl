% ============================================================================
% CONSTRAINT STORY: hamiltonian_path_complexity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_hamiltonian_path_complexity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hamiltonian_path_complexity
 *   human_readable: Computational Complexity of the Hamiltonian Path Problem
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   A Hamiltonian Path is a path in a graph that visits each vertex exactly
 *   once. Finding such a path is an NP-complete problem. While verifying a
 *   given path is easy, finding one is computationally exhaustive for large
 *   graphs. This constraint represents the immutable mathematical difficulty
 *   of solving this problem, a cornerstone of computational complexity theory.
 *
 * KEY AGENTS (by structural relationship):
 *   - Logistics Optimizers: Primary target (powerless/trapped) — bears the computational cost of finding optimal routes.
 *   - Cryptography Systems Designers: Primary beneficiary (institutional/arbitrage) — leverages computational hardness for security.
 *   - The Mathematician: Analytical observer — sees the problem's inherent, unchangeable complexity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% The "extraction" is not social but computational: the energy and time
% required for brute-force search. It's low because it's a property of logic,
% not a designed extractive system.
domain_priors:base_extractiveness(hamiltonian_path_complexity, 0.10).
% Suppression is near-zero. One cannot choose an "alternative" to a mathematical
% truth. The constraint is absolute. This low score is critical for the Mountain
% classification.
domain_priors:suppression_score(hamiltonian_path_complexity, 0.0).
% This is a purely functional mathematical constraint with no performative aspect.
domain_priors:theater_ratio(hamiltonian_path_complexity, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hamiltonian_path_complexity, extractiveness, 0.10).
narrative_ontology:constraint_metric(hamiltonian_path_complexity, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(hamiltonian_path_complexity, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% Accessibility Collapse: Alternatives (e.g., heuristics) are structurally
% different from finding the guaranteed optimal path. The constraint on
% optimality is nearly absolute.
narrative_ontology:constraint_metric(hamiltonian_path_complexity, accessibility_collapse, 0.98).
% Resistance: One cannot "resist" a mathematical truth; resistance is incoherent.
narrative_ontology:constraint_metric(hamiltonian_path_complexity, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
% Analytically, this is a Mountain of computational complexity.
narrative_ontology:constraint_claim(hamiltonian_path_complexity, mountain).
narrative_ontology:human_readable(hamiltonian_path_complexity, "Computational Complexity of the Hamiltonian Path Problem").
narrative_ontology:topic_domain(hamiltonian_path_complexity, "technological/mathematical").

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges naturally from the structure of mathematics and
% computation, without human design or enforcement.
domain_priors:emerges_naturally(hamiltonian_path_complexity).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Even though this is a Mountain from most perspectives, the existence of a
% Rope perspective (the Network Architect) necessitates these declarations
% to explain the coordination function.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(hamiltonian_path_complexity, cryptography_systems_designers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(hamiltonian_path_complexity, logistics_optimizers).

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

% PERSPECTIVE 1: THE LOGISTICS PLANNER (MOUNTAIN)
% For a planner who must find an optimal route visiting every node, the
% problem's complexity is an unchangeable, absolute barrier. It's not a
% snare designed by someone; it's a feature of the logical landscape.
% The low ε (0.1) and suppression (0.0) confirm a Mountain classification.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CRYPTOGRAPHY DESIGNER (ROPE)
% For an agent designing systems that rely on computational hardness, this
% complexity is a tool. It's a pure coordination device: the "agreement" is
% that certain problems are too hard to solve, forming the basis of security.
% They have arbitrage exit, as they can choose which hard problems to use.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE MATHEMATICIAN (MOUNTAIN)
% The analytical observer sees the P vs. NP problem as a fundamental,
% immutable feature of computation. It is a 'Mountain' in the landscape of
% logic, defining the boundaries of what is efficiently computable.
constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hamiltonian_path_complexity_tests).

test(perspectival_gap_mountain_rope) :-
    % Verify the specific gap: Mountain for the powerless and analytical,
    % but Rope for the institutional agent who can leverage it.
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(hamiltonian_path_complexity, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_thresholds_adherence) :-
    % Verify that the base metrics are consistent with a Mountain classification.
    narrative_ontology:constraint_metric(hamiltonian_path_complexity, extractiveness, E),
    narrative_ontology:constraint_metric(hamiltonian_path_complexity, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(hamiltonian_path_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.1) represents the irreducible computational
 *   work required to solve the problem, not a social extraction. The
 *   suppression score was set to 0.0 because a mathematical truth allows no
 *   alternatives, a key feature of a Mountain. These low scores ensure the
 *   Mountain gate fires for observers who are subject to the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between those who experience the constraint as a barrier
 *   (logisticians, mathematicians see a Mountain) and those who use it as a
 *   tool (cryptographers see a Rope). The institutional agent has arbitrage
 *   exit—they can choose to build systems around this specific hard problem
 *   or another one. This ability to leverage the Mountain turns it into a
 *   coordination mechanism for them.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `cryptography_systems_designers` benefit because the
 *     difficulty of solving the problem is the foundation of their security models.
 *   - Victim: `logistics_optimizers` bear the cost, as they must expend
 *     significant computational resources to find (or approximate) solutions.
 *   This beneficiary/victim structure is what enables the Rope classification
 *   from the institutional perspective, as it establishes a coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The original file was flagged for SCAFFOLD_DANGER_ZONE because the
 *   combination of low extraction, a beneficiary, and no enforcement could be
 *   misinterpreted by the engine as a Scaffold. By correcting the suppression
 *   score to 0.0, we ensure the constraint correctly passes the Mountain gate
 *   (which has priority over the Scaffold gate) for the powerless and analytical
 *   perspectives, resolving the ambiguity. The system correctly identifies a
 *   permanent feature of reality, not a temporary support structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% The core uncertainty is the P vs. NP problem itself.
omega_variable(
    omega_p_vs_np,
    'Is P equal to NP? (i.e., can an efficient algorithm find the Hamiltonian path in all cases?)',
    'A formal mathematical proof or disproof, a major unsolved problem in computer science.',
    'If P=NP, this constraint collapses from a Mountain to a Rope for all perspectives, with profound implications for cryptography and optimization. If P!=NP, the Mountain stands.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hamiltonian_path_complexity, 0, 10).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */