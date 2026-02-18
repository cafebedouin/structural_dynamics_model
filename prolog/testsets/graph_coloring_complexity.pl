% ============================================================================
% CONSTRAINT STORY: graph_coloring_complexity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_graph_coloring_complexity, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: graph_coloring_complexity
 *   human_readable: Application of Graph Coloring to Resource Allocation
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint models the application of graph coloring to solve practical
 *   resource allocation and scheduling problems (e.g., frequency assignment,
 *   register allocation). While the underlying mathematical theorems about
 *   coloring are mountains of logic, their application in resource-constrained
 *   systems creates both coordination benefits and extractive costs. The
 *   NP-hard nature of finding the optimal coloring (chromatic number) forces
 *   systems to use heuristics, which can lead to sub-optimal resource use,
 *   imposing a performance cost.
 *
 * KEY AGENTS (by structural relationship):
 *   - Compiler Optimizers: Primary target (powerless/constrained) — bears the
 *     cost of sub-optimal register allocation ("spills") when the interference
 *     graph is complex and available registers (colors) are few.
 *   - Spectrum Regulators / Network Architects: Primary beneficiary
 *     (institutional/arbitrage) — uses the coloring model as a coordination
 *     mechanism to prevent interference and enable efficient resource reuse.
 *   - Discrete Mathematicians: Analytical observer — studies the underlying
 *     mathematical properties, which are a separate 'mountain' constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The constraint extracts "efficiency" and "simplicity." The
% requirement to separate adjacent nodes forces the consumption of extra
% "colors" (resources), and the computational difficulty of finding the
% optimal solution extracts performance (e.g., compile time, runtime speed).
domain_priors:base_extractiveness(graph_coloring_complexity, 0.40).

% Rationale: It suppresses "overlapping utility." The constraint makes
% shared-resource alternatives invisible or illegal within the formal model.
domain_priors:suppression_score(graph_coloring_complexity, 0.50).

% Rationale: A purely technical constraint with minimal performative aspect.
domain_priors:theater_ratio(graph_coloring_complexity, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(graph_coloring_complexity, extractiveness, 0.40).
narrative_ontology:constraint_metric(graph_coloring_complexity, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(graph_coloring_complexity, theater_ratio, 0.03).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(graph_coloring_complexity, tangled_rope).
narrative_ontology:human_readable(graph_coloring_complexity, "Application of Graph Coloring to Resource Allocation").
narrative_ontology:topic_domain(graph_coloring_complexity, "technological").

% --- Binary flags ---
% Rationale: The rules of coloring must be actively enforced by the system
% (e.g., the compiler's allocation algorithm, the network's protocol) to
% function. This is required for the Tangled Rope classification.
domain_priors:requires_active_enforcement(graph_coloring_complexity).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(graph_coloring_complexity, spectrum_regulators).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(graph_coloring_complexity, compiler_optimizers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE COMPILER OPTIMIZER (SNARE)
% For a compiler performing register allocation, the constraint is a Snare.
% As variables (nodes) increase and their lifetimes overlap (edges), the
% available physical registers (colors) become insufficient. The optimizer is
% trapped by the complexity, forced to "spill" variables to slow memory,
% strangling performance.
constraint_indexing:constraint_classification(graph_coloring_complexity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: THE RF ENGINEER / SPECTRUM REGULATOR (ROPE)
% For the engineer, graph coloring is a Rope. It is the coordination
% mechanism that prevents signal interference. By treating cellular towers as
% nodes and frequencies as colors, they use the constraint to create a
% high-functioning network where chaos would otherwise reign.
constraint_indexing:constraint_classification(graph_coloring_complexity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (Rope for engineers) and
% the asymmetric extraction (Snare for compilers). The system requires active
% enforcement to maintain its structure. This dual nature—a genuine
% coordination tool that also creates unavoidable, asymmetric costs due to its
% inherent complexity—is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(graph_coloring_complexity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(graph_coloring_complexity_tests).

test(perspectival_gap) :-
    % Verify that the target, beneficiary, and analyst all see different types.
    constraint_indexing:constraint_classification(graph_coloring_complexity, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(graph_coloring_complexity, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(graph_coloring_complexity, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates) :-
    % Verify that all structural requirements for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(graph_coloring_complexity, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(graph_coloring_complexity, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(graph_coloring_complexity).

test(threshold_validation) :-
    % For a Tangled Rope, ε should be >= 0.30 and suppression >= 0.40.
    narrative_ontology:constraint_metric(graph_coloring_complexity, extractiveness, E),
    narrative_ontology:constraint_metric(graph_coloring_complexity, suppression_requirement, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(graph_coloring_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file incorrectly claimed this constraint was a 'mountain'
 *   while assigning metrics (ε=0.4, S=0.5) that violate mountain thresholds
 *   (ε<=0.25, S<=0.05). This regeneration resolves the conflict by reclassifying
 *   the constraint as a 'tangled_rope'. This correctly models the dual nature
 *   of applying graph coloring: it is a powerful coordination tool (Rope) for
 *   some agents (network architects) but an extractive trap (Snare) for others
 *   (compilers) due to its inherent computational complexity. The analytical
 *   view, seeing both sides, correctly identifies it as a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   - The Network Architect (institutional) has arbitrage exit; they can physically
 *     move towers or lobby for more spectrum, changing the graph or the number
 *     of colors. For them, it's a pure coordination Rope.
 *   - The Compiler Optimizer (powerless) is constrained by the hardware (fixed
 *     registers) and the user's code (fixed graph). It cannot change the rules
 *     and must bear the performance cost of spills, making it a Snare.
 *   - The Analyst sees this irreducible duality and classifies it as Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'spectrum_regulators' who achieve a stable, interference-
 *     free allocation of a public resource.
 *   - Victims: 'compiler_optimizers' which are forced to expend computational
 *     resources (time, memory) to solve or approximate a solution, with costs
 *     passed on to developers and end-users as slower performance.
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying this as a Tangled Rope, we avoid the "false natural law"
 *   error of calling it a Mountain. The NP-hardness theorem is a Mountain, but
 *   its *application* in a socio-technical system is not. This distinction is
 *   critical: it shows that while the math is fixed, the *costs* of applying
 *   that math are a result of system design choices (e.g., how many registers
 *   a CPU has) and are therefore subject to change and analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    p_np_coloring_gap,
    "Is there a hidden heuristic that allows for nearly-optimal coloring on all 'real-world' graphs without hitting the NP-hard wall?",
    "Longitudinal study of average-case vs. worst-case graph complexity in industrial datasets",
    "If Yes: The Snare aspect weakens significantly, shifting the constraint towards a pure Rope. If No: The Tangled Rope classification remains robust.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(graph_coloring_complexity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a stable, long-standing technical constraint. The metrics are not
% expected to drift over the interval.
narrative_ontology:measurement(gcc_tr_t0, graph_coloring_complexity, theater_ratio, 0, 0.03).
narrative_ontology:measurement(gcc_tr_t5, graph_coloring_complexity, theater_ratio, 5, 0.03).
narrative_ontology:measurement(gcc_tr_t10, graph_coloring_complexity, theater_ratio, 10, 0.03).

narrative_ontology:measurement(gcc_ex_t0, graph_coloring_complexity, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(gcc_ex_t5, graph_coloring_complexity, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(gcc_ex_t10, graph_coloring_complexity, base_extractiveness, 10, 0.40).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Graph coloring is a canonical example of a resource allocation problem.
narrative_ontology:coordination_type(graph_coloring_complexity, resource_allocation).

% --- Network Decomposition (Constraint Families) ---
%
% DUAL FORMULATION NOTE:
% This constraint is one of 2 stories decomposed from "Graph Coloring".
% Decomposed because ε differs across observables (ε-invariance principle).
% This story models the *application* of coloring to resource allocation. The
% underlying mathematical fact is a separate, non-extractive constraint.
% Related stories:
%   - graph_coloring_theorem_np_hard (ε=0.01, Mountain)
%
narrative_ontology:affects_constraint(graph_coloring_theorem_np_hard, graph_coloring_complexity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The structural derivation from beneficiary/victim
% declarations and exit options correctly models the directionality for each
% agent perspective.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */