% ============================================================================
% CONSTRAINT STORY: traveling_salesperson_problem
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_traveling_salesperson_problem, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: traveling_salesperson_problem
 *   human_readable: Applied Logistics Optimization (based on TSP)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   This constraint models the application of solutions to the Traveling
 *   Salesperson Problem (TSP) in industrial logistics. While TSP is a
 *   mathematical coordination problem, its implementation becomes a system
 *   of control that optimizes for corporate efficiency (fuel, time) while
 *   often externalizing costs (unpaid time, stress, physical exhaustion)
 *   onto drivers. The system coordinates deliveries but also extracts value
 *   asymmetrically from its human components.
 *
 * KEY AGENTS (by structural relationship):
 *   - Delivery Drivers: Primary target (powerless/trapped) — bear the physical and temporal costs of the optimized route, enforced by performance metrics.
 *   - Logistics Companies: Primary beneficiary (institutional/arbitrage) — benefit from reduced operational costs and increased efficiency.
 *   - Operations Researchers: Analytical observer — see the full structure, including the coordination function and the extractive consequences.
 *
 * DUAL FORMULATION NOTE:
 * This constraint is one of 2 stories decomposed from "The Traveling Salesperson Problem".
 * Decomposed because ε differs across observables (ε-invariance principle).
 * This story models the *applied logistics system* (ε=0.60, Tangled Rope).
 * The other story would be:
 *   - tsp_complexity_class (ε=0.05, Mountain) — The mathematical fact that TSP is NP-hard.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% High extraction, representing wasted fuel/labor from suboptimal heuristics
% and the externalized costs (stress, unpaid time) pushed onto drivers.
domain_priors:base_extractiveness(traveling_salesperson_problem, 0.60).
% High suppression; drivers have no alternative but to follow the prescribed
% route due to GPS tracking, performance KPIs, and employment contracts.
domain_priors:suppression_score(traveling_salesperson_problem, 0.65).
% Low theater; this is a highly functional, non-performative system.
domain_priors:theater_ratio(traveling_salesperson_problem, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(traveling_salesperson_problem, extractiveness, 0.60).
narrative_ontology:constraint_metric(traveling_salesperson_problem, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(traveling_salesperson_problem, theater_ratio, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(traveling_salesperson_problem, tangled_rope).

% --- Binary flags ---
% Required for Tangled Rope. Enforced via GPS monitoring and performance KPIs.
domain_priors:requires_active_enforcement(traveling_salesperson_problem).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(traveling_salesperson_problem, logistics_companies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(traveling_salesperson_problem, delivery_drivers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE DELIVERY DRIVER (SNARE)
% The driver is trapped by performance metrics and GPS monitoring. The
% mathematically "optimal" route ignores human needs, becoming a punitive
% standard that extracts uncompensated time and well-being.
% Engine derives d from: victim + trapped → d ≈ 0.95 → f(d) ≈ 1.42.
% χ ≈ 0.60 * 1.42 * 0.8 (local) ≈ 0.68 (Snare).
constraint_indexing:constraint_classification(traveling_salesperson_problem, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE LOGISTICS COMPANY (ROPE)
% From the company's perspective, this is a pure coordination tool (a Rope)
% for allocating resources (vehicles, fuel, driver time) efficiently.
% Engine derives d from: beneficiary + arbitrage → d ≈ 0.05 → f(d) ≈ -0.12.
% χ is negative, indicating a subsidy/benefit.
constraint_indexing:constraint_classification(traveling_salesperson_problem, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both sides: the genuine coordination function that benefits
% the company and the asymmetric extraction imposed on the drivers. This
% hybrid nature defines it as a Tangled Rope.
constraint_indexing:constraint_classification(traveling_salesperson_problem, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(traveling_salesperson_problem_tests).

test(perspectival_gap_driver_vs_company) :-
    % Verify the driver sees a Snare while the company sees a Rope.
    constraint_indexing:constraint_classification(traveling_salesperson_problem, snare, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(traveling_salesperson_problem, rope, context(agent_power(institutional), _, arbitrage, _)).

test(analytical_view_is_tangled_rope) :-
    % The analytical view must resolve the gap as a Tangled Rope.
    constraint_indexing:constraint_classification(traveling_salesperson_problem, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify all three conditions for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(traveling_salesperson_problem, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(traveling_salesperson_problem, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(traveling_salesperson_problem).

:- end_tests(traveling_salesperson_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file conflated the mathematical problem of TSP (a Mountain of
 *   complexity) with its industrial application (a Tangled Rope of logistics).
 *   This version resolves the conflict by focusing on the applied system, which
 *   has high, measurable extraction (ε=0.60) and suppression (0.65). The
 *   extraction represents both real-world waste (fuel, time) and the
 *   externalized human costs borne by drivers. Suppression comes from the
 *   coercive nature of modern performance management (KPIs, GPS tracking).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the logistics company (beneficiary), the system is a
 *   pure coordination tool (Rope) that creates value. For the driver (victim),
 *   it's a punitive control system (Snare) that extracts their well-being and
 *   unpaid time. The analytical observer sees both functions operating
 *   simultaneously, classifying it correctly as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `logistics_companies` gain efficiency and profit. Their
 *     arbitrage exit option gives them a low directionality `d`.
 *   - Victim: `delivery_drivers` bear the physical and temporal costs. Their
 *     trapped status (risk of job loss) gives them a high directionality `d`.
 *   This structural opposition is what drives the classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies that a system with a valid
 *   coordination function (delivering packages) can simultaneously be highly
 *   extractive. A naive analysis might label it a pure Rope (focusing on
 *   efficiency) or a pure Snare (focusing on labor exploitation). The Tangled
 *   Rope classification acknowledges that both are true, preventing the
 *   coordination aspect from masking the extractive one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_tsp_quantum,
    'Will quantum annealing provide near-instant optimal TSP solutions, eliminating heuristic inefficiency?',
    'Benchmarking quantum annealers against classical heuristics on N>1000 node, real-world logistics networks.',
    'If YES, base extraction from inefficiency drops, but extraction from labor externalization may remain, shifting the constraint towards a purer Snare. If NO, the Tangled Rope structure persists.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(traveling_salesperson_problem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified over time with the advent of GPS tracking
% and algorithmic management, increasing extraction from labor.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(tsp_tr_t0, traveling_salesperson_problem, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tsp_tr_t5, traveling_salesperson_problem, theater_ratio, 5, 0.05).
narrative_ontology:measurement(tsp_tr_t10, traveling_salesperson_problem, theater_ratio, 10, 0.05).

% Extraction over time (increases as monitoring tightens):
narrative_ontology:measurement(tsp_ex_t0, traveling_salesperson_problem, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(tsp_ex_t5, traveling_salesperson_problem, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(tsp_ex_t10, traveling_salesperson_problem, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(traveling_salesperson_problem, resource_allocation).

% Network relationships
% This applied constraint is downstream of the mathematical mountain.
narrative_ontology:affects_constraint(tsp_complexity_class, traveling_salesperson_problem).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% groups and exit options accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */