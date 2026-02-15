% ============================================================================
% CONSTRAINT STORY: local_vs_global_optima
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_local_vs_global_optima, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: local_vs_global_optima
 *   human_readable: The Existence of Local Optima in Non-Convex Spaces
 *   domain: mathematical/computational
 *
 * SUMMARY:
 *   In any non-convex optimization landscape, there exist "local optima"—solutions
 *   that are superior to all their immediate neighbors but are not the best
 *   possible solution (the "global optimum"). This mathematical reality forces
 *   any search or optimization process to risk getting "stuck" on a suboptimal peak.
 *   The constraint is the structure of the landscape itself, not the agent's strategy.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Hill-Climber: An agent using a simple, greedy algorithm (powerless/trapped) — experiences the constraint as an impassable barrier.
 *   - The Explorer/Annealer: An agent with a sophisticated strategy (moderate/mobile) — navigates the constraint as a known feature of the terrain.
 *   - The Mathematician: Analytical observer — understands the constraint as a fundamental, unchangeable property of the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% The landscape itself does not extract value; an agent's poor strategy does.
% The base extractiveness of the mathematical law is near zero.
domain_priors:base_extractiveness(local_vs_global_optima, 0.05).
% The landscape does not coercively suppress alternatives; it simply is.
domain_priors:suppression_score(local_vs_global_optima, 0.05).
% A mathematical truth has no performative aspect.
domain_priors:theater_ratio(local_vs_global_optima, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(local_vs_global_optima, extractiveness, 0.05).
narrative_ontology:constraint_metric(local_vs_global_optima, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(local_vs_global_optima, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
%
% Alternatives to this mathematical reality are inconceivable.
narrative_ontology:constraint_metric(local_vs_global_optima, accessibility_collapse, 1.0).
% One cannot "resist" a mathematical theorem.
narrative_ontology:constraint_metric(local_vs_global_optima, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(local_vs_global_optima, mountain).

% --- Emergence flag (required for mountain constraints) ---
% This is a fundamental property of mathematics, not a human construction.
domain_priors:emerges_naturally(local_vs_global_optima).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. This is a uniform-type Mountain constraint (natural law).
% It has no beneficiaries or victims in the structural sense.

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

% This is a uniform-type constraint (Mountain-only). The classification is
% invariant across all perspectives because it is a mathematical law. The
% agent's subjective experience (e.g., feeling "trapped") does not change the
% objective structure of the constraint itself.

% PERSPECTIVE 1: THE HILL-CLIMBER (MOUNTAIN)
% A simple, greedy algorithm that can only perceive immediate improvements.
% While it experiences being "trapped," the trap is a feature of its own
% limited strategy interacting with an immutable landscape (a Mountain).
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE EXPLORER / ANNEALER (MOUNTAIN)
% An agent with a more sophisticated strategy (e.g., simulated annealing)
% that can accept short-term losses to find better long-term solutions.
% It perceives the landscape not as a trap, but as a terrain to be navigated—
% a fundamental, unchangeable Mountain.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE MATHEMATICIAN (MOUNTAIN)
% The analytical observer who understands the formal properties of non-convex
% spaces. To the mathematician, the existence of local optima is an
% axiom-derived, immutable law—a quintessential Mountain.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE OPTIMIZATION ENGINEER (MOUNTAIN)
% An institutional actor who designs systems (e.g., machine learning models)
% that must operate within this reality. The engineer treats the existence
% of local optima as a given, a foundational Mountain around which all
% effective strategies must be built.
constraint_indexing:constraint_classification(local_vs_global_optima, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(local_vs_global_optima_tests).

test(perspectival_invariance) :-
    % Verify that as a natural law, it classifies as Mountain from all key perspectives.
    constraint_indexing:constraint_classification(local_vs_global_optima, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(local_vs_global_optima, mountain, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(local_vs_global_optima, mountain, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    % Verify the metrics are consistent with a Mountain classification.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(local_vs_global_optima, ExtMetricName, E),
    narrative_ontology:constraint_metric(local_vs_global_optima, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(local_vs_global_optima_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The core decision is to model the *mathematical reality* of the optimization
 *   landscape, not the subjective experience of an agent navigating it. The
 *   existence of local optima is a fundamental property of non-convex spaces,
 *   making it a natural law.
 *
 *   - Base Extractiveness (ε=0.05): The landscape itself is passive; it does not
 *     actively extract value. The "loss" an agent experiences by getting stuck
 *     in a local optimum is a consequence of the agent's strategy, not a tax
 *     imposed by the constraint. The low ε reflects this structural passivity.
 *   - Suppression (S=0.05): The constraint does not use coercion to suppress
 *     alternatives. The global optimum is not hidden by force, but by the
 *     topography of the space.
 *   - NL Profile: Accessibility collapse is 1.0 because a different mathematical
 *     reality is inconceivable. Resistance is 0.0 because one cannot "oppose"
 *     a theorem. `emerges_naturally` is true. These facts certify it as a
 *     natural law.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in classification; all agents view the
 *   constraint's structure as a Mountain. The gap is in the *subjective
 *   experience*. A powerless "hill-climber" feels trapped (a Snare-like
 *   experience), while a mobile "explorer" feels challenged but free (a
 *   Rope-like experience). However, the underlying structure they are both
 *   interacting with is the same immutable Mountain. This story demonstrates
 *   the system's ability to distinguish objective structure from subjective
 *   experience.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain (natural law), this constraint has no beneficiaries or
 *   victims. It is a symmetric feature of reality that affects all agents
 *   impartially, based on their chosen strategies. Therefore, no
 *   beneficiary/victim declarations are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the phenomenon as a feature of
 *   the environment (Mountain) rather than a coercive trap (Snare). A mis-
 *   classification as Snare would imply the local optimum is an artificial,
 *   enforced state designed to extract value, which is false. The framework
 *   correctly attributes the "trap" to the agent's limited power and exit
 *   options, not to the constraint's inherent structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_local_vs_global_optima,
    'Is the "cost" of being in a local optimum a form of structural extraction or merely a strategic failure?',
    'Conceptual analysis of agency vs. structure. Does a passive landscape that predictably causes failure in simple agents count as extractive?',
    'If considered extraction, ε would rise, potentially shifting the classification for powerless agents to Tangled Rope. If strategic failure, it remains a Mountain.',
    confidence_without_resolution(high)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_local_vs_global_optima, conceptual, 'Distinguishing structural extraction from strategic failure in a passive landscape.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(local_vs_global_optima, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. Base extractiveness is < 0.46, and the constraint is a
% timeless mathematical law.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for this type of fundamental constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. This is a symmetric, natural law constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */