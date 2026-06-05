% ============================================================================
% CONSTRAINT STORY: mean_value_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mean_value_theorem, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mean_value_theorem
 *   human_readable: Application of the Mean Value Theorem
 *   domain: mathematical_physics/technological
 *
 * SUMMARY:
 *   The Mean Value Theorem (MVT) is a fundamental result in calculus
 *   asserting that if a function is continuous on a closed interval [a,b] and
 *   differentiable on the open interval (a,b), then there exists at least one
 *   point c in (a,b) where the instantaneous rate of change equals the
 *   average rate of change over the interval. When applied to technological
 *   and physical scenarios—such as determining average velocity from distance
 *   and time—the MVT functions as a pure mathematical constraint with zero
 *   degrees of freedom. No agent, institution, or technological system can
 *   circumvent the theorem. This constraint is a canonical mountain: it
 *   emerges naturally from logical necessity, offers zero discretionary exit,
 *   and carries zero suppression cost because it is transparent and binding
 *   on all observers equally. The theater ratio is minimal (0.15) because the
 *   MVT's verification is direct: compute the average rate, verify the
 *   function's continuity and differentiability, and the conclusion follows
 *   deductively. No performative or hidden apparatus is required.
 *
 * KEY AGENTS:
 *   - Mathematical Observer: Analytical perspective (analytical/analytical) — Views MVT as pure logical necessity
 *   - Engineer: Powerful technological actor (powerful/mobile) — Applies MVT in system design; cannot escape its constraints
 *   - Standards Authority: Institutional actor (institutional/arbitrage) — Must accommodate MVT in measurement standards and regulations
 *   - Collective Agents: Organized groups (organized/constrained) — Cannot collectively circumvent mathematical laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mean_value_theorem, 0.08).
domain_priors:suppression_score(mean_value_theorem, 0.03).
domain_priors:theater_ratio(mean_value_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mean_value_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(mean_value_theorem, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(mean_value_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mean_value_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(mean_value_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mean_value_theorem, mountain).
narrative_ontology:human_readable(mean_value_theorem, "Application of the Mean Value Theorem").
narrative_ontology:topic_domain(mean_value_theorem, "mathematical_physics/technological").

domain_priors:emerges_naturally(mean_value_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL OBSERVER (MOUNTAIN) — From the analytical standpoint, the MVT is an irreducible mathematical law: if a function is continuous on [a,b] and differentiable on (a,b), there must exist at least one point where the instantaneous rate equals the average rate. No agent can circumvent this. ε=0.08, suppression=0.03, accessibility_collapse=0.92, resistance=0.08.
constraint_indexing:constraint_classification(mean_value_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ENGINEER APPLYING MVT (MOUNTAIN) — Even powerful technological actors cannot escape the theorem's constraint. When calculating average speed over a journey, the MVT guarantees an intermediate moment where instantaneous speed equals average speed—regardless of power, resources, or sophistication. Exit is impossible because the constraint is mathematical, not institutional. d≈0.50, f(d)≈0.65, σ=1.2, but χ is irrelevant because ε is so low (0.08) that effective extraction vanishes.
constraint_indexing:constraint_classification(mean_value_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: STANDARDS AUTHORITY (MOUNTAIN) — No institutional actor can legislate around the MVT. Speed enforcement regulations that rely on distance/time measurement are structurally bound by the theorem. Any technological system measuring average velocity over a route must accommodate the MVT's guarantee. Suppression is zero—the constraint is transparent, not hidden.
constraint_indexing:constraint_classification(mean_value_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COLLECTIVE AGENTS (MOUNTAIN) — Even organized groups cannot collectively escape the MVT. Teams of engineers designing control systems, groups of physicists modeling motion, coalitions setting measurement standards—all must accommodate the theorem's guarantee. The constraint is independent of collective agency. d≈0.50, but χ remains negligible given ε=0.08.
constraint_indexing:constraint_classification(mean_value_theorem, mountain,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mean_value_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(mean_value_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mean_value_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(mean_value_theorem, ExtMetricName, E),
    domain_priors:suppression_score(mean_value_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(mean_value_theorem),
    narrative_ontology:constraint_metric(mean_value_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(mean_value_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(mean_value_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): The MVT extracts zero coordinated action from any agent—it is a passive constraint, not an active extraction mechanism. The low value reflects that the theorem is pure mathematics, not a governance or allocation mechanism. Any minimal nonzero value accounts for the fact that applying the theorem requires computational effort and domain knowledge, creating a trivial cognitive load. Suppression (0.03): Nearly zero. The MVT is transparent and its requirements are explicit. There are no hidden costs, no alternative pathways, no obfuscation. The constraint is laid bare in every application. Theater ratio (0.15): Minimal. MVT verification is deductive, not performative. Proving the theorem requires rigorous mathematical argumentation, but once proven, its application is straightforward. The small theater residual accounts for pedagogical presentation and teaching ritual, not functional obfuscation. Accessibility collapse (0.92): Very high. The constraint's logical structure is fully transparent and accessible to anyone with calculus training. No hidden mechanism. Resistance (0.08): Very low. No agent resists the MVT because it is logically necessary, not politically imposed.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on the same classification (mountain) and the same directionality (irrelevance of power, time, exit, scope). The perspectival gap is zero. This is a uniform-type constraint—a natural law that appears identical from all observation positions. The engineer views it as a technical boundary. The standards authority views it as a mathematical requirement. The collective agents view it as inescapable necessity. The analytical observer views it as a logical inevitability. All are seeing the same constraint structure because the structure is truly universal. No indexical reframing changes the classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for mountain constraints. The MVT imposes itself equally on all agents: d is indeterminate because the constraint is not extractive (no beneficiary/victim split). The theorem binds all observers symmetrically. Every application of the MVT respects it identically, regardless of the agent's power, time horizon, exit options, or spatial scope. The constraint is invariant under all changes of observer position.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    differentiability_scope_boundary,
    'In what physical systems does the differentiability assumption of MVT break down, and do these systems constitute genuinely different constraints or merely represent approximation regimes?',
    'Classification of physical systems by continuity/differentiability properties (e.g., discontinuous shock waves, quantum tunneling, fractal trajectories). Determination of whether non-differentiable motion represents a separate constraint or a limiting case of MVT.',
    'If non-differentiable systems form a distinct class: MVT constraint applies only to smooth/continuous motion, reducing its scope from universal. If they are limiting cases: MVT scope remains universal but with well-defined boundary conditions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(differentiability_scope_boundary, empirical, 'Scope of differentiability assumptions in physical systems').

omega_variable(
    measurement_discretization_compatibility,
    'Does the MVT apply to discrete measurement systems that sample motion at finite intervals, or only to idealized continuous functions?',
    'Comparison of MVT predictions with discrete-time velocity measurements. Analysis of whether finite sampling violates the theorem''s assumptions or merely limits its precision.',
    'If discrete systems preserve MVT: the constraint remains universal even in technological implementations. If discrete systems can violate MVT assumptions: the practical scope is narrower than the theoretical scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_discretization_compatibility, empirical, 'Whether MVT applies to discretely sampled systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mean_value_theorem, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mvt_tr_t0, mean_value_theorem, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mvt_tr_t5, mean_value_theorem, theater_ratio, 5, 0.15).
narrative_ontology:measurement(mvt_tr_t10, mean_value_theorem, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(mvt_be_t0, mean_value_theorem, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(mvt_be_t5, mean_value_theorem, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(mvt_be_t10, mean_value_theorem, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mean_value_theorem, information_standard).
narrative_ontology:affects_constraint(mean_value_theorem, rolle_theorem_constraint).
narrative_ontology:affects_constraint(mean_value_theorem, fundamental_theorem_calculus).

% DUAL FORMULATION NOTE:
% The MVT is upstream of multiple applied constraints in control theory, kinematics, and optimization. Any technological constraint relying on continuous motion guarantees derives from the MVT's logical foundation. The MVT itself is not decomposable into multiple structurally distinct constraints—ε is invariant across all observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
