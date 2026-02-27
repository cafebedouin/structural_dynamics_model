% ============================================================================
% CONSTRAINT STORY: fgh_hierarchy_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_fgh_hierarchy_2026, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fgh_hierarchy_2026
 *   human_readable: The Fast-Growing Hierarchy
 *   domain: mathematical/computational
 *
 * SUMMARY:
 *   The Fast-Growing Hierarchy (FGH) is a family of functions indexed by
 *   transfinite ordinals that classifies the growth rates of computable
 *   functions. It is a Mountain—a logical limit where the "extraction" is the
 *   irreducible cost of computation itself. Alternatives are not actively
 *   suppressed; they are simply logically incoherent or occupy a lower,
 *   well-defined rung on the hierarchy.
 *
 * KEY AGENTS (by structural relationship):
 *   - Computable Functions: Primary subject (powerless/trapped) — Bound by the recursive limits defined by ordinals.
 *   - Formal Systems (e.g., PA, ZFC): Structural context (institutional/arbitrage) — The hierarchy provides a measure of their proof-theoretic strength.
 *   - Complexity Theorists: Analytical observer — Mapping the transfinite landscape.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Mountains are defined by near-zero extraction and negligible suppression.
domain_priors:base_extractiveness(fgh_hierarchy_2026, 0.01).
domain_priors:suppression_score(fgh_hierarchy_2026, 0.01).
domain_priors:theater_ratio(fgh_hierarchy_2026, 0.10).       % Low theater; pure abstract logic.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fgh_hierarchy_2026, extractiveness, 0.01).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification, and the mountain metric gate will not fire.
narrative_ontology:constraint_metric(fgh_hierarchy_2026, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(fgh_hierarchy_2026, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fgh_hierarchy_2026, mountain).
narrative_ontology:human_readable(fgh_hierarchy_2026, "The Fast-Growing Hierarchy").
narrative_ontology:topic_domain(fgh_hierarchy_2026, "mathematical/computational").

% --- Binary flags ---
% No flags needed for a Mountain.

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges naturally from the definitions of recursion and
% ordinals without human design or enforcement. Required for the mountain
% metric gate.
domain_priors:emerges_naturally(fgh_hierarchy_2026).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (natural law), this constraint has no
% structurally defined beneficiaries or victims. Its effects are uniform and
% a feature of the logical landscape.

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

% PERSPECTIVE 1: THE COMPUTATIONAL PROCESS (MOUNTAIN)
% As a logical law, the classification is Mountain from all perspectives.
% The low base extraction (ε=0.01) and suppression (0.01) ensure that even
% with maximum powerlessness scaling, χ remains negligible.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE INSTITUTIONAL FRAMEWORK (MOUNTAIN)
% Formal systems do not "benefit" from the hierarchy; they are measured by it.
% The classification remains Mountain, reflecting its status as a fixed
% feature of mathematical reality.
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The analytical observer also classifies this as a Mountain, confirming its
% status as a uniform-type constraint (natural law).
constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fgh_hierarchy_2026_tests).

test(uniform_mountain_classification) :-
    % Verify it is classified as a Mountain from all declared perspectives.
    constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(fgh_hierarchy_2026, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_metric_thresholds) :-
    % Verify metrics adhere to the engine's Mountain gate thresholds.
    narrative_ontology:constraint_metric(fgh_hierarchy_2026, extractiveness, E),
    narrative_ontology:constraint_metric(fgh_hierarchy_2026, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(fgh_hierarchy_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Fast-Growing Hierarchy is a quintessential Mountain. Its properties are
 *   not subject to institutional negotiation or biographical agency. The base
 *   extraction (0.01) represents the irreducible cost of computation itself.
 *   The suppression score (0.01) is low because alternatives are not actively
 *   coerced; they are simply incoherent or non-existent. The addition of the
 *   Natural Law (NL) profile metrics (accessibility_collapse=1.0, resistance=0.0)
 *   and the emerges_naturally flag ensures it passes the engine's strict
 *   certification for Mountain classification.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a uniform-type constraint (natural law),
 *   it is classified as a Mountain from all possible indices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable. As a Mountain, the constraint lacks the
 *   structurally defined beneficiaries and victims required for the engine to
 *   derive a meaningful directionality (d). The effects are symmetric and
 *   universal.
 *
 * MANDATROPHY ANALYSIS:
 *   The low extraction score (0.01) means Mandatrophy is not a concern. The
 *   constraint is correctly identified as a feature of the logical landscape
 *   rather than a system of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_fgh_physicality,
    'Does the physical universe support computation up to arbitrary recursive ordinals, or is there a physical cutoff (e.g., related to the Bekenstein bound)?',
    'Empirical tests of quantum gravity models against information processing limits.',
    'A physical limit would mean the logical Mountain is bounded by a physical Mountain, but does not change the logical classification.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fgh_hierarchy_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. For a logical Mountain, the values
% are static, reflecting its time-invariant nature.
%
% Theater ratio over time:
narrative_ontology:measurement(fgh_hierarchy_2026_tr_t0, fgh_hierarchy_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(fgh_hierarchy_2026_tr_t5, fgh_hierarchy_2026, theater_ratio, 5, 0.10).
narrative_ontology:measurement(fgh_hierarchy_2026_tr_t10, fgh_hierarchy_2026, theater_ratio, 10, 0.10).

% Extraction over time:
narrative_ontology:measurement(fgh_hierarchy_2026_ex_t0, fgh_hierarchy_2026, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(fgh_hierarchy_2026_ex_t5, fgh_hierarchy_2026, base_extractiveness, 5, 0.01).
narrative_ontology:measurement(fgh_hierarchy_2026_ex_t10, fgh_hierarchy_2026, base_extractiveness, 10, 0.01).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Not applicable for a Mountain.
% narrative_ontology:coordination_type(fgh_hierarchy_2026, [type]).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(fgh_hierarchy_2026, [other_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a Mountain.
% constraint_indexing:directionality_override(fgh_hierarchy_2026, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */