% ============================================================================
% CONSTRAINT STORY: indexical_relativity_core
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_indexical_relativity_core, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indexical_relativity_core
 *   human_readable: The Law of Indexical Relativity
 *   domain: epistemological
 *
 * SUMMARY:
 *   This constraint establishes that the classification of any social or physical
 *   limitation is not an intrinsic property of the object itself. Instead, it is
 *   a function of the observer's index (Power, Time, Exit, Scope). This principle
 *   is modeled as a Mountain: a fundamental, unchangeable law of the analytical
 *   framework, analogous to a law of physics. The law *about* perspectival shifts
 *   is itself invariant across all perspectives.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Powerless Subject (powerless/trapped): Experiences the consequences of this law without being able to alter it.
 *   - The Institutional Architect (institutional/arbitrage): Uses this law to design or maintain systems.
 *   - The Analytical Observer (analytical/analytical): Observes this law as a fundamental principle of system dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indexical_relativity_core, 0.10).
domain_priors:suppression_score(indexical_relativity_core, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(indexical_relativity_core, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indexical_relativity_core, extractiveness, 0.10).
narrative_ontology:constraint_metric(indexical_relativity_core, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(indexical_relativity_core, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(indexical_relativity_core, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(indexical_relativity_core, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(indexical_relativity_core, mountain).
narrative_ontology:human_readable(indexical_relativity_core, "The Law of Indexical Relativity").

% --- Binary flags ---
% No active enforcement needed for a logical principle.

% --- Emergence flag (required for mountain constraints) ---
% This is an analytical truth that emerges from the structure of power.
domain_priors:emerges_naturally(indexical_relativity_core).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (a fundamental law), this constraint
% does not have specific beneficiary or victim groups in the structural sense.
% Understanding it benefits analysts, while ignoring it harms those who
% misclassify their environment, but these are outcomes of engagement with
% the law, not properties of the law itself.

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

% PERSPECTIVE 1: THE POWERLESS SUBJECT (MOUNTAIN)
% For the powerless, the law of indexical relativity is an unchangeable fact
% of their environment. They experience its effects as "natural law."
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSTITUTIONAL ARCHITECT (MOUNTAIN)
% For the architect, the law is also a Mountain. It is a fundamental principle
% they must account for when designing systems, like an engineer accounting
% for gravity. They cannot change the law, only work with or against it.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The analytical observer sees the law in its purest form: an invariant
% principle of the system, a foundational axiom. It is the bedrock upon
% which other classifications are built.
constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indexical_relativity_core_tests).

test(mountain_invariance) :-
    % Verify that the law is a Mountain from multiple key perspectives,
    % demonstrating its status as a fundamental, invariant principle.
    constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(indexical_relativity_core, mountain,
        context(agent_power(analytical), _, _, _)).

test(natural_law_metrics_adherence) :-
    % Verify that the constraint's metrics meet the strict thresholds for Mountain classification.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(indexical_relativity_core, ExtMetricName, E),
    narrative_ontology:constraint_metric(indexical_relativity_core, SuppMetricName, S),
    config:param(mountain_extractiveness_max, EMax),
    config:param(mountain_suppression_ceiling, SMax),
    E =< EMax,
    S =< SMax.

:- end_tests(indexical_relativity_core_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file modeled this constraint with a perspectival gap, where
 *   the classification changed for each agent. This was a logical error. The
 *   constraint is the *principle* of indexical relativity itself. A principle
 *   or law is either fundamental (Mountain) or a convention (Rope). Given that
 *   it emerges from the structure of power dynamics, it is best modeled as a
 *   Mountain—an unchangeable feature of the landscape.
 *   - Base Extractiveness (0.10): The principle itself is not extractive; it is a descriptive law.
 *   - Suppression Score (0.05): As a logical principle, it does not suppress alternatives; it describes what is. This was lowered from 0.2 to meet the Mountain threshold.
 *   - NL Profile: Added accessibility_collapse and resistance metrics to pass the natural_law_signature certification, consistent with its status as a foundational law.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. The constraint is a uniform-type Mountain. This is crucial: the law *about* perspectival relativity is *not* itself relative. It is an invariant. This demonstrates that the framework can distinguish between a principle and the application of that principle.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, this constraint does not require beneficiary/victim declarations. The directionality `d` is not a factor in Mountain classification, which is determined solely by base metrics (ε and suppression).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a core part of the Mandatrophy resolution toolkit. By establishing that classifications are indexical, it prevents the error of mistaking a Snare (as seen by the powerless) for a Rope (as described by the powerful), or a Rope for a Mountain. It forces an analysis of who benefits and who pays.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_indexical_relativity_core,
    'How quickly does an agent update their classification of a constraint after their own index (power, exit) changes?',
    'Measure the latency between a structural change in an agent''s status (e.g., unionization, election) and their public re-classification of a governing constraint.',
    'A long lag suggests "ideological stickiness" or high institutional inertia; a short lag suggests high system fluidity and agent rationality.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_indexical_relativity_core, empirical, 'The latency of agent perception shifts following a change in their structural power or exit options.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(indexical_relativity_core, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. As a Mountain with base_extractiveness < 0.46, this
% constraint is considered structurally stable with no lifecycle drift.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for a foundational epistemological constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. Mountain classification is independent of directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */