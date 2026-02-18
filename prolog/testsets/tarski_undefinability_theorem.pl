% ============================================================================
% CONSTRAINT STORY: constraint_tarski_undefinability
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-27
% ============================================================================

:- module(constraint_tarski_undefinability, []).

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
 *   constraint_id: constraint_tarski_undefinability
 *   human_readable: Tarski's Undefinability Theorem
 *   domain: technological
 *
 * SUMMARY:
 *   Tarski's undefinability theorem states that for sufficiently expressive formal systems, like Peano arithmetic, it is impossible to define a truth predicate for the language of the system within the system itself. This inherent limitation on self-reference acts as a mountain constraint, fundamentally limiting the capabilities of formal systems and artificial intelligence.
 *
 * KEY AGENTS (by structural relationship):
 *   - Formal Systems: Primary target (powerless/trapped) — limited in self-reflection.
 *   - Mathematicians/Logicians: Analytical observer (analytical/analytical) — understands the limitation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constraint_tarski_undefinability, 0.05).  % Very low; inherent limitation, not imposed.
domain_priors:suppression_score(constraint_tarski_undefinability, 0.01).    % Extremely low; alternative formal systems are possible, but the limitation remains.
domain_priors:theater_ratio(constraint_tarski_undefinability, 0.0).        % No performative aspect.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constraint_tarski_undefinability, extractiveness, 0.05).
narrative_ontology:constraint_metric(constraint_tarski_undefinability, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(constraint_tarski_undefinability, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(constraint_tarski_undefinability, accessibility_collapse, 0.95). % Alternatives exist but do not circumvent the problem
narrative_ontology:constraint_metric(constraint_tarski_undefinability, resistance, 0.01). % Very little resistance due to mathematical proof.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(constraint_tarski_undefinability, mountain).
narrative_ontology:human_readable(constraint_tarski_undefinability, "Tarski's Undefinability Theorem").
narrative_ontology:topic_domain(constraint_tarski_undefinability, "technological").

% --- Binary flags ---
% This is a mathematical theorem, no enforcement or sunset clauses apply.

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this, the classify_from_metrics
% mountain clause will not fire.
domain_priors:emerges_naturally(constraint_tarski_undefinability).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Not applicable for a mountain constraint. No enrichment needed.

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

% PERSPECTIVE 1: THE FORMAL SYSTEM (TARGET)
% As a natural law (mountain), the classification is invariant.
% The 'powerless' perspective represents the formal system itself,
% which is fundamentally constrained by the theorem.
constraint_indexing:constraint_classification(constraint_tarski_undefinability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE LOGICIAN (INSTITUTIONAL)
% The 'institutional' perspective represents the established body of
% mathematical and logical knowledge, which incorporates the theorem as a
% foundational limit.
constraint_indexing:constraint_classification(constraint_tarski_undefinability, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. The classification remains mountain, demonstrating
% the theorem's status as a fundamental, observer-independent limit.
constraint_indexing:constraint_classification(constraint_tarski_undefinability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constraint_tarski_undefinability_tests).

test(perspectival_agreement, [forall(member(P, [powerless, institutional, analytical]))]) :-
    % Verify perspectival agreement across all perspectives (Mountain).
    constraint_indexing:constraint_classification(constraint_tarski_undefinability, mountain, context(agent_power(P), _, _, _)).

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(constraint_tarski_undefinability, ExtMetricName, E),
    E =< 0.25. % Mountain requires low extractiveness.

test(natural_law_metrics) :-
    narrative_ontology:constraint_metric(constraint_tarski_undefinability, accessibility_collapse, ACC),
    narrative_ontology:constraint_metric(constraint_tarski_undefinability, resistance, RES),
    ACC >= 0.85,
    RES =< 0.15.

:- end_tests(constraint_tarski_undefinability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Tarski's undefinability theorem is a fundamental limitation rooted in the structure of formal systems. As such, it's classified as a mountain constraint. The scores reflect this inherent nature, with very low extractiveness (0.05) and suppression (0.01). It is not a social construct but a consequence of the expressive power and self-referential capacity of formal systems. The theater ratio is 0.0 as there is no performative aspect.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. The theorem's truth is independent of the observer's power, time horizon, exit options, or scope. All perspectives correctly classify it as a mountain, a fundamental and unchangeable feature of the logical landscape.
 *
 * DIRECTIONALITY LOGIC:
 *   As a mountain constraint, directionality is not applicable in the same way as for social constraints. It primarily affects formal systems themselves (powerless/trapped). Mathematicians and logicians (analytical/analytical) can understand and work around this limitation but cannot eliminate it. There are no intended beneficiaries or victims; it is simply a property of formal logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a mountain prevents mislabeling it as a snare or piton. It's not an actively enforced constraint imposing asymmetric extraction, nor is it a degraded institution. It is an inherent limitation, analogous to the laws of physics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_tarski_undefinability,
    'Could a fundamentally different, non-classical formal system (e.g., based on paraconsistent logic) circumvent this limitation in a meaningful way?',
    'Theoretical breakthroughs in logic, computability, and the foundations of mathematics.',
    'If True: The theorem''s scope would be reduced, making it a piton for those clinging to classical systems. If False: It remains a universal mountain.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(constraint_tarski_undefinability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required for low-extraction constraints (ε <= 0.46).
% For a mathematical theorem, metrics are static, showing no drift.
narrative_ontology:measurement(constraint_tarski_undefinability_tr_t0, constraint_tarski_undefinability, theater_ratio, 0, 0.0).
narrative_ontology:measurement(constraint_tarski_undefinability_tr_t5, constraint_tarski_undefinability, theater_ratio, 5, 0.0).
narrative_ontology:measurement(constraint_tarski_undefinability_tr_t10, constraint_tarski_undefinability, theater_ratio, 10, 0.0).

narrative_ontology:measurement(constraint_tarski_undefinability_ex_t0, constraint_tarski_undefinability, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(constraint_tarski_undefinability_ex_t5, constraint_tarski_undefinability, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(constraint_tarski_undefinability_ex_t10, constraint_tarski_undefinability, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Network relationships (structural influence edges)
% Tarski's theorem is closely related to Gödel's incompleteness theorems,
% as both reveal fundamental limitations of formal systems.
narrative_ontology:affects_constraint(constraint_tarski_undefinability, constraint_goedel_incompleteness).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a mountain constraint. The derivation chain is not used.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */