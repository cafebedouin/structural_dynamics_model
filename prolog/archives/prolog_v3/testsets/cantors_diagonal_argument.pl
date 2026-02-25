% ============================================================================
% CONSTRAINT STORY: cantors_diagonal_argument
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_cantors_diagonal_argument, []).

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
 *   constraint_id: cantors_diagonal_argument
 *   human_readable: Cantor's Diagonal Argument
 *   domain: technological
 *
 * SUMMARY:
 *   Cantor's Diagonal Argument demonstrates that the set of real numbers is "uncountable," meaning it cannot be put into a one-to-one correspondence with the set of natural numbers. This result has profound implications for computation and the limits of formal systems, akin to Gödel's incompleteness theorems. It acts as a fundamental constraint on what can be computed or enumerated in a finite system.
 *
 * KEY AGENTS (by structural relationship):
 *   - Formal Systems: Primary target (powerless/trapped) — limited by the argument's conclusion.
 *   - Mathematicians: Primary beneficiary (analytical/analytical) — gain understanding of fundamental limits.
 *   - Computer Scientists: Secondary actor (analytical/analytical) — understand limitations on computation.
 *   - Philosophical Logicians: Analytical observer (analytical/analytical) — sees full structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cantors_diagonal_argument, 0.10).
domain_priors:suppression_score(cantors_diagonal_argument, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(cantors_diagonal_argument, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cantors_diagonal_argument, extractiveness, 0.10).
narrative_ontology:constraint_metric(cantors_diagonal_argument, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(cantors_diagonal_argument, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(cantors_diagonal_argument, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(cantors_diagonal_argument, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(cantors_diagonal_argument, mountain).
narrative_ontology:human_readable(cantors_diagonal_argument, "Cantor's Diagonal Argument").
narrative_ontology:topic_domain(cantors_diagonal_argument, "technological").

% --- Binary flags ---
% This is a mathematical proof; no enforcement or sunset clauses apply.

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this, the classify_from_metrics
% mountain clause will not fire.
domain_priors:emerges_naturally(cantors_diagonal_argument).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Not strictly required for mountain constraints, but included for narrative
% clarity to explain the structural roles.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(cantors_diagonal_argument, mathematicians).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(cantors_diagonal_argument, formal_systems).

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

% UNIFORM-TYPE CONSTRAINT: This is a natural law (mountain-only), so the
% classification is invariant across all perspectives. We include multiple
% perspectives to demonstrate this invariance.

% PERSPECTIVE 1: THE PRIMARY TARGET (FORMAL SYSTEMS)
% The abstract concept of a formal system is "trapped" by this logical limit.
constraint_indexing:constraint_classification(cantors_diagonal_argument, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (MATHEMATICIANS)
% The community of mathematicians who use this result.
constraint_indexing:constraint_classification(cantors_diagonal_argument, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The default analytical context, which confirms the mountain classification.
constraint_indexing:constraint_classification(cantors_diagonal_argument, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cantors_diagonal_argument_tests).

test(classification_is_uniform) :-
    % For a mountain, all perspectives should yield the same classification.
    constraint_indexing:constraint_classification(cantors_diagonal_argument, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cantors_diagonal_argument, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == mountain,
    TypeBeneficiary == mountain.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cantors_diagonal_argument, ExtMetricName, E),
    E =< 0.25. % Mountain threshold.

test(natural_law_profile) :-
    narrative_ontology:constraint_metric(cantors_diagonal_argument, accessibility_collapse, AC),
    AC >= 0.85,
    narrative_ontology:constraint_metric(cantors_diagonal_argument, resistance, R),
    R =< 0.15,
    domain_priors:emerges_naturally(cantors_diagonal_argument).

:- end_tests(cantors_diagonal_argument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Cantor's Diagonal Argument is a canonical Mountain. It represents a fundamental, unchangeable limitation in formal systems and set theory. Its base extractiveness is low (0.10) as it does not actively extract value, but rather reveals an inherent structural property of infinite sets. The suppression score (0.05) is minimal; it doesn't suppress alternatives but proves certain mappings are impossible. The accessibility collapse is very high (0.95) as the proof is logically airtight, foreclosing any attempt to enumerate the reals. Resistance is negligible (0.01), as the proof is universally accepted within mathematics.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a mathematical truth, its status as a Mountain is recognized from all coherent perspectives, from the abstract "formal systems" it constrains to the mathematicians who use it and the observers who analyze it.
 *
 * DIRECTIONALITY LOGIC:
 *   While not strictly necessary for a Mountain, defining beneficiaries and victims clarifies the structural roles. "Formal systems" are the abstract victim, as their capabilities are bounded by the argument's conclusion. "Mathematicians" are the beneficiaries, as the knowledge of this limit is a foundational tool for research in logic, computer science, and analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain prevents mislabeling this fundamental limitation as a Snare or Piton. It is not a human-imposed rule that extracts value or is maintained through inertia; it is a property of the logical universe. The low extractiveness and suppression scores, combined with the `emerges_naturally` flag and high NL profile metrics, secure this classification against misinterpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cantor_1,
    'Could alternative, non-classical logics or set theories (e.g., intuitionistic) alter the implications of the diagonal argument?',
    'Further research into non-standard foundations of mathematics.',
    'If True: The "uncountability" constraint might be reframed as specific to classical systems. If False: The constraint is fundamental across all known logical frameworks.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cantors_diagonal_argument, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. As a mathematical proof, its properties are static.
% Base extractiveness is low (< 0.46), so temporal data is not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a proof, not a coordination mechanism.
% narrative_ontology:coordination_type(cantors_diagonal_argument, information_standard).

% Network relationships (structural influence edges)
% The diagonal method is a precursor to and influences Gödel's work.
narrative_ontology:affects_constraint(cantors_diagonal_argument, godel_incompleteness).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. The standard derivation chain is sufficient for this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */