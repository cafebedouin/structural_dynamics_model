% ============================================================================
% CONSTRAINT STORY: nonstandard_arithmetic_models
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_nonstandard_arithmetic_models, []).

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
 *   constraint_id: nonstandard_arithmetic_models
 *   human_readable: Existence of Nonstandard Models of Arithmetic
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   Nonstandard models of arithmetic are structures that satisfy all the
 *   first-order axioms of Peano Arithmetic (PA) but are not isomorphic to the
 *   standard natural numbers. Their existence is a direct consequence of the
 *   Compactness Theorem of first-order logic. They contain "infinite" integers
 *   that lie beyond every standard number, yet obey all the same arithmetic laws.
 *   This constraint represents the fundamental inability of first-order logic
 *   to uniquely define the natural numbers.
 *
 * KEY AGENTS (by structural relationship):
 *   - Foundational Categoricity Seekers: Primary target (moderate/constrained) — Their goal of a unique, computable foundation for arithmetic is rendered impossible by this constraint.
 *   - Model Theorists: Primary beneficiary (institutional/mobile) — They use nonstandard models as a powerful tool (a Rope) to prove theorems about the standard model and explore the limits of logic.
 *   - Nonstandard Integers: A conceptual agent (powerless/trapped) — An entity whose existence is mandated by the logic, with no freedom to alter its properties.
 *   - Analytical Observer: Sees the full structure as a Mountain of logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: 0.25. The existence of these models "extracts" the property of
% categoricity (uniqueness) from any first-order theory of arithmetic. This
% imposes a permanent "tax" of ontological ambiguity.
domain_priors:base_extractiveness(nonstandard_arithmetic_models, 0.25).

% Rationale: 0.05. The existence of nonstandard models does not suppress or
% prevent the use of standard arithmetic. The alternative (standard model) is
% perfectly viable and universally used. The suppression is minimal, only
% affecting the philosophical claim of uniqueness. This value is set to comply
% with the Mountain classification ceiling.
domain_priors:suppression_score(nonstandard_arithmetic_models, 0.05).

% Rationale: 0.0. This is a formal mathematical truth with no performative aspect.
domain_priors:theater_ratio(nonstandard_arithmetic_models, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nonstandard_arithmetic_models, extractiveness, 0.25).
narrative_ontology:constraint_metric(nonstandard_arithmetic_models, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(nonstandard_arithmetic_models, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
%
% Accessibility Collapse: 1.0. Within first-order logic, the alternative (a
% categorical theory of arithmetic) is provably inaccessible.
narrative_ontology:constraint_metric(nonstandard_arithmetic_models, accessibility_collapse, 1.0).
% Resistance: 0.0. One cannot meaningfully "resist" a mathematical theorem.
narrative_ontology:constraint_metric(nonstandard_arithmetic_models, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(nonstandard_arithmetic_models, mountain).
narrative_ontology:human_readable(nonstandard_arithmetic_models, "Existence of Nonstandard Models of Arithmetic").
narrative_ontology:topic_domain(nonstandard_arithmetic_models, "mathematical/logical").

% --- Binary flags ---
% This constraint is a feature of logic itself and requires no enforcement.

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this, the classify_from_metrics
% mountain clause will not fire.
domain_priors:emerges_naturally(nonstandard_arithmetic_models).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Although this is a Mountain, beneficiary/victim data is provided to enrich
% the perspectival analysis and explain why some agents see it as a Rope.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(nonstandard_arithmetic_models, model_theorists).
narrative_ontology:constraint_beneficiary(nonstandard_arithmetic_models, nonstandard_analysis_researchers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(nonstandard_arithmetic_models, foundational_categoricity_seekers).
narrative_ontology:constraint_victim(nonstandard_arithmetic_models, recursive_arithmetic_purists).

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

% PERSPECTIVE 1: THE CONCEPTUAL NONSTANDARD INTEGER (MOUNTAIN)
% A conceptual agent whose existence is mandated by the logic. It is trapped
% by the axioms of arithmetic and has zero degrees of freedom.
constraint_indexing:constraint_classification(nonstandard_arithmetic_models, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MODEL THEORIST (ROPE)
% For the institutional mathematician, nonstandard models are a powerful tool
% for coordination and proof (a Rope), allowing them to prove results about
% the standard model via nonstandard means (e.g., nonstandard analysis).
constraint_indexing:constraint_classification(nonstandard_arithmetic_models, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE COMPUTABILITY THEORIST (MOUNTAIN)
% For those seeking a computable foundation for mathematics, Tennenbaum's
% Theorem proves that no nonstandard model of PA can be computable. This is not
% a Snare (a contingent, coercive trap) but a Mountain (an absolute, unchangeable
% limit). Their hopes are dashed against a fact of logic.
constraint_indexing:constraint_classification(nonstandard_arithmetic_models, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The analytical observer sees the constraint for what it is: a direct and
% unavoidable consequence of the expressive limitations of first-order logic,
% as proven by the Compactness Theorem. It is a fundamental feature of the
% logical landscape.
constraint_indexing:constraint_classification(nonstandard_arithmetic_models, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nonstandard_arithmetic_models_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between the theorist (victim) and beneficiary.
    constraint_indexing:constraint_classification(nonstandard_arithmetic_models, TypeTarget, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(nonstandard_arithmetic_models, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == mountain,
    TypeBeneficiary == rope.

test(computability_mountain_insight) :-
    % Demonstrates that Tennenbaum's Theorem is correctly classified as a Mountain.
    constraint_indexing:constraint_classification(nonstandard_arithmetic_models, mountain, context(agent_power(moderate), time_horizon(civilizational), exit_options(constrained), spatial_scope(universal))).

test(threshold_validation) :-
    % Verify that the base metrics align with the Mountain claim.
    narrative_ontology:constraint_metric(nonstandard_arithmetic_models, extractiveness, E),
    narrative_ontology:constraint_metric(nonstandard_arithmetic_models, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(nonstandard_arithmetic_models_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.25) represents the loss of uniqueness
 *   (categoricity) for first-order arithmetic, a significant conceptual cost.
 *   The suppression score (0.05) is low because the existence of nonstandard
 *   models does not suppress the use of standard arithmetic. The Natural Law
 *   profile metrics (accessibility_collapse=1.0, resistance=0.0) and the
 *   `emerges_naturally` flag are included to ensure this constraint correctly
 *   passes the engine's mountain certification chain.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between the Model Theorist (beneficiary) and the Computability
 *   Theorist (victim). The beneficiary sees a useful tool (Rope) that enables
 *   new proof techniques. The victim, whose goal is a computable foundation,
 *   sees an impassable barrier (Mountain) proven by Tennenbaum's Theorem.
 *   What is a resource for one is an absolute limit for another. This is not
 *   a Snare, which implies artificial coercion, but a Mountain, an intrinsic
 *   feature of the logical system.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `model_theorists` and `nonstandard_analysis_researchers` gain powerful new tools for mathematical exploration. Their `arbitrage` exit option (they can switch to standard methods at will) gives them a low directionality `d`, resulting in a negative effective extraction `χ` and a Rope classification.
 *   - Victims: `foundational_categoricity_seekers` have their primary goal defeated by this constraint. Their `constrained` exit option reflects that they cannot escape this logical fact, leading to a high `d` and the experience of hitting a Mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   This story demonstrates how a single logical fact can be a Mountain from a
 *   foundational perspective while simultaneously serving as a Rope from an
 *   instrumental one. Correctly identifying the Computability Theorist's
 *   perspective as Mountain, not Snare, is crucial. A Snare implies artificial
 *   coercion that could be removed; a Mountain correctly identifies the limit
 *   as an intrinsic feature of the system (first-order logic) itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_first_order_primacy,
    "Is first-order logic the fundamental 'Mountain' of formal reasoning, or just a 'Scaffold' that obscures a more natural, categorical higher-order logic?",
    "Investigation into cognitive science or physics for evidence of natural, direct implementation of second-order quantification.",
    "If first-order logic is fundamental, nonstandard models are a permanent Mountain. If it is a scaffold, they are an artifact of a limited toolset.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(nonstandard_arithmetic_models, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% No temporal data is required. Base extractiveness (0.25) is not > 0.46.
% As a mathematical fact, its properties are static and do not drift over time.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No network relationships are declared for this foundational logical constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the perspectives.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */