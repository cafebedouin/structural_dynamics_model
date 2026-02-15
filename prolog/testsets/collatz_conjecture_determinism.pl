% ============================================================================
% CONSTRAINT STORY: collatz_conjecture_determinism
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-08
% ============================================================================

:- module(constraint_collatz_conjecture_determinism, []).

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
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: collatz_conjecture_determinism
 *   human_readable: The Collatz Conjecture (3n + 1) Determinism
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   The Collatz conjecture posits that an iterative sequence (if n is even,
 *   n/2; if n is odd, 3n+1) will always eventually reach 1 for any positive
 *   integer starting value. This represents a fundamental, unchangeable
 *   constraint on the behavior of integers under these simple arithmetic
 *   rules. It is a canonical example of a Mountain: a fixed, immutable
 *   feature of a logical system that appears to be true, has resisted all
 *   attempts at falsification, and whose properties are invariant from all
 *   perspectives.
 *
 * KEY AGENTS (by structural relationship):
 *   - Natural Numbers (e.g., '27'): The subject of the constraint (powerless/trapped) — its path is absolutely dictated by the rules.
 *   - Computational Researcher: An observer using the conjecture to test computing limits (institutional/mobile).
 *   - Number Theorist: An analytical observer attempting to prove or disprove the conjecture (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: ε is near-zero. The conjecture does not extract resources or
% freedom in a structural sense. The "extraction" of cognitive labor from
% mathematicians is a metaphorical, not structural, cost.
domain_priors:base_extractiveness(collatz_conjecture_determinism, 0.02).

% Rationale: Suppression is near-zero. The rules do not suppress alternatives;
% they are a defined property of the system being studied.
domain_priors:suppression_score(collatz_conjecture_determinism, 0.01).

% Rationale: Theater is zero. The constraint is a pure mathematical statement
% with no performative aspect.
domain_priors:theater_ratio(collatz_conjecture_determinism, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collatz_conjecture_determinism, extractiveness, 0.02).
narrative_ontology:constraint_metric(collatz_conjecture_determinism, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(collatz_conjecture_determinism, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
%
% Rationale: Accessibility Collapse is 1.0 because within the rules of
% arithmetic, no alternative path is conceivable for a given number.
narrative_ontology:constraint_metric(collatz_conjecture_determinism, accessibility_collapse, 1.0).
% Rationale: Resistance is 0.0. One cannot "resist" a mathematical function;
% one can only compute its result.
narrative_ontology:constraint_metric(collatz_conjecture_determinism, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(collatz_conjecture_determinism, mountain).

% --- Binary flags ---
% No sunset clause, no active enforcement.

% --- Emergence flag (required for mountain constraints) ---
% The constraint emerges from the axioms of integer arithmetic without human design.
domain_priors:emerges_naturally(collatz_conjecture_determinism).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain constraint representing a mathematical
% property, there are no structural beneficiaries or victims.

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

% PERSPECTIVE 1: THE NATURAL NUMBER (e.g., '27')
% For any number, the rules are an absolute, unchangeable law of its existence.
% Its path is fixed. This is a Mountain.
constraint_indexing:constraint_classification(collatz_conjecture_determinism, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE COMPUTATIONAL RESEARCHER
% For an institution using the problem to benchmark hardware, the rules are
% still a fixed, external reality they must compute against. This is a Mountain.
constraint_indexing:constraint_classification(collatz_conjecture_determinism, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE NUMBER THEORIST
% For the analytical observer trying to formulate a proof, the conjecture's
% apparent truth is a fixed feature of the mathematical landscape they are
% trying to map. This is a Mountain.
constraint_indexing:constraint_classification(collatz_conjecture_determinism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collatz_conjecture_determinism_tests).

test(invariance_across_perspectives) :-
    % Verify that the classification is Mountain from all key perspectives.
    constraint_indexing:constraint_classification(collatz_conjecture_determinism, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collatz_conjecture_determinism, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(collatz_conjecture_determinism, Type3, context(agent_power(analytical), _, _, _)),
    Type1 == mountain,
    Type1 == Type2,
    Type2 == Type3.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(collatz_conjecture_determinism, ExtMetricName, E),
    narrative_ontology:constraint_metric(collatz_conjecture_determinism, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_present) :-
    domain_priors:emerges_naturally(collatz_conjecture_determinism),
    narrative_ontology:constraint_metric(collatz_conjecture_determinism, accessibility_collapse, AC), AC >= 0.85,
    narrative_ontology:constraint_metric(collatz_conjecture_determinism, resistance, R), R =< 0.15.

:- end_tests(collatz_conjecture_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Collatz conjecture is a statement about the fundamental properties of
 *   numbers. As such, it is a canonical Mountain. The base extractiveness (ε)
 *   and suppression scores are set near-zero (0.02, 0.01) because the
 *   constraint does not structurally extract resources or foreclose options
 *   in the way a social or economic constraint does. The immense cognitive
 *   effort expended by mathematicians is a consequence of its complexity, not
 *   an extraction in the framework's sense. The classification is uniformly
 *   Mountain across all perspectives because its properties are invariant; a
 *   number, a researcher, and a theorist all confront the same immutable rules.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is a uniform-type constraint (Mountain-only),
 *   a characteristic of natural laws and logical tautologies. The rules of the
 *   sequence are the same for everyone and everything.
 *
 * DIRECTIONALITY LOGIC:
 *   Not applicable. As a Mountain constraint, there are no structural
 *   beneficiaries or victims, so directionality (d) is not a factor in its
 *   classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This story clarifies the definition of a Mountain. By refusing to
 *   metaphorically interpret "effort" as "extraction," it maintains the
 *   structural integrity of the ε metric. This prevents misclassifying a
 *   difficult-to-understand natural law as a Snare or Tangled Rope, which
 *   are categories reserved for constraints with designed, asymmetric social
 *   or economic impacts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_collatz_undecidability,
    'Is the Collatz conjecture unprovable within standard axiomatic systems like ZFC?',
    'Formal investigation into whether the conjecture is independent of ZFC, similar to the Continuum Hypothesis.',
    'If True (undecidable), the Mountain is a fundamental feature of our chosen logic. If False (provable), the Mountain is simply a very tall, but finite, peak of complexity.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_collatz_undecidability, conceptual, 'Is the conjecture unprovable within standard axiomatic systems like ZFC?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(collatz_conjecture_determinism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for low-extraction constraints (base_extractiveness <= 0.46).
% The properties of this mathematical constraint are time-invariant.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for a fundamental mathematical constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a Mountain constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */