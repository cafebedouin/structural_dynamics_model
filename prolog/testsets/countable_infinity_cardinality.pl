% ============================================================================
% CONSTRAINT STORY: countable_infinity_cardinality
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_countable_infinity_cardinality, []).

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
 *   constraint_id: countable_infinity_cardinality
 *   human_readable: Cardinality of Countably Infinite Sets (Aleph-0)
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   The principle that any set which can be put into a one-to-one
 *   correspondence with the natural numbers has the same cardinality,
 *   aleph-naught (ℵ₀). This establishes a fundamental, unchangeable
 *   property of sets like the integers (ℤ) and rational numbers (ℚ),
 *   proving they are "no larger" than the natural numbers (ℕ). This is a
 *   foundational law of set theory, acting as a logical mountain.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Logician (Analytical): An observer who formalizes and applies the principle.
 *   - The Computer Scientist (Moderate/Mobile): An agent who uses countability as a basis for algorithms (e.g., loops, array indexing).
 *   - The Intuitionist (Powerless/Trapped): A philosophical stance that might struggle with the counter-intuitive result that the seemingly "dense" rational numbers are countable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Low extraction. The principle doesn't "take" anything; it reveals
% a pre-existing logical structure. The small value represents the cognitive
% cost of overcoming the intuition that Q is larger than N.
domain_priors:base_extractiveness(countable_infinity_cardinality, 0.05).
% Rationale: Very low suppression. Alternatives are not suppressed by force,
% but are rendered logically incoherent by formal proof.
domain_priors:suppression_score(countable_infinity_cardinality, 0.01).
% Rationale: Zero theater. This is a formal mathematical truth with no
% performative aspect.
domain_priors:theater_ratio(countable_infinity_cardinality, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(countable_infinity_cardinality, extractiveness, 0.05).
narrative_ontology:constraint_metric(countable_infinity_cardinality, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(countable_infinity_cardinality, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
%
% Rationale: Alternatives are logically impossible within ZFC set theory.
narrative_ontology:constraint_metric(countable_infinity_cardinality, accessibility_collapse, 1.0).
% Rationale: Meaningful resistance is non-existent, aside from philosophical
% schools (like ultrafinitism) that reject the premise of actual infinity.
narrative_ontology:constraint_metric(countable_infinity_cardinality, resistance, 0.01).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(countable_infinity_cardinality, mountain).
narrative_ontology:human_readable(countable_infinity_cardinality, "Cardinality of Countably Infinite Sets (Aleph-0)").

% --- Binary flags ---
% (none apply)

% --- Emergence flag (required for mountain constraints) ---
% This constraint is a discovered property of mathematical structures, not a
% human invention or enforced rule.
domain_priors:emerges_naturally(countable_infinity_cardinality).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (natural law), this constraint does not
% have beneficiaries or victims in a structural sense. The effects are
% uniform and derived from logical necessity.

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
% invariant across all perspectives because it is a law of logic.

% PERSPECTIVE 1: THE INTUITIONIST (MOUNTAIN)
% A philosophical stance confronted with a counter-intuitive but undeniable truth.
constraint_indexing:constraint_classification(countable_infinity_cardinality, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE COMPUTER SCIENTIST (MOUNTAIN)
% An agent who relies on this principle for the logical foundation of computation.
constraint_indexing:constraint_classification(countable_infinity_cardinality, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The default analytical context, which recognizes this as a fundamental law.
constraint_indexing:constraint_classification(countable_infinity_cardinality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(countable_infinity_cardinality_tests).

test(classification_invariance) :-
    % Verify that the classification is Mountain from all perspectives.
    constraint_indexing:constraint_classification(countable_infinity_cardinality, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(countable_infinity_cardinality, mountain, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(countable_infinity_cardinality, mountain, context(agent_power(analytical), _, _, _)).

test(natural_law_metrics) :-
    % Verify the metrics are consistent with a Mountain classification.
    narrative_ontology:constraint_metric(countable_infinity_cardinality, extractiveness, E),
    narrative_ontology:constraint_metric(countable_infinity_cardinality, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_present) :-
    % Verify the full NL profile is declared.
    domain_priors:emerges_naturally(countable_infinity_cardinality),
    narrative_ontology:constraint_metric(countable_infinity_cardinality, accessibility_collapse, AC), AC >= 0.85,
    narrative_ontology:constraint_metric(countable_infinity_cardinality, resistance, R), R =< 0.15.

:- end_tests(countable_infinity_cardinality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint represents a formal mathematical truth discovered by Cantor.
 *   Its properties are not socially constructed or enforced, but are logical
 *   consequences of set theory axioms. Therefore, it is a classic Mountain.
 *   The base extractiveness (0.05) and suppression (0.01) are set to near-zero
 *   to reflect this. They are not truly zero only to account for the cognitive
 *   effort required to accept a deeply counter-intuitive result. The full
 *   Natural Law profile (emerges_naturally, accessibility_collapse=1.0,
 *   resistance=0.01) is provided to ensure it passes the NL certification chain.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a mathematical law, its classification is
 *   Mountain from all possible indices. The narrative descriptions of how a
 *   computer scientist "uses" it or an intuitionist "feels" about it do not
 *   change its fundamental, unchangeable structure. The framework models this
 *   structural reality, not subjective experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable. Beneficiary and victim declarations were
 *   omitted because they are a category error for a natural law. The law does
 *   not "target" or "benefit" anyone; it simply is.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Mountain correctly identifies it as a feature of
 *   the logical landscape, not a tool of power. Any attempt to frame it as a
 *   Snare (e.g., "it traps the rationals") would be a category error,
 *   conflating a logical property with social coercion. The framework's strict
 *   metric requirements for Mountains prevent this misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_countable_infinity_cardinality,
    'Does the Continuum Hypothesis (CH) hold? (Is there a set with cardinality strictly between that of the integers and the real numbers?)',
    'Proven to be independent of ZFC axioms (by Gödel and Cohen). Resolution requires adopting new axioms beyond standard set theory.',
    'If CH is false, it implies a more complex structure of infinities, making Aleph-0 just the first step in a potentially dense hierarchy. If true, the structure is simpler.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_countable_infinity_cardinality, conceptual, 'The truth value of the Continuum Hypothesis is independent of ZFC axioms.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(countable_infinity_cardinality, 1874, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. Base extractiveness (0.05) is below the 0.46 threshold
% for required temporal tracking. As a mathematical law, its properties do not drift.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% --- Network Decomposition (Constraint Families) ---
% When a natural-language label covers multiple constraints with different ε
% values, each gets its own file. Link family members with affects_constraint:
%
% DUAL FORMULATION NOTE:
% This constraint is one of 2 stories decomposed from "Cantorian infinities".
% Decomposed because they represent distinct logical claims.
% Related stories:
%   - uncountable_infinity_cardinality (ε=0.05, Mountain)
%
narrative_ontology:affects_constraint(countable_infinity_cardinality, uncountable_infinity_cardinality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a Mountain constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */