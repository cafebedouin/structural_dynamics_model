% ============================================================================
% CONSTRAINT STORY: prime_number_theorem
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_prime_number_theorem, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: prime_number_theorem
 *   human_readable: Prime Number Theorem (Asymptotic Density)
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Prime Number Theorem (PNT) describes the asymptotic distribution of prime
 *   numbers among the positive integers. It formalizes the intuitive idea that
 *   primes become less frequent as they become larger, stating that the number
 *   of primes less than x is approximately x/ln(x). This is a fundamental,
 *   unchangeable feature of the integers.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Search Algorithm (powerless/trapped): A computational process searching
 *     for large primes, subject to the "thinning" reality of PNT.
 *   - The Cryptographer (institutional/mobile): An agent who uses the predictable
 *     distribution of primes to coordinate global security standards (e.g., RSA).
 *   - The Number Theorist (analytical/analytical): An observer studying the
 *     structure of the integers.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The theorem itself extracts nothing. The 0.08 value represents the
% irreducible complexity cost of verifying or applying the theorem, a cost
% inherent in the logical structure it describes.
domain_priors:base_extractiveness(prime_number_theorem, 0.08).

% Rationale: The theorem renders alternative prime distributions (e.g., linear
% density) logically impossible. This is maximum suppression of alternatives,
% but it is logical, not coercive. The score is low because it doesn't suppress
% other valid mathematical truths.
domain_priors:suppression_score(prime_number_theorem, 0.02).

% Rationale: A formal mathematical truth has no performative component.
domain_priors:theater_ratio(prime_number_theorem, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prime_number_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(prime_number_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(prime_number_theorem, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(prime_number_theorem, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(prime_number_theorem, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(prime_number_theorem, mountain).
narrative_ontology:human_readable(prime_number_theorem, "Prime Number Theorem (Asymptotic Density)").
narrative_ontology:topic_domain(prime_number_theorem, "mathematical").

% --- Emergence flag (required for mountain constraints) ---
% The theorem emerges naturally from the structure of the integers without
% human design or enforcement.
domain_priors:emerges_naturally(prime_number_theorem).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (Natural Law), the PNT has no inherent
% beneficiaries or victims. The costs and benefits arise from how agents choose
% to interact with this fixed feature of reality, not from the constraint itself.

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

% UNIFORM-TYPE CONSTRAINT: As a mathematical law, the PNT is a Mountain from
% all perspectives. The classification is invariant. The following perspectives
% demonstrate this invariance.

% PERSPECTIVE 1: THE SEARCH ALGORITHM
% For a computational process searching for primes, the theorem is an
% unchangeable law of the terrain it explores.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE CRYPTOGRAPHER
% For an institution designing security standards, the theorem is a fixed,
% reliable feature of reality that can be built upon.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The default analytical context, which correctly identifies the theorem as a
% fundamental, unchangeable constraint.
constraint_indexing:constraint_classification(prime_number_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prime_number_theorem_tests).

test(classification_invariance) :-
    % Verify that the classification is Mountain from different perspectives.
    constraint_indexing:constraint_classification(prime_number_theorem, Type1,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prime_number_theorem, Type2,
        context(agent_power(institutional), _, _, _)),
    Type1 == mountain,
    Type2 == mountain.

test(threshold_validation) :-
    % Verify metrics are within Mountain thresholds.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(prime_number_theorem, ExtMetricName, E),
    narrative_ontology:constraint_metric(prime_number_theorem, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

test(natural_law_profile_present) :-
    % Verify the required NL profile metrics are declared.
    narrative_ontology:constraint_metric(prime_number_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(prime_number_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(prime_number_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Prime Number Theorem is a canonical example of a Mountain constraint.
 *   Its base extractiveness (0.08) and suppression (0.02) are very low,
 *   reflecting its status as a fundamental, non-coercive mathematical truth.
 *   It emerges naturally from the structure of integers and faces no resistance,
 *   as resistance would be incoherent. The accessibility_collapse is 1.0 because
 *   no alternative prime distributions are logically possible.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a uniform-type Mountain, the classification
 *   is invariant across all indices. While a cryptographer *uses* the theorem
 *   as a coordination tool (a Rope-like application) and a search algorithm
 *   *experiences* its effects as a resource cost (a Snare-like interaction),
 *   the underlying constraint itself remains a Mountain. To model the
 *   application, one would write a separate story (e.g., for RSA standards)
 *   that is affected by this one.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations are omitted because a natural law does
 *   not have inherent directionality. It is a feature of reality. The costs and
 *   benefits are determined by agents' choices in how they interact with it,
 *   not by the law itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a pure Mountain is robust. The extremely low
 *   extractiveness and suppression scores prevent any misclassification as a
 *   Snare or Tangled Rope, correctly identifying it as a feature of the
 *   environment rather than a system of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_pnt_riemann,
    'How does the error term in the PNT (related to the Riemann Hypothesis) affect the fine-grained structure of this Mountain?',
    'Proof or disproof of the Riemann Hypothesis, which would precisely bound the error term Li(x) - pi(x).',
    'If RH is true, the Mountain''s terrain is smooth and predictable. If false, the terrain has chaotic, large-scale fluctuations.',
    confidence_without_resolution(high)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_pnt_riemann, empirical, 'The precise error term of the PNT is contingent on the unresolved Riemann Hypothesis.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(prime_number_theorem, 1896, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness (0.08) is below the 0.46 threshold for
% mandatory lifecycle drift tracking. As a mathematical theorem, its properties
% are time-invariant.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No network relationships declared. As a foundational theorem, it could be
% said to affect many other constraints in cryptography and number theory,
% but these are not yet modeled in the corpus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The constraint is a uniform-type Mountain.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */