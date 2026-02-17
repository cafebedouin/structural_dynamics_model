% ============================================================================
% CONSTRAINT STORY: central_limit_theorem
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_central_limit_theorem, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: central_limit_theorem
 *   human_readable: Central Limit Theorem (CLT)
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Central Limit Theorem (CLT) establishes that, under certain
 *   conditions (e.g., finite variance), the sum or average of a large number
 *   of independent random variables will be approximately normally distributed,
 *   regardless of the underlying distribution. It is a foundational principle
 *   in probability theory and statistics, acting as a "gravitational force"
 *   that pulls aggregates toward a Gaussian form. This story models the
 *   theorem itself as a pure mathematical constraint (a Mountain).
 *
 * KEY AGENTS (by structural relationship):
 *   - The Individual Variable (Subject): A powerless entity whose unique
 *     distributional identity is subsumed into the aggregate.
 *   - The Statistician (Observer): An institutional or analytical agent who
 *     uses the theorem for inference, modeling, and verification.
 *   - The Risk Manager (Mis-applier): An agent in a separate, linked
 *     constraint who incorrectly applies this Mountain to a domain where its
 *     assumptions are violated (e.g., finance with fat-tailed risks),
 *     experiencing a Snare.
 *
 * DUAL FORMULATION NOTE:
 * This constraint is one of 2 stories decomposed from the colloquial use of "CLT".
 * Decomposed because ε differs across observables (ε-invariance principle).
 * Related stories:
 *   - central_limit_theorem (ε=0.08, Mountain) - The mathematical theorem itself.
 *   - clt_misapplication_in_finance (ε=0.65, Snare) - The institutional policy of
 *     enforcing CLT-based models (e.g., VaR) in domains with heavy-tailed risks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Low extraction. The CLT "extracts" the identity of individual
% distributions to form a smooth aggregate, but this is a descriptive act of
% mathematics, not a coercive extraction of value.
domain_priors:base_extractiveness(central_limit_theorem, 0.08).

% Rationale: Very low suppression. As a mathematical theorem, it does not
% suppress alternatives; it describes a reality given certain axioms.
% This value is set to be compliant with the Mountain classification threshold (<= 0.05).
domain_priors:suppression_score(central_limit_theorem, 0.05).

% Rationale: Zero theater. The theorem is a pure, functional mathematical truth.
domain_priors:theater_ratio(central_limit_theorem, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(central_limit_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(central_limit_theorem, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(central_limit_theorem, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
% Accessibility Collapse: Alternatives are logically incoherent. Value of 1.0.
narrative_ontology:constraint_metric(central_limit_theorem, accessibility_collapse, 1.0).
% Resistance: Resistance to a mathematical theorem is incoherent. Value of 0.0.
narrative_ontology:constraint_metric(central_limit_theorem, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(central_limit_theorem, mountain).
narrative_ontology:human_readable(central_limit_theorem, "Central Limit Theorem (CLT)").

% --- Emergence flag (required for mountain constraints) ---
% The CLT emerges from the axioms of probability theory without human design.
domain_priors:emerges_naturally(central_limit_theorem).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (natural law), the CLT does not have
% structural beneficiaries or victims in the sense of receiving or bearing
% asymmetric extraction. Its effects are universal for all agents observing
% a system that meets its criteria.

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

% PERSPECTIVE 1: THE RANDOM VARIABLE (MOUNTAIN)
% For an individual data point being summed, the convergence to a normal
% distribution is an inescapable mathematical law. It has no agency or exit.
constraint_indexing:constraint_classification(central_limit_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE STATISTICIAN (MOUNTAIN)
% For an institutional actor like a polling firm or scientific body, the CLT
% is an unchangeable feature of the world they are modeling. It is a fixed
% point of reference, a natural law of aggregation.
constraint_indexing:constraint_classification(central_limit_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From a global, civilizational perspective, the theorem is a fundamental,
% fixed constraint on the behavior of aggregated random systems.
constraint_indexing:constraint_classification(central_limit_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(central_limit_theorem_tests).

test(perspectival_invariance) :-
    % Verify that the classification is Mountain from all key perspectives.
    constraint_indexing:constraint_classification(central_limit_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(central_limit_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == mountain,
    TypeBeneficiary == mountain.

test(threshold_validation_mountain) :-
    % Verify the metrics adhere to the Mountain classification thresholds.
    narrative_ontology:constraint_metric(central_limit_theorem, extractiveness, E),
    narrative_ontology:constraint_metric(central_limit_theorem, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    % Verify the Natural Law profile metrics are present and valid.
    narrative_ontology:constraint_metric(central_limit_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(central_limit_theorem, resistance, R),
    domain_priors:emerges_naturally(central_limit_theorem),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(central_limit_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This story models the Central Limit Theorem itself, not its applications
 *   or misapplications. As a mathematical truth, it has very low base
 *   extractiveness (0.08) and suppression (0.05), qualifying it as a Mountain.
 *   The suppression score is 0.05 to comply with the Mountain threshold,
 *   reflecting that the theorem describes reality rather than coercively
 *   suppressing alternatives. The full Natural Law profile (accessibility_collapse=1.0,
 *   resistance=0.0, emerges_naturally=true) is declared to ensure it passes
 *   the engine's certification chain for Mountain classification.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a Mountain-type constraint (natural law),
 *   the classification is invariant across all observer indices. All agents,
 *   regardless of power or exit options, perceive it as a fixed feature of
 *   their environment.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, the CLT has no structural beneficiaries or victims.
 *   Therefore, `constraint_beneficiary/2` and `constraint_victim/2` are not
 *   declared. The directionality `d` is derived from canonical power atom
 *   fallbacks, but since ε is so low, the resulting effective extraction χ
 *   is negligible from all perspectives, leading to a uniform Mountain
 *   classification.
 *
 * MANDATROPHY ANALYSIS (ε-Invariance Principle):
 *   The original story conflated the theorem (a Mountain) with its
 *   misapplication in finance (a Snare). This violates the ε-invariance
 *   principle, as the base extractiveness of "using VaR models based on CLT"
 *   is vastly different from the ε of the theorem itself. This revised file
 *   correctly models only the theorem. The "Snare" of misapplication is
 *   modeled as a separate constraint, `clt_misapplication_in_finance`, which
 *   is causally affected by this one. This decomposition prevents the
 *   mislabeling of a natural law as a coercive instrument.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_clt,
    'Is the CLT a purely abstract mathematical construct or a fundamental organizing principle of the physical universe (a "law of physics" for complex systems)?',
    'Empirical verification in novel physical domains (e.g., quantum chaos, cosmological structures) where aggregation occurs.',
    'If physical law, it is an unassailable Mountain. If purely abstract, it is a powerful Rope for modeling, but could be superseded by a more fundamental theory.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(central_limit_theorem, 1810, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required for low-extraction constraints (base_extractiveness <= 0.46).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The CLT acts as a standard for understanding and modeling aggregate data.
narrative_ontology:coordination_type(central_limit_theorem, information_standard).

% Network relationships (structural influence edges)
% The mathematical truth of the CLT enables the creation of a separate,
% highly extractive social constraint: the mandatory use of CLT-based models
% in domains where they are inappropriate.
narrative_ontology:affects_constraint(central_limit_theorem, clt_misapplication_in_finance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for a uniform-type Mountain constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */