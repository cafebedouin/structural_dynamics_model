% ============================================================================
% CONSTRAINT STORY: weierstrass_proof_limits
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_weierstrass_proof_limits, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: weierstrass_proof_limits
 *   human_readable: The Existence of Continuous, Nowhere-Differentiable Functions
 *   domain: technological
 *
 * SUMMARY:
 *   The Weierstrass function is the canonical example of a function that is
 *   continuous everywhere but differentiable nowhere. Its existence establishes
 *   a fundamental limit on mathematical methods that assume smoothness or
 *   differentiability. This constraint is not a human-imposed rule but an
 *   inherent property of the mathematical landscape, impacting fields from
 *   signal processing to chaos theory by demonstrating that intuitive notions
 *   of "smoothness" do not always hold.
 *
 * KEY AGENTS (by structural relationship):
 *   - Novice Practitioners: (powerless/trapped) — Encounter the function as an
 *     absolute barrier to standard calculus techniques.
 *   - Applied Scientists/Engineers: (moderate/constrained) — Must account for
 *     such pathological cases in models, limiting the universal applicability
 *     of certain analytical tools.
 *   - Curriculum Designers: (institutional/constrained) — Must incorporate this
 *     counter-intuitive result into mathematical education, constraining
 *     simplified pedagogical narratives.
 *   - Pure Mathematicians: (analytical/analytical) — See the function not as a
 *     limitation but as a foundational insight into the structure of function spaces.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(weierstrass_proof_limits, 0.15).
domain_priors:suppression_score(weierstrass_proof_limits, 0.02).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(weierstrass_proof_limits, 0.01).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(weierstrass_proof_limits, extractiveness, 0.15).
narrative_ontology:constraint_metric(weierstrass_proof_limits, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(weierstrass_proof_limits, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(weierstrass_proof_limits, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(weierstrass_proof_limits, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(weierstrass_proof_limits, mountain).
narrative_ontology:human_readable(weierstrass_proof_limits, "The Existence of Continuous, Nowhere-Differentiable Functions").

% --- Emergence flag (required for mountain constraints) ---
% This constraint is a mathematical truth, emerging naturally from axioms
% without human design or enforcement.
domain_priors:emerges_naturally(weierstrass_proof_limits).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed for natural law. This is a mountain-only constraint;
% it lacks the coordination/extraction functions that would necessitate
% beneficiary/victim declarations for directionality derivation. The impact
% is universal, even if felt differently by various groups.

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

% This is a uniform-type constraint (mountain-only). The classification is
% invariant across all perspectives, as it represents a fundamental
% mathematical limit.

% PERSPECTIVE 1: THE NOVICE PRACTITIONER (POWERLESS)
% A student or engineer whose tools (e.g., standard calculus) fail when
% confronted with this type of function. They are trapped by their toolkit.
constraint_indexing:constraint_classification(weierstrass_proof_limits, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE CURRICULUM DESIGNER (INSTITUTIONAL)
% An institution (e.g., a university math department) that must incorporate
% this counter-intuitive example into its curriculum. It is constrained by
% this mathematical fact.
constraint_indexing:constraint_classification(weierstrass_proof_limits, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE APPLIED SCIENTIST (MODERATE)
% A researcher whose models rely on smoothness assumptions. The existence of
% such functions constrains the generality of their claims.
constraint_indexing:constraint_classification(weierstrass_proof_limits, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (PURE MATHEMATICIAN)
% The default analytical context. From this view, the function is a foundational
% truth about the nature of function spaces.
constraint_indexing:constraint_classification(weierstrass_proof_limits, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(weierstrass_proof_limits_tests).

test(perspectival_invariance) :-
    % Verify that for a mountain-only constraint, the classification is invariant.
    constraint_indexing:constraint_classification(weierstrass_proof_limits, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(weierstrass_proof_limits, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless == mountain,
    TypeAnalytical == mountain.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(weierstrass_proof_limits, ExtMetricName, E),
    E =< 0.25, % Mountain threshold
    domain_priors:suppression_score(weierstrass_proof_limits, S),
    S =< 0.05. % Mountain threshold

:- end_tests(weierstrass_proof_limits_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Weierstrass function represents an inherent limitation in mathematical
 *   analysis, a "natural law" of the mathematical universe. Its existence is
 *   a fact, not a policy. Base extractiveness is low (0.15) because while it
 *   constrains certain analytical methods, it does not extract value in a
 *   socio-economic sense; it simply defines the boundaries of what is possible.
 *   Suppression is near-zero (0.02) as it doesn't prevent alternative
 *   mathematical approaches (like measure theory), it just proves one approach
 *   (naive differentiation) is not universally applicable. The theater ratio
 *   is negligible (0.01) as there is no performative aspect.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is a uniform-type constraint that
 *   classifies as a Mountain from all perspectives. A student (powerless),
 *   an engineer (moderate), a university (institutional), and a pure
 *   mathematician (analytical) may have different emotional or practical
 *   reactions to it, but they all must ultimately accept it as an unchangeable
 *   feature of the landscape.
 *
 * DIRECTIONALITY LOGIC:
 *   As a mountain-only constraint representing a natural law, formal
 *   beneficiary/victim declarations are not applicable. The directionality
 *   derivation chain is bypassed because the constraint's classification is
 *   invariant. While one could argue pure mathematicians "benefit" from the
 *   richness it reveals and applied scientists are "victims" of its
 *   limitations, these are not structural roles of extraction but different
 *   ways of relating to an objective fact.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain is crucial. It prevents mislabeling an
 *   inherent mathematical property as a Snare or Piton. The "cost" borne by
 *   those whose methods are invalidated is not a form of extraction but a
 *   consequence of confronting a fundamental truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_weierstrass_proof_limits,
    'Could a future paradigm of mathematics (e.g., non-standard analysis, constructive mathematics) fundamentally alter the "constraint" status of such functions?',
    'A major, foundational shift in mathematics that re-contextualizes continuity and differentiability.',
    'If True: The constraint might be re-classified as a feature of a specific axiomatic system, not a universal one. If False: The limitation remains fundamental across all consistent mathematical frameworks.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(weierstrass_proof_limits, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required. Base extractiveness (0.15) is below the 0.46 threshold for
% mandatory lifecycle drift tracking. As a mathematical constant, its
% properties do not drift over time.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for this type of fundamental mathematical constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable. As a mountain-only constraint, classification is invariant
% and does not depend on directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */