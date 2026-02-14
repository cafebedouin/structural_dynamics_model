% ============================================================================
% CONSTRAINT STORY: constraint_pythagorean
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_pythagorean, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constraint_pythagorean
 *   human_readable: Pythagorean Theorem
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Pythagorean Theorem is a fundamental relationship in Euclidean geometry among the three sides of a right triangle. It states that the square of the hypotenuse (the side opposite the right angle) is equal to the sum of the squares of the other two sides. This constraint is a fundamental mathematical truth.
 *
 * KEY AGENTS (by structural relationship):
 *   - Students: Primary target (powerless/trapped) — must learn and apply it in specific contexts
 *   - Mathematicians: Primary beneficiary (institutional/arbitrage) — use it as a foundation for further discoveries and applications
 *   - Engineers: Secondary actor (powerful/mobile) — apply it in real-world constructions.
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees the theorem as an immutable truth.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constraint_pythagorean, 0.05).
domain_priors:suppression_score(constraint_pythagorean, 0.01).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(constraint_pythagorean, 0.0).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constraint_pythagorean, extractiveness, 0.05).
narrative_ontology:constraint_metric(constraint_pythagorean, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(constraint_pythagorean, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.

narrative_ontology:constraint_metric(constraint_pythagorean, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(constraint_pythagorean, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(constraint_pythagorean, mountain).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause([id]).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement([id]). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.

domain_priors:emerges_naturally(constraint_pythagorean).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(constraint_pythagorean, mathematicians).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(constraint_pythagorean, students).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE/MOUNTAIN)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if the constraint is a snare with a critical
% mass of victims, potentially changing the classification.
%
% UNIFORM-TYPE EXCEPTION: For natural law constraints (mountain-only) or pure
% coordination constraints (rope-only), perspectives 1 and 2 may use any power
% atoms — the classification is the same from all perspectives. Include at
% least 2-3 perspectives to demonstrate the invariance.
constraint_indexing:constraint_classification(constraint_pythagorean, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(constraint_pythagorean, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(constraint_pythagorean, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constraint_pythagorean_tests).

test(perspectival_consistency) :-
    % Verify consistency across perspectives (all should be mountain).
    constraint_indexing:constraint_classification(constraint_pythagorean, Type1, context(_, _, _, _)),
    constraint_indexing:constraint_classification(constraint_pythagorean, Type2, context(_, _, _, _)),
    constraint_indexing:constraint_classification(constraint_pythagorean, Type3, context(_, _, _, _)),
    Type1 = mountain,
    Type2 = mountain,
    Type3 = mountain.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(constraint_pythagorean, ExtMetricName, E),
    E =< 0.25. % Mountain: extractiveness <= 0.25

:- end_tests(constraint_pythagorean_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Pythagorean theorem is a mathematical truth with minimal extractiveness, high accessibility collapse, and no meaningful resistance. The base extractiveness is low because while it requires learning and application, the theorem itself provides significant value and utility. The suppression score is low as well since the theorem does not suppress any alternatives; it simply describes a relationship between sides of a right triangle.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify this as a Mountain because it's a fundamental mathematical truth, independent of perspective. While students might find it difficult to learn (making it feel like a snare), and mathematicians find it useful for further research (making it seem like a rope), it is ultimately an unchangeable property of the universe.
 *
 * DIRECTIONALITY LOGIC:
 *   Mathematicians benefit from this constraint as it provides a foundation for further discoveries and applications. Students bear some cost as they must learn and apply it. The categorization identifies the asymmetry of application between mathematicians and students, even if both benefit from mathematical truths in general.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   Not applicable in this context.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain prevents mislabeling this as a pure extraction or coordination mechanism. It's not a pure extraction because the cost to students is minimal compared to the benefits it provides to all who use it. It's not a coordination mechanism because it's not something people actively coordinate to maintain or enforce; it simply *is*.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_pythagorean,
    'Is the subjective experience of learning the theorem a relevant form of extraction?',
    'Empirical studies of student frustration and learning curves.',
    'If yes, reclassify as Tangled Rope for student perspective. If no, maintain Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(constraint_pythagorean, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
% Because extraction is low (0.05), these aren't strictly necessary, but included for demonstration.
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(constraint_pythagorean_tr_t0, constraint_pythagorean, theater_ratio, 0, 0.0).
narrative_ontology:measurement(constraint_pythagorean_tr_t5, constraint_pythagorean, theater_ratio, 5, 0.0).
narrative_ontology:measurement(constraint_pythagorean_tr_t10, constraint_pythagorean, theater_ratio, 10, 0.0).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(constraint_pythagorean_ex_t0, constraint_pythagorean, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(constraint_pythagorean_ex_t5, constraint_pythagorean, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(constraint_pythagorean_ex_t10, constraint_pythagorean, base_extractiveness, 10, 0.05).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(constraint_pythagorean, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override([id], [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
narrative_ontology:affects_constraint(constraint_pythagorean, constraint_euclidean_geometry).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate

% constraint_indexing:directionality_override(constraint_pythagorean, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */