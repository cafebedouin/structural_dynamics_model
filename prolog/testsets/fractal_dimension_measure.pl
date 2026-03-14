% ============================================================================
% CONSTRAINT STORY: fractal_dimension_measure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fractal_dimension_measure, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fractal_dimension_measure
 *   human_readable: Fractal Dimension Measure
 *   domain: mathematics/geometry
 *
 * SUMMARY:
 *   The fractal dimension measure represents a pure mathematical invariant
 *   arising from the definition of self-similarity at arbitrary scales. Any
 *   object that exhibits exact self-similarity — the property that subsets
 *   are geometrically similar to the whole — necessarily possesses a
 *   non-integer dimension calculable from the scaling relationship D =
 *   log(N)/log(r), where N is the number of self-similar copies and r is the
 *   scaling factor. This constraint is universal: it applies identically
 *   regardless of the observer's power, measurement methodology,
 *   institutional context, or time horizon. The fractal dimension is not
 *   enforced through institutional mechanisms, social coordination, or
 *   suppression of alternatives — it is a logical consequence of
 *   self-similarity. No agent benefits from the constraint in an extractive
 *   sense, and no agent bears costs imposed by it. The measure itself is
 *   neutral and accessible to all who engage with self-similar geometry.
 *
 * KEY AGENTS:
 *   - Empirical Measurer: Any agent attempting to quantify fractal dimension encounters the same mathematical structure (powerless/trapped perspective)
 *   - Computational Researcher: Well-resourced programs with arbitrary methodological freedom face identical constraints (powerful/mobile perspective)
 *   - Mathematical Community: Institutional knowledge confirms the necessary relationship between self-similarity and dimension (institutional/arbitrage perspective)
 *   - Analytical Observer: Full structural view reveals a logical invariant independent of institutional or social mediation (analytical/analytical perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fractal_dimension_measure, 0.12).
domain_priors:suppression_score(fractal_dimension_measure, 0.03).
domain_priors:theater_ratio(fractal_dimension_measure, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fractal_dimension_measure, extractiveness, 0.12).
narrative_ontology:constraint_metric(fractal_dimension_measure, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(fractal_dimension_measure, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fractal_dimension_measure, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fractal_dimension_measure, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fractal_dimension_measure, mountain).
narrative_ontology:human_readable(fractal_dimension_measure, "Fractal Dimension Measure").
narrative_ontology:topic_domain(fractal_dimension_measure, "mathematics/geometry").

domain_priors:emerges_naturally(fractal_dimension_measure).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPIRICAL MEASURER (MOUNTAIN) — Any agent attempting to measure fractal dimension encounters the same mathematical constraint regardless of power, time horizon, or context. The Hausdorff-Besicovitch definition and its operational approximations (box-counting, mass scaling exponents) are inescapable features of how fractality is quantified. No measurement methodology can escape the fundamental self-similarity relationship.
constraint_indexing:constraint_classification(fractal_dimension_measure, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPUTATIONAL RESEARCHER (MOUNTAIN) — Even well-resourced research programs with arbitrary computational budgets and methodological freedom encounter the same scaling invariance. Algorithmic sophistication, computational power, and unlimited measurement access do not change the mathematical structure. The constraint is invariant across all implementational approaches.
constraint_indexing:constraint_classification(fractal_dimension_measure, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: MATHEMATICAL COMMUNITY (MOUNTAIN) — Institutional mathematical knowledge, theorem-proving capability, and formalized understanding all confirm: fractal dimension is a logical consequence of self-similarity, not a contingent property dependent on institutional arrangements, funding, or social factors. The mathematical structure is accessible to and acknowledged by all institutional contexts.
constraint_indexing:constraint_classification(fractal_dimension_measure, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From a position of full structural transparency, the fractal dimension measure is a mathematical invariant: a necessary consequence of scale-invariant geometry. No institutional, economic, or social mechanism can suspend or modify the relationship between self-similarity and non-integer dimensionality. The constraint emerges from logical necessity, not from enforcement or coordination problems.
constraint_indexing:constraint_classification(fractal_dimension_measure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fractal_dimension_measure_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fractal_dimension_measure, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fractal_dimension_measure, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fractal_dimension_measure, ExtMetricName, E),
    domain_priors:suppression_score(fractal_dimension_measure, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fractal_dimension_measure),
    narrative_ontology:constraint_metric(fractal_dimension_measure, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fractal_dimension_measure, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fractal_dimension_measure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The fractal dimension constraint does not extract value from any agent or redistribute resources. The measure is knowledge about geometric properties — it constrains how objects can be measured and described, but this is not extraction in the sense of asymmetric cost-bearing. The low value reflects that no beneficiary or victim relationship exists: all agents have access to the same definition and measurement relationships. Suppression (0.03): Negligible. There are no barriers to learning or applying fractal dimension theory. The mathematics is published, available in standard textbooks, and has no gatekeeping mechanisms. No institutional or social force prevents agents from understanding or using the measure. Theater ratio (0.05): Minimal. The fractal dimension measure has no performative component — it is pure mathematical formalism. There is no ritual or theatrical element to the constraint, only logical necessity. Accessibility collapse (0.92): High. The fractal dimension is so deeply embedded in the mathematical structure of self-similarity that alternative ways of characterizing self-similar objects without reference to fractional dimensionality are extremely difficult or impossible. Any meaningful description of fractal geometry will converge to the same dimension value. Resistance (0.08): Low. No observable resistance to adopting the fractal dimension framework. Mathematical communities universally acknowledge its validity. Empirical applications consistently conform to the theory's predictions.
 *
 * PERSPECTIVAL GAP:
 *   Unlike most constraints, the fractal dimension measure exhibits zero perspectival gap. All perspectives — the empirical measurer, the computational researcher, the mathematical community, and the analytical observer — classify this constraint as Mountain. There is no disagreement about the nature or necessity of the constraint. This uniformity is diagnostic: it indicates that the constraint is indeed an invariant, not a social arrangement masquerading as a law. The absence of perspectival gap is the signature of a true natural law in the DR framework.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality derivation applies to this constraint because there are no beneficiaries or victims. The fractal dimension measure does not create asymmetric extraction or coordination problems. The constraint is purely structural and mathematical. Every agent — powerless or institutional, mobile or trapped — encounters the same mathematical relationship. The constraint's structure does not differentiate agents or create differential exit costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_method_equivalence,
    'Are all commonly used fractal dimension measurement methods (Hausdorff-Besicovitch, box-counting, information dimension, correlation dimension) mathematically equivalent or do they capture different aspects of fractality?',
    'Formal proof of equivalence relations; empirical comparison on standard fractal test sets; identification of pathological cases where methods diverge',
    'If equivalent: the constraint is a pure mathematical law with one degree of freedom (D). If distinct: multiple constraints with different ε values, one per measurement method.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_method_equivalence, empirical, 'Equivalence of fractal dimension measurement methodologies').

omega_variable(
    natural_vs_mathematical_fractals,
    'Do empirically measured fractals in nature converge to mathematical fractal dimension, or is empirical ''fractal dimension'' a distinct phenomenon with finite-scale approximation properties?',
    'Long-timescale measurements on natural systems (coastlines, vegetation patterns, porous media); test for asymptotic convergence to mathematical fractal dimension vs persistent finite-scale effects',
    'If convergent: natural fractals instantiate the mathematical constraint. If divergent: empirical fractal dimension is a separate constraint with different ε and classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_mathematical_fractals, empirical, 'Convergence of natural fractals to mathematical fractal dimension').

omega_variable(
    self_similarity_requirement_scope,
    'How precisely must self-similarity hold for an object to possess a well-defined fractal dimension? Does approximate self-similarity or multi-scale self-similarity alter the constraint structure?',
    'Rigorous analysis of fractal dimension under perturbations to self-similarity; characterization of sufficient conditions for dimension existence',
    'If exact self-similarity required: the constraint is restrictive (applies only to idealized structures). If approximate self-similarity sufficient: the constraint is broader and applies to a wider empirical domain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_similarity_requirement_scope, conceptual, 'Precision requirements for self-similarity in fractal dimension definition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fractal_dimension_measure, 0, 66).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fractal_tr_t0, fractal_dimension_measure, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fractal_tr_t33, fractal_dimension_measure, theater_ratio, 33, 0.05).
narrative_ontology:measurement(fractal_tr_t66, fractal_dimension_measure, theater_ratio, 66, 0.05).

% Extraction over time
narrative_ontology:measurement(fractal_be_t0, fractal_dimension_measure, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(fractal_be_t33, fractal_dimension_measure, base_extractiveness, 33, 0.12).
narrative_ontology:measurement(fractal_be_t66, fractal_dimension_measure, base_extractiveness, 66, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fractal_dimension_measure, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
