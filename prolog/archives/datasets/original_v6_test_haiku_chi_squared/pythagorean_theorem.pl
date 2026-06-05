% ============================================================================
% CONSTRAINT STORY: pythagorean_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pythagorean_theorem, []).

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
 *   constraint_id: pythagorean_theorem
 *   human_readable: Pythagorean Theorem
 *   domain: mathematical/euclidean_geometry
 *
 * SUMMARY:
 *   The Pythagorean Theorem represents a mathematical relationship that holds
 *   universally and immutably within Euclidean geometry: for any right
 *   triangle with legs a and b and hypotenuse c, the relationship a² + b² =
 *   c² is necessarily true. This constraint is a paradigm exemplar of a
 *   Mountain in the Deferential Realism framework. It exhibits zero degrees
 *   of freedom for all agents and all observational contexts. No
 *   institutional arrangement, no historical contingency, no agent power
 *   differential can alter the relationship. The theorem appears as a natural
 *   law not because of institutional enforcement but because it is a logical
 *   consequence of the geometric axioms that define Euclidean space. Unlike
 *   social, economic, or policy constraints, the Pythagorean Theorem cannot
 *   be negotiated, captured, escaped, or circumvented through any strategic
 *   action.
 *
 * KEY AGENTS:
 *   - Practical Carpenter: Primary subject (powerless/trapped) — constrained by the theorem when constructing right angles; cannot exit the geometric relationship
 *   - Mathematics Educator: Institutional actor (organized/constrained) — teaches the theorem as structural to geometric knowledge; cannot choose alternate relationships
 *   - Mathematical Analyst: Analytical observer (analytical/analytical) — recognizes the theorem as a logical consequence of geometric axioms; sees full structure across all contexts
 *   - Scientific Standard Body: Beneficiary (institutional/arbitrage) — uses the theorem to establish measurement standards; benefits from its immutability without bearing costs
 *   - Engineering Enterprise: Powerful agent (powerful/mobile) — depends on the theorem for all construction but cannot override it; power differential is irrelevant to the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pythagorean_theorem, 0.12).
domain_priors:suppression_score(pythagorean_theorem, 0.02).
domain_priors:theater_ratio(pythagorean_theorem, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pythagorean_theorem, extractiveness, 0.12).
narrative_ontology:constraint_metric(pythagorean_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(pythagorean_theorem, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(pythagorean_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(pythagorean_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pythagorean_theorem, mountain).
narrative_ontology:human_readable(pythagorean_theorem, "Pythagorean Theorem").
narrative_ontology:topic_domain(pythagorean_theorem, "mathematical/euclidean_geometry").

domain_priors:emerges_naturally(pythagorean_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTICAL CARPENTER (MOUNTAIN) — Cannot construct right angles without conforming to the constraint. The relationship between hypotenuse and legs is invariant regardless of builder intent, material, or effort. d≈1.00, f(d)≈1.42, σ=1.0 → χ≈0.17. The constraint appears as inescapable physical law.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICS EDUCATOR (MOUNTAIN) — Teaching right triangle geometry constrains all pedagogical approaches. Cannot explain diagonal relationships without invoking the theorem. The logical structure of Euclidean space enforces the constraint on curriculum design. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.14. Mountain invariant across all teaching methodologies.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MATHEMATICAL ANALYST (MOUNTAIN) — Within Euclidean geometry, the theorem is a logical consequence of the parallel postulate and the axioms of geometry. No agent, no institutional arrangement, no historical circumstance can alter this relationship. The constraint is structural to the geometric space itself. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.14. Universal and immutable.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: SCIENTIFIC STANDARD BODY (MOUNTAIN) — Standardization bodies (SI, NIST, ISO) cannot override the theorem when defining measurement standards for right angles and orthogonal reference frames. The constraint pre-exists and enables institutional standardization rather than being constrained by it. d≈0.15, f(d)≈0.05, σ=1.0 → χ≈0.01. Beneficial but immutable; no extraction mechanism.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: ENGINEERING ENTERPRISE (MOUNTAIN) — Large-scale engineering (infrastructure, navigation, surveying) depends entirely on the theorem but cannot negotiate with it. The constraint offers massive coordination benefit but imposes zero degrees of freedom. Even the most powerful agents (nation-states, multinational corporations) cannot build structures that violate the relationship. d≈0.40, f(d)≈0.40, σ=1.0 → χ≈0.05. Mountain classification invariant despite power asymmetry.
constraint_indexing:constraint_classification(pythagorean_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pythagorean_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(pythagorean_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pythagorean_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(pythagorean_theorem, ExtMetricName, E),
    domain_priors:suppression_score(pythagorean_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(pythagorean_theorem),
    narrative_ontology:constraint_metric(pythagorean_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(pythagorean_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(pythagorean_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Very low. The theorem does not extract value from any agent in the sense of asymmetric redistribution. Instead, it imposes a uniform constraint that applies identically to all agents. The minimal non-zero value (rather than exactly 0.00) reflects that agents must invest cognitive effort in learning and applying the theorem, but this investment is not extraction — it is coordination cost absorbed equally across all users. Suppression (0.02): Near-zero. Agents are not suppressed in their alternatives because there are no alternatives. In Euclidean geometry, right triangles have only one possible relationship between sides. The minimal value reflects definitional clarity rather than coercion — agents understand immediately that the constraint is inescapable. Theater ratio (0.05): Near-zero. The theorem requires almost no performative maintenance. It is not taught or understood through ritual; the relationship is demonstrated through straightforward geometric construction or algebraic proof. Modern mathematics education teaches the theorem through direct logical argument, not through institutional theater.
 *
 * PERSPECTIVAL GAP:
 *   The Pythagorean Theorem produces no perspectival gap across different observers because it classifies as Mountain from all structural positions. The practical carpenter, the educator, the analyst, the standards body, and the engineer all perceive the same invariant relationship. Their exit options differ (trapped vs arbitrage vs analytical), their time horizons differ (biographical vs civilizational), their power levels differ (powerless vs institutional vs powerful), but all perspectives converge on the Mountain classification. This convergence is not accidental — it is precisely what a natural law should exhibit. If perspectives produced different classifications (some seeing Rope, others seeing Snare), the constraint would be socially contingent, not natural. The absence of perspectival gap is the hallmark of true structural invariance.
 *
 * DIRECTIONALITY LOGIC:
 *   Pythagorean Theorem as a natural law does not produce directionality variation because there is no asymmetric extraction. The derivation chain cannot produce meaningfully different d values across perspectives because no agent is a structural beneficiary or victim — all agents are equally constrained. The carpenter's d≈1.00 (trapped) and the standards body's d≈0.15 (arbitrage) both yield low χ values because extractiveness itself is near-zero. The framework's directionality mechanism (beneficiary/victim → d → f(d) → χ) is correctly modeling a natural law: the constraint exists independent of who benefits or suffers, and its classification does not shift based on power differentials or exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The Pythagorean Theorem resolves the mandatrophy through absence rather than through multi-perspectival resolution. There is no mandatrophy risk because the theorem classifies identically from all perspectives as a Mountain. Mandatrophy arises when a constraint could plausibly be labeled as either coordination (Rope, Scaffold) or extraction (Snare, Piton, Tangled Rope), and the choice of label risks mislabeling the institutional dynamics. The Pythagorean Theorem has no such ambiguity — it is pure constraint, not extraction mechanism. The absence of beneficiaries and victims (no structural asymmetry) means the engine cannot misclassify it as extractive. This exemplifies how natural laws have a distinct verification signature: the theorem would still classify as Mountain even if we lacked any institutional or historical data about how it is taught or used.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    non_euclidean_applicability,
    'Does the Pythagorean Theorem constrain curved (non-Euclidean) geometric spaces?',
    'Mathematical analysis of theorem formulations in hyperbolic and spherical geometry; verification that generalized versions require modification coefficients',
    'If applicable universally: constraint is truly universal (mountain across all metric spaces). If bounded to Euclidean geometry: constraint is conditional on space-type selection, not truly universal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_euclidean_applicability, conceptual, 'Scope of theorem across geometric systems').

omega_variable(
    foundational_independence,
    'Is the Pythagorean Theorem independent of the parallel postulate, or does it logically depend on it?',
    'Formal proof analysis in axiomatic geometry; investigation of theorem derivation in non-Euclidean systems',
    'If independent: theorem is more fundamental than postulate (stronger mountain). If dependent: theorem''s status as natural law is conditional on the axiomatic framework chosen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_independence, conceptual, 'Logical independence from parallel postulate').

omega_variable(
    physical_space_fidelity,
    'Does physical Euclidean space exactly satisfy the Pythagorean relationship, or only approximately?',
    'Empirical measurement of right triangles at different scales; comparison with relativistic geometric predictions; assessment of experimental precision limits',
    'If exact: mountain classification is warranted for physical applications. If approximate: physical space has non-Euclidean curvature, and the theorem is a mathematical idealization rather than a natural law of physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_space_fidelity, empirical, 'Fidelity of theorem to physical space').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pythagorean_theorem, 0, 5000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pyth_tr_t0, pythagorean_theorem, theater_ratio, 0, 0.03).
narrative_ontology:measurement(pyth_tr_t2500, pythagorean_theorem, theater_ratio, 2500, 0.05).
narrative_ontology:measurement(pyth_tr_t5000, pythagorean_theorem, theater_ratio, 5000, 0.05).

% Extraction over time
narrative_ontology:measurement(pyth_be_t0, pythagorean_theorem, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(pyth_be_t2500, pythagorean_theorem, base_extractiveness, 2500, 0.12).
narrative_ontology:measurement(pyth_be_t5000, pythagorean_theorem, base_extractiveness, 5000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pythagorean_theorem, information_standard).
narrative_ontology:affects_constraint(pythagorean_theorem, euclidean_space_closure).
narrative_ontology:affects_constraint(pythagorean_theorem, right_angle_measurement_standard).
narrative_ontology:affects_constraint(pythagorean_theorem, orthogonal_basis_decomposition).

% DUAL FORMULATION NOTE:
% The Pythagorean Theorem is upstream of multiple applied constraints in engineering, surveying, and navigation. It serves as a foundational axiom that enables (rather than constrains) these downstream constraints. Unlike most network relationships where a constraint influences others' behavior, here the theorem is a natural law that enables the possibility of other geometric constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
