% ============================================================================
% CONSTRAINT STORY: space_filling_curve_family
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_space_filling_curve_family, []).

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
 *   constraint_id: space_filling_curve_family
 *   human_readable: Space-Filling Curve Family (Mathematical Existence and Construction)
 *   domain: mathematics/topology
 *
 * SUMMARY:
 *   Space-filling curves (curves that pass through every point of a
 *   two-dimensional region while remaining continuous and one-dimensional)
 *   represent a mathematical necessity: they must exist as a logical
 *   consequence of the real numbers' structure and the topology of Euclidean
 *   space. Peano's 1890 construction proved their existence explicitly; since
 *   then, multiple variant constructions have confirmed that such curves are
 *   not exotic anomalies but unavoidable features of the mathematical
 *   landscape. The constraint is immutable across all contexts: no
 *   mathematical framework, computational method, or pedagogical approach can
 *   eliminate space-filling curves as a structural fact. They are neither
 *   discovered nor invented — they emerge necessarily from the axioms and
 *   definitions of topology. The extractiveness and suppression metrics are
 *   minimal because there is no asymmetric extraction and no coercion:
 *   space-filling curves constrain all agents equally by presenting a logical
 *   necessity that cannot be circumvented, negotiated, or escaped through
 *   alternative framing.
 *
 * KEY AGENTS:
 *   - Analytical Mathematicians: All power levels (universal perspective) — encounter space-filling curves as a universal mathematical necessity
 *   - Topologists and Geometric Analysts: Powerful/organized (research context) — agency in choice of problems but not in fundamental mathematical reality
 *   - Educators and Communicators: Moderate/constrained (pedagogical context) — must convey a result that violates student intuition, but the result itself is unchangeable
 *   - Computational Agents: Powerless/trapped (implementation context) — face the reality that constructing these curves explicitly requires irreducible computational overhead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(space_filling_curve_family, 0.12).
domain_priors:suppression_score(space_filling_curve_family, 0.03).
domain_priors:theater_ratio(space_filling_curve_family, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(space_filling_curve_family, extractiveness, 0.12).
narrative_ontology:constraint_metric(space_filling_curve_family, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(space_filling_curve_family, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(space_filling_curve_family, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(space_filling_curve_family, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(space_filling_curve_family, mountain).
narrative_ontology:human_readable(space_filling_curve_family, "Space-Filling Curve Family (Mathematical Existence and Construction)").
narrative_ontology:topic_domain(space_filling_curve_family, "mathematics/topology").

domain_priors:emerges_naturally(space_filling_curve_family).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL MATHEMATICIAN (MOUNTAIN) — From the standpoint of pure mathematics across all time horizons and spatial contexts, space-filling curves exist as a structural necessity. Peano's construction proves their existence; continuity and surjectivity are non-negotiable properties. No alternative formulation can escape this necessity.
constraint_indexing:constraint_classification(space_filling_curve_family, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: RESEARCH MATHEMATICIAN / CONSTRUCTIVE VIEW (MOUNTAIN) — Even researchers with agency in problem selection and method development face an immutable structural fact: continuous surjections from [0,1] to [0,1]² exist and possess the Peano properties. The mathematician may choose which construction to study or whether to investigate space-filling curves at all, but cannot escape the existence of the phenomenon once engaged.
constraint_indexing:constraint_classification(space_filling_curve_family, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: GRADUATE STUDENT IN TOPOLOGY (MOUNTAIN) — Even a student with limited power and constrained exit options encounters space-filling curves as an irreducible mathematical fact within their field. The constraint is unchangeable at the biographical horizon. Whether they study the topic or not, its existence and properties do not bend to their agency or circumstance.
constraint_indexing:constraint_classification(space_filling_curve_family, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTRAINED PROBLEM SOLVER (MOUNTAIN) — A solver tasked with constructing a continuous map from [0,1] onto [0,1]² faces an immutable requirement: the map exists necessarily and can be constructed explicitly via the Peano method or variants. No workaround, approximation, or alternative coordinate system erases this necessity.
constraint_indexing:constraint_classification(space_filling_curve_family, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(space_filling_curve_family_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(space_filling_curve_family, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(space_filling_curve_family, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(space_filling_curve_family, ExtMetricName, E),
    domain_priors:suppression_score(space_filling_curve_family, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(space_filling_curve_family),
    narrative_ontology:constraint_metric(space_filling_curve_family, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(space_filling_curve_family, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(space_filling_curve_family_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. Space-filling curves do not extract value from any agent in the service of another. They constrain all parties equally by existing as a mathematical necessity. There is no beneficiary or victim — only the shared constraint of mathematical reality. Suppression (0.03): Negligible. No suppression of alternatives is needed because alternatives do not exist. The constraint is pure necessity, not enforcement. Theater ratio (0.15): Minimal but nonzero. The pedagogical presentation of space-filling curves does involve some theatrical element — the counterintuitive framing, the visual demonstrations, the historical narrative of how 19th-century mathematicians reacted to the result. But this theater is pedagogical scaffolding, not functional performance. Once the mathematical proof is understood, the theater dissolves. Accessibility collapse (0.92): Very high. For any mathematician with basic topology knowledge, the existence of space-filling curves is completely accessible — the proofs are constructive and the result follows necessarily from definitions. The 'collapse' is near-total because there is no way to avoid or escape the conclusion. Resistance (0.08): Very low. No agent or community has successfully resisted or delayed acceptance of space-filling curves as real mathematical objects. The constraint is invariant across time, culture, and mathematical tradition. From 1890 onwards, the result has been accepted universally.
 *
 * PERSPECTIVAL GAP:
 *   Space-filling curves generate no perspectival gap because all perspectives converge on the mountain classification. The analytical mathematician sees a universal necessity. The research mathematician, even with agency in problem selection, sees an immutable structural fact within the domain. The graduate student, with constrained mobility, sees an unchangeable property of the mathematical landscape. The powerless problem-solver tasked with a space-filling curve construction sees an unavoidable requirement. All four perspectives (covering the full range of power from powerless to analytical) classify the constraint identically as mountain. This uniformity of classification across all indices is the diagnostic signature of a true natural law: the constraint is invariant across time horizon, spatial scope, power level, and exit options. No amount of agency, resources, or temporal horizon changes the structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for space-filling curves because there are no beneficiaries or victims. The constraint is not an extraction mechanism but a logical necessity that applies equally to all agents. No agent benefits from space-filling curves existing at the expense of others; all agents are constrained by the same immutable mathematical reality. The canonical d value for mountain constraints (0.00, corresponding to institutional/analytical perspective) would apply here as a formal matter, but it is misleading because it suggests a directionality relationship that does not exist. Better framing: the constraint has zero directionality because it binds all parties identically.
 *
 * MANDATROPHY ANALYSIS:
 *   NO MANDATROPHY PRESENT. Space-filling curves represent a pure natural law with no extraction component and minimal coordination overhead. The constraint cannot be misclassified as coordination because it is neither a solution to a collective action problem nor a mechanism for organizing agent interaction. It is a structural fact that all agents must accommodate. The mountain classification is stable and unambiguous: extractiveness ≤ 0.25, suppression ≤ 0.05, accessibility_collapse ≥ 0.85, resistance ≤ 0.15, emerges_naturally = true. All five gates pass. The constraint is mathematically robust across all axiom systems examined to date and has shown no empirical anomalies or hidden extraction mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructive_vs_nonconstructive_existence,
    'Does the mathematical reality of space-filling curves depend on a specific axiom system (e.g., the Axiom of Choice, the Excluded Middle), or are they robust across all reasonable foundations?',
    'Analysis of space-filling curve proofs in constructive mathematics, intuitionistic logic, and predicative frameworks. Examination of whether Peano''s construction is valid without classical axioms.',
    'If constructively valid in all frameworks: mountain classification fully robust. If constructively valid only in classical ZFC: mountain classification is framework-relative, potentially softening to rope (coordination around axiom choice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constructive_vs_nonconstructive_existence, conceptual, 'Axiom system dependence of space-filling curve existence').

omega_variable(
    computational_realizability_threshold,
    'Is the computational barrier to constructing a space-filling curve to arbitrary precision a fundamental limit (mountain) or a technological contingency (rope)?',
    'Theoretical analysis of Turing computability for the Peano curve; empirical measurement of actual computation time vs approximation error; comparison with other mathematical constructions.',
    'If fundamental limit: extraction from any computational agent is unavoidable (mountain confirmed). If technological: the constraint''s force depends on computational resources available (becomes more rope-like as computers improve).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_realizability_threshold, empirical, 'Computational realizability of space-filling curve construction').

omega_variable(
    dimensional_intuition_collapse,
    'Why does the existence of space-filling curves violate intuition so powerfully? Is the intuitive resistance a sign of deep structural incompleteness in how we conceive dimension, or merely a pedagogical artifact?',
    'Historical analysis of how mathematicians'' understanding of dimension and curves evolved post-Peano; comparison with other ''counterintuitive'' results that later revealed conceptual inadequacy (e.g., Cantor''s diagonal, non-Euclidean geometry); development of alternative conceptual frameworks that make space-filling curves ''intuitive''.',
    'If artifact: the constraint is mountain in mathematics but perceived as rope (mere coordination around intuitive conceptualization). If structural: indicates that classical topology''s dimensional framework is incomplete and space-filling curves reveal a real flaw.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dimensional_intuition_collapse, conceptual, 'Whether dimensional intuition collapse indicates deep incompleteness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(space_filling_curve_family, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spac_tr_t0, space_filling_curve_family, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spac_tr_t50, space_filling_curve_family, theater_ratio, 50, 0.14).
narrative_ontology:measurement(spac_tr_t100, space_filling_curve_family, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(spac_be_t0, space_filling_curve_family, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(spac_be_t50, space_filling_curve_family, base_extractiveness, 50, 0.11).
narrative_ontology:measurement(spac_be_t100, space_filling_curve_family, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(space_filling_curve_family, information_standard).
narrative_ontology:affects_constraint(space_filling_curve_family, peano_axioms_completeness).
narrative_ontology:affects_constraint(space_filling_curve_family, metric_space_compactness).
narrative_ontology:affects_constraint(space_filling_curve_family, topological_dimension_theory).

% DUAL FORMULATION NOTE:
% Space-filling curves are downstream of the real number axioms and topological definitions. They do not decompose into structurally distinct constraints — the phenomenon is unified. However, specific space-filling curve constructions (Peano's original, Hilbert's variant, dragon curves, etc.) may each merit separate stories if studying how different constructions distribute computational cost or pedagogical complexity differently. Those would be downstream stories in a constraint family, each with identical ε but potentially differing commentary on constructive methods.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
