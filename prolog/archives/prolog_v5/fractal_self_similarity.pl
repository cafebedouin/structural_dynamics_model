% ============================================================================
% CONSTRAINT STORY: fractal_self_similarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fractal_self_similarity, []).

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
 *   constraint_id: fractal_self_similarity
 *   human_readable: Fractal Self-Similarity as Mathematical Invariant
 *   domain: mathematics/geometry/topology
 *
 * SUMMARY:
 *   Fractal self-similarity is the mathematical principle that a structure
 *   exhibits invariant properties across different scales — a Mandelbrot set
 *   looks identical when magnified arbitrarily deeply, a Cantor set retains
 *   its construction logic at every level of iteration, a power-law
 *   distribution in network degree preserves its exponent across orders of
 *   magnitude. This constraint operates at the foundation of geometry and
 *   measure theory. It is not an institutional arrangement, a policy
 *   artifact, or an economic mechanism — it is a structural property of
 *   certain mathematical objects that emerges naturally from their defining
 *   axioms. The constraint manifests as an immediate, absolute barrier to
 *   complete verification or exhaustive computation: infinite detail cannot
 *   be accessed in finite time with finite resources. This is not a
 *   technological gap to be closed but a fundamental limit on what can be
 *   known or measured. The constraint exhibits zero degrees of freedom — all
 *   observers confront the same mathematical reality regardless of their
 *   power, position, or intent.
 *
 * KEY AGENTS:
 *   - Empirical Observer: Powerless/trapped — cannot measure infinite detail; measurement precision decays at deeper scales
 *   - Mathematical Analyst: Analytical/analytical — can formalize the structure but only up to finite depth; infinite self-similarity is accessible only through proof, not computation
 *   - Applied Modeler: Powerful/mobile — can invest resources to probe deeper scales but faces non-negotiable accuracy-resolution trade-off; funding and computation cannot overcome the scaling law
 *   - Computational System: Any algorithm — Halting Problem limits prevent exhaustive fractal enumeration; algorithms can approximate but not complete
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fractal_self_similarity, 0.08).
domain_priors:suppression_score(fractal_self_similarity, 0.02).
domain_priors:theater_ratio(fractal_self_similarity, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fractal_self_similarity, extractiveness, 0.08).
narrative_ontology:constraint_metric(fractal_self_similarity, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(fractal_self_similarity, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fractal_self_similarity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fractal_self_similarity, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fractal_self_similarity, mountain).
narrative_ontology:human_readable(fractal_self_similarity, "Fractal Self-Similarity as Mathematical Invariant").
narrative_ontology:topic_domain(fractal_self_similarity, "mathematics/geometry/topology").

domain_priors:emerges_naturally(fractal_self_similarity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A measurement device attempting to verify fractal self-similarity at all scales confronts the fundamental limit: infinite detail cannot be measured in finite time with finite precision. This is not a technological barrier that better instruments overcome — it is structural to the nature of measurement itself. The observer is trapped by the mathematics.
constraint_indexing:constraint_classification(fractal_self_similarity, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% Self-similarity at infinite scales is a rigorous mathematical property. The Hausdorff dimension of a Cantor set, the lacunarity structure of a Mandelbrot set, the scale-invariance of a power law — these are theorems, not approximations. No observer position escapes the mathematics. The constraint emerges naturally from the axioms of set theory and topology.
constraint_indexing:constraint_classification(fractal_self_similarity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% An engineer modeling a turbulent cascade or a coastline using fractal approximations faces the non-negotiable trade-off: computational accuracy scales with resolution, and resolution scales exponentially with depth. No amount of funding, computational power, or algorithmic cleverness eliminates the underlying constraint. The scaling law is invariant.
constraint_indexing:constraint_classification(fractal_self_similarity, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fractal_self_similarity_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fractal_self_similarity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fractal_self_similarity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fractal_self_similarity, ExtMetricName, E),
    domain_priors:suppression_score(fractal_self_similarity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fractal_self_similarity),
    narrative_ontology:constraint_metric(fractal_self_similarity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fractal_self_similarity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fractal_self_similarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. No agent extracts value at the expense of another — the constraint is purely epistemological, not extractive. It limits what can be known, not who benefits from knowledge. The low value reflects that this is a Natural Law, not a distribution mechanism. Suppression (0.02): Negligible. There are no alternatives being suppressed — the mathematical axioms do not oppress other geometries, they simply define what self-similarity is. The low value is as expected for mountains. Theater ratio (0.15): Low. Mathematical proofs are transparent — there is no performative content in stating that infinite self-similarity cannot be fully measured. The small non-zero value reflects that some formalism and notation is required to communicate the constraint, but this is necessary communication, not deception.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify the constraint identically as mountain, which is diagnostic of a uniform-type constraint. The perspectival 'gap' here is not disagreement but confirmation: powerless empiricists, powerful engineers, and analytical mathematicians all reach the same conclusion from different vantage points. A powerless observer cannot escape the constraint by lacking resources (empirically trapped). A powerful observer cannot overcome it with resources (scaling law is invariant). An analytical observer proves the constraint holds independently of any particular measurement (formally universal). The absence of a perspectival gap is itself the signature of a true mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality applies. There are no beneficiaries or victims — the constraint is not an extraction mechanism. There is no agent relationship to analyze because the constraint is not relational; it is structural to the mathematics itself. Directionality derivation is inapplicable for true natural laws.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by instantiating it perfectly: the constraint is unambiguously mountain because it satisfies ALL gates and is NOT decomposable into smaller constraints with different ε values. Fractal self-similarity cannot be observed as 'coordination with extraction' or 'temporary scaffolding' or 'degraded ritual' from any observer position. It is not a Snare in disguise depending on measurement frame. The constraint's immutability is not contingent on observational choice — it follows necessarily from the axioms. This is what a true mountain looks like.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_self_similarity_empirical_status,
    'Do naturally occurring fractals (coastlines, turbulent flow, biological branching) exhibit true mathematical self-similarity or merely approximate it over limited scales?',
    'High-precision measurement across 8+ orders of magnitude; identification of scale-dependent deviations from power-law predictions; comparison of theoretical Hausdorff dimension against empirical lacunarity measurements',
    'If true self-similarity: constraint is a mountain (universal immutable principle). If approximate only: constraint might be tangled_rope (natural geometry plus measurement/modeling extractiveness).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(true_self_similarity_empirical_status, empirical, 'Whether fractal self-similarity is mathematically true or empirically approximate').

omega_variable(
    measurement_versus_definition_collapse,
    'Is the impossibility of measuring infinite self-similarity a fundamental physical law or a logical consequence of how we define measurement?',
    'Philosophical analysis of measurement axioms; examination of whether alternative measurement formalisms (non-standard analysis, ultrafinitism) escape the constraint; exploration of quantum gravity scale limits as fundamental versus merely empirical',
    'If fundamental: mountain classification holds (immutable constraint on knowledge). If definitional: constraint might degrade to rope or scaffold under alternative formalisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_versus_definition_collapse, conceptual, 'Whether the measurement barrier is ontological or definitional').

omega_variable(
    computational_halting_connection,
    'Does the incomputability of infinite fractal detail relate to the Halting Problem and Gödel''s Incompleteness, or are these independent constraints?',
    'Formal proof of equivalence/non-equivalence between fractal detail computability and Halting Problem decidability; analysis of algorithmic information theory and Kolmogorov complexity bounds on fractal description',
    'If equivalent: self-similarity constraint is subsumed under deeper logical limits (mountain of mountains). If independent: each constraint requires separate analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_halting_connection, empirical, 'Whether fractal incomputability relates to Halting Problem').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fractal_self_similarity, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frac_tr_t0, fractal_self_similarity, theater_ratio, 0, 0.15).
narrative_ontology:measurement(frac_tr_t3, fractal_self_similarity, theater_ratio, 3, 0.15).
narrative_ontology:measurement(frac_tr_t6, fractal_self_similarity, theater_ratio, 6, 0.15).

% Extraction over time
narrative_ontology:measurement(frac_be_t0, fractal_self_similarity, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(frac_be_t3, fractal_self_similarity, base_extractiveness, 3, 0.08).
narrative_ontology:measurement(frac_be_t6, fractal_self_similarity, base_extractiveness, 6, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fractal_self_similarity, information_standard).
narrative_ontology:affects_constraint(fractal_self_similarity, halting_problem_computational_limit).
narrative_ontology:affects_constraint(fractal_self_similarity, goedel_incompleteness_logical_limit).
narrative_ontology:affects_constraint(fractal_self_similarity, measurement_precision_uncertainty).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
