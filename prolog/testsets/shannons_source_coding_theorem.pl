% ============================================================================
% CONSTRAINT STORY: shannons_source_coding_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shannons_source_coding_theorem, []).

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
 *   constraint_id: shannons_source_coding_theorem
 *   human_readable: Shannon's Source Coding Theorem (Achievable Compression Limit)
 *   domain: information_theory/technological
 *
 * SUMMARY:
 *   Shannon's Source Coding Theorem (1948) establishes that the expected
 *   length of any lossless code for a source with entropy H(X) cannot be less
 *   than H(X) bits per symbol, and codes approaching this limit can be
 *   constructed (e.g., Huffman, arithmetic coding). This theorem is a
 *   mathematical mountain — an irreducible logical consequence of probability
 *   theory and the pigeonhole principle. It exhibits zero degrees of freedom
 *   across all measurement methodologies: whether encoding binary strings,
 *   natural language text, images, or abstract data sources, the entropy
 *   limit is invariant. The constraint does not emerge from institutional
 *   design, physical hardware limitations, or policy choices; it emerges from
 *   the mathematical structure of information itself. All agents — data
 *   compressors, communication engineers, data centers optimizing storage,
 *   adversaries trying to evade the limit — encounter the same immutable
 *   boundary. The theorem's resistance value (0.08) captures the small gap
 *   between asymptotic theoretical achievability and finite-block practical
 *   performance; this gap closes as engineers employ more sophisticated
 *   algorithms and longer block lengths. The theater ratio (0.15) reflects
 *   minor pedagogical performance: explaining the theorem involves proof
 *   machinery (pigeonhole principle, Jensen's inequality) that can obscure
 *   the underlying insight, but this is pedagogical theater, not operational
 *   theater.
 *
 * KEY AGENTS:
 *   - Data Source: Passive entity (powerless/trapped) — all sources are subject to the entropy limit regardless of their statistical properties
 *   - Communications Engineer: Active optimizer (powerful/analytical) — can approach the limit but cannot exceed it; ingenuity is applied to narrowing the gap, not escaping the bound
 *   - Analytical Observer: Detached analyst (analytical/analytical) — observes the theorem as a mathematical fact with zero degrees of freedom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shannons_source_coding_theorem, 0.08).
domain_priors:suppression_score(shannons_source_coding_theorem, 0.02).
domain_priors:theater_ratio(shannons_source_coding_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shannons_source_coding_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(shannons_source_coding_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(shannons_source_coding_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shannons_source_coding_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(shannons_source_coding_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shannons_source_coding_theorem, mountain).
narrative_ontology:human_readable(shannons_source_coding_theorem, "Shannon's Source Coding Theorem (Achievable Compression Limit)").
narrative_ontology:topic_domain(shannons_source_coding_theorem, "information_theory/technological").

domain_priors:emerges_naturally(shannons_source_coding_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SOURCE (MOUNTAIN) — Any source exhibiting statistical regularities is subject to Shannon's limit. No exit, no escape, no alternative encoding scheme can exceed the entropy bound. The compression ceiling is invariant across all measurement methodologies and encoding approaches. This is not a constraint imposed by human choice but a structural feature of information itself.
constraint_indexing:constraint_classification(shannons_source_coding_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMMUNICATIONS ENGINEER (MOUNTAIN) — Can approach the Shannon limit with Huffman codes, arithmetic coding, or modern entropy coding schemes, but cannot exceed it regardless of ingenuity or computational resources. The limit is not a barrier to be overcome but a law to be approached asymptotically. Resistance to the constraint is zero — engineers can get arbitrarily close but never surpass.
constraint_indexing:constraint_classification(shannons_source_coding_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (MOUNTAIN) — From the mathematical perspective, Shannon's theorem is a proven lower bound on the expected codeword length for any lossless compression of a source with entropy H. The theorem derives from the pigeonhole principle and basic probability — it is a logical necessity, not a contingent physical law. Zero degrees of freedom. No observational dependence. The compression limit is a theorem, not a measurement artifact.
constraint_indexing:constraint_classification(shannons_source_coding_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shannons_source_coding_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(shannons_source_coding_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shannons_source_coding_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(shannons_source_coding_theorem, ExtMetricName, E),
    domain_priors:suppression_score(shannons_source_coding_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(shannons_source_coding_theorem),
    narrative_ontology:constraint_metric(shannons_source_coding_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(shannons_source_coding_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(shannons_source_coding_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): The theorem has negligible extractiveness because it is a pure lower bound with no coercive content. No agent extracts value at another's expense; the constraint applies uniformly. The small non-zero value reflects that the gap between theoretical limit and practical algorithm performance can be exploited by sophisticated designers (they extract efficiency gains that naive compressors leave on the table), but this is benefit capture, not extraction — both the designer and the data source benefit from better compression. Suppression (0.02): Minimal. Engineers have complete freedom to develop new algorithms, implement existing ones, or trade compression ratio for speed. The only 'suppression' is that no amount of effort can exceed the entropy bound — but this is logical necessity, not coercive suppression. Theater ratio (0.15): Low. The theorem's statement is relatively direct: H(X) is the compression floor. Proof exposition involves some machinery (Jensen's inequality, the Kraft inequality), but once the theorem is stated, its content is clear. Unlike Pitons (where the theater hides functional decay), the theater here is purely pedagogical, not masking loss of function.
 *
 * PERSPECTIVAL GAP:
 *   No meaningful perspectival gap exists for this constraint — it is a uniform-type mountain. All agents (powerless sources, powerful engineers, analytical observers) see the same constraint. The theorem's classification is invariant across all contexts because it is a logical/mathematical truth, not a contingent institutional arrangement. The gap measurement here is zero: the analytical observer's view and the engineer's practical experience align perfectly. The only variation is in how quickly different agents approach the limit (sophisticated Huffman vs naive run-length encoding), but they all ultimately face the same boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is undefined for this constraint in the traditional sense. There is no beneficiary/victim distinction because the constraint is not extractive. The entropy bound applies equally to all data sources and all encoding agents. If anything, the constraint is collectively beneficial — it provides a target that engineers can aim for and legitimizes the search for ever-better algorithms. There is no directional flow of resources or power; the constraint is a shared property of information that all actors navigate together.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    finite_vs_asymptotic,
    'Does the theorem''s asymptotic achievability (limit as block length approaches infinity) constitute a practical constraint on finite-length compression tasks?',
    'Empirical analysis of gap between theoretical limit and practical performance for finite block lengths (N=10, 100, 1000 symbols); comparison of Huffman vs arithmetic coding for various source distributions',
    'If gap is small for practical N: theorem is effectively tight even in real systems. If gap is large: the theorem''s constraint is more aspirational than operational for finite sources.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(finite_vs_asymptotic, empirical, 'Whether asymptotic achievability applies to finite-length compression').

omega_variable(
    entropy_estimation_precision,
    'How much estimation error in source entropy (due to finite sample size or model mismatch) propagates to compression performance?',
    'Monte Carlo estimation of entropy from finite samples; comparison of theoretical H vs estimated H-hat; impact on compression ratio for mismatched models',
    'High error tolerance: theorem remains operative even with rough entropy estimates. Low tolerance: entropy must be precisely known, limiting practical applicability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entropy_estimation_precision, empirical, 'Sensitivity of compression performance to entropy estimation error').

omega_variable(
    lossy_vs_lossless_boundary,
    'Does the theorem constrain lossy compression or only lossless? At what distortion threshold does lossy information theory take over?',
    'Rate-distortion analysis; comparison of source coding theorem with rate-distortion theorem predictions across distortion budgets',
    'If boundary is sharp: two distinct constraint types (mountains). If fuzzy: single constraint with distortion parameter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lossy_vs_lossless_boundary, conceptual, 'Scope boundary between lossless and lossy compression domains').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shannons_source_coding_theorem, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shannon_tr_t0, shannons_source_coding_theorem, theater_ratio, 0, 0.15).
narrative_ontology:measurement(shannon_tr_t50, shannons_source_coding_theorem, theater_ratio, 50, 0.15).
narrative_ontology:measurement(shannon_tr_t100, shannons_source_coding_theorem, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(shannon_be_t0, shannons_source_coding_theorem, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(shannon_be_t50, shannons_source_coding_theorem, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(shannon_be_t100, shannons_source_coding_theorem, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shannons_source_coding_theorem, information_standard).
narrative_ontology:affects_constraint(shannons_source_coding_theorem, kraft_inequality).
narrative_ontology:affects_constraint(shannons_source_coding_theorem, huffman_optimality).
narrative_ontology:affects_constraint(shannons_source_coding_theorem, lossless_compression_limits).

% DUAL FORMULATION NOTE:
% Shannon's Source Coding Theorem is an upstream constraint that directly implies Kraft's Inequality (the necessary condition for codeword length distributions) and Huffman optimality (the constructive algorithm for achieving near-Shannon performance). It also bounds the feasible region for Lossy Compression Limits via Rate-Distortion Theory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
