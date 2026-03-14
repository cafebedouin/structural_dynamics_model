% ============================================================================
% CONSTRAINT STORY: huffman_optimality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_huffman_optimality, []).

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
 *   constraint_id: huffman_optimality
 *   human_readable: Huffman Optimality Theorem
 *   domain: information_theory/computer_science
 *
 * SUMMARY:
 *   Huffman's algorithm, published in 1952, yields a prefix-free binary code
 *   that minimizes expected codeword length for any probability distribution.
 *   The theorem is one of the clearest examples of a natural law in
 *   information science: no algorithm, no matter how clever or resource-rich,
 *   can produce a prefix-free code with lower average length than the Huffman
 *   code for a given set of symbol frequencies. The optimality is absolute,
 *   universal, and indifferent to the observer's perspective or agency. It is
 *   a structural ceiling, not a negotiable boundary.
 *
 * KEY AGENTS:
 *   - Any Prefix-Free Code: Target (universal/trapped) — must satisfy the optimality bound; cannot escape through any encoding scheme
 *   - The Huffman Code: Benchmarks (analytical/analytical) — defines the immutable optimality frontier
 *   - Systems Designers: Powerful agents (powerful/mobile) — even with maximal agency and resources cannot exceed the bound
 *   - The Information Science Community: Institutional observer (institutional/arbitrage) — benefits from the stability and universality of the theorem; no extractive position available
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(huffman_optimality, 0.08).
domain_priors:suppression_score(huffman_optimality, 0.02).
domain_priors:theater_ratio(huffman_optimality, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(huffman_optimality, extractiveness, 0.08).
narrative_ontology:constraint_metric(huffman_optimality, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(huffman_optimality, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(huffman_optimality, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(huffman_optimality, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(huffman_optimality, mountain).
narrative_ontology:human_readable(huffman_optimality, "Huffman Optimality Theorem").
narrative_ontology:topic_domain(huffman_optimality, "information_theory/computer_science").

domain_priors:emerges_naturally(huffman_optimality).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPTIMAL PREFIX CODE — Cannot escape the optimality bound. Any prefix-free code attempting to achieve average codeword length shorter than the Huffman code for a given probability distribution will necessarily violate the instantaneous decodability constraint or fail to preserve the probability ordering. The optimality is inescapable — mathematically determined.
constraint_indexing:constraint_classification(huffman_optimality, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: INFORMATION THEORIST — Huffman codes minimize expected length among all uniquely decodable codes for any probability distribution. This is not contingent on implementation, application, or measurement basis. The theorem holds across all observable framings of code design. The optimality persists regardless of whether the coder is aware of it or whether the probability distribution is discrete or continuous (in the limit).
constraint_indexing:constraint_classification(huffman_optimality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: SYSTEMS DESIGNER — Even with full agency, resources, and alternative encoding schemes available, cannot achieve better expected compression than Huffman for any fixed probability distribution. The optimality bound is indifferent to power level or implementation flexibility. The constraint is equally immutable from a position of maximum agency.
constraint_indexing:constraint_classification(huffman_optimality, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 4: DATA COMPRESSION INDUSTRY — Huffman optimality means no arbitrage opportunity exists in prefix-code design for fixed distributions. All implementations converge toward the same optimality bound. Competition cannot create advantage; institutional power cannot escape mathematical constraints. The industry benefits from the theorem's stability, not from any extractive position.
constraint_indexing:constraint_classification(huffman_optimality, mountain,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(huffman_optimality_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(huffman_optimality, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(huffman_optimality, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(huffman_optimality, ExtMetricName, E),
    domain_priors:suppression_score(huffman_optimality, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(huffman_optimality),
    narrative_ontology:constraint_metric(huffman_optimality, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(huffman_optimality, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(huffman_optimality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. Huffman optimality imposes no extraction from any party; it is a pure structural ceiling. The constraint arises from the mathematics of prefix-free codes and Shannon entropy, not from any agent's agenda. Base extraction is near-zero because there is no beneficiary asymmetrically exploiting anyone. Suppression (0.02): Negligible. The only 'suppression' is the mathematical fact that codes cannot beat the bound — this is not suppression of alternatives but elimination of false possibilities. Theater ratio (0.15): Very low. Huffman codes have transparent functional content; verification is straightforward algorithmic implementation. The small theater component reflects only the minimal gap between the theoretical optimality guarantee and practical implementation details (tie-breaking, symbol ordering). Accessibility collapse (0.92): Very high. The optimality bound cannot be circumvented through any clever measurement, reformulation, or extended context. The constraint is equally inaccessible from every direction. Resistance (0.05): Minimal. There is no meaningful resistance to the constraint — the constraint is not imposed by anyone or any group, so resistance would mean rejecting mathematical fact itself.
 *
 * PERSPECTIVAL GAP:
 *   This is a uniform-type mountain constraint. All perspectives produce Mountain classification. The gap across perspectives is not in classification type but in the meaning of 'mountain' from each viewpoint. The powerless agent sees an absolute immovable law. The analytical observer sees the same law revealed through proof. The powerful agent sees an inviolable limit on their agency. The institutional observer sees stable, predictable behavior. There is no perspectival disagreement about the type — all perspectives agree this is a natural law. The disagreement would be about whether the constraint *feels* like limitation (from the trapped agent's view) vs elegant structure (from the analytical view) vs boundary condition (from the systems designer's view). But structurally, all are recognizing the same immutable fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to mountain-only constraints. There is no extraction flow, no beneficiary-victim relationship, and no asymmetric power dynamics. The constraint is equally binding on all agents regardless of their structural position. All agents experience d approaching 1.0 (maximum immutability) because no agent has the power to escape the constraint. However, this d-value does not translate to high extractiveness — it reflects that the constraint is inescapable, not that it extracts. The chi formula produces near-zero χ because ε is near-zero, dominating any f(d) factor.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    probability_distribution_specification,
    'Does the optimality constraint change if the probability distribution is unknown or dynamic?',
    'Formal analysis of adaptive Huffman codes and universal coding theorems (Lempel-Ziv); comparison of optimal expected length for known vs unknown distributions',
    'If distributions are truly fixed and known: mountain classification holds absolutely. If distributions are unknown or adaptive: the constraint shifts to a generalized form (Gallager/Shannon coding bounds) with different optimality characteristics. The core mountain persists but its scope narrows.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(probability_distribution_specification, conceptual, 'Scope of optimality under unknown or dynamic probability distributions').

omega_variable(
    continuous_versus_discrete_formulation,
    'Does Huffman optimality extend to continuous-alphabet sources, or is it fundamentally a discrete constraint?',
    'Analysis of Shannon source coding theorem and uniform quantization; whether the discrete Huffman result is a special case of continuous information theory or a structurally distinct constraint',
    'If purely discrete: mountain classification is sharp. If generalizable to continuous: the constraint is a special case of a broader natural law. Either way, the classification remains mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuous_versus_discrete_formulation, conceptual, 'Whether Huffman optimality generalizes to continuous alphabets').

omega_variable(
    non_prefix_free_codes,
    'Is the restriction to prefix-free (uniquely decodable) codes essential to the optimality claim, or can non-prefix codes achieve the same average length?',
    'Formal comparison of Huffman codes with optimal uniquely decodable codes and optimal variable-length codes without the prefix-free constraint; proof that no code class can asymptotically beat Huffman',
    'If prefix-free restriction is binding: optimality is specific to that constraint class. If true across all decodable codes: mountain classification is even more robust. The Kraft inequality suggests the former, but verification confirms the logical structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_prefix_free_codes, empirical, 'Whether optimality is specific to prefix-free codes or generalizes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(huffman_optimality, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huff_tr_t0, huffman_optimality, theater_ratio, 0, 0.15).
narrative_ontology:measurement(huff_tr_t5, huffman_optimality, theater_ratio, 5, 0.15).
narrative_ontology:measurement(huff_tr_t10, huffman_optimality, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(huff_be_t0, huffman_optimality, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(huff_be_t5, huffman_optimality, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(huff_be_t10, huffman_optimality, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(huffman_optimality, information_standard).
narrative_ontology:affects_constraint(huffman_optimality, shannon_source_coding_theorem).
narrative_ontology:affects_constraint(huffman_optimality, kraft_inequality).

% DUAL FORMULATION NOTE:
% Huffman optimality is a special case (achievable lower bound) of the Shannon source coding theorem, which establishes the information-theoretic limit. Kraft's inequality provides the necessary and sufficient condition for the existence of prefix-free codes. All three constraints are linked: Kraft inequality defines feasibility space → Huffman algorithm achieves Shannon's bound in that space. Huffman is the concrete algorithmic instantiation of the abstract theorem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
