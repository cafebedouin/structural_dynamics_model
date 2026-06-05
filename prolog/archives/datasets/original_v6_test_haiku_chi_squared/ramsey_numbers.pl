% ============================================================================
% CONSTRAINT STORY: ramsey_numbers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ramsey_numbers, []).

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
 *   constraint_id: ramsey_numbers
 *   human_readable: Inevitability of Order (Ramsey's Theorem)
 *   domain: mathematical/combinatorics
 *
 * SUMMARY:
 *   Ramsey's Theorem is a foundational result in combinatorics: for any
 *   finite number of colors and any finite target substructure size, there
 *   exists a threshold n such that any coloring of the complete graph K_n (or
 *   any similar combinatorial object) must contain a large monochromatic
 *   substructure. The theorem is pure necessity — no coercion, no
 *   institutional enforcement, no beneficiary-victim dynamics. It emerges
 *   from the logical structure of finite partition spaces. The constraint
 *   exhibits the canonical signature of a Mountain: base extractiveness ≤
 *   0.08 (the theorem extracts nothing from agents — it is not a mechanism of
 *   power, but a limit on structure), suppression ≤ 0.02 (no coercion is
 *   needed; the structure is inevitable), theater ratio ≤ 0.05 (the theorem
 *   is not performative — it makes a substantive claim about combinatorial
 *   necessity), accessibility collapse ≥ 0.92 (agents cannot invent around
 *   Ramsey bounds; the constraint is universal and inescapable), and
 *   emerges_naturally = true (no human institution, law, or convention
 *   created this constraint; it follows from the axioms of mathematics). All
 *   perspectives classify identically as Mountain because Ramsey's theorem is
 *   invariant across all observables, all contexts, and all measurement
 *   methodologies.
 *
 * KEY AGENTS:
 *   - The Finite Colorist: Any agent attempting to partition elements into classes without generating large uniform substructure — subject to inevitable constraint
 *   - Applied Mathematicians and Computer Scientists: Agents seeking to avoid Ramsey-type phenomena in networks, algorithms, and data structures — constrained by absolute bounds
 *   - The Mathematical Observer: The civilizational perspective recognizing the theorem as a proven fact of formal logic — sees pure necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ramsey_numbers, 0.08).
domain_priors:suppression_score(ramsey_numbers, 0.02).
domain_priors:theater_ratio(ramsey_numbers, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ramsey_numbers, extractiveness, 0.08).
narrative_ontology:constraint_metric(ramsey_numbers, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(ramsey_numbers, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ramsey_numbers, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(ramsey_numbers, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ramsey_numbers, mountain).
narrative_ontology:human_readable(ramsey_numbers, "Inevitability of Order (Ramsey's Theorem)").
narrative_ontology:topic_domain(ramsey_numbers, "mathematical/combinatorics").

domain_priors:emerges_naturally(ramsey_numbers).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FINITE COLORIST (MOUNTAIN) — Any agent attempting to partition a sufficiently large finite set into finitely many classes without generating large monochromatic substructure fails by necessity. There is no exit, no negotiation, no enforcement needed. The constraint emerges from logical necessity alone. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.11 (but classification is mountain regardless of χ due to ε≤0.25, suppression≤0.05, accessibility_collapse≥0.85, emerges_naturally=true).
constraint_indexing:constraint_classification(ramsey_numbers, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE MATHEMATICAL OBSERVER (MOUNTAIN) — From the vantage of formal logic and set theory, Ramsey's theorem is a provable consequence of the axioms of mathematics. Its necessity follows from ZFC + logic alone. No coercion exists; no beneficiary-victim relation applies. The constraint is a natural law of combinatorial structure. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(ramsey_numbers, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE APPLIED MATHEMATICIAN (MOUNTAIN) — Whether in network analysis, computer science, or statistics, Ramsey numbers represent an absolute ceiling on the ability to avoid large uniform structures. This is not a policy constraint — it is an immutable property of discrete systems. Accessibility collapse (0.92): agents cannot invent their way around Ramsey bounds; resistance (0.03): no meaningful opposition to the theorem's conclusions. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(ramsey_numbers, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ramsey_numbers_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ramsey_numbers, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ramsey_numbers, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ramsey_numbers, ExtMetricName, E),
    domain_priors:suppression_score(ramsey_numbers, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ramsey_numbers),
    narrative_ontology:constraint_metric(ramsey_numbers, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ramsey_numbers, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ramsey_numbers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. Ramsey's theorem does not extract resources, advantage, or power from any agent. It is a constraint on the structure of finite partitions, not a mechanism of advantage transfer. The small non-zero value (not 0.0) reflects that the theorem does impose a structural necessity that agents must account for in design — but this is not extraction in the sense of coercive advantage, only the burden of working within logical limits. Suppression (0.02): Negligible. No coercion is involved. The theorem is not enforced by any agency; it simply holds. The minimal value reflects that some agents might prefer to avoid the monochromatic substructure, but they have no mechanism to suppress or escape it — the constraint is inescapable by logical necessity alone, not by enforcement. Theater ratio (0.05): Negligible. The theorem makes a substantive, falsifiable claim: any sufficiently large partition of k-colored elements contains a monochromatic m-clique. This is testable, provable, and carries no performative content. The minimal non-zero value reflects only the pedagogical stage-setting required to present the theorem, not any performative function within the theorem itself. Accessibility collapse (0.92): Very high. The constraint is universal and cannot be negotiated, re-interpreted, or escaped. Every finite partition space is subject to Ramsey bounds. There is no exception, no bargaining, no exit. Resistance (0.03): Minimal. There is nothing to resist — the theorem is a fact, not an enforced policy. The small non-zero value reflects only that some mathematicians historically resisted accepting infinite Ramsey theory (requiring set-theoretic axioms), but finite Ramsey theory has universal acceptance.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in the finite Ramsey constraint. All observers — powerless finite colorists, analytical mathematicians, applied computational agents — classify it identically as Mountain. This uniformity is not a flaw in the classification system; it is the defining feature of natural law constraints. The theorem's necessity does not depend on the observer's position, power level, time horizon, or spatial scope. Whether you are a single-agent algorithm designer constrained to avoid monochromatic substructures, a network analyst studying large graphs, or a formal logician examining ZFC, Ramsey's theorem applies to you in exactly the same way. The invariance across all observables confirms the mountain classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Ramsey's theorem has no directionality in the traditional sense because it has no beneficiaries or victims. It is not a constraint imposed by one agent on another; it is a constraint imposed by the structure of logical space itself. The d-values derived for different perspectives (d≈0.95 for powerless, d≈0.72 for analytical, d≈0.65 for organized) measure the observational distance from the constraint's logical necessity, not the structural extraction or exploitation. In all cases, f(d) produces a multiplicative factor applied to ε=0.08, yielding small χ values (0.09-0.11) that are consistent with the mountain classification regardless of the index tuple. The theorem makes no distinction between agents: a colorist is subject to the same bounds as an algorithm, a human mathematician, or a superintelligent system. The constraint is perfectly impartial.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effective_computability_ramsey,
    'Are Ramsey numbers computable in finite time for all k and m, or do they become uncomputable beyond a certain threshold?',
    'Proof-theoretic analysis of the growth rate of Ramsey numbers; investigation of whether R(k,m) can always be computed within arithmetic hierarchy bounds',
    'If all Ramsey numbers are computable: the constraint remains Mountain (logically necessary and practically accessible). If some become uncomputable: the constraint stratifies into computable (Mountain) and non-computable (approaches epistemic limit). This does not change classification — both are mountains — but clarifies the practical knowledge boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effective_computability_ramsey, empirical, 'Computability of Ramsey numbers beyond small cases').

omega_variable(
    ramsey_independence_from_foundational_system,
    'Are all Ramsey-type results (finite and infinite) provable in ZFC, or do some require stronger axioms like large cardinals?',
    'Proof-theoretic comparison of ZFC provability vs. stronger systems; investigation of whether infinite Ramsey theorem (infinite colorings, infinite monochromatic sets) requires inaccessible cardinals or similar',
    'If all provable in ZFC: Ramsey constraint is Mountain in standard mathematics. If some require large cardinals: the infinite case may be contingent on foundational assumptions (moves toward Piton for some perspectives). This is a rare omega where the resolution affects the mountain classification itself, not just the mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ramsey_independence_from_foundational_system, conceptual, 'Dependence of Ramsey results on foundational axioms').

omega_variable(
    ramsey_randomness_and_algorithmic_depth,
    'Does the algorithmic incompressibility (Kolmogorov complexity) of avoiding Ramsey structure increase with system size, suggesting that Ramsey inevitability is a consequence of compressibility limits?',
    'Formal analysis of Kolmogorov complexity bounds on colorings that avoid monochromatic k-cliques; comparison with Shannon entropy and information-theoretic lower bounds',
    'If yes: Ramsey constraint emerges from a deeper principle (information-theoretic necessity). If no: Ramsey is a combinatorial fact independent of compressibility. Either way, classification remains Mountain — the impact is on the philosophical grounding, not the structural type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ramsey_randomness_and_algorithmic_depth, conceptual, 'Relationship between Ramsey inevitability and Kolmogorov complexity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ramsey_numbers, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ramsey_tr_t0, ramsey_numbers, theater_ratio, 0, 0.02).
narrative_ontology:measurement(ramsey_tr_t100, ramsey_numbers, theater_ratio, 100, 0.05).
narrative_ontology:measurement(ramsey_tr_t200, ramsey_numbers, theater_ratio, 200, 0.05).

% Extraction over time
narrative_ontology:measurement(ramsey_be_t0, ramsey_numbers, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(ramsey_be_t100, ramsey_numbers, base_extractiveness, 100, 0.08).
narrative_ontology:measurement(ramsey_be_t200, ramsey_numbers, base_extractiveness, 200, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ramsey_numbers, information_standard).
narrative_ontology:affects_constraint(ramsey_numbers, halting_problem).
narrative_ontology:affects_constraint(ramsey_numbers, pigeonhole_principle).

% DUAL FORMULATION NOTE:
% Ramsey's theorem is the upstream member of a constraint family in combinatorial necessity. The pigeonhole principle is a special case of Ramsey theory (R(2,2)=4 reduces to pigeonhole counting). The halting problem shares Ramsey's mountain classification: both are mathematical impossibility results rooted in logical limits, not institutional mechanisms. The network link reflects that all three constraints exemplify the same fundamental principle: sufficiently large/complex discrete systems cannot avoid certain global structures (pigeonhole), patterns (Ramsey), or unsolvability (halting). A system designed to avoid monochromatic Ramsey cliques will necessarily encounter halting-problem-like undecidability at sufficiently large scales.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
