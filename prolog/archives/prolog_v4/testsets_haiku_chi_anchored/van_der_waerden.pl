% ============================================================================
% CONSTRAINT STORY: van_der_waerden
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_van_der_waerden, []).

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
 *   constraint_id: van_der_waerden
 *   human_readable: Van der Waerden's Theorem on Arithmetic Progressions
 *   domain: mathematical/combinatorics/ramsey_theory
 *
 * SUMMARY:
 *   Van der Waerden's theorem (1927) is a cornerstone of Ramsey theory
 *   asserting that for any finite number of colors and any desired length of
 *   arithmetic progression, there exists a threshold N such that any coloring
 *   of the integers {1, 2, ..., N} with the given colors must contain a
 *   monochromatic arithmetic progression of the desired length. This theorem
 *   is a prototypical mountain constraint: it expresses an unavoidable
 *   structural property of finite partitions of infinite sets. No agent can
 *   negotiate with the constraint, no institutional arrangement can bypass
 *   it, and the theorem's truth does not depend on observables, measurement
 *   basis, or perspective. The constraint exhibits zero degrees of freedom
 *   across all indices. Its accessibility collapse (0.92) reflects that the
 *   theorem has a formal, published proof accessible to trained
 *   mathematicians; its resistance (0.08) reflects universal acceptance among
 *   mathematicians and no credible alternative proposals. The theater ratio
 *   (0.15) is low because the theorem's verification is algorithmic and
 *   transparent — there is minimal performative content in its statement or
 *   proof verification.
 *
 * KEY AGENTS:
 *   - The Finite Colorist: Any entity attempting to partition integers into color classes experiences this as an inescapable constraint (powerless/analytical)
 *   - The Combinatorial Analyst: Mathematician studying Ramsey theory and viewing the theorem as a logical consequence (analytical/analytical)
 *   - The Mathematical Community: Collective that treats the theorem as a fixed truth (institutional/analytical)
 *   - Ramsey Theory Research Community: Researchers for whom this is a boundary condition on problem feasibility (organized/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(van_der_waerden, 0.08).
domain_priors:suppression_score(van_der_waerden, 0.02).
domain_priors:theater_ratio(van_der_waerden, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(van_der_waerden, extractiveness, 0.08).
narrative_ontology:constraint_metric(van_der_waerden, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(van_der_waerden, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(van_der_waerden, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(van_der_waerden, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(van_der_waerden, mountain).
narrative_ontology:human_readable(van_der_waerden, "Van der Waerden's Theorem on Arithmetic Progressions").
narrative_ontology:topic_domain(van_der_waerden, "mathematical/combinatorics/ramsey_theory").

domain_priors:emerges_naturally(van_der_waerden).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FINITE COLORIST (MOUNTAIN) — Any agent attempting to partition the positive integers into finitely many color classes cannot avoid monochromatic arithmetic progressions. This is not a rule imposed by convention or enforcement; it is an unavoidable structural property of finite partitions of infinite sets. No exit exists; the constraint is invariant across all coloring schemes. d=0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(van_der_waerden, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE COMBINATORIAL ANALYST (MOUNTAIN) — Van der Waerden's theorem is a logical consequence of the pigeonhole principle applied recursively to arithmetic structure. From the perspective of mathematical logic, the constraint is a consequence of the rules of combinatorial reasoning itself. The theorem's inevitability follows from axioms; it cannot be negotiated or bypassed. The accessibility of this truth is extremely high (formal proof exists); resistance to acceptance is minimal (all mathematicians agree). d=0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(van_der_waerden, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE MATHEMATICAL COMMUNITY (MOUNTAIN) — Mathematics as a collective institution treats Van der Waerden's theorem as a fixed truth with zero degrees of freedom. No mathematical society, no funding body, no institutional arrangement can alter the theorem's truth. The constraint appears to the mathematical establishment as a natural law of the logical structure they study. d=0.5, f(d)≈0.65, σ=1.0 → χ≈0.05.
constraint_indexing:constraint_classification(van_der_waerden, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: RAMSEY THEORY RESEARCH COMMUNITY (MOUNTAIN) — Research groups working on Ramsey theory view Van der Waerden's theorem as a foundational boundary condition. The theorem constrains which research programs are feasible (those consistent with the theorem) and which are not (those contradicting it). This constraint is not enforced externally; it is intrinsic to the problem space. d=0.5, f(d)≈0.65, σ=1.0 → χ≈0.05.
constraint_indexing:constraint_classification(van_der_waerden, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(van_der_waerden_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(van_der_waerden, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(van_der_waerden, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(van_der_waerden, ExtMetricName, E),
    domain_priors:suppression_score(van_der_waerden, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(van_der_waerden),
    narrative_ontology:constraint_metric(van_der_waerden, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(van_der_waerden, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(van_der_waerden_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.08): Minimal. The theorem does not extract resources, advantage, or asymmetric value from any agent. It is a statement of fact about combinatorial structure with no preferential distribution. Base extraction is near zero because there is no beneficiary-victim asymmetry. Suppression (0.02): Minimal. The theorem imposes a constraint without coercion or suppression of alternatives — it is simply true that monochromatic arithmetic progressions must exist. Agents are not suppressed from trying to avoid them; they are informed that avoidance is impossible. Theater ratio (0.15): Low. The theorem's statement is transparent, its proof is verifiable, and its application is algorithmic. There is minimal performative content. The small nonzero value reflects the pedagogical framing sometimes used (Ramsey theory is often presented as surprising, capturing attention through paradox of inevitability), but the core constraint is non-theatrical.
 *
 * PERSPECTIVAL GAP:
 *   Van der Waerden's theorem exhibits no perspectival gap. All four perspectives classify it as Mountain, reflecting the theorem's invariance across all observables and measurement contexts. This is the defining property of a natural law in mathematics: no observer can negotiate a different relationship to the constraint. The powerless colorist, the analytical mathematician, the institutional community, and the research organization all experience the same structural inevitability. The absence of perspectival gap is itself evidence that the classification is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Mathematical theorems do not have beneficiaries or victims in the structural sense. The theorem is not an arrangement that extracts from some agents and benefits others. Instead, all agents in the mathematical domain are equally subject to the theorem's truth. Directionality for a universal mountain is symmetric: every perspective derives d≈0.5-0.72 (depending on power atom), and all converge on mountain classification with χ near 0.05-0.09. No override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   Van der Waerden's theorem exemplifies the resolution of mandatrophy in the mathematical domain. The apparent tension — is this a universal law or a contingent institutional fact? — is resolved by noting that the theorem is indeed universal and necessary within any logical system recognizing combinatorial structure. It is not contingent on social arrangement, measurement choice, or institutional preference. The theorem cannot be misclassified as a Snare (pure extraction) because there is no extraction. It cannot be misclassified as a Scaffold (temporary support) because there is no sunset. It cannot be misclassified as a Rope (coordination) because it is not solving a collective action problem — it is stating an invariant fact. The mandatrophy is fully resolved by the theorem's logical structure: all six constraint types are ruled out except Mountain by the mathematical properties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructive_bound_computability,
    'Can the Van der Waerden number W(k,r) be computed constructively for all finite k and r, or do some instances require non-constructive existence arguments?',
    'Development of explicit algorithms for computing W(k,r) bounds; analysis of proof structure in the Erdős-Ko-Rado framework and subsequent improvements',
    'If fully constructive: the mountain classification is reinforced — the theorem produces computable guarantees. If inherently non-constructive for some (k,r): the theorem remains mountain but with epistemological interest in the gap between existence and construction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constructive_bound_computability, empirical, 'Whether all Van der Waerden numbers are constructively computable').

omega_variable(
    algorithmic_lower_bounds,
    'What is the tightest known lower bound on W(k,r) and how does it compare to the theoretical existence guarantee?',
    'Computational surveys of colorings; analysis of explicit constructions achieving maximal monochromatic-free lengths; SAT solver verification of bounds',
    'Large gaps between lower and upper bounds indicate the theorem''s existence guarantee is far weaker than the most efficient possible coloring — the theorem''s force is structural (inevitability) rather than quantitative precision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_lower_bounds, empirical, 'Tightness of known Van der Waerden number bounds').

omega_variable(
    axiom_independence_variants,
    'Are there consistent variants of arithmetic (e.g., intuitionistic or constructive set theory) in which Van der Waerden''s theorem holds with a different statement or quantification structure?',
    'Proof-theoretic analysis of the theorem in different foundational systems (classical ZFC, intuitionistic ZF, constructive type theory); comparison of statement strength',
    'If the theorem''s structure changes across logical systems: the mountain is relative to classical logic (not universal). If invariant: the mountain classification is strengthened (true across all reasonable foundations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_independence_variants, conceptual, 'Logical independence of Van der Waerden''s theorem across foundational systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(van_der_waerden, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vdw_tr_t0, van_der_waerden, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vdw_tr_t50, van_der_waerden, theater_ratio, 50, 0.15).
narrative_ontology:measurement(vdw_tr_t100, van_der_waerden, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(vdw_be_t0, van_der_waerden, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(vdw_be_t50, van_der_waerden, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(vdw_be_t100, van_der_waerden, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(van_der_waerden, information_standard).
narrative_ontology:affects_constraint(van_der_waerden, ramsey_number_computation).
narrative_ontology:affects_constraint(van_der_waerden, pigeonhole_principle_finite_versions).
narrative_ontology:affects_constraint(van_der_waerden, hales_jewett_theorem).

% DUAL FORMULATION NOTE:
% Van der Waerden's theorem is part of a family of Ramsey-theoretic results sharing identical mountain structure. The theorem influences weaker results (specific Van der Waerden numbers for small k,r) and generalizations (Hales-Jewett, multidimensional Van der Waerden). All share ε≈0.08 and mountain classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
