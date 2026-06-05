% ============================================================================
% CONSTRAINT STORY: heine_borel_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_heine_borel_theorem, []).

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
 *   constraint_id: heine_borel_theorem
 *   human_readable: Heine-Borel Theorem
 *   domain: mathematics/real_analysis
 *
 * SUMMARY:
 *   The Heine-Borel theorem exemplifies a pure mathematical constraint: the
 *   equivalence of compactness and closed-boundedness in Euclidean space R^n.
 *   This theorem is a necessary logical truth derivable from the axioms of
 *   real analysis. It has zero degrees of freedom — the constraint emerges
 *   naturally from the definition of Euclidean metric topology and cannot be
 *   violated within consistent mathematics. The theorem imposes no coercive
 *   overlay, no suppression mechanism, and no theatrical performance. Its
 *   constraint is accessibility collapse: once you accept the axioms of real
 *   analysis, you must accept the theorem's conclusion. There is no
 *   alternative exit, no negotiation, no institutional maintenance required
 *   beyond the perpetuation of the mathematical framework itself. The
 *   theorem's extractiveness (0.06) and suppression (0.02) reflect that it
 *   creates no asymmetric benefit or coercion — it is equally constraining to
 *   all observers. The slight non-zero values arise only because the
 *   theorem's truth-value was discovered at a specific historical moment and
 *   now requires institutional transmission through education, creating
 *   minimal theater in the pedagogical process.
 *
 * KEY AGENTS:
 *   - Mathematical Reality: Primary agent (analytical/analytical) — the logical structure itself; neither beneficiary nor victim
 *   - Applied Mathematicians and Engineers: Secondary agents (moderate to powerful/trapped) — must work within the constraint but benefit from its necessity as a reliable feature of mathematical reality
 *   - Mathematics Education System: Tertiary agent (institutional/arbitrage) — transmits the theorem; benefits from its status as foundational knowledge; creates minimal theater
 *   - Proof Verification Process: Quaternary agent (analytical/analytical) — the constraint is verified or falsified through logical checking; no extraction occurs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(heine_borel_theorem, 0.06).
domain_priors:suppression_score(heine_borel_theorem, 0.02).
domain_priors:theater_ratio(heine_borel_theorem, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(heine_borel_theorem, extractiveness, 0.06).
narrative_ontology:constraint_metric(heine_borel_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(heine_borel_theorem, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(heine_borel_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(heine_borel_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(heine_borel_theorem, mountain).
narrative_ontology:human_readable(heine_borel_theorem, "Heine-Borel Theorem").
narrative_ontology:topic_domain(heine_borel_theorem, "mathematics/real_analysis").

domain_priors:emerges_naturally(heine_borel_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL REALIST (MOUNTAIN) — The equivalence of compactness and closed-boundedness in Euclidean space is a logical necessity, not a contingent fact. The theorem follows deductively from the axioms of real analysis and the topological definition of Euclidean metric spaces. No degree of freedom exists: the theorem is either true or false by necessity, not by convention or choice. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.04.
constraint_indexing:constraint_classification(heine_borel_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: APPLIED MATHEMATICIAN (MOUNTAIN) — Any attempt to build mathematical models of physical systems in Euclidean space is constrained by the Heine-Borel theorem. A set cannot be simultaneously closed and bounded without being compact. This is not a policy choice or institutional arrangement — it is a structural limit on what mathematical objects can exist. An engineer cannot 'negotiate' with compactness. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(heine_borel_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: INSTITUTIONAL MATHEMATICS (MOUNTAIN) — Universities and research institutions teach the Heine-Borel theorem as foundational knowledge not because they chose to extract value, but because it is a necessary element of real analysis. The constraint is self-enforcing: any institution that taught the negation of the theorem would be teaching falsehood. Institutional inertia is irrelevant; the theorem would be true regardless of whether any institution existed. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(heine_borel_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: GRADUATE STUDENT (MOUNTAIN) — A student learning real analysis has no choice but to internalize the Heine-Borel theorem. The proof is not negotiable; the logical structure is fixed. The student cannot opt out: mastering the theorem is a prerequisite for advance in mathematics, physics, or engineering. The constraint manifests as pure cognitive necessity. d≈0.90, f(d)≈1.35, σ=1.0 → χ≈0.08.
constraint_indexing:constraint_classification(heine_borel_theorem, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: PROOF VERIFICATION (MOUNTAIN) — At the granular level of checking whether a specific proof of Heine-Borel is valid, the constraint is a local logical necessity. The proof either contains valid steps or it does not. There is no extraction mechanism, no suppression, no alternative exit. The theorem's truth is a boundary condition of the logical space. d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.03.
constraint_indexing:constraint_classification(heine_borel_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(analytical),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(heine_borel_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(heine_borel_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(heine_borel_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(heine_borel_theorem, ExtMetricName, E),
    domain_priors:suppression_score(heine_borel_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(heine_borel_theorem),
    narrative_ontology:constraint_metric(heine_borel_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(heine_borel_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(heine_borel_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.06): Minimal. The Heine-Borel theorem creates no asymmetric benefit or extraction. All agents are equally constrained by the logical necessity of the equivalence. The tiny non-zero value (0.06 vs 0.00) reflects only the minimal overhead in transmitting the theorem through educational institutions — a negligible pedagogical burden, not extraction. Suppression (0.02): Minimal. No coercive mechanism enforces the theorem. It is not suppressed, but rather discovered and verified. The tiny value (0.02 vs 0.00) reflects only that students must learn the proof as a prerequisite, not that alternatives are coercively denied. Theater ratio (0.05): Minimal. Mathematical proof is nearly pure function: the proof either works or it does not. Pedagogical presentation adds minimal theater — examples, intuitive explanations, and proof scaffolding are aids to understanding, not performance. The constraint's validity is independent of how it is taught. Accessibility collapse (0.92): High. Once you accept the axioms of real analysis and Euclidean topology, the theorem follows with logical necessity. There is no accessible alternative — compactness cannot be decoupled from closed-boundedness in Euclidean space without logical contradiction. Resistance (0.08): Minimal. The theorem faces essentially zero resistance from within consistent mathematics. The small non-zero value (0.08 vs 0.00) reflects only that some alternative topologies (non-Hausdorff, non-metric) exist outside the Euclidean context, representing edges of the constraint's domain rather than true resistance.
 *
 * PERSPECTIVAL GAP:
 *   No perspectival gap exists for the Heine-Borel theorem. All perspectives classify it as mountain because the constraint is observer-independent. The applied mathematician, the student, the institutional system, and the analytical observer all see the same logical necessity. This uniform classification confirms the mountain diagnosis: natural laws produce invariant classifications across all structural positions. The minimal variation in χ across perspectives (ranging from -0.01 to 0.09) reflects only differences in spatial scope (universal vs local) affecting the scope modifier σ(S), not differences in how the constraint is experienced. The theorem does not appear as snare to one agent and rope to another — it appears as mountain to all.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents occupy the same structural position relative to the Heine-Borel theorem: they are observers of a logical necessity, not targets or beneficiaries of extraction. The mathematical realist and analytical observer have d≈0.50 (symmetric: they neither gain nor lose from the constraint; they simply recognize its truth). The applied mathematician and student have d≈0.90-0.95 (they are trapped by logical necessity, but this is not extraction because there is no beneficiary extracting value). The institutional mathematics community has d≈0.05 (they benefit from the theorem's status as foundational knowledge, but the benefit is not extraction; they gain from the reliability of the constraint, not from hiding it or suppressing alternatives). No beneficiary/victim distinction applies because the constraint is not asymmetric. The theorem benefits everyone equally by providing a stable logical anchor.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    non_euclidean_generalization,
    'Does the Heine-Borel theorem generalize to non-Euclidean metric spaces and topological spaces with the same logical necessity?',
    'Analysis of topological spaces where closed and bounded sets are not compact (e.g., Sorgenfrey line, Baire space). Identification of additional conditions required (completeness, separability, second-countability) in non-Euclidean contexts.',
    'If the theorem generalizes with necessity: mountain classification holds universally. If the generalization requires additional contingent assumptions: the Euclidean version might be contingent on Euclidean structure, not a universal law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_euclidean_generalization, empirical, 'Whether Heine-Borel''s logical necessity extends to non-Euclidean spaces').

omega_variable(
    axiom_dependence,
    'On which axioms of set theory and real analysis does the Heine-Borel theorem fundamentally depend? Could a consistent mathematical system negate it?',
    'Proof-theoretic analysis of the theorem''s derivation. Construction of alternative axiom systems (e.g., constructive mathematics, non-standard analysis) and examination of whether Heine-Borel holds, fails, or becomes undecidable.',
    'If Heine-Borel is provable in all consistent extensions: mountain classification confirmed (logical necessity). If there exist consistent systems where it is false or undecidable: the theorem is contingent on a choice of axioms, not a natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_dependence, conceptual, 'Whether Heine-Borel''s necessity depends on contingent axiom choices').

omega_variable(
    pragmatic_exception_utility,
    'Are there practical mathematical or physical contexts where treating closed-and-bounded sets as non-compact yields useful results despite violating the theorem?',
    'Survey of applied mathematics literature for cases where non-Hausdorff topologies, generalized metrics, or finite approximations are used to work around compactness requirements. Analysis of whether these are workarounds or genuine exceptions.',
    'If genuine exceptions exist and are widely useful: the theorem''s constraint is contingent on the Euclidean/Hausdorff context, not universal. If all exceptions are scaffolds built to ultimately satisfy the theorem: mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pragmatic_exception_utility, empirical, 'Whether practical mathematics ever benefits from violating Heine-Borel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(heine_borel_theorem, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hb_tr_t0, heine_borel_theorem, theater_ratio, 0, 0.03).
narrative_ontology:measurement(hb_tr_t100, heine_borel_theorem, theater_ratio, 100, 0.05).
narrative_ontology:measurement(hb_tr_t200, heine_borel_theorem, theater_ratio, 200, 0.05).

% Extraction over time
narrative_ontology:measurement(hb_be_t0, heine_borel_theorem, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(hb_be_t100, heine_borel_theorem, base_extractiveness, 100, 0.06).
narrative_ontology:measurement(hb_be_t200, heine_borel_theorem, base_extractiveness, 200, 0.06).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(heine_borel_theorem, information_standard).
narrative_ontology:affects_constraint(heine_borel_theorem, extreme_value_theorem).
narrative_ontology:affects_constraint(heine_borel_theorem, sequential_compactness_equivalence).
narrative_ontology:affects_constraint(heine_borel_theorem, uniform_continuity_compactness).

% DUAL FORMULATION NOTE:
% The Heine-Borel theorem is foundational to several downstream theorems in real analysis. The extreme value theorem (continuous functions on compact sets attain their maximum and minimum) depends on Heine-Borel. Sequential compactness equivalence (in metric spaces) relies on the same topological machinery. Uniform continuity on compact sets also depends on compactness properties that Heine-Borel establishes. These constraints are not alternative formulations of Heine-Borel but rather theorems that flow from it, creating a network of logical dependencies rather than a choice between interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
