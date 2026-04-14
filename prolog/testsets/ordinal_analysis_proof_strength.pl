% ============================================================================
% CONSTRAINT STORY: ordinal_analysis_proof_strength
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ordinal_analysis_proof_strength, []).

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
 *   constraint_id: ordinal_analysis_proof_strength
 *   human_readable: Ordinal Analysis Proof Strength Hierarchy
 *   domain: mathematical_logic/proof_theory
 *
 * SUMMARY:
 *   Ordinal analysis proof strength is a natural-law constraint on the
 *   expressive power of formal systems. The ordinal hierarchy reflects a
 *   fundamental fact about formal logic: a system formalized at ordinal level
 *   α can prove theorems about all lower ordinals, but cannot access theorems
 *   whose proof necessarily requires ordinal level β > α. This constraint has
 *   no behavioral component — it is pure mathematical structure. The
 *   hierarchy of ordinal notations (ω, ω^ω, ε₀, Γ₀, Bachmann-Howard ordinal)
 *   directly indexes the strength of proof systems (first-order arithmetic,
 *   second-order arithmetic, Peano Arithmetic with transfinite induction,
 *   Kripke-Platek set theory, etc.). No agent can escape this ordering by
 *   choice, persuasion, or institutional arrangement. The constraint emerges
 *   from the self-referential structure of formal systems themselves — a
 *   system cannot contain a complete formal proof of its own consistency
 *   without exceeding its own ordinal bound (Gödel's incompleteness theorem).
 *   This makes the ordering irreducible and immutable.
 *
 * KEY AGENTS:
 *   - Proof Theorists: Analytical observers (analytical/analytical) — study the ordinal hierarchy as mathematical structure
 *   - Mathematical Community: Institutional custodians (powerful/mobile) — organize research and curriculum around proof-strength boundaries
 *   - Individual Mathematicians: Active agents (moderate/constrained) — attempt proofs within the constraints of available proof-theoretic tools
 *   - Formal Systems: Structural objects (analytical/analytical) — inherit their proof-theoretic strength from their ordinal classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ordinal_analysis_proof_strength, 0.18).
domain_priors:suppression_score(ordinal_analysis_proof_strength, 0.03).
domain_priors:theater_ratio(ordinal_analysis_proof_strength, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ordinal_analysis_proof_strength, extractiveness, 0.18).
narrative_ontology:constraint_metric(ordinal_analysis_proof_strength, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(ordinal_analysis_proof_strength, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ordinal_analysis_proof_strength, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(ordinal_analysis_proof_strength, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ordinal_analysis_proof_strength, mountain).
narrative_ontology:human_readable(ordinal_analysis_proof_strength, "Ordinal Analysis Proof Strength Hierarchy").
narrative_ontology:topic_domain(ordinal_analysis_proof_strength, "mathematical_logic/proof_theory").

domain_priors:emerges_naturally(ordinal_analysis_proof_strength).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROOF THEORIST (MOUNTAIN) — The hierarchy of proof strength indexed by ordinal notations reflects a fundamental structural feature of formal systems. No alternative framework eliminates ordinal analysis; the limitation is intrinsic to formal expressiveness. Higher ordinals prove stronger theorems about lower systems, but cannot be embedded within those systems without contradiction. This constraint is immutable across all formal contexts.
constraint_indexing:constraint_classification(ordinal_analysis_proof_strength, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL COMMUNITY (MOUNTAIN) — Within formal mathematics, ordinal analysis is simply how proof strength is ordered. No mathematician can escape this ordering — attempting to prove a theorem with resources below its ordinal bound will fail. The community's practices (peer review, textbook curricula, journal standards) all presuppose this ordering. The ordering cannot be circumvented by institutional choice or resource allocation.
constraint_indexing:constraint_classification(ordinal_analysis_proof_strength, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: INDIVIDUAL MATHEMATICIAN (MOUNTAIN) — From the career perspective of a working mathematician, ordinal proof strength is an immutable constraint on which theorems can be attacked with available techniques. A theorem requiring proof-theoretic strength ω^ω cannot be proven using first-order methods alone, regardless of effort, funding, or talent. The individual faces this constraint as a limit on achievable results within a given framework.
constraint_indexing:constraint_classification(ordinal_analysis_proof_strength, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: LOGICAL OBSERVER (MOUNTAIN) — From the perspective of formal semantics and model theory, the ordinal hierarchy is simply the natural stratification of definability and interpretability across formal systems. The hierarchy emerges from the mathematics of recursive ordinals and their properties. No observational frame changes this structure.
constraint_indexing:constraint_classification(ordinal_analysis_proof_strength, mountain,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ordinal_analysis_proof_strength_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(ordinal_analysis_proof_strength, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ordinal_analysis_proof_strength, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ordinal_analysis_proof_strength, ExtMetricName, E),
    domain_priors:suppression_score(ordinal_analysis_proof_strength, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ordinal_analysis_proof_strength),
    narrative_ontology:constraint_metric(ordinal_analysis_proof_strength, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ordinal_analysis_proof_strength, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ordinal_analysis_proof_strength_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Extremely low. The ordinal hierarchy does not extract value from any agent — it is a classification of proof strength, not an extraction mechanism. The value captures only the minimal formalization cost of representing ordinal bounds in formal reasoning. No agent is systematically disadvantaged by the ordering itself; rather, the ordering is constitutive of what 'proof strength' means. Suppression (0.03): Negligible. There are no barriers to understanding or using ordinal analysis — the constraint is fully transparent and uniformly accessible to all agents in the formal system. Any mathematician can study the ordinal hierarchy; understanding it improves rather than diminishes one's capacity. Theater ratio (0.05): Negligible. Ordinal analysis is purely functional — it describes exactly what it claims to describe with no performative component. The notation and terminology are dense but precise; there is no gap between the formal representation and the mathematical content. The small residual value (0.05) reflects only the necessary formalism overhead in any symbolic representation.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap — all contexts classify this constraint identically as a mountain. This is the canonical signature of a true natural law in mathematics. Whether viewed from the career perspective of an individual mathematician, the institutional perspective of the mathematical community, the immediate formal perspective of a logical system, or the civilizational perspective of proof theory as a discipline, the ordinal hierarchy presents itself as immutable. This uniformity across observatories confirms the mountain classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Ordinal analysis has no beneficiaries or victims because it is not an extraction mechanism. The constraint is purely structural — it defines the topology of provability space. All agents (mathematicians, formal systems, mathematical communities) experience the same constraint in the same way: as an invariant property of formal expressiveness. There is no asymmetry in who benefits or who bears costs, because the constraint does not redistribute anything. It simply defines the space of possible proofs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint has no mandatrophy because it is a mountain-only universal constraint. All perspectives produce mountain classification. There is no competing narrative that naturalizes contingent extraction as proof strength — the constraint is genuinely immutable. The ordinal hierarchy cannot be reinterpreted as coordination, temporary support, or degraded function. It is what it claims to be: the natural ordering of proof strength in formal systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ordinal_notation_choice,
    'Does the choice of ordinal notation system (Bachmann-Howard, Veblen, Rathjen) affect the fundamental strength hierarchy?',
    'Proof that all equivalent notation systems produce identical strength comparisons; demonstration that any notation system capable of measuring these ordinals produces the same ordering',
    'If notations are genuinely equivalent: the hierarchy is notation-invariant and the mountain classification is confirmed. If notations diverge: the hierarchy may be a contingent feature of notation choice, suggesting Piton rather than Mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ordinal_notation_choice, empirical, 'Whether ordinal notation choice affects the strength hierarchy').

omega_variable(
    intuitionistic_vs_classical_boundary,
    'Are theorems requiring classical logic fundamentally inaccessible to intuitionistic methods, or is this a limitation of current translation techniques?',
    'Formal proof of whether certain classical theorems admit constructive proofs via modified ordinal bounds; investigation of whether double-negation translation exhausts the relationship between classical and intuitionistic strength',
    'If classical-inaccessible: ordinal analysis reveals genuine proof-theoretic boundaries. If translation-resolvable: ordinal strength is context-relative rather than absolute, suggesting the mountain classification naturalizes a contingent framework choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intuitionistic_vs_classical_boundary, empirical, 'Whether classical-intuitionistic boundary is fundamental or technical').

omega_variable(
    large_cardinal_dependence,
    'Does the ordinal hierarchy depend on set-theoretic axioms (continuum hypothesis, large cardinals), or is it independent of foundational choices?',
    'Analysis of which theorems about ordinal analysis are provable in ZFC alone versus requiring large cardinal axioms; demonstration of the framework-invariance of the hierarchy',
    'If independent: ordinal analysis reveals universally structured proof strength. If dependent: the hierarchy is relative to foundational assumptions, and the ''naturalness'' of the ordering is contingent on axiom choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(large_cardinal_dependence, empirical, 'Whether ordinal hierarchy is independent of set-theoretic axioms').

omega_variable(
    proof_extraction_algorithmic_completeness,
    'Can every ordinal analysis at level α be algorithmically translated into a formal proof in system S(α) without loss of content?',
    'Formal proof of completeness of proof extraction from ordinal analysis; computational analysis of whether extracted proofs preserve all relevant structure from the ordinal-labeled argument',
    'If complete: ordinal analysis is a faithful representation of proof strength. If incomplete: some structure is lost in extraction, suggesting ordinal analysis captures something beyond formal provability (possibly its theater component).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proof_extraction_algorithmic_completeness, empirical, 'Whether proof extraction from ordinal analysis is algorithmic complete').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ordinal_analysis_proof_strength, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ordinal_tr_t0, ordinal_analysis_proof_strength, theater_ratio, 0, 0.04).
narrative_ontology:measurement(ordinal_tr_t50, ordinal_analysis_proof_strength, theater_ratio, 50, 0.05).
narrative_ontology:measurement(ordinal_tr_t100, ordinal_analysis_proof_strength, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(ordinal_be_t0, ordinal_analysis_proof_strength, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(ordinal_be_t50, ordinal_analysis_proof_strength, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(ordinal_be_t100, ordinal_analysis_proof_strength, base_extractiveness, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ordinal_analysis_proof_strength, information_standard).
narrative_ontology:affects_constraint(ordinal_analysis_proof_strength, incompleteness_godel_first).
narrative_ontology:affects_constraint(ordinal_analysis_proof_strength, incompleteness_godel_second).
narrative_ontology:affects_constraint(ordinal_analysis_proof_strength, halting_problem_undecidability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
