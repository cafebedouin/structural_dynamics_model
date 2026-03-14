% ============================================================================
% CONSTRAINT STORY: bolzano_weierstrass_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bolzano_weierstrass_theorem, []).

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
 *   constraint_id: bolzano_weierstrass_theorem
 *   human_readable: Bolzano-Weierstrass Theorem: Compactness and Convergence
 *   domain: mathematical_analysis/topology
 *
 * SUMMARY:
 *   The Bolzano-Weierstrass theorem is a foundational result in real analysis
 *   stating that every bounded infinite sequence in Euclidean space ℝⁿ
 *   contains a convergent subsequence. This theorem emerges necessarily from
 *   the mathematical structure of compact sets in finite-dimensional
 *   Euclidean space, mediated by the Heine-Borel characterization (a set is
 *   compact if and only if it is closed and bounded). The constraint it
 *   instantiates is purely structural—mathematical, not physical or social.
 *   No agent experiences extraction or suppression in the conventional sense;
 *   rather, the bounded sequence is 'constrained' to contain a convergent
 *   subsequence in the same way a point in space is constrained by the laws
 *   of geometry. The theorem admits no observational ambiguity, no
 *   alternative measurement methodology, and no escape clause. It classifies
 *   uniformly as Mountain across all perspectives because compactness is an
 *   immutable property of the topology itself.
 *
 * KEY AGENTS:
 *   - Bounded Sequences: The 'subjects' of the constraint (universal/trapped) — every such sequence is necessarily structured to contain convergence
 *   - Topological Space: The institutional container (universal/analytical) — defines the constraint through its definition of compactness and closure
 *   - Mathematical Analyst: The observer (universal/analytical) — analyzes the constraint's logical necessity and generalization properties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bolzano_weierstrass_theorem, 0.08).
domain_priors:suppression_score(bolzano_weierstrass_theorem, 0.02).
domain_priors:theater_ratio(bolzano_weierstrass_theorem, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bolzano_weierstrass_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(bolzano_weierstrass_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(bolzano_weierstrass_theorem, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bolzano_weierstrass_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(bolzano_weierstrass_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bolzano_weierstrass_theorem, mountain).
narrative_ontology:human_readable(bolzano_weierstrass_theorem, "Bolzano-Weierstrass Theorem: Compactness and Convergence").
narrative_ontology:topic_domain(bolzano_weierstrass_theorem, "mathematical_analysis/topology").

domain_priors:emerges_naturally(bolzano_weierstrass_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A bounded infinite sequence in Euclidean space cannot escape convergence. Every bounded sequence must contain a convergent subsequence — this is not a choice or a constraint imposed externally, but a structural necessity of compactness. The sequence is 'trapped' in the topology itself.
constraint_indexing:constraint_classification(bolzano_weierstrass_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% From the perspective of topology as an institutional structure, the Bolzano-Weierstrass theorem is a foundational natural law of Euclidean geometry. No amount of institutional innovation or policy change can override it. It emerges necessarily from the definition of compactness and the Heine-Borel characterization.
constraint_indexing:constraint_classification(bolzano_weierstrass_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% From a civilizational analytical perspective, the Bolzano-Weierstrass theorem is an immutable structural principle of analysis. It holds in ℝⁿ by the Heine-Borel theorem (compact sets are closed and bounded). It generalizes to metric spaces and beyond. The constraint is purely mathematical — no observational ambiguity or measurement methodology can change its truth value.
constraint_indexing:constraint_classification(bolzano_weierstrass_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bolzano_weierstrass_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(bolzano_weierstrass_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bolzano_weierstrass_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(bolzano_weierstrass_theorem, ExtMetricName, E),
    domain_priors:suppression_score(bolzano_weierstrass_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(bolzano_weierstrass_theorem),
    narrative_ontology:constraint_metric(bolzano_weierstrass_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(bolzano_weierstrass_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(bolzano_weierstrass_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Extremely low. The theorem imposes no extraction in the conventional sense—it describes a structural property, not an asymmetric resource transfer. The value is minimal because the theorem is purely descriptive of mathematical structure. Suppression (0.02): Nearly zero. There are no alternatives that could be suppressed; the convergence is logically necessary given the definitions. Theater ratio (0.05): Minimal. The theorem's proof is transparent and non-performative. Once the definitions of compactness and boundedness are established, the convergence follows directly from the Heine-Borel characterization. No institutional theater is required to maintain the constraint—it is self-evident from first principles. Accessibility collapse (0.92): Very high. The constraint is inaccessible to any alternative formulation. No bounded sequence in Euclidean space can avoid containing a convergent subsequence. Resistance (0.08): Very low. There is no mechanism by which this constraint could be resisted or overcome. The theorem is a consequence of the metric topology's structure. Claimed type: Mountain. All three gates are satisfied: extractiveness ≤ 0.25, suppression ≤ 0.05, accessibility collapse ≥ 0.85, resistance ≤ 0.15, and emerges_naturally = true.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap in this constraint because the theorem is universal across all mathematical observers and all topological contexts (within its domain of applicability). All three perspectives—the bounded sequence, the topological framework, and the analytical observer—unanimously classify the constraint as Mountain. This uniform classification is diagnostic of a true natural law in mathematics: the constraint is not observer-dependent, measurement-dependent, or context-dependent. The only potential gap arises in the omega variables: different mathematical frameworks (constructive vs classical, different generalization domains) may change the framing, but they do not change the theorem's truth value within ℝⁿ.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is inapplicable to this constraint because there are no beneficiaries or victims in the structural sense. The theorem does not benefit one agent at the expense of another; it describes the necessary topology of compact sets. The constraint has no extraction flow, no power asymmetry, and no beneficial/harmful directionality. All agents (bounded sequences, topological frameworks, analysts) are subject to the same immutable constraint. If directionality were computed via the standard derivation chain (beneficiary/victim + exit → d), the constraint has neither beneficiaries nor victims, so d would default to the analytical canonical value (≈0.73), which would then produce an anomalously high f(d) for a purely structural theorem. This mismatch is correctly resolved by recognizing that mountains have no meaningful directionality—the constraint is symmetric and universal.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does not arise for this constraint because the theorem unambiguously instantiates a Mountain across all perspectives. There is no risk of mislabeling coordination as extraction, or vice versa. The theorem is not coordination (no agents are cooperating to solve a mutual problem), not extraction (no asymmetry), and not scaffolding (no sunset clause). It is simply a law of mathematics. The uniform Mountain classification is the resolution: mandatrophy is averted by the constraint's mathematical character. If any perspective had classified this as Rope or Tangled Rope, that would indicate a misunderstanding of the theorem's structure, and mandatrophy resolution would require clarification that the constraint is descriptive, not normative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generalization_domain,
    'Does the Bolzano-Weierstrass theorem hold in all metric spaces or only in finite-dimensional Euclidean spaces?',
    'Analysis of compactness definitions in general metric and topological spaces; examination of counterexamples in infinite-dimensional spaces',
    'The theorem''s scope is universal in finite dimensions but constrained to compact metric spaces in general settings. This is not an omega for the theorem''s truth, but for its domain of applicability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generalization_domain, empirical, 'Scope of the theorem across different topological spaces').

omega_variable(
    constructivity_requirement,
    'Can the convergent subsequence be constructed algorithmically, or does its existence require the axiom of choice?',
    'Examination of the proof structure; investigation of constructive vs classical mathematics formulations',
    'In classical mathematics, the theorem relies on the axiom of choice via the Bolzano property. In constructive mathematics, the theorem requires explicit convergence bounds. This is a metamathematical distinction, not a physical one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constructivity_requirement, conceptual, 'Constructivity and axiom of choice dependence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bolzano_weierstrass_theorem, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bw_tr_t0, bolzano_weierstrass_theorem, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bw_tr_t50, bolzano_weierstrass_theorem, theater_ratio, 50, 0.05).
narrative_ontology:measurement(bw_tr_t100, bolzano_weierstrass_theorem, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(bw_be_t0, bolzano_weierstrass_theorem, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(bw_be_t50, bolzano_weierstrass_theorem, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(bw_be_t100, bolzano_weierstrass_theorem, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bolzano_weierstrass_theorem, information_standard).
narrative_ontology:affects_constraint(bolzano_weierstrass_theorem, heine_borel_theorem).
narrative_ontology:affects_constraint(bolzano_weierstrass_theorem, compactness_in_metric_spaces).

% DUAL FORMULATION NOTE:
% The Bolzano-Weierstrass theorem is a downstream consequence of the Heine-Borel characterization of compactness in ℝⁿ. It also affects constraints on sequence convergence and compactness in infinite-dimensional spaces. These are linked via the topological generalization hierarchy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
