% ============================================================================
% CONSTRAINT STORY: rice_theorem_undecidability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rice_theorem_undecidability, []).

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
 *   constraint_id: rice_theorem_undecidability
 *   human_readable: Rice's Theorem: Undecidability of Semantic Program Properties
 *   domain: theoretical_computer_science/computability_theory
 *
 * SUMMARY:
 *   Rice's Theorem establishes that every non-trivial semantic property of
 *   programs (properties that depend on the behavior of the program when
 *   executed, not just its syntactic form) is undecidable — no algorithm can
 *   correctly determine whether an arbitrary program has that property for
 *   all possible inputs. This is a mathematical impossibility, not a
 *   engineering challenge or a coordination problem. The constraint is
 *   invariant across all three perspectives because it is grounded in formal
 *   logic and computability theory, not in institutional design or actor
 *   interests. The undecidability is a consequence of the Turing-completeness
 *   of the computational model and the Halting Problem's own undecidability.
 *   Theater ratio is negligibly low (0.05) because there is no performative
 *   element to the constraint — the mathematical proof is direct and
 *   non-negotiable. Extractiveness is minimal (0.12) because no agent
 *   benefits from the constraint; it is a pure limitation. Suppression is
 *   negligible (0.02) because the constraint operates at the level of formal
 *   mathematics, not enforcement or behavioral suppression.
 *
 * KEY AGENTS:
 *   - Compiler writers and static analysis tool developers: Encounter this constraint when attempting to prove properties of arbitrary programs — they cannot exit or overcome it through better algorithms
 *   - Software verification researchers: Accept the constraint as fundamental and work within it via approximation, partial solutions, and restricted domains
 *   - Programming language designers: Design languages with restricted expressiveness (sub-Turing-complete) to enable semantic decidability, accepting reduced computational power
 *   - Theoretical computer scientists: Document the constraint and establish its relationships to other undecidability results
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rice_theorem_undecidability, 0.12).
domain_priors:suppression_score(rice_theorem_undecidability, 0.02).
domain_priors:theater_ratio(rice_theorem_undecidability, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rice_theorem_undecidability, extractiveness, 0.12).
narrative_ontology:constraint_metric(rice_theorem_undecidability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(rice_theorem_undecidability, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rice_theorem_undecidability, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(rice_theorem_undecidability, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rice_theorem_undecidability, mountain).
narrative_ontology:human_readable(rice_theorem_undecidability, "Rice's Theorem: Undecidability of Semantic Program Properties").
narrative_ontology:topic_domain(rice_theorem_undecidability, "theoretical_computer_science/computability_theory").

domain_priors:emerges_naturally(rice_theorem_undecidability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPILER OPTIMIZATION (MOUNTAIN) — A compiler seeking to determine semantic properties of arbitrary programs (whether code terminates, uses memory correctly, implements a specific algorithm) faces a mathematical impossibility. No algorithm can solve this for all programs. This is not a limitation of current technology or technique — it is a logical limit. The compiler designer cannot exit this constraint through better engineering.
constraint_indexing:constraint_classification(rice_theorem_undecidability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: STATIC ANALYSIS TOOL DEVELOPER (MOUNTAIN) — The tool developer can approximate semantic properties via abstract interpretation, but cannot achieve completeness. The constraint is real at the formal level — any total decision procedure for non-trivial semantic properties does not exist. The developer is constrained by mathematics, not by market competition or institutional inertia. Can work around specific cases but cannot defeat the general theorem.
constraint_indexing:constraint_classification(rice_theorem_undecidability, mountain,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: THEORETICAL OBSERVER (MOUNTAIN) — From the perspective of mathematical logic and computability theory, Rice's Theorem is a consequence of the undecidability of the Halting Problem. It is a structural property of Turing-complete computation, not a contingent institutional constraint. The theorem emerged from first principles and holds across all possible computational models equivalent to Turing machines. No observational perspective or measurement methodology changes this classification.
constraint_indexing:constraint_classification(rice_theorem_undecidability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rice_theorem_undecidability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(rice_theorem_undecidability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rice_theorem_undecidability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rice_theorem_undecidability, ExtMetricName, E),
    domain_priors:suppression_score(rice_theorem_undecidability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(rice_theorem_undecidability),
    narrative_ontology:constraint_metric(rice_theorem_undecidability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(rice_theorem_undecidability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(rice_theorem_undecidability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. No agent extracts value from this constraint in the extractive sense. The constraint is a pure limitation — it removes degrees of freedom from all observers equally. The small non-zero value reflects that the theoretical result does enable some communities to claim expertise (semantic analysis researchers, formal methods specialists) and could theoretically direct resources or reputation. But this is incidental, not structural. Suppression (0.02): Negligible. The constraint operates through mathematical impossibility, not behavioral suppression or barrier-raising. Agents understand the constraint through proof, not enforcement. Theater ratio (0.05): Negligible. The mathematical proof is direct — Rice's proof establishes the result via a reduction from the Halting Problem. No performative layer masks the mechanism. Accessibility collapse (0.95): Very high. Alternative approaches to semantic analysis are blocked completely. Approximation and restricted domains are not alternatives to Rice's Theorem — they are acknowledgments of it. Resistance (0.02): Very low. The theorem has stood for 70+ years without effective challenge. No alternative computational model equivalent to Turing machines escapes the undecidability.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap for this constraint. All three perspectives classify it as Mountain because the constraint's source is formal logic, not institutional design or actor interests. A compiler writer, a language designer, and a theoretical computer scientist all agree that semantic undecidability is a mathematical fact, not a coordination problem or extraction mechanism. The invariance across perspectives is itself the diagnostic signal — when a constraint classifies identically from powerless, moderate, and analytical positions across all time horizons and scopes, it is very likely a true mountain or a false summit. The commentary below tests whether this is genuine or naturalized.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by being a genuine natural law. The base properties meet all mountain gates: extractiveness (0.12) ≤ 0.25, suppression (0.02) ≤ 0.05, emerges_naturally: true, accessibility_collapse (0.95) ≥ 0.85, resistance (0.02) ≤ 0.15. All three perspectives classify as Mountain. The theater ratio (0.05) is stable over the interval — no degradation or drift toward Piton. The constraint is not maintained by institutional inertia or performative activity; it is maintained by mathematics. No perspective perceives meaningful extraction, coordination, or enforcement. This is the negative mandatrophy case: the threat is not over-classification as mountain (false summit), but under-classification or conflation with institutional constraints. Rice's Theorem must not be confused with institutional barriers to semantic analysis (budgets, tool availability, researcher specialization) — those are separate constraints. The formal undecidability is distinct from the practical difficulty of semantic analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    non_turing_complete_escape,
    'Do restricted computational models (linear logic, bounded-resource languages, Presburger arithmetic) escape Rice''s Theorem?',
    'Formal analysis of expressiveness hierarchy; demonstration that semantic decidability requires sub-Turing-completeness',
    'If true: Rice''s Theorem is specific to Turing-complete models, suggesting computational expressiveness trades off against semantic decidability. If false: the undecidability is more fundamental than Turing completeness alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_turing_complete_escape, conceptual, 'Whether Rice''s Theorem is specific to Turing-complete computation or more fundamental').

omega_variable(
    approximation_sufficiency_boundary,
    'For practical software systems, at what approximation threshold do abstract interpretation and heuristic tools become ''sufficient'' semantic analysis despite formal undecidability?',
    'Empirical metrics: false positive/negative rates of static analysis tools on real codebases; industry adoption thresholds for different tool precision levels',
    'If approximation suffices for all practical cases: Rice''s Theorem becomes a mountain with zero operational impact. If not: the theorem establishes permanent resource-extraction tradeoffs (more precision requires more computation/cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(approximation_sufficiency_boundary, empirical, 'Practical sufficiency boundary for approximate semantic analysis').

omega_variable(
    proof_mechanism_necessity,
    'Is the undecidability of semantic properties a direct consequence of Gödel''s Incompleteness, or does it rest on independent foundations (e.g., the Halting Problem specifically)?',
    'Proof-theoretic analysis: formalize Rice''s Theorem in minimal axiom systems and identify which axioms are essential',
    'If derivable from Gödel: semantic undecidability is part of a broader impossibility family. If independent: it may have different implications for logic and languages that evade Gödelian limits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proof_mechanism_necessity, conceptual, 'Relationship between Rice''s Theorem and Gödel''s Incompleteness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rice_theorem_undecidability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rice_tr_t0, rice_theorem_undecidability, theater_ratio, 0, 0.02).
narrative_ontology:measurement(rice_tr_t50, rice_theorem_undecidability, theater_ratio, 50, 0.04).
narrative_ontology:measurement(rice_tr_t100, rice_theorem_undecidability, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(rice_be_t0, rice_theorem_undecidability, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(rice_be_t50, rice_theorem_undecidability, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(rice_be_t100, rice_theorem_undecidability, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rice_theorem_undecidability, information_standard).
narrative_ontology:affects_constraint(rice_theorem_undecidability, halting_problem_undecidability).
narrative_ontology:affects_constraint(rice_theorem_undecidability, godel_incompleteness_first).
narrative_ontology:affects_constraint(rice_theorem_undecidability, semantic_program_verification).

% DUAL FORMULATION NOTE:
% Rice's Theorem is downstream of the Halting Problem undecidability (via reduction) and related to Gödel's Incompleteness, but represents a distinct structural impossibility specific to semantic properties of programs. Affects practical constraints like semantic program verification, which attempts to work around the undecidability via approximation and restricted domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
