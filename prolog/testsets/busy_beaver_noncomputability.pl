% ============================================================================
% CONSTRAINT STORY: busy_beaver_noncomputability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_busy_beaver_noncomputability, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: busy_beaver_noncomputability
 *   human_readable: The Non-Computability of the Busy Beaver Function (Σ)
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   The Busy Beaver function, Σ(n), represents the maximum number of steps a
 *   halting Turing machine with n states can perform before halting. While
 *   the function is well-defined for every n, it is non-computable. This
 *   means no single algorithm (i.e., no Turing machine) can exist that
 *   calculates Σ(n) for any given n. This is not a matter of current
 *   technological limitation but a fundamental, provable boundary of what is
 *   computable. It serves as a concrete example of the limits established by
 *   the Halting Problem.
 *
 * KEY AGENTS:
 *   - Theoretical Computer Scientists: Analytical observers who define and study these limits.
 *   - Software Engineers & Programmers: Practitioners who encounter the practical implications of non-computability when trying to create general-purpose code analysis tools.
 *   - AI Safety Researchers: Institutional actors who must design systems that operate safely despite the impossibility of predicting the behavior of all possible programs.
 *   - Mathematicians: Analytical observers who see this as a feature of formal systems, related to Gödel's incompleteness theorems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(busy_beaver_noncomputability, 0.01).
domain_priors:suppression_score(busy_beaver_noncomputability, 0.02).
domain_priors:theater_ratio(busy_beaver_noncomputability, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(busy_beaver_noncomputability, extractiveness, 0.01).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(busy_beaver_noncomputability, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(busy_beaver_noncomputability, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(busy_beaver_noncomputability, mountain).
narrative_ontology:human_readable(busy_beaver_noncomputability, "The Non-Computability of the Busy Beaver Function (Σ)").
narrative_ontology:topic_domain(busy_beaver_noncomputability, "technological/mathematical").

domain_priors:emerges_naturally(busy_beaver_noncomputability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE AMBITIOUS PROGRAMMER (MOUNTAIN) — A programmer attempting to write a general 'program termination analyzer' is fundamentally blocked. They are trapped by a logical limit they cannot engineer around. The constraint is an impassable wall.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE AI SAFETY INSTITUTE (MOUNTAIN) — An institution seeking to create a provably safe, general-purpose AI system confronts this limit. Even with vast resources, they cannot build a tool to determine if an arbitrary advanced AI will halt or loop. The limit is structural and resource-invariant.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE THEORETICAL COMPUTER SCIENTIST (MOUNTAIN) — The analytical observer sees the constraint not as a barrier but as a fundamental feature of the computational universe, a direct consequence of the Halting Problem. It is a fixed landmark in the landscape of logic.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE COMPUTATIONAL COMPLEXITY RESEARCHER (MOUNTAIN) — This group works around the constraint by focusing on what *can* be known (e.g., finding values or bounds for Σ(n) for very small n). Their mobility is the ability to shift research questions, but the general non-computability remains an unchangeable background fact.
constraint_indexing:constraint_classification(busy_beaver_noncomputability, mountain,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(busy_beaver_noncomputability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(busy_beaver_noncomputability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(busy_beaver_noncomputability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, ExtMetricName, E),
    domain_priors:suppression_score(busy_beaver_noncomputability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(busy_beaver_noncomputability),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(busy_beaver_noncomputability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(busy_beaver_noncomputability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is a canonical example of a Mountain. Extractiveness (ε=0.01) is negligible; the limit does not transfer value, it simply exists. Suppression (0.02) is also negligible; there are no 'alternatives' to a mathematical proof. The theater ratio is zero as there is no performative aspect. The Natural Law profile is met with perfect scores: it `emerges_naturally` from the axioms of computation, `accessibility_collapse` (0.98) is extremely high (once the proof is understood, its conclusion is inescapable), and `resistance` (0.02) is futile.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. This is a key diagnostic feature of a true, non-social Mountain. The non-computability of Σ(n) is an objective, invariant fact within the Turing model of computation. Every agent, regardless of power, resources, or goals, perceives the constraint as an unchangeable feature of their environment. The classification is stable across all possible indices.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain constraint with no extractive function, there are no beneficiaries or victims. The `beneficiaries` and `victims` arrays are empty. The directionality `d` will be determined by the canonical fallback values for each power atom. However, since base extractiveness (ε) is approximately zero, the effective extraction (χ = ε × f(d) × σ(S)) is also approximately zero for all perspectives, leading to a uniform Mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint serves as a crucial baseline for the entire classification system. It represents a 'pure' Mountain, free of social construction, enforcement, or potential for extraction. By establishing a clear signature for a mathematical limit (ε≈0, suppression≈0, perfect NL profile), it provides a powerful tool to detect 'false summits'—socially constructed Snares or Tangled Ropes that are falsely claimed to be natural or inevitable (i.e., claimed to be Mountains). Any constraint with significantly higher ε or suppression cannot be a Mountain of this type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(busy_beaver_noncomputability, 1962, 2100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(busy_beaver_noncomputability, halting_problem).
narrative_ontology:affects_constraint(busy_beaver_noncomputability, goedel_incompleteness).

% DUAL FORMULATION NOTE:
% The non-computability of the Busy Beaver function is a direct corollary of the undecidability of the Halting Problem. It can be seen as a more concrete or 'maximal' formulation of the same underlying logical barrier.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
