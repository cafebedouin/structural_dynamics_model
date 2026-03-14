% ============================================================================
% CONSTRAINT STORY: program_verification_hardness
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_program_verification_hardness, []).

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
 *   constraint_id: program_verification_hardness
 *   human_readable: Program Verification Hardness
 *   domain: computer_science/mathematical_logic
 *
 * SUMMARY:
 *   Program verification hardness is a fundamental constraint arising from
 *   the mathematical structure of formal proof and computation. It manifests
 *   as multiple interlocking barriers: the Halting Problem's undecidability
 *   makes general program termination verification impossible; proof
 *   expansion theorems show that formal proofs can require exponentially more
 *   symbols than the theorems they prove; and bounded model checking is
 *   PSPACE-complete, making verification of even small bounded traces
 *   computationally intractable. This constraint is invariant across all
 *   observers, all time horizons, and all technological contexts within
 *   Church-Turing equivalence. It is not a policy choice, an institutional
 *   arrangement, or a contingent engineering trade-off. It is a law of formal
 *   logic.
 *
 * KEY AGENTS:
 *   - Software Engineers: Powerless/trapped — must verify correctness of programs they write but face immutable barriers to complete verification
 *   - Theoretical Computer Scientists: Analytical/analytical — recognize and formalize the mathematical structure of the hardness
 *   - Safety-Critical System Authorities: Institutional/arbitrage — must work within the constraint by restricting to tractable subclasses of programs
 *   - Formal Methods Research Community: Organized/constrained — develops strategies (abstraction, approximation, automation) to mitigate hardness without overcoming it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(program_verification_hardness, 0.18).
domain_priors:suppression_score(program_verification_hardness, 0.03).
domain_priors:theater_ratio(program_verification_hardness, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(program_verification_hardness, extractiveness, 0.18).
narrative_ontology:constraint_metric(program_verification_hardness, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(program_verification_hardness, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(program_verification_hardness, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(program_verification_hardness, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(program_verification_hardness, mountain).
narrative_ontology:human_readable(program_verification_hardness, "Program Verification Hardness").
narrative_ontology:topic_domain(program_verification_hardness, "computer_science/mathematical_logic").

domain_priors:emerges_naturally(program_verification_hardness).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOFTWARE ENGINEER (MOUNTAIN) — From the position of one tasked with verifying program correctness, the hardness of formal verification appears as an immutable constraint. No amount of effort, resources, or innovation removes the fundamental barrier: proving program correctness requires more formal proof steps than the program itself contains (proof expansion problem). The engineer cannot exit this constraint through cleverness or investment — it is a law of formal logic, not a policy choice.
constraint_indexing:constraint_classification(program_verification_hardness, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THEORETICAL COMPUTER SCIENTIST (MOUNTAIN) — Verification hardness is a mathematical necessity. The undecidability of the Halting Problem, the PSPACE-completeness of bounded model checking, and the proof expansion theorems constitute natural law. These results are formal mathematical facts, not engineering trade-offs. The constraint exists independent of any observer's preferences and cannot be negotiated, circumvented, or re-framed. It is immutable across all possible computational frameworks within Church-Turing equivalence.
constraint_indexing:constraint_classification(program_verification_hardness, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: SAFETY-CRITICAL SYSTEM AUTHORITY (MOUNTAIN) — Organizations responsible for certifying airplanes, medical devices, or nuclear systems experience verification hardness as a natural law that defines the boundary of what they can claim to have certified. They cannot exit this constraint by choice or funding. They can only work within it: selecting subclasses of programs that are tractable to verify (e.g., finite-state systems, synchronous designs), investing in proof automation and theorem provers, or accepting residual risk. The constraint is immutable; only the agent's strategy within its bounds changes.
constraint_indexing:constraint_classification(program_verification_hardness, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: FORMAL METHODS RESEARCH COMMUNITY (MOUNTAIN) — The research community collectively experiences verification hardness as an immutable obstacle that shapes their entire research agenda. Bounded model checking, abstraction refinement, parametric verification, and automated theorem proving are all responses to hardness, not solutions that overcome it. The constraint is generational — it has persisted through 70 years of computer science and will persist for the foreseeable future. Exit options are constrained: researchers can work within the hardness by developing better approximations, but they cannot eliminate the underlying barrier.
constraint_indexing:constraint_classification(program_verification_hardness, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(program_verification_hardness_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(program_verification_hardness, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(program_verification_hardness, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(program_verification_hardness, ExtMetricName, E),
    domain_priors:suppression_score(program_verification_hardness, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(program_verification_hardness),
    narrative_ontology:constraint_metric(program_verification_hardness, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(program_verification_hardness, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(program_verification_hardness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. Verification hardness does not extract value from any agent for the benefit of others. It is a mathematical barrier that affects all agents symmetrically — it constrains capability universally without creating asymmetric benefit flows. The modest non-zero value reflects that some agents (researchers developing workarounds, tool vendors, consultants) may capture marginal economic benefit from the existence of the problem. But this is incidental to the constraint's structure, not its primary mechanism. Suppression (0.03): Minimal. The constraint operates through logical necessity, not through the suppression of alternatives. No agent is forced to pretend the constraint does not exist; all acknowledge its mathematical reality. Accessibility collapse (0.92): Very high. There is no accessible alternative to the constraint. No agent can choose a different computational model or logical framework that avoids undecidability and proof expansion — these are properties of all known models within Church-Turing equivalence. The constraint is logically inescapable. Resistance (0.08): Very low. The constraint meets no resistance from agents because all agents recognize its mathematical inevitability. There is no struggle against verification hardness as an unjust imposition; there is only acceptance of a mathematical fact and adaptation to it. Theater ratio (0.05): Minimal. The constraint is purely functional — verification truly is hard. There is almost no performative element. Some organizations may perform compliance with formal methods practices that do not actually improve verification (theater), but this is distinct from the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. All four perspectives converge on the same classification: mountain. The software engineer, the theoretical computer scientist, the safety authority, and the research community all experience and understand verification hardness as an immutable constraint. This invariance across all observer positions is the signature of a true natural law. When all perspectives agree on the same classification from the same constraint, the system has found a genuine invariant of the domain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality logic does not apply to mountain constraints. Mountains are invariant across all observer positions and all directionality parameters. Verification hardness is not a constraint that benefits some agents at the cost of others. It is a constraint that affects all agents uniformly — it restricts capability across the entire landscape of computation. There is no beneficiary, no victim, no asymmetric extraction. The constraint is symmetric in its impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(program_verification_hardness, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(program_verification_hardness, program_synthesis_limitation).
narrative_ontology:affects_constraint(program_verification_hardness, formal_specification_incompleteness).
narrative_ontology:affects_constraint(program_verification_hardness, automated_theorem_proving_decidability_ceiling).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
