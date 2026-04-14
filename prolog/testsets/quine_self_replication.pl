% ============================================================================
% CONSTRAINT STORY: quine_self_replication
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quine_self_replication, []).

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
 *   constraint_id: quine_self_replication
 *   human_readable: Quines (Computational Self-Replication)
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   Quines represent a pure logical constraint on computational systems. The
 *   constraint asserts: any Turing-complete programming language admits at
 *   least one non-empty program that takes no input and produces a copy of
 *   its own source code as output. This is not a contingent institutional
 *   arrangement, a policy choice, or a feature of a particular technology. It
 *   is a direct consequence of Kleene's recursion theorem and the
 *   formalization of self-reference in computation. The constraint exhibits
 *   zero degrees of freedom across all observational contexts and from all
 *   structural positions. Neither programmers, language designers, nor
 *   theoretical computer scientists can eliminate or circumvent the existence
 *   of quines without abandoning Turing-completeness entirely. This makes
 *   quines a canonical example of a Mountain constraint — a natural law of
 *   computation.
 *
 * KEY AGENTS:
 *   - Logician/Theoretical Computer Scientist: Observer (analytical/analytical) — recognizes quines as a theorem, not a contingent constraint
 *   - Programming Language Designer: Powerful agent (powerful/mobile) — can restrict language features but cannot prevent quines within Turing-complete systems
 *   - Programmer: Moderate agent (moderate/constrained) — works within any given language; encounters quines as an immutable logical fact
 *   - Academic Community: Organized community (organized/arbitrage) — codifies quines as established theory; no contestation of existence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quine_self_replication, 0.12).
domain_priors:suppression_score(quine_self_replication, 0.03).
domain_priors:theater_ratio(quine_self_replication, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quine_self_replication, extractiveness, 0.12).
narrative_ontology:constraint_metric(quine_self_replication, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(quine_self_replication, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quine_self_replication, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(quine_self_replication, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quine_self_replication, mountain).
narrative_ontology:human_readable(quine_self_replication, "Quines (Computational Self-Replication)").
narrative_ontology:topic_domain(quine_self_replication, "technological/mathematical").

domain_priors:emerges_naturally(quine_self_replication).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOGICAL INVARIANT (MOUNTAIN) — From the standpoint of formal computation theory, the existence of quines follows directly from Kleene's recursion theorem and the Church-Turing thesis. Any Turing-complete language admits quines. This is not a constraint imposed by external force or institutional design, but an irreducible property of self-reference in computational systems. The constraint emerges from first principles of logic, not from enforcement. Zero degrees of freedom — quines cannot be eliminated by choosing a different language or runtime environment unless you abandon Turing-completeness itself.
constraint_indexing:constraint_classification(quine_self_replication, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: LANGUAGE DESIGNER (MOUNTAIN) — A language designer might hope to prevent quines through careful restrictions — disabling output functions, preventing reflection, limiting recursion. Every attempt fails. The constraint re-emerges in a different form (using string manipulation, leveraging ASCII codes, exploiting printf format strings). The existence of quines is not a bug in any specific language; it is a mathematical theorem about the expressiveness of Turing-complete systems. Escape is logically impossible without crippling the language to sub-Turing-complete status.
constraint_indexing:constraint_classification(quine_self_replication, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: PROGRAMMER (MOUNTAIN) — A programmer working in any Turing-complete language encounters the quine as an immutable logical fact. Writing a quine is non-trivial (it feels like a puzzle), but the possibility is not subject to external enforcement or removal. It is more like gravity than like a regulation — you can acknowledge it, work with it, or try creative workarounds, but you cannot negotiate it away. The constraint has zero degrees of freedom from the programmer's vantage point.
constraint_indexing:constraint_classification(quine_self_replication, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ACADEMIC COMPUTER SCIENCE (MOUNTAIN) — Quines are recognized as a fundamental property of computation theory, formally proven by recursion theorems. Academic consensus is overwhelming: this is not a constraint that can be modified through policy, research effort, or institutional change. The constraint is invariant across all observational contexts. No academic community has ever successfully disputed or circumvented the logical necessity of quines.
constraint_indexing:constraint_classification(quine_self_replication, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quine_self_replication_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(quine_self_replication, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quine_self_replication, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quine_self_replication, ExtMetricName, E),
    domain_priors:suppression_score(quine_self_replication, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quine_self_replication),
    narrative_ontology:constraint_metric(quine_self_replication, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quine_self_replication, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quine_self_replication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. Quines extract nothing from any agent. There is no extraction flow, no beneficiary, no victim. The 'extraction' value reflects only that any logical constraint has some minimal friction cost (the computational overhead of producing self-referential code), but this is not what extraction means in the DR framework. The value is near-zero because no agent is being coerced or denied alternatives. Suppression (0.03): Minimal. Quines do not suppress alternatives by coercion. No institutional force prevents you from writing non-quine programs. The low value reflects that the constraint does not operate through suppression at all. Theater ratio (0.15): Minimal. There is no performative or theatrical component to quines. The constraint is purely functional — either a program outputs a copy of its source code or it does not. No ritual, no signaling, no appearance-reality gap.
 *
 * PERSPECTIVAL GAP:
 *   No perspectival gap. All four perspectives converge on Mountain classification. This perfect consensus is the signature of natural law constraints and distinguishes mountains from all other types.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is undefined for mountain constraints. The chi formula χ = ε × f(d) × σ(S) does not apply because there is no extraction flow, no beneficiary-victim asymmetry, and no agent-relative coercion. The constraint operates identically for all agents. No agent benefits; no agent is harmed. The constraint is simply a logical truth that all agents must accommodate. This is the defining feature of a mountain: zero degrees of freedom means zero variation in how different agents experience the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN-ONLY CONSTRAINT: No mandatrophy to resolve. The constraint does not conflate coordination and extraction because it performs neither. Quines are pure logical necessities with no institutional or extractive component. All perspectives agree; no disambiguation needed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quine_self_replication, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quine_self_replication, godel_incompleteness_arithmetic).
narrative_ontology:affects_constraint(quine_self_replication, halting_problem_undecidability).
narrative_ontology:affects_constraint(quine_self_replication, turing_completeness_expressiveness).

% DUAL FORMULATION NOTE:
% Quines are part of a constraint family within mathematical/computational logic. Kleene's recursion theorem (the upstream constraint establishing existence of fixed points in computation) provides the foundation; quines are a direct application of this theorem to self-referential code. Gödel's incompleteness theorem operates in the same logical space (self-reference in formal systems). Halting problem undecidability is a related limit on what computation can decide about its own behavior. All three are mountains from all perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
