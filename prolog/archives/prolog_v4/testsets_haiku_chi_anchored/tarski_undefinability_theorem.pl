% ============================================================================
% CONSTRAINT STORY: tarski_undefinability_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tarski_undefinability_theorem, []).

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
 *   constraint_id: tarski_undefinability_theorem
 *   human_readable: Tarski's Undefinability Theorem
 *   domain: mathematical_logic/foundations
 *
 * SUMMARY:
 *   Tarski's Undefinability Theorem is a foundational result in mathematical
 *   logic: for any sufficiently expressive formal system S (containing Peano
 *   arithmetic), it is impossible to define a truth predicate T(x) within S
 *   such that T encodes exactly which sentences of S are true under the
 *   standard interpretation. The theorem does not claim that truth is
 *   unknowable — semantic/model-theoretic approaches can define truth from
 *   outside the system (in a metalanguage). Rather, it establishes a logical
 *   boundary: self-reference and expressive power create a structural
 *   incompleteness. This constraint is a natural law of formal systems, not
 *   an institutional arrangement. It exhibits zero degrees of freedom across
 *   all indexical perspectives. The theorem has been known since 1933 and has
 *   remained invariant — no clever reformulation, computational advance, or
 *   institutional strategy has altered its basic claim. Theater ratio remains
 *   low (0.08–0.15) because the theorem is substantive: its proofs are
 *   genuinely difficult, its implications are non-trivial, and its
 *   universality across formal systems is empirically confirmed. There is no
 *   performative content — the constraint is what it states.
 *
 * KEY AGENTS:
 *   - The Formal System: Structural entity governed by the constraint. Cannot define its own truth predicate without inconsistency.
 *   - The Logician/Mathematician: Cognitive agent working with formal systems. Understands the constraint as a boundary condition on system design.
 *   - The Metalanguage/Oracle: Semantic framework outside the constrained system. Can define truth for the system by stepping outside it.
 *   - The Computational Implementer: Engineer designing proof assistants and automated systems. Works within the constraint via hierarchical architectures.
 *   - The Analytical Observer: Civilizational perspective on formal foundations. Recognizes the constraint as invariant across all equivalent formulations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tarski_undefinability_theorem, 0.08).
domain_priors:suppression_score(tarski_undefinability_theorem, 0.02).
domain_priors:theater_ratio(tarski_undefinability_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tarski_undefinability_theorem, extractiveness, 0.08).
narrative_ontology:constraint_metric(tarski_undefinability_theorem, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(tarski_undefinability_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tarski_undefinability_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(tarski_undefinability_theorem, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tarski_undefinability_theorem, mountain).
narrative_ontology:human_readable(tarski_undefinability_theorem, "Tarski's Undefinability Theorem").
narrative_ontology:topic_domain(tarski_undefinability_theorem, "mathematical_logic/foundations").

domain_priors:emerges_naturally(tarski_undefinability_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOGICAL FOUNDATION (MOUNTAIN) — From the perspective of formal systems theory, Tarski's undefinability is an immutable property of sufficiently expressive languages. No formal system containing arithmetic can define its own truth predicate without inconsistency. This is a consequence of logical structure itself, not a contingent institutional arrangement. The theorem follows necessarily from the Liar's Paradox and Gödel's completeness results. χ ≈ 0.02 (universal scope, analytical power, no directionality asymmetry).
constraint_indexing:constraint_classification(tarski_undefinability_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICIAN'S CONTEXT (MOUNTAIN) — Working mathematicians encounter this constraint when designing formal systems. The inability to define truth internally forces specific architectural choices (moving to stronger formal systems, using semantic/model-theoretic approaches, or accepting truth-apt statements outside the formalized theory). But these choices do not escape the constraint — they merely relocate it. Every choice involves a structurally isomorphic limitation. Mobile exit options (switching formalization frameworks) do not reduce the extractiveness because the constraint is invariant across all frameworks. d ≈ 0.50, f(d) ≈ 0.65, σ = 1.0 → χ ≈ 0.05.
constraint_indexing:constraint_classification(tarski_undefinability_theorem, mountain,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: FORMAL SYSTEMS ENGINEERING (MOUNTAIN) — Computer science and mathematical logic use formal systems to establish decidability, soundness, and consistency properties. Tarski's theorem imposes a structural limit: no effective algorithm can decide truth-in-the-system for the system itself. Institutional actors (university logic programs, automated theorem provers, proof assistants) routinely work around this constraint by constructing hierarchies (object language / metalanguage) or using external oracles. But these are not escapes — they are design patterns that accept the constraint and work within it. The constraint remains invariant. d ≈ 0.10, f(d) ≈ -0.05, σ = 1.0 → χ ≈ -0.004. Negative χ reflects that institutions benefit from understanding the constraint (it guides system design).
constraint_indexing:constraint_classification(tarski_undefinability_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: ASPIRATIONAL FORMALIZATION (MOUNTAIN) — Any attempt to formalize truth itself within a sufficiently expressive system confronts Tarski's theorem directly. The theorem is not a barrier to be overcome by cleverness or effort — it is a logical wall. An agent attempting to define truth-in-PA within PA will necessarily fail. But this 'failure' is not an extraction mechanism (there is no beneficiary, no coercion apparatus, no suppression of alternatives). The constraint is invariant across all observational frames. d ≈ 0.95, f(d) ≈ 1.42, σ = 1.0 → χ ≈ 0.11. Even the maximally-trapped agent experiences this as a natural limit, not as extraction, because there is no institutional structure maintaining it.
constraint_indexing:constraint_classification(tarski_undefinability_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tarski_undefinability_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(tarski_undefinability_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tarski_undefinability_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tarski_undefinability_theorem, ExtMetricName, E),
    domain_priors:suppression_score(tarski_undefinability_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tarski_undefinability_theorem),
    narrative_ontology:constraint_metric(tarski_undefinability_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tarski_undefinability_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tarski_undefinability_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. Tarski's theorem does not extract value from any agent. No institutional structure benefits from the undefinability or coerces compliance. The constraint is a mathematical limit, not a mechanism of power or resource transfer. Suppression (0.02): Negligible. The theorem does not suppress alternatives — it defines the boundaries of what alternatives can exist. One cannot 'suppress' a truth predicate by hiding it; the theorem proves such a predicate cannot exist consistently. Theater ratio (0.15): Very low. The theorem is substantive and difficult. Its proof involves genuine logical work (diagonalization, Liar's Paradox, model theory). The small residual theater (0.15 vs 0.0) reflects only the pedagogical gap between understanding the theorem and understanding why it matters — a minimal performative element in teaching, not in the constraint itself. Accessibility collapse (0.92): Very high. The constraint is nearly impossible to work around — any attempted workaround either accepts the constraint or moves to a different formal framework where an isomorphic limitation applies. Resistance (0.03): Negligible. The theorem is not resisted; it is accepted as foundational. Mathematicians do not rail against Tarski — they build systems that respect it.
 *
 * PERSPECTIVAL GAP:
 *   Unlike the verification bottleneck exemplar, Tarski's theorem produces NO perspectival gap. All indexed observers — from powerless to institutional to analytical — classify this constraint identically as a Mountain. The theorem makes no distinction based on power, time horizon, exit options, or spatial scope. A mathematician powerless to reform formal systems and an institutional proof assistant designer both encounter the same structural limit. A generational timeframe and a civilizational one both see the constraint as invariant. Local and global scopes both respect the same logical boundaries. This uniformity is a diagnostic marker of a true natural law: the constraint is observer-independent. If the constraint classified differently from different perspectives (e.g., Rope from the semanticist's view, Snare from the formalist's view), it would signal that the label 'Tarski's theorem' conflates two structurally distinct claims and should be decomposed. The absence of perspectival gap confirms that the constraint is a single, unified phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is invariant across all perspectives because there are no beneficiaries or victims. Tarski's theorem imposes a symmetric logical boundary. The mathematician is not extracted from by the theorem. The formal system does not suffer asymmetric costs. The semantic framework does not benefit at the expense of another agent. The theorem is not an institutional arrangement maintained by coercion or suppression — it is a consequence of logical structure. d ≈ 0.50 ± 0.05 across all perspectives (symmetric), f(d) ≈ 0.65, σ ≈ 1.0 → χ ≈ 0.05 for all observers. This invariance is why the perspective cluster is Mountain-only.
 *
 * MANDATROPHY ANALYSIS:
 *   Tarski's theorem presents zero mandatrophy risk. The constraint cannot be misclassified as coordination (it has no coordination function) or as extraction (it has no beneficiary). The theorem does not attempt to solve a collective action problem — it defines a boundary on possible solutions. It does not extract resources from victims — it applies equally to all agents. The mountain classification is justified by the mathematical content, not by a performance of naturalness. The theorem is what it claims to be: an immutable property of logical systems. There is no incentive to relabel it (e.g., as a Rope to make it appear less restrictive) because its restrictiveness is precise and mathematical, not institutional. The constraint resolves mandatrophy by being what it is — a true natural law of formal systems, not a contingent institutional arrangement masquerading as natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    truth_in_weak_systems,
    'Can truth be internally defined in sufficiently weak formal systems (below Peano arithmetic, e.g., Presburger arithmetic)?',
    'Proof-theoretic analysis of the completeness gap in weak systems; determination of the minimal expressive power threshold for undefinability to apply',
    'If true: the constraint applies only to expressive systems, not to all formal systems universally. The ''mountain'' classification becomes specific to rich languages, not universal. If false: even weak systems exhibit variant forms of the undefinability property.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(truth_in_weak_systems, empirical, 'Boundary of undefinability in weaker formal systems').

omega_variable(
    semantic_vs_syntactic_truth,
    'Is the distinction between semantic truth (model-theoretic) and syntactic truth (provability within the system) a true escape route or a relabeling of the same constraint?',
    'Analysis of whether moving to semantic frameworks genuinely avoids the undefinability limitation or merely shifts the problem to the metalanguage',
    'If escape: the constraint is purely syntactic, and semantic approaches provide genuine alternatives. Mountain classification is too strong. If relabeling: the constraint persists in all equivalent formulations. Mountain classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(semantic_vs_syntactic_truth, conceptual, 'Whether semantic approaches genuinely escape undefinability').

omega_variable(
    institutional_bypass_mechanisms,
    'Do practical computational systems (proof assistants, automated theorem provers) successfully bypass the undefinability constraint through hierarchical or oracle-based architectures?',
    'Empirical study of decidability gaps in Coq, Isabelle, Lean, and other systems; measurement of whether truth-in-the-system can be effectively computed via metalanguage oracles without loss of information',
    'If successful bypass: the constraint is a mathematical limit on self-reference, not an operational limit. Institutions escape via architectural choice. If no true bypass: hierarchical systems relocate but do not eliminate the constraint. Mountain classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_bypass_mechanisms, empirical, 'Whether practical proof systems bypass undefinability via metalanguage oracles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tarski_undefinability_theorem, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tarski_tr_t0, tarski_undefinability_theorem, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tarski_tr_t50, tarski_undefinability_theorem, theater_ratio, 50, 0.12).
narrative_ontology:measurement(tarski_tr_t100, tarski_undefinability_theorem, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(tarski_be_t0, tarski_undefinability_theorem, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(tarski_be_t50, tarski_undefinability_theorem, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(tarski_be_t100, tarski_undefinability_theorem, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tarski_undefinability_theorem, information_standard).
narrative_ontology:affects_constraint(tarski_undefinability_theorem, godel_incompleteness_theorem).
narrative_ontology:affects_constraint(tarski_undefinability_theorem, church_turing_decidability_boundary).
narrative_ontology:affects_constraint(tarski_undefinability_theorem, halting_problem_undecidability).

% DUAL FORMULATION NOTE:
% Tarski's undefinability is part of a constraint family with Gödel's incompleteness and the Church-Turing halting problem. These three constraints are structurally isomorphic — they are all consequences of self-reference and expressive power in formal systems. They should not be decomposed into separate stories; rather, they form a single family linked by logical dependence. Tarski's theorem is the most general: undefinability is the logical core; Gödel's incompleteness and the halting problem are specific instantiations of the same phenomenon in different formalisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
