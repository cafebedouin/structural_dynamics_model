% ============================================================================
% CONSTRAINT STORY: tarski_undefinability_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: mathematical_logic/foundational_mathematics
 *
 * SUMMARY:
 *   Tarski's undefinability theorem (1933/1936) states that for any
 *   sufficiently expressive formal system (such as Peano Arithmetic or
 *   Zermelo-Fraenkel set theory), it is impossible to define a predicate that
 *   captures truth for sentences in that system using only the resources
 *   available within the system itself. The proof uses diagonalization: any
 *   attempted definition of 'true(x)' for the system's sentences leads to a
 *   self-referential contradiction analogous to the Liar's paradox ('this
 *   sentence is false'). This constraint differs fundamentally from Gödel's
 *   incompleteness theorems — it is not about unprovability but about
 *   undefinability. The theorem establishes that truth transcends formal
 *   expression within expressive systems. From all perspectives examined,
 *   this constraint classifies as Mountain: it is an immutable logical limit
 *   that holds across all frameworks, all observers, and all times. No agent
 *   benefits from undefinability, and no agent experiences extraction — it is
 *   a structural necessity of language and meaning.
 *
 * KEY AGENTS:
 *   - Mathematical Logician: Analytical observer (analytical/analytical) — perceives undefinability as a pure mathematical necessity
 *   - Computability Theorist: Analytical observer (analytical/analytical) — perceives constraint as structural limit on effective procedures
 *   - Categorical Logician: Analytical observer (analytical/analytical) — perceives constraint as categorical invariant
 *   - Semantic Philosopher: Analytical observer (analytical/analytical) — perceives constraint as feature of meaning and reference
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
narrative_ontology:constraint_metric(tarski_undefinability_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tarski_undefinability_theorem, mountain).
narrative_ontology:human_readable(tarski_undefinability_theorem, "Tarski's Undefinability Theorem").
narrative_ontology:topic_domain(tarski_undefinability_theorem, "mathematical_logic/foundational_mathematics").

domain_priors:emerges_naturally(tarski_undefinability_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MATHEMATICAL LOGICIAN (MOUNTAIN) — From the foundational mathematics perspective, Tarski's theorem is an immutable mathematical fact. No degree of freedom exists: sufficiently expressive formal systems (PA, ZFC) cannot contain their own truth predicate. This is not a contingent institutional arrangement or a policy choice — it follows necessarily from Gödel completeness and the Liar's paradox structure. The constraint appears as a pure mathematical limit.
constraint_indexing:constraint_classification(tarski_undefinability_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPUTABILITY THEORIST (MOUNTAIN) — From the recursion-theoretic view, the undefinability result maps to the halting problem and Rice's theorem: no effective procedure can decide truth in the metalanguage from within the object language. This is a structural limit on computation itself, not a limitation of any particular system or approach. The constraint holds across all Turing-complete formalisms.
constraint_indexing:constraint_classification(tarski_undefinability_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: CATEGORICAL LOGICIAN (MOUNTAIN) — From topos theory and category-theoretic foundations, the constraint appears as a categorical invariant: the forgetful functor from a category of models to its syntax has no section that preserves truth in all instances. This is a theorem about the structure of logical categories, not a property of any single formal system. The constraint is intrinsic to the category-theoretic architecture itself.
constraint_indexing:constraint_classification(tarski_undefinability_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: SEMANTIC PHILOSOPHER (MOUNTAIN) — From philosophy of language and model theory, Tarski's theorem reveals a fundamental asymmetry: semantic concepts (truth, reference, satisfaction) belong to the metalanguage by necessity, not by choice. No linguistic community can bootstrap their way out of this constraint by adopting new conventions or redefining terms. The gap between object language and metalanguage is not a failure of current systems but a structural feature of meaning itself.
constraint_indexing:constraint_classification(tarski_undefinability_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
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
 *   Extractiveness (0.08): Minimal. Tarski's theorem creates no extraction — no agent captures rent or value from other agents through the undefinability constraint. The theorem is a pure structural fact about the limits of formal systems. Suppression (0.02): Negligible. There is no coercive enforcement, no suppression of alternatives, no lack of exit options. The constraint is not enforced but discovered. Theater ratio (0.15): Low. Mathematical proofs of undefinability are direct and functionally complete — the diagonalization argument is transparent, the structure is clear, and there is minimal performative activity. The slight theater (not zero) reflects only the pedagogical presentation required to communicate a complex abstract result to human minds. The constraint itself has zero theatrical content.
 *
 * PERSPECTIVAL GAP:
 *   This is a uniform-type constraint: all perspectives yield Mountain classification. The gap exists not in classification type but in the descriptive frame used to articulate the same mathematical fact. The logician emphasizes incompleteness of self-reference; the computability theorist emphasizes undecidability of halting; the categorical logician emphasizes structural limits of forgetful functors; the semantic philosopher emphasizes the irreducible gap between language and metalanguage. These are four windows into the same invariant mathematical structure, not four different constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation is not applicable to this constraint because all perspectives have agent_power='analytical' and exit_options='analytical'. No agent experiences the constraint as extraction or coordination. The constraint is observer-invariant across all measurement bases and all indexical positions. There are no beneficiaries or victims — the undefinability is a shared structural fact that all reasoning agents must accept equally.
 *
 * MANDATROPHY ANALYSIS:
 *   NATURAL LAW CONFIRMED: Tarski's undefinability theorem is a genuine mountain — a mathematical necessary truth with zero degrees of freedom. No observational basis, no alternative framing, no policy choice can alter the core result: sufficiently expressive systems cannot contain their own truth predicate. The theorem holds in classical logic, intuitionistic logic, linear logic, and (in appropriately restated form) in paraconsistent frameworks. The accessibility_collapse (0.92) reflects that the constraint is fully accessible to any competent mathematical logician; the resistance (0.08) reflects only the cognitive difficulty of proof presentation, not structural resistance to understanding. This constraint serves as a canonical example of a pure mathematical mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    weak_vs_strong_expressiveness,
    'Does the undefinability constraint apply equally to weakly expressive systems (propositional logic) versus strongly expressive systems (PA, ZFC)?',
    'Formal derivation of Tarskian diagonalization for subsystems of varying expressive power; demonstration of where the constraint first becomes operative',
    'If the constraint requires high expressiveness: weaker systems might admit self-contained truth predicates (changing the scope of the mountain). If it applies universally: the mountain is broader than typically understood.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(weak_vs_strong_expressiveness, empirical, 'Expressiveness threshold for undefinability to become operative').

omega_variable(
    paraconsistent_escape,
    'Do paraconsistent or multi-valued logics genuinely escape Tarski''s undefinability, or do they restate the constraint in different formal vocabulary?',
    'Formal analysis of whether paraconsistent truth predicates avoid diagonalization or merely suppress contradiction; proof-theoretic comparison of standard and paraconsistent versions',
    'If truly escape: the constraint is logic-relative, not universal (downgrade from mountain). If restate: the constraint is universal across logical frameworks (mountain confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paraconsistent_escape, conceptual, 'Whether paraconsistent logic escapes undefinability or restates it').

omega_variable(
    partial_truth_definition,
    'Can a formal system define truth for a proper subset of its sentences (e.g., Horn clauses, decidable predicates) without falling into the undefinability trap?',
    'Examination of restricted truth predicates in subsystems; determination of whether restriction is compatible with the logical power needed for meaningful discourse',
    'If yes: the constraint is about totality, and partial solutions exist (constraint degrades to Tangled Rope or Rope). If no: even partial truth is undefinable (confirms mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partial_truth_definition, empirical, 'Whether restricted truth predicates escape undefinability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tarski_undefinability_theorem, 1931, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tarski_tr_t1931, tarski_undefinability_theorem, theater_ratio, 1931, 0.05).
narrative_ontology:measurement(tarski_tr_t1960, tarski_undefinability_theorem, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(tarski_tr_t2000, tarski_undefinability_theorem, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(tarski_be_t1931, tarski_undefinability_theorem, base_extractiveness, 1931, 0.08).
narrative_ontology:measurement(tarski_be_t1960, tarski_undefinability_theorem, base_extractiveness, 1960, 0.08).
narrative_ontology:measurement(tarski_be_t2000, tarski_undefinability_theorem, base_extractiveness, 2000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tarski_undefinability_theorem, information_standard).
narrative_ontology:affects_constraint(tarski_undefinability_theorem, godel_incompleteness_first).
narrative_ontology:affects_constraint(tarski_undefinability_theorem, halting_problem_undecidability).
narrative_ontology:affects_constraint(tarski_undefinability_theorem, godel_incompleteness_second).

% DUAL FORMULATION NOTE:
% Tarski's undefinability is upstream of Gödel's incompleteness theorems (which use similar diagonalization methods) and of the halting problem. The undefinability of truth is a more fundamental constraint than any specific unprovability or undecidability result — it establishes the existential gap that those theorems exploit. All three form a constraint family linked by shared diagonalization structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
