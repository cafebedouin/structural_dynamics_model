% ============================================================================
% CONSTRAINT STORY: yoneda_lemma
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_yoneda_lemma, []).

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
 *   constraint_id: yoneda_lemma
 *   human_readable: Yoneda Lemma Adherence in Mathematical Research
 *   domain: mathematics/category_theory
 *
 * SUMMARY:
 *   The Yoneda Lemma, proved by Nobuo Yoneda in 1954 and formalized within
 *   category theory, is a foundational theorem stating that an object in a
 *   category is completely determined by its relationships (morphisms) to all
 *   other objects. Equivalently, the representable functor Hom(−, X)
 *   completely determines the object X. This constraint operates as a natural
 *   law within mathematics: it is not enforced through coercion or social
 *   pressure but emerges as a logical necessity from the structure of
 *   categories themselves. The constraint is invariant across all
 *   mathematical observers and frameworks that adopt categorical reasoning.
 *   Extractiveness is minimal (0.12) because the constraint provides no
 *   asymmetric benefit — all mathematicians who use category theory benefit
 *   equally from Yoneda's structural insights. Suppression is negligible
 *   (0.03) because mathematicians have complete freedom to adopt or ignore
 *   categorical frameworks depending on their research needs. Theater ratio
 *   is low (0.15) because Yoneda's statement and proof are mathematically
 *   precise with no performative component — the constraint is operationally
 *   transparent.
 *
 * KEY AGENTS:
 *   - Individual Category Theorist (analytical/trapped) — bears the constraint that all categorical reasoning must respect Yoneda structure; no escape within categorical framework
 *   - Mathematical Community (institutional/arbitrage) — benefits from Yoneda's unifying power across diverse mathematical domains; institution cannot negotiate the truth of the theorem
 *   - Research Programs Outside Category Theory (powerful/mobile) — freely ignore or adopt Yoneda depending on research needs; full exit option; experience constraint as pure coordination opportunity
 *   - Analytical Observer (analytical/analytical) — sees Yoneda as a universal logical truth independent of human convention or institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(yoneda_lemma, 0.12).
domain_priors:suppression_score(yoneda_lemma, 0.03).
domain_priors:theater_ratio(yoneda_lemma, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(yoneda_lemma, extractiveness, 0.12).
narrative_ontology:constraint_metric(yoneda_lemma, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(yoneda_lemma, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(yoneda_lemma, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(yoneda_lemma, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(yoneda_lemma, mountain).
narrative_ontology:human_readable(yoneda_lemma, "Yoneda Lemma Adherence in Mathematical Research").
narrative_ontology:topic_domain(yoneda_lemma, "mathematics/category_theory").

domain_priors:emerges_naturally(yoneda_lemma).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (MOUNTAIN) — The Yoneda Lemma is a mathematical theorem proven within category theory. From a civilizational/universal viewpoint, it represents a logical necessity: if an object's identity is constituted by its relationships to all other objects in a category, then any morphism-preserving functor completely determines the object. This is not a contingent fact about mathematical practice but a structural feature of how category theory models identity. No observer can escape this constraint through choice or negotiation.
constraint_indexing:constraint_classification(yoneda_lemma, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: INDIVIDUAL CATEGORY THEORIST (MOUNTAIN) — A mathematician working within category theory cannot escape the Yoneda Lemma's logical consequences. Any proof or construction that relies on category-theoretic reasoning must respect Yoneda's constraint that objects are determined by their hom-sets. A theorist might choose different objects or categories, but within a chosen categorical framework, the constraint is binding and inescapable. The constraint appears to this agent as a natural law of the mathematical universe they inhabit.
constraint_indexing:constraint_classification(yoneda_lemma, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MATHEMATICAL COMMUNITY (MOUNTAIN) — Institutionally, the Yoneda Lemma is an established theorem taught in graduate mathematics curricula worldwide. Its truth is not subject to institutional negotiation, reform, or alternative frameworks — it either holds or it does not. The mathematical community benefits from Yoneda (it enables profound structural insights and unifies disparate areas), but the community cannot choose whether Yoneda is true any more than physicists can choose whether the speed of light is constant. The constraint emerges as a natural law that the institution discovers and transmits.
constraint_indexing:constraint_classification(yoneda_lemma, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RESEARCH PROGRAM OUTSIDE CATEGORY THEORY (ROPE) — A mathematician or computer scientist working in a domain that does not directly invoke category theory (e.g., combinatorics, numerical analysis, applied statistics) experiences the Yoneda Lemma as a pure coordination mechanism: it is available as a tool when useful, can be ignored when irrelevant, and poses no extraction cost. The constraint is experienced as the opportunity structure of mathematics rather than as a burden. Exit options are entirely open — one can work productively in mathematics without ever invoking Yoneda.
constraint_indexing:constraint_classification(yoneda_lemma, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(yoneda_lemma_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(yoneda_lemma, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(yoneda_lemma, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(yoneda_lemma, ExtMetricName, E),
    domain_priors:suppression_score(yoneda_lemma, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(yoneda_lemma),
    narrative_ontology:constraint_metric(yoneda_lemma, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(yoneda_lemma, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(yoneda_lemma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The Yoneda Lemma provides no mechanism for asymmetric extraction — all users of category theory benefit equally from the theorem's insights about object identity and structure. There is no scarcity that Yoneda creates or controls, no privileged access, no group whose benefit comes at another group's cost. The small non-zero value (rather than exactly 0.0) reflects that some mathematicians spend cognitive effort learning Yoneda while others benefit from results derived using it, creating a microscopic asymmetry in knowledge labor. However, this asymmetry is entirely voluntary and reversible — no agent is trapped. Suppression (0.03): Negligible. The Yoneda Lemma places no restrictions on alternatives. Mathematicians outside category theory face zero suppression of alternative methods. Even within category theory, Yoneda does not suppress other techniques — it coexists with diverse proof strategies. The small non-zero value reflects only that mastering Yoneda requires cognitive effort, a trivial form of friction. Theater ratio (0.15): Low. The Yoneda Lemma is stated as a precise mathematical theorem with a complete proof. There is no performative component, no ritual maintenance, no rhetorical work required to sustain it. The modest non-zero value reflects only minor pedagogical theater — some textbooks emphasize Yoneda's philosophical significance beyond its mathematical necessity, adding slight theatrical framing to an otherwise austere logical statement.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is minimal, reflecting Yoneda's status as a natural law. All four perspectives classify as mountain or rope, with no conflict. The analytical observer sees a universal logical necessity. The category theorist sees an inescapable constraint within their chosen framework. The mathematical community sees an established truth. The external researcher sees a freely available tool. These are not genuinely different classifications — they are different expressions of the same underlying structure. The rope perspective (research outside category theory) is not a conflicting classification but a limiting case: the constraint simply does not apply to this agent because they operate outside its domain. This is characteristic of mountains with limited scope — they are invariant within their domain and irrelevant outside it, producing no perspectival tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not meaningfully defined for the Yoneda Lemma because there are no beneficiaries or victims. All agents who engage with the constraint benefit equally — it is pure coordination with zero asymmetry. The minimal extractiveness (0.12) does not flow directionally from one group to another; it is distributed evenly. This is consistent with mountain classification: no agent bears costs that another agent captures. The constraint is not a mechanism of wealth or power transfer but a logical truth that enables all mathematical work equally.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    yoneda_necessity_vs_contingency,
    'Is the Yoneda Lemma a necessary truth (theorem provable from category-theoretic axioms) or a contingent discovery (true in one framework but potentially false in alternative formal systems)?',
    'Metamathematical analysis: proof of Yoneda from first-order axioms of category theory; exploration of non-categorical formal systems that might violate Yoneda-like principles; comparison with intuitionistic vs classical logic variants',
    'If necessary: Yoneda is a mountain for all possible mathematical frameworks. If contingent: Yoneda is a rope in frameworks that adopt it, but not binding in alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(yoneda_necessity_vs_contingency, conceptual, 'Whether Yoneda is logically necessary or contingent on axiomatic choice').

omega_variable(
    yoneda_operational_scope,
    'How much of working mathematical practice actually relies on explicit Yoneda reasoning versus implicit relationship-based reasoning that happens to align with Yoneda structure?',
    'Corpus analysis of published proofs: percentage of major theorems that explicitly invoke Yoneda vs achieve equivalent results through other methods; survey of mathematicians on whether Yoneda shapes their intuition or remains abstract',
    'If most practice implicitly respects Yoneda: the constraint is deeply embedded, a mountain. If most practice bypasses Yoneda: it is a niche tool, more rope than mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(yoneda_operational_scope, empirical, 'The scope of Yoneda''s influence on actual mathematical practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(yoneda_lemma, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(yoneda_tr_t0, yoneda_lemma, theater_ratio, 0, 0.1).
narrative_ontology:measurement(yoneda_tr_t25, yoneda_lemma, theater_ratio, 25, 0.14).
narrative_ontology:measurement(yoneda_tr_t50, yoneda_lemma, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(yoneda_be_t0, yoneda_lemma, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(yoneda_be_t25, yoneda_lemma, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(yoneda_be_t50, yoneda_lemma, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(yoneda_lemma, information_standard).
narrative_ontology:affects_constraint(yoneda_lemma, functor_naturality).
narrative_ontology:affects_constraint(yoneda_lemma, category_homology).
narrative_ontology:affects_constraint(yoneda_lemma, representable_functors).

% DUAL FORMULATION NOTE:
% The Yoneda Lemma is a foundational constraint that structures how other category-theoretic results are formulated and proved. Related constraints like functor naturality and representable functors are downstream applications of Yoneda's structural insight. These do not decompose the Yoneda constraint itself but rather elaborate its consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
