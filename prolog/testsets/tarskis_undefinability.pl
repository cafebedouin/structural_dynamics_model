% ============================================================================
% CONSTRAINT STORY: tarskis_undefinability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tarskis_undefinability, []).

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
 *   constraint_id: tarskis_undefinability
 *   human_readable: Tarski's Undefinability of Truth
 *   domain: mathematical_logic/semantics
 *
 * SUMMARY:
 *   Tarski's undefinability theorem (1936) states that no sufficiently
 *   complex formal system can define a truth predicate for its own language
 *   that is both consistent and complete. The result is a mathematical
 *   theorem with a rigorous proof, not an institutional constraint or social
 *   mechanism. Yet it functions as a constraint on what any formal system can
 *   express about itself. This constraint exhibits the signature of a
 *   Mountain in the Deferential Realism framework: it emerges naturally from
 *   logical structure (not imposed externally), it is resistant to
 *   circumvention (accessibility collapse 0.92), and it provides zero degrees
 *   of freedom to any agent — the mathematician, the logician, the AI
 *   designer, and the formal system itself all experience it as an immutable
 *   boundary. The constraint has never been relaxed, never been successfully
 *   evaded, and is universally applicable across all formal systems meeting
 *   minimal complexity thresholds.
 *
 * KEY AGENTS:
 *   - Formal Systems: The primary 'target' of the constraint (powerless/trapped) — they cannot define their own truth predicate and remain consistent
 *   - Logicians and Mathematicians: Professional agents (institutional/analytical) — treat Tarski's theorem as a natural law and build their practice around it
 *   - Philosophers of Language: Seekers of alternatives (moderate/constrained) — attempt paraconsistent logics and non-standard semantics but ultimately accept the undefinability result
 *   - Computational Systems: Agents implementing formal reasoning (powerful/mobile) — encounter undefinability as a hard limit when attempting self-reflection on their own truth claims
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the theorem as a fundamental truth about the structure of formal language and logical consistency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tarskis_undefinability, 0.12).
domain_priors:suppression_score(tarskis_undefinability, 0.03).
domain_priors:theater_ratio(tarskis_undefinability, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tarskis_undefinability, extractiveness, 0.12).
narrative_ontology:constraint_metric(tarskis_undefinability, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(tarskis_undefinability, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tarskis_undefinability, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(tarskis_undefinability, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tarskis_undefinability, mountain).
narrative_ontology:human_readable(tarskis_undefinability, "Tarski's Undefinability of Truth").
narrative_ontology:topic_domain(tarskis_undefinability, "mathematical_logic/semantics").

domain_priors:emerges_naturally(tarskis_undefinability).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FORMALIST (MOUNTAIN) — A system that attempts to formalize truth within its own axioms encounters Tarski's result as an insurmountable logical barrier. No formal system of sufficient complexity can define its own truth predicate consistently. The formalist sees this as an immutable feature of logical structure, not a constraint imposed by external agents. Zero degrees of freedom.
constraint_indexing:constraint_classification(tarskis_undefinability, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — Tarski's undefinability emerges directly from Gödel's fixed-point argument applied to semantic self-reference. The constraint is a logical theorem, not a social mechanism. It holds universally across all formal systems meeting the complexity threshold. No measurement basis or observable dependency can change the result.
constraint_indexing:constraint_classification(tarskis_undefinability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL COMMUNITY (MOUNTAIN) — Professional mathematicians treat Tarski's theorem as a natural law of formal systems, not as an extractive constraint or coordination problem. No institutional mechanism, grant system, or research program can circumvent the undefinability result. The theorem has been proven and the proof is airtight.
constraint_indexing:constraint_classification(tarskis_undefinability, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AI SYSTEM DESIGNER (MOUNTAIN) — A computational system designed to reason about truth must accept Tarski's constraint as a hard limit. A sufficiently advanced AI system cannot bootstrap its own truth predicate in a way that remains consistent with its object-level reasoning. This is not a limitation imposed by humans; it is a logical boundary on coherent reasoning itself.
constraint_indexing:constraint_classification(tarskis_undefinability, mountain,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LOGICIAN SEEKING WORKAROUNDS (MOUNTAIN) — Despite intense research into paraconsistent logics, many-valued semantics, and non-standard models, the core undefinability result persists. Workarounds change the framework (three-valued logic, hierarchy of truth predicates) but do not eliminate the underlying constraint: no single unified truth predicate exists for the language itself. The boundary moves but remains.
constraint_indexing:constraint_classification(tarskis_undefinability, mountain,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tarskis_undefinability_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(tarskis_undefinability, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tarskis_undefinability, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tarskis_undefinability, ExtMetricName, E),
    domain_priors:suppression_score(tarskis_undefinability, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tarskis_undefinability),
    narrative_ontology:constraint_metric(tarskis_undefinability, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tarskis_undefinability, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tarskis_undefinability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. Tarski's theorem does not extract resources or advantage from any agent. It places a boundary on what can be achieved, but there is no beneficiary gaining at the expense of a victim. The low value reflects that this is a structural limitation, not an extraction mechanism. Suppression (0.03): Minimal. The undefinability result does not suppress alternatives through coercion or asymmetric information. It is a logical fact that can be stated explicitly. Theater ratio (0.08): Minimal. The proof of Tarski's theorem is direct and non-performative. There is no ritual or ceremony around the constraint — it is simply true. Accessibility collapse (0.92): Extremely high. Once the theorem is stated and proven, there is no way to access a state where Tarski's undefinability does not hold. Every sufficiently complex formal system encounters this boundary. Resistance (0.08): Very low. The theorem cannot be resisted through political action, institutional reform, or alternative frameworks. Alternative logics (paraconsistent, many-valued) acknowledge undefinability in their own form rather than evading it.
 *
 * PERSPECTIVAL GAP:
 *   All five perspectives classify identically as Mountain because the constraint is truly uniform-type: it holds universally across all observables and all observers. The formalist experiences undefinability as a logical boundary; the analytical observer sees it as a theorem; the mathematical community treats it as a natural law; the AI designer encounters it as a limit on self-reference; and the logician seeking workarounds finds that workarounds preserve the core result in transformed coordinates. There is no perspectival gap because the constraint is mathematically invariant — no change of measurement basis, observer position, or temporal horizon can make Tarski's theorem false. This uniformity is the diagnostic signature of a Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Tarski's undefinability does not create directionality in the traditional sense because there are no beneficiaries and no victims. No agent extracts from another; instead, all agents encounter the same logical boundary regardless of their power, position, or resources. The constraint is perfectly symmetric across all observational positions. The 'd' parameter (directionality) would be set to 0.50 (symmetric — no asymmetric extraction) for all perspectives, but this is descriptive rather than explanatory. The mountain classification is independent of directionality: the constraint is immutable and natural-emergent regardless of agent positions.
 *
 * MANDATROPHY ANALYSIS:
 *   Tarski's undefinability is a constraint with zero mandatrophy risk because it is uniformly classified across all perspectives and all indices. There is no tension between coordination function and extraction because there is no extraction and no coordination function — the constraint is purely structural. The question 'Is this extraction or coordination?' does not arise because neither term applies. Tarski's theorem is a boundary condition, not a social mechanism. The constraint's truthfulness is not contested and has been proven beyond reasonable doubt in formal mathematics. The only remaining uncertainties (the omegas) concern conceptual and preference-based questions about alternative semantic frameworks, not empirical questions about whether the undefinability holds in classical formal systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semantic_hierarchy_necessity,
    'Is the hierarchy of truth predicates (truth in L, truth in L^(1), truth in L^(2), ...) a genuine feature of formal semantics or a byproduct of the specific formalization chosen?',
    'Examine whether alternative foundations (category theory, type-theoretic hierarchies, or quantum logic) produce non-hierarchical truth structures; test whether the hierarchy is a theorem or a definition.',
    'If the hierarchy is essential: undefinability is a deep structural fact. If it is a choice: undefinability reflects formalism selection, not logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(semantic_hierarchy_necessity, conceptual, 'Whether truth hierarchies are necessary or conventional').

omega_variable(
    self_reference_inevitability,
    'Is self-referential truth (the sentence ''this sentence is true'') an inevitable feature of sufficiently expressive languages, or can languages avoid self-reference by design?',
    'Formal proof that any language rich enough to express Peano arithmetic can construct self-referential sentences; or construction of a restricted language that cannot self-reference and remains useful.',
    'If self-reference is inevitable: Tarski''s result is unavoidable for any sufficiently powerful language. If it can be restricted: languages avoiding self-reference might define their own truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(self_reference_inevitability, empirical, 'Whether self-reference is inevitable in expressive languages').

omega_variable(
    external_semantics_sufficiency,
    'Can the undefinability problem be fully dissolved by always treating truth as an external (meta-level) predicate, never attempting self-application?',
    'Test whether the hierarchy of languages (object language, metalanguage, meta-metalanguage) can be formalized in a way that treats truth as fundamentally external; assess whether practical mathematics and computer science benefit from this division or require unified semantics.',
    'If external semantics is sufficient: undefinability is only a problem for systems demanding internal self-closure. If unified semantics are necessary: undefinability is a real foundational constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_semantics_sufficiency, preference, 'Whether external semantics can dissolve the undefinability problem').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tarskis_undefinability, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tars_tr_t0, tarskis_undefinability, theater_ratio, 0, 0.08).
narrative_ontology:measurement(tars_tr_t50, tarskis_undefinability, theater_ratio, 50, 0.08).
narrative_ontology:measurement(tars_tr_t100, tarskis_undefinability, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(tars_be_t0, tarskis_undefinability, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(tars_be_t50, tarskis_undefinability, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(tars_be_t100, tarskis_undefinability, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tarskis_undefinability, information_standard).
narrative_ontology:affects_constraint(tarskis_undefinability, godel_incompleteness).
narrative_ontology:affects_constraint(tarskis_undefinability, halting_problem).
narrative_ontology:affects_constraint(tarskis_undefinability, liar_paradox_formalization).

% DUAL FORMULATION NOTE:
% Tarski's undefinability is downstream of Gödel's incompleteness theorems and the formal treatment of self-reference. It shares the same foundational mechanism (Gödel's fixed-point argument) with incompleteness but applies specifically to the semantic notion of truth rather than provability. Each constraint in this family has its own extractiveness and suppression values, but all share the mountain classification and the same natural-law emergence pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
