% ============================================================================
% CONSTRAINT STORY: context_dependent_concept_function
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_context_dependent_concept_function, []).

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
 *   constraint_id: context_dependent_concept_function
 *   human_readable: Context-Dependent Concept Function in Discourse
 *   domain: philosophy_of_language/social_epistemology/discourse_analysis
 *
 * SUMMARY:
 *   Context-dependent concept function is the phenomenon that the same
 *   lexical concept performs structurally different cognitive operations
 *   depending on social context. When you think 'I am brave' in private
 *   reflection, the concept 'brave' functions as a self-assessment tool — it
 *   carves your action space, identifies which behaviors are consistent with
 *   your self-model, and guides future choices. When you say 'I am brave' to
 *   an audience that rewards bravery claims, the same lexical item performs a
 *   different operation: it signals group membership, claims status, or
 *   coordinates expectations. The concept's extension (which actions count as
 *   brave) and intension (what bravery means) shift systematically between
 *   these contexts. This is not a bug in language or a failure of semantic
 *   precision — it is a structural feature of how natural language mediates
 *   between private cognition and public coordination. The constraint appears
 *   as a mountain from all perspectives because it is a consequence of the
 *   dual function concepts must serve: they must be stable enough to enable
 *   coordination (public function) yet flexible enough to track individual
 *   cognitive states (private function). Formal languages eliminate this by
 *   stipulating fixed denotations, but only by sacrificing the flexibility
 *   that makes natural language useful for social life. The constraint has
 *   identifiable beneficiaries (strategic communicators who exploit concept
 *   function shifts; discourse analysts who study them) but these agents do
 *   not extract from the constraint — they observe and utilize a natural
 *   phenomenon. The omega variable 'beneficiary_extraction_mechanism'
 *   addresses whether this constitutes a false summit.
 *
 * KEY AGENTS:
 *   - Naive Language User: Powerless agent (powerless/trapped) — experiences concept function shifts as immutable, no awareness of the structural pattern
 *   - Reflective Speaker: Moderate agent (moderate/constrained) — recognizes the pattern but experiences it as unchangeable natural law
 *   - Linguistic Community: Organized collective (organized/mobile) — can develop conventions but cannot eliminate the underlying constraint
 *   - Strategic Communicators: Institutional beneficiaries (institutional/arbitrage) — exploit concept function shifts for persuasion; benefit from understanding the constraint but do not create or maintain it
 *   - Discourse Analysts: Institutional beneficiaries (institutional/arbitrage) — study concept function shifts; benefit from the constraint's existence as an object of analysis
 *   - Analytical Observer: Meta-level perspective (analytical/analytical) — recognizes the constraint as a structural invariant of natural language interfacing with social cognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(context_dependent_concept_function, 0.08).
domain_priors:suppression_score(context_dependent_concept_function, 0.03).
domain_priors:theater_ratio(context_dependent_concept_function, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(context_dependent_concept_function, extractiveness, 0.08).
narrative_ontology:constraint_metric(context_dependent_concept_function, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(context_dependent_concept_function, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(context_dependent_concept_function, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(context_dependent_concept_function, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(context_dependent_concept_function, mountain).
narrative_ontology:human_readable(context_dependent_concept_function, "Context-Dependent Concept Function in Discourse").
narrative_ontology:topic_domain(context_dependent_concept_function, "philosophy_of_language/social_epistemology/discourse_analysis").

domain_priors:emerges_naturally(context_dependent_concept_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(context_dependent_concept_function, strategic_communicators).
narrative_ontology:constraint_beneficiary(context_dependent_concept_function, discourse_analysts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NAIVE LANGUAGE USER (MOUNTAIN) — Experiences concept function shifts as immutable features of communication. Cannot exit the constraint that concepts mean different things in private thought vs public speech. No awareness that this is a structural feature rather than semantic content.
constraint_indexing:constraint_classification(context_dependent_concept_function, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: REFLECTIVE SPEAKER (MOUNTAIN) — Recognizes that they use concepts differently when thinking alone vs speaking to others, but experiences this as an unchangeable property of language itself. Constrained by social context but sees the constraint as natural law.
constraint_indexing:constraint_classification(context_dependent_concept_function, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: LINGUISTIC COMMUNITY (MOUNTAIN) — Organized language users collectively experience context-dependent concept function as a structural invariant across all natural languages. Can develop new conventions but cannot eliminate the underlying pattern that concepts perform different cognitive operations in different social contexts.
constraint_indexing:constraint_classification(context_dependent_concept_function, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: STRATEGIC COMMUNICATORS / DISCOURSE ANALYSTS (MOUNTAIN) — Benefit from understanding the constraint: can exploit concept function shifts for persuasion, detect them for analysis. But even with full awareness and institutional resources, cannot eliminate the underlying pattern. The constraint is a feature of how concepts interface with social cognition, not a contingent institutional arrangement.
constraint_indexing:constraint_classification(context_dependent_concept_function, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — Recognizes context-dependent concept function as a structural invariant of natural language. The same lexical item performs different cognitive operations depending on: (1) presence/absence of audience, (2) reward structure for application, (3) social accountability for usage. This is not a contingent feature of particular languages or cultures but a consequence of how concepts mediate between private cognition and public coordination. Formal languages can eliminate this (by stipulating fixed denotations) but only by sacrificing the flexibility that makes natural language useful for social coordination.
constraint_indexing:constraint_classification(context_dependent_concept_function, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(context_dependent_concept_function_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(context_dependent_concept_function, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(context_dependent_concept_function, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(context_dependent_concept_function, ExtMetricName, E),
    domain_priors:suppression_score(context_dependent_concept_function, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(context_dependent_concept_function),
    narrative_ontology:constraint_metric(context_dependent_concept_function, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(context_dependent_concept_function, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(context_dependent_concept_function_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint imposes minimal extraction because it is a structural feature of how concepts work, not an imposed cost. The 'cost' of context-dependent concept function is that speakers must track social context when using concepts, but this is inherent to the dual function concepts serve (private cognition + public coordination), not an extractive overhead. Strategic communicators and discourse analysts benefit from understanding the constraint, but their advantage does not constitute extraction — anyone can learn to recognize concept function shifts. The extractiveness value reflects the minimal cognitive overhead of context-tracking, not asymmetric extraction. Suppression (0.03): Minimal. Agents are not prevented from recognizing or discussing context-dependent concept function. The constraint is not maintained by active suppression of alternatives — it is a structural feature of natural language. Formal languages provide an alternative (fixed denotations) but at the cost of flexibility, not because natural language suppresses formal alternatives. Accessibility collapse (0.92): Very high. The constraint is highly accessible to analysis once pointed out. Reflective speakers recognize it immediately ('I do use words differently when I'm alone vs when I'm talking to others'). The pattern is observable in everyday discourse and has been documented across languages and cultures. Resistance (0.08): Very low. The constraint shows minimal resistance to analysis. Once the pattern is named, it is easily recognized and studied. Discourse analysis, pragmatics, and social epistemology have well-developed frameworks for analyzing context-dependent concept function. Theater ratio (0.15): Low. There is minimal performative content. The constraint is not maintained by ritual or theater — it is a direct consequence of how concepts interface with social cognition. The low theater reflects that the constraint is functional (concepts genuinely perform different operations in different contexts) rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all perspectives classify as mountain. The gap that does exist is in awareness, not in classification. The naive language user experiences the constraint without recognizing it as a structural pattern. The reflective speaker recognizes the pattern but still experiences it as immutable. The linguistic community recognizes the pattern as a collective feature. Strategic communicators and discourse analysts recognize the pattern and benefit from understanding it, but still cannot eliminate it. The analytical observer recognizes the pattern as a structural invariant of natural language interfacing with social cognition. The uniform mountain classification across perspectives is diagnostic: it indicates that the constraint is genuinely a natural law, not a contingent institutional arrangement that appears as natural law from some perspectives (false summit). The omega variable 'beneficiary_extraction_mechanism' tests this: if the beneficiaries' advantage constitutes extraction, the constraint would reclassify as tangled_rope from some perspectives. Current assessment: the advantage is symmetric (anyone can learn to recognize concept function shifts), so no extraction occurs, and mountain classification holds.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives classify as mountain because the constraint is a structural invariant of natural language. Directionality values are derived from the agent's relationship to the constraint, but even beneficiaries (strategic communicators, discourse analysts) experience the constraint as immutable. Strategic communicators are beneficiaries (d ≈ 0.05) because they gain advantage from understanding concept function shifts, but they do not extract from other agents — the advantage is symmetric (anyone can learn this). Discourse analysts are beneficiaries (d ≈ 0.05) because the constraint provides an object of study, but again, this is not extraction from victims. The naive language user is neither beneficiary nor victim (d ≈ 0.50) — they simply use language without awareness of the structural pattern. The reflective speaker is also symmetric (d ≈ 0.50) — they recognize the pattern but are not disadvantaged by it. The linguistic community is symmetric (d ≈ 0.50) — the constraint is a collective feature, not imposed by some members on others. The analytical observer is symmetric (d ≈ 0.72, canonical analytical fallback) — observes the constraint without being advantaged or disadvantaged by it. The low extractiveness across all perspectives reflects that this is a genuine natural law, not a false summit naturalizing contingent extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that beneficiaries can exist without extraction. Strategic communicators and discourse analysts benefit from understanding context-dependent concept function, but their advantage does not constitute extraction because: (1) the constraint is not maintained by their actions — it is a structural feature of natural language that would exist even if no one studied or exploited it; (2) the advantage is symmetric — anyone can learn to recognize concept function shifts; (3) no victims exist — naive language users are not disadvantaged by others' understanding of the constraint. The constraint is a genuine mountain (natural law) that happens to be more useful to some agents than others, but usefulness does not equal extraction. The false summit detector will evaluate whether the declared beneficiaries trigger reclassification. Current hypothesis: they do not, because the beneficiary advantage is observational rather than extractive. If empirical analysis reveals that strategic communicators create or amplify concept function shifts (rather than merely observing them), the constraint would reclassify as tangled_rope — genuine coordination (concepts enable communication) with embedded extraction (strategic manipulation of concept function). The omega variable 'beneficiary_extraction_mechanism' captures this irreducible uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_extraction_mechanism,
    'Do strategic communicators and discourse analysts extract value from this constraint, or merely observe a natural phenomenon?',
    'Analysis of whether understanding context-dependent concept function creates asymmetric advantage that constitutes extraction, or whether the advantage is symmetric (anyone can learn this) and thus not extractive.',
    'If extractive: constraint is a false summit (tangled_rope from some perspectives). If non-extractive: genuine mountain — beneficiaries exist but do not extract.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_extraction_mechanism, conceptual, 'Whether beneficiary advantage constitutes extraction or symmetric learning').

omega_variable(
    formal_language_exit,
    'Do formal languages (mathematics, logic, programming) constitute genuine exit from context-dependent concept function, or merely trade one constraint for another?',
    'Comparative analysis of concept stability in formal vs natural language contexts; assessment of whether formal languages eliminate context-dependence or relocate it to meta-level (choice of formalism, interpretation of symbols).',
    'If genuine exit exists: constraint is not universal mountain but domain-specific rope. If exit is illusory: mountain classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_language_exit, empirical, 'Whether formal languages provide exit from context-dependent concept function').

omega_variable(
    developmental_invariance,
    'Is context-dependent concept function present in early language acquisition, or does it emerge through socialization?',
    'Developmental psycholinguistic studies tracking concept application patterns in children: do toddlers already show self-directed vs other-directed usage differences, or does this emerge later?',
    'If present from acquisition: stronger evidence for mountain (cognitive universal). If learned through socialization: evidence for rope or tangled_rope (contingent social norm).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developmental_invariance, empirical, 'Developmental timeline of context-dependent concept function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(context_dependent_concept_function, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cdcf_tr_t0, context_dependent_concept_function, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cdcf_tr_t500, context_dependent_concept_function, theater_ratio, 500, 0.15).
narrative_ontology:measurement(cdcf_tr_t1000, context_dependent_concept_function, theater_ratio, 1000, 0.15).

% Extraction over time
narrative_ontology:measurement(cdcf_be_t0, context_dependent_concept_function, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(cdcf_be_t500, context_dependent_concept_function, base_extractiveness, 500, 0.08).
narrative_ontology:measurement(cdcf_be_t1000, context_dependent_concept_function, base_extractiveness, 1000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(context_dependent_concept_function, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a singleton — it does not decompose into multiple stories with different epsilon values. The same structural phenomenon (context-dependent concept function) has the same extractiveness regardless of which observable is used to evaluate it. Concept application patterns in self-directed vs other-directed usage, costly vs costless applications, and private vs public contexts all reveal the same underlying constraint with the same epsilon (0.08). This is diagnostic of a genuine mountain: the constraint is invariant across observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
