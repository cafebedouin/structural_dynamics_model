% ============================================================================
% CONSTRAINT STORY: kirby_paris_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kirby_paris_theorem, []).

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
 *   constraint_id: kirby_paris_theorem
 *   human_readable: The Kirby-Paris Theorem (Independence of Goodstein's Theorem)
 *   domain: mathematical_logic/proof_theory
 *
 * SUMMARY:
 *   The Kirby-Paris theorem establishes that Goodstein's theorem—a statement
 *   asserting that certain sequences of natural numbers, built by a specific
 *   iterative process (base bumping followed by subtraction), always
 *   terminate—is true but unprovable within Peano Arithmetic (PA). The result
 *   exemplifies a pure mathematical constraint: not a social arrangement, not
 *   an institutional bottleneck, not a coordination problem, but a logical
 *   limit intrinsic to formal systems. Goodstein's theorem is provable in
 *   stronger systems (second-order arithmetic, ZFC) and true in the standard
 *   model of natural numbers. Its PA-unprovability is not a gap waiting to be
 *   closed by cleverness or resources; it is a structural feature of what
 *   first-order arithmetic can and cannot express. This constraint
 *   demonstrates that Mountain classification is not merely about physical
 *   laws or computational limits—it applies to mathematical truths that
 *   transcend any particular formal system while remaining unprovable within
 *   specified axiomatic boundaries.
 *
 * KEY AGENTS:
 *   - Proof Seeker Within PA: Represents any agent or system constrained to PA's proof rules; cannot escape the boundary through increased effort, resources, or creativity
 *   - Mathematical Community: Institutional actor accepting the result as a natural law of mathematics; responds by deepening structural understanding rather than challenging the boundary
 *   - Proof Theory Research Coalition: Organized mathematicians (Gentzen, Simpson, et al.) developing richer formal systems; achieves understanding of the gap without evading it
 *   - Analytical Observer: Recognizes Goodstein independence as a fundamental constraint on formal systems, exemplifying Gödel incompleteness at a specific instance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kirby_paris_theorem, 0.12).
domain_priors:suppression_score(kirby_paris_theorem, 0.03).
domain_priors:theater_ratio(kirby_paris_theorem, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kirby_paris_theorem, extractiveness, 0.12).
narrative_ontology:constraint_metric(kirby_paris_theorem, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(kirby_paris_theorem, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kirby_paris_theorem, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(kirby_paris_theorem, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kirby_paris_theorem, mountain).
narrative_ontology:human_readable(kirby_paris_theorem, "The Kirby-Paris Theorem (Independence of Goodstein's Theorem)").
narrative_ontology:topic_domain(kirby_paris_theorem, "mathematical_logic/proof_theory").

domain_priors:emerges_naturally(kirby_paris_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROOF SEEKER WITHIN PA (MOUNTAIN) — An agent or system bound to PA's axioms cannot prove Goodstein's theorem, no matter the computational resources deployed. This is not a resource constraint or a coordination problem—it is a logical ceiling. The boundary is absolute: proofs in PA form a recursively enumerable set, and Goodstein's truth is simply outside this set. d≈1.0 (trapped), but f(d)≈1.42 applies to an impossible demand—the constraint is immovable, not extractive. χ≈0.15 reflects the minimal performative content (the theorem's statement is simple; its unprovability is the substance).
constraint_indexing:constraint_classification(kirby_paris_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MATHEMATICAL ANALYST / MODEL THEORY (MOUNTAIN) — From the analytical standpoint, the Kirby-Paris theorem exemplifies a fundamental constraint on formal systems themselves. Goodstein's theorem is true in the standard model of natural numbers (provable in stronger systems like ZFC or second-order arithmetic), but PA-unprovable. This boundary is a property of mathematical logic, not of psychology, economics, or politics. No agent can negotiate it; no coalition can overcome it; no institutional reform can change it. The constraint emerges from the nature of recursive enumerability and Gödel incompleteness. d≈0.72 (analytical observer), f(d)≈1.15, but the mountain gates are satisfied: accessibility_collapse≥0.85, resistance≤0.15, emerges_naturally=true. σ=1.0 (universal scope). χ≈0.12.
constraint_indexing:constraint_classification(kirby_paris_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL COMMUNITY (MOUNTAIN) — Mathematics as an institutional enterprise accepts the Kirby-Paris result as a natural law of its domain. No amount of funding, collaboration, or methodological innovation will produce a PA proof of Goodstein's theorem. The community has relocated its interest to understanding the structure of the gap: why does Goodstein's theorem require ordinal induction beyond PA? What does this tell us about the arithmetic hierarchy? The institutional response is not to challenge the constraint but to deepen understanding within it. d≈0.35 (institutional, analytical), f(d)≈0.28, σ=1.0 → χ≈0.03. The constraint is accepted as a feature of mathematics, not as extraction.
constraint_indexing:constraint_classification(kirby_paris_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: PROOF THEORY COALITION (MOUNTAIN) — Organized research programs (Gentzen, Takeuti, Simpson, and descendants) have developed proof-theoretic frameworks (ordinal notation, transfinite induction) that permit Goodstein's theorem to be stated and proven. These frameworks do not evade the Kirby-Paris boundary—they transcend it by moving to a richer formal system (second-order arithmetic, weak Kripke-Platek set theory). Within each stronger system, new independent statements emerge (Goodstein becomes provable, but higher-order generalizations become unprovable). The organized effort does not extract value from the constraint; it reorganizes the problem. d≈0.40 (organized, analytical), f(d)≈0.40, σ=1.0 → χ≈0.05. The constraint persists at every level of formal power; it is not defeated, only shifted.
constraint_indexing:constraint_classification(kirby_paris_theorem, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kirby_paris_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(kirby_paris_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kirby_paris_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kirby_paris_theorem, ExtMetricName, E),
    domain_priors:suppression_score(kirby_paris_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kirby_paris_theorem),
    narrative_ontology:constraint_metric(kirby_paris_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kirby_paris_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kirby_paris_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The Kirby-Paris theorem does not extract value from any agent or group. It is a pure logical fact: certain mathematical truths exceed the expressive power of certain formal systems. No beneficiary accrues advantage; no victim suffers disadvantage. The small non-zero value (not exactly 0.0) reflects the minimal theater involved in the theorem's proof strategy—the statement of Goodstein's theorem is simple, but the unprovability argument requires ordinal notation and technical proof-theoretic machinery. This machinery has performative content (it demonstrates understanding) but no extractive content. Suppression (0.03): Negligible. There is no coercion, no alternative suppression, no barrier to understanding (for those with mathematical training). The theorem is publicly stated, widely taught in proof theory courses, and accessible to any mathematician. Accessibility collapse (0.92): Very high. The constraint is logically absolute: there is no 'easier' version or workaround. Either a statement is PA-provable or it is not. This is a Boolean property with no intermediate states. Resistance (0.08): Very low. The constraint cannot be resisted, negotiated, or evaded within PA. It is not a barrier one can push against; it is a ceiling one encounters. Emerges naturally: Yes. The boundary follows from the definitions of first-order logic, recursive enumerability, and PA's axiom set. No institutional arrangement created this constraint; it exists in the logical structure itself.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap because all perspectives classify the constraint as Mountain. The proof seeker within PA sees an absolute ceiling; the analyst sees a fundamental property of formal systems; the mathematical community accepts it as a natural law; the organized coalition of proof theorists deepens structural understanding without evading the boundary. The 'gap' is not in classification but in how different observers respond to the constraint: a PA-bound agent experiences entrapment; the mathematical community reframes the constraint as an opportunity for deeper understanding; the research coalition translates the problem into questions about proof-theoretic strength and ordinal hierarchies. But all recognize the constraint itself as immovable. This uniformity of classification is the hallmark of a true Mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not applicable to this constraint in the usual sense because there are no beneficiaries or victims—no structural extraction or coordination. The constraint is a property of mathematical logic, not a social or institutional arrangement. If forced to assign directionality: all agents are equally 'trapped' by the boundary (d≈1.0 for anyone seeking a PA proof), but this trapped-ness is not an extraction in the DR sense. It is a fundamental constraint on what can be expressed within a formal system. The mountain classification absorbs this: the constraint is not classifiable as extraction, coordination, or coercion because it is logical necessity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_standard_model,
    'Is the standard model of natural numbers privileged by logic itself, or is its privilege a contingent choice of mathematicians?',
    'Philosophical analysis of model theory and the categoricity of second-order arithmetic. Examination of whether non-standard models have equal claim to ''naturalness'' or whether the standard model''s role is foundational.',
    'If standard model is privileged by logic: Goodstein''s theorem is ''truly true'' and the Kirby-Paris boundary is a logical fact. If privilege is conventional: Goodstein''s truth is model-dependent, and the boundary reflects a choice, not a law. This does not change the classification (still Mountain) but reframes its ontology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_standard_model, conceptual, 'Whether the standard model of natural numbers is logically privileged').

omega_variable(
    ordinal_measurement_consistency,
    'Do different ordinal notation systems (Cantor normal form, Veblen hierarchy, Bachmann-Howard ordinal) all measure the same ''height'' of Goodstein sequences, or do they yield substantively different measurements?',
    'Comparative proof-theoretic analysis of ordinal assignments to Goodstein sequences under different notation schemes. Verification that the termination property is invariant across notational systems.',
    'If consistent: ordinal height is an intrinsic property of Goodstein sequences, validating the structural boundary. If variant: ordinal assignment is a choice of proof system, and the ''natural'' strength required for Goodstein is not absolute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ordinal_measurement_consistency, empirical, 'Consistency of ordinal measurements across notation systems').

omega_variable(
    pa_augmentation_necessity,
    'Is the specific strength of PA (first-order axioms for arithmetic) necessary and sufficient for the Goodstein boundary, or would weaker or differently-structured first-order theories produce the same independence result?',
    'Proof-theoretic examination of minimal first-order theories that generate Goodstein independence. Testing whether the boundary is a property of first-order logic itself or specific to PA''s axiom set.',
    'If necessary to PA: the boundary is PA-specific, and richer first-order theories might accommodate Goodstein. If general to first-order logic: the boundary is a ceiling on what first-order systems can express, validating the universality of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pa_augmentation_necessity, empirical, 'Necessity of PA''s specific axiomatization for Goodstein independence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kirby_paris_theorem, 1974, 2074).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kp_tr_t0, kirby_paris_theorem, theater_ratio, 0, 0.1).
narrative_ontology:measurement(kp_tr_t50, kirby_paris_theorem, theater_ratio, 50, 0.15).
narrative_ontology:measurement(kp_tr_t100, kirby_paris_theorem, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(kp_be_t0, kirby_paris_theorem, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(kp_be_t50, kirby_paris_theorem, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(kp_be_t100, kirby_paris_theorem, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kirby_paris_theorem, information_standard).
narrative_ontology:affects_constraint(kirby_paris_theorem, godel_incompleteness_first_order).
narrative_ontology:affects_constraint(kirby_paris_theorem, halting_problem_decidability).

% DUAL FORMULATION NOTE:
% The Kirby-Paris theorem is downstream of Gödel's incompleteness theorems and upstream of general results about the undecidability of higher-order properties in formal systems. Goodstein's independence is one instance of a broader principle: every first-order axiom system has true statements unprovable within it. The network links show how Kirby-Paris exemplifies and extends the incompleteness principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
