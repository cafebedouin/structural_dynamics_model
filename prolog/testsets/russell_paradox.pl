% ============================================================================
% CONSTRAINT STORY: russell_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_russell_paradox, []).

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
 *   constraint_id: russell_paradox
 *   human_readable: Russell's Paradox: The Self-Referential Set Contradiction
 *   domain: mathematical_logic/foundations
 *
 * SUMMARY:
 *   Russell's Paradox is the foundational logical contradiction that emerges
 *   when unrestricted set comprehension permits the formation of the set R =
 *   {x | x ∉ x}. The question 'Is R a member of itself?' generates a logical
 *   loop: R ∈ R if and only if R ∉ R. This paradox revealed that 19th-century
 *   naive set theory (Frege, Cantor) was inconsistent and forced the
 *   development of alternative foundational frameworks (Zermelo-Fraenkel set
 *   theory, type theory, constructivism). The paradox is the canonical
 *   exemplar of an immutable logical limit — it cannot be negotiated,
 *   circumvented through increased resources, or resolved by social
 *   agreement. Every formal system that permits unrestricted self-referential
 *   totality collapses into inconsistency when the paradox is applied. This
 *   constraint exhibits the purest mountain classification: zero
 *   extractiveness, minimal suppression, no theater, and invariance across
 *   all observational contexts.
 *
 * KEY AGENTS:
 *   - Logical Truth: Immutable constraint (analytical/analytical) — the contradiction is necessarily true in any system permitting unrestricted comprehension
 *   - Formal Systems: Institutional victims (institutional/analytical) — any system claiming unrestricted expressivity is forced to accept inconsistency
 *   - Mathematical Foundations: Community constraint (institutional/analytical) — the entire field treats the paradox as an inexplicable boundary on foundational frameworks
 *   - Self-Referential Totality: Abstract constraint (analytical/analytical) — the concept of 'all sets' when allowed to reference itself generates the paradox necessarily
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(russell_paradox, 0.12).
domain_priors:suppression_score(russell_paradox, 0.03).
domain_priors:theater_ratio(russell_paradox, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(russell_paradox, extractiveness, 0.12).
narrative_ontology:constraint_metric(russell_paradox, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(russell_paradox, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(russell_paradox, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(russell_paradox, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(russell_paradox, mountain).
narrative_ontology:human_readable(russell_paradox, "Russell's Paradox: The Self-Referential Set Contradiction").
narrative_ontology:topic_domain(russell_paradox, "mathematical_logic/foundations").

domain_priors:emerges_naturally(russell_paradox).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOGICAL ANALYST (MOUNTAIN) — From the civilizational/universal analytical context, Russell's Paradox is an immutable logical limit. The contradiction R = {x | x ∉ x} → (R ∈ R ↔ R ∉ R) cannot be resolved by convention, social agreement, or computational effort. It is a structural impossibility in unrestricted set comprehension. Zero degrees of freedom for all indices.
constraint_indexing:constraint_classification(russell_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: FORMAL SYSTEM (MOUNTAIN) — Any formal system that permits unrestricted set comprehension (Frege's logic, naive set theory) is forced into inconsistency by the paradox. There is no escape through reframing, resource investment, or negotiation. The constraint is that self-referential totality collapses proof systems. The system has no exit option and no agency relative to the paradox.
constraint_indexing:constraint_classification(russell_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL INSTITUTIONS (MOUNTAIN) — Across 120+ years, every mathematical institution (universities, research programs, foundational frameworks) treats the paradox as an immutable boundary on what can be formalized without restriction. The institutional response (adopt type theory, ZFC, constructivism, or other bounded comprehension) does not resolve the paradox — it circumnavigates it. The paradox remains an inexplicable logical limit.
constraint_indexing:constraint_classification(russell_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: IMMEDIATE CONTEXT (MOUNTAIN) — At immediate time horizon, the paradox is pure logical necessity: R ∈ R ↔ R ∉ R is a tautological falsehood. No matter what power or scope we assign, the contradiction fires immediately. This is the strongest mountain classification — it exhibits zero temporal or contextual slack.
constraint_indexing:constraint_classification(russell_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(russell_paradox_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(russell_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(russell_paradox, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(russell_paradox, ExtMetricName, E),
    domain_priors:suppression_score(russell_paradox, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(russell_paradox),
    narrative_ontology:constraint_metric(russell_paradox, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(russell_paradox, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(russell_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Minimal. The paradox does not extract resources, benefits, or asymmetric gains. It is pure logical necessity. No agent benefits relative to others. Suppression (0.03): Minimal. The paradox does not suppress alternatives or foreclose choices. Once the contradiction is recognized, the response is transparent: restrict comprehension to avoid self-reference. There are no hidden mechanisms. Theater ratio (0.15): Low. The paradox requires almost no performative apparatus. The contradiction is immediate and inexplicable — it generates no need for ritual, institutional maintenance, or cover narratives. Accessibility collapse (0.92): Very high. No agent or system can access a state where the paradox does not hold if unrestricted comprehension is permitted. The collapse is unavoidable. Resistance (0.08): Very low. There is no meaningful resistance to the paradox. Once the contradiction is demonstrated, acceptance is forced. No degrees of freedom exist.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits zero perspectival gap — all perspectives classify identically as mountain. The paradox is invariant across all observational contexts: analytical vs powerless agents, immediate vs civilizational horizons, local vs universal scopes. This uniformity is the defining property of a true natural law in the DR framework. The paradox is one of the few constraints where indexical variation produces no change in classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Standard directionality computation does not apply to mountains. The paradox has no beneficiaries or victims — no agent experiences extraction or coordination. The constraint is a pure logical necessity that affects all agents equally: any formal system attempting unrestricted comprehension must accept inconsistency. Directionality d is undefined because there is no structural relationship to the constraint — there is only structural impossibility.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    semantic_vs_syntactic_boundary,
    'Is the paradox a semantic contradiction (about actual set membership) or a syntactic constraint (about what formal systems can express)?',
    'Philosophical clarification via model theory: can we construct models where the paradox is avoided through linguistic/formal machinery, or does it represent a deeper impossibility about reference itself?',
    'Semantic reading: mountain of thought (logical limit on reference). Syntactic reading: mountain of formalism (limit on proof systems). Both preserve mountain classification but differ on mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(semantic_vs_syntactic_boundary, conceptual, 'Semantic vs syntactic interpretation of the paradox').

omega_variable(
    paraconsistent_escape_validity,
    'Do paraconsistent logics that tolerate true contradictions genuinely escape the paradox or merely suppress its epistemic significance?',
    'Formal analysis of paraconsistent systems: does the paradox generate a true contradiction that is harmless, or does it generate logical inconsistency that merely isn''t exploited for ex falso quodlibet?',
    'If genuine escape: paradox is a mountain of classical logic, not of logic itself (reclassify to rope in paraconsistent systems). If suppression: paradox remains a mountain even in paraconsistent contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paraconsistent_escape_validity, empirical, 'Whether paraconsistent logics escape or suppress the paradox').

omega_variable(
    naturalness_of_boundary_conditions,
    'Are type restrictions, stratification, or other machinery to avoid the paradox natural limits on formalization or arbitrary ad hoc patches?',
    'Historical and conceptual analysis of foundational frameworks: do restriction axioms emerge naturally from independent concerns or were they invented specifically to block Russell''s Paradox?',
    'Natural limits: paradox reveals fundamental structure of expressibility (mountain confirmed). Arbitrary patches: paradox is contingent on a naive formalism choice (mountain weakened to tangled_rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_boundary_conditions, conceptual, 'Whether boundary conditions are natural or ad hoc').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(russell_paradox, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(russ_tr_t0, russell_paradox, theater_ratio, 0, 0.15).
narrative_ontology:measurement(russ_tr_t40, russell_paradox, theater_ratio, 40, 0.15).
narrative_ontology:measurement(russ_tr_t120, russell_paradox, theater_ratio, 120, 0.15).

% Extraction over time
narrative_ontology:measurement(russ_be_t0, russell_paradox, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(russ_be_t40, russell_paradox, base_extractiveness, 40, 0.12).
narrative_ontology:measurement(russ_be_t120, russell_paradox, base_extractiveness, 120, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(russell_paradox, information_standard).
narrative_ontology:affects_constraint(russell_paradox, godel_incompleteness_first).
narrative_ontology:affects_constraint(russell_paradox, cantor_diagonal_argument).

% DUAL FORMULATION NOTE:
% Russell's Paradox is foundational to the family of self-referential logical impossibilities. Gödel's Incompleteness Theorem and Cantor's Diagonal Argument are downstream constraints that exploit self-reference in formal systems. Russell's Paradox constrains the upstream question: what kinds of totalities can be formalized without inconsistency?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
