% ============================================================================
% CONSTRAINT STORY: fundamental_theorem_of_algebra
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fundamental_theorem_of_algebra, []).

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
 *   constraint_id: fundamental_theorem_of_algebra
 *   human_readable: Fundamental Theorem of Algebra
 *   domain: mathematical/algebra
 *
 * SUMMARY:
 *   The Fundamental Theorem of Algebra states that every non-constant
 *   polynomial with complex coefficients has at least one complex root. This
 *   theorem has been a foundational constraint on mathematical practice for
 *   over three centuries, proven in multiple ways (algebraic, topological,
 *   analytic, geometric) yet always arriving at the same conclusion. The
 *   constraint exhibits the defining signature of a mountain: it emerges
 *   necessarily from the formal structure of the complex number system,
 *   offers no degrees of freedom, cannot be avoided or modified within
 *   standard mathematics, and persists identically across all observational
 *   contexts. No agent benefits from FTA while another bears its cost — it is
 *   a structural necessity that binds equally on all mathematicians. The
 *   near-perfect invariance of the theorem across diverse proof techniques
 *   and mathematical frameworks, combined with its logical indispensability
 *   in complex analysis, polynomial algebra, and algebraic geometry, confirms
 *   its mountain classification.
 *
 * KEY AGENTS:
 *   - Working Mathematicians: Structural position is universal participation in FTA's constraint. All polynomial work implicitly relies on root existence. No special exit options.
 *   - Analytic Observers: Civilizational perspective shows FTA as emerging from field completeness axioms. No alternative mathematical reality available.
 *   - Mathematical Pedagogy: Educational transmission of FTA as unchangeable truth. Theater ratio is minimal because the theorem's necessity is self-evident to practitioners.
 *   - Research Communities: Even powerful mathematical institutions cannot modify or escape FTA. Institutional power confers no arbitrage.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_theorem_of_algebra, 0.08).
domain_priors:suppression_score(fundamental_theorem_of_algebra, 0.02).
domain_priors:theater_ratio(fundamental_theorem_of_algebra, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, extractiveness, 0.08).
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fundamental_theorem_of_algebra, mountain).
narrative_ontology:human_readable(fundamental_theorem_of_algebra, "Fundamental Theorem of Algebra").
narrative_ontology:topic_domain(fundamental_theorem_of_algebra, "mathematical/algebra").

domain_priors:emerges_naturally(fundamental_theorem_of_algebra).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING MATHEMATICIAN — The theorem appears as an immutable structural property of the complex number system. All mathematical work in polynomial algebra implicitly relies on FTA. No exit from this constraint — it is a foundational bedrock. Zero degrees of freedom.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER — From the civilizational mathematical perspective, FTA is a logical consequence of the completeness axiom of the real numbers and the algebraic closure property of the complex field. The theorem emerges necessarily from the formal structure. No alternative formulation exists. Zero degrees of freedom.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL PEDAGOGY — The theorem is taught as a foundational truth that cannot be avoided or modified within the standard real and complex number systems. Educational systems transmit it as an unchangeable constraint on mathematical reality. No suppression is needed — the theorem's necessity is self-evident.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: PURE MATHEMATICS COMMUNITY — Despite institutional power within academia, mathematicians cannot modify or escape FTA. The constraint binds equally on the most powerful research programs and the novice student. No special access or arbitrage exists.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fundamental_theorem_of_algebra_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, ExtMetricName, E),
    domain_priors:suppression_score(fundamental_theorem_of_algebra, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fundamental_theorem_of_algebra),
    narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fundamental_theorem_of_algebra_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. FTA imposes no extraction from any agent — there is no beneficiary and no victim. The 'cost' of knowing a polynomial has a root is structural necessity, not deprivation. All agents experience FTA identically as an immutable property. Suppression (0.02): Negligible. The theorem requires no coercion to maintain. No alternatives exist within standard mathematics; suppression is neither needed nor possible. Theater ratio (0.15): Very low. FTA is proven rigorously across multiple independent frameworks (topological, algebraic, analytic). The redundant proofs demonstrate functional verification, not performative theater. Each proof independently establishes root existence through different logical pathways, reducing any possibility of hidden assumptions or theatrical maintenance. Accessibility collapse (0.92): Very high. No agent can access an alternative to FTA — the complete complex field necessarily contains roots for all non-constant polynomials. This structural necessity is irreversible. Resistance (0.04): Negligible. Mathematicians do not resist FTA; they rely on it. The theorem generates no friction or opposition.
 *
 * PERSPECTIVAL GAP:
 *   Unlike the verification bottleneck exemplar, FTA produces NO perspectival gap. All four perspectives — working mathematician, analytical observer, pedagogical institution, powerful research community — classify the constraint identically as mountain. The uniformity of classification across all observational positions is itself the diagnostic signature of a true mountain. No agent perceives FTA differently. No agent has exit options. No agent bears extraction or receives benefit. The structure is invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   FTA has no beneficiary/victim structure. Directionality (d) is undefined because there is no cost-bearing agent or benefit-receiving agent. The constraint is purely structural — it is a property of the complex number system itself, not a social, institutional, or economic arrangement. All agents (powerless, moderate, powerful, institutional, analytical) experience FTA identically as a binding universal truth. The engine's directionality derivation is not applicable to mountains because mountains have no extraction to direct.
 *
 * MANDATROPHY ANALYSIS:
 *   FTA resolves mandatrophy by being a constraint with zero mandatrophy potential. Mandatrophy arises when a coordination mechanism (Rope) accumulates extraction and becomes mistaken for pure extraction (Snare). FTA has no coordination function and no extraction mechanism — it is pure structural necessity. The theorem cannot be reframed as either pure coordination or pure extraction because it operates at the level of mathematical structure itself, not social or institutional coordination. The mandatrophy question — 'Is this coordinated extraction disguised as coordination, or genuine coordination disguised as extraction?' — does not apply to logical/mathematical constraints that bind universally and identically on all agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructive_proof_requirement,
    'Does FTA hold in constructive mathematics where the law of excluded middle is rejected?',
    'Formal analysis of FTA proof in constructivist frameworks (Bishop-style constructivism); examination of whether root existence can be established constructively for all polynomials',
    'If constructive proof exists: FTA is universal across logical systems (true mountain). If constructive proof fails for some polynomials: FTA depends on classical logic choice, suggesting a conceptual rather than physical constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constructive_proof_requirement, conceptual, 'Whether FTA holds in constructive mathematics').

omega_variable(
    non_standard_field_validity,
    'Does FTA generalize to finite fields, p-adic fields, or other complete fields beyond the complex numbers?',
    'Formal verification of FTA analogues in algebraic number theory and non-Archimedean analysis; identification of fields where root existence fails',
    'If FTA holds universally: confirms the theorem''s necessity (true mountain). If FTA fails in non-standard fields: reveals the theorem as specific to classical complex numbers, not a universal logical constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_standard_field_validity, empirical, 'Whether FTA generalizes to non-standard fields').

omega_variable(
    proof_irreducibility,
    'Is there a purely algebraic proof of FTA without topological or analytic machinery?',
    'Systematic search of algebraic proof strategies; analysis of proof complexity and what logical structures are unavoidable',
    'If purely algebraic proof exists: FTA is rooted in algebraic structure alone. If topological/analytic tools are mandatory: FTA bridges categories, suggesting a deeper structural necessity rather than a surface-level logical claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proof_irreducibility, conceptual, 'Whether FTA has a purely algebraic proof').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_theorem_of_algebra, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fta_tr_t0, fundamental_theorem_of_algebra, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fta_tr_t500, fundamental_theorem_of_algebra, theater_ratio, 500, 0.14).
narrative_ontology:measurement(fta_tr_t1000, fundamental_theorem_of_algebra, theater_ratio, 1000, 0.15).

% Extraction over time
narrative_ontology:measurement(fta_be_t0, fundamental_theorem_of_algebra, base_extractiveness, 0, 0.07).
narrative_ontology:measurement(fta_be_t500, fundamental_theorem_of_algebra, base_extractiveness, 500, 0.08).
narrative_ontology:measurement(fta_be_t1000, fundamental_theorem_of_algebra, base_extractiveness, 1000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fundamental_theorem_of_algebra, information_standard).
narrative_ontology:affects_constraint(fundamental_theorem_of_algebra, algebraic_closure_property).
narrative_ontology:affects_constraint(fundamental_theorem_of_algebra, completeness_axiom_reals).
narrative_ontology:affects_constraint(fundamental_theorem_of_algebra, complex_field_structure).

% DUAL FORMULATION NOTE:
% FTA is downstream of the definition of the complex field and the completeness axiom of the real numbers. These parent constraints establish the structural preconditions that make FTA inevitable. All three constraints are mountains with zero degrees of freedom.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
