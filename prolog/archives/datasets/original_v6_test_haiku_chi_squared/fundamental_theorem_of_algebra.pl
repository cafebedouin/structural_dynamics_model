% ============================================================================
% CONSTRAINT STORY: fundamental_theorem_of_algebra
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   human_readable: Fundamental Theorem of Algebra (FTA)
 *   domain: mathematics/algebra
 *
 * SUMMARY:
 *   The Fundamental Theorem of Algebra states that every non-constant
 *   polynomial with complex coefficients has at least one complex root. This
 *   is a necessary consequence of the completeness of the complex number
 *   field and the intermediate value theorem applied to polynomial
 *   continuity. FTA exhibits the canonical properties of a Mountain
 *   constraint: zero degrees of freedom, invariance across all known
 *   mathematical frameworks, and emergence from basic structural properties
 *   rather than institutional enforcement. There is no agent that can evade,
 *   negotiate, or suppress FTA. All observers — from the polynomial solver
 *   facing a quadratic equation to the mathematical institution teaching
 *   algebra to organized research communities — encounter the constraint as
 *   immutable natural law. Theater ratio (performative content) is negligible
 *   because FTA requires no enforcement, advocacy, or institutional
 *   maintenance. It simply is.
 *
 * KEY AGENTS:
 *   - The Polynomial Practitioner: Encounters FTA as fact (powerless/analytical) — no alternatives available
 *   - The Analytical Observer: Verifies FTA through proof (analytical/analytical) — invariant across all rigorous frameworks
 *   - The Mathematical Institution: Teaches FTA in curricula (institutional/analytical) — cannot negotiate its binding
 *   - The Organized Mathematical Community: Cannot collectively override FTA (organized/analytical) — no coalition power available
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fundamental_theorem_of_algebra, 0.08).
domain_priors:suppression_score(fundamental_theorem_of_algebra, 0.02).
domain_priors:theater_ratio(fundamental_theorem_of_algebra, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, extractiveness, 0.08).
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(fundamental_theorem_of_algebra, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fundamental_theorem_of_algebra, mountain).
narrative_ontology:human_readable(fundamental_theorem_of_algebra, "Fundamental Theorem of Algebra (FTA)").
narrative_ontology:topic_domain(fundamental_theorem_of_algebra, "mathematics/algebra").

domain_priors:emerges_naturally(fundamental_theorem_of_algebra).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLYNOMIAL PRACTITIONER (MOUNTAIN) — A student, engineer, or researcher solving polynomial equations encounters FTA as an immutable fact about the mathematical universe. There is no alternative system where this constraint does not hold. No exit, no negotiation, no suppression possible. ε=0.08, suppression≤0.05, accessibility_collapse=0.92 → Mountain.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — Across all rigorous proof traditions (Gauss, Cauchy, topology, abstract algebra), FTA emerges as a necessary consequence of the completeness of complex numbers and properties of continuous functions. The constraint is invariant under all known mathematical frameworks. Zero degrees of freedom. ε=0.08, suppression≤0.05 → Mountain.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: MATHEMATICAL INSTITUTION (MOUNTAIN) — Universities, research centers, and mathematical societies cannot escape FTA in their curricula or research. It is taught in every algebra course worldwide because it is universally binding. No institutional agent can negotiate or suppress this constraint. ε=0.08, suppression≤0.05, emerges_naturally=true → Mountain from all institutional perspectives.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: ORGANIZED MATHEMATICAL COMMUNITY (MOUNTAIN) — Even collectively, mathematicians cannot vote FTA away, redefine it, or find an exception that matters. The constraint binds organized agents just as it binds individuals. No coalition has the power to change fundamental mathematical structure. ε=0.08, suppression≤0.05 → Mountain.
constraint_indexing:constraint_classification(fundamental_theorem_of_algebra, mountain,
    context(agent_power(organized),
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
 *   Extractiveness (0.08): FTA has near-zero extractiveness because it is a pure logical/mathematical fact with no asymmetric distribution of costs or benefits. Everyone who uses polynomial mathematics benefits equally from knowing that roots exist (whether or not they can compute them). No agent is privileged or exploited by the constraint's existence. Suppression (0.02): FTA requires no suppression because there is no alternative system that any agent would prefer. No one is forced into silence or compliance — the constraint simply defines the behavior of polynomials universally. Theater ratio (0.05): Near-zero performative content. FTA requires no institutional maintenance, advocacy, ceremonial recitation, or performative verification. When it is taught or proved, the performance is explanation, not enforcement. Accessibility collapse (0.92): Extremely high. FTA is foundational to all polynomial mathematics. There is no escape, no partial exit, no workaround. Every polynomial user must accept it. Resistance (0.03): Nearly zero. No mathematical tradition, no alternate axiom set, no alternative framework has ever produced a polynomial that violates FTA. The constraint is universally resistant to contradiction.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap in FTA classification. All observers from powerless to institutional to analytical see a Mountain because the constraint is universally binding. This is the signature of a true natural law: invariance across all observation points. The absence of perspectival gap is itself evidence of Mountain status.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is not computed for FTA because it is a Mountain constraint with no beneficiary/victim structure. No agent extracts from another via FTA. The constraint is not enforced by anyone against anyone. All agents experience it equally as an immutable boundary condition of the mathematical universe.
 *
 * MANDATROPHY ANALYSIS:
 *   FTA resolves the mandatrophy trivially by exhibiting the canonical pure Mountain signature: zero extractiveness (≤0.08), zero suppression (≤0.02), high accessibility collapse (≥0.85), low resistance (≤0.15), and emergence from basic mathematical structure (not institutional enforcement). There is no risk of misclassifying FTA as coordination (Rope) or extraction (Snare) because it has neither coordination function nor asymmetric distribution. All perspectives produce identical Mountain classification. The theater ratio stabilizes at 0.05 because explaining or proving FTA requires no performative maintenance — once understood, it simply constrains behavior.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructive_versus_classical_proof,
    'Does FTA hold in constructive mathematics without the Law of Excluded Middle?',
    'Examination of intuitionistic/constructive proofs of FTA; analysis of whether constructive root-finding requires different assumptions about the complex number field',
    'If FTA holds constructively: the constraint is even more fundamental (independent of LEM). If not: FTA depends on classical logic, suggesting it is not a pure natural law but a consequence of axiom choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constructive_versus_classical_proof, conceptual, 'Whether FTA holds in constructive mathematics').

omega_variable(
    alternative_number_systems_scope,
    'Does FTA apply to finite fields, hypercomplex algebras, or non-commutative rings?',
    'Survey of FTA analogues in abstract algebra; characterization of which algebraic structures preserve the root-existence property',
    'If FTA generalizes uniformly: the constraint is universal across algebra. If it fails in some algebras: FTA is specific to complex numbers, narrowing its universality scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_number_systems_scope, conceptual, 'Scope of FTA across alternative number systems').

omega_variable(
    polynomial_degree_verification_computational,
    'Can the existence of a complex root be verified algorithmically for all polynomials, or does the FTA guarantee existence without effective computability?',
    'Analysis of root-finding algorithms; characterization of computational complexity for arbitrary polynomial degrees; identification of polynomials where root-finding provably exceeds classical complexity bounds',
    'If roots are effectively computable: FTA is a practical constraint, not just an existence claim. If not: FTA is a pure existence theorem with no computational enforcement mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polynomial_degree_verification_computational, empirical, 'Computational verifiability of FTA-guaranteed roots').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fundamental_theorem_of_algebra, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fta_tr_t0, fundamental_theorem_of_algebra, theater_ratio, 0, 0.02).
narrative_ontology:measurement(fta_tr_t500, fundamental_theorem_of_algebra, theater_ratio, 500, 0.04).
narrative_ontology:measurement(fta_tr_t1000, fundamental_theorem_of_algebra, theater_ratio, 1000, 0.05).

% Extraction over time
narrative_ontology:measurement(fta_be_t0, fundamental_theorem_of_algebra, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(fta_be_t500, fundamental_theorem_of_algebra, base_extractiveness, 500, 0.08).
narrative_ontology:measurement(fta_be_t1000, fundamental_theorem_of_algebra, base_extractiveness, 1000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fundamental_theorem_of_algebra, information_standard).
narrative_ontology:affects_constraint(fundamental_theorem_of_algebra, polynomial_root_existence_constructive).
narrative_ontology:affects_constraint(fundamental_theorem_of_algebra, algebraic_closure_property).

% DUAL FORMULATION NOTE:
% FTA is a foundational constraint in pure mathematics. Related constraints (polynomial root existence in constructive mathematics, algebraic closure in abstract algebras) depend structurally on FTA but may exhibit different ε values in different algebraic contexts. FTA itself is universal; its generalizations are domain-specific.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
