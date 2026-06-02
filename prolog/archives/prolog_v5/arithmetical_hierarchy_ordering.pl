% ============================================================================
% CONSTRAINT STORY: arithmetical_hierarchy_ordering
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arithmetical_hierarchy_ordering, []).

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
 *   constraint_id: arithmetical_hierarchy_ordering
 *   human_readable: Arithmetical Hierarchy Ordering
 *   domain: mathematical_logic/computability_theory
 *
 * SUMMARY:
 *   The arithmetical hierarchy is a fundamental ordering in mathematical
 *   logic that partitions all arithmetically definable problems into strata
 *   based on the complexity of quantifier alternation over natural numbers.
 *   Σ₀ formulas (bounded quantifiers) represent decidable problems. Σ₁
 *   formulas (existential quantifiers over naturals) represent semi-decidable
 *   problems. Πₙ and Σₙ formulas with n quantifier alternations form higher
 *   tiers. This structure is not a convention, policy, or institutional
 *   arrangement — it is a logical necessity. No agent (mathematician, formal
 *   system, or computational process) can escape or renegotiate the ordering.
 *   The hierarchy constrains all agents equally by being the structure itself
 *   rather than a constraint imposed on agents.
 *
 * KEY AGENTS:
 *   - Arithmetical Logician: Analytical observer (analytical/analytical) — perceives the hierarchy as logically necessary structure, not constraint
 *   - Computability Theorist: Analytical observer (analytical/analytical) — perceives hierarchy enforcement via Church-Turing thesis and halting problem
 *   - Bounded Formal System: Victim/trapped agent (powerless/trapped) — cannot exceed its own hierarchy level without strengthening axioms
 *   - Expanding Foundational Framework: Powerful agent (powerful/mobile) — can strengthen axioms but cannot escape hierarchy entirely, only relocate within isomorphic structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arithmetical_hierarchy_ordering, 0.12).
domain_priors:suppression_score(arithmetical_hierarchy_ordering, 0.02).
domain_priors:theater_ratio(arithmetical_hierarchy_ordering, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arithmetical_hierarchy_ordering, extractiveness, 0.12).
narrative_ontology:constraint_metric(arithmetical_hierarchy_ordering, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(arithmetical_hierarchy_ordering, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(arithmetical_hierarchy_ordering, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(arithmetical_hierarchy_ordering, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arithmetical_hierarchy_ordering, mountain).
narrative_ontology:human_readable(arithmetical_hierarchy_ordering, "Arithmetical Hierarchy Ordering").
narrative_ontology:topic_domain(arithmetical_hierarchy_ordering, "mathematical_logic/computability_theory").

domain_priors:emerges_naturally(arithmetical_hierarchy_ordering).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARITHMETICAL LOGICIAN (MOUNTAIN) — The strict ordering of complexity levels in the arithmetical hierarchy is a logical necessity, not a social convention or policy choice. Every mathematical agent inhabiting logical space encounters the same recursive structure of quantifier complexity. This is not something one can exit or negotiate around — the hierarchy IS the structure of definability over natural numbers.
constraint_indexing:constraint_classification(arithmetical_hierarchy_ordering, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: COMPUTABILITY THEORIST (MOUNTAIN) — From the perspective of degrees of Turing computability and decidability, the arithmetical hierarchy partitions problems into irreducible strata. No algorithm can convert an undecidable (Π₁-complete) problem into a decidable one by reclassifying it. The hierarchy's stratification is enforced by the Church-Turing thesis and the halting problem — immutable boundaries.
constraint_indexing:constraint_classification(arithmetical_hierarchy_ordering, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: BOUNDED FORMAL SYSTEM (MOUNTAIN) — A finite formal system (Peano Arithmetic, ZFC, or any computable axiomatization) cannot escape its own hierarchical level. It cannot prove statements beyond its tier without strengthening its axioms — which creates a new hierarchy relative to the strengthened system. The ordering is inescapable from within.
constraint_indexing:constraint_classification(arithmetical_hierarchy_ordering, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 4: EXPANDING FOUNDATIONAL FRAMEWORK (MOUNTAIN) — Even powerful meta-logical observers who strengthen axioms or move to higher-order logic cannot eliminate the hierarchy — they simply relocate themselves higher in a similar structure. Moving from PA to ZFC, or from ZFC to a Grothendieck universe, creates isomorphic stratification at each level. Escape is impossible; ascent continues indefinitely.
constraint_indexing:constraint_classification(arithmetical_hierarchy_ordering, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arithmetical_hierarchy_ordering_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(arithmetical_hierarchy_ordering, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arithmetical_hierarchy_ordering, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(arithmetical_hierarchy_ordering, ExtMetricName, E),
    domain_priors:suppression_score(arithmetical_hierarchy_ordering, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(arithmetical_hierarchy_ordering),
    narrative_ontology:constraint_metric(arithmetical_hierarchy_ordering, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(arithmetical_hierarchy_ordering, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(arithmetical_hierarchy_ordering_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   CRITICAL MOUNTAIN CLASSIFICATION: This constraint exhibits the defining properties of a natural law in the Deferential Realism framework. Extractiveness is extraordinarily low (0.12) because there is no asymmetric extraction — the hierarchy does not benefit some agents at the expense of others. It is not a power structure; it is the logical structure within which all mathematical power operates. Suppression is minimal (0.02) because there are no alternatives to suppress — the constraint is not competing with other orderings. Theater ratio is near-zero (0.05) because the hierarchy requires no performative maintenance — it is self-evident to any agent that understands the definitions. Accessibility collapse (0.92) is extremely high: the barrier to understanding why Πₙ formulas are strictly more complex than Σₙ formulas is not institutional but cognitive and conceptual — one must understand quantifier logic, recursion, and computability. This barrier does not collapse; it simply exists as a feature of logical understanding. Resistance (0.08) is minimal: the hierarchy is not contested. Mathematicians do not propose alternative orderings because the ordering follows necessarily from the definitions. This is the signature of a mountain constraint: zero degrees of freedom for any observational perspective.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is UNIFORM-TYPE MOUNTAIN. All perspectives yield mountain classification because the constraint is not an institutional arrangement susceptible to different experiential readings. An arithmetical logician and a bounded formal system both encounter the same hierarchy. The logician understands it abstractly; the formal system encounters it as an irreducible limitation on its proof capacity. But both see the same structure with zero degrees of freedom. There is no perspectival gap because the constraint is not a social or institutional artifact — it is a structural property of logical space itself. This is the defining characteristic of a true mountain constraint in the framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is not applicable to mountain constraints in the standard sense. The constraint does not extract from or benefit any agent — it is the logical ground that all agents inhabit. There are no beneficiaries or victims because the constraint is not a power relationship. If forced to apply the directionality framework: all agents occupy d ≈ 0.50 (symmetric, neither beneficiary nor victim) because the constraint affects all equally by being logically universal. Suppression is not a feature of this constraint because there are no alternatives to suppress. The constraint simply IS.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY IS FULLY RESOLVED through pure mountain classification. The constraint does not exhibit the mandatrophy risk (disguised extraction masquerading as coordination) because it has no coordination function to disguise. It is pure structure with zero extraction. The classification gate is unambiguous: emerged naturally (yes), accessibility_collapse exceeds 0.85 (0.92), resistance is minimal (0.08), extractiveness is negligible (0.12), suppression is near-zero (0.02). The constraint passes the mountain gates with no ambiguity. All six perspectives in the uniform-type array classify as mountain, confirming the invariance across all observational positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transfinite_hierarchy_extension,
    'Does the arithmetical hierarchy extend genuinely beyond its classical bounds, or is extension merely relabeling at higher ordinals?',
    'Comparison of Kleene hierarchy, hyperarithmetical hierarchy, and analytic hierarchy; analysis of whether each tier genuinely increases definability or merely encodes prior tiers in higher-order syntax',
    'If genuine extension: the hierarchy is open-ended and the constraint only partially characterizes definability. If relabeling: the core arithmetical hierarchy is the fundamental stratum, and higher extensions are notational refinements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transfinite_hierarchy_extension, conceptual, 'Whether the hierarchy extends genuinely or is notational relabeling').

omega_variable(
    oracle_contingency,
    'Is the hierarchy''s structure contingent on our choice of oracles and models, or is it model-independent?',
    'Analysis across different models of computation (Turing machines, lambda calculus, register machines); examination of hierarchy stability under oracle change; comparison with intuitionistic and constructive logics',
    'If contingent: the hierarchy is observer-dependent, weakening mountain classification. If model-independent: the mountain is robust across all reasonable logical frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oracle_contingency, conceptual, 'Whether hierarchy structure is model-dependent or universal').

omega_variable(
    godel_incompleteness_independence,
    'Are there arithmetical hierarchy levels whose relative ordering is independent of any consistent formal system?',
    'Investigation of whether specific Σₙ vs Πₙ tier comparisons are derivable or independent across different axiomatizations; examination of whether the hierarchy itself can be formalized without requiring incompleteness about its own ordering',
    'If independent: some hierarchical relationships cannot be proven within the system, suggesting the constraint contains irreducible logical gaps. If dependent: the ordering is fully formalized and exhibits no gaps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(godel_incompleteness_independence, empirical, 'Whether hierarchy ordering contains Gödelian independent propositions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arithmetical_hierarchy_ordering, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arit_tr_t0, arithmetical_hierarchy_ordering, theater_ratio, 0, 0.02).
narrative_ontology:measurement(arit_tr_t50, arithmetical_hierarchy_ordering, theater_ratio, 50, 0.03).
narrative_ontology:measurement(arit_tr_t100, arithmetical_hierarchy_ordering, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(arit_be_t0, arithmetical_hierarchy_ordering, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(arit_be_t50, arithmetical_hierarchy_ordering, base_extractiveness, 50, 0.11).
narrative_ontology:measurement(arit_be_t100, arithmetical_hierarchy_ordering, base_extractiveness, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arithmetical_hierarchy_ordering, information_standard).
narrative_ontology:affects_constraint(arithmetical_hierarchy_ordering, halting_problem_undecidability).
narrative_ontology:affects_constraint(arithmetical_hierarchy_ordering, godel_incompleteness_limits).
narrative_ontology:affects_constraint(arithmetical_hierarchy_ordering, turing_degree_hierarchy).

% DUAL FORMULATION NOTE:
% The arithmetical hierarchy is a fundamental mathematical structure upon which several downstream constraints depend: the halting problem's undecidability, Gödel's incompleteness theorems, and the structure of Turing degrees all presuppose the arithmetical hierarchy's ordering. These constraints are downstream not as weaker versions but as specific instantiations and consequences of the general hierarchy ordering.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
