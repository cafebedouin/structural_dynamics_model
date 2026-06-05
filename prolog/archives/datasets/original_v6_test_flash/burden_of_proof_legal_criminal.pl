% ============================================================================
% CONSTRAINT STORY: burden_of_proof_legal_criminal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_burden_of_proof_legal_criminal, []).

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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: burden_of_proof_legal_criminal
 *   human_readable: "Beyond a Reasonable Doubt" (Criminal Legal Burden)
 *   domain: political/social
 *
 * SUMMARY:
 *   In common law criminal systems, the burden of proof rests entirely on the
 *   prosecution to prove every element of a crime "beyond a reasonable
 *   doubt." This is a foundational constraint designed to protect individuals
 *   from the overwhelming power of the state, codifying the principle that it
 *   is better to let a guilty person go free than to wrongly convict an
 *   innocent one. It is a fundamental aspect of justice systems, requiring a
 *   high standard of evidence and proof before depriving someone of their
 *   liberty.
 *
 * KEY AGENTS:
 *   - Accused: Protected by the burden of proof (powerless/trapped).
 *   - Prosecution: Constrained by the burden of proof (institutional/constrained).
 *   - Society: Benefits from a fair and just legal system.
 *   - Analytical Observer: Views the system analytically.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burden_of_proof_legal_criminal, 0.15).
domain_priors:suppression_score(burden_of_proof_legal_criminal, 0.03).
domain_priors:theater_ratio(burden_of_proof_legal_criminal, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, extractiveness, 0.15).
narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burden_of_proof_legal_criminal, mountain).
narrative_ontology:human_readable(burden_of_proof_legal_criminal, "\"Beyond a Reasonable Doubt\" (Criminal Legal Burden)").
narrative_ontology:topic_domain(burden_of_proof_legal_criminal, "political/social").

domain_priors:emerges_naturally(burden_of_proof_legal_criminal).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of the accused, the burden of proof provides fundamental protection against wrongful conviction. There's no exit option as it's a foundational principle.
constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% From the prosecution's perspective, the burden of proof is a necessary constraint for maintaining justice and fairness, ensuring rigorous evidence gathering and presentation.
constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the burden of proof reflects a fundamental principle of justice designed to protect individual liberties against state overreach, even with the potential cost of some guilty parties not being convicted.
constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burden_of_proof_legal_criminal_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burden_of_proof_legal_criminal, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, ExtMetricName, E),
    domain_priors:suppression_score(burden_of_proof_legal_criminal, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(burden_of_proof_legal_criminal),
    narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(burden_of_proof_legal_criminal, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(burden_of_proof_legal_criminal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low because, while it constrains the prosecution, its primary purpose is to prevent extraction from the accused. Suppression is low as it allows for alternative defenses and challenges to the prosecution's case. The theater ratio is low as the system is primarily functional, with procedural elements directly supporting the goal of just outcomes.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives largely agree that the burden of proof serves a necessary and fundamental role, leading to a consistent classification as a Mountain. The slight variations in perspective reflect different experiences within the system but do not fundamentally alter the perception of its core function.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality focuses on the fact that the burden of proof protects individuals from the state. While the prosecution is constrained, this constraint is essential for a fair and just legal system.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is appropriately classified as a mountain because it represents a fundamental principle of justice, resistant to change and essential for protecting individual liberties. This classification prevents mislabeling it as pure extraction, which would ignore its essential protective function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burden_of_proof_legal_criminal, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
