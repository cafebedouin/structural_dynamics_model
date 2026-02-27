% ============================================================================
% CONSTRAINT STORY: fermats_last_theorem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [RESOLVED]
% ============================================================================

:- module(constraint_fermats_last_theorem, []).

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
 *   constraint_id: fermats_last_theorem
 *   human_readable: The Difficulty of Proving Fermat's Last Theorem
 *   domain: mathematical
 *
 * SUMMARY:
 *   Fermat's Last Theorem, the claim that no three positive integers a, b,
 *   and c can satisfy the equation a^n + b^n = c^n for any integer value of n
 *   greater than 2, posed a significant barrier to mathematicians for over
 *   350 years. The difficulty in proving the theorem stemmed from its subtle
 *   relationship to deep mathematical structures, requiring the development
 *   of entirely new techniques in number theory.
 *
 * KEY AGENTS:
 *   - The Unproven Theorem: A fixed mathematical statement
 *   - The Analytical Observer: An analytical and civilizational view
 *   - The Mathematical Community: The mathematicians globally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fermats_last_theorem, 0.15).
domain_priors:suppression_score(fermats_last_theorem, 0.03).
domain_priors:theater_ratio(fermats_last_theorem, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fermats_last_theorem, extractiveness, 0.15).
narrative_ontology:constraint_metric(fermats_last_theorem, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(fermats_last_theorem, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fermats_last_theorem, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(fermats_last_theorem, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fermats_last_theorem, mountain).
narrative_ontology:human_readable(fermats_last_theorem, "The Difficulty of Proving Fermat's Last Theorem").
narrative_ontology:topic_domain(fermats_last_theorem, "mathematical").

domain_priors:emerges_naturally(fermats_last_theorem).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The theorem itself, an unyielding mathematical statement, presents an insurmountable barrier to proof using available methods.
constraint_indexing:constraint_classification(fermats_last_theorem, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% From the perspective of an analytical observer, the theorem represents a fundamental mathematical truth that resisted proof for centuries due to its inherent complexity.
constraint_indexing:constraint_classification(fermats_last_theorem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% The collective mathematical community viewed the theorem as a significant challenge, representing a deep property of numbers that defied existing techniques.
constraint_indexing:constraint_classification(fermats_last_theorem, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fermats_last_theorem_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(fermats_last_theorem, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fermats_last_theorem, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fermats_last_theorem, ExtMetricName, E),
    domain_priors:suppression_score(fermats_last_theorem, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(fermats_last_theorem),
    narrative_ontology:constraint_metric(fermats_last_theorem, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(fermats_last_theorem, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(fermats_last_theorem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low, as the theorem did not actively extract resources from mathematicians. The suppression is also low, as mathematicians were free to explore other avenues of research. The high accessibility collapse and low resistance indicate that the theorem was eventually proven through a natural extension of mathematical knowledge. The theater ratio is low, as there was little performative activity associated with the theorem. The primary perspective is the analytical observer, who recognizes the theorem as a fundamental truth.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives converge on the mountain classification. The theorem presented a fixed barrier that resisted all attempts at proof until Andrew Wiles developed the necessary techniques. The mountain classification reflects the inherent difficulty of the problem and its eventual resolution through the advancement of mathematical knowledge.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents perceive the constraint as a fixed property of mathematical space. There is no extraction or suppression in the conventional sense, but rather a fundamental limit to accessibility with existing tools.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is correctly classified as a mountain due to its fixed and immutable nature. The long period of resistance to proof does not indicate extraction or suppression, but rather the inherent complexity of the problem and the need for novel mathematical techniques.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fermats_last_theorem, 1637, 1995).

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
