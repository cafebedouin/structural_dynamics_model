% ============================================================================
% CONSTRAINT STORY: zipfs_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zipfs_law, []).

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
 *   constraint_id: zipfs_law
 *   human_readable: Zipf's Law (The Power Law of Information)
 *   domain: technological
 *
 * SUMMARY:
 *   Zipf's Law describes the mathematical inevitability where the frequency
 *   of an item is inversely proportional to its rank. This phenomenon arises
 *   naturally in diverse systems, including the distribution of words in a
 *   language, the size of cities, and the popularity of websites. It is a
 *   fundamental property of information systems rather than a consequence of
 *   deliberate action.
 *
 * KEY AGENTS:
 *   - Analytical Observer (analytical/analytical): Sees Zipf's Law as an inherent property of information systems.
 *   - Information Archive (institutional/analytical): Adapts to Zipf's Law for data management.
 *   - Individual User (powerless/analytical): Experiences the effects of Zipf's Law indirectly.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zipfs_law, 0.15).
domain_priors:suppression_score(zipfs_law, 0.02).
domain_priors:theater_ratio(zipfs_law, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zipfs_law, extractiveness, 0.15).
narrative_ontology:constraint_metric(zipfs_law, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(zipfs_law, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zipfs_law, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(zipfs_law, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zipfs_law, mountain).
narrative_ontology:human_readable(zipfs_law, "Zipf's Law (The Power Law of Information)").
narrative_ontology:topic_domain(zipfs_law, "technological").

domain_priors:emerges_naturally(zipfs_law).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From an analytical perspective, Zipf's Law reflects a fundamental mathematical property inherent in many complex systems, including language and information distribution. Irrespective of specific implementations, the power-law distribution emerges naturally.
constraint_indexing:constraint_classification(zipfs_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% Institutional actors responsible for archiving and analyzing large datasets observe Zipf's Law as a consistent, predictable pattern. Their analysis tools and storage strategies must account for this distribution, but it doesn't present a manipulable extraction opportunity.
constraint_indexing:constraint_classification(zipfs_law, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Individual users experience Zipf's Law indirectly through the distribution of information they encounter. The power law shapes the frequency with which they encounter certain words, websites, or topics. While they are exposed to this distribution, they can't alter it, making the escape impossible. The individual does not have agency to change the structural properties of Zipf's law. This reinforces the classification as a mountain. 
constraint_indexing:constraint_classification(zipfs_law, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(analytical),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zipfs_law_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(zipfs_law, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(zipfs_law, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(zipfs_law, ExtMetricName, E),
    domain_priors:suppression_score(zipfs_law, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(zipfs_law),
    narrative_ontology:constraint_metric(zipfs_law, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(zipfs_law, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(zipfs_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Very low. Zipf's Law, in its pure form, does not involve extraction. It is a descriptive law rather than an operative mechanism. Suppression (0.02): Extremely low. The 'suppression' aspect is near-zero because no entity is actively preventing alternative distributions. Theater Ratio (0.05): Negligible. There is no performance or ritual associated with Zipf's Law.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify Zipf's Law as a Mountain because it is viewed as an inevitable property of information systems. There is no significant perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Because all parties perceive this phenomena to be a mountain range, the structural analysis between all three groups is similar. Extractiveness is negligibly low.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain prevents mislabeling Zipf's Law as extraction. While entities may exploit Zipf's Law (e.g., in search engine optimization), the law itself is not extractive. Any observed extraction is a result of additional constraints operating in conjunction with Zipf's Law, not Zipf's Law on its own.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zipfs_law, 0, 100).

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
