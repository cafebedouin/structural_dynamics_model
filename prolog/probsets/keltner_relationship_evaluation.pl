% ============================================================================
% CONSTRAINT STORY: keltner_relationship_evaluation
% ============================================================================
% Version: 0.2 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_keltner_relationship_evaluation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: keltner_relationship_evaluation
 *   human_readable: The Keltner Relationship List
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The Keltner List is a 15-question diagnostic framework designed to
 *   evaluate the psychological and social health of a romantic relationship.
 *   It aims to facilitate communication, understanding, and improvement
 *   within the dyad. It represents a form of structured coordination to
 *   improve relationship quality.
 *
 * KEY AGENTS:
 *   - Relationship Partners: Both benefit from improved communication and understanding within the relationship (moderate/mobile)
 *   - Relationship Counselors: Benefit from a structured framework for assessment and guidance (institutional/analytical)
 *   - Analytical Observer: Sees the list as a standard and tool for relationship analysis (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(keltner_relationship_evaluation, 0.3).
domain_priors:suppression_score(keltner_relationship_evaluation, 0.1).
domain_priors:theater_ratio(keltner_relationship_evaluation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(keltner_relationship_evaluation, extractiveness, 0.3).
narrative_ontology:constraint_metric(keltner_relationship_evaluation, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(keltner_relationship_evaluation, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(keltner_relationship_evaluation, rope).
narrative_ontology:human_readable(keltner_relationship_evaluation, "The Keltner Relationship List").
narrative_ontology:topic_domain(keltner_relationship_evaluation, "social/psychological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(keltner_relationship_evaluation, relationship_counselors).
narrative_ontology:constraint_beneficiary(keltner_relationship_evaluation, relationship_partners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of an individual partner using the list for self-reflection and improvement. The list helps facilitate communication and understanding, acting as a coordination mechanism.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% Perspective of a relationship counselor using the list as a diagnostic tool. They benefit from a structured way to assess the relationship dynamics and guide the therapeutic process.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% From an analytical perspective, the Keltner List represents a structured framework for evaluating relationship health. It serves as a standard for communication and understanding, facilitating coordination within the dyad.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(keltner_relationship_evaluation_tests).
:- end_tests(keltner_relationship_evaluation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Keltner List is designed to be a coordination mechanism, facilitating better understanding and communication between partners. Its extractiveness is low, as it aims to empower individuals and improve the relationship, not extract value. The suppression is low, as participation is voluntary and alternative methods of relationship evaluation exist.
 *
 * PERSPECTIVAL GAP:
 *   All actors generally perceive the list as beneficial for the relationship.
 *
 * DIRECTIONALITY LOGIC:
 *   All identified agents benefit from the proper use of the tool, thus directionality remains consistently low. 
 *
 * MANDATROPHY ANALYSIS:
 *   The Keltner list is designed to be used as a beneficial relationship tool. As such, a proper deployment will avoid acting as a snare. 
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(keltner_relationship_evaluation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(keltner_relationship_evaluation, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
