% ============================================================================
% CONSTRAINT STORY: biological_curiosity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biological_curiosity, []).

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
 *   constraint_id: biological_curiosity
 *   human_readable: Curiosity (The Information-Seeking Drive)
 *   domain: biological/technological/social
 *
 * SUMMARY:
 *   Curiosity is the innate biological drive to seek out new information and
 *   reduce uncertainty in the environment. It serves as a fundamental driver
 *   for learning, adaptation, and discovery across various scales, from
 *   individual exploration to scientific progress and societal development.
 *   It's a rope that enables better understanding and navigation of the
 *   world.
 *
 * KEY AGENTS:
 *   - individuals: Beneficiaries seeking learning and adaptation
 *   - scientific_community: Beneficiaries seeking research and discovery
 *   - society: Beneficiaries of technological and social progress
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biological_curiosity, 0.15).
domain_priors:suppression_score(biological_curiosity, 0.02).
domain_priors:theater_ratio(biological_curiosity, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biological_curiosity, extractiveness, 0.15).
narrative_ontology:constraint_metric(biological_curiosity, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(biological_curiosity, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biological_curiosity, rope).
narrative_ontology:human_readable(biological_curiosity, "Curiosity (The Information-Seeking Drive)").
narrative_ontology:topic_domain(biological_curiosity, "biological/technological/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biological_curiosity, individuals).
narrative_ontology:constraint_beneficiary(biological_curiosity, scientific_community).
narrative_ontology:constraint_beneficiary(biological_curiosity, society).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of an individual, curiosity is a rope because it enhances their ability to learn and adapt within their immediate environment. They are mobile and have the option to seek novel information sources.
constraint_indexing:constraint_classification(biological_curiosity, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% From the perspective of the scientific community, curiosity acts as a rope because it drives research and discovery, leading to technological and social progress. The community has arbitrage through different research avenues.
constraint_indexing:constraint_classification(biological_curiosity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a universal, analytical perspective, curiosity is a rope because it's a fundamental driver of growth, adaptation, and development. It allows for constant learning and innovation across all time horizons.
constraint_indexing:constraint_classification(biological_curiosity, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biological_curiosity_tests).
:- end_tests(biological_curiosity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low, as curiosity primarily offers benefits. Suppression (0.02): Very low, as seeking information is generally encouraged and faces minimal barriers. Theater ratio (0.1): Low, as curiosity is primarily functional.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify curiosity as a rope, indicating a shared view of its coordinating benefits. This is because curiosity, at its core, facilitates the seeking of new information, leading to learning, adaptation, and progress at various scales.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals, scientific communities, and society all benefit from curiosity as it enables learning, innovation, and problem-solving. There isn't significant extraction, as curiosity is intrinsically rewarding and generally faces few barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   Curiosity is classified as a rope because it provides coordination benefits with minimal extraction. Mislabeling it as a snare would imply that seeking information is primarily detrimental, which is contrary to its nature as a driver of learning and adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biological_curiosity, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biological_curiosity, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
