% ============================================================================
% CONSTRAINT STORY: average_is_over_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_average_is_over_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: average_is_over_2026
 * human_readable: The AI-Talent Barbell Economy
 * domain: economic/educational
 * * SUMMARY:
 * As of 2026, the economy increasingly rewards a combination of natural talent 
 * and "internal pressure" to work with intelligent machines. 
 * Models like the Alpha School utilize AI-first tutoring to bypass traditional 
 * classroom structures, creating a "Rope" for high achievers while 
 * hollowing out the middle-skill class.
 * * KEY AGENTS:
 * - Middle-Skill Workers / Traditional Students: Subject (Powerless)
 * - AI-Augmented High Achievers / Alpha School: Beneficiary (Institutional)
 * - Economic Analysts (Arnold Kling/Tyler Cowen): Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.68). The "Barbell Economy" extracts wealth and status 
% from those whose skills compete with machines.
domain_priors:base_extractiveness(average_is_over_2026, 0.68). 

% Suppression is moderate-high (0.75). The requirement for extreme 
% conscientiousness suppresses those without high internal pressure.
domain_priors:suppression_score(average_is_over_2026, 0.75).   

% Theater ratio is moderate (0.42). While the "outstanding" mindset is 
% functional, "average" status is often theater masked as progress.
domain_priors:theater_ratio(average_is_over_2026, 0.42).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(average_is_over_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(average_is_over_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(average_is_over_2026, theater_ratio, 0.42).

% Primary keys for the classification engine
% High-extraction stakeholders
narrative_ontology:constraint_beneficiary(average_is_over_2026, ai_complementary_workforce).
narrative_ontology:constraint_victim(average_is_over_2026, routine_cognitive_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISPLACED WORKER (SNARE)
% For those competing with algorithms, the economy is a Snare: a trap of 
% declining wages and hollowing middle-class opportunities.
constraint_indexing:constraint_classification(average_is_over_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE ALPHA STUDENT (ROPE)
% High achievers view AI-tutors and internal pressure as a Rope: essential 
% tools to accomplish far more than was previously possible.
constraint_indexing:constraint_classification(average_is_over_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Analysts view traditional schooling as a Piton: an inertial structure 
% that no longer aligns with the "Average is Over" environment.
constraint_indexing:constraint_classification(average_is_over_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(average_is_over_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(average_is_over_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(average_is_over_2026, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(average_is_over_2026, E),
    E >= 0.46.

:- end_tests(average_is_over_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.68) is driven by "technological complementarity"—
 * where those who cannot leverage AI face a "Sovereignty Gap" in income 
 * and status. The Alpha School's model acts as a 
 * functional Rope for those with high conscientiousness.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system identifies the Snare by the hollowing out of "routine cognitive" 
 * tasks. For the "average" individual, the historical status of the middle 
 * class has atrophied into a non-functional Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_conscientiousness_scaling,
    'Can internal pressure be taught, or is it a non-modifiable demographic factor?',
    'Review of Brownsville Alpha pilot blocks and stress-related attrition.',
    'Success implies a universal Rope; Failure implies a permanent demographic Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(average_is_over_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Modeling the drift from the "Middle Class Era" to the "Barbell Economy."
% Theater ratio rises as "average" success becomes performative rather than functional.
narrative_ontology:measurement(avg_tr_t0, average_is_over_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(avg_tr_t5, average_is_over_2026, theater_ratio, 5, 0.28).
narrative_ontology:measurement(avg_tr_t10, average_is_over_2026, theater_ratio, 10, 0.42).

% Extraction rises as high earners cluster in expensive, amenities-rich hubs.
narrative_ontology:measurement(avg_ex_t0, average_is_over_2026, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(avg_ex_t5, average_is_over_2026, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(avg_ex_t10, average_is_over_2026, base_extractiveness, 10, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
