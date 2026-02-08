% ============================================================================
% CONSTRAINT STORY: global_economic_anxiety_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_global_economic_anxiety_2026, []).

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
 * * constraint_id: global_economic_anxiety_2026
 * human_readable: The Global Economic Anxiety Snare
 * domain: economic/social
 * * SUMMARY:
 * Gallup's 2026 global survey reveals that economic issues are the #1 concern 
 * in 107 countries, cited by 23-26% of adults. The constraint is defined by 
 * a "Perception Gap" where GDP growth has little relation to individual 
 * financial pressure, especially regarding housing in high-income nations like 
 * Ireland, Australia, and Canada.
 * * KEY AGENTS:
 * - Younger Adults (Ages 15-34): Subject (Powerless/Locked out of prosperity)
 * - National Governments/Leaders: Beneficiary (Institutional - Judged by indicators)
 * - Gallup World Poll: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.68) as the housing crisis and living costs extract 
% wealth from younger generations, particularly in Ireland (49% concern).
domain_priors:base_extractiveness(global_economic_anxiety_2026, 0.68). 

% Suppression is moderate-high (0.72) because high living standards in 
% prosperous nations suppress the visibility of internal financial failure.
domain_priors:suppression_score(global_economic_anxiety_2026, 0.72).   

% Theater ratio is high (0.75). GDP growth is a "theatrical" indicator that 
% bears little relation to individual household struggle.
domain_priors:theater_ratio(global_economic_anxiety_2026, 0.75).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(global_economic_anxiety_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(global_economic_anxiety_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(global_economic_anxiety_2026, theater_ratio, 0.75).

% Primary keys for the classification engine
% High-extraction stakeholders
narrative_ontology:constraint_beneficiary(global_economic_anxiety_2026, asset_holders).
narrative_ontology:constraint_victim(global_economic_anxiety_2026, youth_economic_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE YOUTH (SNARE)
% For adults aged 15-34, the economy is a Snare—a trap where they feel 
% locked out of prosperity despite national growth headlines.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE INSTITUTION (ROPE)
% Leaders view GDP and standard economic indicators as a Rope—the essential 
% coordination tools for managing national progress.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Analysts view GDP-focused governance as a Piton: an inertial maintenance 
% of metrics that have lost functional alignment with public well-being.
constraint_indexing:constraint_classification(global_economic_anxiety_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_economic_anxiety_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_economic_anxiety_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_economic_anxiety_2026, rope, context(agent_power(institutional), _, _, _)).

test(perception_over_performance) :-
    domain_priors:theater_ratio(global_economic_anxiety_2026, TR),
    TR > 0.70.

:- end_tests(global_economic_anxiety_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.68) is driven by the housing crisis in high-income 
 * countries, where satisfaction with affordable housing has fallen to 25%. 
 * The high Theater Ratio (0.75) reflects the "Perception Gap" where national 
 * growth bear little relation to individual challenge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_economic_priority,
    'Will leaders pivot to household-centric metrics or maintain GDP theater?',
    'Review of national priority tracking in 2026 electoral cycles.',
    'Success shifts the system to a Rope; failure confirms a permanent Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_economic_anxiety_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio rises as the gap between household feel and GDP headlines grows.
narrative_ontology:measurement(ec_tr_t0, global_economic_anxiety_2026, theater_ratio, 0, 0.40).
narrative_ontology:measurement(ec_tr_t5, global_economic_anxiety_2026, theater_ratio, 5, 0.60).
narrative_ontology:measurement(ec_tr_t10, global_economic_anxiety_2026, theater_ratio, 10, 0.75).

% Extraction rises as housing satisfaction declines globally to 25%.
narrative_ontology:measurement(ec_ex_t0, global_economic_anxiety_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ec_ex_t5, global_economic_anxiety_2026, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ec_ex_t10, global_economic_anxiety_2026, base_extractiveness, 10, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
