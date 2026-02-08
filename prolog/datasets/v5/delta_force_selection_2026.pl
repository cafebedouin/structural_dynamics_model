% ============================================================================
% CONSTRAINT STORY: delta_force_selection_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_delta_selection, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: delta_force_selection_2026
 * human_readable: Delta Force (1st SFOD-D) Selection & Assessment
 * domain: military/special_operations
 * * SUMMARY:
 * This constraint tracks the extreme exclusionary pressure of the 1st Special 
 * Forces Operational Detachment-Delta selection process. With an attrition 
 * rate averaging 90%, the process extracts maximum physiological and 
 * psychological output from candidates to identify "absolute reliability" 
 *.
 * * KEY AGENTS:
 * - [Candidates]: Subject (Powerless) - Elite soldiers from Rangers/SF 
 * attempting to endure the "Long Walk".
 * - [1st SFOD-D Command]: Beneficiary (Institutional) - The unit maintains 
 * its Tier 1 lethality through this high-extraction filter.
 * - [Unit Psychologists]: Auditor (Analytical) - Evaluate character and 
 * mental resilience.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extremely high extraction (0.92) due to the intentional "breakdown" of 
% candidates.
domain_priors:base_extractiveness(delta_force_selection_2026, 0.92). 

% High suppression (0.85); candidates are "kept in the dark" and forbidden 
% from speaking to assess individual grit.
domain_priors:suppression_score(delta_force_selection_2026, 0.85).   

% High theater ratio (0.75); instructors (cadre) maintain a performative 
% silence/anonymity to increase psychological stress.
domain_priors:theater_ratio(delta_force_selection_2026, 0.75).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(delta_force_selection_2026, extractiveness, 0.92).
narrative_ontology:constraint_metric(delta_force_selection_2026, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(delta_force_selection_2026, theater_ratio, 0.75).

% Requires active enforcement by the Cadre at Camp Dawson.
domain_priors:requires_active_enforcement(delta_force_selection_2026).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the candidate, selection is a snare: a trap of physical punishment 
% and psychological "darkness" where the only exit is to quit.
constraint_indexing:constraint_classification(delta_force_selection_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the Institutional Command, the process is a rope: it coordinates 
% human talent into a functional, ultra-reliable unit.
constraint_indexing:constraint_classification(delta_force_selection_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Auditors detect the inertial maintenance of 1970s SAS-modeled standards 
% (The Long Walk) as a non-negotiable Piton.
constraint_indexing:constraint_classification(delta_force_selection_2026, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(arbitrage), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(delta_force_selection_2026, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(delta_selection_tests).

test(perspectival_gap) :-
    % Subject feels a Snare (Trapped/Excluded); Unit sees a Rope (Coordination).
    constraint_indexing:constraint_classification(delta_force_selection_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(delta_force_selection_2026, rope, context(agent_power(institutional), _, _, _)).

test(extraction_limit) :-
    domain_priors:base_extractiveness(delta_force_selection_2026, E),

    E > 0.46. % Correctly identifies high-extraction exclusionary filters.

:- end_tests(delta_selection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.92) is anchored in the 90% attrition rate—a process 
 * where the "input" (elite soldiers) is almost entirely "consumed" (extracted) 
 * to find a tiny fraction of "output". 
 * The theater_ratio (0.75) is high because the cadre intentionally uses 
 * performative "grey-man" tactics to simulate an environment of ambiguity 
 * and unreliability, testing for the candidate's internal drive.
 *
 * MANDATROPHY ANALYSIS:
 * The Piton classification is assigned because the physical rucking standards 
 * are often viewed as "degraded" or "archaic" by modern sports-science 
 * standards, yet they remain due to institutional inertia regarding "character 
 * refinement" through suffering.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_mental_quantification,
    'Is mental toughness a trainable Scaffold or a fixed Mountain of personality?',
    'Analysis of successful selection candidates over 20 years vs. early-life resilience markers.',
    'If trainable, it is a Scaffold; if genetic/formative, it is a Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(delta_force_selection_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Model the selection window from T=0 (Initial Screening) to T=10 (Final Board).

% Theater ratio: Cadre silence (theater) increases as candidates enter 
% the stress phase and "Long Walk".
narrative_ontology:measurement(df_tr_t0, delta_force_selection_2026, theater_ratio, 0, 0.40).
narrative_ontology:measurement(df_tr_t5, delta_force_selection_2026, theater_ratio, 5, 0.65).
narrative_ontology:measurement(df_tr_t10, delta_force_selection_2026, theater_ratio, 10, 0.75).

% Extraction: Physiological extraction peaks during the 40-mile "Long Walk" 
% and psychological extraction peaks at the Commander's Board.
narrative_ontology:measurement(df_ex_t0, delta_force_selection_2026, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(df_ex_t5, delta_force_selection_2026, base_extractiveness, 5, 0.88).
narrative_ontology:measurement(df_ex_t10, delta_force_selection_2026, base_extractiveness, 10, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
