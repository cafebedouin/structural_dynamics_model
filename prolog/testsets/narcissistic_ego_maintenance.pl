% ============================================================================
% CONSTRAINT STORY: narcissistic_ego_maintenance
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-03
% ============================================================================

:- module(constraint_general_narcissism, []).

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
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: narcissistic_ego_maintenance
 * human_readable: The Mirror Trap
 * domain: psychological/social
 * * SUMMARY:
 * A relational structure where the subject (The Echo) is compelled to provide 
 * constant narcissistic supply to the beneficiary (The Image). The constraint 
 * functions via emotional extraction and the suppression of the subject's 
 * independent agency to maintain the beneficiary's fragile ego stability.
 * * KEY AGENTS:
 * - The Echo: Subject (Powerless) - Providing emotional labor.
 * - The Image: Beneficiary (Institutional) - The rigid self-construct of the narcissist.
 * - The Biographer: Auditor (Analytical) - Tracking the lifecycle of the "Greatness" narrative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(narcissistic_ego_maintenance, 0.75). 
domain_priors:suppression_score(narcissistic_ego_maintenance, 0.60).   
domain_priors:theater_ratio(narcissistic_ego_maintenance, 0.85).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(narcissistic_ego_maintenance, extractiveness, 0.75).
narrative_ontology:constraint_metric(narcissistic_ego_maintenance, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(narcissistic_ego_maintenance, theater_ratio, 0.85).

% Constraint classification claim
narrative_ontology:constraint_claim(narcissistic_ego_maintenance, piton).

% Binary flags
domain_priors:requires_active_enforcement(narcissistic_ego_maintenance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% High extraction ($E=0.75$) felt as an emotional trap with limited exit.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The "Image" views this as necessary infrastructure for internal psychological coherence.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Detects high theater ($TR=0.85$) sustaining a non-functional social bond.
constraint_indexing:constraint_classification(narcissistic_ego_maintenance, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(narcissistic_ego_maintenance, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(narcissism_tests).

test(perspectival_gap) :-
    % Verify the Subject sees a Snare while the internal System sees a Rope.
    constraint_indexing:constraint_classification(narcissistic_ego_maintenance, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(narcissistic_ego_maintenance, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(narcissistic_ego_maintenance, E),

    E >= 0.46.

:- end_tests(narcissism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Perspectival Gap exists because the "The Image" perceives the extraction 
 * as a survival requirement (Rope), whereas "The Echo" experiences the 
 * unilateral flow of resources as a predatory trap (Snare). 
 * * [RESOLVED MANDATROPHY]:
 * The high extractiveness ($E=0.75$) is contextualized by the Piton status; 
 * the auditor recognizes that the "social coordination" is purely performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_supply_elasticity,
    'What is the true cost to the Subject if the supply is severed?',
    'Longitudinal study of Subject psychological health post-exit.',
    'Determines if the constraint is a Snare (pure extraction) or a Mountain (psychological necessity).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(narcissistic_ego_maintenance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time (The "Idealization" to "Devaluation" drift)
narrative_ontology:measurement(nem_tr_t0, narcissistic_ego_maintenance, theater_ratio, 0, 0.30).
narrative_ontology:measurement(nem_tr_t5, narcissistic_ego_maintenance, theater_ratio, 5, 0.60).
narrative_ontology:measurement(nem_tr_t10, narcissistic_ego_maintenance, theater_ratio, 10, 0.85).

% Extraction over time (Accumulation of emotional debt)
narrative_ontology:measurement(nem_ex_t0, narcissistic_ego_maintenance, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(nem_ex_t5, narcissistic_ego_maintenance, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(nem_ex_t10, narcissistic_ego_maintenance, base_extractiveness, 10, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for remaining gaps
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from narrative context in this file (narcissistic_ego_maintenance)
% ============================================================================
narrative_ontology:constraint_beneficiary(narcissistic_ego_maintenance, narcissists_self_image).
narrative_ontology:constraint_victim(narcissistic_ego_maintenance, emotional_labor_providers).
