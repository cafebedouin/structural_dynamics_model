% ============================================================================
% CONSTRAINT STORY: communal_narcissism_social_trap
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-03
% ============================================================================

:- module(constraint_communal_narcissism, []).

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
 * * constraint_id: communal_narcissism_social_trap
 * human_readable: The Altruistic Extraction Snare
 * domain: social/psychological
 * * SUMMARY:
 * A social structure where a central "benefactor" uses performative prosociality 
 * (communal narcissism) to extract emotional and material labor from a group. 
 * High theater masks the predatory nature of the resource transfer.
 * * KEY AGENTS:
 * - The Disciple: Subject (Powerless) - Bound by moral debt and social shaming.
 * - The Saint: Beneficiary (Institutional) - The charismatic leader of the network.
 * - The Clinician: Auditor (Analytical) - Observes the divergence between claims and outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(communal_narcissism_social_trap, 0.78). 
domain_priors:suppression_score(communal_narcissism_social_trap, 0.65).   
domain_priors:theater_ratio(communal_narcissism_social_trap, 0.88).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(communal_narcissism_social_trap, extractiveness, 0.78).
narrative_ontology:constraint_metric(communal_narcissism_social_trap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(communal_narcissism_social_trap, theater_ratio, 0.88).

% Binary flags
domain_priors:requires_active_enforcement(communal_narcissism_social_trap).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The Disciple perceives high extraction ($E=0.78$) as a trap.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The Saint views the structure as essential coordination for "The Greater Good."
constraint_indexing:constraint_classification(communal_narcissism_social_trap, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% The Clinician detects high theater ($TR > 0.70$) maintaining a non-functional bond.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(communal_narcissism_social_trap, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(communal_narcissism_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(communal_narcissism_social_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(communal_narcissism_social_trap, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(high_extraction_threshold) :-
    domain_priors:base_extractiveness(communal_narcissism_social_trap, E),
    E >= 0.46.

test(piton_detection) :-
    domain_priors:theater_ratio(communal_narcissism_social_trap, TR),

    TR > 0.70.

:- end_tests(communal_narcissism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Perspectival Gap" here is extreme because the constraint operates via 
 * moral gaslighting. The Subject (Disciple) is unable to exit due to social 
 * suppression ($S=0.65$), while the Beneficiary (Saint) utilizes high theater 
 * ($TR=0.88$) to frame extraction as "mutual aid."
 * * [RESOLVED MANDATROPHY]:
 * The high extraction ($E=0.78$) is resolved by the Clinician's Piton 
 * classification, which identifies that the primary "coordination" is actually 
 * a vestigial narrative mask for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_communal_identity,
    'Is the Saint genuinely delusional (believing their own theater) or purely Machiavellian?',
    'Analysis of private vs public resource allocation over a 10-year period.',
    'If delusional: Scaffold/Rope drift. If Machiavellian: Pure Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(communal_narcissism_social_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time (Goodhart drift into performative altruism)
narrative_ontology:measurement(cnt_tr_t0, communal_narcissism_social_trap, theater_ratio, 0, 0.20).
narrative_ontology:measurement(cnt_tr_t5, communal_narcissism_social_trap, theater_ratio, 5, 0.55).
narrative_ontology:measurement(cnt_tr_t10, communal_narcissism_social_trap, theater_ratio, 10, 0.88).

% Extraction over time (Gradual intensification of rent-seeking)
narrative_ontology:measurement(cnt_ex_t0, communal_narcissism_social_trap, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cnt_ex_t5, communal_narcissism_social_trap, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cnt_ex_t10, communal_narcissism_social_trap, base_extractiveness, 10, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
