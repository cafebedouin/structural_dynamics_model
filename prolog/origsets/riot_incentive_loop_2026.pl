% ============================================================================
% CONSTRAINT STORY: riot_incentive_loop_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_riot_loop, []).

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
 * * constraint_id: riot_incentive_loop_2026
 * human_readable: The Riot-Incentive Loop (State-Managed Chaos)
 * domain: political/social/governance
 * * SUMMARY:
 * This constraint models the feedback loop where state agencies deploy non-lethal 
 * chemical agents (tear gas) to incite panic and aggression. This "chaos" is 
 * then theatrically labeled a "riot" to justify expanded federal funding, 
 * tactical escalation, and the suspension of civic rights. 
 * * KEY AGENTS:
 * - [The Public/Protesters]: Subject (Powerless) - Trapped in a physiological 
 * extraction of breath and safety.
 * - [Security Apparatus]: Beneficiary (Institutional) - Gains budget and 
 * authority through the maintenance of the "riot" state.
 * - [Sarah Jeong / Independent Press]: Auditor (Analytical) - Uncovers the 
 * functional failure of gas as a "dispersal" tool.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extremely high extraction (0.82) as it consumes civil liberties and 
% physical health for institutional growth.
domain_priors:base_extractiveness(riot_incentive_loop_2026, 0.82). 

% High suppression (0.85); the use of unmarked vans and persistent gas 
% clouds removes the "exit option" of peaceful assembly.
domain_priors:suppression_score(riot_incentive_loop_2026, 0.85).   

% Very high theater ratio (0.90); the "riot" label is a performative mask 
% used to justify force that functionally incites the very behavior it claims to stop.
domain_priors:theater_ratio(riot_incentive_loop_2026, 0.90).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(riot_incentive_loop_2026, extractiveness, 0.82).
narrative_ontology:constraint_metric(riot_incentive_loop_2026, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(riot_incentive_loop_2026, theater_ratio, 0.9).

% Requires active enforcement by federal tactical units (CBP, ICE, DHS).
domain_priors:requires_active_enforcement(riot_incentive_loop_2026).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the civilian, the loop is a snare: an inescapable trap where presence 
% in public leads to chemical extraction and arbitrary detention.
constraint_indexing:constraint_classification(riot_incentive_loop_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the State, the loop is a rope: it coordinates the "protection of property" 
% and the "rule of law" through a standardized tactical response.
constraint_indexing:constraint_classification(riot_incentive_loop_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% The Auditor identifies this as a Piton: the loop is no longer functional 
% for "dispersal" but exists as an inertial mechanical anchor for governance.
constraint_indexing:constraint_classification(riot_incentive_loop_2026, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(arbitrage), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(riot_incentive_loop_2026, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(riot_loop_tests).

test(piton_detection) :-
    % Verify the Auditor detects the Piton due to high theater ratio.
    constraint_indexing:constraint_classification(riot_incentive_loop_2026, piton, _).

test(perspectival_gap) :-
    % Subject feels a Snare; State sees coordination (Rope).
    constraint_indexing:constraint_classification(riot_incentive_loop_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(riot_incentive_loop_2026, rope, context(agent_power(institutional), _, _, _)).

:- end_tests(riot_loop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Riot-Incentive Loop is the quintessential "Piton." While the state claims 
 * the "Rope" of public safety, the high theater_ratio (0.90) indicates that 
 * the "riot" is a manufactured byproduct. 
 *
 * [RESOLVED MANDATROPHY]: Extraction (0.82) is resolved via the "Riot-Incentive" 
 * feedback mechanism—the extraction of rights creates the theater needed to 
 * maintain the institutional anchor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_incitement_intent,
    'Is the incitement a bug or a feature of modern threat projection?',
    'Analysis of federal agent training manuals regarding "agitation" vs. "dispersal".',
    'If intentional, it is a Snare; if accidental/inertial, it is a Piton.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(riot_incentive_loop_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Model the drift from "Crowd Control" (T=0) to "Riot Incitement" (T=10).

% Theater ratio: Increases as the functional use of gas fails and the 
% performative "riot" narrative becomes the primary output.
narrative_ontology:measurement(riot_tr_t0, riot_incentive_loop_2026, theater_ratio, 0, 0.45).
narrative_ontology:measurement(riot_tr_t5, riot_incentive_loop_2026, theater_ratio, 5, 0.72).
narrative_ontology:measurement(riot_tr_t10, riot_incentive_loop_2026, theater_ratio, 10, 0.90).

% Extraction: Increases as tactical force replaces negotiation, extracting 
% bodily autonomy from larger civilian segments.
narrative_ontology:measurement(riot_ex_t0, riot_incentive_loop_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(riot_ex_t5, riot_incentive_loop_2026, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(riot_ex_t10, riot_incentive_loop_2026, base_extractiveness, 10, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
