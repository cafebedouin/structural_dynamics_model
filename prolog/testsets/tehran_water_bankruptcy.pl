% ============================================================================
% CONSTRAINT STORY: TEHRAN_WATER_BANKRUPTCY
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_tehran_water, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: tehran_water_bankruptcy
 * human_readable: Tehran Hydrological Bankruptcy and Subsidence
 * domain: environmental/political
 * * SUMMARY:
 * Tehran is navigating a "Day Zero" scenario where reservoir levels (e.g., Lar Dam 
 * at 2%) and aquifer depletion have led to irreversible land subsidence (30cm/year).
 * The crisis represents a total unbundling of urban density from hydrological 
 * reality.
 * * KEY AGENTS:
 * - The Southern Tehran Resident: Subject (Powerless/Trapped by subsidence)
 * - The Ministry of Energy/Agri-Lobby: Beneficiary (Institutional/Coordinating)
 * - The UNU-INWEH Global Auditor: Auditor (Analytical/Diagnostic)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is extremely high (0.88) because bankruptcy extracts the 
% physical floor (subsidence) and biological life-support of the city. 
% [RESOLVED MANDATROPHY]
domain_priors:base_extractiveness(tehran_water_bankruptcy, 0.88). 
domain_priors:suppression_score(tehran_water_bankruptcy, 0.75).   
domain_priors:theater_ratio(tehran_water_bankruptcy, 0.15).       

% Binary flags
domain_priors:requires_active_enforcement(tehran_water_bankruptcy). % Rationing/Pumping permits.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISPLACED RESIDENT (SNARE)
% Experienced as a predatory trap (Snare) where foundations crack and 
% taps run dry, with no feasible exit options.
constraint_indexing:constraint_classification(tehran_water_bankruptcy, snare, 
    context(agent_power(individual_powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: THE HYDRAULIC MISSION LEADERSHIP (ROPE)
% Viewed as the essential coordination of dams and agricultural 
% self-sufficiency that sustained the capital's 15M population for decades.
constraint_indexing:constraint_classification(tehran_water_bankruptcy, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE GEOLOGICAL ANALYST (PITON)
% Land subsidence is viewed as a Piton—a fixed, irreversible structural 
% spike driven into the urban landscape that outlasts the policy that caused it.
constraint_indexing:constraint_classification(tehran_water_bankruptcy, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(regional))) :-
    domain_priors:theater_ratio(tehran_water_bankruptcy, TR), TR < 0.20.

% PERSPECTIVE 4: RATIONING AUTHORITY (SCAFFOLD)
% Nightly pressure reductions and supply cuts act as a temporary 
% support structure (Scaffold) to delay "Day Zero".
constraint_indexing:constraint_classification(tehran_water_bankruptcy, scaffold, 
    context(agent_power(organized), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tehran_water_tests).

test(perspectival_shift) :-
    % Verify the crisis is a Snare for the resident but a Rope for the mission leadership.
    constraint_indexing:constraint_classification(tehran_water_bankruptcy, snare, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(tehran_water_bankruptcy, rope, context(agent_power(institutional), _, _, _)).

test(extraction_resolution) :-
    domain_priors:base_extractiveness(tehran_water_bankruptcy, E),
    E >= 0.70. % Validates high-extraction status requirement for [RESOLVED MANDATROPHY].

:- end_tests(tehran_water_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects the literal "bankruptcy" of the system, 
 * where the ground itself sinks (subsidence) to pay the interest on water 
 * over-consumption. The Perspectival Gap exists because the state maintains 
 * the 'Rope' of coordination for 15M people, even as the 'Snare' of physical 
 * collapse traps the individual resident.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system prevents collapse into pure extraction by identifying the 
 * 'Piton' status of subsidence. This acknowledges that the constraint is 
 * no longer just a policy (Snare) but a fixed geological fact (Piton/Mountain) 
 * created by past choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_relocation_feasibility,
    'Can the capital be moved to Makran before Day Zero triggers a kinetic crisis?',
    'Analysis of infrastructure deployment rates in coastal zones vs. reservoir decline rates.',
    'Success = New Scaffold; Failure = Kinetic Snare (Uninhabitability).',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
% Tracks from the start of the current 6-year drought to the 2026 Day Zero warning.
narrative_ontology:interval(tehran_water_bankruptcy, 2020, 2026). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
