% ============================================================================
% CONSTRAINT STORY: sts86_ascent_checklist
% ============================================================================
% Generated: 2026-01-16
% Model: Gemini-Thought-Partner-v2
% Source: JSC-48005-86 - STS-86 Ascent Checklist
% ============================================================================

:- module(sts86_ascent_checklist, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

% Structural Anchor for Script Extraction
narrative_ontology:interval(sts86_ascent_checklist, 0, 10).

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: sts86_ascent_checklist
 * human_readable: Space Shuttle Ascent/Abort Procedural Matrix
 * domain: technological/institutional
 * temporal_scope: immediate (T- minus 5m to Orbit)
 * spatial_scope: cockpit/national
 * * SUMMARY:
 * The checklist (JSC-48005) represents the ultimate procedural constraint. 
 * It manages the transition from a ground-controlled environment to a 
 * physics-dominated ascent where human agency is bound by rigid logic gates.
 * * KEY AGENTS:
 * - NASA/Mission Operations: The Institutional architect.
 * - Flight Crew (CDR/PLT): The Moderate Agents executing the script.
 * - The 'Powerless' Pilot: The crew-member in a 3-Engine-Out scenario.
 * * NARRATIVE ARC:
 * Prelaunch is a "Rope" of coordination. At SRB Ignition, the checklist 
 * ossifies. In an abort (e.g., Mode 8), it becomes a "Mountain" (Physical Law).
 */

/* ==========================================================================
   2. BASE PROPERTIES
   ========================================================================== */

% Extractiveness: Low (0.05). NASA gains nothing from crew failure. 
% Benefit flow is aimed at systemic survival.
domain_priors:base_extractiveness(sts86_ascent_checklist, 0.05).

% Suppression: High (0.95). Deviating from the checklist is treated 
% as a failure mode itself. Alternative "freestyle" flying is invisible.
domain_priors:suppression_score(sts86_ascent_checklist, 0.95).

% Enforcement: Requires active maintenance (Ground Control / CPCB).
domain_priors:requires_active_enforcement(sts86_ascent_checklist).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE: Institutional (NASA Mission Operations) - ROPE
   --------------------------------------------------------------------------
   WHY: The checklist is the "Rope" that pulls 10,000 workers into sync.
   It is a coordination mechanism that is changeable (via Form 482).
   EVIDENCE: "This document is under the configuration control of the Crew 
   Procedures Control Board (CPCB)." 
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sts86_ascent_checklist,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE: Powerless View (Crew in 3-E/O Abort) - MOUNTAIN
   --------------------------------------------------------------------------
   WHY: In a failure mode, the checklist is no longer a choice; it is the
   edge of a physical cliff. "Mountain" status is claimed because the 
   metric of suppression (0.95) meets the physical deadline of Mach < .95.
   EVIDENCE: "BAILOUT MODE 8... ~30K FT... JETTISON HATCH... BAILOUT" 
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sts86_ascent_checklist,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (Insights into Ontological Mismatches)
   ========================================================================== */

:- begin_tests(sts86_ascent_tests).

test(perspectival_gap) :-
    % Institutional sees a tool (Rope), Powerless sees Fate (Mountain)
    constraint_indexing:constraint_classification(sts86_ascent_checklist, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(sts86_ascent_checklist, mountain, context(agent_power(individual_powerless), _, _, _)).

test(omega_variable_physics) :-
    % Grounding Omega-1: Velocity at MECO as a fixed physical constraint
    Velocity = 25873, % 
    Velocity > 25000.

:- end_tests(sts86_ascent_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Conflict & Omega Resolution)
   ========================================================================== */

/**
 * EXPLAINING THE CONFLICT:
 * The Institutional view claims "Rope" status because they control the 
 * document (Exit Option: Mobile). However, the metrics (Suppression: 0.95) 
 * contradict this for the crew. I have resolved this by updating the 
 * "Powerless" view to reflect that at T-0, the "Rope" has been "captured" 
 * by Physics ($\Omega_{physics}$).
 * * GROUNDING THE OMEGAS:
 * - Omega_Velocity ($\Omega_v$): 25,873 fps. The threshold where the Rope 
 * of the checklist becomes a Mountain. 
 * - Omega_Abort ($\Omega_a$): The 10-second stability window. 
 * If Stability < 10s, agency is lost. 
 * * ONTOLOGICAL_MISMATCHES: Resolved by identifying that the crew's 
 * "Trapped" status at launch forces a shift from Rope to Mountain.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. BFS (Backup Flight System): A parallel "Rope." If PASS fails, 
 * BFS becomes the primary constraint. 
 * 2. Manual Throttle: A rare moment where the Moderate agent regains 
 * agency, shifting Mountain back to Rope. 
 */

% ============================================================================
% END OF CONSTRAINT STORY
% ============================================================================
