% ============================================================================
% CONSTRAINT STORY: cuban_missile_crisis_excomm_deliberation
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Cuban Missile Crisis 1962 (Declassified Records/ExComm Transcripts)
% ============================================================================

:- module(constraint_cuban_missile_crisis, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cuban_missile_crisis_excomm_deliberation
 * human_readable: The ExComm Multi-Channel Deliberation Protocol
 * domain: political/military
 * temporal_scope: October 16–28, 1962
 * spatial_scope: Global (US-USSR-Cuba)
 * * SUMMARY:
 * Following the failure of the Bay of Pigs, President Kennedy established 
 * the Executive Committee of the National Security Council (ExComm). This 
 * constraint functioned as a deliberate "anti-silo" mechanism, forcing 
 * decision-makers to weigh multiple competing options—primarily a 
 * naval blockade (quarantine) vs. a direct air strike—under the 
 * existential constraint of Nuclear Mutually Assured Destruction (MAD) 
 *.
 * * KEY AGENTS:
 * - The Hawks (JCS/LeMay): Proponents of a surgical air strike and 
 * invasion.
 * - The Doves (Robert Kennedy/McNamara): Proponents of the naval blockade 
 * as a "Rope" for de-escalation.
 * - Analytical Observer (The Historian): Evaluates the efficacy of the 
 * ExComm protocol in avoiding nuclear escalation.
 * * NARRATIVE ARC:
 * The discovery of Soviet missiles in Cuba initially appeared as a 
 * Mountain (an unchangeable threat requiring immediate force). Through the 
 * ExComm protocol, this was reframed as a Rope (a coordination problem 
 * requiring a blockade and diplomatic back-channels), successfully 
 * avoiding the Snare of a global thermonuclear exchange.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(thirteen_days_crisis, 0, 10).
narrative_ontology:constraint_claim(cuban_missile_crisis_excomm_deliberation, rope).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: Low (0.2). Unlike the Bay of Pigs, the ExComm process was 
% designed to protect the collective interest of global survival rather 
% than institutional prestige.
domain_priors:base_extractiveness(cuban_missile_crisis_excomm_deliberation, 0.2).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Low (0.15). The protocol explicitly encouraged the airing 
% of conflicting viewpoints and dissent to avoid groupthink.
domain_priors:suppression_score(cuban_missile_crisis_excomm_deliberation, 0.15).

% Enforcement requirements
% Requires active enforcement (JFK intentionally removed himself from 
% some meetings to allow for candid discussion).
domain_priors:requires_active_enforcement(cuban_missile_crisis_excomm_deliberation).

% Metrics required for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
constraint_beneficiary(cuban_missile_crisis_excomm_deliberation, global_population).
constraint_beneficiary(cuban_missile_crisis_excomm_deliberation, diplomatic_stability).
constraint_victim(cuban_missile_crisis_excomm_deliberation, castro_regime_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE JOINT CHIEFS (The Hawks) - Mountain
   --------------------------------------------------------------------------
   
   WHO: institutional - Military leadership with a rigid doctrine of force.
   WHEN: immediate - Focused on the closing window for a pre-emptive strike.
   WHERE: trapped - View themselves as bound by the military necessity to 
         remove the missile threat immediately.
   SCOPE: continental - Primary focus on the defense of the US mainland.
   
   WHY THIS CLASSIFICATION:
   The Chiefs viewed the missiles as an unchangeable military fact (Mountain). 
   To them, the only "natural" response was physical removal through force, 
   viewing any delay as a failure to adhere to the laws of war.
   
   NARRATIVE EVIDENCE:
   General LeMay argued that the blockade was "weak" and that a military 
   strike was the only way to resolve the crisis effectively.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cuban_missile_crisis_excomm_deliberation,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(national)
    )
) :-
    domain_priors:requires_active_enforcement(cuban_missile_crisis_excomm_deliberation),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: ROBERT KENNEDY/MCNAMARA (The Doves) - Rope
   --------------------------------------------------------------------------
   
   WHO: powerful - Strategic advisors to the President.
   WHEN: biographical - Understanding the weight of history and presidential legacy.
   WHERE: mobile - Actively seeking alternatives like the blockade/quarantine.
   SCOPE: global - Managing the risk of total nuclear escalation.
   
   WHY THIS CLASSIFICATION:
   For RFK and McNamara, the ExComm process and the blockade were a 
   Rope—a coordination mechanism that allowed for communication and 
   signaling to Khrushchev without immediate bloodshed.
   
   NARRATIVE EVIDENCE:
   Robert Kennedy emphasized that "an air strike would be a Pearl Harbor 
   in reverse," framing the decision as a choice of coordination/morality 
   over raw force.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cuban_missile_crisis_excomm_deliberation,
    rope,
    context(
        agent_power(powerful),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :-
    domain_priors:suppression_score(cuban_missile_crisis_excomm_deliberation, S),
    S < 0.3,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: POST-COLD WAR ANALYST - Analytical
   --------------------------------------------------------------------------
   
   WHO: analytical - Outside observer examining declassified transcripts.
   WHEN: historical - Evaluating the crisis in the context of the 20th century.
   WHERE: analytical - Free from the life-and-death pressures of 1962.
   SCOPE: global - Analyzing the long-term impact on arms control.
   
   WHY THIS CLASSIFICATION:
   From a safe distance, the ExComm is seen as an ideal Rope—a system 
   designed to prevent the "Snare" of a binary choice (Total Surrender vs. 
   Total War) by creating a middle-path mechanism for de-escalation.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cuban_missile_crisis_excomm_deliberation,
    rope,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE GLOBAL CIVILIAN (SUBJECT) - Snare
   --------------------------------------------------------------------------
   WHO: powerless - The billions of subjects with no agency in the room.
   WHEN: immediate - The "Thirteen Days" where every second is a potential end.
   WHERE: trapped - No exit from the reach of global thermonuclear war.
   SCOPE: global - The stakes encompass the entire biosphere.
   
   WHY THIS CLASSIFICATION:
   The "Snare" is the state of nuclear brinkmanship. While EXCOMM uses 
   deliberation as a Rope to pull back from the edge, the individual subject 
   feels the Snare tightening. For them, this is not "coordination" but a 
   passive wait for an existential verdict. The stakes (0.95) and 
   suppression of agency (0.9) create the ultimate structural trap.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cuban_missile_crisis_excomm_delibration,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    domain_priors:base_extractiveness(cuban_missile_crisis_excomm_delibration, E),
    E > 0.8,
    !.

% Explicit priors reflecting the existential stakes for the powerless subject.
domain_priors:base_extractiveness(cuban_missile_crisis_excomm_delibration, 0.9).
domain_priors:suppression_score(cuban_missile_crisis_excomm_delibration, 0.9).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_deliberation, extractiveness, 0.2).
narrative_ontology:constraint_metric(cuban_missile_crisis_excomm_deliberation, suppression_requirement, 0.15).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(cuban_missile_crisis_tests).

test(multi_perspective_differentiation) :-
    constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_deliberation, T1, context(institutional, immediate, trapped, national)),
    constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_deliberation, T2, context(powerful, biographical, mobile, global)),
    T1 \= T2.

test(low_suppression_verification) :-
    domain_priors:suppression_score(cuban_missile_crisis_excomm_deliberation, S),
    S < 0.2.

:- end_tests(cuban_missile_crisis_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS MADE BY MODEL:
 * 1. CLASSIFICATION: Labeled as 'Rope' because the ExComm was specifically 
 * created to provide options and coordination where none seemed to exist.
 * 2. SUPPRESSION SCORE (0.15): I chose a very low score to highlight that 
 * the success of the crisis management relied on the *lack* of suppression. 
 * Contrast this with the Bay of Pigs (0.8 suppression).
 */

omega_variable(
    soviet_intent_omega,
    "Would Khrushchev have actually launched if a 'surgical' air strike had occurred?",
    resolution_mechanism("Cross-referencing declassified Politburo minutes with field-level operational orders"),
    impact("If Yes: The ExComm was the only Rope that worked. If No: The Hawks' Mountain was an illusion."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Surgical Air Strike
 * Viability: Highly viable and preferred by the military; could have 
 * physically removed the threat but risked total war.
 * Suppression: Actively debated but rejected by JFK after evaluating 
 * the long-term risks.
 * * CONCLUSION:
 * Unlike the Bay of Pigs, where alternatives were hidden, here they 
 * were interrogated. This transparency is what keeps the ExComm in 
 * the 'Rope' category rather than 'Snare' or 'Mountain'.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [cuban_missile_crisis_excomm_deliberation].
% 2. Report: ?- multi_index_report(cuban_missile_crisis_excomm_deliberation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(cuban_missile_crisis_excomm_delibration, rope, agent_power(institutional)).
