% ============================================================================
% CONSTRAINT STORY: sts86_ascent_checklist
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: JSC-48005-86 - STS-86 Ascent Checklist
% ============================================================================

:- module(constraint_sts86_ascent_checklist, []).

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
 * 
 * constraint_id: sts86_ascent_checklist
 * human_readable: Space Shuttle Ascent/Abort Procedural Matrix
 * domain: technological/institutional
 * temporal_scope: immediate (T- minus 5m to Orbit)
 * spatial_scope: cockpit/national
 * 
 * SUMMARY:
 * The checklist (JSC-48005) represents the ultimate procedural constraint. 
 * It manages the transition from a ground-controlled environment to a 
 * physics-dominated ascent where human agency is bound by rigid logic gates.
 * 
 * KEY AGENTS:
 * - NASA/Mission Operations (Institutional): The architect of the procedural matrix.
 * - Flight Crew (CDR/PLT) (Individual Moderate): The agents executing the script.
 * - The 'Powerless' Pilot (Individual Powerless): The crew-member in a 3-Engine-Out scenario.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(sts86_ascent_checklist, 0, 10).
narrative_ontology:constraint_claim(sts86_ascent_checklist, mountain).

% Base extractiveness: 0.05.
% NASA gains nothing from crew failure. 
% Benefit flow is aimed at systemic survival.
domain_priors:base_extractiveness(sts86_ascent_checklist, 0.05).

% Suppression: 0.95.
% Deviating from the checklist is treated 
% as a failure mode itself. Alternative "freestyle" flying is invisible.
domain_priors:suppression_score(sts86_ascent_checklist, 0.95).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(sts86_ascent_checklist, extractiveness, 0.05).
narrative_ontology:constraint_metric(sts86_ascent_checklist, suppression_requirement, 0.95).

% Enforcement: Requires active maintenance (Ground Control / CPCB).
domain_priors:requires_active_enforcement(sts86_ascent_checklist).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(sts86_ascent_checklist, mission_success).
constraint_victim(sts86_ascent_checklist, pilot_discretion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: NASA/MISSION OPERATIONS - Rope
   --------------------------------------------------------------------------
   WHO: institutional (The architect of the procedural matrix)
   WHEN: historical (From mission planning to execution)
   WHERE: arbitrage (Balances safety, performance, and mission objectives)
   
   WHY THIS CLASSIFICATION:
   For NASA, the checklist is a 'Rope'. It is a crucial coordination mechanism
   that synchronizes the actions of thousands of individuals and complex systems
   to ensure a successful launch. It is a tool for managing immense complexity
   and mitigating risk.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sts86_ascent_checklist,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: FLIGHT CREW (CDR/PLT) - Tangled Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (The agents executing the script)
   WHEN: immediate (During the ascent phase)
   WHERE: constrained (Bound by the checklist's procedures)
   
   WHY THIS CLASSIFICATION:
   For the flight crew, the checklist is a 'Tangled Rope'. It's a 'Rope' because
   it provides a clear, life-saving path through a high-stakes environment.
   It's 'Tangled' because it severely restricts their autonomy, suppressing
   discretionary decision-making in favor of rigid adherence to pre-planned procedures.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sts86_ascent_checklist,
    tangled_rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE 'POWERLESS' PILOT - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (The crew-member in a 3-Engine-Out scenario)
   WHEN: immediate (During a catastrophic failure)
   WHERE: trapped (Bound by the laws of physics and the only available abort mode)
   
   WHY THIS CLASSIFICATION:
   In a 3-Engine-Out scenario, the checklist becomes a 'Mountain'. It is no
   longer a choice but a reflection of the physical laws governing the shuttle's
   trajectory. The "BAILOUT" procedure is the only path, an immutable reality
   dictated by the physics of the situation.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    sts86_ascent_checklist,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(sts86_ascent_checklist_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(sts86_ascent_checklist, Type1, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(sts86_ascent_checklist, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(sts86_ascent_checklist, Type3, context(agent_power(powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(sts86_ascent_checklist_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. CLASSIFICATION RATIONALE:
 *    - NASA (Rope): A tool for coordination and risk management.
 *    - Flight Crew (Tangled Rope): A life-saving guide that constrains autonomy.
 *    - Powerless Pilot (Mountain): An immutable reality dictated by physics.
 * 
 * 2. CORE INSIGHT: The STS-86 Ascent Checklist demonstrates how a single
 *    constraint can transform based on context. What is a flexible 'Rope' for
 *    the institution becomes a 'Tangled Rope' for the operators and, in a crisis,
 *    a stark 'Mountain' of physical necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the interplay between human agency and procedural rigidity in extreme situations.
 */

omega_variable(
    automation_vs_discretion,
    "In an unforeseen crisis, is strict adherence to the checklist (Mountain) more likely to lead to survival than a crew commander's discretionary action (Rope), and at what point does the checklist become a 'Snare'?",
    resolution_mechanism("Analysis of historical spaceflight incidents; advanced simulation of off-nominal scenarios with human-in-the-loop testing."),
    impact("If adherence is better: The 'Mountain' is a safe haven. If discretion is better: The 'Mountain' is a potential 'Snare'."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: "Freestyle" Piloting (Full Discretion)
 *    Viability: Theoretically possible but catastrophic in practice due to the complexity of the system.
 *    Suppression: Actively and entirely suppressed by the institutional culture of NASA, training, and the very existence of the checklist. Deviation is treated as a severe failure mode.
 *
 * CONCLUSION:
 * The STS-86 checklist is a 'Rope' that actively suppresses the alternative of
 * full pilot discretion, which, in such a complex system, would almost certainly
 * be a fatal 'Snare'. The rigidity of the checklist is a direct response to the
 * unforgiving 'Mountain' of orbital mechanics.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/sts86_ascent_checklist].
 * 2. Multi-perspective: ?- multi_index_report(sts86_ascent_checklist).
 * 3. Run tests: ?- run_tests(sts86_ascent_checklist_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */