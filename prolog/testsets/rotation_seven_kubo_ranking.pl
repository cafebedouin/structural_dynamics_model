% ============================================================================
% CONSTRAINT STORY: rotation_seven_kubo_ranking
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "Rotation Seven" narrative
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_rotation_seven_kubo_ranking, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: rotation_seven_kubo_ranking
 * human_readable: R7 Kubo Credit and Ranking System
 * domain: economic/social/political
 * temporal_scope: Rotation Era
 * spatial_scope: Rotation Seven Station
 * 
 * SUMMARY:
 * The Kubo system is a gamified labor-extraction mechanism. It presents 
 * a "ladder" of advancement to children on the R7 station, rewarding 
 * agricultural output with "colored plastic" credits. While it appears 
 * as an unchangeable reality to the children (a False Mountain), it is 
 * an institutionally maintained Snare designed to extract maximum labor 
 * while suppressing awareness of the health costs (kidney failure).
 * 
 * KEY AGENTS:
 * - Rina/Anna (Individual Powerless): Subjects who initially "believe" in the ladder.
 * - Supervisor Kwan (Institutional): Manager who uses the system as a tool (Rope).
 * - The Station Logic (Analytical): The systemic logic benefiting from the labor extraction.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(rotation_seven_kubo_ranking, 0, 10).
narrative_ontology:constraint_claim(rotation_seven_kubo_ranking, snare).

% Base extractiveness: 0.75 (High).
% Essential agricultural labor and biological health are exchanged for 
% non-essential "credits" and minor privileges.
domain_priors:base_extractiveness(rotation_seven_kubo_ranking, 0.75).

% Suppression: 0.90 (Extreme).
% Total enclosure. No memory of Earth. Alternatives are non-existent 
% in the conceptual field of the children.
domain_priors:suppression_score(rotation_seven_kubo_ranking, 0.90).

% Enforcement: Requires active maintenance.
% Enforced via wristbands, medical isolation (R7), and Kwan's tablet.
domain_priors:requires_active_enforcement(rotation_seven_kubo_ranking).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(rotation_seven_kubo_ranking, station_command).
constraint_victim(rotation_seven_kubo_ranking, station_children).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: RINA/ANNA (THE CHILDREN) - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Subjects who initially "believe" in the ladder)
   WHEN: biographical (Their entire lives within the system)
   WHERE: trapped (No escape from the ranking system)
   
   WHY THIS CLASSIFICATION:
   For the children, the Kubo system is a 'Snare'. They are trapped in a
   gamified labor system where their health and well-being are extracted
   in exchange for "colored plastic" credits. The system is designed to
   suppress awareness of the true cost of their labor.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rotation_seven_kubo_ranking,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: SUPERVISOR KWAN - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Manager who uses the system as a tool)
   WHEN: generational (Maintaining the system over multiple rotations)
   WHERE: constrained (Bound by the station's protocols, but with local control)
   
   WHY THIS CLASSIFICATION:
   For Supervisor Kwan, the Kubo system is a 'Rope'. It is a functional tool
   to manage labor, ensure agricultural output, and maintain order among the
   children. He can "adjust rankings with a tablet tap," using the system to
   coordinate the workforce.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rotation_seven_kubo_ranking,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(constrained),
        spatial_scope(regional)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE STATION LOGIC - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (The systemic logic benefiting from the labor extraction)
   WHEN: historical (The long-term survival of the station)
   WHERE: analytical (Observing the system as a whole)
   
   WHY THIS CLASSIFICATION:
   From the perspective of the Station Logic, the Kubo system is a 'Mountain'.
   It is an immutable, necessary component of the station's long-term survival strategy.
   The extraction of labor and health is a calculated cost for the benefit of
   the entire station, a fixed reality of their existence.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    rotation_seven_kubo_ranking,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(rotation_seven_kubo_ranking_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_kubo_ranking, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(rotation_seven_kubo_ranking_tests).

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
 * 1. MANDATROPHY STATUS: High extractiveness (0.75) and suppression (0.90)
 *    confirm this as a severe Mandatrophic constraint. It is 'RESOLVED' by
 *    showing the 'Snare' for the children, the 'Rope' for the managers, and
 *    the 'Mountain' for the station's survival logic.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Children (Snare): Trapped in a gamified labor system with fatal consequences.
 *    - Supervisor Kwan (Rope): A tool for labor management and social control.
 *    - Station Logic (Mountain): An immutable necessity for the station's survival.
 * 
 * 3. CORE INSIGHT: The Kubo ranking system is a chilling example of a
 *    Mandatrophic 'Snare' presented as a 'Rope' of advancement. It highlights
 *    the extreme exploitation that can occur when a system's true costs are
 *    suppressed from those who bear them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */

omega_variable(
    kubo_system_collapse_trigger,
    "What is the critical failure point of the Kubo system? Will it be a psychological breakdown (rebellion), a biological catastrophe (disease outbreak), or a technological failure?",
    resolution_mechanism("Long-term monitoring of health data, social unrest metrics, and system-wide agricultural output within the R7 station."),
    impact("If rebellion: The 'Snare' is cut by the victims. If biological collapse: The 'Snare' strangles the entire station. If technological failure: The 'Rope' of control frays."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Direct Democratic Labor Allocation
 *    Viability: Allowing the children to manage their own labor and health risks democratically.
 *    Suppression: Explicitly suppressed by the "Ladder" narrative of individual advancement and the authority of supervisors like Kwan. The system is designed to prevent collective action.
 *
 * CONCLUSION:
 * The Kubo system is a 'Snare' that masquerades as a 'Rope' of opportunity.
 * By suppressing the alternative of collective self-governance, it ensures a
 * steady supply of compliant labor, even at the cost of individual lives.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/rotation_seven_kubo_ranking].
 * 2. Multi-perspective: ?- multi_index_report(rotation_seven_kubo_ranking).
 * 3. Run tests: ?- run_tests(rotation_seven_kubo_ranking_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */