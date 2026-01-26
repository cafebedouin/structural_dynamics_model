% ============================================================================
% CONSTRAINT STORY: social_loafing
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Psychometrics / Social Psychology / Ringelmann (1913) / Latane (1979)
% ============================================================================

:- module(constraint_social_loafing, []).

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
 * * constraint_id: social_loafing
 * human_readable: Social Loafing (The Ringelmann Effect)
 * domain: social/economic/psychological
 * temporal_scope: Permanent (Human Social Interaction)
 * spatial_scope: Global (Groups and Organizations)
 * * SUMMARY:
 * Social loafing is the phenomenon where individuals exert less effort when working 
 * collectively than when working individually. First observed by Max Ringelmann in 
 * rope-pulling tasks, it represents a fundamental diffusion of responsibility 
 * where individual contribution is masked by the group aggregate.
 * * KEY AGENTS:
 * - The Social Psychologist (Analytical): Observes the statistical decline in 
 * per-capita output as group size increases.
 * - The Team Manager (Institutional): Attempts to implement accountability 
 * mechanisms to mitigate effort loss.
 * - The Diligent Contributor (Individual Powerless): Subject to the increased 
 * burden of carrying the group's output when others slack.
 * * NARRATIVE ARC:
 * In any group endeavor, social loafing emerges as a "Mountain"—a natural 
 * byproduct of the human brain's energy-conservation heuristics. For the 
 * administrator, it is a "Rope" that must be managed through task 
 * individualization. However, for the high-performing individual in a 
 * low-accountability group, it becomes a "Snare," as the collective 
 * "failure to thrive" strangles their own results and extracts their labor.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural identification and ID extraction
narrative_ontology:interval(social_loafing_interval, 0, 10).
narrative_ontology:constraint_claim(social_loafing, snare).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.5). Social loafing extracts labor value from the 
% organization and effort from the "diligent" members to subsidize the "loafers."
domain_priors:base_extractiveness(social_loafing, 0.5).

% Suppression score (0.0-1.0)
% Rationale: Moderate (0.4). The "True Potential" of the group is suppressed 
% by the mask of the collective. Individuals' specific contributions are 
% often invisible to observers, aiding the loafing behavior.
domain_priors:suppression_score(social_loafing, 0.4).

% Enforcement: Emerges naturally from human group dynamics and energy conservation.
domain_priors:emerges_naturally(social_loafing).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(social_loafing, extractiveness, 0.5).
narrative_ontology:constraint_metric(social_loafing, suppression_requirement, 0.4).

% BENEFICIARIES & VICTIMS
% Beneficiary: The "Loafer" who saves energy while reaping group rewards.
constraint_beneficiary(social_loafing, low_effort_agents).
% Victim: The organization (lost output) and the diligent individual (overworked).
constraint_victim(social_loafing, [high_effort_agents, organizations]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE RESEARCHER - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of social-psychological laws.
   WHEN: civilizational - Viewing human nature as a biological constant.
   WHERE: analytical - Not a participant in the work-loop.
   SCOPE: global - Applies to all human group structures.
   
   WHY THIS CLASSIFICATION:
   To the researcher, social loafing is a Mountain. It is an unchangeable 
   feature of the "human hardware." Regardless of the task or culture, the 
   inverse relationship between group size and individual effort is a 
   predictable, fixed peak in the topography of group dynamics.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    social_loafing,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE PROJECT MANAGER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to structure tasks and evaluations.
   WHEN: biographical - Managing teams over a career.
   WHERE: arbitrage - Can shift between group and individual task structures.
   SCOPE: regional - Within a specific company or office.
   
   WHY THIS CLASSIFICATION:
   For management, the phenomenon is a Rope. It is a coordination challenge 
   to be managed. By breaking groups into smaller units or making individual 
   contributions identifiable, they "pull" the team toward higher 
   performance, using the constraint as a tether for organizational design.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    social_loafing,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DILIGENT STUDENT - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the grade/outcome of the group.
   WHEN: immediate - Today's project deadline.
   WHERE: trapped - Cannot exit the group without administrative penalty.
   SCOPE: local - The immediate project team.
   
   WHY THIS CLASSIFICATION:
   For the high-performer in a group project, social loafing is a Snare. 
   The presence of loafers "strangles" their ability to achieve a high grade 
   without doing the work of three people. The extraction of their extra labor 
   and time is felt as a direct trap created by the system's group-based reward structure.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    social_loafing,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(social_loafing_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain
    constraint_indexing:constraint_classification(social_loafing, mountain, context(analytical, civilizational, analytical, global)),
    % Institutional sees Rope
    constraint_indexing:constraint_classification(social_loafing, rope, context(institutional, biographical, arbitrage, regional)),
    % Powerless sees Snare
    constraint_indexing:constraint_classification(social_loafing, snare, context(individual_powerless, immediate, trapped, local)).

test(power_extractiveness_effort) :-
    % Powerless individuals feel the extraction of their labor (Snare).
    % Institutional managers use it as a design challenge (Rope).
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, arbitrage, regional),
    constraint_indexing:extractiveness_for_agent(social_loafing, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(social_loafing, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_loafing) :-
    % Over biographical time, the individual is trapped (Snare).
    % Over civilizational time, it is a Law of Human Nature (Mountain).
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(social_loafing_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. BASE EXTRACTIVENESS (0.5):
 * Reasoning: Social loafing is inherently extractive. It allows one party 
 * to preserve energy/utility at the expense of others' contributions. 
 * It is moderate because it doesn't always lead to total system failure.
 * * 2. PERSPECTIVE SELECTION:
 * Chose the Researcher (Law), the Manager (Tool), and the Student/Worker (Victim) 
 * to reflect the spectrum of how group dynamics are experienced.
 * * 3. CLASSIFICATION RATIONALE:
 * Analytical → Mountain: It is a statistical inevitability of human groups.
 * Institutional → Rope: It is a parameter in organizational engineering.
 * Individual Powerless → Snare: It is an unavoidable theft of labor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_anonymity_impact,
    "Does digital mediation exacerbate the social loafing 'Snare' via 
    increased anonymity, or can it untie it via automated tracking (Rope)?",
    resolution_mechanism("Longitudinal study of per-capita output in 
    remote vs. in-person high-stakes collaboration"),
    impact("If Mountain: Loafing is deeper online. If Rope: Digital tools 
    can effectively eliminate it."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Individualized Accountability (The Peer Review)
 * Viability: High. Making each person's work identifiable is the 
 * standard "exit" from the loafing trap.
 * Suppression: Moderate. Organizations often favor "Team Unity" 
 * rhetoric which suppresses individualized metrics.
 * * CONCLUSION:
 * The existence of accountability as an "exit" (Rope) confirms that for 
 * the powerless, social loafing is a Snare—they are denied the tools to 
 * prove their own effort by the group structure.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_social_loafing].
 * 2. Multi-perspective: ?- multi_index_report(social_loafing).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
