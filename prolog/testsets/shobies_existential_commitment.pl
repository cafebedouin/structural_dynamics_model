% ============================================================================
% CONSTRAINT STORY: shobies_existential_commitment
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: Ursula K. Le Guin, “The Shobies’ Story”
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_shobies_existential_commitment, []).

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
 * * constraint_id: shobies_existential_commitment
 * human_readable: The Risk-Safety Paradox
 * domain: social/religious/existential
 * temporal_scope: Perennial / Multi-generational
 * spatial_scope: Global / Foundational
 * * SUMMARY:
 * This constraint defines a state where safety and functionality are paradoxically dependent on total existential risk. It posits that "nothing works" unless an agent commits their "soul" to it, and that "nothing’s safe" except that which has been put at risk.
 * * KEY AGENTS:
 * - The Committer (The Shoby): An agent who "stakes everything" to achieve a functional reality.
 * - The Cautious (The Victim): An agent who seeks safety through risk-aversion, only to find that "nothing works" for them.
 * - The Sage (Narrator): An analytical observer who recognizes the "soul" as the necessary currency of coordination.
 * * NARRATIVE ARC:
 * The agents are engaged in a project (likely a voyage or social creation) where they realize that standard safety protocols are illusory. To survive or succeed, they must "give their souls" to the endeavor. The constraint acts as a "Mountain" of metaphysical law that demands total "Snare-like" extraction of the self to become a functional "Rope" for collective survival.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(shobies_existential_commitment, 0, 10).
narrative_ontology:constraint_claim(shobies_existential_commitment, [existential_coordination]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: High (0.65). The constraint requires agents to "stake everything" and "give our souls" to the task. This represents a severe extraction of individual autonomy and security in exchange for the "safe" result.
domain_priors:base_extractiveness(shobies_existential_commitment, 0.65).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: Moderate (0.5). The claim "nothing works except..." suppresses the validity of low-risk, low-commitment alternatives, framing them as fundamentally broken.
domain_priors:suppression_score(shobies_existential_commitment, 0.5).

% Enforcement requirements
% Emerges naturally from the nature of high-stakes coordination but requires active internal enforcement (giving the soul).
domain_priors:requires_active_enforcement(shobies_existential_commitment).



% BENEFICIARIES & VICTIMS
constraint_beneficiary(shobies_existential_commitment, collective_cohesion). % The project "works".
constraint_victim(shobies_existential_commitment, individual_caution).    % The cautious agent's soul/risk.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE COMMITTED NAVIGATOR - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (Has the agency to "give their soul")
   WHEN: biographical (The duration of the project/voyage)
   WHERE: mobile (Moving through risk toward safety)
   SCOPE: local (The immediate coordination task)
   
   WHY THIS CLASSIFICATION:
   For the agent who has accepted the terms, the paradox is a "Rope"—the only tool that allows things to "work". Commitment is the coordination mechanism that makes life possible.
   
   NARRATIVE EVIDENCE:
   "staking everything on it... because nothing works except what we give our souls to".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    shobies_existential_commitment,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE HESITANT SUBJECT - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A "little person" whose soul is being demanded)
   WHEN: immediate (The terrifying moment of putting things at risk)
   WHERE: trapped (No safe, low-risk alternative exists that "works")
   SCOPE: local (The personal cost)
   
   WHY THIS CLASSIFICATION:
   For the uncommitted or fearful agent, this is a "Snare." The requirement to "stake everything" feels like total extraction, strangling their sense of safety until they surrender their autonomy.
   
   NARRATIVE EVIDENCE:
   "nothing’s safe except what we put at risk".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    shobies_existential_commitment,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE SAGE / NARRATOR - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical (The "ancient and wise" observer of reality)
   WHEN: civilizational (A fundamental law of how anything works)
   WHERE: analytical (Observer stance)
   SCOPE: global (Applies to all meaningful endeavors)
   
   WHY THIS CLASSIFICATION:
   To the observer, this is a "Mountain"—an unchangeable law of nature. It is a zero-degree-of-freedom reality: risk and commitment are the only paths to functionality.
   
   NARRATIVE EVIDENCE:
   "nothing works except... nothing’s safe except...".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    shobies_existential_commitment,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 4: THE PROJECT LEAD - Rope
   --------------------------------------------------------------------------
   WHO: institutional (The leader responsible for mission success)
   WHEN: biographical (The project's lifecycle)
   WHERE: constrained (Cannot exit the project, but can make decisions)
   SCOPE: regional (The scope of the project)
   
   WHY THIS CLASSIFICATION:
   From the institutional perspective of the project lead, the crew's total
   commitment is the fundamental coordination mechanism (Rope) that makes
   an impossible mission viable. The leader wields this existential
   imperative as a tool to bind the team and ensure the project "works".
   The high stakes are not a bug, but the feature that guarantees cohesion.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    shobies_existential_commitment,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(regional)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(shobies_existential_commitment_tests).

test(multi_perspective_commitment) :-
    % Navigator sees Rope
    constraint_indexing:constraint_classification(shobies_existential_commitment, Type1, context(agent_power(individual_moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(local))),
    % Subject sees Snare
    constraint_indexing:constraint_classification(shobies_existential_commitment, Type2, context(agent_power(individual_powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(local))),
    % Sage sees Mountain
    constraint_indexing:constraint_classification(shobies_existential_commitment, Type3, context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global))),
    % Project Lead sees Rope
    constraint_indexing:constraint_classification(shobies_existential_commitment, Type4, context(agent_power(institutional), time_horizon(biographical), exit_options(constrained), spatial_scope(regional))),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3. % Rope is not Mountain

test(power_extractiveness_commitment) :-
    % The powerless experience higher extraction (fear of loss)
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextModerate = context(individual_moderate, biographical, mobile, local),
    constraint_indexing:extractiveness_for_agent(shobies_existential_commitment, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(shobies_existential_commitment, ContextModerate, Score2),
    Score1 > Score2.

:- end_tests(shobies_existential_commitment_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-21
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.65):
 * Reasoning: Chose high because "staking everything" is a literal liquidation of current security.
 * Evidence: The text implies that functionality is impossible without this total buy-in.
 * * 2. PERSPECTIVE SELECTION:
 * Contrasted the Navigator (Rope) with the Hesitant Subject (Snare) to demonstrate the "Mandatrophy" resolution: high extraction becomes a functional tool when the agent adopts the "institutional" goal of the project.
 * * 3. CLASSIFICATION RATIONALE:
 * The Analyst (Mountain) sees this as a perennial truth, whereas the participant sees it as a terrifying, high-stakes choice.
 *
 * 5. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 *    omega_variable(
 *        risk_safety_causality,
 *        "Is risk truly the cause of safety (Mountain) or is this a narrative Rope used to justify extreme labor and danger?",
 *        resolution_mechanism("Comparison of project success rates in 'total risk' vs 'mitigated risk' environments"),
 *        impact("If Mountain: The Shobies are wise. If narrative: They are victims of a cult-like Snare."),
 *        confidence_without_resolution(medium)
 *    ).
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Bureaucratic / Mitigated Safety
 * Viability: Standard risk-management protocols (insurance, back-ups).
 * Suppression: Explicitly rejected by the text: "nothing's safe except...".
 * Evidence: "nothing works except what we give our souls to".
 * * CONCLUSION:
 * The active suppression of Alternative 1 makes the Shobies' claim a Snare for those who would prefer standard safety, but a Rope for the "awakened" Navigator.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/shobies_existential_commitment].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report(shobies_existential_commitment).
 * 
 * 3. Run tests:
 *    ?- run_tests(shobies_existential_commitment_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report(shobies_existential_commitment).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints(shobies_existential_commitment, [other_id]).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
