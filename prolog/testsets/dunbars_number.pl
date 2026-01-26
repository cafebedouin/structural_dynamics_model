% ============================================================================
% CONSTRAINT STORY: dunbars_number
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Robin Dunbar (1992) / Evolutionary Psychology / Anthropology
% ============================================================================

:- module(constraint_dunbars_number, []).

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
 * * constraint_id: dunbars_number
 * human_readable: Dunbar's Number
 * domain: social/technological
 * temporal_scope: Permanent (Evolved Human Cognition)
 * spatial_scope: Global (Human Social Networks)
 * * SUMMARY:
 * Dunbar's number is a suggested cognitive limit to the number of people with 
 * whom one can maintain stable social relationships—relationships in which 
 * an individual knows who each person is and how each person relates to 
 * every other person. Usually cited as approximately 150, it is based on a 
 * correlation between primate brain size and social group size.
 * * KEY AGENTS:
 * - The Evolutionary Psychologist: Analytical observer mapping the neural 
 * constraints of the neocortex.
 * - The Community Architect: Institutional agent using the number to 
 * design manageable organizational units or software platforms.
 * - The Socially Overwhelmed Individual: Individual powerless agent 
 * experiencing "social fatigue" and the extraction of attention when 
 * pushed beyond their cognitive ceiling.
 * * NARRATIVE ARC:
 * The number functions as a "Mountain" of biological reality—the hardware 
 * limit of human memory and social processing. For a company founder, it 
 * acts as a "Rope" for coordination (knowing when to split a growing office 
 * into two). However, for a user on a hyper-connected social network, it 
 * becomes a "Snare," as the platform extracts attention for 500+ "friends," 
 * strangling the quality of deep, stable relationships.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural identification
narrative_ontology:interval(dunbar_interval, 0, 10).
narrative_ontology:constraint_claim(dunbars_number, mountain).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.4). The limit extracts "social energy." When 
% networks grow too large, the individual's limited attention is 
% fragmented, extracting the utility of their primary social bonds.
domain_priors:base_extractiveness(dunbars_number, 0.4).

% Suppression score (0.0-1.0)
% Rationale: Moderate (0.5). It suppresses the alternative of "Infinite 
% Community." While we can imagine global brotherhood, our biology 
% suppresses the ability to actually process it as a stable social reality.
domain_priors:suppression_score(dunbars_number, 0.5).

% Enforcement requirements
% Emerges naturally from neocortex volume and cognitive processing limits.
domain_priors:emerges_naturally(dunbars_number).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(dunbars_number, extractiveness, 0.4).
narrative_ontology:constraint_metric(dunbars_number, suppression_requirement, 0.5).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(dunbars_number, [small_tribes, focused_workgroups]).
constraint_victim(dunbars_number, [hyper_connected_social_media_users, massive_unstructured_organizations]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================= */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ANTHROPOLOGIST - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of the evolved human hardware.
   WHEN: civilizational - Viewing human social structures over millennia.
   WHERE: trapped - The size of the human neocortex is a biological fixed point.
   SCOPE: global - Universal to the human species.
   
   WHY THIS CLASSIFICATION:
   To the scientist, Dunbar's number is a Mountain. It is an unchangeable 
   feature of the human "hardware." No amount of software or social 
   engineering has yet increased the underlying neural capacity for 
   multi-way social tracking. It is a fixed peak in the landscape of 
   human nature.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dunbars_number,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :-
    constraint_indexing:effective_immutability_for_context(
        context(analytical, civilizational, trapped, global),
        mountain
    ),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE STARTUP FOUNDER - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to structure a company and its culture.
   WHEN: biographical - Managing a single organization's growth (10-20 years).
   WHERE: arbitrage - Can choose to split the organization or remain small.
   SCOPE: regional - Local office or national company structure.
   
   WHY THIS CLASSIFICATION:
   For the founder, the number is a Rope. It is a vital coordination 
   mechanism. By knowing that culture breaks down once a team exceeds 
   ~150 people, they can use the limit to "tether" their growth strategy, 
   proactively splitting into smaller units to maintain trust and 
   efficiency.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dunbars_number,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(dunbars_number, E),
    E > 0.2, % Managed as a tool for social efficiency
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE OVER-NETWORKED USER - Snare
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the "Attention Economy" of a platform.
   WHEN: immediate - The daily fatigue of managing digital acquaintances.
   WHERE: constrained - High social cost to "quitting" or deleting friends.
   SCOPE: local - Immediate social circle and digital feed.
   
   WHY THIS CLASSIFICATION:
   For the person with 1,000 "friends" on a digital platform, Dunbar's number 
   is a Snare. The platform's design ignores their biological limit to 
   extract their attention. The attempt to "keep up" with everyone strangles 
   the time needed for their closest 5 or 15 people, extracting their 
   emotional well-being while they remain trapped in the "friend" count.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dunbars_number,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(dunbars_number, E),
    E > 0.3, % Extraction of attention and relationship depth
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(dunbars_number_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Snare
    constraint_indexing:constraint_classification(dunbars_number, mountain, context(analytical, civilizational, trapped, global)),
    constraint_indexing:constraint_classification(dunbars_number, rope, context(institutional, biographical, arbitrage, national)),
    constraint_indexing:constraint_classification(dunbars_number, snare, context(individual_powerless, immediate, constrained, local)).

test(power_extractiveness_attention) :-
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(dunbars_number, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(dunbars_number, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_cognition) :-
    % Biological evolution scale = Mountain
    constraint_indexing:effective_immutability_for_context(context(analytical, civilizational, trapped, global), mountain).

:- end_tests(dunbars_number_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.4):
 * Dunbar's number extracts "social quality." In a large system, it forces 
 * the individual to pay for breadth with depth. It's a moderate score 
 * because the extraction is often psychological rather than material.
 * * 2. PERSPECTIVE SELECTION:
 * Selected the Anthropologist (Law), the Founder (Tool), and the 
 * Digital User (Victim) to highlight how a biological ceiling shifts 
 * from a fact of nature to a structural trap in the attention economy.
 * * 3. NOOSE LOGIC:
 * Applied to the "over-networked" individual, where the social cost of 
 * leaving a platform prevents them from returning to their biological 
 * comfort zone, creating an attention trap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_cognitive_offloading,
    "Can AI and CRM tools effectively 'untie' the Snare of Dunbar's Number 
    by managing our social memory (Rope), or is the constraint biological 
    at the level of emotional trust (Mountain)?",
    resolution_mechanism("Long-term study of high-connectedness groups 
    using advanced AI social-memory assistants"),
    impact("If Rope: Human social reach can expand. If Mountain: Trust 
    always collapses at the biological boundary."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Formal Bureaucracy
 * Viability: High. Allows millions of people to work together by replacing 
 * "trust-based" relationships with "rule-based" ones.
 * Suppression: None. Bureaucracy is the standard "Rope" used to manage 
 * the Dunbar Mountain, though it is often experienced as a Snare.
 * * CONCLUSION:
 * Since we can use rules (Bureaucracy) to coordinate, the *need* for 
 * trust-based 150-person groups becomes a Mountain we build around, 
 * using the Rope of formal institutions.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_dunbars_number].
 * 2. Multi-perspective: ?- multi_index_report(dunbars_number).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
