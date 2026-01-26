% ============================================================================
% CONSTRAINT STORY: iron_law_of_oligarchy
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Robert Michels (1911) / Political Sociology
% ============================================================================

:- module(constraint_iron_law_of_oligarchy, []).

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
 * * constraint_id: iron_law_of_oligarchy
 * human_readable: The Iron Law of Oligarchy
 * domain: political/social
 * temporal_scope: 1911-Present (Modern Organizational Theory)
 * spatial_scope: Global (Large-scale Organizations)
 * * SUMMARY:
 * Proposed by Robert Michels, this principle states that all complex organizations, 
 * regardless of how democratic they are at the start, eventually develop into 
 * oligarchies. The necessity of bureaucracy and specialized knowledge creates 
 * a leadership class that prioritizes its own survival over the group's original goals.
 * * KEY AGENTS:
 * - The Elite Leader: Benefits from specialized information and control over the 
 * administrative apparatus.
 * - The Rank-and-File Member: The original source of the organization's power, 
 * who becomes increasingly alienated from decision-making.
 * - The Organizational Scientist: Observes the inevitable shift from horizontal 
 * ideals to vertical hierarchies.
 * * NARRATIVE ARC:
 * The law functions as an entropic force in social movements. It begins as a 
 * "Rope" (the need for leaders to coordinate action), but hardens into a 
 * "Mountain" of bureaucratic inertia. For those seeking radical change, it 
 * becomes a "Snare" when the organization they built to fight power becomes 
 * the very power that suppresses them.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(oligarchy_interval, 0, 10).
narrative_ontology:constraint_claim(iron_law_of_oligarchy, mountain).

% Base extractiveness: 0.6 (Moderate-High)
% Rationale: Leaders extract status, specialized knowledge, and administrative 
% control from the collective, often steering resources toward self-preservation.
domain_priors:base_extractiveness(iron_law_of_oligarchy, 0.6).

% Suppression score: 0.6 (Moderate-High)
% Rationale: Alternatives to hierarchy (like total horizontalism) are suppressed 
% by the "efficiency" requirements of large-scale operations.
domain_priors:suppression_score(iron_law_of_oligarchy, 0.6).

% Enforcement: Emerges naturally from the technical necessity of organization.
domain_priors:emerges_naturally(iron_law_of_oligarchy).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(iron_law_of_oligarchy, extractiveness, 0.6).
narrative_ontology:constraint_metric(iron_law_of_oligarchy, suppression_requirement, 0.6).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(iron_law_of_oligarchy, leadership_elite).
constraint_victim(iron_law_of_oligarchy, democratic_idealists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE POLITICAL SCIENTIST - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (The objective observer)
   WHEN: civilizational (Observing a universal law of social physics)
   WHERE: trapped (Human organization seems bound by this pattern)
   SCOPE: global
   
   WHY THIS CLASSIFICATION:
   To the scientist, the law is a Mountain. It is an unchangeable consequence 
   of scale and complexity. Once an organization reaches a certain size, 
   differentiation between leaders and led is a structural certainty.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    iron_law_of_oligarchy,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE HIGH-LEVEL BUREAUCRAT - ROPE
   --------------------------------------------------------------------------
   
   WHO: institutional (In power, managing the organization)
   WHEN: biographical (Achieving goals through professional systems)
   WHERE: arbitrage (Can move between different hierarchical structures)
   SCOPE: national
   
   WHY THIS CLASSIFICATION:
   For the bureaucrat, hierarchy is a Rope. It is the only way to get things done. 
   Centralized leadership provides the coordination, speed, and specialization 
   required to achieve the organization's mission effectively.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    iron_law_of_oligarchy,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE GRASSROOTS ACTIVIST - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A member of a movement that has "matured")
   WHEN: immediate (Seeking direct representation and action now)
   WHERE: constrained (Must work through the movement they joined)
   SCOPE: local
   
   WHY THIS CLASSIFICATION:
   For the activist, the law is a Snare. They find themselves in an organization 
   where the leaders have "sold out" or become more interested in maintaining 
   their offices than in the original cause. The very hierarchy that was meant 
   to empower the collective now strangles internal dissent.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    iron_law_of_oligarchy,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(iron_law_of_oligarchy_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(iron_law_of_oligarchy, Type1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(iron_law_of_oligarchy, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(iron_law_of_oligarchy, Type3, context(agent_power(individual_powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_scaling) :-
    ContextPowerless = context(individual_powerless, immediate, constrained, local),
    ContextInstitutional = context(institutional, biographical, arbitrage, national),
    % While the model reports a base score, the experienced extraction scales
    % because the institution gains what the powerless lose.
    domain_priors:base_extractiveness(iron_law_of_oligarchy, Score),
    Score > 0.5.

test(time_immutability) :-
    % Long horizon = mountain (the "Iron" part of the law)
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(iron_law_of_oligarchy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS MADE BY MODEL:
 * * 1. BASE EXTRACTIVENESS (0.6):
 * Reasoning: Hierarchy inherently creates a differential in power and reward. 
 * Leaders gain "psychological" and "administrative" rents.
 * * 2. SUPPRESSION SCORE (0.6):
 * Evidence: Michels argued that direct democracy is physically impossible 
 * for large groups, making the "alternatives" seem like a fantasy, thus 
 * suppressing them as viable options.
 * * 3. PERSPECTIVE SELECTION:
 * Contrast between the "Efficiency" of the Leader (Rope) and the 
 * "Disillusionment" of the Activist (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_direct_democracy,
    "Do modern communication technologies (AI, Blockchain) break the 'Technical Necessity' of oligarchy?",
    resolution_mechanism("Longitudinal study of decentralized autonomous organizations (DAOs)"),
    impact("If Yes: The Law is a Rope that was tied by low-bandwidth communication. If No: It is a Mountain of human nature."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Liquid Democracy (Delegation by topic)
 * Viability: Theoretically allows for specialization without fixed elites.
 * Suppression: High. Dismissed by institutional parties as "unstable" or "technocratic."
 * * ALTERNATIVE 2: Horizontalism (Occupy-style consensus)
 * Viability: Works at small scales.
 * Suppression: Natural. Tends to collapse due to "the tyranny of structurelessness."
 * * CONCLUSION:
 * The absence of working, large-scale alternatives reinforces the status of 
 * this constraint as a "Mountain" for the current era.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_iron_law_of_oligarchy].
 * 2. Multi-perspective: ?- multi_index_report(iron_law_of_oligarchy).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
