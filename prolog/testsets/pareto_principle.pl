% ============================================================================
% CONSTRAINT STORY: pareto_principle
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Statistical Observation / Vilfredo Pareto (1896)
% ============================================================================

:- module(constraint_pareto_principle, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    constraint_beneficiary/2,
    constraint_victim/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: pareto_principle
 * human_readable: The Pareto Principle (80/20 Rule)
 * domain: economic/social/technological
 * temporal_scope: Permanent (Universal Law of Power Distributions)
 * spatial_scope: Global (Systems with Cumulative Advantage)
 * * SUMMARY:
 * The Pareto Principle states that for many outcomes, roughly 80% of consequences 
 * come from 20% of causes. This power-law distribution manifests in wealth, 
 * software bugs, city sizes, and biological populations. It represents a 
 * fundamental asymmetry in how value or effort is distributed within a system.
 * * KEY AGENTS:
 * - The System Optimizer: Seeks to identify the high-leverage 20% to maximize ROI.
 * - The "Vital Few": The 20% that hold the majority of resources or influence.
 * - The "Trivial Many": The 80% that contribute a minority of the output or wealth.
 * * NARRATIVE ARC:
 * The principle acts as a gravity-like force. In a state of nature, systems 
 * drift toward this distribution (Mountain). For those managing complexity, it 
 * is a tool for focus (Rope). For those in the bottom 80% of a wealth 
 * distribution, the "Matthew Effect" (rich getting richer) makes it a Noose.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(pareto_interval, 0, 10).
narrative_ontology:constraint_claim(pareto_principle, mountain).

% Base extractiveness: 0.4 (Moderate)
% Rationale: It describes an inherent asymmetry where a small group of inputs 
% "extracts" or captures the majority of the outputs.
domain_priors:base_extractiveness(pareto_principle, 0.4).

% Suppression score: 0.3 (Low)
% Rationale: Alternatives (Normal distributions) are visible and frequent in 
% nature (e.g., height), but in complex networks, the Pareto distribution 
% is rarely successfully "suppressed" for long.
domain_priors:suppression_score(pareto_principle, 0.3).

% Enforcement: Emerges naturally from preferential attachment and power laws.
domain_priors:emerges_naturally(pareto_principle).

% Metrics for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(pareto_principle, extractiveness, 0.4).
narrative_ontology:constraint_metric(pareto_principle, suppression_requirement, 0.3).

% Beneficiaries: High-leverage actors and the existing elite (The 20%).
constraint_beneficiary(pareto_principle, vital_few).

% Victims: The majority who compete for the remaining 20% of value (The 80%).
constraint_victim(pareto_principle, trivial_many).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE STATISTICIAN - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: analytical (The scientific observer).
   WHEN: civilizational (A permanent mathematical reality).
   WHERE: trapped (The distribution is an emergent property of complex systems).
   SCOPE: global.
   
   WHY THIS CLASSIFICATION:
   From a purely mathematical stance, the 80/20 rule is a Mountain. It is an 
   invariant feature of systems with feedback loops and cumulative advantage. 
   One does not "change" the power law; one merely measures its exponent.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    pareto_principle,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE BUSINESS MANAGER - ROPE
   --------------------------------------------------------------------------
   
   WHO: individual_moderate (A professional optimizing a system).
   WHEN: biographical (Achieving goals within a career).
   WHERE: mobile (Can shift focus to different inputs).
   SCOPE: national/regional.
   
   WHY THIS CLASSIFICATION:
   For the manager, the principle is a Rope. It is a coordination mechanism for 
   productivity. By focusing on the "vital few" customers or tasks, they can 
   achieve disproportionate results with minimal effort. It is a tool for 
   leveraging reality.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    pareto_principle,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE BOTTOM-TIER COMPETITOR - NOOSE
   --------------------------------------------------------------------------
   
   WHO: individual_powerless (A small creator or worker in the tail end).
   WHEN: immediate (Fighting for survival in a winner-take-all market).
   WHERE: constrained (Locked into a system where rewards are centralized).
   SCOPE: local.
   
   WHY THIS CLASSIFICATION:
   For someone in the "long tail," the principle is a Noose. The more the system 
   grows, the more value concentrates at the top (The 20%). This makes it 
   increasingly difficult for those at the bottom to gain traction, as the 
   distribution naturally "strangles" those without initial leverage.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    pareto_principle,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(pareto_principle_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(pareto_principle, Type1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(pareto_principle, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(pareto_principle, Type3, context(agent_power(individual_powerless), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(leverage_detection) :-
    % High leverage (Manager) sees it as a Rope; Low leverage (Worker) as a Noose.
    domain_priors:base_extractiveness(pareto_principle, E),
    E >= 0.3.

test(immutability_scale) :-
    % Civilizational scale correctly identifies it as a Mountain.
    constraint_indexing:effective_immutability(civilizational, trapped, mountain).

:- end_tests(pareto_principle_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE SHIFT: The 80/20 rule is often taught as a positive (Rope), 
 * but for those on the losing side of a power law, it is a structural barrier. 
 * 2. EXTRACTIVENESS: Set to 0.4 because the distribution "extracts" 
 * opportunity from the many and gives it to the few through 
 * "Preferential Attachment."
 * 3. NOOSE VS MOUNTAIN: It's a Mountain to the observer, but for the 
 * person actually being pushed out of the market by the "Vital Few," it 
 * feels like an active Noose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    preferential_attachment_origin,
    "Is the 80/20 distribution a result of initial skill/quality (Rope) or pure stochastic luck (Noose)?",
    resolution_mechanism("Simulating high-fidelity agents with varying luck vs. skill ratios"),
    impact("If Skill: The distribution is a Rope for meritocracy. If Luck: It is a Noose of unfair advantage."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Normal Distribution (Gaussian)
 * Viability: Occurs in physical traits, but rarely in information or wealth 
 * systems.
 * Suppression: Low (it's a natural phenomenon, not a suppressed one).
 * * ALTERNATIVE 2: Radical Egalitarian Redistribution
 * Viability: Mathematically suppresses the Pareto tail.
 * Suppression: High (viewed as economically inefficient or coercive).
 * * CONCLUSION:
 * The Pareto Principle is widely accepted as a Mountain, but the 
 * "suppression" of egalitarian alternatives often makes it function as 
 * a Noose for the bottom 80%.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_pareto_principle].
 * 2. Run multi-perspective: ?- multi_index_report(pareto_principle).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
