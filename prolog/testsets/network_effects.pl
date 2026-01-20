% ============================================================================
% CONSTRAINT STORY: network_effects
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Economics / Digital Strategy / Metcalfe's Law
% ============================================================================

:- module(constraint_network_effects, []).

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
 * * constraint_id: network_effects
 * human_readable: Network Effects (Demand-Side Economies of Scale)
 * domain: economic/technological
 * temporal_scope: Digital Age (1990-Present)
 * spatial_scope: Global (Digital Networks)
 * * SUMMARY:
 * A phenomenon where a product or service gains additional value as more people 
 * use it. This creates a powerful positive feedback loop that often leads to 
 * "winner-take-all" dynamics, where a dominant player becomes nearly impossible 
 * to displace due to the high cost for users to leave the established network.
 * * KEY AGENTS:
 * - The Platform Owner: Benefits from exponential value growth and "lock-in."
 * - The Early Adopter: Gains value from network growth but may face future costs.
 * - The Late Entrant/Competitor: Faces a massive barrier to entry regardless of 
 * product quality.
 * * NARRATIVE ARC:
 * Initially, network effects function as a coordination tool (Rope). As the 
 * network matures and hits a critical mass, the feedback loop hardens into a 
 * barrier (Mountain) for competitors and a "switching cost" trap (Noose) for 
 * users who wish to leave but cannot take their "network value" with them.
 */

/* ==========================================================================
   2. BASE PROPERTIES (The "Reality" Layer)
   ========================================================================== */

% Required for structural integration
narrative_ontology:interval(network_effects_interval, 0, 10).
narrative_ontology:constraint_claim(network_effects, mountain).

% Base extractiveness: 0.5 (Moderate)
% Rationale: While users gain utility from the network, the platform owner 
% extracts disproportionate value through data, advertising, or rent once 
% "lock-in" is achieved.
domain_priors:base_extractiveness(network_effects, 0.5).

% Suppression score: 0.6 (High)
% Rationale: Incumbent networks actively suppress alternatives by preventing 
% interoperability or "multi-homing," making the exit to a new platform 
% conceptually and practically difficult.
domain_priors:suppression_score(network_effects, 0.6).

% Enforcement requirements: Emerges naturally from user behavior and Metcalfe's Law.
domain_priors:emerges_naturally(network_effects).

% Metrics for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(network_effects, extractiveness, 0.5).
narrative_ontology:constraint_metric(network_effects, suppression_requirement, 0.6).

% Beneficiaries: The First Mover and the Platform Monopolist.
constraint_beneficiary(network_effects, platform_owner).

% Victims: Potential competitors and users facing "walled garden" pricing.
constraint_victim(network_effects, innovative_startups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE STRATEGIC COMPETITOR - MOUNTAIN
   --------------------------------------------------------------------------
   
   WHO: Individual Moderate/Powerful (A startup with a better product).
   WHEN: Biographical (The timeframe of a business venture).
   WHERE: Trapped (By the lack of a user base).
   SCOPE: Global.
   
   WHY THIS CLASSIFICATION:
   For a competitor, network effects are a Mountain. Even with superior 
   technology, they cannot move the "mass" of the existing user base. The 
   network effect acts as a natural law of the marketplace that dictates 
   "the biggest wins."
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    network_effects,
    mountain,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE ACTIVE USER - ROPE
   --------------------------------------------------------------------------
   
   WHO: Individual Powerless (Standard consumer).
   WHEN: Immediate (Utility today).
   WHERE: Mobile (Can technically use multiple apps).
   SCOPE: Local/Regional.
   
   WHY THIS CLASSIFICATION:
   For the user, the network is a Rope. It is a coordination mechanism that 
   makes life easier. The fact that everyone is on the same platform allows 
   for seamless communication and social coordination. It is a beneficial 
   tether rather than a restriction.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    network_effects,
    rope,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(regional)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE DISGRUNTLED CREATOR - NOOSE
   --------------------------------------------------------------------------
   
   WHO: Individual Moderate (Creator/Influencer dependent on the platform).
   WHEN: Generational (Their career is built on the network).
   WHERE: Constrained (Leaving means losing their entire audience).
   SCOPE: Global.
   
   WHY THIS CLASSIFICATION:
   For a creator, the network effect is a Noose. They may hate the platform's 
   new rules or increased fees, but they cannot leave because their entire 
   social and economic capital is locked into that specific network. The 
   cost of "exiting" is the destruction of their livelihood.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    network_effects,
    noose,
    context(
        agent_power(individual_moderate),
        time_horizon(generational),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(network_effects_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(network_effects, Type1, context(agent_power(individual_moderate), biographical, trapped, _)),
    constraint_indexing:constraint_classification(network_effects, Type2, context(agent_power(individual_powerless), immediate, mobile, _)),
    constraint_indexing:constraint_classification(network_effects, Type3, context(agent_power(individual_moderate), generational, constrained, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(power_extractiveness_scaling) :-
    % Higher power (platform owner) experiences less extraction (negative) 
    % than the powerless user/creator.
    domain_priors:base_extractiveness(network_effects, Score),
    Score >= 0.5.

test(time_immutability) :-
    % Long-term dependency (Generational) + High Exit Cost = Noose.
    constraint_indexing:constraint_classification(network_effects, noose, context(_, generational, constrained, _)).

:- end_tests(network_effects_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. SUPPRESSION SCORE (0.6): High because incumbents often use "anti-steering" 
 * policies or proprietary data formats to prevent users from bridging to 
 * other networks.
 * 2. NOOSE VS MOUNTAIN: For a user, it's a Noose (they choose to stay but 
 * feel forced); for a competitor, it's a Mountain (it's just a physical 
 * fact of the market).
 * 3. EXTRACTIVENESS: Set at 0.5 to reflect the "double-edged" nature—users 
 * get real value, but the owner takes a significant "tax" on that value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    interoperability_standardization,
    "Will government mandates for data portability effectively decouple 'social capital' from 'platform identity'?",
    resolution_mechanism("Monitoring the success of the Digital Markets Act (DMA) in the EU"),
    impact("If Yes: Network Effects move from Mountain to Rope. If No: They remain a Noose."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Federated Networks (e.g., Email, Mastodon)
 * Viability: High (The underlying protocols are mature).
 * Suppression: High (Consumer friction and lack of marketing make them 
 * less visible than centralized 'walled gardens').
 * * ALTERNATIVE 2: Multi-homing (Using multiple platforms simultaneously)
 * Viability: Moderate (Technically easy, but cognitively taxing).
 * * CONCLUSION:
 * The existence of Alternatives (Federation) shows that the Noose of network 
 * effects is artificial. If "Suppression" were reduced via regulation, 
 * the Noose would loosen into a Rope (a standard way to connect).
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_network_effects].
 * 2. Multi-perspective: ?- multi_index_report(network_effects).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
