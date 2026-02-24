% ============================================================================
% CONSTRAINT STORY: indexical_relativity_core
% ============================================================================
% Generated: 2026-01-21
% Model: Gemini 2.0 Flash
% Source: indexical_relativity.md
% ============================================================================

:- module(constraint_indexical_relativity, []).

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
 * * constraint_id: indexical_relativity_core
 * human_readable: The Law of Indexical Relativity
 * domain: philosophical/epistemological
 * temporal_scope: Permanent (Analytical)
 * spatial_scope: Universal
 * * SUMMARY:
 * This constraint establishes that the classification of any social or physical 
 * limitation as a Mountain, Rope, or Snare is not an intrinsic property of 
 * the object itself. Instead, it is a function of the observer's power level, 
 * time horizon, and exit options.
 * * KEY AGENTS:
 * - The Powerless Subject: Experiences constraints as immutable "Natural Law" (Mountain).
 * - The Institutional Architect: Experiences constraints as "Choice-based Coordination" (Rope).
 * - The Analytical Historian: Experiences constraints as "Extractive Structures" (Snare/Rope).
 * * NARRATIVE ARC:
 * As an agent's power or time horizon increases, Mountains "melt" into Ropes. 
 * Conversely, as power decreases, coordination Ropes tighten into coercive Snares 
 * or unchangeable Mountains.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(meta_logic_01, 0, 10).
narrative_ontology:constraint_claim(indexical_relativity_core, rope).

% Base extractiveness score (0.1: It is a logical framework, not inherently extractive)
domain_priors:base_extractiveness(indexical_relativity_core, 0.1).

% Suppression score (0.2: Low, though often ignored in favor of 'objective' universalism)
domain_priors:suppression_score(indexical_relativity_core, 0.2).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(indexical_relativity_core, extractiveness, 0.1).
narrative_ontology:constraint_metric(indexical_relativity_core, suppression_requirement, 0.2).

% This is an analytical truth that emerges from the structure of power
domain_priors:emerges_naturally(indexical_relativity_core).

% Metrics for Section 1
% Beneficiaries of understanding this:
constraint_beneficiary(indexical_relativity_core, [analytical_observers, strategic_planners]).
% Victims of ignoring this:
constraint_victim(indexical_relativity_core, [those_mistaking_snares_for_mountains]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: The Powerless Individual - Mountain
   --------------------------------------------------------------------------
   WHO: powerless (No ability to shape the rule)
   WHEN: biographical (Single lifetime)
   WHERE: trapped (No exit options)
   
   WHY THIS CLASSIFICATION:
   For the powerless, a constraint is a Mountain because they lack the agency 
   to alter it and the time to see it decay. It functions as "natural law".
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    indexical_relativity_core,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    % Mountain for powerless individuals within a single lifetime
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: The Institution / Pope - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Rule-making power)
   WHEN: generational (Maintained over decades/centuries)
   WHERE: arbitrage (Can modify the system)
   
   WHY THIS CLASSIFICATION:
   For the architect, the constraint is a Rope. It is a coordination mechanism 
   maintained by choice to achieve specific functional goals.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    indexical_relativity_core,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :-
    % Rope for those with rule-making power
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: The Historian / Observer - Snare
   --------------------------------------------------------------------------
   WHO: analytical (Non-participant observer)
   WHEN: historical (Centuries-long view)
   WHERE: analytical (Unconstrained by the system)
   
   WHY THIS CLASSIFICATION:
   The historian sees a Snare or a decayed Rope. They observe the extractive 
   nature and the eventual collapse of constraints that others saw as 
   permanent.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    indexical_relativity_core,
    snare,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :-
    % Snare when analyzing long-term extractiveness and collapse
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(indexical_relativity_tests).

/**
 * TEST: The Melting Point of Mountains
 * Demonstrates that increasing power/time horizon changes Mountain to Rope.
 */
test(mountain_melting_point) :-
    % Powerless see Mountain
    constraint_indexing:constraint_classification(indexical_relativity_core, mountain, 
        context(powerless, biographical, trapped, local)),
    % Institutional see Rope
    constraint_indexing:constraint_classification(indexical_relativity_core, rope, 
        context(institutional, generational, arbitrage, global)).

/**
 * TEST: Objective Truth Persistence
 * Verifies that each indexed classification is objectively true for its context.
 */
test(objective_indexed_truth) :-
    constraint_indexing:constraint_classification(indexical_relativity_core, Type, 
        context(powerless, biographical, trapped, local)),
    Type == mountain.

:- end_tests(indexical_relativity_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. PERSPECTIVE SELECTION: Used the "Medieval Church" example from the source 
 * to ground the abstract theory in its primary use-case.
 * 2. CORE OMEGA:
 * omega_variable(agent_perception_lag,
 * "How quickly does an agent realize their status has shifted from Powerless to Powerful?",
 * resolution_mechanism("Measure latency between agency acquisition and constraint re-classification"),
 * impact("Determines if the system is 'sticky' or fluid."),
 * confidence_without_resolution(medium)
 * ).
 * * CONFIDENCE ASSESSMENT:
 * High: The source material explicitly defines these three perspectives.
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Objective Universalism
 * Viability: The belief that a constraint is "actually" one type for everyone.
 * Suppression: Actively challenged by the Indexical Relativity framework, 
 * which labels universalism as a "perspective blind-spot".
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CORE LOGIC:
 * 1. Load: ?- [constraint_indexical_relativity].
 * 2. Audit: ?- constraint_indexing:multi_index_report(indexical_relativity_core).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
