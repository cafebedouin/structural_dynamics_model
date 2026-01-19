% ============================================================================
% CONSTRAINT STORY: [CONSTRAINT_ID]
% ============================================================================
% Generated: [DATE]
% Model: [MODEL NAME]
% Source: [SOURCE]
% ============================================================================

:- module(constraint_[id], []).

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
 * constraint_id: [unique_identifier]
 * human_readable: [Descriptive name]
 * domain: [economic/political/social/religious/technological]
 * temporal_scope: [When it operates, e.g., "1200-1500 CE" or "Future/R7"]
 * spatial_scope: [Where it operates, e.g., "Medieval Europe" or "Station R7"]
 * 
 * SUMMARY:
 * [2-3 sentence description of the constraint and its context]
 * 
 * KEY AGENTS:
 * - [Agent1]: [Role/perspective]
 * - [Agent2]: [Role/perspective]
 * - [Agent3]: [Role/perspective]
 * 
 * NARRATIVE ARC:
 * [Brief description of how the constraint functions in the story/domain]
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(unknown_interval, 0, 10).
narrative_ontology:constraint_claim([constraint_id], [claimed_type]).

% Bridge to the Auditor's Metric Logic, next three
% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
domain_priors:base_extractiveness([constraint_id], [0.0-1.0]).

% Base extractiveness score (0.0 = no extraction, 1.0 = full extraction)
% Rationale: [Why this score?]
domain_priors:base_extractiveness([constraint_id], [0.0-1.0]).

% Suppression score (0.0 = no suppression, 1.0 = full suppression)
% Rationale: [Why this score?]
domain_priors:suppression_score([constraint_id], [0.0-1.0]).

% Enforcement requirements
% [Requires active enforcement OR emerges naturally]
domain_priors:requires_active_enforcement([constraint_id]).
% OR: emerges_naturally([constraint_id]).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric([constraint_id], extractiveness, [score]).
narrative_ontology:constraint_metric([constraint_id], suppression_requirement, [score]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: [AGENT NAME/ROLE] - [Mountain/Rope/Noose]
   --------------------------------------------------------------------------
   
   WHO: [Power level description]
   WHEN: [Time horizon description]
   WHERE: [Exit options description]
   SCOPE: [Spatial scope description]
   
   WHY THIS CLASSIFICATION:
   [2-3 sentences explaining why this agent/perspective sees it this way]
   
   NARRATIVE EVIDENCE:
   [Quotes or references from source material]
   -------------------------------------------------------------------------- */

% [Classification rules using constraint_indexing:constraint_classification/3...]
constraint_indexing:constraint_classification(
    [constraint_id],
    [mountain/rope/noose],
    context(
        agent_power([individual_powerless/individual_moderate/individual_powerful/collective_organized/institutional/analytical]),
        time_horizon([immediate/biographical/generational/historical/civilizational]),
        exit_options([trapped/constrained/mobile/arbitrage/analytical]),
	constraint_beneficiary([constraint_id], [beneficiary_agent_or_class]),
	constraint_victim([constraint_id], [victim_agent_or_class]),
        spatial_scope([local/regional/national/continental/global])
    )
) :-
    % Classification logic
    constraint_indexing:effective_immutability_for_context(
        context([power], [time], [exit], [scope]),
        [mountain/rope]
    ),
    % Additional conditions
    domain_priors:base_extractiveness([constraint_id], E),
    E > [threshold],  % or E < [threshold]
    !.  % Cut to prevent backtracking

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: [AGENT NAME/ROLE] - [Mountain/Rope/Noose]
   --------------------------------------------------------------------------
   [Same structure as Perspective 1]
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    [constraint_id],
    [type],
    context([power], [time], [exit], [scope])
) :-
    % Classification logic
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: [AGENT NAME/ROLE] - [Mountain/Rope/Noose]
   --------------------------------------------------------------------------
   [Same structure as Perspective 1]
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    [constraint_id],
    [type],
    context([power], [time], [exit], [scope])
) :-
    % Classification logic
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests([constraint_id]_tests).

/**
 * TEST 1: Multi-perspective variance
 * Demonstrates that same constraint = different types from different perspectives
 */
test(multi_perspective_[name]) :-
    % Perspective 1
    constraint_indexing:constraint_classification(
        [constraint_id],
        Type1,
        context([p1_power], [p1_time], [p1_exit], [p1_scope])
    ),
    % Perspective 2
    constraint_indexing:constraint_classification(
        [constraint_id],
        Type2,
        context([p2_power], [p2_time], [p2_exit], [p2_scope])
    ),
    % Perspective 3
    constraint_indexing:constraint_classification(
        [constraint_id],
        Type3,
        context([p3_power], [p3_time], [p3_exit], [p3_scope])
    ),
    % Verify they differ
    Type1 \= Type2,
    Type2 \= Type3.

/**
 * TEST 2: Power-based extractiveness scaling
 * Demonstrates that extraction experienced varies with agent power
 */
test(power_extractiveness_[name]) :-
    ContextPowerless = context(individual_powerless, [time], [exit], [scope]),
    ContextPowerful = context([institutional/individual_powerful], [time], [exit], [scope]),
    constraint_indexing:extractiveness_for_agent([constraint_id], ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent([constraint_id], ContextPowerful, Score2),
    Score1 > Score2.  % Powerless experience more extraction

/**
 * TEST 3: Time-horizon immutability
 * Demonstrates that mutability varies with time horizon
 */
test(time_immutability_[name]) :-
    % Short horizon + trapped = mountain
    constraint_indexing:effective_immutability([biographical/immediate], trapped, mountain),
    % Long horizon = rope (changeable)
    constraint_indexing:effective_immutability([civilizational/historical], [any_exit], rope).

/**
 * TEST 4: [Domain-specific insight]
 * [What this test demonstrates about the constraint]
 */
test([specific_insight_name]) :-
    % Test logic
    true.

:- end_tests([constraint_id]_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: [Gemini 2.0 Flash / Claude Sonnet 4 / etc]
 * 
 * KEY DECISIONS MADE BY MODEL:
 * 
 * 1. BASE EXTRACTIVENESS ([score]):
 *    Reasoning: [Why the model chose this score]
 *    Evidence: [What from the narrative supported this]
 *    Uncertainty: [What the model was uncertain about]
 *    Beneficiaries: [Who benefits]
 * 
 * 2. PERSPECTIVE SELECTION:
 *    Model chose to analyze from [X] perspectives because [reason]
 *    Alternative perspectives considered: [list]
 *    Why they were excluded: [reason]
 * 
 * 3. CLASSIFICATION RATIONALE:
 *    [Agent 1] as [Type]: [Model's reasoning]
 *    [Agent 2] as [Type]: [Model's reasoning]
 *    [Agent 3] as [Type]: [Model's reasoning]
 * 
 * 4. AMBIGUITIES IN SOURCE MATERIAL:
 *    - [Issue 1]: [How model resolved it]
 *    - [Issue 2]: [How model resolved it]
 *
 * 5. OMEGAS 
 *    Define uncertainty so your analysis is cleaner
 *    omega_variable([omega_id],
 *	"[Question form of uncertainty]",
 *	resolution_mechanism("[How to resolve]"),
 *	impact("[What changes if resolved]"),
 *	confidence_without_resolution([low/medium/high])
 *	).
 * 
 * 6. CONFIDENCE ASSESSMENT:
 *    High confidence: [Which aspects]
 *    Medium confidence: [Which aspects]
 *    Low confidence: [Which aspects]
 *    Would benefit from: [Additional evidence/clarification needed]
 * 
 * 7. EDGE CASES:
 *    [Situations where classification might be ambiguous]
 *    [Boundary conditions between types]
 */

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS (If Applicable)
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * 
 * Does this constraint have alternatives that were suppressed?
 * This matters for distinguishing Rope (no alternatives) from Noose (alternatives exist).
 * 
 * ALTERNATIVE 1: [Name/Description]
 *    Viability: [Why it's a real alternative]
 *    Suppression: [How/why it was rejected]
 *    Evidence: [From narrative or historical record]
 * 
 * ALTERNATIVE 2: [If applicable]
 *    ...
 * 
 * CONCLUSION:
 * [Does the existence of alternatives affect classification?]
 * [Changes Rope → Noose if alternatives were actively suppressed]
 */

% If alternatives exist (affects signature detection):
% intent_viable_alternative([interval_id], [alternative_id], '[description]').
% intent_alternative_rejected([interval_id], [alternative_id], '[reason]').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS CONSTRAINT:
 * 
 * 1. Load into main system:
 *    ?- [constraints/[constraint_id]].
 * 
 * 2. Run multi-perspective analysis:
 *    ?- constraint_indexing:multi_index_report([constraint_id]).
 * 
 * 3. Run tests:
 *    ?- run_tests([constraint_id]_tests).
 * 
 * 4. Generate pedagogical report:
 *    ?- pedagogical_report([constraint_id]).
 * 
 * 5. Compare with other constraints:
 *    ?- compare_constraints([constraint_id], [other_id]).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
