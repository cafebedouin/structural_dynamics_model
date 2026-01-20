% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: trump_second_term_authoritarianism_2026
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Opinion Column by M. Gessen, NYT, Jan 18, 2026
% ============================================================================

:- module(trump_authoritarianism_2026, []).

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
 * * constraint_id: trump_authoritarianism_2026
 * human_readable: Electoral Authoritarianism (Trump II)
 * domain: political/legal/social
 * temporal_scope: 2025-2026 (Active/Emergent)
 * spatial_scope: National (United States)
 * * SUMMARY:
 * A year into Donald Trump’s second term, the U.S. is described as transitioning 
 * into "electoral authoritarianism." The constraint operates through the 
 * normalization of paramilitary force (ICE/Alligator Alcatraz), the summary 
 * execution of protesters (Renee Good), and the dismantling of democratic 
 * infrastructure (elections, media, universities).
 * * KEY AGENTS:
 * - The Administration (Trump/Vance): Institutional rule-makers using paramilitary forces and regulatory pressure.
 * - The Citizen/Protester (Renee Good/Gessen): Individuals whose "mental calculus" is altered by fear and state violence.
 * - Civil Society (Lawyers/Universities/Media): Decreasingly effective buffers against institutional overreach.
 * * NARRATIVE ARC:
 * The "walls are closing in." Freedom, once a "Rope" for societal coordination, 
 * is being replaced by a "Noose" of coercive surveillance and violence. 
 * What was once "Mountain-like" (the durability of democracy) is revealed 
 * as a "Scaffold" being rapidly dismantled.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for DR-Audit Suite
narrative_ontology:interval(trump_ii_year_one, 2025, 2026).
narrative_ontology:constraint_claim(trump_authoritarianism_2026, noose).

% Base extractiveness score (0.0-1.0)
% Rationale: High extraction of safety and civil liberties; "bribes" ($2,000 checks) 
% used to maintain compliance while seizing absolute power.
domain_priors:base_extractiveness(trump_authoritarianism_2026, 0.85).

% Suppression score (0.0-1.0)
% Rationale: Extreme suppression of protest (execution) and media independence 
% (lawsuits/Pentagon exclusion). Alternatives are being made "impossible."
domain_priors:suppression_score(trump_authoritarianism_2026, 0.90).

% Enforcement requirements
% Requires active maintenance via paramilitary deployment and judicial appointments.
domain_priors:requires_active_enforcement(trump_authoritarianism_2026).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(trump_authoritarianism_2026, extractiveness, 0.85).
narrative_ontology:constraint_metric(trump_authoritarianism_2026, suppression_requirement, 0.90).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(trump_authoritarianism_2026, trump_administration).
constraint_beneficiary(trump_authoritarianism_2026, loyalist_media_influencers).
constraint_victim(trump_authoritarianism_2026, political_protesters).
constraint_victim(trump_authoritarianism_2026, independent_media).
constraint_victim(trump_authoritarianism_2026, university_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================= */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE PROTESTER (Renee Good/Mental Calculus) - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to "summary execution" and state violence.
   WHEN: immediate - Every protest involves a "mental calculus" of survival.
   WHERE: trapped - Paramilitary forces hunt people in "apartments, city streets."
   SCOPE: local - Immediate physical safety in cities like Minneapolis.
   
   WHY THIS CLASSIFICATION:
   For the protester, the state is no longer a neutral arbiter but a coercive 
   Noose. The penalty for dissent has shifted from legal to lethal.
   
   NARRATIVE EVIDENCE:
   "The execution of Renee Good has surely affected every potential protester’s 
   [cite_start]mental calculus." [cite: 1]
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trump_authoritarianism_2026,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE INSTITUTIONAL LOYALIST (Loyal Media/Influencers) - Rope
   --------------------------------------------------------------------------
   
   WHO: individual_moderate - Gains access by conforming to new rules.
   WHEN: biographical - Seeking career advancement in a "Trump-friendly" era.
   WHERE: mobile - Legacy media "kicked out," but loyalists are "replacing them."
   SCOPE: national - Shaping the national discourse.
   
   WHY THIS CLASSIFICATION:
   For those who align with the administration, the new order is a beneficial 
   coordination mechanism (Rope) that provides "reapportioned access" and 
   influence.
   
   NARRATIVE EVIDENCE:
   "replacing them with loyal journalists and influencers... CBS is rapidly 
   [cite_start]transforming itself into a Trump-friendly network." [cite: 1]
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    trump_authoritarianism_2026,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ANALYTICAL OBSERVER (M. Gessen) - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Comparing current events to autocratic patterns (Russia/Hungary).
   WHEN: historical - Viewing 2026 as a pivotal shift in the American story.
   WHERE: constrained - Living in a country where "normalizing" is the default.
   SCOPE: global - Placing the U.S. in the context of global autocracies.
   
   WHY THIS CLASSIFICATION:
   From an analytical perspective, the transition to "electoral authoritarianism" 
   appears as an inevitable structural closure (Mountain). The "space is 
   imploding" and the "walls are closing in."
   
   NARRATIVE EVIDENCE:
   "Ask anyone who has lived in a country that became an autocracy... some version 
   [cite_start]of a story about walls closing in on them." [cite: 1]
   -------------------------------------------------------------------------- */


constraint_indexing:constraint_classification(
    trump_authoritarianism_2026,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(constrained),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(trump_authoritarianism_2026_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(trump_authoritarianism_2026, Type1, context(individual_powerless, immediate, trapped, local)),
    constraint_indexing:constraint_classification(trump_authoritarianism_2026, Type2, context(individual_moderate, biographical, mobile, national)),
    constraint_indexing:constraint_classification(trump_authoritarianism_2026, Type3, context(analytical, historical, constrained, global)),
    Type1 = noose,
    Type2 = rope,
    Type3 = mountain.

test(power_extractiveness_scaling) :-
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, arbitrage, national),
    constraint_indexing:extractiveness_for_agent(trump_authoritarianism_2026, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(trump_authoritarianism_2026, ContextPowerful, Score2),
    Score1 > Score2.

:- end_tests(trump_authoritarianism_2026_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * * 1. EXTRACTIVENESS SCORE (0.85):
 * The source highlights systematic extraction of liberty ("concentration 
 * camps," "disappeared") and truth ("proudly ignorant," "assaulting museums").
 * The "bribe" of $2,000 checks confirms the extractive nature—buying 
 * [cite_start]compliance with the subject's own future resources. [cite: 1]
 * * 2. SUPPRESSION SCORE (0.90):
 * Suppression is explicit: lethal force against protesters, frivolous 
 * lawsuits against media, and "reapportioning access" to exclude legacy 
 * outlets. [cite_start]Alternatives are effectively "imploding." [cite: 1]
 * * 3. OMEGAS:
 * There is irreducible uncertainty regarding the 2026 midterms and the 
 * willingness of Trump to "cancel the vote."
 */

omega_variable(
    midterm_2026_integrity,
    "How free and fair will the 2026 elections be under systematically restrictive executive actions?",
    resolution_mechanism("Monitor the ratio of purged voters to actual turnout and the prevalence of military deployment on election day."),
    impact("If Mountain: The transition to electoral authoritarianism is complete. If Rope: Resistance is still viable."),
    confidence_without_resolution(low)
).

omega_variable(
    lethal_deterrence_threshold,
    "Will the 'lethal mental calculus' of the Renee Good execution permanently suppress mass protest?",
    resolution_mechanism("Compare mass protest turnout levels post-Jan 7 vs previous high-water marks (e.g., 2020)."),
    impact("If Noose: Public space is effectively dead. If Rope: Movement adapts to paramilitary conditions."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Robust Civil Society Resistance
 * [cite_start]Viability: Lawyers and citizens are still organizing ("The lawyers have fought..."). [cite: 1]
 * [cite_start]Suppression: "Assault on the judiciary" and "threats against nonprofits" have already altered their function. [cite: 1]
 * * ALTERNATIVE 2: Free and Fair Elections
 * Viability: The primary democratic mechanism for change.
 * [cite_start]Suppression: Trump is "laying the groundwork" for systematic intimidation and cancelling the vote. [cite: 1]
 * * CONCLUSION:
 * The active suppression of these alternatives shifts the American order from 
 * a coordination Rope to a coercive Noose.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Load: ?- [trump_authoritarianism_2026].
% Report: ?- constraint_indexing:multi_index_report(trump_authoritarianism_2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(trumps_second_term_authoritarianism_2026, noose, agent_power(individual_powerless)).
